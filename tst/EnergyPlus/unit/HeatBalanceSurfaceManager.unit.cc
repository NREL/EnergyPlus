// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::HeatBalanceSurfaceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ConfiguredFunctions.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataMoistureBalance.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputReportTabular.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/WindowManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceSurfaceManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_CalcOutsideSurfTemp)
{

    int SurfNum;      // Surface number DO loop counter
    int ZoneNum;      // Zone number the current surface is attached to
    int ConstrNum;    // Construction index for the current surface
    Real64 HMovInsul; // "Convection" coefficient of movable insulation
    Real64 TempExt;   // Exterior temperature boundary condition
    bool ErrorFlag;   // Movable insulation error flag

    SurfNum = 1;
    ZoneNum = 1;
    ConstrNum = 1;
    HMovInsul = 1.0;
    TempExt = 23.0;
    ErrorFlag = false;

    state->dataGlobal->NumOfTimeStepInHour = 4;
    state->dataGlobal->TimeStepZoneSec = 900.0;

    state->dataConstruction->Construct.allocate(ConstrNum);
    state->dataConstruction->Construct(ConstrNum).Name = "TestConstruct";
    state->dataConstruction->Construct(ConstrNum).CTFCross(0) = 0.0;
    state->dataConstruction->Construct(ConstrNum).CTFOutside(0) = 1.0;
    state->dataConstruction->Construct(ConstrNum).SourceSinkPresent = true;
    state->dataMaterial->Material.allocate(1);
    state->dataMaterial->Material(1).Name = "TestMaterial";

    state->dataSurface->TotSurfaces = SurfNum;
    state->dataGlobal->NumOfZones = ZoneNum;

    state->dataSurface->Surface.allocate(SurfNum);
    state->dataHeatBal->Zone.allocate(ZoneNum);

    state->dataSurface->Surface(SurfNum).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->Surface(SurfNum).Area = 10.0;
    WindowManager::initWindowModel(*state);
    SurfaceGeometry::AllocateSurfaceWindows(*state, SurfNum);
    SolarShading::AllocateModuleArrays(*state);
    AllocateSurfaceHeatBalArrays(*state);
    SurfaceGeometry::AllocateSurfaceArrays(*state);

    state->dataHeatBalSurf->SurfHcExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHAirExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHSkyExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHGrdExt(SurfNum) = 1.0;

    state->dataHeatBalSurf->SurfCTFConstOutPart(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfTempIn(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) = 0.0;
    state->dataSurface->SurfHasSurroundingSurfProperties(SurfNum) = 0;
    state->dataSurface->SurfMaterialMovInsulExt(SurfNum) = 1;

    state->dataSurface->SurfOutDryBulbTemp = 0;
    state->dataEnvrn->SkyTemp = 23.0;
    state->dataEnvrn->OutDryBulbTemp = 23.0;

    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;

    state->dataHeatBal->Zone(ZoneNum).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(ZoneNum).HTSurfaceLast = 1;
    state->dataHeatBal->Zone(ZoneNum).OpaqOrIntMassSurfaceFirst = 1;
    state->dataHeatBal->Zone(ZoneNum).OpaqOrIntMassSurfaceLast = 1;
    state->dataHeatBal->Zone(ZoneNum).OpaqOrWinSurfaceFirst = 1;
    state->dataHeatBal->Zone(ZoneNum).OpaqOrWinSurfaceLast = 1;

    CalcOutsideSurfTemp(*state, SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt, ErrorFlag);

    state->dataHeatBalSurf->SurfTempOut(SurfNum) = state->dataHeatBalSurf->SurfOutsideTempHist(1)(SurfNum);

    ReportSurfaceHeatBalance(*state);

    std::string const error_string = delimited_string({
        "   ** Severe  ** Exterior movable insulation is not valid with embedded sources/sinks",
        "   **   ~~~   ** Construction TestConstruct contains an internal source or sink but also uses",
        "   **   ~~~   ** exterior movable insulation TestMaterial for a surface with that construction.",
        "   **   ~~~   ** This is not currently allowed because the heat balance equations do not currently accommodate this combination.",
    });

    EXPECT_TRUE(ErrorFlag);
    EXPECT_TRUE(compare_err_stream(error_string, true));
    EXPECT_EQ(10.0 * 1.0 * (state->dataHeatBalSurf->SurfOutsideTempHist(1)(SurfNum) - state->dataSurface->SurfOutDryBulbTemp(SurfNum)),
              state->dataHeatBalSurf->SurfQAirExtReport(SurfNum));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfTempCalcHeatBalanceInsideSurf)
{

    Real64 surfTemp;
    state->dataSurface->Surface.allocate(1);
    state->dataSurface->SurfLowTempErrCount.allocate(1);
    state->dataSurface->SurfHighTempErrCount.allocate(1);
    DataHeatBalance::ZoneData testZone;
    int cntWarmupSurfTemp = 0;
    state->dataSurface->Surface(1).Name = "TestSurface";
    testZone.Name = "TestZone";
    testZone.InternalHeatGains = 2.5;
    testZone.NominalInfilVent = 0.5;
    testZone.NominalMixing = 0.7;

    // no error
    surfTemp = 26;
    state->dataGlobal->WarmupFlag = true;
    state->dataSurface->SurfLowTempErrCount(1) = 0;
    state->dataSurface->SurfHighTempErrCount(1) = 0;
    testZone.TempOutOfBoundsReported = true;
    testZone.FloorArea = 1000;
    testZone.IsControlled = true;
    TestSurfTempCalcHeatBalanceInsideSurf(*state, surfTemp, 1, testZone, cntWarmupSurfTemp);
    EXPECT_TRUE(compare_err_stream("", true));

    // to hot - first time
    surfTemp = 201;
    state->dataGlobal->WarmupFlag = false;
    state->dataSurface->SurfLowTempErrCount(1) = 0;
    state->dataSurface->SurfHighTempErrCount(1) = 0;
    testZone.TempOutOfBoundsReported = false;
    testZone.FloorArea = 1000;
    testZone.IsControlled = true;
    TestSurfTempCalcHeatBalanceInsideSurf(*state, surfTemp, 1, testZone, cntWarmupSurfTemp);
    std::string const error_string01 =
        delimited_string({"   ** Severe  ** Temperature (high) out of bounds (201.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
                          "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
                          "   **   ~~~   ** Zone=\"TestZone\", Diagnostic Details:",
                          "   **   ~~~   ** ...Internal Heat Gain [2.500E-003] W/m2",
                          "   **   ~~~   ** ...Infiltration/Ventilation [0.500] m3/s",
                          "   **   ~~~   ** ...Mixing/Cross Mixing [0.700] m3/s",
                          "   **   ~~~   ** ...Zone is part of HVAC controlled system."});
    EXPECT_TRUE(compare_err_stream(error_string01, true));
    EXPECT_TRUE(testZone.TempOutOfBoundsReported);

    // to hot - subsequent times
    surfTemp = 201;
    state->dataGlobal->WarmupFlag = false;
    state->dataSurface->SurfLowTempErrCount(1) = 0;
    state->dataSurface->SurfHighTempErrCount(1) = 0;
    testZone.TempOutOfBoundsReported = true;
    testZone.FloorArea = 1000;
    testZone.IsControlled = true;
    TestSurfTempCalcHeatBalanceInsideSurf(*state, surfTemp, 1, testZone, cntWarmupSurfTemp);
    std::string const error_string02 = delimited_string({
        "   ** Severe  ** Temperature (high) out of bounds (201.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
        "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
    });
    EXPECT_TRUE(compare_err_stream(error_string02, true));
    EXPECT_TRUE(testZone.TempOutOfBoundsReported);

    // to cold - first time
    surfTemp = -101;
    state->dataGlobal->WarmupFlag = false;
    state->dataSurface->SurfLowTempErrCount(1) = 0;
    state->dataSurface->SurfHighTempErrCount(1) = 0;
    testZone.TempOutOfBoundsReported = false;
    testZone.FloorArea = 1000;
    testZone.IsControlled = true;
    TestSurfTempCalcHeatBalanceInsideSurf(*state, surfTemp, 1, testZone, cntWarmupSurfTemp);
    std::string const error_string03 =
        delimited_string({"   ** Severe  ** Temperature (low) out of bounds [-101.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
                          "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00",
                          "   **   ~~~   ** Zone=\"TestZone\", Diagnostic Details:",
                          "   **   ~~~   ** ...Internal Heat Gain [2.500E-003] W/m2",
                          "   **   ~~~   ** ...Infiltration/Ventilation [0.500] m3/s",
                          "   **   ~~~   ** ...Mixing/Cross Mixing [0.700] m3/s",
                          "   **   ~~~   ** ...Zone is part of HVAC controlled system."});
    EXPECT_TRUE(compare_err_stream(error_string03, true));
    EXPECT_TRUE(testZone.TempOutOfBoundsReported);

    // to cold - subsequent times
    surfTemp = -101;
    state->dataGlobal->WarmupFlag = false;
    state->dataSurface->SurfLowTempErrCount(1) = 0;
    state->dataSurface->SurfHighTempErrCount(1) = 0;
    testZone.TempOutOfBoundsReported = true;
    testZone.FloorArea = 1000;
    testZone.IsControlled = true;
    TestSurfTempCalcHeatBalanceInsideSurf(*state, surfTemp, 1, testZone, cntWarmupSurfTemp);
    std::string const error_string04 =
        delimited_string({"   ** Severe  ** Temperature (low) out of bounds [-101.00] for zone=\"TestZone\", for surface=\"TestSurface\"",
                          "   **   ~~~   **  Environment=, at Simulation time= 00:00 - 00:00"});
    EXPECT_TRUE(compare_err_stream(error_string04, true));
    EXPECT_TRUE(testZone.TempOutOfBoundsReported);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_ComputeIntThermalAbsorpFactors)
{

    state->dataSurface->TotSurfaces = 1;
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->TotMaterials = 1;
    state->dataHeatBal->TotConstructs = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBal->Zone(1).WindowSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).WindowSurfaceLast = 1;
    state->dataHeatBal->Zone(1).zoneRadEnclosureFirst = 1;
    state->dataHeatBal->Zone(1).zoneRadEnclosureLast = 1;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->SurfaceWindow.allocate(state->dataSurface->TotSurfaces);
    SurfaceGeometry::AllocateSurfaceWindows(*state, state->dataSurface->TotSurfaces);
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);
    state->dataMaterial->Material.allocate(state->dataHeatBal->TotMaterials);
    state->dataSurface->SurfaceWindow(1).EffShBlindEmiss(1) = 0.1;
    state->dataSurface->SurfaceWindow(1).EffGlassEmiss(1) = 0.1;

    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).Construction = 1;
    state->dataSurface->Surface(1).Area = 1;
    state->dataSurface->SurfWinShadingFlag(1) = DataSurfaces::WinShadingType::IntBlind;
    state->dataConstruction->Construct(1).InsideAbsorpThermal = 0.9;
    state->dataHeatBalSurf->SurfAbsThermalInt.allocate(1);

    state->dataViewFactor->NumOfRadiantEnclosures = 1;
    state->dataViewFactor->EnclRadInfo.allocate(1);
    state->dataHeatBal->EnclRadReCalc.allocate(1);
    state->dataHeatBal->EnclRadReCalc(1) = true;
    state->dataHeatBal->EnclRadThermAbsMult.allocate(1);
    state->dataViewFactor->EnclRadInfo(1).SurfacePtr.allocate(1);
    state->dataViewFactor->EnclRadInfo(1).SurfacePtr(1) = 1;

    ComputeIntThermalAbsorpFactors(*state);

    EXPECT_EQ(0.2, state->dataHeatBalSurf->SurfAbsThermalInt(1));
    EXPECT_EQ(5, state->dataHeatBal->EnclRadThermAbsMult(1));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_UpdateFinalThermalHistories)
{
    state->dataSurface->TotSurfaces = 1;
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->TotConstructs = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->SurfaceWindow.allocate(state->dataSurface->TotSurfaces);
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);
    state->dataHeatBal->AnyInternalHeatSourceInInput = true;
    state->dataHeatBal->SimpleCTFOnly = false;

    AllocateSurfaceHeatBalArrays(*state); // allocates a host of variables related to CTF calculations

    state->dataSurface->Surface(1).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(1).ExtBoundCond = 1;
    state->dataSurface->Surface(1).Construction = 1;
    state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceLast = 1;
    state->dataHeatBal->Zone(1).HTSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).HTSurfaceLast = 1;

    state->dataConstruction->Construct(1).NumCTFTerms = 2;
    state->dataConstruction->Construct(1).SourceSinkPresent = true;
    state->dataConstruction->Construct(1).NumHistories = 1;
    state->dataConstruction->Construct(1).CTFTUserOut(0) = 0.5;
    state->dataConstruction->Construct(1).CTFTUserIn(0) = 0.25;
    state->dataConstruction->Construct(1).CTFTUserSource(0) = 0.25;

    state->dataHeatBalSurf->SurfCurrNumHist(1) = 0;
    state->dataHeatBalSurf->SurfOutsideTempHist(1)(1) = 20.0;
    state->dataHeatBalSurf->SurfTempIn(1) = 10.0;

    state->dataHeatBalFanSys->CTFTuserConstPart(1) = 0.0;

    UpdateThermalHistories(*state); // First check to see if it is calculating the user location temperature properly

    EXPECT_EQ(12.5, state->dataHeatBalSurf->SurfTempUserLoc(1));
    EXPECT_EQ(0.0, state->dataHeatBalSurf->SurfTuserHist(1, 3));

    UpdateThermalHistories(*state);

    EXPECT_EQ(12.5, state->dataHeatBalSurf->SurfTuserHist(1, 3)); // Now check to see that it is shifting the temperature history properly
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfTempCalcHeatBalanceInsideSurfAirRefT)
{

    std::string const idf_objects = delimited_string({
        "  Building,",
        "    House with AirflowNetwork simulation,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.001,                   !- Loads Convergence Tolerance Value",
        "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  RunPeriod,",
        "    WinterDay,               !- Name",
        "    1,                       !- Begin Month",
        "    14,                      !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    1,                       !- End Month",
        "    14,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  RunPeriod,",
        "    SummerDay,               !- Name",
        "    7,                       !- Begin Month",
        "    7,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    7,                       !- End Month",
        "    7,                       !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    No;                      !- Use Weather File Snow Indicators",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    CB11,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2032000,               !- Thickness {m}",
        "    1.048000,                !- Conductivity {W/m-K}",
        "    1105.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material,",
        "    GP01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1600000,               !- Conductivity {W/m-K}",
        "    801.0000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    9.0099998E-02,           !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN05,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2458000,               !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    PW03,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    545.0000,                !- Density {kg/m3}",
        "    1213.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "  Material,",
        "    CC03,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1016000,               !- Thickness {m}",
        "    1.310000,                !- Conductivity {W/m-K}",
        "    2243.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    HF-A3,                   !- Name",
        "    Smooth,                  !- Roughness",
        "    1.5000000E-03,           !- Thickness {m}",
        "    44.96960,                !- Conductivity {W/m-K}",
        "    7689.000,                !- Density {kg/m3}",
        "    418.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    AR02,                    !- Name",
        "    VeryRough,               !- Roughness",
        "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    CP02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2170000,               !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Construction,",
        "    EXTWALL:LIVING,          !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    GP01;                    !- Layer 3",

        "  Construction,",
        "    FLOOR:LIVING,            !- Name",
        "    CC03,                    !- Outside Layer",
        "    CP02;                    !- Layer 2",

        "  Construction,",
        "    ROOF,                    !- Name",
        "    AR02,                    !- Outside Layer",
        "    PW03;                    !- Layer 2",

        "  Zone,",
        "    LIVING ZONE,             !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Living:North,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:East,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:South,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:West,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",
        "  BuildingSurface:Detailed,",
        "    Living:Floor,            !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR:LIVING,            !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Living:Floor,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Ceiling,          !- Name",
        "    ROOF,                 !- Surface Type",
        "    ROOF,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::GetSurfaceData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "LIVING ZONE";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    // allocate surface level adj ratio data member
    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(4) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(5) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(6) = 0.5;
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);
    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    SolarShading::AllocateModuleArrays(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);

    for (int loop = 1; loop <= state->dataSurface->TotSurfaces; ++loop) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(loop) = 20.0;
    }
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::ZoneSupplyAirTemp;

    // with supply air
    CalcHeatBalanceInsideSurf(*state);
    EXPECT_EQ(24.0, state->dataHeatBal->SurfTempEffBulkAir(1));
    EXPECT_EQ(23.0, state->dataHeatBal->SurfTempEffBulkAir(2));
    EXPECT_EQ(20.0, state->dataHeatBal->SurfTempEffBulkAir(3));

    // Supply air flow rate = 0
    state->dataLoopNodes->Node(1).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.0;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.0;
    CalcHeatBalanceInsideSurf(*state);
    EXPECT_EQ(24.0, state->dataHeatBal->SurfTempEffBulkAir(1));
    EXPECT_EQ(23.0, state->dataHeatBal->SurfTempEffBulkAir(2));
    EXPECT_EQ(24.0, state->dataHeatBal->SurfTempEffBulkAir(3));

    state->dataZoneEquip->ZoneEquipConfig.deallocate();
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataHeatBalFanSys->MAT.deallocate(); // Zone temperature C
    state->dataHeatBalFanSys->ZoneAirHumRat.deallocate();
    state->dataLoopNodes->Node.deallocate();
    state->dataGlobal->KickOffSimulation = false;
    state->dataHeatBalSurf->SurfTempInTmp.deallocate();
    state->dataHeatBalSurf->SurfHConvInt.deallocate();
    state->dataMstBal->HConvInFD.deallocate();
    state->dataMstBal->RhoVaporAirIn.deallocate();
    state->dataMstBal->HMassConvInFD.deallocate();
    state->dataHeatBalFanSys->ZoneLatentGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRep.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.deallocate();
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfTempCalcHeatBalanceInsideSurfKiva)
{

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
        "Material,",
        "  1/2IN Gypsum,            !- Name",
        "  Smooth,                  !- Roughness",
        "  0.0127,                  !- Thickness {m}",
        "  0.1600,                  !- Conductivity {W/m-K}",
        "  784.9000,                !- Density {kg/m3}",
        "  830.0000,                !- Specific Heat {J/kg-K}",
        "  0.9000,                  !- Thermal Absorptance",
        "  0.9200,                  !- Solar Absorptance",
        "  0.9200;                  !- Visible Absorptance",
        " ",
        "Material,",
        "  MAT-CC05 4 HW CONCRETE,  !- Name",
        "  Rough,                   !- Roughness",
        "  0.1016,                  !- Thickness {m}",
        "  1.3110,                  !- Conductivity {W/m-K}",
        "  2240.0000,               !- Density {kg/m3}",
        "  836.8000,                !- Specific Heat {J/kg-K}",
        "  0.9000,                  !- Thermal Absorptance",
        "  0.7000,                  !- Solar Absorptance",
        "  0.7000;                  !- Visible Absorptance",
        " ",
        "Material:NoMass,",
        "  CP02 CARPET PAD,         !- Name",
        "  VeryRough,               !- Roughness",
        "  0.2165,                  !- Thermal Resistance {m2-K/W}",
        "  0.9000,                  !- Thermal Absorptance",
        "  0.7000,                  !- Solar Absorptance",
        "  0.8000;                  !- Visible Absorptance",
        " ",
        "Material,",
        "  Std AC02,                !- Name",
        "  MediumSmooth,            !- Roughness",
        "  1.2700000E-02,           !- Thickness {m}",
        "  5.7000000E-02,           !- Conductivity {W/m-K}",
        "  288.0000,                !- Density {kg/m3}",
        "  1339.000,                !- Specific Heat {J/kg-K}",
        "  0.9000000,               !- Thermal Absorptance",
        "  0.7000000,               !- Solar Absorptance",
        "  0.2000000;               !- Visible Absorptance",
        " ",
        "Construction,",
        "  int-walls,               !- Name",
        "  1/2IN Gypsum,            !- Outside Layer",
        "  1/2IN Gypsum;            !- Layer 2",
        " ",
        "Construction,",
        "  INT-FLOOR-TOPSIDE,       !- Name",
        "  MAT-CC05 4 HW CONCRETE,  !- Outside Layer",
        "  CP02 CARPET PAD;         !- Layer 2",
        " ",
        "Construction,",
        "  DropCeiling,             !- Name",
        "  Std AC02;                !- Outside Layer",
        " ",
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
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Ceiling,   !- Name",
        "  Ceiling,                 !- Surface Type",
        "  DropCeiling,             !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  4.5732,44.1650,2.7440,  !- X,Y,Z ==> Vertex 1 {m}",
        "  4.5732,4.5732,2.7440,  !- X,Y,Z ==> Vertex 2 {m}",
        "  68.5340,4.5732,2.7440,  !- X,Y,Z ==> Vertex 3 {m}",
        "  68.5340,44.1650,2.7440;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Floor,     !- Name",
        "  Floor,                   !- Surface Type",
        "  INT-FLOOR-TOPSIDE,       !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  68.5340,44.1650,0.0000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  68.5340,4.5732,0.0000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  4.5732,4.5732,0.0000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  4.5732,44.1650,0.0000;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Wall_East, !- Name",
        "  Wall,                    !- Surface Type",
        "  int-walls,               !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  68.5340,4.5732,2.7440,  !- X,Y,Z ==> Vertex 1 {m}",
        "  68.5340,4.5732,0.0000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  68.5340,44.1650,0.0000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  68.5340,44.1650,2.7440;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Wall_North,!- Name",
        "  Wall,                    !- Surface Type",
        "  int-walls,               !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  68.5340,44.1650,2.7440,  !- X,Y,Z ==> Vertex 1 {m}",
        "  68.5340,44.1650,0.0000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  4.5732,44.1650,0.0000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  4.5732,44.1650,2.7440;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Wall_South,!- Name",
        "  Wall,                    !- Surface Type",
        "  int-walls,               !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  4.5732,4.5732,2.7440,  !- X,Y,Z ==> Vertex 1 {m}",
        "  4.5732,4.5732,0.0000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  68.5340,4.5732,0.0000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  68.5340,4.5732,2.7440;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "BuildingSurface:Detailed,",
        "  Core_bot_ZN_5_Wall_West, !- Name",
        "  Wall,                    !- Surface Type",
        "  int-walls,               !- Construction Name",
        "  Core_bottom,             !- Zone Name",
        "  ,                        !- Space Name",
        "  Adiabatic,               !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  AutoCalculate,           !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  4.5732,44.1650,2.7440,  !- X,Y,Z ==> Vertex 1 {m}",
        "  4.5732,44.1650,0.0000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  4.5732,4.5732,0.0000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  4.5732,4.5732,2.7440;  !- X,Y,Z ==> Vertex 4 {m}",
        " ",
        "People,",
        "  Core_bottom People,      !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  Core_bottom Occupancy,   !- Number of People Schedule Name",
        "  People,                  !- Number of People Calculation Method",
        "  4,                       !- Number of People",
        "  ,                        !- People per Zone Floor Area",
        "  ,                        !- Zone Floor Area per Person",
        "  0.9,                     !- Fraction Radiant",
        "  0.1,                     !- Sensible Heat Fraction",
        "  Core_bottom Activity,    !- Activity Level Schedule Name",
        "  3.82e-08,                !- Carbon Dioxide Generation Rate",
        "  Yes,                     !- Enable ASHRAE 55 Comfort Warnings",
        "  ZoneAveraged,            !- Mean Radiant Temperature Calculation Type",
        "  ,                        !- Surface NameAngle Factor List Name",
        "  Work Eff Sched,          !- Work Efficiency Schedule Name",
        "  ClothingInsulationSchedule,  !- Clothing Insulation Calculation Method",
        "  ,                        !- Clothing Insulation Calculation Method Schedule Name",
        "  Clothing Schedule,       !- Clothing Insulation Schedule Name",
        "  Air Velocity Schedule,   !- Air Velocity Schedule Name",
        "  Fanger;                  !- Thermal Comfort Model 1 Type",
        " ",
        "Schedule:Compact,",
        "  Core_bottom Occupancy,   !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: Alldays,            !- Field 2",
        "  Until: 07:00,            !- Field 3",
        "  0,                       !- Field 4",
        "  Until: 21:00,            !- Field 13",
        "  1,                       !- Field 14",
        "  Until: 24:00,            !- Field 15",
        "  0;                       !- Field 16",
        " ",
        "Schedule:Compact,",
        "  Core_bottom Activity,    !- Name",
        "  Activity Level,          !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: Alldays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  166;                     !- Field 4",
        " ",
        "Schedule:Compact,",
        "  Work Eff Sched,          !- Name",
        "  Dimensionless,           !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  0;                       !- Field 4",
        " ",
        "Schedule:Compact,",
        "  Clothing Schedule,       !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  0.5;                     !- Field 4",
        " ",
        "Schedule:Compact,",
        "  Air Velocity Schedule,   !- Name",
        "  Velocity,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,            !- Field 3",
        "  0.129999995231628;       !- Field 4",
        " ",
        "ZoneControl:Thermostat,",
        "  Core_bottom Thermostat,  !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  Dual Zone Control Type Sched,  !- Control Type Schedule Name",
        "  ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "  Core_bottom DualSPSched; !- Control 1 Name",
        " ",
        "ZoneControl:Thermostat:ThermalComfort,",
        "  Core_bottom Comfort,     !- Name",
        "  Core_bottom,             !- Zone or ZoneList Name",
        "  SpecificObject,          !- Averaging Method",
        "  Core_bottom People,      !- Specific People Name",
        "  0,                       !- Minimum DryBulb Temperature Setpoint",
        "  50,                      !- Maximum DryBulb Temperature Setpoint",
        "  Comfort Control,         !- Thermal Comfort Control Type Schedule Name",
        "  ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating,    !- Thermal Comfort Control 1 Object Type",
        "  Single Htg PMV,          !- Thermal Comfort Control 1 Name",
        "  ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling,    !- Thermal Comfort Control 2 Object Type",
        "  Single Cooling PMV;      !- Thermal Comfort Control 2 Name,",
        " ",
        "Schedule:Compact,",
        "  Comfort Control,          !- Name",
        "  Control Type,             !- Schedule Type Limits Name",
        "  Through: 5/31,            !- Field 1",
        "  For: AllDays,             !- Field 2",
        "  Until: 24:00,             !- Field 3",
        "  1,                        !- Field 4",
        "  Through: 8/31,            !- Field 5",
        "  For: AllDays,             !- Field 6",
        "  Until: 24:00,             !- Field 7",
        "  2,                        !- Field 8",
        "  Through: 12/31,           !- Field 9",
        "  For: AllDays,             !- Field 10",
        "  Until: 24:00,             !- Field 11",
        "  1;                        !- Field 12",
        " ",
        "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:SINGLEHEATING,",
        "  Single Htg PMV,          !- Name",
        "  Single Htg PMV;          !- Fanger Thermal Comfort Schedule Name",
        " ",
        "THERMOSTATSETPOINT:THERMALCOMFORT:FANGER:SINGLECOOLING,",
        "  Single Cooling PMV,      !- Name",
        "  Single Cooling PMV;      !- Fanger Thermal Comfort Schedule Name",
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
        "Schedule:Compact,",
        "  Single Htg PMV,          !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 6:00,             !- Field 3",
        "  -0.5,                    !- Field 4",
        "  Until: 23:00,            !- Field 5",
        "  -0.2,                    !- Field 6",
        "  Until: 24:00,            !- Field 7",
        "  -0.5;                    !- Field 8",
        " ",
        "Schedule:Compact,",
        "  Single Cooling PMV,      !- Name",
        "  Any Number,              !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 6:00,             !- Field 3",
        "  0.5,                     !- Field 4",
        "  Until: 23:00,            !- Field 5",
        "  0.2,                     !- Field 6",
        "  Until: 24:00,            !- Field 7",
        "  0.5;                     !- Field 8",
        " ",
        "ScheduleTypeLimits,",
        "  Fraction,                 !- Name",
        "  0,                        !- Lower Limit Value",
        "  1,                        !- Upper Limit Value",
        "  Continuous,               !- Numeric Type",
        "  Dimensionless;            !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  Temperature,              !- Name",
        "  -60,                      !- Lower Limit Value",
        "  200,                      !- Upper Limit Value",
        "  Continuous,               !- Numeric Type",
        "  Temperature;              !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  Control Type,             !- Name",
        "  0,                        !- Lower Limit Value",
        "  4,                        !- Upper Limit Value",
        "  Discrete,                 !- Numeric Type",
        "  Dimensionless;            !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  On/Off,                   !- Name",
        "  0,                        !- Lower Limit Value",
        "  1,                        !- Upper Limit Value",
        "  Discrete,                 !- Numeric Type",
        "  Dimensionless;            !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  Any Number,               !- Name",
        "  ,                         !- Lower Limit Value",
        "  ,                         !- Upper Limit Value",
        "  Continuous;               !- Numeric Type",
        " ",
        "ScheduleTypeLimits,",
        "  Velocity,                 !- Name",
        "  ,                         !- Lower Limit Value",
        "  ,                         !- Upper Limit Value",
        "  Continuous,               !- Numeric Type",
        "  Velocity;                 !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  Activity Level,           !- Name",
        "  0,                        !- Lower Limit Value",
        "  ,                         !- Upper Limit Value",
        "  Continuous,               !- Numeric Type",
        "  ActivityLevel;            !- Unit Type",
        " ",
        "ScheduleTypeLimits,",
        "  Dimensionless,            !- Name",
        "  -1,                       !- Lower Limit Value",
        "  1,                        !- Upper Limit Value",
        "  Continuous;               !- Numeric Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    ASSERT_FALSE(ErrorsFound);

    state->dataEnvrn->DayOfYear_Schedule = 1;      // must initialize this to get schedules initialized
    state->dataEnvrn->DayOfWeek = 1;               // must initialize this to get schedules initialized
    state->dataGlobal->HourOfDay = 1;              // must initialize this to get schedules initialized
    state->dataGlobal->TimeStep = 1;               // must initialize this to get schedules initialized
    state->dataGlobal->NumOfTimeStepInHour = 1;    // must initialize this to get schedules initialized
    state->dataGlobal->MinutesPerTimeStep = 60;    // must initialize this to get schedules initialized
    ScheduleManager::ProcessScheduleInput(*state); // read schedules

    state->files.inputWeatherFilePath.filePath = configured_source_directory() / "tst/EnergyPlus/unit/Resources/HeatBalanceKivaManagerOSkyTest.epw";
    state->dataWeatherManager->WeatherFileExists = true;
    HeatBalanceManager::GetHeatBalanceInput(*state);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);
    EXPECT_TRUE(compare_err_stream(""));
    EXPECT_FALSE(has_err_output(true));

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    // allocate surface level adj ratio data member
    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(4) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(5) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(6) = 0.5;
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);

    state->dataScheduleMgr->Schedule(1).CurrentValue = -0.1;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0.1;

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;

    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);
    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    for (int loop = 1; loop <= state->dataSurface->TotSurfaces; ++loop) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(loop) = 20.0;
    }
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::ZoneSupplyAirTemp;

    InitSurfaceHeatBalance(*state);

    state->dataSurface->Surface(5).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::Kiva;
    state->dataSurface->AllHTKivaSurfaceList = {5};
    CalcHeatBalanceInsideSurf(*state);

    ReportSurfaceHeatBalance(*state);

    // Check that the inside face surface conduction = -(convection + radiation)
    EXPECT_NEAR(state->dataHeatBalSurf->SurfOpaqInsFaceCond(5), -21.7, 0.1);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfPropertyLocalEnv)
{

    std::string const idf_objects =
        delimited_string({"  Building,",
                          "    House with Local Air Nodes,  !- Name",
                          "    0,                       !- North Axis {deg}",
                          "    Suburbs,                 !- Terrain",
                          "    0.001,                   !- Loads Convergence Tolerance Value",
                          "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
                          "    FullInteriorAndExterior, !- Solar Distribution",
                          "    25,                      !- Maximum Number of Warmup Days",
                          "    6;                       !- Minimum Number of Warmup Days",

                          "  Timestep,6;",

                          "  SurfaceConvectionAlgorithm:Inside,TARP;",

                          "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

                          "  HeatBalanceAlgorithm,ConductionTransferFunction;",

                          "  SimulationControl,",
                          "    No,                      !- Do Zone Sizing Calculation",
                          "    No,                      !- Do System Sizing Calculation",
                          "    No,                      !- Do Plant Sizing Calculation",
                          "    Yes,                     !- Run Simulation for Sizing Periods",
                          "    Yes;                     !- Run Simulation for Weather File Run Periods",

                          "  RunPeriod,",
                          "    WinterDay,               !- Name",
                          "    1,                       !- Begin Month",
                          "    14,                      !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    1,                       !- End Month",
                          "    14,                      !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    Yes;                     !- Use Weather File Snow Indicators",

                          "  RunPeriod,",
                          "    SummerDay,               !- Name",
                          "    7,                       !- Begin Month",
                          "    7,                       !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    7,                       !- End Month",
                          "    7,                       !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    No;                      !- Use Weather File Snow Indicators",

                          "  Site:Location,",
                          "    CHICAGO_IL_USA TMY2-94846,  !- Name",
                          "    41.78,                   !- Latitude {deg}",
                          "    -87.75,                  !- Longitude {deg}",
                          "    -6.00,                   !- Time Zone {hr}",
                          "    190.00;                  !- Elevation {m}",

                          "  SizingPeriod:DesignDay,",
                          "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
                          "    1,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    WinterDesignDay,         !- Day Type",
                          "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
                          "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    99063.,                  !- Barometric Pressure {Pa}",
                          "    4.9,                     !- Wind Speed {m/s}",
                          "    270,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    0.0;                     !- Sky Clearness",

                          "  SizingPeriod:DesignDay,",
                          "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
                          "    7,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    SummerDesignDay,         !- Day Type",
                          "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
                          "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    99063.,                  !- Barometric Pressure {Pa}",
                          "    5.3,                     !- Wind Speed {m/s}",
                          "    230,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    1.0;                     !- Sky Clearness",

                          "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

                          "  Material,",
                          "    A1 - 1 IN STUCCO,        !- Name",
                          "    Smooth,                  !- Roughness",
                          "    2.5389841E-02,           !- Thickness {m}",
                          "    0.6918309,               !- Conductivity {W/m-K}",
                          "    1858.142,                !- Density {kg/m3}",
                          "    836.8000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.9200000,               !- Solar Absorptance",
                          "    0.9200000;               !- Visible Absorptance",

                          "  Material,",
                          "    CB11,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.2032000,               !- Thickness {m}",
                          "    1.048000,                !- Conductivity {W/m-K}",
                          "    1105.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material,",
                          "    GP01,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1600000,               !- Conductivity {W/m-K}",
                          "    801.0000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    9.0099998E-02,           !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN05,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2458000,               !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    PW03,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1150000,               !- Conductivity {W/m-K}",
                          "    545.0000,                !- Density {kg/m3}",
                          "    1213.000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7800000,               !- Solar Absorptance",
                          "    0.7800000;               !- Visible Absorptance",

                          "  Material,",
                          "    CC03,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.1016000,               !- Thickness {m}",
                          "    1.310000,                !- Conductivity {W/m-K}",
                          "    2243.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.6500000,               !- Solar Absorptance",
                          "    0.6500000;               !- Visible Absorptance",

                          "  Material,",
                          "    HF-A3,                   !- Name",
                          "    Smooth,                  !- Roughness",
                          "    1.5000000E-03,           !- Thickness {m}",
                          "    44.96960,                !- Conductivity {W/m-K}",
                          "    7689.000,                !- Density {kg/m3}",
                          "    418.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    AR02,                    !- Name",
                          "    VeryRough,               !- Roughness",
                          "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7000000,               !- Solar Absorptance",
                          "    0.7000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    CP02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2170000,               !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Construction,",
                          "    EXTWALL:LIVING,          !- Name",
                          "    A1 - 1 IN STUCCO,        !- Outside Layer",
                          "    GP01;                    !- Layer 3",

                          "  Construction,",
                          "    FLOOR:LIVING,            !- Name",
                          "    CC03,                    !- Outside Layer",
                          "    CP02;                    !- Layer 2",

                          "  Construction,",
                          "    ROOF,                    !- Name",
                          "    AR02,                    !- Outside Layer",
                          "    PW03;                    !- Layer 2",

                          "  Zone,",
                          "    LIVING ZONE,             !- Name",
                          "    0,                       !- Direction of Relative North {deg}",
                          "    0,                       !- X Origin {m}",
                          "    0,                       !- Y Origin {m}",
                          "    0,                       !- Z Origin {m}",
                          "    1,                       !- Type",
                          "    1,                       !- Multiplier",
                          "    autocalculate,           !- Ceiling Height {m}",
                          "    autocalculate;           !- Volume {m3}",

                          "  GlobalGeometryRules,",
                          "    UpperLeftCorner,         !- Starting Vertex Position",
                          "    CounterClockWise,        !- Vertex Entry Direction",
                          "    World;                   !- Coordinate System",

                          "  BuildingSurface:Detailed,",
                          "    Living:North,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:East,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:South,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:West,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",
                          "  BuildingSurface:Detailed,",
                          "    Living:Floor,            !- Name",
                          "    FLOOR,                   !- Surface Type",
                          "    FLOOR:LIVING,            !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Surface,                 !- Outside Boundary Condition",
                          "    Living:Floor,            !- Outside Boundary Condition Object",
                          "    NoSun,                   !- Sun Exposure",
                          "    NoWind,                  !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:Ceiling,          !- Name",
                          "    ROOF,                 !- Surface Type",
                          "    ROOF,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  SurfaceProperty:LocalEnvironment,",
                          "    LocEnv:Living:North,          !- Name",
                          "    Living:North,                 !- Exterior Surface Name",
                          "    ,                             !- External Shading Fraction Schedule Name",
                          "    ,                             !- Surrounding Surfaces Object Name",
                          "    OutdoorAirNode:0001;          !- Outdoor Air Node Name",

                          "  OutdoorAir:Node,",
                          "    OutdoorAirNode:0001,          !- Name",
                          "    ,                             !- Height Above Ground",
                          "    OutdoorAirNodeDryBulb:0001,   !- Drybulb Temperature Schedule Name",
                          "    OutdoorAirNodeWetBulb:0001,   !- Wetbulb Schedule Name",
                          "    OutdoorAirNodeWindSpeed:0001, !- Wind Speed Schedule Name",
                          "    OutdoorAirNodeWindDir:0001;   !- Wind Direction Schedule Name",

                          "  ScheduleTypeLimits,",
                          "    Any Number;                   !- Name",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeDryBulb:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 15.0;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWetBulb:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 12.0;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWindSpeed:0001, !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 1.23;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWindDir:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 90;             !- Field 3"});

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    ScheduleManager::ProcessScheduleInput(*state);

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);
    EXPECT_FALSE(has_err_output(true));

    EXPECT_TRUE(state->dataGlobal->AnyLocalEnvironmentsInModel);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "LIVING ZONE";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);
    state->dataHeatBal->Zone(1).IsControlled = true;

    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(4) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(5) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(6) = 0.5;
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);

    // Set up
    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);

    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    OutAirNodeManager::GetOutAirNodesInput(*state);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 25.0;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 20.0;
    state->dataScheduleMgr->Schedule(3).CurrentValue = 1.5;
    state->dataScheduleMgr->Schedule(4).CurrentValue = 90.0;
    for (int loop = 1; loop <= state->dataSurface->TotSurfaces; ++loop) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(loop) = 20.0;
    }
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::ZoneSupplyAirTemp;

    OutAirNodeManager::InitOutAirNodes(*state);

    // Test if local nodes data correctly overwritten
    EXPECT_EQ(25.0, state->dataLoopNodes->Node(1).OutAirDryBulb);
    EXPECT_EQ(20.0, state->dataLoopNodes->Node(1).OutAirWetBulb);
    EXPECT_EQ(1.5, state->dataLoopNodes->Node(1).OutAirWindSpeed);
    EXPECT_EQ(90.0, state->dataLoopNodes->Node(1).OutAirWindDir);
    EXPECT_DOUBLE_EQ(0.012611481326656135, state->dataLoopNodes->Node(1).HumRat);
    EXPECT_DOUBLE_EQ(57247.660939392081, state->dataLoopNodes->Node(1).Enthalpy);

    InitSurfaceHeatBalance(*state);

    // Test if local value correctly overwritten
    EXPECT_EQ(25.0, state->dataSurface->SurfOutDryBulbTemp(1));
    EXPECT_EQ(20.0, state->dataSurface->SurfOutWetBulbTemp(1));
    EXPECT_EQ(1.5, state->dataSurface->SurfOutWindSpeed(1));
    EXPECT_EQ(90.0, state->dataSurface->SurfOutWindDir(1));

    // Test if local value used in surface hc calculation
    // Surface(1) - local; Surface(2) - global;
    for (int SurfNum = 1; SurfNum <= 6; SurfNum++) {
        state->dataSurface->SurfExtConvCoeffIndex(SurfNum) = -1;
    }
    CalcHeatBalanceOutsideSurf(*state);
    Real64 HExt_Expect_Surf1 = ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff(DataSurfaces::SurfaceRoughness::Smooth, 1.5);
    Real64 HExt_Expect_Surf2 = ConvectionCoefficients::CalcASHRAESimpExtConvectCoeff(DataSurfaces::SurfaceRoughness::Smooth, 0.0);
    EXPECT_EQ(HExt_Expect_Surf1, state->dataHeatBalSurf->SurfHcExt(1));
    EXPECT_EQ(HExt_Expect_Surf2, state->dataHeatBalSurf->SurfHcExt(2));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfPropertySrdSurfLWR)
{

    std::string const idf_objects = delimited_string({
        "  Building,",
        "    House with Local Air Nodes,  !- Name",
        "    0,                       !- North Axis {deg}",
        "    Suburbs,                 !- Terrain",
        "    0.001,                   !- Loads Convergence Tolerance Value",
        "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullInteriorAndExterior, !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "  Timestep,6;",

        "  SurfaceConvectionAlgorithm:Inside,TARP;",

        "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

        "  HeatBalanceAlgorithm,ConductionTransferFunction;",

        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    Yes;                     !- Run Simulation for Weather File Run Periods",

        "  RunPeriod,",
        "    WinterDay,               !- Name",
        "    1,                       !- Begin Month",
        "    14,                      !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    1,                       !- End Month",
        "    14,                      !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    Yes,                     !- Use Weather File Holidays and Special Days",
        "    Yes,                     !- Use Weather File Daylight Saving Period",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    Yes;                     !- Use Weather File Snow Indicators",

        "  RunPeriod,",
        "    SummerDay,               !- Name",
        "    7,                       !- Begin Month",
        "    7,                       !- Begin Day of Month",
        "    ,                        !- Begin Year",
        "    7,                       !- End Month",
        "    7,                       !- End Day of Month",
        "    ,                        !- End Year",
        "    Tuesday,                 !- Day of Week for Start Day",
        "    No,                      !- Apply Weekend Holiday Rule",
        "    Yes,                     !- Use Weather File Rain Indicators",
        "    No;                      !- Use Weather File Snow Indicators",

        "  Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "  SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.9200000,               !- Solar Absorptance",
        "    0.9200000;               !- Visible Absorptance",

        "  Material,",
        "    CB11,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2032000,               !- Thickness {m}",
        "    1.048000,                !- Conductivity {W/m-K}",
        "    1105.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material,",
        "    GP01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1600000,               !- Conductivity {W/m-K}",
        "    801.0000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    9.0099998E-02,           !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    IN05,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2458000,               !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Material,",
        "    PW03,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    545.0000,                !- Density {kg/m3}",
        "    1213.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "  Material,",
        "    CC03,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1016000,               !- Thickness {m}",
        "    1.310000,                !- Conductivity {W/m-K}",
        "    2243.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "  Material,",
        "    HF-A3,                   !- Name",
        "    Smooth,                  !- Roughness",
        "    1.5000000E-03,           !- Thickness {m}",
        "    44.96960,                !- Conductivity {W/m-K}",
        "    7689.000,                !- Density {kg/m3}",
        "    418.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.2000000,               !- Solar Absorptance",
        "    0.2000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    AR02,                    !- Name",
        "    VeryRough,               !- Roughness",
        "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "  Material:NoMass,",
        "    CP02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.2170000,               !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "  Construction,",
        "    EXTWALL:LIVING,          !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    GP01;                    !- Layer 3",

        "  Construction,",
        "    FLOOR:LIVING,            !- Name",
        "    CC03,                    !- Outside Layer",
        "    CP02;                    !- Layer 2",

        "  Construction,",
        "    ROOF,                    !- Name",
        "    AR02,                    !- Outside Layer",
        "    PW03;                    !- Layer 2",

        "  Zone,",
        "    LIVING ZONE,             !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    World;                   !- Coordinate System",

        "  BuildingSurface:Detailed,",
        "    Living:North,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:East,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:South,            !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:West,             !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL:LIVING,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5000000,               !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Floor,            !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR:LIVING,            !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Living:Floor,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Living:Ceiling,          !- Name",
        "    ROOF,                 !- Surface Type",
        "    ROOF,          !- Construction Name",
        "    LIVING ZONE,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0,                       !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
        "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
        "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

        "  SurfaceProperty:LocalEnvironment,",
        "    LocEnv:Living:North,          !- Name",
        "    Living:North,                 !- Exterior Surface Name",
        "    ,                             !- External Shading Fraction Schedule Name",
        "    SrdSurfs:Living:North,        !- Surrounding Surfaces Object Name",
        "    ;                             !- Outdoor Air Node Name",

        "  SurfaceProperty:LocalEnvironment,",
        "    LocEnv:Living:East,           !- Name",
        "    Living:East,                  !- Exterior Surface Name",
        "    ,                             !- External Shading Fraction Schedule Name",
        "    SrdSurfs:Living:East,         !- Surrounding Surfaces Object Name",
        "    ;                             !- Outdoor Air Node Name",

        "  SurfaceProperty:LocalEnvironment,",
        "    LocEnv:Living:South,          !- Name",
        "    Living:South,                 !- Exterior Surface Name",
        "    ,                             !- External Shading Fraction Schedule Name",
        "    SrdSurfs:Living:South,        !- Surrounding Surfaces Object Name",
        "    ;                             !- Outdoor Air Node Name",

        "  SurfaceProperty:SurroundingSurfaces,",
        "    SrdSurfs:Living:North, !- Name",
        "    0.3,",
        "    Sky Temp Sch,",
        "    0.1,",
        "    Ground Temp Sch,",
        "    SurroundingSurface1,",
        "    0.6,",
        "    Surrounding Temp Sch 1;",

        "  SurfaceProperty:SurroundingSurfaces,",
        "    SrdSurfs:Living:East, !- Name",
        "    0.2,",
        "    ,",
        "    ,",
        "    ,",
        "    SurroundingSurface1,",
        "    0.3,",
        "    Surrounding Temp Sch 1,",
        "    SurroundingSurface2,",
        "    0.3,",
        "    Surrounding Temp Sch 1;",

        "  SurfaceProperty:SurroundingSurfaces,",
        "    SrdSurfs:Living:South, !- Name",
        "    ,",
        "    ,",
        "    ,",
        "    ,",
        "    SurroundingSurface1,",
        "    0.5,",
        "    Surrounding Temp Sch 1;",

        "  ScheduleTypeLimits,",
        "    Any Number;                   !- Name",

        "  Schedule:Compact,",
        "    Surrounding Temp Sch 1,       !- Name",
        "    Any Number,                   !- Schedule Type Limits Name",
        "    Through: 12/31,               !- Field 1",
        "    For: AllDays,                 !- Field 2",
        "    Until: 24:00, 15.0;           !- Field 3",

        "  Schedule:Compact,",
        "    Sky Temp Sch,                 !- Name",
        "    Any Number,                   !- Schedule Type Limits Name",
        "    Through: 12/31,               !- Field 1",
        "    For: AllDays,                 !- Field 2",
        "    Until: 24:00, 15.0;           !- Field 3",

        "  Schedule:Compact,",
        "    Ground Temp Sch,              !- Name",
        "    Any Number,                   !- Schedule Type Limits Name",
        "    Through: 12/31,               !- Field 1",
        "    For: AllDays,                 !- Field 2",
        "    Until: 24:00, 15.0;           !- Field 3",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    ScheduleManager::ProcessScheduleInput(*state);

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);
    EXPECT_FALSE(has_err_output(true));

    EXPECT_TRUE(state->dataGlobal->AnyLocalEnvironmentsInModel);

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "LIVING ZONE";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);
    state->dataHeatBal->Zone(1).IsControlled = true;

    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(4) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(5) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(6) = 0.5;
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);

    // Set up
    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);

    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);

    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::ZoneSupplyAirTemp;

    InitSurfaceHeatBalance(*state);

    state->dataSurface->SurfAirSkyRadSplit.allocate(6);
    state->dataScheduleMgr->Schedule(1).CurrentValue = 25.0; // Srd Srfs Temp
    state->dataScheduleMgr->Schedule(2).CurrentValue = 15.0; // Sky temp
    state->dataScheduleMgr->Schedule(3).CurrentValue = 22.0; // Grd temp

    for (int SurfNum = 1; SurfNum <= 6; SurfNum++) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(SurfNum) = 20; // Surf temp
        state->dataSurface->SurfOutDryBulbTemp(SurfNum) = 22;         // Air temp
        state->dataSurface->SurfExtConvCoeffIndex(SurfNum) = -6;
        state->dataSurface->SurfAirSkyRadSplit(SurfNum) = 1.0;
    }
    CalcHeatBalanceOutsideSurf(*state);

    // Test if local value correctly overwritten
    // Surface(1-3) - local; Surface(4-6) - global;
    EXPECT_DOUBLE_EQ(0.3, state->dataSurface->Surface(1).ViewFactorSkyIR);
    EXPECT_DOUBLE_EQ(0.1, state->dataSurface->Surface(1).ViewFactorGroundIR);
    EXPECT_DOUBLE_EQ(0.2, state->dataSurface->Surface(2).ViewFactorSkyIR);
    EXPECT_DOUBLE_EQ(0.2, state->dataSurface->Surface(2).ViewFactorGroundIR);
    EXPECT_DOUBLE_EQ(0.25, state->dataSurface->Surface(3).ViewFactorSkyIR);
    EXPECT_DOUBLE_EQ(0.25, state->dataSurface->Surface(3).ViewFactorGroundIR);
    // Test if sky and grd view factor and temperature correctly overwritten
    EXPECT_DOUBLE_EQ((DataGlobalConstants::StefanBoltzmann * 0.9 * 0.3 *
                      (pow_4(20.0 + DataGlobalConstants::KelvinConv) - pow_4(15.0 + DataGlobalConstants::KelvinConv)) / (20.0 - 15.0)),
                     state->dataHeatBalSurf->SurfHSkyExt(1));
    EXPECT_DOUBLE_EQ((DataGlobalConstants::StefanBoltzmann * 0.9 * 0.1 *
                      (pow_4(20.0 + DataGlobalConstants::KelvinConv) - pow_4(22.0 + DataGlobalConstants::KelvinConv)) / (20.0 - 22.0)),
                     state->dataHeatBalSurf->SurfHGrdExt(1));

    // Test if LWR from surrounding surfaces correctly calculated
    EXPECT_DOUBLE_EQ(DataGlobalConstants::StefanBoltzmann * 0.9 * 0.6 *
                         (pow_4(25.0 + DataGlobalConstants::KelvinConv) - pow_4(20.0 + DataGlobalConstants::KelvinConv)),
                     state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(1));
    EXPECT_DOUBLE_EQ(DataGlobalConstants::StefanBoltzmann * 0.9 *
                         (0.3 * (pow_4(25.0 + DataGlobalConstants::KelvinConv) - pow_4(20.0 + DataGlobalConstants::KelvinConv)) +
                          0.3 * (pow_4(25.0 + DataGlobalConstants::KelvinConv) - pow_4(20.0 + DataGlobalConstants::KelvinConv))),
                     state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(2));
    EXPECT_DOUBLE_EQ(DataGlobalConstants::StefanBoltzmann * 0.9 * 0.5 *
                         (pow_4(25.0 + DataGlobalConstants::KelvinConv) - pow_4(20.0 + DataGlobalConstants::KelvinConv)),
                     state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(3));
    EXPECT_DOUBLE_EQ(0.0, state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(4));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_SurfaceCOnstructionIndexTest)
{

    std::string const idf_objects = delimited_string({
        " Output:Variable,Perimeter_ZN_1_wall_south_Window_1,Surface Window Transmitted Solar Radiation Rate,timestep;",
        " Output:Variable,*,SURFACE CONSTRUCTION INDEX,timestep;",
        " Output:Diagnostics, DisplayAdvancedReportVariables;",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataGlobal->DisplayAdvancedReportVariables = true;

    state->dataSurface->TotSurfaces = 1;
    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->TotConstructs = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->SurfaceWindow.allocate(state->dataSurface->TotSurfaces);
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);
    state->dataHeatBal->AnyInternalHeatSourceInInput = true;

    state->dataSurface->Surface(1).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::CTF;
    state->dataSurface->Surface(1).ExtBoundCond = 1;
    state->dataSurface->Surface(1).Construction = 1;

    state->dataConstruction->Construct(1).NumCTFTerms = 2;
    state->dataConstruction->Construct(1).SourceSinkPresent = true;
    state->dataConstruction->Construct(1).NumHistories = 1;
    state->dataConstruction->Construct(1).CTFTUserOut(0) = 0.5;
    state->dataConstruction->Construct(1).CTFTUserIn(0) = 0.25;
    state->dataConstruction->Construct(1).CTFTUserSource(0) = 0.25;
    SurfaceGeometry::AllocateSurfaceArrays(*state);
    AllocateSurfaceHeatBalArrays(*state); // allocates a host of variables related to CTF calculations
    OutputProcessor::GetReportVariableInput(*state);

    EXPECT_EQ(state->dataOutputProcessor->ReqRepVars(2).VarName, "SURFACE CONSTRUCTION INDEX");
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestSurfTempCalcHeatBalanceAddSourceTerm)
{

    std::string const idf_objects =
        delimited_string({"  Building,",
                          "    House with AirflowNetwork simulation,  !- Name",
                          "    0,                       !- North Axis {deg}",
                          "    Suburbs,                 !- Terrain",
                          "    0.001,                   !- Loads Convergence Tolerance Value",
                          "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
                          "    FullInteriorAndExterior, !- Solar Distribution",
                          "    25,                      !- Maximum Number of Warmup Days",
                          "    6;                       !- Minimum Number of Warmup Days",

                          "  Timestep,6;",

                          "  SurfaceConvectionAlgorithm:Inside,TARP;",

                          "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

                          "  HeatBalanceAlgorithm,ConductionTransferFunction;",

                          "  SimulationControl,",
                          "    No,                      !- Do Zone Sizing Calculation",
                          "    No,                      !- Do System Sizing Calculation",
                          "    No,                      !- Do Plant Sizing Calculation",
                          "    Yes,                     !- Run Simulation for Sizing Periods",
                          "    No;                      !- Run Simulation for Weather File Run Periods",

                          "  RunPeriod,",
                          "    WinterDay,               !- Name",
                          "    1,                       !- Begin Month",
                          "    14,                      !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    1,                       !- End Month",
                          "    14,                      !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    Yes;                     !- Use Weather File Snow Indicators",

                          "  RunPeriod,",
                          "    SummerDay,               !- Name",
                          "    7,                       !- Begin Month",
                          "    7,                       !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    7,                       !- End Month",
                          "    7,                       !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    No;                      !- Use Weather File Snow Indicators",

                          "  Site:Location,",
                          "    CHICAGO_IL_USA TMY2-94846,  !- Name",
                          "    41.78,                   !- Latitude {deg}",
                          "    -87.75,                  !- Longitude {deg}",
                          "    -6.00,                   !- Time Zone {hr}",
                          "    190.00;                  !- Elevation {m}",

                          "  SizingPeriod:DesignDay,",
                          "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
                          "    1,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    WinterDesignDay,         !- Day Type",
                          "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
                          "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    99063.,                  !- Barometric Pressure {Pa}",
                          "    4.9,                     !- Wind Speed {m/s}",
                          "    270,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    0.0;                     !- Sky Clearness",

                          "  SizingPeriod:DesignDay,",
                          "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
                          "    7,                       !- Month",
                          "    21,                      !- Day of Month",
                          "    SummerDesignDay,         !- Day Type",
                          "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
                          "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
                          "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
                          "    Wetbulb,                 !- Humidity Condition Type",
                          "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
                          "    ,                        !- Humidity Condition Day Schedule Name",
                          "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
                          "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
                          "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
                          "    99063.,                  !- Barometric Pressure {Pa}",
                          "    5.3,                     !- Wind Speed {m/s}",
                          "    230,                     !- Wind Direction {deg}",
                          "    No,                      !- Rain Indicator",
                          "    No,                      !- Snow Indicator",
                          "    No,                      !- Daylight Saving Time Indicator",
                          "    ASHRAEClearSky,          !- Solar Model Indicator",
                          "    ,                        !- Beam Solar Day Schedule Name",
                          "    ,                        !- Diffuse Solar Day Schedule Name",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
                          "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
                          "    1.0;                     !- Sky Clearness",

                          "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

                          "  Material,",
                          "    A1 - 1 IN STUCCO,        !- Name",
                          "    Smooth,                  !- Roughness",
                          "    2.5389841E-02,           !- Thickness {m}",
                          "    0.6918309,               !- Conductivity {W/m-K}",
                          "    1858.142,                !- Density {kg/m3}",
                          "    836.8000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.9200000,               !- Solar Absorptance",
                          "    0.9200000;               !- Visible Absorptance",

                          "  Material,",
                          "    CB11,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.2032000,               !- Thickness {m}",
                          "    1.048000,                !- Conductivity {W/m-K}",
                          "    1105.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material,",
                          "    GP01,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1600000,               !- Conductivity {W/m-K}",
                          "    801.0000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    9.0099998E-02,           !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN05,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2458000,               !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    PW03,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1150000,               !- Conductivity {W/m-K}",
                          "    545.0000,                !- Density {kg/m3}",
                          "    1213.000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7800000,               !- Solar Absorptance",
                          "    0.7800000;               !- Visible Absorptance",

                          "  Material,",
                          "    CC03,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.1016000,               !- Thickness {m}",
                          "    1.310000,                !- Conductivity {W/m-K}",
                          "    2243.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.6500000,               !- Solar Absorptance",
                          "    0.6500000;               !- Visible Absorptance",

                          "  Material,",
                          "    HF-A3,                   !- Name",
                          "    Smooth,                  !- Roughness",
                          "    1.5000000E-03,           !- Thickness {m}",
                          "    44.96960,                !- Conductivity {W/m-K}",
                          "    7689.000,                !- Density {kg/m3}",
                          "    418.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    AR02,                    !- Name",
                          "    VeryRough,               !- Roughness",
                          "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7000000,               !- Solar Absorptance",
                          "    0.7000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    CP02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2170000,               !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Construction,",
                          "    EXTWALL:LIVING,          !- Name",
                          "    A1 - 1 IN STUCCO,        !- Outside Layer",
                          "    GP01;                    !- Layer 3",

                          "  Construction,",
                          "    FLOOR:LIVING,            !- Name",
                          "    CC03,                    !- Outside Layer",
                          "    CP02;                    !- Layer 2",

                          "  Construction,",
                          "    ROOF,                    !- Name",
                          "    AR02,                    !- Outside Layer",
                          "    PW03;                    !- Layer 2",

                          "  Zone,",
                          "    LIVING ZONE,             !- Name",
                          "    0,                       !- Direction of Relative North {deg}",
                          "    0,                       !- X Origin {m}",
                          "    0,                       !- Y Origin {m}",
                          "    0,                       !- Z Origin {m}",
                          "    1,                       !- Type",
                          "    1,                       !- Multiplier",
                          "    autocalculate,           !- Ceiling Height {m}",
                          "    autocalculate;           !- Volume {m3}",

                          "  GlobalGeometryRules,",
                          "    UpperLeftCorner,         !- Starting Vertex Position",
                          "    CounterClockWise,        !- Vertex Entry Direction",
                          "    World;                   !- Coordinate System",

                          "  BuildingSurface:Detailed,",
                          "    Living:North,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:East,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:South,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:West,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",
                          "  BuildingSurface:Detailed,",
                          "    Living:Floor,            !- Name",
                          "    FLOOR,                   !- Surface Type",
                          "    FLOOR:LIVING,            !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Surface,                 !- Outside Boundary Condition",
                          "    Living:Floor,            !- Outside Boundary Condition Object",
                          "    NoSun,                   !- Sun Exposure",
                          "    NoWind,                  !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:Ceiling,          !- Name",
                          "    ROOF,                 !- Surface Type",
                          "    ROOF,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    ,                        !- Space Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",
                          "",
                          "  Schedule:Compact,",
                          "    Sche_Q_Evap_Cool,        !- Name",
                          "	 Any Number,              !- Schedule Type Limits Name",
                          "    Through: 12/31,",
                          "    For: AllDays,",
                          "    Until: 24:00, -0.1;",
                          "",
                          "  Schedule:Compact,",
                          "    Sche_Q_Add_Heat,         !- Name",
                          "	 Any Number,              !- Schedule Type Limits Name",
                          "    Through: 12/31,",
                          "    For: AllDays,",
                          "    Until: 24:00, 0.1;",
                          "",
                          "  SurfaceProperty:HeatBalanceSourceTerm,",
                          "    Living:North,               !- Surface Name",
                          "	 ,                           !- Inside Face Heat Source Term Schedule Name",
                          "    Sche_Q_Evap_Cool;           !- Outside Face Heat Source Term Schedule Name",
                          "",
                          "  SurfaceProperty:HeatBalanceSourceTerm,",
                          "    Living:Ceiling,             !- Surface Name",
                          "	 Sche_Q_Add_Heat,            !- Inside Face Heat Source Term Schedule Name",
                          "    ;                           !- Outside Face Heat Source Term Schedule Name"});

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Clear schedule type warnings
    EXPECT_TRUE(has_err_output(true));

    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);
    EXPECT_TRUE(compare_err_stream(""));
    EXPECT_FALSE(has_err_output(true));

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "LIVING ZONE";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(1);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    // allocate surface level adj ratio data member
    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataHeatBalSurf->SurfHConvInt(1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(3) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(4) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(5) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(6) = 0.5;
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);

    state->dataScheduleMgr->Schedule(1).CurrentValue = -0.1;
    state->dataScheduleMgr->Schedule(2).CurrentValue = 0.1;

    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(6, 1.0);
    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
    for (int loop = 1; loop <= state->dataSurface->TotSurfaces; ++loop) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(loop) = 20.0;
    }
    state->dataSurface->SurfTAirRef(1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(2) = DataSurfaces::AdjacentAirTemp;
    state->dataSurface->SurfTAirRef(3) = DataSurfaces::ZoneSupplyAirTemp;

    InitSurfaceHeatBalance(*state);
    for (int SurfNum = 1; SurfNum <= state->dataSurface->TotSurfaces; SurfNum++) {
        state->dataSurface->SurfExtConvCoeffIndex(SurfNum) = -1;
    }

    // Test Additional Heat Source Calculation
    CalcHeatBalanceOutsideSurf(*state);
    EXPECT_EQ(-0.1, state->dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(1));
    CalcHeatBalanceInsideSurf(*state);
    EXPECT_EQ(0.1, state->dataHeatBalSurf->SurfQAdditionalHeatSourceInside(6));

    state->dataZoneEquip->ZoneEquipConfig.deallocate();
    state->dataSize->ZoneEqSizing.deallocate();
    state->dataHeatBalFanSys->MAT.deallocate(); // Zone temperature C
    state->dataHeatBalFanSys->ZoneAirHumRat.deallocate();
    state->dataLoopNodes->Node.deallocate();
    state->dataGlobal->KickOffSimulation = false;
    state->dataHeatBalSurf->SurfTempInTmp.deallocate();
    state->dataHeatBal->SurfTempEffBulkAir.deallocate();
    state->dataHeatBalSurf->SurfHConvInt.deallocate();
    state->dataMstBal->HConvInFD.deallocate();
    state->dataMstBal->RhoVaporAirIn.deallocate();
    state->dataMstBal->HMassConvInFD.deallocate();
    state->dataHeatBalFanSys->ZoneLatentGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRep.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.deallocate();
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestReportIntMovInsInsideSurfTemp)
{

    Real64 ExpectedResult1;
    Real64 ExpectedResult2;

    state->dataSurface->TotSurfaces = 2;
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBalSurf->SurfTempIn.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBalSurf->SurfTempInTmp.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBalSurf->SurfTempInMovInsRep.allocate(state->dataSurface->TotSurfaces);
    state->dataHeatBalSurf->SurfMovInsulIntPresent.allocate(state->dataSurface->TotSurfaces);
    state->dataSurface->AnyMovableInsulation = true;
    state->dataHeatBalSurf->SurfMovInsulIndexList.push_back(1);
    state->dataHeatBalSurf->SurfMovInsulIndexList.push_back(2);
    // Test 1 Data: Surface does NOT have movable insulation
    state->dataHeatBalSurf->SurfMovInsulIntPresent(1) = false; // No movable insulation
    state->dataHeatBalSurf->SurfTempIn(1) = 23.0;
    state->dataHeatBalSurf->SurfTempInTmp(1) = 12.3;
    state->dataHeatBalSurf->SurfTempInMovInsRep(1) = 1.23;
    ExpectedResult1 = 23.0; // SurfTempInMovInsRep should be set to SurfTempIn

    // Test 2 Data: Surface has movable insulation
    state->dataHeatBalSurf->SurfMovInsulIntPresent(2) = true;
    state->dataHeatBalSurf->SurfTempIn(2) = 123.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 12.3;
    state->dataHeatBalSurf->SurfTempInMovInsRep(2) = 1.23;
    ExpectedResult2 = 12.3; // SurfTempInMovInsRep should be set to SurfTempIn

    // Now call the subroutine which will run all of the test cases at once and then make the comparisons
    HeatBalanceSurfaceManager::ReportIntMovInsInsideSurfTemp(*state);
    EXPECT_NEAR(state->dataHeatBalSurf->SurfTempInMovInsRep(1), ExpectedResult1, 0.00001);
    EXPECT_NEAR(state->dataHeatBalSurf->SurfTempInMovInsRep(2), ExpectedResult2, 0.00001);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_OutsideSurfHeatBalanceWhenRainFlag)
{
    state->dataSurface->Surface.allocate(1);
    state->dataSurface->SurfOutWetBulbTemp.allocate(1);
    state->dataSurface->SurfOutDryBulbTemp.allocate(1);
    state->dataHeatBalSurf->SurfHcExt.allocate(1);
    state->dataHeatBalSurf->SurfOutsideTempHist.allocate(1);
    state->dataHeatBalSurf->SurfOutsideTempHist(1).allocate(1);

    state->dataSurface->Surface(1).Area = 58.197;
    state->dataHeatBalSurf->SurfHcExt(1) = 1000;
    state->dataHeatBalSurf->SurfOutsideTempHist(1)(1) = 6.71793958923051;
    state->dataSurface->SurfOutWetBulbTemp(1) = 6.66143784594778;
    state->dataSurface->SurfOutDryBulbTemp(1) = 7.2;

    // If Rain Flag = on, GetSurfQdotConvOutRep uses Outdoor Air Wet Bulb Temp.
    state->dataEnvrn->IsRain = true;
    Real64 ExpectedQconvPerArea1 = -1000 * (6.71793958923051 - 6.66143784594778);

    EXPECT_NEAR(ExpectedQconvPerArea1, GetQdotConvOutPerArea(*state, 1), 0.01);

    // Otherwise, GetSurfQdotConvOutRep uses Outdoor Air Dry Bulb Temp.
    state->dataEnvrn->IsRain = false;
    state->dataHeatBalSurf->SurfHcExt(1) = 5.65361106051348;
    Real64 ExpectedQconvPerArea2 = -5.65361106051348 * (6.71793958923051 - 7.2);

    EXPECT_NEAR(ExpectedQconvPerArea2, GetQdotConvOutPerArea(*state, 1), 0.01);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestInterzoneRadFactorCalc)
{

    state->dataSurface->TotSurfaces = 2;
    state->dataGlobal->NumOfZones = 2;
    state->dataHeatBal->TotMaterials = 1;
    state->dataHeatBal->TotConstructs = 1;
    state->dataViewFactor->NumOfSolarEnclosures = 2;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);
    state->dataHeatBal->EnclSolVMULT.allocate(state->dataViewFactor->NumOfSolarEnclosures);
    state->dataViewFactor->EnclSolInfo.allocate(state->dataViewFactor->NumOfSolarEnclosures);
    state->dataConstruction->Construct(1).TransDiff = 0.1;
    state->dataHeatBal->EnclSolVMULT(1) = 1.0;
    state->dataHeatBal->EnclSolVMULT(2) = 1.0;

    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).Construction = 1;
    state->dataSurface->Surface(1).ExtBoundCond = 2;
    state->dataSurface->Surface(1).Area = 1.0;
    state->dataSurface->Surface(1).Zone = 1;

    state->dataSurface->Surface(2).HeatTransSurf = true;
    state->dataSurface->Surface(2).Construction = 1;
    state->dataSurface->Surface(2).ExtBoundCond = 1;
    state->dataSurface->Surface(2).Area = 1.0;
    state->dataSurface->Surface(2).Zone = 2;

    state->dataSurface->Surface(1).SolarEnclIndex = 1;
    state->dataSurface->Surface(2).SolarEnclIndex = 2;

    ComputeDifSolExcZonesWIZWindows(*state, state->dataGlobal->NumOfZones);

    EXPECT_EQ(1, state->dataHeatBalSurf->ZoneFractDifShortZtoZ(1, 1));
    EXPECT_EQ(1, state->dataHeatBalSurf->ZoneFractDifShortZtoZ(2, 2));
    EXPECT_FALSE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(1));
    EXPECT_FALSE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(2));

    state->dataViewFactor->EnclSolInfo(1).HasInterZoneWindow = true;
    state->dataViewFactor->EnclSolInfo(2).HasInterZoneWindow = true;

    ComputeDifSolExcZonesWIZWindows(*state, state->dataGlobal->NumOfZones);

    EXPECT_TRUE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(1));
    EXPECT_TRUE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(2));

    state->dataGlobal->KickOffSimulation = true;
    ComputeDifSolExcZonesWIZWindows(*state, state->dataGlobal->NumOfZones);

    EXPECT_EQ(1, state->dataHeatBalSurf->ZoneFractDifShortZtoZ(1, 1));
    EXPECT_EQ(1, state->dataHeatBalSurf->ZoneFractDifShortZtoZ(2, 2));
    EXPECT_FALSE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(1));
    EXPECT_FALSE(state->dataHeatBalSurf->EnclSolRecDifShortFromZ(2));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestResilienceMetricReport)
{

    state->dataGlobal->NumOfZones = 1;
    state->dataGlobal->KindOfSim = DataGlobalConstants::KindOfSim::RunPeriodWeather;
    state->dataOutRptTab->displayThermalResilienceSummary = true;
    state->dataEnvrn->Month = 7;
    state->dataEnvrn->DayOfMonth = 1;

    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->TimeStepZone = 1;
    state->dataEnvrn->OutBaroPress = 101325.0;

    state->dataGlobal->NumOfZones = 1;
    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZTAV.dimension(state->dataGlobal->NumOfZones, 0.0);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.dimension(state->dataGlobal->NumOfZones, 0.0);

    state->dataHeatBalFanSys->ZoneHeatIndex.dimension(state->dataGlobal->NumOfZones, 0.0);
    state->dataHeatBalFanSys->ZoneHumidex.dimension(state->dataGlobal->NumOfZones, 0.0);
    state->dataHeatBalFanSys->ZoneNumOcc.dimension(state->dataGlobal->NumOfZones, 0);
    state->dataHeatBalFanSys->ZoneHeatIndexHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneHumidexHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneHeatIndexOccuHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneHumidexOccuHourBins.allocate(state->dataGlobal->NumOfZones);

    state->dataHeatBal->TotPeople = 1;
    state->dataHeatBal->People.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBal->People(1).ZonePtr = 1;
    state->dataHeatBal->People(1).Pierce = true;
    state->dataHeatBal->People(1).NumberOfPeople = 2;
    state->dataHeatBal->People(1).NumberOfPeoplePtr = 1;
    state->dataScheduleMgr->Schedule.allocate(1);

    state->dataThermalComforts->ThermalComfortData.allocate(state->dataHeatBal->TotPeople);
    state->dataHeatBalFanSys->ZoneOccPierceSET.dimension(state->dataGlobal->NumOfZones, 0);
    state->dataHeatBalFanSys->ZoneOccPierceSETLastStep.dimension(state->dataGlobal->NumOfZones, 0);
    state->dataHeatBalFanSys->ZoneLowSETHours.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneHighSETHours.allocate(state->dataGlobal->NumOfZones);

    state->dataThermalComforts->ThermalComfortData(1).PierceSET = 31;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 0;

    // Heat Index Case 1: Zone T < 80 F;
    state->dataGlobal->HourOfDay = 1;
    state->dataHeatBalFanSys->ZTAV(1) = 25;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.00988; // RH = 50%
    CalcThermalResilience(*state);
    ReportThermalResilience(*state);
    EXPECT_NEAR(25, state->dataHeatBalFanSys->ZoneHeatIndex(1), 0.5);
    EXPECT_NEAR(28, state->dataHeatBalFanSys->ZoneHumidex(1), 1);

    // Heat Index Case 2: Zone RH > 85, 80 < T < 87 F;
    state->dataGlobal->HourOfDay = 2;
    state->dataHeatBalFanSys->ZTAV(1) = 27;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.02035; // RH = 90%
    CalcThermalResilience(*state);
    ReportThermalResilience(*state);
    EXPECT_NEAR(31, state->dataHeatBalFanSys->ZoneHeatIndex(1), 0.5);
    EXPECT_NEAR(39, state->dataHeatBalFanSys->ZoneHumidex(1), 1);

    // Heat Index Case 3: < Zone RH > 85, 80 < T < 87 F;
    state->dataGlobal->HourOfDay = 3;
    state->dataHeatBalFanSys->ZTAV(1) = 27;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.0022; // RH = 10%
    CalcThermalResilience(*state);
    ReportThermalResilience(*state);
    EXPECT_NEAR(26, state->dataHeatBalFanSys->ZoneHeatIndex(1), 0.5);
    EXPECT_NEAR(23, state->dataHeatBalFanSys->ZoneHumidex(1), 1);

    // Heat Index Case 4: Rothfusz regression, other than the above conditions;
    state->dataGlobal->HourOfDay = 4;
    state->dataHeatBalFanSys->ZTAV(1) = 30;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.01604; // RH = 60%
    CalcThermalResilience(*state);
    ReportThermalResilience(*state);
    EXPECT_NEAR(33, state->dataHeatBalFanSys->ZoneHeatIndex(1), 0.5);
    EXPECT_NEAR(38, state->dataHeatBalFanSys->ZoneHumidex(1), 1);

    // Test categorization of the first 4 hours.
    EXPECT_EQ(2, state->dataHeatBalFanSys->ZoneHeatIndexHourBins(1)[0]); // Safe: Heat Index <= 80 °F (32.2 °C).
    EXPECT_EQ(1, state->dataHeatBalFanSys->ZoneHeatIndexHourBins(1)[1]); // Caution: (80, 90 °F] / (26.7, 32.2 °C]
    EXPECT_EQ(1, state->dataHeatBalFanSys->ZoneHeatIndexHourBins(1)[2]); // Extreme Caution (90, 105 °F] / (32.2, 40.6 °C]
    EXPECT_EQ(0, state->dataHeatBalFanSys->ZoneHeatIndexHourBins(1)[3]);
    EXPECT_EQ(0, state->dataHeatBalFanSys->ZoneHeatIndexOccuHourBins(1)[0]); // # of People = 0

    EXPECT_EQ(2, state->dataHeatBalFanSys->ZoneHumidexHourBins(1)[0]);     // Humidex <= 29
    EXPECT_EQ(2, state->dataHeatBalFanSys->ZoneHumidexHourBins(1)[1]);     // Humidex (29, 40]
    EXPECT_EQ(0, state->dataHeatBalFanSys->ZoneHumidexOccuHourBins(1)[0]); // # of People = 0

    // Test SET-hours calculation - No occupant
    EXPECT_EQ(0, state->dataHeatBalFanSys->ZoneHighSETHours(1)[0]); // SET Hours
    EXPECT_EQ(0, state->dataHeatBalFanSys->ZoneHighSETHours(1)[1]); // SET OccupantHours

    state->dataThermalComforts->ThermalComfortData(1).PierceSET = 11.2;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    for (int hour = 5; hour <= 7; hour++) {
        state->dataGlobal->HourOfDay = hour;
        //        CalcThermalResilience(*state);
        ReportThermalResilience(*state);
    }
    // Test SET-hours calculation - Heating unmet
    EXPECT_EQ(3, state->dataHeatBalFanSys->ZoneLowSETHours(1)[0]); // SET Hours = (12.2 - 11.2) * 3 Hours
    EXPECT_EQ(6, state->dataHeatBalFanSys->ZoneLowSETHours(1)[1]); // SET OccupantHours = (12.2 - 11.2) * 3 Hours * 2 OCC

    state->dataThermalComforts->ThermalComfortData(1).PierceSET = 32;
    for (int hour = 8; hour <= 10; hour++) {
        state->dataGlobal->HourOfDay = hour;
        ReportThermalResilience(*state);
    }
    // Test SET-hours calculation - Cooling unmet
    EXPECT_EQ(6, state->dataHeatBalFanSys->ZoneHighSETHours(1)[0]);  // SET Hours = (32 - 30) * 3 Hours
    EXPECT_EQ(12, state->dataHeatBalFanSys->ZoneHighSETHours(1)[1]); // SET OccupantHours = (32 - 30) * 3 Hours * 2 OCC

    state->dataThermalComforts->ThermalComfortData(1).PierceSET = 25;
    for (int hour = 11; hour <= 12; hour++) {
        state->dataGlobal->HourOfDay = hour;
        ReportThermalResilience(*state);
    }
    state->dataThermalComforts->ThermalComfortData(1).PierceSET = 11.2;
    for (int hour = 13; hour <= 18; hour++) {
        state->dataGlobal->HourOfDay = hour;
        ReportThermalResilience(*state);
    }
    state->dataScheduleMgr->Schedule(1).CurrentValue = 0;
    for (int hour = 18; hour <= 20; hour++) {
        state->dataGlobal->HourOfDay = hour;
        ReportThermalResilience(*state);
    }

    // Test SET longest duration calculation
    // Cooling Unmet Duration: Hour 1 - 4 (no occupants), Hour 8 - 10;
    // Heating Unmet Duration: Hour 5 - 7, Hour 13 - 18, Hour 18 - 20 (no occupants);
    EXPECT_EQ(9, state->dataHeatBalFanSys->ZoneLowSETHours(1)[0]);  // SET Hours = (12.2 - 11.2) * (3 + 6) Hours
    EXPECT_EQ(6, state->dataHeatBalFanSys->ZoneHighSETHours(1)[0]); // SET Hours = SET Hours = (32 - 30) * 3 Hours
    EXPECT_EQ(6, state->dataHeatBalFanSys->ZoneLowSETHours(1)[2]);  // Longest Heating SET Unmet Duration
    EXPECT_EQ(3, state->dataHeatBalFanSys->ZoneHighSETHours(1)[2]); //  Longest Cooling SET Unmet Duration

    state->dataHeatBalFanSys->ZoneCO2LevelHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneCO2LevelOccuHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataContaminantBalance->ZoneAirCO2Avg.allocate(state->dataGlobal->NumOfZones);
    state->dataContaminantBalance->Contaminant.CO2Simulation = true;
    state->dataScheduleMgr->Schedule(1).CurrentValue = 1;
    state->dataOutRptTab->displayCO2ResilienceSummary = true;
    state->dataContaminantBalance->ZoneAirCO2Avg(1) = 1100;
    ReportCO2Resilience(*state);
    EXPECT_EQ(1, state->dataHeatBalFanSys->ZoneCO2LevelHourBins(1)[1]);
    EXPECT_EQ(2, state->dataHeatBalFanSys->ZoneCO2LevelOccuHourBins(1)[1]);

    state->dataHeatBalFanSys->ZoneLightingLevelHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataHeatBalFanSys->ZoneLightingLevelOccuHourBins.allocate(state->dataGlobal->NumOfZones);
    state->dataDaylightingData->ZoneDaylight.allocate(state->dataGlobal->NumOfZones);
    state->dataDaylightingData->totDaylightingControls = state->dataGlobal->NumOfZones;
    state->dataDaylightingData->daylightControl.allocate(state->dataDaylightingData->totDaylightingControls);
    state->dataDaylightingData->daylightControl(1).DaylightMethod = DataDaylighting::DaylightingMethod::SplitFlux;
    state->dataDaylightingData->daylightControl(1).zoneIndex = 1;
    state->dataDaylightingData->daylightControl(1).TotalDaylRefPoints = 1;
    state->dataDaylightingData->ZoneDaylight(1).totRefPts = 1;
    state->dataDaylightingData->daylightControl(1).DaylIllumAtRefPt.allocate(1);
    state->dataDaylightingData->daylightControl(1).IllumSetPoint.allocate(1);
    state->dataDaylightingData->daylightControl(1).PowerReductionFactor = 0.5;
    state->dataDaylightingData->daylightControl(1).DaylIllumAtRefPt(1) = 300;
    state->dataDaylightingData->daylightControl(1).IllumSetPoint(1) = 400;
    state->dataOutRptTab->displayVisualResilienceSummary = true;

    ReportVisualResilience(*state);
    EXPECT_EQ(1, state->dataHeatBalFanSys->ZoneLightingLevelHourBins(1)[2]);
    EXPECT_EQ(2, state->dataHeatBalFanSys->ZoneLightingLevelOccuHourBins(1)[2]);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestInitHBInterzoneWindow)
{

    std::string const idf_objects = delimited_string({"  Building,",
                                                      "    House with Local Air Nodes,  !- Name",
                                                      "    0,                       !- North Axis {deg}",
                                                      "    Suburbs,                 !- Terrain",
                                                      "    0.001,                   !- Loads Convergence Tolerance Value",
                                                      "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
                                                      "    FullInteriorAndExterior, !- Solar Distribution",
                                                      "    25,                      !- Maximum Number of Warmup Days",
                                                      "    6;                       !- Minimum Number of Warmup Days",

                                                      "  Timestep,6;",

                                                      "  SimulationControl,",
                                                      "    No,                      !- Do Zone Sizing Calculation",
                                                      "    No,                      !- Do System Sizing Calculation",
                                                      "    No,                      !- Do Plant Sizing Calculation",
                                                      "    No,                     !- Run Simulation for Sizing Periods",
                                                      "    YES;                     !- Run Simulation for Weather File Run Periods",

                                                      "  RunPeriod,",
                                                      "    WinterDay,               !- Name",
                                                      "    1,                       !- Begin Month",
                                                      "    14,                      !- Begin Day of Month",
                                                      "    ,                        !- Begin Year",
                                                      "    1,                       !- End Month",
                                                      "    14,                      !- End Day of Month",
                                                      "    ,                        !- End Year",
                                                      "    Tuesday,                 !- Day of Week for Start Day",
                                                      "    Yes,                     !- Use Weather File Holidays and Special Days",
                                                      "    Yes,                     !- Use Weather File Daylight Saving Period",
                                                      "    No,                      !- Apply Weekend Holiday Rule",
                                                      "    Yes,                     !- Use Weather File Rain Indicators",
                                                      "    Yes;                     !- Use Weather File Snow Indicators",

                                                      "  Material,",
                                                      "    A1 - 1 IN STUCCO,        !- Name",
                                                      "    Smooth,                  !- Roughness",
                                                      "    2.5389841E-02,           !- Thickness {m}",
                                                      "    0.6918309,               !- Conductivity {W/m-K}",
                                                      "    1858.142,                !- Density {kg/m3}",
                                                      "    836.8000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.9200000,               !- Solar Absorptance",
                                                      "    0.9200000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    CB11,                    !- Name",
                                                      "    MediumRough,             !- Roughness",
                                                      "    0.2032000,               !- Thickness {m}",
                                                      "    1.048000,                !- Conductivity {W/m-K}",
                                                      "    1105.000,                !- Density {kg/m3}",
                                                      "    837.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.2000000,               !- Solar Absorptance",
                                                      "    0.2000000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    GP01,                    !- Name",
                                                      "    MediumSmooth,            !- Roughness",
                                                      "    1.2700000E-02,           !- Thickness {m}",
                                                      "    0.1600000,               !- Conductivity {W/m-K}",
                                                      "    801.0000,                !- Density {kg/m3}",
                                                      "    837.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7500000,               !- Solar Absorptance",
                                                      "    0.7500000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    IN02,                    !- Name",
                                                      "    Rough,                   !- Roughness",
                                                      "    9.0099998E-02,           !- Thickness {m}",
                                                      "    4.3000001E-02,           !- Conductivity {W/m-K}",
                                                      "    10.00000,                !- Density {kg/m3}",
                                                      "    837.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7500000,               !- Solar Absorptance",
                                                      "    0.7500000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    IN05,                    !- Name",
                                                      "    Rough,                   !- Roughness",
                                                      "    0.2458000,               !- Thickness {m}",
                                                      "    4.3000001E-02,           !- Conductivity {W/m-K}",
                                                      "    10.00000,                !- Density {kg/m3}",
                                                      "    837.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7500000,               !- Solar Absorptance",
                                                      "    0.7500000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    PW03,                    !- Name",
                                                      "    MediumSmooth,            !- Roughness",
                                                      "    1.2700000E-02,           !- Thickness {m}",
                                                      "    0.1150000,               !- Conductivity {W/m-K}",
                                                      "    545.0000,                !- Density {kg/m3}",
                                                      "    1213.000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7800000,               !- Solar Absorptance",
                                                      "    0.7800000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    CC03,                    !- Name",
                                                      "    MediumRough,             !- Roughness",
                                                      "    0.1016000,               !- Thickness {m}",
                                                      "    1.310000,                !- Conductivity {W/m-K}",
                                                      "    2243.000,                !- Density {kg/m3}",
                                                      "    837.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.6500000,               !- Solar Absorptance",
                                                      "    0.6500000;               !- Visible Absorptance",

                                                      "  Material,",
                                                      "    HF-A3,                   !- Name",
                                                      "    Smooth,                  !- Roughness",
                                                      "    1.5000000E-03,           !- Thickness {m}",
                                                      "    44.96960,                !- Conductivity {W/m-K}",
                                                      "    7689.000,                !- Density {kg/m3}",
                                                      "    418.0000,                !- Specific Heat {J/kg-K}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.2000000,               !- Solar Absorptance",
                                                      "    0.2000000;               !- Visible Absorptance",

                                                      "  Material:NoMass,",
                                                      "    AR02,                    !- Name",
                                                      "    VeryRough,               !- Roughness",
                                                      "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7000000,               !- Solar Absorptance",
                                                      "    0.7000000;               !- Visible Absorptance",

                                                      "  Material:NoMass,",
                                                      "    CP02,                    !- Name",
                                                      "    Rough,                   !- Roughness",
                                                      "    0.2170000,               !- Thermal Resistance {m2-K/W}",
                                                      "    0.9000000,               !- Thermal Absorptance",
                                                      "    0.7500000,               !- Solar Absorptance",
                                                      "    0.7500000;               !- Visible Absorptance",

                                                      "  Construction,",
                                                      "    EXTWALL:LIVING,          !- Name",
                                                      "    A1 - 1 IN STUCCO,        !- Outside Layer",
                                                      "    GP01;                    !- Layer 3",

                                                      "  Construction,",
                                                      "    FLOOR:LIVING,            !- Name",
                                                      "    CC03,                    !- Outside Layer",
                                                      "    CP02;                    !- Layer 2",

                                                      "  Construction,",
                                                      "    ROOF,                    !- Name",
                                                      "    AR02,                    !- Outside Layer",
                                                      "    PW03;                    !- Layer 2",

                                                      "  Zone,",
                                                      "    LIVING ZONE,             !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0,                       !- X Origin {m}",
                                                      "    0,                       !- Y Origin {m}",
                                                      "    0,                       !- Z Origin {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    autocalculate,           !- Ceiling Height {m}",
                                                      "    autocalculate;           !- Volume {m3}",

                                                      "  GlobalGeometryRules,",
                                                      "    UpperLeftCorner,         !- Starting Vertex Position",
                                                      "    CounterClockWise,        !- Vertex Entry Direction",
                                                      "    World;                   !- Coordinate System",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:North,            !- Name",
                                                      "    Wall,                    !- Surface Type",
                                                      "    EXTWALL:LIVING,          !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Outdoors,                !- Outside Boundary Condition",
                                                      "    ,                        !- Outside Boundary Condition Object",
                                                      "    SunExposed,              !- Sun Exposure",
                                                      "    WindExposed,             !- Wind Exposure",
                                                      "    0.5000000,               !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:East,             !- Name",
                                                      "    Wall,                    !- Surface Type",
                                                      "    EXTWALL:LIVING,          !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Outdoors,                !- Outside Boundary Condition",
                                                      "    ,                        !- Outside Boundary Condition Object",
                                                      "    SunExposed,              !- Sun Exposure",
                                                      "    WindExposed,             !- Wind Exposure",
                                                      "    0.5000000,               !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:South,            !- Name",
                                                      "    Wall,                    !- Surface Type",
                                                      "    EXTWALL:LIVING,          !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Outdoors,                !- Outside Boundary Condition",
                                                      "    ,                        !- Outside Boundary Condition Object",
                                                      "    SunExposed,              !- Sun Exposure",
                                                      "    WindExposed,             !- Wind Exposure",
                                                      "    0.5000000,               !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:West,             !- Name",
                                                      "    Wall,                    !- Surface Type",
                                                      "    EXTWALL:LIVING,          !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Outdoors,                !- Outside Boundary Condition",
                                                      "    ,                        !- Outside Boundary Condition Object",
                                                      "    SunExposed,              !- Sun Exposure",
                                                      "    WindExposed,             !- Wind Exposure",
                                                      "    0.5000000,               !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:Floor,            !- Name",
                                                      "    FLOOR,                   !- Surface Type",
                                                      "    FLOOR:LIVING,            !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Surface,                 !- Outside Boundary Condition",
                                                      "    Living:Floor,            !- Outside Boundary Condition Object",
                                                      "    NoSun,                   !- Sun Exposure",
                                                      "    NoWind,                  !- Wind Exposure",
                                                      "    0,                       !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

                                                      "  BuildingSurface:Detailed,",
                                                      "    Living:Ceiling,          !- Name",
                                                      "    ROOF,                 !- Surface Type",
                                                      "    ROOF,          !- Construction Name",
                                                      "    LIVING ZONE,             !- Zone Name",
                                                      "    ,                        !- Space Name",
                                                      "    Outdoors,                !- Outside Boundary Condition",
                                                      "    ,                        !- Outside Boundary Condition Object",
                                                      "    SunExposed,              !- Sun Exposure",
                                                      "    WindExposed,             !- Wind Exposure",
                                                      "    0,                       !- View Factor to Ground",
                                                      "    4,                       !- Number of Vertices",
                                                      "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                                                      "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
                                                      "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
                                                      "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}"});

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosBldgRotAppGonly = 1.0;
    state->dataSurfaceGeometry->SinBldgRotAppGonly = 0.0;
    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);
    EXPECT_FALSE(has_err_output(true));

    state->dataHeatBalFanSys->MAT.allocate(1); // Zone temperature C
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);

    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);

    InitSurfaceHeatBalance(*state);

    EXPECT_FALSE(state->dataHeatBalSurf->InterZoneWindow);
    EXPECT_FALSE(allocated(state->dataHeatBalSurf->ZoneFractDifShortZtoZ));

    state->dataGlobal->BeginSimFlag = false;
    state->dataHeatBalSurf->InterZoneWindow = true;
    InitSurfaceHeatBalance(*state);

    EXPECT_TRUE(allocated(state->dataHeatBalSurf->ZoneFractDifShortZtoZ));
    EXPECT_EQ(1, state->dataHeatBalSurf->ZoneFractDifShortZtoZ(1, 1));

    // bypass internal solar distribution at night
    state->dataHeatBalSurf->InterZoneWindow = false;
    state->dataHeatBal->ZoneBmSolFrIntWinsRep(1) = 10.0;
    state->dataEnvrn->SunIsUp = false;
    InitIntSolarDistribution(*state);
    EXPECT_EQ(0.0, state->dataHeatBal->SurfIntBmIncInsSurfIntensRep(1));
    state->dataEnvrn->SunIsUp = true;
    InitIntSolarDistribution(*state);
    EXPECT_NEAR(1.666667, state->dataHeatBal->SurfIntBmIncInsSurfIntensRep(1), 0.00001);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestInitHBDaylightingNoExtWindow)
{

    std::string const idf_objects1 = R"IDF(
    Building,
        Building,                !- Name
        0,                       !- North Axis {deg}
        Suburbs,                 !- Terrain
        0.04,                    !- Loads Convergence Tolerance Value {W}
        0.4,                     !- Temperature Convergence Tolerance Value {deltaC}
        FullExterior,            !- Solar Distribution
        25,                      !- Maximum Number of Warmup Days
        6;                       !- Minimum Number of Warmup Days

    Timestep,6;

    SimulationControl,
        No,                      !- Do Zone Sizing Calculation
        No,                      !- Do System Sizing Calculation
        No,                      !- Do Plant Sizing Calculation
        No,                     !- Run Simulation for Sizing Periods
        YES;                     !- Run Simulation for Weather File Run Periods

    RunPeriod,
        TESTSim,                 !- Name
        1,                       !- Begin Month
        1,                       !- Begin Day of Month
        ,                        !- Begin Year
        12,                      !- End Month
        31,                      !- End Day of Month
        ,                        !- End Year
        Sunday,                  !- Day of Week for Start Day
        No,                      !- Use Weather File Holidays and Special Days
        No,                      !- Use Weather File Daylight Saving Period
        No,                      !- Apply Weekend Holiday Rule
        Yes,                     !- Use Weather File Rain Indicators
        Yes,                     !- Use Weather File Snow Indicators
        No;                      !- Treat Weather as Actual

    Site:GroundTemperature:BuildingSurface,
        18,                      !- January Ground Temperature {C}
        18,                      !- February Ground Temperature {C}
        18,                      !- March Ground Temperature {C}
        18,                      !- April Ground Temperature {C}
        18,                      !- May Ground Temperature {C}
        18,                      !- June Ground Temperature {C}
        18,                      !- July Ground Temperature {C}
        18,                      !- August Ground Temperature {C}
        18,                      !- September Ground Temperature {C}
        18,                      !- October Ground Temperature {C}
        18,                      !- November Ground Temperature {C}
        18;                      !- December Ground Temperature {C}

    ! Materials
    Material,
        defaultMat_0.25_defaultConstruction ,  !- Name
        Rough,                   !- Roughness
        0.25,                    !- Thickness {m}
        2.3,                     !- Conductivity {W/m-K}
        2400,                    !- Density {kg/m3}
        840,                     !- Specific Heat {J/kg-K}
        0.9,                     !- Thermal Absorptance
        0.7,                     !- Solar Absorptance
        0.7;                     !- Visible Absorptance

    ! Materials
    Material,
        Mineral Wool_0.194_UVal_0.2_Mass ,  !- Name
        Rough,                   !- Roughness
        0.194,                   !- Thickness {m}
        0.041,                   !- Conductivity {W/m-K}
        155,                     !- Density {kg/m3}
        1130,                    !- Specific Heat {J/kg-K}
        0.9,                     !- Thermal Absorptance
        0.7,                     !- Solar Absorptance
        0.7;                     !- Visible Absorptance

    Material,
        General Concrete_0.2_UVal_0.2_Mass ,  !- Name
        Rough,                   !- Roughness
        0.2,                     !- Thickness {m}
        2,                       !- Conductivity {W/m-K}
        2400,                    !- Density {kg/m3}
        950,                     !- Specific Heat {J/kg-K}
        0.9,                     !- Thermal Absorptance
        0.7,                     !- Solar Absorptance
        0.7;                     !- Visible Absorptance

    Construction,
        defaultConstruction,     !- Name
        defaultMat_0.25_defaultConstruction;  !- Outside Layer

    Construction,
        defaultConstruction_FLIPPED,  !- Name
        defaultMat_0.25_defaultConstruction;  !- Outside Layer

    ! UVal_0.2_Mass Type: Facade
    Construction,
        UVal_0.2_Mass,           !- Name
        Mineral Wool_0.194_UVal_0.2_Mass,  !- Outside Layer
        General Concrete_0.2_UVal_0.2_Mass;  !- Layer 2

    Construction,
        UVal_0.2_Mass_FLIPPED,   !- Name
        General Concrete_0.2_UVal_0.2_Mass,  !- Outside Layer
        Mineral Wool_0.194_UVal_0.2_Mass;  !- Layer 2

    Zone,
        Zone_0,                  !- Name
        0,                       !- Direction of Relative North {deg}
        0,                       !- X Origin {m}
        0,                       !- Y Origin {m}
        0,                       !- Z Origin {m}
        1,                       !- Type
        1.0,                     !- Multiplier
        ,                        !- Ceiling Height {m}
        8197.0573178193,         !- Volume {m3}
        631.643058536491,        !- Floor Area {m2}
        TARP,                    !- Zone Inside Convection Algorithm
        DOE-2;                   !- Zone Outside Convection Algorithm

    GlobalGeometryRules,
        LowerLeftCorner,         !- Starting Vertex Position
        CounterClockWise,        !- Vertex Entry Direction
        Relative;                !- Coordinate System

    BuildingSurface:Detailed,
        Zone_0:f0,               !- Name
        Wall,                    !- Surface Type
        UVal_0.2_Mass,           !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Outdoors,                !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        SunExposed,              !- Sun Exposure
        WindExposed,             !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        -2.6475,                 !- Vertex 1 X-coordinate {m}
        16.8426,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        24.7463,                 !- Vertex 2 X-coordinate {m}
        16.8426,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        24.7463,                 !- Vertex 3 X-coordinate {m}
        16.8426,                 !- Vertex 3 Y-coordinate {m}
        12.9774,                 !- Vertex 3 Z-coordinate {m}
        -2.6475,                 !- Vertex 4 X-coordinate {m}
        16.8426,                 !- Vertex 4 Y-coordinate {m}
        12.9774;                 !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  1 3.46944695195361E-17 0
    BuildingSurface:Detailed,
        Zone_0:f1,               !- Name
        Wall,                    !- Surface Type
        UVal_0.2_Mass,           !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Outdoors,                !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        SunExposed,              !- Sun Exposure
        WindExposed,             !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        24.7463,                 !- Vertex 1 X-coordinate {m}
        16.8426,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        24.7463,                 !- Vertex 2 X-coordinate {m}
        39.9005,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        24.7463,                 !- Vertex 3 X-coordinate {m}
        39.9005,                 !- Vertex 3 Y-coordinate {m}
        12.9774,                 !- Vertex 3 Z-coordinate {m}
        24.7463,                 !- Vertex 4 X-coordinate {m}
        16.8426,                 !- Vertex 4 Y-coordinate {m}
        12.9774;                 !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  -4.16333634234434E-17 1 0
    BuildingSurface:Detailed,
        Zone_0:f2,               !- Name
        Wall,                    !- Surface Type
        defaultConstruction,     !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Surface,                 !- Outside Boundary Condition
        Zone_0:f2,               !- Outside Boundary Condition Object
        NoSun,                   !- Sun Exposure
        NoWind,                  !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        24.7463,                 !- Vertex 1 X-coordinate {m}
        39.9005,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        -2.6475,                 !- Vertex 2 X-coordinate {m}
        39.9005,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        -2.6475,                 !- Vertex 3 X-coordinate {m}
        39.9005,                 !- Vertex 3 Y-coordinate {m}
        12.9774,                 !- Vertex 3 Z-coordinate {m}
        24.7463,                 !- Vertex 4 X-coordinate {m}
        39.9005,                 !- Vertex 4 Y-coordinate {m}
        12.9774;                 !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  -1 6.07153216591882E-18 0
    BuildingSurface:Detailed,
        Zone_0:f3,               !- Name
        Wall,                    !- Surface Type
        UVal_0.2_Mass,           !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Outdoors,                !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        SunExposed,              !- Sun Exposure
        WindExposed,             !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        -2.6475,                 !- Vertex 1 X-coordinate {m}
        39.9005,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        -2.6475,                 !- Vertex 2 X-coordinate {m}
        16.8426,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        -2.6475,                 !- Vertex 3 X-coordinate {m}
        16.8426,                 !- Vertex 3 Y-coordinate {m}
        12.9774,                 !- Vertex 3 Z-coordinate {m}
        -2.6475,                 !- Vertex 4 X-coordinate {m}
        39.9005,                 !- Vertex 4 Y-coordinate {m}
        12.9774;                 !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  0 0 -1
    BuildingSurface:Detailed,
        Zone_0:f4,               !- Name
        Floor,                   !- Surface Type
        defaultConstruction,     !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Surface,                 !- Outside Boundary Condition
        Zone_0:f4,               !- Outside Boundary Condition Object
        NoSun,                   !- Sun Exposure
        NoWind,                  !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        -2.6475,                 !- Vertex 1 X-coordinate {m}
        39.9005,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        24.7463,                 !- Vertex 2 X-coordinate {m}
        39.9005,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        24.7463,                 !- Vertex 3 X-coordinate {m}
        30.1033,                 !- Vertex 3 Y-coordinate {m}
        0.0000,                  !- Vertex 3 Z-coordinate {m}
        -2.6475,                 !- Vertex 4 X-coordinate {m}
        30.1033,                 !- Vertex 4 Y-coordinate {m}
        0.0000;                  !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  0 0 1
    BuildingSurface:Detailed,
        Zone_0:f5,               !- Name
        Roof,                    !- Surface Type
        UVal_0.2_Mass,           !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Outdoors,                !- Outside Boundary Condition
        ,                        !- Outside Boundary Condition Object
        SunExposed,              !- Sun Exposure
        WindExposed,             !- Wind Exposure
        0,                       !- View Factor to Ground
        4,                       !- Number of Vertices
        24.7463,                 !- Vertex 1 X-coordinate {m}
        16.8426,                 !- Vertex 1 Y-coordinate {m}
        12.9774,                 !- Vertex 1 Z-coordinate {m}
        24.7463,                 !- Vertex 2 X-coordinate {m}
        39.9005,                 !- Vertex 2 Y-coordinate {m}
        12.9774,                 !- Vertex 2 Z-coordinate {m}
        -2.6475,                 !- Vertex 3 X-coordinate {m}
        39.9005,                 !- Vertex 3 Y-coordinate {m}
        12.9774,                 !- Vertex 3 Z-coordinate {m}
        -2.6475,                 !- Vertex 4 X-coordinate {m}
        16.8426,                 !- Vertex 4 Y-coordinate {m}
        12.9774;                 !- Vertex 4 Z-coordinate {m}

    ! FACE NORMAL  0 0 -1
    BuildingSurface:Detailed,
        Zone_0:f6,               !- Name
        Floor,                   !- Surface Type
        defaultConstruction,     !- Construction Name
        Zone_0,                  !- Zone Name
        ,                        !- Space Name
        Surface,                 !- Outside Boundary Condition
        Zone_0:f6,               !- Outside Boundary Condition Object
        NoSun,                   !- Sun Exposure
        NoWind,                  !- Wind Exposure
        Autocalculate,           !- View Factor to Ground
        4,                       !- Number of Vertices
        24.7463,                 !- Vertex 1 X-coordinate {m}
        30.1033,                 !- Vertex 1 Y-coordinate {m}
        0.0000,                  !- Vertex 1 Z-coordinate {m}
        24.7463,                 !- Vertex 2 X-coordinate {m}
        16.8426,                 !- Vertex 2 Y-coordinate {m}
        0.0000,                  !- Vertex 2 Z-coordinate {m}
        -2.6475,                 !- Vertex 3 X-coordinate {m}
        16.8426,                 !- Vertex 3 Y-coordinate {m}
        0.0000,                  !- Vertex 3 Z-coordinate {m}
        -2.6475,                 !- Vertex 4 X-coordinate {m}
        30.1033,                 !- Vertex 4 Y-coordinate {m}
        0.0000;                  !- Vertex 4 Z-coordinate {m}
    )IDF";

    std::string const idf_objects2 = R"IDF(
    Daylighting:Controls,
        Zone_0_DaylCtrl,         !- Name
        Zone_0,                  !- Zone Name
        SplitFlux,               !- Daylighting Method
        lightsOffice,            !- Availability Schedule Name
        Continuous,              !- Lighting Control Type
        0.1,                     !- Minimum Input Power Fraction for Continuous or ContinuousOff Dimming Control
        0.1,                     !- Minimum Light Output Fraction for Continuous or ContinuousOff Dimming Control
        3,                       !- Number of Stepped Control Steps
        1,                       !- Probability Lighting will be Reset When Needed in Manual Stepped Control
        ,                        !- Glare Calculation Daylighting Reference Point Name
        ,                        !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis {deg}
        22,                      !- Maximum Allowable Discomfort Glare Index
        ,                        !- DElight Gridding Resolution {m2}
        Zone_0_DaylCtrlRefPt1,   !- Daylighting Reference Point 1 Name
        1,                       !- Fraction of Zone Controlled by Reference Point 1
        500;                     !- Illuminance Setpoint at Reference Point 1 {lux}

    Daylighting:ReferencePoint,
        Zone_0_DaylCtrlRefPt1,   !- Name
        Zone_0,                  !- Zone Name
        11.0493979164892,        !- X-Coordinate of Reference Point {m}
        28.3715348505495,        !- Y-Coordinate of Reference Point {m}
        0.85;                    !- Z-Coordinate of Reference Point {m}
    )IDF";

    std::string const idf_objects = idf_objects1 + idf_objects2;
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);

    state->dataHeatBalSurf->SurfHConvInt.allocate(6);
    state->dataMstBal->HConvInFD.allocate(6);
    state->dataMstBal->RhoVaporAirIn.allocate(6);
    state->dataMstBal->HMassConvInFD.allocate(6);

    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->KickOffSimulation = true;
    state->dataHeatBalFanSys->ZoneLatentGain.allocate(1);
    state->dataGlobal->TimeStepZoneSec = 900;
    state->dataGlobal->NumOfTimeStepInHour = 6;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->TimeStep = 1;

    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);

    state->dataEnvrn->SunIsUp = true;
    state->dataEnvrn->BeamSolarRad = 50;
    state->dataEnvrn->GndSolarRad = 50;
    state->dataEnvrn->DifSolarRad = 0;
    state->dataSurface->Surface(1).SolarEnclIndex = 1;
    state->dataSurface->Surface(2).SolarEnclIndex = 1;
    state->dataSurface->Surface(3).SolarEnclIndex = 1;
    state->dataSurface->Surface(4).SolarEnclIndex = 1;
    state->dataSurface->Surface(5).SolarEnclIndex = 1;
    state->dataSurface->Surface(6).SolarEnclIndex = 1;
    state->dataSurface->Surface(7).SolarEnclIndex = 1;
    state->dataViewFactor->EnclSolInfo(1).TotalEnclosureDaylRefPoints = 1;
    state->dataDaylightingData->enclDaylight.allocate(1);
    state->dataDaylightingData->enclDaylight(1).hasSplitFluxDaylighting = true;
    InitSurfaceHeatBalance(*state);
    EXPECT_FALSE(has_err_output(true));
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestTDDSurfWinHeatGain)
{

    std::string const idf_objects = delimited_string({
        "  Zone,",
        "    Daylit Zone,             !- Name",
        "    0.0,                     !- Direction of Relative North {deg}",
        "    0.0,                     !- X Origin {m}",
        "    0.0,                     !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  BuildingSurface:Detailed,",
        "    Daylit South Wall,       !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit West Wall,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit North Wall,       !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit East Wall,        !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.0,0.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Floor,            !- Name",
        "    Floor,                   !- Surface Type",
        "    FLOOR SLAB 8 IN,         !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Daylit Floor,            !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    1.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,0.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,0.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,10.0,0.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,0.0,0.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Ceiling,          !- Name",
        "    Roof,                    !- Surface Type",
        "    CEILING IN ZONE,         !- Construction Name",
        "    Daylit Zone,             !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Daylit Attic Floor,      !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,10.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Zone,",
        "    Daylit Attic Zone,       !- Name",
        "    0.0,                     !- Direction of Relative North {deg}",
        "    0.0,                     !- X Origin {m}",
        "    0.0,                     !- Y Origin {m}",
        "    0.0,                     !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic South Wall, !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic West Wall,  !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,0.0,3.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic North Wall, !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    0.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic East Wall,  !- Name",
        "    Wall,                    !- Surface Type",
        "    EXTWALL80,               !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.5,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    5.0,0.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic Floor,      !- Name",
        "    Floor,                   !- Surface Type",
        "    CEILING IN ATTIC,        !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    Daylit Ceiling,          !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,0.0,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,10.0,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,10.0,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,0.0,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  BuildingSurface:Detailed,",
        "    Daylit Attic Roof,       !- Name",
        "    Roof,                    !- Surface Type",
        "    ROOF,                    !- Construction Name",
        "    Daylit Attic Zone,       !- Zone Name",
        "    ,                        !- Space Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0,10.0,5.0,  !- X,Y,Z ==> Vertex 1 {m}",
        "    0.0,0.0,3.0,  !- X,Y,Z ==> Vertex 2 {m}",
        "    5.0,0.0,3.0,  !- X,Y,Z ==> Vertex 3 {m}",
        "    5.0,10.0,5.0;  !- X,Y,Z ==> Vertex 4 {m}",

        "  DaylightingDevice:Tubular,",
        "    Pipe1,                   !- Name",
        "    Dome1,                   !- Dome Name",
        "    Diffuser1,               !- Diffuser Name",
        "    TDD Pipe,                !- Construction Name",
        "    0.3556,                  !- Diameter {m}",
        "    1.4,                     !- Total Length {m}",
        "    0.28,                    !- Effective Thermal Resistance {m2-K/W}",
        "    Daylit Attic Zone,       !- Transition Zone 1 Name",
        "    1.1;                     !- Transition Zone 1 Length {m}",

        "  FenestrationSurface:Detailed,",
        "    Dome1,                   !- Name",
        "    TubularDaylightDome,     !- Surface Type",
        "    TDD Dome,                !- Construction Name",
        "    Daylit Attic Roof,       !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.3425,3.209,3.64,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.3425,2.906,3.58,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.6575,2.906,3.58,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.6575,3.209,3.64;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Diffuser1,               !- Name",
        "    TubularDaylightDiffuser, !- Surface Type",
        "    TDD Diffuser,            !- Construction Name",
        "    Daylit Ceiling,          !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.3425,3.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.3425,2.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.6575,2.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.6575,3.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  DaylightingDevice:Tubular,",
        "    Pipe2,                   !- Name",
        "    Dome2,                   !- Dome Name",
        "    Diffuser2,               !- Diffuser Name",
        "    TDD Pipe,                !- Construction Name",
        "    0.3556,                  !- Diameter {m}",
        "    2.2,                     !- Total Length {m}",
        "    0.28,                    !- Effective Thermal Resistance {m2-K/W}",
        "    Daylit Attic Zone,       !- Transition Zone 1 Name",
        "    1.9;                     !- Transition Zone 1 Length {m}",

        "  FenestrationSurface:Detailed,",
        "    Dome2,                   !- Name",
        "    TubularDaylightDome,     !- Surface Type",
        "    TDD Dome,                !- Construction Name",
        "    Daylit Attic Roof,       !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.3425,7.209134615385,4.441826923077,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.3425,6.90625,4.38125,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.6575,6.90625,4.38125,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.6575,7.209134615385,4.441826923077;  !- X,Y,Z ==> Vertex 4 {m}",

        "  FenestrationSurface:Detailed,",
        "    Diffuser2,               !- Name",
        "    TubularDaylightDiffuser, !- Surface Type",
        "    TDD Diffuser,            !- Construction Name",
        "    Daylit Ceiling,          !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.0,                     !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1.0,                     !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    2.3425,7.1575,2.5,  !- X,Y,Z ==> Vertex 1 {m}",
        "    2.3425,6.8425,2.5,  !- X,Y,Z ==> Vertex 2 {m}",
        "    2.6575,6.8425,2.5,  !- X,Y,Z ==> Vertex 3 {m}",
        "    2.6575,7.1575,2.5;  !- X,Y,Z ==> Vertex 4 {m}",

        "  Material,",
        "    A1 - 1 IN STUCCO,        !- Name",
        "    Smooth,                  !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    0.6918309,               !- Conductivity {W/m-K}",
        "    1858.142,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.92,                    !- Solar Absorptance",
        "    0.92;                    !- Visible Absorptance",

        "  Material,",
        "    C4 - 4 IN COMMON BRICK,  !- Name",
        "    Rough,                   !- Roughness",
        "    0.1014984,               !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1922.216,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.76,                    !- Solar Absorptance",
        "    0.76;                    !- Visible Absorptance",

        "  Material,",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Name",
        "    Smooth,                  !- Roughness",
        "    1.9050000E-02,           !- Thickness {m}",
        "    0.7264224,               !- Conductivity {W/m-K}",
        "    1601.846,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.92,                    !- Solar Absorptance",
        "    0.92;                    !- Visible Absorptance",

        "  Material,",
        "    C6 - 8 IN CLAY TILE,     !- Name",
        "    Smooth,                  !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    0.5707605,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.82,                    !- Solar Absorptance",
        "    0.82;                    !- Visible Absorptance",

        "  Material,",
        "    C10 - 8 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    0.2033016,               !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.65,                    !- Solar Absorptance",
        "    0.65;                    !- Visible Absorptance",

        "  Material,",
        "    E2 - 1 / 2 IN SLAG OR STONE,  !- Name",
        "    Rough,                   !- Roughness",
        "    1.2710161E-02,           !- Thickness {m}",
        "    1.435549,                !- Conductivity {W/m-K}",
        "    881.0155,                !- Density {kg/m3}",
        "    1673.6,                  !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.55,                    !- Solar Absorptance",
        "    0.55;                    !- Visible Absorptance",

        "  Material,",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Name",
        "    Rough,                   !- Roughness",
        "    9.5402403E-03,           !- Thickness {m}",
        "    0.1902535,               !- Conductivity {W/m-K}",
        "    1121.292,                !- Density {kg/m3}",
        "    1673.6,                  !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.75,                    !- Solar Absorptance",
        "    0.75;                    !- Visible Absorptance",

        "  Material,",
        "    B5 - 1 IN DENSE INSULATION,  !- Name",
        "    VeryRough,               !- Roughness",
        "    2.5389841E-02,           !- Thickness {m}",
        "    4.3239430E-02,           !- Conductivity {W/m-K}",
        "    91.30524,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.50,                    !- Solar Absorptance",
        "    0.50;                    !- Visible Absorptance",

        "  Material,",
        "    C12 - 2 IN HW CONCRETE,  !- Name",
        "    MediumRough,             !- Roughness",
        "    5.0901599E-02,           !- Thickness {m}",
        "    1.729577,                !- Conductivity {W/m-K}",
        "    2242.585,                !- Density {kg/m3}",
        "    836.8,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.65,                    !- Solar Absorptance",
        "    0.65;                    !- Visible Absorptance",

        "  Material,",
        "    ROOFING - ASPHALT SHINGLES,  !- Name",
        "    VeryRough,               !- Roughness",
        "    3.1999999E-03,           !- Thickness {m}",
        "    2.9999999E-02,           !- Conductivity {W/m-K}",
        "    1121.29,                 !- Density {kg/m3}",
        "    830.0,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.70,                    !- Solar Absorptance",
        "    0.70;                    !- Visible Absorptance",

        "  Material,",
        "    BB46 - 5 / 8 IN PLYWOOD, !- Name",
        "    Smooth,                  !- Roughness",
        "    9.9999998E-03,           !- Thickness {m}",
        "    0.110,                   !- Conductivity {W/m-K}",
        "    544.62,                  !- Density {kg/m3}",
        "    1210.0,                  !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.70,                    !- Solar Absorptance",
        "    0.70;                    !- Visible Absorptance",

        "  Material,",
        "    INS - GLASS FIBER BONDED 3 IN,  !- Name",
        "    VeryRough,               !- Roughness",
        "    7.000E-02,               !- Thickness {m}",
        "    2.9999999E-02,           !- Conductivity {W/m-K}",
        "    96.11,                   !- Density {kg/m3}",
        "    790.0,                   !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.50,                    !- Solar Absorptance",
        "    0.50;                    !- Visible Absorptance",

        "  WindowMaterial:Glazing,",
        "    Clear Acrylic Plastic,   !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.92,                    !- Solar Transmittance at Normal Incidence",
        "    0.05,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.05,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.92,                    !- Visible Transmittance at Normal Incidence",
        "    0.05,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.05,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.00,                    !- Infrared Transmittance at Normal Incidence",
        "    0.90,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.90,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.90;                    !- Conductivity {W/m-K}",

        "  WindowMaterial:Glazing,",
        "    Diffusing Acrylic Plastic,  !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.0022,                  !- Thickness {m}",
        "    0.90,                    !- Solar Transmittance at Normal Incidence",
        "    0.08,                    !- Front Side Solar Reflectance at Normal Incidence",
        "    0.08,                    !- Back Side Solar Reflectance at Normal Incidence",
        "    0.90,                    !- Visible Transmittance at Normal Incidence",
        "    0.08,                    !- Front Side Visible Reflectance at Normal Incidence",
        "    0.08,                    !- Back Side Visible Reflectance at Normal Incidence",
        "    0.00,                    !- Infrared Transmittance at Normal Incidence",
        "    0.90,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.90,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.90;                    !- Conductivity {W/m-K}",

        "  Material,",
        "    Very High Reflectivity Surface,  !- Name",
        "    Smooth,                  !- Roughness",
        "    0.0005,                  !- Thickness {m}",
        "    237,                     !- Conductivity {W/m-K}",
        "    2702,                    !- Density {kg/m3}",
        "    903,                     !- Specific Heat {J/kg-K}",
        "    0.90,                    !- Thermal Absorptance",
        "    0.05,                    !- Solar Absorptance",
        "    0.05;                    !- Visible Absorptance",

        "  Construction,",
        "    EXTWALL80,               !- Name",
        "    A1 - 1 IN STUCCO,        !- Outside Layer",
        "    C4 - 4 IN COMMON BRICK,  !- Layer 2",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 3",

        "  Construction,",
        "    FLOOR SLAB 8 IN,         !- Name",
        "    C10 - 8 IN HW CONCRETE;  !- Outside Layer",

        "  Construction,",
        "    ROOF,                    !- Name",
        "    ROOFING - ASPHALT SHINGLES,  !- Outside Layer",
        "    E3 - 3 / 8 IN FELT AND MEMBRANE,  !- Layer 2",
        "    BB46 - 5 / 8 IN PLYWOOD; !- Layer 3",

        "  Construction,",
        "    CEILING IN ZONE,         !- Name",
        "    INS - GLASS FIBER BONDED 3 IN,  !- Outside Layer",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD;  !- Layer 2",

        "  Construction,",
        "    CEILING IN ATTIC,        !- Name",
        "    E1 - 3 / 4 IN PLASTER OR GYP BOARD,  !- Outside Layer",
        "    INS - GLASS FIBER BONDED 3 IN;  !- Layer 2",

        "  Construction,",
        "    TDD Pipe,                !- Name",
        "    Very High Reflectivity Surface;  !- Outside Layer",

        "  Construction,",
        "    TDD Dome,                !- Name",
        "    Clear Acrylic Plastic;   !- Outside Layer",

        "  Construction,",
        "    TDD Diffuser,            !- Name",
        "    Diffusing Acrylic Plastic;  !- Outside Layer",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SurfaceGeometry::GetGeometryParameters(*state, ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    state->dataSurfaceGeometry->CosZoneRelNorth.allocate(2);
    state->dataSurfaceGeometry->SinZoneRelNorth.allocate(2);

    state->dataSurfaceGeometry->CosZoneRelNorth(1) = std::cos(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(1) = std::sin(-state->dataHeatBal->Zone(1).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosZoneRelNorth(2) = std::cos(-state->dataHeatBal->Zone(2).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->SinZoneRelNorth(2) = std::sin(-state->dataHeatBal->Zone(2).RelNorth * DataGlobalConstants::DegToRadians);
    state->dataSurfaceGeometry->CosBldgRelNorth = 1.0;
    state->dataSurfaceGeometry->SinBldgRelNorth = 0.0;
    int const HoursInDay(24);
    state->dataSurface->SurfSunCosHourly.allocate(HoursInDay);
    for (int hour = 1; hour <= HoursInDay; hour++) {
        state->dataSurface->SurfSunCosHourly(hour) = 0.0;
    }
    //    SurfaceGeometry::GetSurfaceData(*state, ErrorsFound);
    //    EXPECT_FALSE(ErrorsFound);

    SurfaceGeometry::SetupZoneGeometry(*state, ErrorsFound); // this calls GetSurfaceData()
    EXPECT_FALSE(ErrorsFound);                               // expect no errors
    HeatBalanceIntRadExchange::InitSolarViewFactors(*state);

    state->dataZoneEquip->ZoneEquipConfig.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Daylit Zone";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(1) = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).InletNode(2) = 2;
    state->dataZoneEquip->ZoneEquipConfig(1).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ExhaustNode(1) = 3;
    state->dataZoneEquip->ZoneEquipConfig(1).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ReturnNode(1) = 4;
    state->dataZoneEquip->ZoneEquipConfig(1).FixedReturnFlow.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(2).ZoneName = "Daylit Attic Zone";
    state->dataZoneEquip->ZoneEquipConfig(2).ActualZoneNum = 2;
    state->dataHeatBal->Zone(2).IsControlled = true;
    state->dataHeatBal->Zone(2).ZoneEqNum = 2;
    state->dataZoneEquip->ZoneEquipConfig(2).NumInletNodes = 2;
    state->dataZoneEquip->ZoneEquipConfig(2).InletNode.allocate(2);
    state->dataZoneEquip->ZoneEquipConfig(2).InletNode(1) = 6;
    state->dataZoneEquip->ZoneEquipConfig(2).InletNode(2) = 7;
    state->dataZoneEquip->ZoneEquipConfig(2).NumExhaustNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(2).ExhaustNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(2).ExhaustNode(1) = 8;
    state->dataZoneEquip->ZoneEquipConfig(2).NumReturnNodes = 1;
    state->dataZoneEquip->ZoneEquipConfig(2).ReturnNode.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(2).ReturnNode(1) = 9;
    state->dataZoneEquip->ZoneEquipConfig(2).FixedReturnFlow.allocate(1);

    state->dataSize->ZoneEqSizing.allocate(2);
    state->dataHeatBal->Zone(1).SystemZoneNodeNumber = 5;
    state->dataHeatBal->Zone(2).SystemZoneNodeNumber = 10;
    state->dataEnvrn->OutBaroPress = 101325.0;
    state->dataHeatBalFanSys->MAT.allocate(2); // Zone temperature C
    state->dataHeatBalFanSys->MAT(1) = 24.0;
    state->dataHeatBalFanSys->MAT(2) = 24.0;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(2);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.001;
    state->dataHeatBalFanSys->ZoneAirHumRat(2) = 0.001;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(2);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = 0.001;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(2) = 0.001;

    state->dataLoopNodes->Node.allocate(4);

    state->dataHeatBalSurf->SurfTempInTmp.allocate(6);
    state->dataHeatBalSurf->SurfTempInTmp(1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(3) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(4) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(5) = 25.0;
    state->dataHeatBalSurf->SurfTempInTmp(6) = 25.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfWinCoeffAdjRatio.dimension(state->dataSurface->TotSurfaces, 1.0);
    state->dataHeatBalSurf->SurfHConvInt.dimension(state->dataSurface->TotSurfaces, 0.5);
    state->dataMstBal->HConvInFD.allocate(state->dataSurface->TotSurfaces);
    state->dataMstBal->RhoVaporAirIn.allocate(state->dataSurface->TotSurfaces);
    state->dataMstBal->HMassConvInFD.allocate(state->dataSurface->TotSurfaces);

    SolarShading::AllocateModuleArrays(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);

    for (int loop = 1; loop <= state->dataSurface->TotSurfaces; ++loop) {
        state->dataHeatBalSurf->SurfOutsideTempHist(1)(loop) = 20.0;
    }

    state->dataConstruction->Construct(state->dataSurface->Surface(8).Construction).TransDiff = 0.001; // required for GetTDDInput function to work.
    DaylightingDevices::GetTDDInput(*state);

    CalcHeatBalanceInsideSurf(*state);
    EXPECT_NEAR(37.63, state->dataSurface->SurfWinHeatGain(7), 0.1);
    EXPECT_NEAR(37.63, state->dataSurface->SurfWinHeatGain(8), 0.1);
}
} // namespace EnergyPlus
