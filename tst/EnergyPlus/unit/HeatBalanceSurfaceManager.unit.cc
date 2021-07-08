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

// EnergyPlus::HeatBalanceSurfaceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
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

    state->dataConstruction->Construct.allocate(ConstrNum);
    state->dataConstruction->Construct(ConstrNum).Name = "TestConstruct";
    state->dataConstruction->Construct(ConstrNum).CTFCross(0) = 0.0;
    state->dataConstruction->Construct(ConstrNum).CTFOutside(0) = 1.0;
    state->dataConstruction->Construct(ConstrNum).SourceSinkPresent = true;
    state->dataMaterial->Material.allocate(1);
    state->dataMaterial->Material(1).Name = "TestMaterial";

    state->dataHeatBalSurf->SurfHcExt.allocate(SurfNum);
    state->dataHeatBalSurf->SurfHcExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHAirExt.allocate(SurfNum);
    state->dataHeatBalSurf->SurfHAirExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHSkyExt.allocate(SurfNum);
    state->dataHeatBalSurf->SurfHSkyExt(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfHGrdExt.allocate(SurfNum);
    state->dataHeatBalSurf->SurfHGrdExt(SurfNum) = 1.0;

    state->dataHeatBalSurf->SurfCTFConstOutPart.allocate(SurfNum);
    state->dataHeatBalSurf->SurfCTFConstOutPart(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfOpaqQRadSWOutAbs.allocate(SurfNum);
    state->dataHeatBalSurf->SurfOpaqQRadSWOutAbs(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfTempIn.allocate(SurfNum);
    state->dataHeatBalSurf->SurfTempIn(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQRadSWOutMvIns.allocate(SurfNum);
    state->dataHeatBalSurf->SurfQRadSWOutMvIns(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs.allocate(SurfNum);
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs(SurfNum) = 1.0;
    state->dataHeatBalSurf->SurfQAdditionalHeatSourceOutside.allocate(SurfNum);
    state->dataHeatBalSurf->SurfQAdditionalHeatSourceOutside(SurfNum) = 0.0;
    state->dataSurface->SurfHasSurroundingSurfProperties.allocate(SurfNum);
    state->dataSurface->SurfHasSurroundingSurfProperties(SurfNum) = 0;
    state->dataSurface->SurfMaterialMovInsulExt.allocate(SurfNum);
    state->dataSurface->SurfMaterialMovInsulExt(SurfNum) = 1;

    state->dataHeatBalSurf->TH.allocate(2, 2, 1);
    state->dataSurface->Surface.allocate(SurfNum);
    state->dataSurface->Surface(SurfNum).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->Surface(SurfNum).Area = 10.0;
    state->dataSurface->SurfOutDryBulbTemp.allocate(SurfNum);
    state->dataSurface->SurfOutDryBulbTemp = 0;
    state->dataEnvrn->SkyTemp = 23.0;
    state->dataEnvrn->OutDryBulbTemp = 23.0;

    state->dataHeatBalSurf->QdotRadOutRep.allocate(SurfNum);
    state->dataHeatBalSurf->QdotRadOutRepPerArea.allocate(SurfNum);
    state->dataHeatBalSurf->QRadOutReport.allocate(SurfNum);
    state->dataHeatBalSurf->QAirExtReport.allocate(SurfNum);
    state->dataHeatBalSurf->QHeatEmiReport.allocate(SurfNum);
    state->dataGlobal->TimeStepZoneSec = 900.0;

    CalcOutsideSurfTemp(*state, SurfNum, ZoneNum, ConstrNum, HMovInsul, TempExt, ErrorFlag);

    std::string const error_string = delimited_string({
        "   ** Severe  ** Exterior movable insulation is not valid with embedded sources/sinks",
        "   **   ~~~   ** Construction TestConstruct contains an internal source or sink but also uses",
        "   **   ~~~   ** exterior movable insulation TestMaterial for a surface with that construction.",
        "   **   ~~~   ** This is not currently allowed because the heat balance equations do not currently accommodate this combination.",
    });

    EXPECT_TRUE(ErrorFlag);
    EXPECT_TRUE(compare_err_stream(error_string, true));
    EXPECT_EQ(10.0 * 1.0 * (state->dataHeatBalSurf->TH(1, 1, SurfNum) - state->dataSurface->SurfOutDryBulbTemp(SurfNum)),
              state->dataHeatBalSurf->QAirExtReport(SurfNum));
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
    state->dataViewFactor->ZoneRadiantInfo.allocate(1);
    state->dataHeatBal->EnclRadReCalc.allocate(1);
    state->dataHeatBal->EnclRadReCalc(1) = true;
    state->dataHeatBal->EnclRadThermAbsMult.allocate(1);
    state->dataViewFactor->ZoneRadiantInfo(1).SurfacePtr.allocate(1);
    state->dataViewFactor->ZoneRadiantInfo(1).SurfacePtr(1) = 1;

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

    AllocateSurfaceHeatBalArrays(*state); // allocates a host of variables related to CTF calculations

    state->dataSurface->Surface(1).Class = DataSurfaces::SurfaceClass::Wall;
    state->dataSurface->Surface(1).HeatTransSurf = true;
    state->dataSurface->Surface(1).HeatTransferAlgorithm = DataSurfaces::iHeatTransferModel::CTF;
    state->dataSurface->Surface(1).ExtBoundCond = 1;
    state->dataSurface->Surface(1).Construction = 1;
    state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceFirst = 1;
    state->dataHeatBal->Zone(1).OpaqOrIntMassSurfaceLast = 1;

    state->dataConstruction->Construct(1).NumCTFTerms = 2;
    state->dataConstruction->Construct(1).SourceSinkPresent = true;
    state->dataConstruction->Construct(1).NumHistories = 1;
    state->dataConstruction->Construct(1).CTFTUserOut(0) = 0.5;
    state->dataConstruction->Construct(1).CTFTUserIn(0) = 0.25;
    state->dataConstruction->Construct(1).CTFTUserSource(0) = 0.25;

    state->dataHeatBalSurf->SurfCurrNumHist(1) = 0;
    state->dataHeatBalSurf->TH(1, 1, 1) = 20.0;
    state->dataHeatBalSurf->SurfTempIn(1) = 10.0;

    state->dataHeatBalFanSys->CTFTuserConstPart(1) = 0.0;

    UpdateThermalHistories(*state); // First check to see if it is calculating the user location temperature properly

    EXPECT_EQ(12.5, state->dataHeatBalSurf->SurfTempUserLoc(1));
    EXPECT_EQ(0.0, state->dataHeatBalSurf->TuserHist(1, 3));

    UpdateThermalHistories(*state);

    EXPECT_EQ(12.5, state->dataHeatBalSurf->TuserHist(1, 3)); // Now check to see that it is shifting the temperature history properly
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

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->TH.allocate(2, 2, 6);
    state->dataHeatBalSurf->TH(1, 1, 1) = 20;
    state->dataHeatBalSurf->TH(1, 1, 2) = 20;
    state->dataHeatBalSurf->TH(1, 1, 3) = 20;
    state->dataHeatBalSurf->TH(1, 1, 4) = 20;
    state->dataHeatBalSurf->TH(1, 1, 5) = 20;
    state->dataHeatBalSurf->TH(1, 1, 6) = 20;
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
    state->dataHeatBalSurf->TH.deallocate();
    state->dataHeatBalSurf->SurfHConvInt.deallocate();
    state->dataMstBal->HConvInFD.deallocate();
    state->dataMstBal->RhoVaporAirIn.deallocate();
    state->dataMstBal->HMassConvInFD.deallocate();
    state->dataHeatBalFanSys->ZoneLatentGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGain.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRep.deallocate();
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.deallocate();
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

    state->dataHeatBalSurf->TH.allocate(2, 2, 6);
    state->dataHeatBalSurf->TH(1, 1, 1) = 20;
    state->dataHeatBalSurf->TH(1, 1, 2) = 20;
    state->dataHeatBalSurf->TH(1, 1, 3) = 20;
    state->dataHeatBalSurf->TH(1, 1, 4) = 20;
    state->dataHeatBalSurf->TH(1, 1, 5) = 20;
    state->dataHeatBalSurf->TH(1, 1, 6) = 20;
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

    state->dataHeatBalSurf->TH.allocate(2, 2, 6);
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

    int SurfNum;
    for (SurfNum = 1; SurfNum <= 6; SurfNum++) {
        state->dataHeatBalSurf->TH(1, 1, SurfNum) = 20;       // Surf temp
        state->dataSurface->SurfOutDryBulbTemp(SurfNum) = 22; // Air temp
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
    state->dataSurface->Surface(1).HeatTransferAlgorithm = DataSurfaces::iHeatTransferModel::CTF;
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

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->TH.allocate(2, 2, 6);
    state->dataHeatBalSurf->TH(1, 1, 1) = 20;
    state->dataHeatBalSurf->TH(1, 1, 2) = 20;
    state->dataHeatBalSurf->TH(1, 1, 3) = 20;
    state->dataHeatBalSurf->TH(1, 1, 4) = 20;
    state->dataHeatBalSurf->TH(1, 1, 5) = 20;
    state->dataHeatBalSurf->TH(1, 1, 6) = 20;
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

    AllocateSurfaceHeatBalArrays(*state);
    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::AllocateZoneHeatBalArrays(*state);
    SolarShading::AllocateModuleArrays(*state);
    SolarShading::DetermineShadowingCombinations(*state);
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
    state->dataHeatBalSurf->TH.deallocate();
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
    state->dataHeatBalSurf->TH.allocate(1, 1, 1);

    state->dataSurface->Surface(1).Area = 58.197;
    state->dataHeatBalSurf->SurfHcExt(1) = 1000;
    state->dataHeatBalSurf->TH(1, 1, 1) = 6.71793958923051;
    state->dataSurface->SurfOutWetBulbTemp(1) = 6.66143784594778;
    state->dataSurface->SurfOutDryBulbTemp(1) = 7.2;

    // If Rain Flag = on, GetQdotConvOutRep uses Outdoor Air Wet Bulb Temp.
    state->dataEnvrn->IsRain = true;
    Real64 ExpectedQconvPerArea1 = -1000 * (6.71793958923051 - 6.66143784594778);

    EXPECT_NEAR(ExpectedQconvPerArea1, GetQdotConvOutRepPerArea(*state, 1), 0.01);

    // Otherwise, GetQdotConvOutRep uses Outdoor Air Dry Bulb Temp.
    state->dataEnvrn->IsRain = false;
    state->dataHeatBalSurf->SurfHcExt(1) = 5.65361106051348;
    Real64 ExpectedQconvPerArea2 = -5.65361106051348 * (6.71793958923051 - 7.2);

    EXPECT_NEAR(ExpectedQconvPerArea2, GetQdotConvOutRepPerArea(*state, 1), 0.01);
}

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_TestInterzoneRadFactorCalc)
{

    state->dataSurface->TotSurfaces = 2;
    state->dataGlobal->NumOfZones = 2;
    state->dataHeatBal->TotMaterials = 1;
    state->dataHeatBal->TotConstructs = 1;

    state->dataHeatBal->Zone.allocate(state->dataGlobal->NumOfZones);
    state->dataSurface->Surface.allocate(state->dataSurface->TotSurfaces);
    state->dataConstruction->Construct.allocate(state->dataHeatBal->TotConstructs);
    state->dataHeatBal->EnclSolVMULT.allocate(state->dataGlobal->NumOfZones);
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

    state->dataHeatBal->Zone(1).HasInterZoneWindow = true;
    state->dataHeatBal->Zone(2).HasInterZoneWindow = true;

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
    state->dataDaylightingData->ZoneDaylight(1).DaylightMethod = DataDaylighting::iDaylightingMethod::SplitFluxDaylighting;
    state->dataDaylightingData->ZoneDaylight(1).DaylIllumAtRefPt.allocate(1);
    state->dataDaylightingData->ZoneDaylight(1).IllumSetPoint.allocate(1);
    state->dataDaylightingData->ZoneDaylight(1).ZonePowerReductionFactor = 0.5;
    state->dataDaylightingData->ZoneDaylight(1).DaylIllumAtRefPt(1) = 300;
    state->dataDaylightingData->ZoneDaylight(1).IllumSetPoint(1) = 400;
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

    state->dataHeatBalSurf->TH.allocate(2, 2, 6);
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

TEST_F(EnergyPlusFixture, HeatBalanceSurfaceManager_QdotConvInRepAdjRatioTest)
{
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Material,",
                          "  Concrete Block,          !- Name",
                          "  MediumRough,             !- Roughness",
                          "  0.1014984,               !- Thickness {m}",
                          "  0.3805070,               !- Conductivity {W/m-K}",
                          "  608.7016,                !- Density {kg/m3}",
                          "  836.8000;                !- Specific Heat {J/kg-K}",
                          "Construction,",
                          "  WallConstruction,        !- Name",
                          "  Concrete Block;          !- Outside Layer",
                          "WindowMaterial:SimpleGlazingSystem,",
                          "  WindowMaterial,          !- Name",
                          "  5.778,                   !- U-Factor {W/m2-K}",
                          "  0.819,                   !- Solar Heat Gain Coefficient",
                          "  0.881;                   !- Visible Transmittance",
                          "Construction,",
                          "  WindowConstruction,      !- Name",
                          "  WindowMaterial;          !- Outside Layer",
                          "WindowProperty:FrameAndDivider,",
                          "  WindowFrame,             !- Name",
                          "  0.05,                    !- Frame Width {m}",
                          "  0.00,                    !- Frame Outside Projection {m}",
                          "  0.00,                    !- Frame Inside Projection {m}",
                          "  5.0,                     !- Frame Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Frame-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Frame Solar Absorptance",
                          "  0.8,                     !- Frame Visible Absorptance",
                          "  0.9,                     !- Frame Thermal Hemispherical Emissivity",
                          "  DividedLite,             !- Divider Type",
                          "  0.02,                    !- Divider Width {m}",
                          "  2,                       !- Number of Horizontal Dividers",
                          "  2,                       !- Number of Vertical Dividers",
                          "  0.00,                    !- Divider Outside Projection {m}",
                          "  0.00,                    !- Divider Inside Projection {m}",
                          "  5.0,                     !- Divider Conductance {W/m2-K}",
                          "  1.2,                     !- Ratio of Divider-Edge Glass Conductance to Center-Of-Glass Conductance",
                          "  0.8,                     !- Divider Solar Absorptance",
                          "  0.8,                     !- Divider Visible Absorptance",
                          "  0.9;                     !- Divider Thermal Hemispherical Emissivity",
                          "FenestrationSurface:Detailed,",
                          "  FenestrationSurface,     !- Name",
                          "  Window,                  !- Surface Type",
                          "  WindowConstruction,      !- Construction Name",
                          "  Wall,                    !- Building Surface Name",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  0.5000000,               !- View Factor to Ground",
                          "  WindowFrame,             !- Frame and Divider Name",
                          "  1.0,                     !- Multiplier",
                          "  4,                       !- Number of Vertices",
                          "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Wall,                    !- Name",
                          "  Wall,                    !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  SunExposed,              !- Sun Exposure",
                          "  WindExposed,             !- Wind Exposure",
                          "  0.5000000,               !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
                          "BuildingSurface:Detailed,",
                          "  Floor,                   !- Name",
                          "  Floor,                   !- Surface Type",
                          "  WallConstruction,        !- Construction Name",
                          "  Zone,                    !- Zone Name",
                          "  Outdoors,                !- Outside Boundary Condition",
                          "  ,                        !- Outside Boundary Condition Object",
                          "  NoSun,                   !- Sun Exposure",
                          "  NoWind,                  !- Wind Exposure",
                          "  1.0,                     !- View Factor to Ground",
                          "  4,                       !- Number of Vertices",
                          "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
                          "Zone,",
                          "  Zone,                    !- Name",
                          "  0,                       !- Direction of Relative North {deg}",
                          "  6.000000,                !- X Origin {m}",
                          "  6.000000,                !- Y Origin {m}",
                          "  0,                       !- Z Origin {m}",
                          "  1,                       !- Type",
                          "  1,                       !- Multiplier",
                          "  autocalculate,           !- Ceiling Height {m}",
                          "  autocalculate;           !- Volume {m3}"});

    ASSERT_TRUE(process_idf(idf_objects));

    state->dataHeatBal->ZoneIntGain.allocate(1);

    createFacilityElectricPowerServiceObject(*state);
    HeatBalanceManager::SetPreConstructionInputParameters(*state);
    HeatBalanceManager::GetProjectControlData(*state, ErrorsFound);
    HeatBalanceManager::GetFrameAndDividerData(*state, ErrorsFound);
    HeatBalanceManager::GetMaterialData(*state, ErrorsFound);
    HeatBalanceManager::GetConstructData(*state, ErrorsFound);
    HeatBalanceManager::GetBuildingData(*state, ErrorsFound);

    //Psychrometrics::InitializePsychRoutines(*state);

    state->dataGlobal->TimeStep = 1;
    state->dataGlobal->TimeStepZone = 1;
    state->dataGlobal->HourOfDay = 1;
    state->dataGlobal->NumOfTimeStepInHour = 1;
    state->dataGlobal->BeginSimFlag = true;
    state->dataGlobal->BeginEnvrnFlag = true;
    state->dataEnvrn->OutBaroPress = 100000;

    state->dataZoneEquip->ZoneEquipConfig.allocate(1);
    state->dataZoneEquip->ZoneEquipConfig(1).ZoneName = "Zone";
    state->dataZoneEquip->ZoneEquipConfig(1).ActualZoneNum = 1;
    state->dataHeatBal->Zone(1).ZoneEqNum = 1;
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

    state->dataLoopNodes->Node.allocate(4);
    state->dataHeatBal->SurfTempEffBulkAir.allocate(3);
    state->dataHeatBalSurf->SurfTempInTmp.allocate(3);

    int surfNum1 = UtilityRoutines::FindItemInList("WALL", state->dataSurface->Surface);
    int surfNum2 = UtilityRoutines::FindItemInList("FENESTRATIONSURFACE", state->dataSurface->Surface);
    int surfNum3 = UtilityRoutines::FindItemInList("FLOOR", state->dataSurface->Surface);

    state->dataSurface->Surface(surfNum1).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum2).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum3).HeatTransSurf = true;
    state->dataSurface->Surface(surfNum1).Area = 10.0;
    state->dataSurface->Surface(surfNum2).Area = 10.0;
    state->dataSurface->Surface(surfNum3).Area = 10.0;
    state->dataSurface->Surface(surfNum1).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum2).SolarEnclIndex = 1;
    state->dataSurface->Surface(surfNum3).SolarEnclIndex = 1;
    state->dataHeatBalSurf->SurfTempInTmp(surfNum1) = 15.0;
    state->dataHeatBalSurf->SurfTempInTmp(surfNum2) = 20.0;
    state->dataHeatBalSurf->SurfTempInTmp(surfNum3) = 25.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum1) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum2) = 10.0;
    state->dataHeatBal->SurfTempEffBulkAir(surfNum3) = 10.0;

    state->dataLoopNodes->Node(1).Temp = 20.0;
    state->dataLoopNodes->Node(2).Temp = 20.0;
    state->dataLoopNodes->Node(3).Temp = 20.0;
    state->dataLoopNodes->Node(4).Temp = 20.0;
    state->dataLoopNodes->Node(1).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(2).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(3).MassFlowRate = 0.1;
    state->dataLoopNodes->Node(4).MassFlowRate = 0.1;

    state->dataHeatBalSurf->SurfHConvInt.allocate(3);
    state->dataHeatBalSurf->SurfHConvInt(surfNum1) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(surfNum2) = 0.5;
    state->dataHeatBalSurf->SurfHConvInt(surfNum3) = 0.5;
    state->dataHeatBal->Zone(1).IsControlled = true;
    state->dataHeatBalFanSys->ZoneAirHumRat.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;
    state->dataHeatBalFanSys->ZoneAirHumRatAvg.allocate(1);
    state->dataHeatBalFanSys->ZoneAirHumRatAvg(1) = state->dataHeatBalFanSys->ZoneAirHumRat(1) = 0.011;

    state->dataHeatBalFanSys->MAT.allocate(1);
    state->dataHeatBalFanSys->MAT(1) = 25.0;
    state->dataHeatBalFanSys->QHTRadSysSurf.allocate(3);
    state->dataHeatBalFanSys->QHWBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QSteamBaseboardSurf.allocate(3);
    state->dataHeatBalFanSys->QElecBaseboardSurf.allocate(3);
    state->dataHeatBal->SurfWinQRadSWwinAbs.allocate(3, 1);
    state->dataHeatBal->SurfQRadThermInAbs.allocate(3);
    state->dataHeatBal->SurfQRadSWOutIncident.allocate(3);
    state->dataSurface->SurfWinTransSolar.allocate(3);
    state->dataHeatBal->ZoneWinHeatGain.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRep.allocate(1);
    state->dataHeatBal->ZoneWinHeatGainRepEnergy.allocate(1);
    state->dataSurface->SurfWinHeatGain.allocate(3);
    state->dataSurface->SurfWinHeatTransfer.allocate(3);
    state->dataSurface->SurfWinGainConvGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRep.allocate(3);
    state->dataSurface->SurfWinGapConvHtFlowRepEnergy.allocate(3);
    state->dataHeatBal->EnclSolQSWRad.allocate(1);
    state->dataSurface->SurfWinLossSWZoneToOutWinRep.allocate(3);
    state->dataSurface->SurfWinSysSolTransmittance.allocate(3);
    state->dataSurface->SurfWinSysSolAbsorptance.allocate(3);
    state->dataSurface->SurfWinSysSolReflectance.allocate(3);
    state->dataSurface->SurfWinInsideGlassCondensationFlag.allocate(3);
    state->dataSurface->SurfWinGainFrameDividerToZoneRep.allocate(3);
    state->dataSurface->SurfWinInsideFrameCondensationFlag.allocate(3);
    state->dataSurface->SurfWinInsideDividerCondensationFlag.allocate(3);

    state->dataSurface->SurfTAirRef(surfNum1) = DataSurfaces::ZoneMeanAirTemp;
    state->dataSurface->SurfTAirRef(surfNum2) = DataSurfaces::ZoneSupplyAirTemp;
    state->dataSurface->SurfTAirRef(surfNum3) = DataSurfaces::AdjacentAirTemp;

    state->dataHeatBalSurf->QdotConvOutRep.allocate(3);
    state->dataHeatBalSurf->QdotConvOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QConvOutReport.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRep.allocate(3);
    state->dataHeatBalSurf->QdotRadOutRepPerArea.allocate(3);
    state->dataHeatBalSurf->QRadOutReport.allocate(3);
    state->dataHeatBalSurf->SurfQRadLWOutSrdSurfs.allocate(3);
    state->dataHeatBalSurf->QAirExtReport.allocate(3);
    state->dataHeatBalSurf->QHeatEmiReport.allocate(3);

    state->dataHeatBal->SurfQRadSWOutIncident = 0.0;
    state->dataHeatBal->SurfWinQRadSWwinAbs = 0.0;
    state->dataHeatBal->SurfQRadThermInAbs = 0.0;

    state->dataHeatBalFanSys->QHTRadSysSurf = 0.0;
    state->dataHeatBalFanSys->QHWBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QSteamBaseboardSurf = 0.0;
    state->dataHeatBalFanSys->QElecBaseboardSurf = 0.0;
    state->dataSurface->SurfWinTransSolar = 0.0;
    state->dataHeatBal->EnclSolQSWRad = 0.0;

    state->dataHeatBalSurf->SurfTempIn.allocate(3);
    state->dataHeatBalSurf->SurfTempIn(surfNum1) = 297.0;
    state->dataHeatBalSurf->SurfTempIn(surfNum2) = 297.0;
    state->dataHeatBalSurf->SurfTempIn(surfNum3) = 297.0;
    state->dataHeatBalSurfMgr->RefAirTemp.allocate(3);
    state->dataHeatBalSurfMgr->RefAirTemp(surfNum2) = 296.0;
    state->dataHeatBalSurf->QdotConvInRep.allocate(3);
    state->dataHeatBalSurf->QConvInReport.allocate(3);
    state->dataHeatBalSurf->QdotConvInRepPerArea.allocate(3);

    // fixme: make these more realistic
    state->dataWindowManager->hcout = 4.0;
    state->dataWindowManager->tout = 295.0;
    state->dataGlobal->TimeStepZoneSec = 50.0;
    state->dataHeatBal->CoeffAdjRatioIn.allocate(3);
    state->dataHeatBal->CoeffAdjRatioIn(surfNum2) = 1.0;
    state->dataHeatBalSurf->CoeffAdjRatioOut.allocate(3);
    state->dataHeatBalSurf->CoeffAdjRatioOut(surfNum2) = 1.0;

    state->dataHeatBalSurf->SurfIsAdiabatic.allocate(3);
    state->dataHeatBalSurf->SurfIsAdiabatic(2) = 0;
    state->dataHeatBalSurf->SurfIsSourceOrSink.allocate(3);
    state->dataHeatBalSurf->SurfIsSourceOrSink(2) = 0;

    state->dataHeatBalSurf->SurfCTFCross0.allocate(3);
    state->dataHeatBalSurf->SurfCTFInside0.allocate(3);
    state->dataHeatBalSurf->SurfCTFSourceIn0.allocate(3);
    state->dataHeatBalSurf->SurfTempOutHist.allocate(3);
    state->dataHeatBalSurf->TH.allocate(2, 2, 3);
    state->dataHeatBalSurf->TH(1, 1, surfNum2) = 22.0;
    state->dataHeatBalSurf->TH(2, 1, surfNum2) = 22.0;

    state->dataSurface->SurfWinHeatGainRep.allocate(3);
    state->dataSurface->SurfWinHeatGainRep.allocate(3);
    state->dataSurface->SurfWinHeatLossRep.allocate(3);
    state->dataSurface->SurfWinGainConvGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinLossSWZoneToOutWinRep.allocate(3);
    state->dataSurface->SurfWinGainFrameDividerToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainConvGlazShadGapToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainConvShadeToZoneRep.allocate(3);
    state->dataSurface->SurfWinOtherConvGainInsideFaceToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRShadeToZoneRep.allocate(3);
    state->dataSurface->SurfWinFrameQRadOutAbs.allocate(3);
    state->dataSurface->SurfWinFrameQRadInAbs.allocate(3);
    state->dataSurface->SurfWinDividerQRadOutAbs.allocate(3);
    state->dataSurface->SurfWinDividerQRadInAbs.allocate(3);
    state->dataSurface->SurfWinHeatLossRep.allocate(3);
    state->dataSurface->SurfWinGainConvGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRGlazToZoneRep.allocate(3);
    state->dataSurface->SurfWinLossSWZoneToOutWinRep.allocate(3);
    state->dataSurface->SurfWinGainFrameDividerToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainConvGlazShadGapToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainConvShadeToZoneRep.allocate(3);
    state->dataSurface->SurfWinOtherConvGainInsideFaceToZoneRep.allocate(3);
    state->dataSurface->SurfWinGainIRShadeToZoneRep.allocate(3);
    state->dataSurface->SurfWinFrameQRadOutAbs.allocate(3);
    state->dataSurface->SurfWinFrameQRadInAbs.allocate(3);
    state->dataSurface->SurfWinDividerQRadOutAbs.allocate(3);
    state->dataSurface->SurfWinDividerQRadInAbs.allocate(3);
    state->dataHeatBalSurf->SurfCTFConstInPart.allocate(3);
    state->dataHeatBalSurf->SurfOpaqQRadSWInAbs.allocate(3);
    state->dataHeatBalSurf->SurfQAdditionalHeatSourceInside.allocate(3);
    state->dataHeatBalFanSys->QCoolingPanelSurf.allocate(3);
    state->dataHeatBalFanSys->QRadSurfAFNDuct.allocate(3);
    state->dataHeatBalSurf->SurfTempDiv.allocate(3);
    state->dataHeatBalSurf->SurfIsOperatingPool.allocate(3);
    state->dataHeatBalFanSys->PoolHeatTransCoefs.allocate(3);
    state->dataHeatBalSurf->SurfIsOperatingPool.allocate(3);
    state->dataHeatBalSurf->SurfTempTerm.allocate(3);
    state->dataHeatBalSurf->SurfNetLWRadToSurf.allocate(3);
    state->dataHeatBalSurf->SurfQSourceSinkHist.allocate(3);
    state->dataHeatBalFanSys->QPoolSurfNumerator.allocate(3);
    state->dataHeatBalSurf->SurfHcExt.allocate(3);
    state->dataHeatBalSurf->SurfHSkyExt.allocate(3);
    state->dataHeatBalSurf->SurfHGrdExt.allocate(3);
    state->dataHeatBalSurf->SurfHAirExt.allocate(3);
    state->dataHeatBalSurf->SurfTempInRep.allocate(3);
    state->dataHeatBalSurf->SurfTempOut.allocate(3);
    state->dataSurface->SurfWinHeatGainRepEnergy.allocate(3);
    state->dataSurface->SurfWinHeatTransferRepEnergy.allocate(3);

    state->dataHeatBal->ZoneMRT.allocate(1);

    state->dataHeatBalSurf->SurfTempTerm(surfNum2) = 1.0;
    state->dataHeatBalSurf->SurfQSourceSinkHist(surfNum2) = 1.0;
    state->dataHeatBalSurf->SurfCTFConstInPart(surfNum2) = 1.0;
    state->dataHeatBalFanSys->QPoolSurfNumerator(surfNum2) = 1.0;
    state->dataHeatBalSurf->SurfTempOutHist(surfNum2) = 1.0;
    state->dataHeatBalSurf->SurfTempDiv(surfNum2) = 1.0;

    CalcHeatBalanceInsideSurf2CTFOnly(*state, 1, state->dataGlobal->NumOfZones, state->dataSurface->AllIZSurfaceList);

    Real64 QdotConvInRepOld = state->dataHeatBalSurf->QdotConvInRep(surfNum2);
    Real64 QdotConvInRepPerAreaOld = state->dataHeatBalSurf->QdotConvInRepPerArea(surfNum2);
    Real64 QConvInReportOld = state->dataHeatBalSurf->QConvInReport(surfNum2);
    Real64 SurfHConvIntOld = state->dataHeatBalSurf->SurfHConvInt(surfNum2);
    Real64 SurfTempInOld = state->dataHeatBalSurf->SurfTempIn(surfNum2);
    Real64 RefAirTempOld = state->dataHeatBalSurfMgr->RefAirTemp(surfNum2);

    state->dataHeatBal->CoeffAdjRatioIn(surfNum2) = 1.3;
    state->dataHeatBalSurf->CoeffAdjRatioOut(surfNum2) = 1.3;
    CalcHeatBalanceInsideSurf2CTFOnly(*state, 1, state->dataGlobal->NumOfZones, state->dataSurface->AllIZSurfaceList);

    Real64 ratio = 1.3 * state->dataHeatBalSurf->SurfHConvInt(surfNum2) / SurfHConvIntOld *
                   (state->dataHeatBalSurf->SurfTempIn(surfNum2) - state->dataHeatBalSurfMgr->RefAirTemp(surfNum2)) / (SurfTempInOld - RefAirTempOld);

    EXPECT_NEAR(ratio,
                state->dataHeatBalSurf->QdotConvInRep(surfNum2) / QdotConvInRepOld,
                0.001);
    EXPECT_NEAR(ratio,
                state->dataHeatBalSurf->QdotConvInRepPerArea(surfNum2) / QdotConvInRepPerAreaOld,
                0.001);
    EXPECT_NEAR(ratio, state->dataHeatBalSurf->QConvInReport(surfNum2) / QConvInReportOld, 0.001);
}
} // namespace EnergyPlus
