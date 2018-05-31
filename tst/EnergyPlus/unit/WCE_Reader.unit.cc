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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataIPShortCuts.hh>
/*#include <ConvectionCoefficients.hh>
#include <CurveManager.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataSurfaces.hh>
#include <ElectricPowerServiceManager.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/SimulationManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <HeatBalanceIntRadExchange.hh>
#include <HeatBalanceManager.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <SolarShading.hh>*/
#include <HeatBalanceManager.hh>
#include <WindowManager.hh>
#include <WindowManagerExteriorData.hh>
#include <WCEMultiLayerOptics.hpp>

#include "Fixtures/EnergyPlusFixture.hh"
#include "Windows-CalcEngine/src/Common/src/FenestrationCommon.hpp"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, WCEReader)
{

    DataIPShortCuts::lAlphaFieldBlanks = true;
    bool ErrorsFound(false);

    std::string const idf_objects =
        delimited_string({"Version,8.9;",
                          "WindowsCalculationEngine,",
                          "ExternalWindowsModel;",
                          "WindowMaterial:Glazing,",
                          "Glass_102_LayerAvg,      !- Name",
                          "SpectralAverage,         !- Optical Data Type",
                          ",                        !- Window Glass Spectral Data Set Name",
                          "0.003048,                !- Thickness {m}",
                          "0.833848,                !- Solar Transmittance at Normal Incidence",
                          "7.476376e-002,           !- Front Side Solar Reflectance at Normal Incidence",
                          "7.485449e-002,           !- Back Side Solar Reflectance at Normal Incidence",
                          "0.899260,                !- Visible Transmittance at Normal Incidence",
                          "0.082563,                !- Front Side Visible Reflectance at Normal Incidence",
                          "0.082564,                !- Back Side Visible Reflectance at Normal Incidence",
                          "0.000000,                !- Infrared Transmittance at Normal Incidence",
                          "0.840000,                !- Front Side Infrared Hemispherical Emissivity",
                          "0.840000,                !- Back Side Infrared Hemispherical Emissivity",
                          "1.000000;                !- Conductivity {W/m-K}",
                          "CONSTRUCTION,",
                          "GlzSys_1,                !- Name",
                          "Glass_102_LayerAvg;      !- Outside Layer",
                          "CONSTRUCTION,",
                          "GlzSys_1_withShade,      !- Name",
                          "Glass_102_LayerAvg,      !- Outside Layer",
                          "VenBlind_ShdDvc_25;      !- Layer 2",
                          "WindowMaterial:Blind,",
                          "VenBlind_ShdDvc_25,      !- Name",
                          "HORIZONTAL,              !- Slat Orientation",
                          "0.016,                   !- Slat Width {m}",
                          "0.012,                   !- Slat Separation {m}",
                          "0.0006,                  !- Slat Thickness {m}",
                          "135,                     !- Slat Angle {deg}",
                          "160,                     !- Slat Conductivity {W/m-K}",
                          ",                        !- Slat Beam Solar Transmittance",
                          "0.7,                     !- Front Side Slat Beam Solar Reflectance",
                          "0.7,                     !- Back Side Slat Beam Solar Reflectance",
                          "0,                       !- Slat Diffuse Solar Transmittance",
                          "0.7,                     !- Front Side Slat Diffuse Solar Reflectance",
                          "0.7,                     !- Back Side Slat Diffuse Solar Reflectance",
                          "0,                       !- Slat Beam Visible Transmittance",
                          "0.7,                     !- Front Side Slat Beam Visible Reflectance",
                          "0.7,                     !- Back Side Slat Beam Visible Reflectance",
                          "0,                       !- Slat Diffuse Visible Transmittance",
                          "0.7,                     !- Front Side Slat Diffuse Visible Reflectance",
                          "0.7,                     !- Back Side Slat Diffuse Visible Reflectance",
                          "0,                       !- Slat Infrared Hemispherical Transmittance",
                          "0.9,                     !- Front Side Slat Infrared Hemispherical Emissivity",
                          "0.9,                     !- Back Side Slat Infrared Hemispherical Emissivity",
                          "0.0127,                  !- Blind to Glass Distance {m}",
                          "0.0,                     !- Blind Top Opening Multiplier",
                          "0.0,                     !- Blind Bottom Opening Multiplier",
                          "0.0,                     !- Blind Left Side Opening Multiplier",
                          "0.0,                     !- Blind Right Side Opening Multiplier",
                          "0,                       !- Minimum Slat Angle {deg}",
                          "0;                       !- Maximum Slat Angle {deg}"
                          });

    ASSERT_TRUE(process_idf(idf_objects));

    HeatBalanceManager::GetMaterialData(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    WindowManager::initWindowModel();
    WindowManager::InitWindowOpticalCalculations();
    HeatBalanceManager::InitHeatBalance();

    auto aWinConstSimp = WindowManager::CWindowConstructionsSimplified::instance();
    auto solarLayer = aWinConstSimp.getEquivalentLayer(FenestrationCommon::WavelengthRange::Solar, 1);

    // Transmittance Front
    const auto Tfront = solarLayer->getPropertySimple(FenestrationCommon::PropertySimple::T, FenestrationCommon::Side::Front, 
        FenestrationCommon::Scattering::DirectDirect);
    EXPECT_NEAR(0.8239, Tfront, 1e-6);

    // Reflectance Front
    const auto Rfront = solarLayer->getPropertySimple(FenestrationCommon::PropertySimple::R, FenestrationCommon::Side::Front,
        FenestrationCommon::Scattering::DirectDirect);
    EXPECT_NEAR(0.073578, Rfront, 1e-6);

    // Transmittance Back
    const auto Tback = solarLayer->getPropertySimple(FenestrationCommon::PropertySimple::T, FenestrationCommon::Side::Back,
        FenestrationCommon::Scattering::DirectDirect);
    EXPECT_NEAR(0.8239, Tback, 1e-6);

    // Reflectance Back
    const auto Rback = solarLayer->getPropertySimple(FenestrationCommon::PropertySimple::R, FenestrationCommon::Side::Back,
        FenestrationCommon::Scattering::DirectDirect);
    EXPECT_NEAR(0.073682, Rback, 1e-6);
}
