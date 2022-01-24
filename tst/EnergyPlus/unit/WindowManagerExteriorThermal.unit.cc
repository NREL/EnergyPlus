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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/WindowManager.hh>

// Windows library headers
#include <WCEMultiLayerOptics.hpp>
#include <WCETarcog.hpp>

// EnergyPlus headers
#include <EnergyPlus/WindowManagerExteriorThermal.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::WindowManager;

TEST_F(EnergyPlusFixture, test_overallUfactorFromFilmsAndCond)
{
    // set up for using CWCEHeatTransferFactory
    int numSurf = 1;
    state->dataSurface->Surface.allocate(numSurf);
    state->dataSurface->SurfaceWindow.allocate(numSurf);
    int numCons = 1;
    state->dataConstruction->Construct.allocate(numCons);
    state->dataSurface->Surface(numSurf).Construction = numCons;
    int numLayers = 2;
    state->dataConstruction->Construct(numCons).LayerPoint.allocate(numLayers);
    int materialOutside = 1;
    int materialInside = 2;
    state->dataConstruction->Construct(numCons).TotLayers = numLayers;
    state->dataConstruction->Construct(numCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(numCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(numCons).AbsDiff.allocate(2);
    int numMaterials = materialInside;
    state->dataMaterial->Material.allocate(numMaterials);
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    auto aFactory = CWCEHeatTransferFactory(*state, state->dataSurface->Surface(numSurf), numSurf, numCons);

    double hIntConvCoeff = 0.;
    double hExtConvCoeff = 0.;
    double conductance = 0.;

    double uvalue = aFactory.overallUfactorFromFilmsAndCond(conductance, hIntConvCoeff, hExtConvCoeff);
    EXPECT_NEAR(uvalue, 0., 0.0001);

    conductance = 0.5;
    uvalue = aFactory.overallUfactorFromFilmsAndCond(conductance, hIntConvCoeff, hExtConvCoeff);
    EXPECT_NEAR(uvalue, 0., 0.0001);

    hIntConvCoeff = 1.;
    hExtConvCoeff = 1.;
    conductance = 1.;
    uvalue = aFactory.overallUfactorFromFilmsAndCond(conductance, hIntConvCoeff, hExtConvCoeff);
    EXPECT_NEAR(uvalue, 0.33333, 0.0001);

    hIntConvCoeff = 8.;
    hExtConvCoeff = 30.;
    conductance = 2.326112;
    uvalue = aFactory.overallUfactorFromFilmsAndCond(conductance, hIntConvCoeff, hExtConvCoeff);
    EXPECT_NEAR(uvalue, 1.700, 0.001);

    hIntConvCoeff = 30.;
    hExtConvCoeff = 8.;
    conductance = 3.543645;
    uvalue = aFactory.overallUfactorFromFilmsAndCond(conductance, hIntConvCoeff, hExtConvCoeff);
    EXPECT_NEAR(uvalue, 2.270, 0.001);
}

TEST_F(EnergyPlusFixture, test_getOutdoorNfrc)
{
    // set up for using CWCEHeatTransferFactory
    int numSurf = 1;
    state->dataSurface->Surface.allocate(numSurf);
    state->dataSurface->SurfaceWindow.allocate(numSurf);
    int numCons = 1;
    state->dataConstruction->Construct.allocate(numCons);
    state->dataSurface->Surface(numSurf).Construction = numCons;
    int numLayers = 2;
    state->dataConstruction->Construct(numCons).LayerPoint.allocate(numLayers);
    int materialOutside = 1;
    int materialInside = 2;
    state->dataConstruction->Construct(numCons).TotLayers = numLayers;
    state->dataConstruction->Construct(numCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(numCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(numCons).AbsDiff.allocate(2);
    int numMaterials = materialInside;
    state->dataMaterial->Material.allocate(numMaterials);
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    auto aFactory = CWCEHeatTransferFactory(*state, state->dataSurface->Surface(numSurf), numSurf, numCons);

    auto indoor = aFactory.getOutdoorNfrc(true);
    EXPECT_NEAR(indoor->getAirTemperature(), 305.15, 0.01);
    EXPECT_NEAR(indoor->getDirectSolarRadiation(), 783.0, 0.01);

    indoor = aFactory.getOutdoorNfrc(false);
    EXPECT_NEAR(indoor->getAirTemperature(), 255.15, 0.01);
    EXPECT_NEAR(indoor->getDirectSolarRadiation(), 0.0, 0.01);
}

TEST_F(EnergyPlusFixture, test_getIndoorNfrc)
{
    // set up for using CWCEHeatTransferFactory
    int numSurf = 1;
    state->dataSurface->Surface.allocate(numSurf);
    state->dataSurface->SurfaceWindow.allocate(numSurf);
    int numCons = 1;
    state->dataConstruction->Construct.allocate(numCons);
    state->dataSurface->Surface(numSurf).Construction = numCons;
    int numLayers = 2;
    state->dataConstruction->Construct(numCons).LayerPoint.allocate(numLayers);
    int materialOutside = 1;
    int materialInside = 2;
    state->dataConstruction->Construct(numCons).TotLayers = numLayers;
    state->dataConstruction->Construct(numCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(numCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(numCons).AbsDiff.allocate(2);
    int numMaterials = materialInside;
    state->dataMaterial->Material.allocate(numMaterials);
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    auto aFactory = CWCEHeatTransferFactory(*state, state->dataSurface->Surface(numSurf), numSurf, numCons);

    auto indoor = aFactory.getIndoorNfrc(true);
    EXPECT_NEAR(indoor->getAirTemperature(), 297.15, 0.01);

    indoor = aFactory.getIndoorNfrc(false);
    EXPECT_NEAR(indoor->getAirTemperature(), 294.15, 0.01);
}

TEST_F(EnergyPlusFixture, test_getShadeType)
{
    // set up for using CWCEHeatTransferFactory
    int numSurf = 1;
    state->dataSurface->Surface.allocate(numSurf);
    state->dataSurface->SurfaceWindow.allocate(numSurf);
    int numCons = 2;
    int simpleCons = 1;
    state->dataConstruction->Construct.allocate(numCons);
    state->dataSurface->Surface(numSurf).Construction = simpleCons;
    int numLayers = 3;
    state->dataConstruction->Construct(simpleCons).LayerPoint.allocate(numLayers);
    int materialOutside = 1;
    int materialInside = 2;
    state->dataConstruction->Construct(simpleCons).TotLayers = numLayers;
    state->dataConstruction->Construct(simpleCons).TotGlassLayers = numLayers - 1;
    state->dataConstruction->Construct(simpleCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(simpleCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(simpleCons).AbsDiff.allocate(2);
    int numMaterials = materialInside + 1;
    state->dataMaterial->Material.allocate(numMaterials);
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    auto aFactory = CWCEHeatTransferFactory(*state, state->dataSurface->Surface(numSurf), numSurf, simpleCons);

    //outside
    auto typeOfShade = aFactory.getShadeType(*state, simpleCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::NoShade);

    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::Shade;
    typeOfShade = aFactory.getShadeType(*state, simpleCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::ExtShade);

    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowBlind;
    typeOfShade = aFactory.getShadeType(*state, simpleCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::ExtBlind);

    //reset the outside to glass
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;

    //inside
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::Shade;
    typeOfShade = aFactory.getShadeType(*state, simpleCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::IntShade);
    
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowBlind;
    typeOfShade = aFactory.getShadeType(*state, simpleCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::IntBlind);

    // reset the outside to glass
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;

    // between glass - double pane
    int betweenCons = 2;
    state->dataSurface->Surface(numSurf).Construction = betweenCons;
    numLayers = 4;
    int shadeLayer = 3;
    state->dataConstruction->Construct(betweenCons).LayerPoint.allocate(numLayers);
    int materialShade = 3;
    state->dataConstruction->Construct(betweenCons).TotLayers = numLayers;
    state->dataConstruction->Construct(betweenCons).TotGlassLayers = 2;
    state->dataConstruction->Construct(betweenCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(betweenCons).LayerPoint(shadeLayer) = materialShade;
    state->dataConstruction->Construct(betweenCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(betweenCons).AbsDiff.allocate(2);

    state->dataMaterial->Material(materialShade).Group = DataHeatBalance::MaterialGroup::Shade;
    typeOfShade = aFactory.getShadeType(*state, betweenCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::BGShade);

    state->dataMaterial->Material(materialShade).Group = DataHeatBalance::MaterialGroup::WindowBlind;
    typeOfShade = aFactory.getShadeType(*state, betweenCons);
    EXPECT_EQ(typeOfShade, DataSurfaces::WinShadingType::BGBlind);
}

TEST_F(EnergyPlusFixture, test_getActiveConstructionNumber)
{
    // set up for using CWCEHeatTransferFactory
    int numSurf = 1;
    state->dataSurface->Surface.allocate(numSurf);
    state->dataSurface->SurfaceWindow.allocate(numSurf);
    state->dataSurface->SurfWinShadingFlag.allocate(numSurf);
    state->dataSurface->SurfWinActiveShadedConstruction.allocate(numSurf);

    int numCons = 2;
    state->dataConstruction->Construct.allocate(numCons);
    state->dataSurface->Surface(numSurf).Construction = numCons;
    int numLayers = 2;
    state->dataConstruction->Construct(numCons).LayerPoint.allocate(numLayers);
    int materialOutside = 1;
    int materialInside = 2;
    state->dataConstruction->Construct(numCons).TotLayers = numLayers;
    state->dataConstruction->Construct(numCons).LayerPoint(1) = materialOutside;
    state->dataConstruction->Construct(numCons).LayerPoint(numLayers) = materialInside;
    state->dataConstruction->Construct(numCons).AbsDiff.allocate(2);
    int numMaterials = materialInside;
    state->dataMaterial->Material.allocate(numMaterials);
    state->dataMaterial->Material(materialOutside).Group = DataHeatBalance::MaterialGroup::WindowGlass;
    state->dataMaterial->Material(materialInside).Group = DataHeatBalance::MaterialGroup::WindowGlass;

    auto aFactory = CWCEHeatTransferFactory(*state, state->dataSurface->Surface(numSurf), numSurf, numCons);

    auto &surface(state->dataSurface->Surface(numSurf));
    state->dataSurface->SurfWinShadingFlag(numSurf) = DataSurfaces::WinShadingType::ExtBlind;
    state->dataSurface->SurfWinActiveShadedConstruction(numSurf) = 7;

    int consSelected = aFactory.getActiveConstructionNumber(*state, surface, numSurf);
    EXPECT_EQ(consSelected, 7);

    state->dataSurface->SurfWinShadingFlag(numSurf) = DataSurfaces::WinShadingType::NoShade;

    consSelected = aFactory.getActiveConstructionNumber(*state, surface, numSurf);
    EXPECT_EQ(consSelected, numCons);
}


