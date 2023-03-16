// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::DataHeatBalance {

// MODULE INFORMATION:
//       AUTHOR         Rick Strand
//       DATE WRITTEN   August 1997 (rewritten)
//       MODIFIED       Aug-Oct 1997 RKS (added derived types)
//       MODIFIED       Feb 1999 (FW) Added visible radiation parameters,
//                      WindowShadingControl type and SurfaceWindowCalc type
//                      Sep 1999 (FW) Added/modified Window4 gas variables
//                      Jul 2003 (CC) Added reference temperature variable for air models
//                      Aug 2003 (FW) Added FractionReturnAirPlenTempCoeff1 and
//                      FractionReturnAirPlenTempCoeff2 to Type LightsData
//                      Nov 2003 (FW) Add FullExteriorWithRefl and FullInteriorExteriorWithRefl
//                       as SolarDistribution values
//                      Dec 2003 (PGE) Added Zone List and Zone Group; added SNLoad variables
//                      August 2006 (COP) Added variable k coefficient and PCM enthalpy.
//                      Dec 2006 (DJS-PSU) Added ecoroof material
//                      Dec 2008 TH added new properties to MaterialProperties and
//                              ConstructionData for thermochromic windows
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module should contain the information that is needed to pass
// from the Heat Balance Module and all of the Zone Initializations
// such as ConductionTransferFunction, GlassCalculation,
// SolarShading, etc. Modules.

// Using/Aliasing
using namespace DataVectorTypes;
using DataBSDFWindow::BSDFLayerAbsorpStruct;
using DataBSDFWindow::BSDFWindowInputStruct;

// Functions

Real64 SpaceData::sumHATsurf(EnergyPlusData &state)
{
    // PURPOSE OF THIS FUNCTION:
    // This function calculates the space sum of Hc*Area*Tsurf.

    Real64 sumHATsurf = 0.0;

    for (int surfNum = this->HTSurfaceFirst; surfNum <= this->HTSurfaceLast; ++surfNum) {
        Real64 Area = state.dataSurface->Surface(surfNum).Area;

        if (state.dataSurface->Surface(surfNum).Class == DataSurfaces::SurfaceClass::Window) {
            if (state.dataSurface->SurfWinDividerArea(surfNum) > 0.0) {
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(surfNum))) {
                    // The area is the shade or blind area = sum of the glazing area and the divider area (which is zero if no divider)
                    Area += state.dataSurface->SurfWinDividerArea(surfNum);
                } else {
                    // Window divider contribution (only for window with divider and no interior shade or blind)
                    sumHATsurf += state.dataHeatBalSurf->SurfHConvInt(surfNum) * state.dataSurface->SurfWinDividerArea(surfNum) *
                                  (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(surfNum)) * state.dataSurface->SurfWinDividerTempIn(surfNum);
                }
            }

            if (state.dataSurface->SurfWinFrameArea(surfNum) > 0.0) {
                // Window frame contribution
                sumHATsurf += state.dataHeatBalSurf->SurfHConvInt(surfNum) * state.dataSurface->SurfWinFrameArea(surfNum) *
                              (1.0 + state.dataSurface->SurfWinProjCorrFrIn(surfNum)) * state.dataSurface->SurfWinFrameTempIn(surfNum);
            }
        }

        sumHATsurf += state.dataHeatBalSurf->SurfHConvInt(surfNum) * Area * state.dataHeatBalSurf->SurfTempInTmp(surfNum);
    }

    return sumHATsurf;
}

Real64 ZoneData::sumHATsurf(EnergyPlusData &state)
{
    Real64 sumHATsurf = 0.0;
    for (int spaceNum : this->spaceIndexes) {
        sumHATsurf += state.dataHeatBal->space(spaceNum).sumHATsurf(state);
    }
    return sumHATsurf;
}
void ZoneData::SetOutBulbTempAt(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Noel Keen (LBL)/Linda Lawrie
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Routine provides facility for doing bulk Set Temperature at Height.

    if (state.dataEnvrn->SiteTempGradient == 0.0) {
        OutDryBulbTemp = state.dataEnvrn->OutDryBulbTemp;
        OutWetBulbTemp = state.dataEnvrn->OutWetBulbTemp;
    } else {
        // Base temperatures at Z = 0 (C)
        Real64 const BaseDryTemp(state.dataEnvrn->OutDryBulbTemp + state.dataEnvrn->WeatherFileTempModCoeff);
        Real64 const BaseWetTemp(state.dataEnvrn->OutWetBulbTemp + state.dataEnvrn->WeatherFileTempModCoeff);

        Real64 const Z(Centroid.z); // Centroid value
        if (Z <= 0.0) {
            OutDryBulbTemp = BaseDryTemp;
            OutWetBulbTemp = BaseWetTemp;
        } else {
            OutDryBulbTemp = BaseDryTemp - state.dataEnvrn->SiteTempGradient * DataEnvironment::EarthRadius * Z / (DataEnvironment::EarthRadius + Z);
            OutWetBulbTemp = BaseWetTemp - state.dataEnvrn->SiteTempGradient * DataEnvironment::EarthRadius * Z / (DataEnvironment::EarthRadius + Z);
        }
    }
}

void ZoneData::SetWindSpeedAt(EnergyPlusData &state, Real64 const fac)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Routine provides facility for doing bulk Set Windspeed at Height.

    if (state.dataEnvrn->SiteWindExp == 0.0) {
        WindSpeed = state.dataEnvrn->WindSpeed;
    } else {
        Real64 const Z(Centroid.z); // Centroid value
        if (Z <= 0.0) {
            WindSpeed = 0.0;
        } else {
            //  [Met] - at meterological Station, Height of measurement is usually 10m above ground
            //  LocalWindSpeed = Windspeed [Met] * (Wind Boundary LayerThickness [Met]/Height [Met])**Wind Exponent[Met] &
            //                     * (Height above ground / Site Wind Boundary Layer Thickness) ** Site Wind Exponent
            WindSpeed = fac * std::pow(Z, state.dataEnvrn->SiteWindExp);
        }
    }
}

void ZoneData::SetWindDirAt(Real64 const fac)
{
    WindDir = fac;
}

void AirReportVars::setUpOutputVars(EnergyPlusData &state, std::string_view prefix, std::string_view name)
{
    SetupOutputVariable(state,
                        format("{} Mean Air Temperature", prefix),
                        OutputProcessor::Unit::C,
                        this->MeanAirTemp,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Operative Temperature", prefix),
                        OutputProcessor::Unit::C,
                        this->OperativeTemp,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Mean Air Dewpoint Temperature", prefix),
                        OutputProcessor::Unit::C,
                        this->MeanAirDewPointTemp,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Mean Air Humidity Ratio", prefix),
                        OutputProcessor::Unit::kgWater_kgDryAir,
                        this->MeanAirHumRat,
                        OutputProcessor::SOVTimeStepType::Zone,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance Internal Convective Heat Gain Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumIntGains,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance Surface Convection Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumHADTsurfs,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance Interzone Air Transfer Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumMCpDTzones,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance Outdoor Air Transfer Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumMCpDtInfil,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance System Air Transfer Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumMCpDTsystem,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance System Convective Heat Gain Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->SumNonAirSystem,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    SetupOutputVariable(state,
                        format("{} Air Heat Balance Air Energy Storage Rate", prefix),
                        OutputProcessor::Unit::W,
                        this->CzdTdt,
                        OutputProcessor::SOVTimeStepType::System,
                        OutputProcessor::SOVStoreType::Average,
                        name);
    if (state.dataGlobal->DisplayAdvancedReportVariables) {
        SetupOutputVariable(state,
                            format("{} Air Heat Balance Deviation Rate", prefix),
                            OutputProcessor::Unit::W,
                            this->imBalance,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            name);
    }
}

void SetZoneOutBulbTempAt(EnergyPlusData &state)
{
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetOutBulbTempAt(state);
    }
}

void CheckZoneOutBulbTempAt(EnergyPlusData &state)
{
    // Using/Aliasing
    using DataEnvironment::SetOutBulbTempAt_error;

    Real64 minBulb = 0.0;
    for (auto &zone : state.dataHeatBal->Zone) {
        minBulb = min(minBulb, zone.OutDryBulbTemp, zone.OutWetBulbTemp);
        if (minBulb < -100.0) SetOutBulbTempAt_error(state, "Zone", zone.Centroid.z, zone.Name);
    }
}

void SetZoneWindSpeedAt(EnergyPlusData &state)
{
    Real64 const fac(state.dataEnvrn->WindSpeed * state.dataEnvrn->WeatherFileWindModCoeff *
                     std::pow(state.dataEnvrn->SiteWindBLHeight, -state.dataEnvrn->SiteWindExp));
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetWindSpeedAt(state, fac);
    }
}

void SetZoneWindDirAt(EnergyPlusData &state)
{
    // Using/Aliasing
    Real64 const fac(state.dataEnvrn->WindDir);
    for (auto &zone : state.dataHeatBal->Zone) {
        zone.SetWindDirAt(fac);
    }
}

void CheckAndSetConstructionProperties(EnergyPlusData &state,
                                       int const ConstrNum, // Construction number to be set/checked
                                       bool &ErrorsFound    // error flag that is set when certain errors have occurred
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2006

    // This routine checks some properties of entered constructions; sets some properties; and sets
    // an error flag for certain error conditions.

    auto &thisConstruct = state.dataConstruction->Construct(ConstrNum);
    int TotLayers = thisConstruct.TotLayers;                // Number of layers in a construction
    if (TotLayers == 0) return;                             // error condition, hopefully caught elsewhere
    int InsideLayer = TotLayers;                            // Inside Layer of Construct; for window construct, layer no. of inside glass
    if (thisConstruct.LayerPoint(InsideLayer) <= 0) return; // Error condition

    //   window screen is not allowed on inside layer

    thisConstruct.DayltPropPtr = 0;
    int InsideMaterNum = thisConstruct.LayerPoint(InsideLayer); // Material "number" of the Inside layer
    if (InsideMaterNum != 0) {
        auto const *thisMaterialInside = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(InsideMaterNum));
        thisConstruct.InsideAbsorpVis = thisMaterialInside->AbsorpVisible;
        thisConstruct.InsideAbsorpSolar = thisMaterialInside->AbsorpSolar;

        // Following line applies only to opaque surfaces; it is recalculated later for windows.
        thisConstruct.ReflectVisDiffBack = 1.0 - thisMaterialInside->AbsorpVisible;
    }

    int OutsideMaterNum = thisConstruct.LayerPoint(1); // Material "number" of the Outside layer
    if (OutsideMaterNum != 0) {
        auto const *thisMaterialOutside = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(OutsideMaterNum));
        assert(thisMaterialOutside != nullptr);
        thisConstruct.OutsideAbsorpVis = thisMaterialOutside->AbsorpVisible;
        thisConstruct.OutsideAbsorpSolar = thisMaterialOutside->AbsorpSolar;
    }

    thisConstruct.TotSolidLayers = 0;
    thisConstruct.TotGlassLayers = 0;
    thisConstruct.AbsDiffShade = 0.0;

    // Check if any layer is glass, gas, shade, screen or blind; if so it is considered a window construction for
    // purposes of error checking.

    thisConstruct.TypeIsWindow = false;
    for (int Layer = 1; Layer <= TotLayers; ++Layer) {
        int const MaterNum = thisConstruct.LayerPoint(Layer);
        if (MaterNum == 0) continue; // error -- has been caught will stop program later
        auto const *thisMaterial = state.dataMaterial->Material(MaterNum);
        thisConstruct.TypeIsWindow =
            (thisMaterial->group == Material::Group::WindowGlass || thisMaterial->group == Material::Group::WindowGas ||
             thisMaterial->group == Material::Group::WindowGasMixture || thisMaterial->group == Material::Group::Shade ||
             thisMaterial->group == Material::Group::WindowBlind || thisMaterial->group == Material::Group::Screen ||
             thisMaterial->group == Material::Group::WindowSimpleGlazing || thisMaterial->group == Material::Group::ComplexWindowShade ||
             thisMaterial->group == Material::Group::ComplexWindowGap || thisMaterial->group == Material::Group::GlassEquivalentLayer ||
             thisMaterial->group == Material::Group::ShadeEquivalentLayer || thisMaterial->group == Material::Group::DrapeEquivalentLayer ||
             thisMaterial->group == Material::Group::ScreenEquivalentLayer || thisMaterial->group == Material::Group::BlindEquivalentLayer ||
             thisMaterial->group == Material::Group::GapEquivalentLayer);
        bool TypeIsNotWindow = (thisMaterial->group == Material::Group::Invalid || thisMaterial->group == Material::Group::Air ||
                                thisMaterial->group == Material::Group::Regular || thisMaterial->group == Material::Group::EcoRoof ||
                                thisMaterial->group == Material::Group::IRTransparent);
        if (!thisConstruct.TypeIsWindow && !TypeIsNotWindow) assert(false);
    }

    if (InsideMaterNum == 0) return;
    auto const *thisMaterialInside = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(InsideMaterNum));
    if (OutsideMaterNum == 0) return;
    auto const *thisMaterialOutside = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(OutsideMaterNum));

    if (thisConstruct.TypeIsWindow) {

        bool WrongMaterialsMix = false;
        thisConstruct.NumCTFTerms = 0;
        thisConstruct.NumHistories = 0;
        for (int Layer = 1; Layer <= TotLayers; ++Layer) {
            int const MaterNum = thisConstruct.LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            auto const *thisMaterial = state.dataMaterial->Material(MaterNum);
            WrongMaterialsMix =
                !((thisMaterial->group == Material::Group::WindowGlass) || (thisMaterial->group == Material::Group::WindowGas) ||
                  (thisMaterial->group == Material::Group::WindowGasMixture) || (thisMaterial->group == Material::Group::Shade) ||
                  (thisMaterial->group == Material::Group::WindowBlind) || (thisMaterial->group == Material::Group::Screen) ||
                  (thisMaterial->group == Material::Group::WindowSimpleGlazing) || (thisMaterial->group == Material::Group::ComplexWindowShade) ||
                  (thisMaterial->group == Material::Group::ComplexWindowGap) || (thisMaterial->group == Material::Group::GlassEquivalentLayer) ||
                  (thisMaterial->group == Material::Group::ShadeEquivalentLayer) || (thisMaterial->group == Material::Group::DrapeEquivalentLayer) ||
                  (thisMaterial->group == Material::Group::ScreenEquivalentLayer) || (thisMaterial->group == Material::Group::BlindEquivalentLayer) ||
                  (thisMaterial->group == Material::Group::GapEquivalentLayer));
        }

        if (WrongMaterialsMix) { // Illegal material for a window construction
            ShowSevereError(state,
                            format("Error: Window construction={} has materials other than glass, gas, shade, screen, blind, complex shading, "
                                   "complex gap, or simple system.",
                                   thisConstruct.Name));
            ErrorsFound = true;
            // Do not check number of layers for BSDF type of window since that can be handled
        } else if ((TotLayers > 8) && (!thisConstruct.WindowTypeBSDF) &&
                   (!thisConstruct.WindowTypeEQL)) { // Too many layers for a window construction
            ShowSevereError(state,
                            format("CheckAndSetConstructionProperties: Window construction={} has too many layers (max of 8 allowed -- 4 glass + 3 "
                                   "gap + 1 shading device).",
                                   thisConstruct.Name));
            ErrorsFound = true;

        } else if (TotLayers == 1) {
            auto const *thisMaterial = state.dataMaterial->Material(thisConstruct.LayerPoint(1));
            Material::Group thisMaterialGroup = thisMaterial->group;
            if ((thisMaterialGroup == Material::Group::Shade) || (thisMaterialGroup == Material::Group::WindowGas) ||
                (thisMaterialGroup == Material::Group::WindowGasMixture) || (thisMaterialGroup == Material::Group::WindowBlind) ||
                (thisMaterialGroup == Material::Group::Screen) || (thisMaterialGroup == Material::Group::ComplexWindowShade) ||
                (thisMaterialGroup == Material::Group::ComplexWindowGap)) {
                ShowSevereError(state,
                                format("CheckAndSetConstructionProperties: The single-layer window construction={} has a gas, complex gap, shade, "
                                       "complex shade, screen or blind material; it should be glass of simple glazing system.",
                                       thisConstruct.Name));
                ErrorsFound = true;
            }
        }

        // Find total glass layers, total shade/blind layers and total gas layers in a window construction

        bool WrongWindowLayering = false;
        int TotGlassLayers = 0;
        int TotShadeLayers = 0; // Includes shades, blinds, and screens
        int TotGasLayers = 0;
        for (int Layer = 1; Layer <= TotLayers; ++Layer) {
            int const MaterNum = thisConstruct.LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            auto const *thisMaterial = state.dataMaterial->Material(MaterNum);
            if (thisMaterial->group == Material::Group::WindowGlass) ++TotGlassLayers;
            if (thisMaterial->group == Material::Group::WindowSimpleGlazing) ++TotGlassLayers;
            if (thisMaterial->group == Material::Group::Shade || thisMaterial->group == Material::Group::WindowBlind ||
                thisMaterial->group == Material::Group::Screen || thisMaterial->group == Material::Group::ComplexWindowShade)
                ++TotShadeLayers;
            if (thisMaterial->group == Material::Group::WindowGas || thisMaterial->group == Material::Group::WindowGasMixture ||
                thisMaterial->group == Material::Group::ComplexWindowGap)
                ++TotGasLayers;
            if (Layer < TotLayers) {
                int const MaterNumNext = thisConstruct.LayerPoint(Layer + 1);
                // Adjacent layers of same type not allowed
                if (MaterNumNext == 0) continue;
                if (thisMaterial->group == state.dataMaterial->Material(MaterNumNext)->group) WrongWindowLayering = true;
            }
        }

        // It is not necessary to check rest of BSDF window structure since that is performed inside TARCOG90 routine.
        // That routine also allow structures which are not allowed in rest of this routine
        if (thisConstruct.WindowTypeBSDF) {
            thisConstruct.TotGlassLayers = TotGlassLayers;
            thisConstruct.TotSolidLayers = TotGlassLayers + TotShadeLayers;
            thisConstruct.InsideAbsorpThermal = thisMaterialInside->AbsorpThermalBack;
            thisConstruct.OutsideAbsorpThermal = thisMaterialOutside->AbsorpThermalFront;
            return;
        }

        if (thisConstruct.WindowTypeEQL) {
            thisConstruct.InsideAbsorpThermal = thisMaterialInside->AbsorpThermalBack;
            thisConstruct.OutsideAbsorpThermal = thisMaterialOutside->AbsorpThermalFront;
            return;
        }

        if (thisMaterialOutside->group == Material::Group::WindowGas || thisMaterialOutside->group == Material::Group::WindowGasMixture ||
            thisMaterialInside->group == Material::Group::WindowGas || thisMaterialInside->group == Material::Group::WindowGasMixture)
            WrongWindowLayering = true;                     // Gas cannot be first or last layer
        if (TotShadeLayers > 1) WrongWindowLayering = true; // At most one shade, screen or blind allowed

        // If there is a diffusing glass layer no shade, screen or blind is allowed
        for (int Layer = 1; Layer <= TotLayers; ++Layer) {
            int const MaterNum = thisConstruct.LayerPoint(Layer);
            if (MaterNum == 0) continue; // error -- has been caught will stop program later
            auto const *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MaterNum));
            assert(thisMaterial != nullptr);
            if (thisMaterial->SolarDiffusing && TotShadeLayers > 0) {
                ErrorsFound = true;
                ShowSevereError(state, format("CheckAndSetConstructionProperties: Window construction={}", thisConstruct.Name));
                ShowContinueError(state, format("has diffusing glass={} and a shade, screen or blind layer.", thisMaterial->Name));
                break;
            }
        }

        // If there is a diffusing glass layer it must be the innermost layer
        if (TotGlassLayers > 1) {
            int GlassLayNum = 0;
            for (int Layer = 1; Layer <= TotLayers; ++Layer) {
                int const MaterNum = thisConstruct.LayerPoint(Layer);
                if (MaterNum == 0) continue; // error -- has been caught will stop program later
                auto const *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MaterNum));
                assert(thisMaterial != nullptr);
                if (thisMaterial->group == Material::Group::WindowGlass) {
                    ++GlassLayNum;
                    if (GlassLayNum < TotGlassLayers && thisMaterial->SolarDiffusing) {
                        ErrorsFound = true;
                        ShowSevereError(state, format("CheckAndSetConstructionProperties: Window construction={}", thisConstruct.Name));
                        ShowContinueError(state, format("has diffusing glass={} that is not the innermost glass layer.", thisMaterial->Name));
                    }
                }
            }
        }

        // interior window screen is not allowed. Check for invalid between-glass screen is checked below.
        if (TotShadeLayers == 1 && thisMaterialInside->group == Material::Group::Screen && TotLayers != 1) {
            WrongWindowLayering = true;
        }

        // Consistency checks for a construction with a between-glass shade or blind

        if (TotShadeLayers == 1 && thisMaterialOutside->group != Material::Group::Shade &&
            thisMaterialOutside->group != Material::Group::WindowBlind && thisMaterialOutside->group != Material::Group::Screen &&
            thisMaterialInside->group != Material::Group::Shade && thisMaterialInside->group != Material::Group::WindowBlind &&
            thisMaterialInside->group != Material::Group::ComplexWindowShade && !WrongWindowLayering) {

            // This is a construction with a between-glass shade or blind

            if (TotGlassLayers >= 4) {
                // Quadruple pane not allowed.
                WrongWindowLayering = true;
            } else if (TotGlassLayers == 2 || TotGlassLayers == 3) {
                bool ValidBGShadeBlindConst = false;
                if (TotGlassLayers == 2) {
                    if (TotLayers != 5) {
                        WrongWindowLayering = true;
                    } else {
                        if (thisMaterialOutside->group == Material::Group::WindowGlass &&
                            (state.dataMaterial->Material(thisConstruct.LayerPoint(2))->group == Material::Group::WindowGas ||
                             state.dataMaterial->Material(thisConstruct.LayerPoint(2))->group == Material::Group::WindowGasMixture) &&
                            ((state.dataMaterial->Material(thisConstruct.LayerPoint(3))->group == Material::Group::Shade ||
                              state.dataMaterial->Material(thisConstruct.LayerPoint(3))->group == Material::Group::WindowBlind) &&
                             state.dataMaterial->Material(thisConstruct.LayerPoint(3))->group != Material::Group::Screen) &&
                            (state.dataMaterial->Material(thisConstruct.LayerPoint(4))->group == Material::Group::WindowGas ||
                             state.dataMaterial->Material(thisConstruct.LayerPoint(4))->group == Material::Group::WindowGasMixture) &&
                            state.dataMaterial->Material(thisConstruct.LayerPoint(5))->group == Material::Group::WindowGlass)
                            ValidBGShadeBlindConst = true;
                    }
                } else { // TotGlassLayers = 3
                    if (TotLayers != 7) {
                        WrongWindowLayering = true;
                    } else {
                        if (thisMaterialOutside->group == Material::Group::WindowGlass &&
                            (state.dataMaterial->Material(thisConstruct.LayerPoint(2))->group == Material::Group::WindowGas ||
                             state.dataMaterial->Material(thisConstruct.LayerPoint(2))->group == Material::Group::WindowGasMixture) &&
                            state.dataMaterial->Material(thisConstruct.LayerPoint(3))->group == Material::Group::WindowGlass &&
                            (state.dataMaterial->Material(thisConstruct.LayerPoint(4))->group == Material::Group::WindowGas ||
                             state.dataMaterial->Material(thisConstruct.LayerPoint(4))->group == Material::Group::WindowGasMixture) &&
                            ((state.dataMaterial->Material(thisConstruct.LayerPoint(5))->group == Material::Group::Shade ||
                              state.dataMaterial->Material(thisConstruct.LayerPoint(5))->group == Material::Group::WindowBlind) &&
                             state.dataMaterial->Material(thisConstruct.LayerPoint(5))->group != Material::Group::Screen) &&
                            (state.dataMaterial->Material(thisConstruct.LayerPoint(6))->group == Material::Group::WindowGas ||
                             state.dataMaterial->Material(thisConstruct.LayerPoint(6))->group == Material::Group::WindowGasMixture) &&
                            state.dataMaterial->Material(thisConstruct.LayerPoint(7))->group == Material::Group::WindowGlass)
                            ValidBGShadeBlindConst = true;
                    }
                } // End of check if TotGlassLayers = 2 or 3
                if (!ValidBGShadeBlindConst) WrongWindowLayering = true;
                if (!WrongWindowLayering) {
                    int const LayNumSh = 2 * TotGlassLayers - 1;
                    int const MatSh = thisConstruct.LayerPoint(LayNumSh);
                    auto const *thisMaterialSh = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatSh));
                    assert(thisMaterialSh != nullptr);
                    // For double pane, shade/blind must be layer #3.
                    // For triple pane, it must be layer #5 (i.e., between two inner panes).
                    if (thisMaterialSh->group != Material::Group::Shade && thisMaterialSh->group != Material::Group::WindowBlind)
                        WrongWindowLayering = true;
                    if (TotLayers != 2 * TotGlassLayers + 1) WrongWindowLayering = true;
                    if (!WrongWindowLayering) {
                        // Gas on either side of a between-glass shade/blind must be the same
                        int const MatGapL = thisConstruct.LayerPoint(LayNumSh - 1);
                        int const MatGapR = thisConstruct.LayerPoint(LayNumSh + 1);
                        auto const *thisMaterialGapL = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(MatGapL));
                        auto const *thisMaterialGapR = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(MatGapR));
                        for (int IGas = 1; IGas <= 5; ++IGas) {
                            if ((thisMaterialGapL->gasTypes(IGas) != thisMaterialGapR->gasTypes(IGas)) ||
                                (thisMaterialGapL->GasFract(IGas) != thisMaterialGapR->GasFract(IGas)))
                                WrongWindowLayering = true;
                        }
                        // Gap width on either side of a between-glass shade/blind must be the same
                        if (std::abs(thisMaterialGapL->Thickness - thisMaterialGapR->Thickness) > 0.0005) WrongWindowLayering = true;
                        if (thisMaterialSh->group == Material::Group::WindowBlind) {
                            int const BlNum = thisMaterialSh->BlindDataPtr;
                            if (BlNum > 0) {
                                if ((thisMaterialGapL->Thickness + thisMaterialGapR->Thickness) < state.dataMaterial->Blind(BlNum).SlatWidth) {
                                    ErrorsFound = true;
                                    ShowSevereError(state,
                                                    format("CheckAndSetConstructionProperties: For window construction {}", thisConstruct.Name));
                                    ShowContinueError(state, "the slat width of the between-glass blind is greater than");
                                    ShowContinueError(state, "the sum of the widths of the gas layers adjacent to the blind.");
                                }
                            } // End of check if BlNum > 0
                        }     // End of check if material is window blind
                    }         // End of check if WrongWindowLayering
                }             // End of check if WrongWindowLayering
            }                 // End of check on total glass layers
        }                     // End of check if construction has between-glass shade/blind

        // Check Simple Windows,
        if (state.dataMaterial->Material(thisConstruct.LayerPoint(1))->group == Material::Group::WindowSimpleGlazing) {
            if (TotLayers > 1) {
                // check that none of the other layers are glazing or gas
                for (int Layer = 1; Layer <= TotLayers; ++Layer) {
                    int const MaterNum = thisConstruct.LayerPoint(Layer);
                    if (MaterNum == 0) continue; // error -- has been caught will stop program later
                    auto const *thisMaterial = state.dataMaterial->Material(MaterNum);
                    if (thisMaterial->group == Material::Group::WindowGlass) {
                        ErrorsFound = true;
                        ShowSevereError(state, format("CheckAndSetConstructionProperties: Error in window construction {}--", thisConstruct.Name));
                        ShowContinueError(state, "For simple window constructions, no other glazing layers are allowed.");
                    }
                    if (thisMaterial->group == Material::Group::WindowGas) {
                        ErrorsFound = true;
                        ShowSevereError(state, format("CheckAndSetConstructionProperties: Error in window construction {}--", thisConstruct.Name));
                        ShowContinueError(state, "For simple window constructions, no other gas layers are allowed.");
                    }
                }
            }
        }

        if (WrongWindowLayering) {
            ShowSevereError(state, format("CheckAndSetConstructionProperties: Error in window construction {}--", thisConstruct.Name));
            ShowContinueError(state, "  For multi-layer window constructions the following rules apply:");
            ShowContinueError(state, "    --The first and last layer must be a solid layer (glass or shade/screen/blind),");
            ShowContinueError(state, "    --Adjacent glass layers must be separated by one and only one gas layer,");
            ShowContinueError(state, "    --Adjacent layers must not be of the same type,");
            ShowContinueError(state, "    --Only one shade/screen/blind layer is allowed,");
            ShowContinueError(state, "    --An exterior shade/screen/blind must be the first layer,");
            ShowContinueError(state, "    --An interior shade/blind must be the last layer,");
            ShowContinueError(state, "    --An interior screen is not allowed,");
            ShowContinueError(state, "    --For an exterior shade/screen/blind or interior shade/blind, there should not be a gas layer");
            ShowContinueError(state, "    ----between the shade/screen/blind and adjacent glass,");
            ShowContinueError(state, "    --A between-glass screen is not allowed,");
            ShowContinueError(state, "    --A between-glass shade/blind is allowed only for double and triple glazing,");
            ShowContinueError(state, "    --A between-glass shade/blind must have adjacent gas layers of the same type and width,");
            ShowContinueError(state, "    --For triple glazing the between-glass shade/blind must be between the two inner glass layers,");
            ShowContinueError(state, "    --The slat width of a between-glass blind must be less than the sum of the widths");
            ShowContinueError(state, "    ----of the gas layers adjacent to the blind.");
            ErrorsFound = true;
        }

        thisConstruct.TotGlassLayers = TotGlassLayers;
        thisConstruct.TotSolidLayers = TotGlassLayers + TotShadeLayers;

        // In following, InsideLayer is layer number of inside glass and InsideAbsorpThermal applies
        // only to inside glass; it is corrected later in InitGlassOpticalCalculations
        // if construction has inside shade or blind.
        if (thisMaterialInside->group == Material::Group::Shade || thisMaterialInside->group == Material::Group::WindowBlind) {
            --InsideLayer;
        }
        if (InsideLayer > 0) {
            InsideMaterNum = thisConstruct.LayerPoint(InsideLayer);
            thisConstruct.InsideAbsorpThermal = thisMaterialInside->AbsorpThermalBack;
        }
        if (InsideMaterNum != 0) {
            auto const *thisMaterialInside = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(InsideMaterNum));
            thisConstruct.InsideAbsorpVis = thisMaterialInside->AbsorpVisible;
            thisConstruct.InsideAbsorpSolar = thisMaterialInside->AbsorpSolar;
        }

        if ((thisMaterialOutside->group == Material::Group::WindowGlass) ||
            (thisMaterialOutside->group == Material::Group::WindowSimpleGlazing)) { // Glass
            thisConstruct.OutsideAbsorpThermal = thisMaterialOutside->AbsorpThermalFront;
        } else { // Exterior shade, blind or screen
            thisConstruct.OutsideAbsorpThermal = thisMaterialOutside->AbsorpThermal;
        }

    } else { // Opaque surface
        thisConstruct.InsideAbsorpThermal = thisMaterialInside->AbsorpThermal;
        thisConstruct.OutsideAbsorpThermal = thisMaterialOutside->AbsorpThermal;
    }

    thisConstruct.OutsideRoughness = thisMaterialOutside->Roughness;

    if (thisMaterialOutside->group == Material::Group::Air) {
        ShowSevereError(state, format("CheckAndSetConstructionProperties: Outside Layer is Air for construction {}", thisConstruct.Name));
        ShowContinueError(state, format("  Error in material {}", thisMaterialOutside->Name));
        ErrorsFound = true;
    }
    if (InsideLayer > 0) {
        if (thisMaterialInside->group == Material::Group::Air) {
            ShowSevereError(state, format("CheckAndSetConstructionProperties: Inside Layer is Air for construction {}", thisConstruct.Name));
            ShowContinueError(state, format("  Error in material {}", thisMaterialInside->Name));
            ErrorsFound = true;
        }
    }

    if (thisMaterialOutside->group == Material::Group::EcoRoof) {
        thisConstruct.TypeIsEcoRoof = true;
        // need to check EcoRoof is not non-outside layer
        for (int Layer = 2; Layer <= TotLayers; ++Layer) {
            if (state.dataMaterial->Material(thisConstruct.LayerPoint(Layer))->group == Material::Group::EcoRoof) {
                ShowSevereError(state,
                                format("CheckAndSetConstructionProperties: Interior Layer is EcoRoof for construction {}", thisConstruct.Name));
                ShowContinueError(state, format("  Error in material {}", state.dataMaterial->Material(thisConstruct.LayerPoint(Layer))->Name));
                ErrorsFound = true;
            }
        }
    }

    if (thisMaterialOutside->group == Material::Group::IRTransparent) {
        thisConstruct.TypeIsIRT = true;
        if (thisConstruct.TotLayers != 1) {
            ShowSevereError(
                state,
                format("CheckAndSetConstructionProperties: Infrared Transparent (IRT) Construction is limited to 1 layer {}", thisConstruct.Name));
            ShowContinueError(state, "  Too many layers in referenced construction.");
            ErrorsFound = true;
        }
    }
}

int AssignReverseConstructionNumber(EnergyPlusData &state,
                                    int const ConstrNum, // Existing Construction number of first surface
                                    bool &ErrorsFound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2006

    // PURPOSE OF THIS FUNCTION:
    // For interzone, unentered surfaces, we need to have "reverse" constructions
    // assigned to the created surfaces.  These need to be the reverse (outside to inside layer)
    // of existing surfaces.  Plus, there may be one already in the data structure so this is looked for as well.

    // METHODOLOGY EMPLOYED:
    // Create reverse layers.  Look in current constructions to see if match.  If no match, create a new one.

    // Return value
    int NewConstrNum; // Reverse Construction Number

    if (ConstrNum == 0) {
        // error caught elsewhere
        NewConstrNum = 0;
        return NewConstrNum;
    }

    auto &thisConstruct = state.dataConstruction->Construct(ConstrNum);
    thisConstruct.IsUsed = true;
    int nLayer = 0;
    state.dataConstruction->LayerPoint = 0;
    for (int Loop = thisConstruct.TotLayers; Loop >= 1; --Loop) {
        ++nLayer;
        state.dataConstruction->LayerPoint(nLayer) = thisConstruct.LayerPoint(Loop);
    }

    // now, got thru and see if there is a match already....
    NewConstrNum = 0;
    for (int Loop = 1; Loop <= state.dataHeatBal->TotConstructs; ++Loop) {
        bool Found = true;
        for (nLayer = 1; nLayer <= Construction::MaxLayersInConstruct; ++nLayer) {
            if (state.dataConstruction->Construct(Loop).LayerPoint(nLayer) != state.dataConstruction->LayerPoint(nLayer)) {
                Found = false;
                break;
            }
        }
        if (Found) {
            NewConstrNum = Loop;
            state.dataConstruction->Construct(Loop).IsUsed = true;
            break;
        }
    }

    // if need new one, bunch o stuff
    if (NewConstrNum == 0) {
        ++state.dataHeatBal->TotConstructs;
        state.dataConstruction->Construct.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalRforNominalUCalculation.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) = 0.0;
        state.dataHeatBal->NominalU.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalU(state.dataHeatBal->TotConstructs) = 0.0;
        state.dataHeatBal->NominalUBeforeAdjusted.redimension(state.dataHeatBal->TotConstructs);
        state.dataHeatBal->NominalUBeforeAdjusted(state.dataHeatBal->TotConstructs) = 0.0;
        state.dataHeatBal->CoeffAdjRatio.redimension(state.dataHeatBal->TotConstructs) = 1.0;
        //  Put in new attributes
        NewConstrNum = state.dataHeatBal->TotConstructs;
        state.dataConstruction->Construct(NewConstrNum).IsUsed = true;
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs) =
            state.dataConstruction->Construct(ConstrNum); // preserve some of the attributes.
        // replace others...
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).Name = "iz-" + state.dataConstruction->Construct(ConstrNum).Name;
        state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
        for (nLayer = 1; nLayer <= Construction::MaxLayersInConstruct; ++nLayer) {
            state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).LayerPoint(nLayer) = state.dataConstruction->LayerPoint(nLayer);
            if (state.dataConstruction->LayerPoint(nLayer) != 0) {
                state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) +=
                    state.dataHeatBal->NominalR(state.dataConstruction->LayerPoint(nLayer));
            }
        }

        // no error if zero -- that will have been caught with earlier construction
        // the following line was changed to fix CR7601
        if (state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) != 0.0) {
            state.dataHeatBal->NominalU(state.dataHeatBal->TotConstructs) =
                1.0 / state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs);
        }

        CheckAndSetConstructionProperties(state, state.dataHeatBal->TotConstructs, ErrorsFound);
    }

    return NewConstrNum;
}

void AddVariableSlatBlind(EnergyPlusData &state,
                          int const inBlindNumber, // current Blind Number/pointer to name
                          int &outBlindNumber,     // resultant Blind Number to pass back
                          bool &errFlag            // error flag should one be needed
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Window Blinds are presented as "fixed" slat blinds.  However for certain Window Shading Controls,
    // the program needs to set the property to "variable"/movable slats.  Since a blind could be in use
    // elsewhere with "fixed", a material needs to be added with variable properties -- having most of the
    // "fixed" properties in tact.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Found;
    Real64 MinSlatAngGeom;
    Real64 MaxSlatAngGeom;

    // Object Data

    // maybe it's already there
    errFlag = false;
    Found = UtilityRoutines::FindItemInList("~" + state.dataMaterial->Blind(inBlindNumber).Name, state.dataMaterial->Blind);
    if (Found == 0) {
        // Add a new blind
        state.dataHeatBal->TotBlinds += 1;
        state.dataMaterial->Blind.push_back(Material::WindowBlindProperties());
        state.dataMaterial->Blind(state.dataHeatBal->TotBlinds) = state.dataMaterial->Blind(inBlindNumber);
        state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).Name = "~" + state.dataMaterial->Blind(inBlindNumber).Name;
        outBlindNumber = state.dataHeatBal->TotBlinds;
        state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatAngleType = DataWindowEquivalentLayer::AngleType::Variable;

        // Minimum and maximum slat angles allowed by slat geometry
        if (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatWidth >
            state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatSeparation) {
            MinSlatAngGeom = std::asin(state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatThickness /
                                       (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatThickness +
                                        state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatSeparation)) /
                             DataGlobalConstants::DegToRadians;
        } else {
            MinSlatAngGeom = 0.0;
        }
        MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

        // Error if maximum slat angle less than minimum

        if (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle <
            state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle) {
            errFlag = true;
            ShowSevereError(state, format("WindowMaterial:Blind=\"{}\", Illegal value combination.", state.dataMaterial->Blind(inBlindNumber).Name));
            ShowContinueError(state,
                              format("Minimum Slat Angle=[{:.1R}], is greater than Maximum Slat Angle=[{:.1R}] deg.",
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle));
        }

        // Error if input slat angle not in input min/max range

        if (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle >
                state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle &&
            (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatAngle <
                 state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle ||
             state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatAngle >
                 state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle)) {
            errFlag = true;
            ShowSevereError(state, format("WindowMaterial:Blind=\"{}\", Illegal value combination.", state.dataMaterial->Blind(inBlindNumber).Name));
            ShowContinueError(state,
                              format("Slat Angle=[{:.1R}] is outside of the input min/max range, min=[{:.1R}], max=[{:.1R}] deg.",
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).SlatAngle,
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle));
        }

        // Warning if input minimum slat angle is less than that allowed by slat geometry

        if (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle < MinSlatAngGeom) {
            ShowWarningError(state, format("WindowMaterial:Blind=\"{}\", Illegal value combination.", state.dataMaterial->Blind(inBlindNumber).Name));
            ShowContinueError(
                state,
                format("Minimum Slat Angle=[{:.1R}] is less than the smallest allowed by slat dimensions and spacing, min=[{:.1R}] deg.",
                       state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle,
                       MinSlatAngGeom));
            ShowContinueError(state, format("Minimum Slat Angle will be set to {:.1R} deg.", MinSlatAngGeom));
            state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MinSlatAngle = MinSlatAngGeom;
        }

        // Warning if input maximum slat angle is greater than that allowed by slat geometry

        if (state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle > MaxSlatAngGeom) {
            ShowWarningError(state, format("WindowMaterial:Blind=\"{}\", Illegal value combination.", state.dataMaterial->Blind(inBlindNumber).Name));
            ShowContinueError(state,
                              format("Maximum Slat Angle=[{:.1R}] is greater than the largest allowed by slat dimensions and spacing, [{:.1R}] deg.",
                                     state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle,
                                     MaxSlatAngGeom));
            ShowContinueError(state, format("Maximum Slat Angle will be set to {:.1R} deg.", MaxSlatAngGeom));
            state.dataMaterial->Blind(state.dataHeatBal->TotBlinds).MaxSlatAngle = MaxSlatAngGeom;
        }
    } else {
        outBlindNumber = Found;
    }
}

void CalcScreenTransmittance(EnergyPlusData &state,
                             int const SurfaceNum,
                             ObjexxFCL::Optional<Real64 const> Phi,     // Optional sun altitude relative to surface outward normal (radians)
                             ObjexxFCL::Optional<Real64 const> Theta,   // Optional sun azimuth relative to surface outward normal (radians)
                             ObjexxFCL::Optional_int_const ScreenNumber // Optional screen number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   May 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    //  Calculate transmittance of window screen given azimuth and altitude angle
    //  of sun and surface orientation.

    // METHODOLOGY EMPLOYED:
    //  Window screen solar beam transmittance varies as the sun moves across the sky
    //  due to the geometry of the screen material and the angle of incidence
    //  of the solar beam. Azimuth and altitude angle are calculated with respect
    //  to the surface outward normal. Solar beam reflectance and absorptance are also
    //  accounted for.

    //  CALLs to CalcScreenTransmittance are primarily based on surface index. A typical call is:
    //  CALL CalcScreenTransmittance(SurfaceNum)
    //  Since a single Material:WindowScreen object may be used for multiple windows, the
    //  screen's direct beam properties are calculated for the screen material attached to this surface.
    //  If a single Material:WindowScreen object is used for 3 windows then Screens(3) is allocated.

    //  CALLs to CalcScreenTransmittance may be done by using the optional arguments as follows:
    //  CALLs to CalcScreenTransmittance at normal incidence are:
    //  CALL with a screen number and relative azimuth and altitude angles
    //  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=ScNum)
    //   -OR-
    //  CALL same as above using the material structure
    //  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=dataMaterial.Material(MatShade)%ScreenDataPtr)
    //   -OR-
    //  CALL with the surface number and relative azimuth and altitude angles
    //  CALL CalcScreenTransmittance(SurfaceNum, Phi=0.0, Theta=0.0)

    //  CALL's passing the screen number without the relative azimuth and altitude angles is not allowed
    //  CALL CalcScreenTransmittance(0, ScreenNumber=ScNum) ! DO NOT use this syntax

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:
    // The optional arguments Phi and Theta are used to integrate over a hemisphere and are passed as pairs
    // The optional argument ScreenNumber is used during CalcWindowScreenProperties to integrate over a quarter hemispere
    // "before" the surface # is known. Theta and Phi can be passed without ScreenNumber, but DO NOT pass ScreenNumber
    // without Theta and Phi.

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 constexpr Small(1.E-9); // Small Number used to approximate zero

    // FUNCTION PARAMETER DEFINITIONS:
    int ScNum;                        // Index to screen data
    Real64 Tdirect;                   // Beam solar transmitted through screen (dependent on sun angle)
    Real64 Tscattered;                // Beam solar reflected through screen (dependent on sun angle)
    Real64 TscatteredVis;             // Visible beam solar reflected through screen (dependent on sun angle)
    Real64 SunAzimuth;                // Solar azimuth angle from north (rad)
    Real64 SunAltitude;               // Solar altitude angle from horizon (rad)
    Real64 SurfaceAzimuth;            // Surface azimuth angle from north (rad)
    Real64 SurfaceTilt;               // Surface tilt angle from vertical (rad)
    Real64 SunAzimuthToScreenNormal;  // Relative solar azimuth (sun angle from screen normal, 0 to PiOvr2, rad)
    Real64 SunAltitudeToScreenNormal; // Relative solar altitude (sun angle from screen normal, -PiOvr2 to PiOvr2, rad)
    Real64 Beta;                      // Compliment of relative solar azimuth (rad)
    Real64 TransXDir;                 // Horizontal component of direct beam transmittance
    Real64 TransYDir;                 // Vertical component of direct beam transmittance
    Real64 Delta;                     // Intermediate variable used for Tscatter calculation (deg)
    Real64 DeltaMax;                  // Intermediate variable used for Tscatter calculation (deg)
    Real64 Tscattermax;               // Maximum solar beam  scattered transmittance
    Real64 TscattermaxVis;            // Maximum visible beam scattered transmittance
    Real64 ExponentInterior;          // Exponent used in scattered transmittance calculation
    // when Delta < DeltaMax (0,0 to peak)
    Real64 ExponentExterior; // Exponent used in scattered transmittance calculation
    // when Delta > DeltaMax (peak to max)
    Real64 AlphaDblPrime; // Intermediate variables (used in Eng. Doc.)
    Real64 COSMu;
    Real64 Epsilon;
    Real64 Eta;
    Real64 MuPrime;
    Real64 Gamma;
    Real64 NormalAltitude; // Actual altitude angle of sun wrt surface outward normal (rad)
    Real64 NormalAzimuth;  // Actual azimuth angle of sun wrt surface outward normal (rad)
    Real64 IncidentAngle;  // Solar angle wrt surface outward normal to determine
    // if sun is in front of screen (rad)
    Real64 PeakToPlateauRatio;    // Ratio of peak scattering to plateau at 0,0 incident angle
    Real64 PeakToPlateauRatioVis; // Ratio of peak visible scattering to plateau at 0,0 incident angle
    Real64 ReflectCyl;            // Screen material reflectance
    Real64 ReflectCylVis;         // Screen material visible reflectance

    // Screens structure may be accessed using either the surface or screen index
    // The screen index is based on the number of Surface:HeatTransfer:Sub objects using any Material:WindowScreen object
    if (present(ScreenNumber)) {
        ScNum = ScreenNumber;
        if (!present(Theta) || !present(Phi)) {
            ShowFatalError(state, "Syntax error, optional arguments Theta and Phi must be present when optional ScreenNumber is used.");
        }
    } else {
        ScNum = state.dataSurface->SurfWinScreenNumber(SurfaceNum);
    }

    if (present(Theta)) {
        SunAzimuthToScreenNormal = std::abs(Theta);
        if (SunAzimuthToScreenNormal > DataGlobalConstants::Pi) {
            SunAzimuthToScreenNormal = 0.0;
        } else {
            if (SunAzimuthToScreenNormal > DataGlobalConstants::PiOvr2) {
                SunAzimuthToScreenNormal = DataGlobalConstants::Pi - SunAzimuthToScreenNormal;
            }
        }
        NormalAzimuth = SunAzimuthToScreenNormal;
    } else {
        SunAzimuth = std::atan2(state.dataEnvrn->SOLCOS(1), state.dataEnvrn->SOLCOS(2));
        if (SunAzimuth < 0.0) SunAzimuth += 2.0 * DataGlobalConstants::Pi;
        SurfaceAzimuth = state.dataSurface->Surface(SurfaceNum).Azimuth * DataGlobalConstants::DegToRadians;
        NormalAzimuth = SunAzimuth - SurfaceAzimuth;
        //   Calculate the transmittance whether sun is in front of or behind screen, place result in BmBmTrans or BmBmTransBack
        if (std::abs(SunAzimuth - SurfaceAzimuth) > DataGlobalConstants::PiOvr2) {
            SunAzimuthToScreenNormal = std::abs(SunAzimuth - SurfaceAzimuth) - DataGlobalConstants::PiOvr2;
        } else {
            SunAzimuthToScreenNormal = std::abs(SunAzimuth - SurfaceAzimuth);
        }
    }

    if (present(Phi)) {
        SunAltitudeToScreenNormal = std::abs(Phi);
        if (SunAltitudeToScreenNormal > DataGlobalConstants::PiOvr2) {
            SunAltitudeToScreenNormal = DataGlobalConstants::Pi - SunAltitudeToScreenNormal;
        }
        SunAltitude = SunAltitudeToScreenNormal;
    } else {
        SunAltitude = (DataGlobalConstants::PiOvr2 - std::acos(state.dataEnvrn->SOLCOS(3)));
        SurfaceTilt = state.dataSurface->Surface(SurfaceNum).Tilt * DataGlobalConstants::DegToRadians;
        SunAltitudeToScreenNormal = std::abs(SunAltitude + (SurfaceTilt - DataGlobalConstants::PiOvr2));
        if (SunAltitudeToScreenNormal > DataGlobalConstants::PiOvr2) {
            SunAltitudeToScreenNormal -= DataGlobalConstants::PiOvr2;
        }
    }

    if (SurfaceNum == 0 || !present(ScreenNumber)) {
        NormalAltitude = SunAltitude;
    } else {
        NormalAltitude = SunAltitude + (SurfaceTilt - DataGlobalConstants::PiOvr2);
    }

    if (NormalAltitude != 0.0 && NormalAzimuth != 0.0) {
        IncidentAngle = std::acos(std::sin(NormalAltitude) / (std::tan(NormalAzimuth) * std::tan(NormalAltitude) / std::sin(NormalAzimuth)));
    } else if (NormalAltitude != 0.0 && NormalAzimuth == 0.0) {
        IncidentAngle = NormalAltitude;
    } else if (NormalAltitude == 0.0 && NormalAzimuth != 0.0) {
        IncidentAngle = NormalAzimuth;
    } else {
        IncidentAngle = 0.0;
    }

    auto &thisScreen = state.dataMaterial->Screens(ScNum);

    // ratio of screen material diameter to screen material spacing
    Gamma = thisScreen.ScreenDiameterToSpacingRatio;

    // ************************************************************************************************
    // * calculate transmittance of totally absorbing screen material (beam passing through open area)*
    // ************************************************************************************************

    // calculate compliment of relative solar azimuth
    Beta = DataGlobalConstants::PiOvr2 - SunAzimuthToScreenNormal;

    // Catch all divide by zero instances
    if (Beta > Small) {
        if (std::abs(SunAltitudeToScreenNormal - DataGlobalConstants::PiOvr2) > Small) {
            AlphaDblPrime = std::atan(std::tan(SunAltitudeToScreenNormal) / std::cos(SunAzimuthToScreenNormal));
            TransYDir = 1.0 - Gamma * (std::cos(AlphaDblPrime) +
                                       std::sin(AlphaDblPrime) * std::tan(SunAltitudeToScreenNormal) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Beta))));
            TransYDir = max(0.0, TransYDir);
        } else {
            TransYDir = 0.0;
        }
    } else {
        TransYDir = 0.0;
    }

    COSMu = std::sqrt(pow_2(std::cos(SunAltitudeToScreenNormal)) * pow_2(std::cos(SunAzimuthToScreenNormal)) +
                      pow_2(std::sin(SunAltitudeToScreenNormal)));
    if (COSMu > Small) {
        Epsilon = std::acos(std::cos(SunAltitudeToScreenNormal) * std::cos(SunAzimuthToScreenNormal) / COSMu);
        Eta = DataGlobalConstants::PiOvr2 - Epsilon;
        if (std::cos(Epsilon) != 0.0) {
            MuPrime = std::atan(std::tan(std::acos(COSMu)) / std::cos(Epsilon));
            if (Eta != 0.0) {
                TransXDir =
                    1.0 - Gamma * (std::cos(MuPrime) + std::sin(MuPrime) * std::tan(std::acos(COSMu)) * std::sqrt(1.0 + pow_2(1.0 / std::tan(Eta))));
                TransXDir = max(0.0, TransXDir);
            } else {
                TransXDir = 0.0;
            }
        } else {
            TransXDir = 0.0;
        }
    } else {
        TransXDir = 1.0 - Gamma;
    }
    Tdirect = max(0.0, TransXDir * TransYDir);

    // *******************************************************************************
    // * calculate transmittance of scattered beam due to reflecting screen material *
    // *******************************************************************************

    ReflectCyl = thisScreen.ReflectCylinder;
    ReflectCylVis = thisScreen.ReflectCylinderVis;

    if (std::abs(SunAzimuthToScreenNormal - DataGlobalConstants::PiOvr2) < Small ||
        std::abs(SunAltitudeToScreenNormal - DataGlobalConstants::PiOvr2) < Small) {
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else {
        //   DeltaMax and Delta are in degrees
        DeltaMax = 89.7 - (10.0 * Gamma / 0.16);
        Delta = std::sqrt(pow_2(SunAzimuthToScreenNormal / DataGlobalConstants::DegToRadians) +
                          pow_2(SunAltitudeToScreenNormal / DataGlobalConstants::DegToRadians));

        //   Use empirical model to determine maximum (peak) scattering
        Tscattermax = 0.0229 * Gamma + 0.2971 * ReflectCyl - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflectCyl) - 0.44416 * Gamma * ReflectCyl;
        TscattermaxVis =
            0.0229 * Gamma + 0.2971 * ReflectCylVis - 0.03624 * pow_2(Gamma) + 0.04763 * pow_2(ReflectCylVis) - 0.44416 * Gamma * ReflectCylVis;

        //   Vary slope of interior and exterior surface of scattering model
        ExponentInterior = -pow_2(Delta - DeltaMax) / 600.0;
        ExponentExterior = -std::pow(std::abs(Delta - DeltaMax), 2.5) / 600.0;

        //   Determine ratio of scattering at 0,0 incident angle to maximum (peak) scattering
        PeakToPlateauRatio = 1.0 / (0.2 * (1 - Gamma) * ReflectCyl);
        PeakToPlateauRatioVis = 1.0 / (0.2 * (1 - Gamma) * ReflectCylVis);

        if (Delta > DeltaMax) {
            //     Apply offset for plateau and use exterior exponential function to simulate actual scattering as a function of solar angles
            Tscattered = 0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentExterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentExterior));
            //     Trim off offset if solar angle (delta) is greater than maximum (peak) scattering angle
            Tscattered -= (0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
            TscatteredVis -= (0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis) * max(0.0, (Delta - DeltaMax) / (90.0 - DeltaMax));
        } else {
            //     Apply offset for plateau and use interior exponential function to simulate actual scattering as a function of solar angles
            Tscattered = 0.2 * (1.0 - Gamma) * ReflectCyl * Tscattermax * (1.0 + (PeakToPlateauRatio - 1.0) * std::exp(ExponentInterior));
            TscatteredVis = 0.2 * (1.0 - Gamma) * ReflectCylVis * TscattermaxVis * (1.0 + (PeakToPlateauRatioVis - 1.0) * std::exp(ExponentInterior));
        }
    }
    Tscattered = max(0.0, Tscattered);
    TscatteredVis = max(0.0, TscatteredVis);

    if (thisScreen.screenBeamReflectanceModel == Material::ScreenBeamReflectanceModel::DoNotModel) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            thisScreen.BmBmTrans = Tdirect;
            thisScreen.BmBmTransVis = Tdirect;
            thisScreen.BmBmTransBack = 0.0;
        } else {
            thisScreen.BmBmTrans = 0.0;
            thisScreen.BmBmTransVis = 0.0;
            thisScreen.BmBmTransBack = Tdirect;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (thisScreen.screenBeamReflectanceModel == Material::ScreenBeamReflectanceModel::DirectBeam) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            thisScreen.BmBmTrans = Tdirect + Tscattered;
            thisScreen.BmBmTransVis = Tdirect + TscatteredVis;
            thisScreen.BmBmTransBack = 0.0;
        } else {
            thisScreen.BmBmTrans = 0.0;
            thisScreen.BmBmTransVis = 0.0;
            thisScreen.BmBmTransBack = Tdirect + Tscattered;
        }
        Tscattered = 0.0;
        TscatteredVis = 0.0;
    } else if (thisScreen.screenBeamReflectanceModel == Material::ScreenBeamReflectanceModel::Diffuse) {
        if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
            thisScreen.BmBmTrans = Tdirect;
            thisScreen.BmBmTransVis = Tdirect;
            thisScreen.BmBmTransBack = 0.0;
        } else {
            thisScreen.BmBmTrans = 0.0;
            thisScreen.BmBmTransVis = 0.0;
            thisScreen.BmBmTransBack = Tdirect;
        }
    }

    if (std::abs(IncidentAngle) <= DataGlobalConstants::PiOvr2) {
        thisScreen.BmDifTrans = Tscattered;
        thisScreen.BmDifTransVis = TscatteredVis;
        thisScreen.BmDifTransBack = 0.0;
        thisScreen.ReflectSolBeamFront = max(0.0, ReflectCyl * (1.0 - Tdirect) - Tscattered);
        thisScreen.ReflectVisBeamFront = max(0.0, ReflectCylVis * (1.0 - Tdirect) - TscatteredVis);
        thisScreen.AbsorpSolarBeamFront = max(0.0, (1.0 - Tdirect) * (1.0 - ReflectCyl));
        thisScreen.ReflectSolBeamBack = 0.0;
        thisScreen.ReflectVisBeamBack = 0.0;
        thisScreen.AbsorpSolarBeamBack = 0.0;
    } else {
        thisScreen.BmDifTrans = 0.0;
        thisScreen.BmDifTransVis = 0.0;
        thisScreen.BmDifTransBack = Tscattered;
        thisScreen.ReflectSolBeamBack = max(0.0, ReflectCyl * (1.0 - Tdirect) - Tscattered);
        thisScreen.ReflectVisBeamBack = max(0.0, ReflectCylVis * (1.0 - Tdirect) - TscatteredVis);
        thisScreen.AbsorpSolarBeamBack = max(0.0, (1.0 - Tdirect) * (1.0 - ReflectCyl));
        thisScreen.ReflectSolBeamFront = 0.0;
        thisScreen.ReflectVisBeamFront = 0.0;
        thisScreen.AbsorpSolarBeamFront = 0.0;
    }
}

Real64 ComputeNominalUwithConvCoeffs(EnergyPlusData &state,
                                     int const numSurf, // index for Surface array.
                                     bool &isValid      // returns true if result is valid
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Jason Glazer
    //       DATE WRITTEN   September 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate Nominal U-value with convection/film coefficients for reporting by
    // adding on prescribed R-values for interior and exterior convection coefficients
    // as found in ASHRAE 90.1-2004, Appendix A. Used in EIO and tabular reports.
    // ASHRAE 90.1-2004 Section A9.4.1 shows the following:
    //      R-value Condition
    //      All exterior conditions                        IP: 0.17  SI: 0.0299
    //      All semi-exterior surfaces                     IP: 0.46  SI: 0.0810
    //      Interior horizontal surfaces, heat flow up     IP: 0.61  SI: 0.1074
    //      Interior horizontal surfaces, heat flow down   IP: 0.92  SI: 0.1620
    //      Interior vertical surfaces                     IP: 0.68  SI: 0.1198
    // This section shows the same value in 90.1-2010 and 90.2-2010
    // Note that this report does not use the semi-exterior surface value because
    // EnergyPlus does not have a way to specifically tell whether or not a surface
    // is connected to a semi-exterior area of the building.  Users can always use
    // the Nominal U-Value to manually calculated this.  The values calculated here
    // are simply reported to the EIO file and not used for any calculations.

    // Return value
    Real64 NominalUwithConvCoeffs; // return value

    static constexpr std::array<Real64, static_cast<int>(DataSurfaces::SurfaceClass::Num)> filmCoefs = {
        0.0,       // None
        0.1197548, // Wall
        0.1620212, // Floor
        0.1074271, // Roof
        0.0,       // IntMass
        0.0,       // Detached_B
        0.0,       // Detached_F
        0.1197548, // Window
        0.1197548, // GlassDoor
        0.1197548, // Door
        0.0,       // Shading
        0.0,       // Overhang
        0.0,       // Fin
        0.0,       // TDD_Dome
        0.0        // TDD_Diffuser
    };             // If anything added to the enum SurfaceClass, adjust this list appropriately

    Real64 insideFilm;
    Real64 outsideFilm;

    isValid = true;

    auto &thisSurface = state.dataSurface->Surface(numSurf);

    // exterior conditions
    switch (thisSurface.ExtBoundCond) {
    case DataSurfaces::ExternalEnvironment: { // ExtBoundCond = 0
        outsideFilm = 0.0299387;              // All exterior conditions
    } break;
    case DataSurfaces::OtherSideCoefCalcExt: {
        outsideFilm = state.dataSurface->OSC(thisSurface.OSCPtr).SurfFilmCoef;
    } break;
    case DataSurfaces::Ground:
    case DataSurfaces::OtherSideCoefNoCalcExt:
    case DataSurfaces::OtherSideCondModeledExt:
    case DataSurfaces::GroundFCfactorMethod:
    case DataSurfaces::KivaFoundation: { // All these cases have a negative ExtBoundCond so don't use film coefficients
        outsideFilm = 0.0;
    } break;
    default: { // Interior Surface Attached to a Zone (ExtBoundCond is a surface)
        outsideFilm = filmCoefs[static_cast<int>(state.dataSurface->Surface(thisSurface.ExtBoundCond).Class)];
    } break;
    }
    // interior conditions and calculate the return value
    if (state.dataHeatBal->NominalU(thisSurface.Construction) > 0.0) {
        insideFilm = filmCoefs[static_cast<int>(thisSurface.Class)];
        if (insideFilm == 0.0) outsideFilm = 0.0;
        NominalUwithConvCoeffs =
            1.0 / (insideFilm + (1.0 / state.dataHeatBal->NominalU(state.dataSurface->Surface(numSurf).Construction)) + outsideFilm);
    } else {
        isValid = false;
        NominalUwithConvCoeffs = state.dataHeatBal->NominalU(state.dataSurface->Surface(numSurf).Construction);
    }

    return NominalUwithConvCoeffs;
}

void SetFlagForWindowConstructionWithShadeOrBlindLayer(EnergyPlusData &state)
{

    // PURPOSE OF THIS SUBROUTINE:
    // check fenestrations with shading control and set a flag to true if its construction has
    // either shade or blind material layer

    // METHODOLOGY EMPLOYED:
    // Loop through Surface and register any shading controls, and loop through the construction
    // material layer

    // Using/Aliasing
    using DataSurfaces::ExternalEnvironment;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int loopSurfNum(0); // surface index
    int ConstrNum(0);   // construction index
    int NumLayers(0);   // number of material layers in a construction
    int Layer(0);       // construction material layer index
    int MaterNum(0);    // construction material index

    for (loopSurfNum = 1; loopSurfNum <= state.dataSurface->TotSurfaces; ++loopSurfNum) {

        if (state.dataSurface->Surface(loopSurfNum).Class != DataSurfaces::SurfaceClass::Window) continue;
        if (state.dataSurface->Surface(loopSurfNum).ExtBoundCond != ExternalEnvironment) continue;
        if (!state.dataSurface->Surface(loopSurfNum).HasShadeControl) continue;
        if (state.dataSurface->Surface(loopSurfNum).activeShadedConstruction == 0) continue;

        ConstrNum = state.dataSurface->Surface(loopSurfNum).activeShadedConstruction;
        auto const &thisConstruct = state.dataConstruction->Construct(ConstrNum);
        if (thisConstruct.TypeIsWindow) {
            NumLayers = thisConstruct.TotLayers;
            for (Layer = 1; Layer <= NumLayers; ++Layer) {
                MaterNum = thisConstruct.LayerPoint(Layer);
                if (MaterNum == 0) continue;
                auto const *thisMaterial = state.dataMaterial->Material(MaterNum);
                if (thisMaterial->group == Material::Group::Shade || thisMaterial->group == Material::Group::WindowBlind)
                    state.dataSurface->SurfWinHasShadeOrBlindLayer(loopSurfNum) = true;
            }
        }
    }
}

void AllocateIntGains(EnergyPlusData &state)
{
    state.dataHeatBal->ZoneIntGain.allocate(state.dataGlobal->NumOfZones);
    state.dataHeatBal->spaceIntGain.allocate(state.dataGlobal->numSpaces);
    state.dataHeatBal->spaceIntGainDevices.allocate(state.dataGlobal->numSpaces);
    state.dataDaylightingData->spacePowerReductionFactor.dimension(state.dataGlobal->numSpaces, 1.0);
}

} // namespace EnergyPlus::DataHeatBalance
