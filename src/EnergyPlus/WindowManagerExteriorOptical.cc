// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

#include <cassert>

// EnergyPlus headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/WindowManager.hh>

// Windows library headers
#include <WCEMultiLayerOptics.hpp>

#include "WindowManagerExteriorData.hh"
#include "WindowManagerExteriorOptical.hh"

namespace EnergyPlus {

using namespace FenestrationCommon;
using namespace SpectralAveraging;
using namespace SingleLayerOptics;

using namespace DataEnvironment;
using namespace DataSurfaces;
using namespace DataHeatBalance;
namespace WindowManager {

    std::shared_ptr<CBSDFLayer> getBSDFLayer(EnergyPlusData &state, const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // BSDF will be created in different ways that is based on material type

        std::shared_ptr<CWCELayerFactory> aFactory = nullptr;
        if (t_Material.group == Material::Group::WindowGlass) {
            aFactory = std::make_shared<CWCESpecularLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::WindowBlind) {
            aFactory = std::make_shared<CWCEVenetianBlindLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::Screen) {
            aFactory = std::make_shared<CWCEScreenLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::Shade) {
            aFactory = std::make_shared<CWCEDiffuseShadeLayerFactory>(t_Material, t_Range);
        }
        return aFactory->getBSDFLayer(state);
    }

    CScatteringLayer getScatteringLayer(EnergyPlusData &state, const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2017
        //       MODIFIED       na
        //       RE-ENGINEERED
        //          April 2021: returning CScatteringLayer instead of pointer to it

        // PURPOSE OF THIS SUBROUTINE:
        // Scattering will be created in different ways that is based on material type

        std::shared_ptr<CWCELayerFactory> aFactory = nullptr;
        if (t_Material.group == Material::Group::WindowGlass || t_Material.group == Material::Group::WindowSimpleGlazing) {
            aFactory = std::make_shared<CWCESpecularLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::WindowBlind) {
            aFactory = std::make_shared<CWCEVenetianBlindLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::Screen) {
            aFactory = std::make_shared<CWCEScreenLayerFactory>(t_Material, t_Range);
        } else if (t_Material.group == Material::Group::Shade) {
            aFactory = std::make_shared<CWCEDiffuseShadeLayerFactory>(t_Material, t_Range);
        }
        return aFactory->getLayer(state);
    }

    // void InitWCE_BSDFOpticalData() {
    //     // SUBROUTINE INFORMATION:
    //     //       AUTHOR         Simon Vidanovic
    //     //       DATE WRITTEN   September 2016
    //     //       MODIFIED       na
    //     //       RE-ENGINEERED  na
    //
    //     // PURPOSE OF THIS SUBROUTINE:
    //     // Initialize BSDF construction layers in Solar and Visible spectrum.
    //
    //     auto aWinConstBSDF = CWindowConstructionsBSDF::instance();
    //     for ( auto ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {
    //         auto& construction( Construct( ConstrNum ) );
    //         if ( construction.isGlazingConstruction() ) {
    //             for ( auto LayNum = 1; LayNum <= construction.TotLayers; ++LayNum ) {
    //                 auto& material( dataMaterial.Material( construction.LayerPoint( LayNum ) ) );
    //                 if ( material->group != WindowGas && material->group != WindowGasMixture &&
    //                     material->group != ComplexWindowGap && material->group != ComplexWindowShade ) {
    //                     auto aMaterial = std::make_shared< Material::MaterialBase >();
    //                     *aMaterial = material;
    //
    //                     // This is necessary because rest of EnergyPlus code relies on TransDiff property
    //                     // of construction. It will basically trigger Window optical calculations if this
    //                     // property is >0.
    //                     construction.TransDiff = 0.1;
    //
    //                     auto aRange = WavelengthRange::Solar;
    //                     auto aSolarLayer = getBSDFLayer( aMaterial, aRange );
    //                     aWinConstBSDF.pushBSDFLayer( aRange, ConstrNum, aSolarLayer );
    //
    //                     aRange = WavelengthRange::Visible;
    //                     auto aVisibleLayer = getBSDFLayer( aMaterial, aRange );
    //                     aWinConstBSDF.pushBSDFLayer( aRange, ConstrNum, aVisibleLayer );
    //                 }
    //
    //             }
    //         }
    //     }
    // }

    void InitWCE_SimplifiedOpticalData(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initialize scattering construction layers in Solar and Visible spectrum.

        // Calculate optical properties of blind-type layers entered with MATERIAL:WindowBlind
        // Calculation from this is used for IR properties. Need to make sure that properties
        // are calculated with new WCE optical engine (for both blinds and screens)
        if (state.dataHeatBal->TotBlinds > 0) CalcWindowBlindProperties(state);

        // Initialize SurfaceScreen structure
        state.dataHeatBal->NumScreens = state.dataHeatBal->TotScreens;
        if (state.dataHeatBal->NumScreens > 0) CalcWindowScreenProperties(state);

        auto &aWinConstSimp = CWindowConstructionsSimplified::instance(state);
        for (int ConstrNum = 1; ConstrNum <= state.dataHeatBal->TotConstructs; ++ConstrNum) {
            auto &construction = state.dataConstruction->Construct(ConstrNum);
            if (construction.isGlazingConstruction(state)) {
                for (int LayNum = 1; LayNum <= construction.TotLayers; ++LayNum) {
                    auto *materialBase(state.dataMaterial->Material(construction.LayerPoint(LayNum)));
                    auto *material = dynamic_cast<Material::MaterialChild *>(materialBase);
                    assert(material != nullptr);
                    if (material->group != Material::Group::WindowGas && material->group != Material::Group::WindowGasMixture &&
                        material->group != Material::Group::ComplexWindowGap && material->group != Material::Group::ComplexWindowShade) {
                        // This is necessary because rest of EnergyPlus code relies on TransDiff property
                        // of construction. It will basically trigger Window optical calculations if this
                        // property is >0.
                        construction.TransDiff = 0.1;

                        WavelengthRange aRange = WavelengthRange::Solar;
                        auto aSolarLayer = getScatteringLayer(state, *material, aRange); // (AUTO_OK_OBJ)
                        aWinConstSimp.pushLayer(aRange, ConstrNum, aSolarLayer);

                        aRange = WavelengthRange::Visible;
                        auto aVisibleLayer = getScatteringLayer(state, *material, aRange); // (AUTO_OK_OBJ)
                        aWinConstSimp.pushLayer(aRange, ConstrNum, aVisibleLayer);
                    }
                }
            }
        }

        // Get effective glass and shade/blind emissivities for windows that have interior blind or
        // shade. These are used to calculate zone MRT contribution from window when
        // interior blind/shade is deployed.

        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (!state.dataSurface->Surface(SurfNum).HeatTransSurf) continue;
            if (!state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).TypeIsWindow) continue;
            if (state.dataSurface->SurfWinWindowModelType(SurfNum) == WindowModel::BSDF) continue; // Irrelevant for Complex Fen
            if (state.dataConstruction->Construct(state.dataSurface->Surface(SurfNum).Construction).WindowTypeEQL) continue; // not required
            int ConstrNumSh = state.dataSurface->Surface(SurfNum).activeShadedConstruction;
            if (ConstrNumSh == 0) continue;
            int TotLay = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
            bool IntShade = false;
            bool IntBlind = false;
            int ShadeLayPtr = 0;
            int BlNum = 0;
            auto const *thisMaterial = dynamic_cast<Material::MaterialChild *>(
                state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay)));
            assert(thisMaterial != nullptr);
            if (thisMaterial->group == Material::Group::Shade) {
                IntShade = true;
                ShadeLayPtr = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay);
            }
            if (thisMaterial->group == Material::Group::WindowBlind) {
                IntBlind = true;
                BlNum = thisMaterial->BlindDataPtr;
            }

            if (IntShade || IntBlind) {
                for (int ISlatAng = 1; ISlatAng <= Material::MaxSlatAngs; ++ISlatAng) {
                    Real64 EpsGlIR = 0.0;
                    Real64 RhoGlIR = 0.0;
                    if (IntShade || IntBlind) {
                        EpsGlIR = dynamic_cast<const Material::MaterialChild *>(
                                      state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay - 1)))
                                      ->AbsorpThermalBack;
                        RhoGlIR = 1 - EpsGlIR;
                    }
                    if (IntShade) {
                        auto const *thisMaterialShade = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(ShadeLayPtr));
                        Real64 TauShIR = thisMaterialShade->TransThermal;
                        Real64 EpsShIR = thisMaterialShade->AbsorpThermal;
                        Real64 RhoShIR = max(0.0, 1.0 - TauShIR - EpsShIR);
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss[1] =
                            EpsShIR * (1.0 + RhoGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR));
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss[1] = EpsGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR);
                    }
                    if (IntBlind) {
                        Real64 TauShIR = state.dataMaterial->Blind(BlNum).IRFrontTrans(ISlatAng);
                        Real64 EpsShIR = state.dataMaterial->Blind(BlNum).IRBackEmiss(ISlatAng);
                        Real64 RhoShIR = max(0.0, 1.0 - TauShIR - EpsShIR);
                        state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss[ISlatAng] =
                            EpsShIR * (1.0 + RhoGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR));
                        state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss[ISlatAng] = EpsGlIR * TauShIR / (1.0 - RhoGlIR * RhoShIR);
                    }
                    // Loop over remaining slat angles only if blind with movable slats
                    if (IntShade) break; // Loop over remaining slat angles only if blind
                    if (IntBlind) {
                        if (state.dataMaterial->Blind(BlNum).SlatAngleType == DataWindowEquivalentLayer::AngleType::Fixed) break;
                    }
                } // End of slat angle loop
            }     // End of check if interior shade or interior blind
        }         // End of surface loop
    }

    Real64 GetSolarTransDirectHemispherical(EnergyPlusData &state, int ConstrNum)
    {
        const auto aWinConstSimp = // (AUTO_OK_SHARED_PTR)
            CWindowConstructionsSimplified::instance(state).getEquivalentLayer(state, FenestrationCommon::WavelengthRange::Solar, ConstrNum);
        return aWinConstSimp->getPropertySimple(
            0.3, 2.5, FenestrationCommon::PropertySimple::T, FenestrationCommon::Side::Front, FenestrationCommon::Scattering::DirectHemispherical);
    }

    Real64 GetVisibleTransDirectHemispherical(EnergyPlusData &state, int ConstrNum)
    {
        const auto aWinConstSimp = // (AUTO_OK_SHARED_PTR)
            CWindowConstructionsSimplified::instance(state).getEquivalentLayer(state, FenestrationCommon::WavelengthRange::Visible, ConstrNum);
        return aWinConstSimp->getPropertySimple(
            0.38, 0.78, FenestrationCommon::PropertySimple::T, FenestrationCommon::Side::Front, FenestrationCommon::Scattering::DirectHemispherical);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEMaterialFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEMaterialFactory::CWCEMaterialFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : m_MaterialProperties(t_Material), m_Range(t_Range), m_Initialized(false)
    {
    }

    std::shared_ptr<CMaterial> CWCEMaterialFactory::getMaterial(EnergyPlusData &state)
    {
        if (!m_Initialized) {
            init(state);
            m_Initialized = true;
        }
        return m_Material;
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecularMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCESpecularMaterialsFactory::CWCESpecularMaterialsFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCEMaterialFactory(t_Material, t_Range)
    {
    }

    void CWCESpecularMaterialsFactory::init(EnergyPlusData &state)
    {
        if (m_MaterialProperties.GlassSpectralDataPtr > 0) {
            auto aSolarSpectrum = CWCESpecturmProperties::getDefaultSolarRadiationSpectrum(state); // (AUTO_OK_OBJ)
            std::shared_ptr<CSpectralSampleData> aSampleData = nullptr;
            aSampleData = CWCESpecturmProperties::getSpectralSample(state, m_MaterialProperties.GlassSpectralDataPtr);

            auto aSample = std::make_shared<CSpectralSample>(aSampleData, aSolarSpectrum); // (AUTO_OK_SHARED_PTR)

            FenestrationCommon::MaterialType aType = MaterialType::Monolithic;
            CWavelengthRange aRange(m_Range);
            Real64 lowLambda = aRange.minLambda();
            Real64 highLambda = aRange.maxLambda();

            // Do not apply detector data if we do not have spectral data. This will only cause more inaccurate results at the end. (Simon)
            if (m_Range == WavelengthRange::Visible && m_MaterialProperties.GlassSpectralDataPtr != 0) {
                const auto aPhotopicResponse = CWCESpecturmProperties::getDefaultVisiblePhotopicResponse(state); // (AUTO_OK_OBJ)
                aSample->setDetectorData(aPhotopicResponse);
            }

            Real64 thickness = m_MaterialProperties.Thickness;
            m_Material = std::make_shared<CMaterialSample>(aSample, thickness, aType, lowLambda, highLambda);
        } else {
            if (m_Range == WavelengthRange::Solar) {
                m_Material = std::make_shared<CMaterialSingleBand>(m_MaterialProperties.Trans,
                                                                   m_MaterialProperties.Trans,
                                                                   m_MaterialProperties.ReflectSolBeamFront,
                                                                   m_MaterialProperties.ReflectSolBeamBack,
                                                                   m_Range);
            }
            if (m_Range == WavelengthRange::Visible) {
                m_Material = std::make_shared<CMaterialSingleBand>(m_MaterialProperties.TransVis,
                                                                   m_MaterialProperties.TransVis,
                                                                   m_MaterialProperties.ReflectVisBeamFront,
                                                                   m_MaterialProperties.ReflectVisBeamBack,
                                                                   m_Range);
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEMaterialDualBandFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEMaterialDualBandFactory::CWCEMaterialDualBandFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCEMaterialFactory(t_Material, t_Range)
    {
    }

    void CWCEMaterialDualBandFactory::init([[maybe_unused]] EnergyPlusData &state)
    {
        if (m_Range == WavelengthRange::Visible) {
            m_Material = createVisibleRangeMaterial(state);
        } else {
            auto aVisibleRangeMaterial = createVisibleRangeMaterial(state); // (AUTO_OK_OBJ)
            auto aSolarRangeMaterial = createSolarRangeMaterial(state);     // (AUTO_OK_OBJ)
            // Ratio visible to solar range. It can be calculated from solar spectrum.
            Real64 ratio = 0.49;
            m_Material = std::make_shared<CMaterialDualBand>(aVisibleRangeMaterial, aSolarRangeMaterial, ratio);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindMaterialsFactory::CWCEVenetianBlindMaterialsFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCEMaterialDualBandFactory(t_Material, t_Range)
    {
    }

    std::shared_ptr<CMaterialSingleBand> CWCEVenetianBlindMaterialsFactory::createVisibleRangeMaterial(EnergyPlusData &state)
    {
        int blindDataPtr = m_MaterialProperties.BlindDataPtr;
        assert(blindDataPtr > 0);
        auto &blind = state.dataMaterial->Blind(blindDataPtr);

        CWavelengthRange aRange(WavelengthRange::Visible);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = blind.SlatTransVisDiffDiff;
        Real64 Tb = blind.SlatTransVisDiffDiff;
        Real64 Rf = blind.SlatFrontReflVisDiffDiff;
        Real64 Rb = blind.SlatBackReflVisDiffDiff;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    std::shared_ptr<CMaterialSingleBand> CWCEVenetianBlindMaterialsFactory::createSolarRangeMaterial([[maybe_unused]] EnergyPlusData &state)
    {
        int blindDataPtr = m_MaterialProperties.BlindDataPtr;
        assert(blindDataPtr > 0);
        auto &blind = state.dataMaterial->Blind(blindDataPtr);

        CWavelengthRange aRange(WavelengthRange::Solar);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = blind.SlatTransSolDiffDiff;
        Real64 Tb = blind.SlatTransSolDiffDiff;
        Real64 Rf = blind.SlatFrontReflSolDiffDiff;
        Real64 Rb = blind.SlatBackReflSolDiffDiff;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenMaterialsFactory::CWCEScreenMaterialsFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCEMaterialDualBandFactory(t_Material, t_Range)
    {
        // Current EnergyPlus model does not support material transmittance different from zero.
        // To enable that, it would be necessary to change input in IDF
    }

    std::shared_ptr<CMaterialSingleBand> CWCEScreenMaterialsFactory::createVisibleRangeMaterial([[maybe_unused]] EnergyPlusData &state)
    {
        CWavelengthRange aRange(WavelengthRange::Visible);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = 0.0;
        Real64 Tb = 0.0;
        Real64 Rf = m_MaterialProperties.ReflectShadeVis;
        Real64 Rb = m_MaterialProperties.ReflectShadeVis;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    std::shared_ptr<CMaterialSingleBand> CWCEScreenMaterialsFactory::createSolarRangeMaterial([[maybe_unused]] EnergyPlusData &state)
    {
        CWavelengthRange aRange(WavelengthRange::Solar);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = 0.0;
        Real64 Tb = 0.0;
        Real64 Rf = m_MaterialProperties.ReflectShade;
        Real64 Rb = m_MaterialProperties.ReflectShade;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeMaterialsFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeMaterialsFactory::CWCEDiffuseShadeMaterialsFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCEMaterialDualBandFactory(t_Material, t_Range)
    {
    }

    std::shared_ptr<CMaterialSingleBand> CWCEDiffuseShadeMaterialsFactory::createVisibleRangeMaterial([[maybe_unused]] EnergyPlusData &state)
    {
        CWavelengthRange aRange(WavelengthRange::Visible);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = m_MaterialProperties.TransVis;
        Real64 Tb = m_MaterialProperties.TransVis;
        Real64 Rf = m_MaterialProperties.ReflectShadeVis;
        Real64 Rb = m_MaterialProperties.ReflectShadeVis;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    std::shared_ptr<CMaterialSingleBand> CWCEDiffuseShadeMaterialsFactory::createSolarRangeMaterial([[maybe_unused]] EnergyPlusData &state)
    {
        CWavelengthRange aRange(WavelengthRange::Solar);
        Real64 lowLambda = aRange.minLambda();
        Real64 highLambda = aRange.maxLambda();

        Real64 Tf = m_MaterialProperties.Trans;
        Real64 Tb = m_MaterialProperties.Trans;
        Real64 Rf = m_MaterialProperties.ReflectShade;
        Real64 Rb = m_MaterialProperties.ReflectShade;

        return std::make_shared<CMaterialSingleBand>(Tf, Tb, Rf, Rb, lowLambda, highLambda);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCECellFactory
    ///////////////////////////////////////////////////////////////////////////////
    IWCECellDescriptionFactory::IWCECellDescriptionFactory(const Material::MaterialChild &t_Material) : m_Material(t_Material)
    {
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecularCellFactory
    ///////////////////////////////////////////////////////////////////////////////

    CWCESpecularCellFactory::CWCESpecularCellFactory(const Material::MaterialChild &t_Material) : IWCECellDescriptionFactory(t_Material)
    {
    }

    std::shared_ptr<ICellDescription> CWCESpecularCellFactory::getCellDescription([[maybe_unused]] EnergyPlusData &state)
    {
        return std::make_shared<CSpecularCellDescription>();
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindCellFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindCellFactory::CWCEVenetianBlindCellFactory(const Material::MaterialChild &t_Material) : IWCECellDescriptionFactory(t_Material)
    {
    }

    std::shared_ptr<ICellDescription> CWCEVenetianBlindCellFactory::getCellDescription([[maybe_unused]] EnergyPlusData &state)
    {
        const int blindDataPtr = m_Material.BlindDataPtr;
        assert(blindDataPtr > 0);
        auto &blind = state.dataMaterial->Blind(blindDataPtr);

        Real64 slatWidth = blind.SlatWidth;
        Real64 slatSpacing = blind.SlatSeparation;
        Real64 slatTiltAngle = 90.0 - blind.SlatAngle; // Need to convert to WCE system
        Real64 curvatureRadius = 0.0;                  // No curvature radius in current IDF definition
        size_t numOfSlatSegments = 5;                  // Number of segments to use in venetian calculations
        return std::make_shared<CVenetianCellDescription>(slatWidth, slatSpacing, slatTiltAngle, curvatureRadius, numOfSlatSegments);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenCellFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenCellFactory::CWCEScreenCellFactory(const Material::MaterialChild &t_Material) : IWCECellDescriptionFactory(t_Material)
    {
    }

    std::shared_ptr<ICellDescription> CWCEScreenCellFactory::getCellDescription([[maybe_unused]] EnergyPlusData &state)
    {
        Real64 diameter = m_Material.Thickness; // Thickness in this case is diameter
        // ratio is not saved withing material but rather calculated from transmittance
        const Real64 ratio = 1.0 - sqrt(m_Material.Trans);
        Real64 spacing = diameter / ratio;
        return std::make_shared<CWovenCellDescription>(diameter, spacing);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeCellFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeCellFactory::CWCEDiffuseShadeCellFactory(const Material::MaterialChild &t_Material) : IWCECellDescriptionFactory(t_Material)
    {
    }

    std::shared_ptr<ICellDescription> CWCEDiffuseShadeCellFactory::getCellDescription([[maybe_unused]] EnergyPlusData &state)
    {
        return std::make_shared<CFlatCellDescription>();
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEBSDFLayerFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCELayerFactory::CWCELayerFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : m_Material(t_Material), m_Range(t_Range), m_BSDFInitialized(false), m_SimpleInitialized(false), m_MaterialFactory(nullptr)
    {
    }

    std::pair<std::shared_ptr<CMaterial>, std::shared_ptr<ICellDescription>> CWCELayerFactory::init(EnergyPlusData &state)
    {
        createMaterialFactory();
        auto aMaterial = m_MaterialFactory->getMaterial(state); // (AUTO_OK_SHARED_PTR)
        assert(aMaterial != nullptr);
        auto aCellDescription = getCellDescription(state); // (AUTO_OK_SHARED_PTR)
        assert(aCellDescription != nullptr);

        return std::make_pair(aMaterial, aCellDescription);
    }

    std::shared_ptr<CBSDFLayer> CWCELayerFactory::getBSDFLayer(EnergyPlusData &state)
    {
        if (!m_BSDFInitialized) {
            auto res = init(state);                                      // (AUTO_OK_SHARED_PTR)
            const auto aBSDF = CBSDFHemisphere::create(BSDFBasis::Full); // (AUTO_OK_OBJ)

            CBSDFLayerMaker aMaker(res.first, aBSDF, res.second);
            m_BSDFLayer = aMaker.getLayer();
            m_BSDFInitialized = true;
        }
        return m_BSDFLayer;
    }

    CScatteringLayer CWCELayerFactory::getLayer(EnergyPlusData &state)
    {
        if (!m_SimpleInitialized) {
            auto res = init(state); // (AUTO_OK_SHARED_PTR)

            m_ScatteringLayer = CScatteringLayer(res.first, res.second);
            m_SimpleInitialized = true;
        }
        return m_ScatteringLayer;
    }

    std::shared_ptr<ICellDescription> CWCELayerFactory::getCellDescription([[maybe_unused]] EnergyPlusData &state) const
    {
        return m_CellFactory->getCellDescription(state);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCESpecularLayerFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCESpecularLayerFactory::CWCESpecularLayerFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCELayerFactory(t_Material, t_Range)
    {
        m_CellFactory = std::make_shared<CWCESpecularCellFactory>(t_Material);
    }

    void CWCESpecularLayerFactory::createMaterialFactory()
    {
        m_MaterialFactory = std::make_shared<CWCESpecularMaterialsFactory>(m_Material, m_Range);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEVenetianBlindLayerFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEVenetianBlindLayerFactory::CWCEVenetianBlindLayerFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCELayerFactory(t_Material, t_Range)
    {
        m_CellFactory = std::make_shared<CWCEVenetianBlindCellFactory>(t_Material);
    }

    void CWCEVenetianBlindLayerFactory::createMaterialFactory()
    {
        m_MaterialFactory = std::make_shared<CWCEVenetianBlindMaterialsFactory>(m_Material, m_Range);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEScreenLayerFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEScreenLayerFactory::CWCEScreenLayerFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCELayerFactory(t_Material, t_Range)
    {
        m_CellFactory = std::make_shared<CWCEScreenCellFactory>(t_Material);
    }

    void CWCEScreenLayerFactory::createMaterialFactory()
    {
        m_MaterialFactory = std::make_shared<CWCEScreenMaterialsFactory>(m_Material, m_Range);
    }

    ///////////////////////////////////////////////////////////////////////////////
    //   CWCEDiffuseShadeLayerFactory
    ///////////////////////////////////////////////////////////////////////////////
    CWCEDiffuseShadeLayerFactory::CWCEDiffuseShadeLayerFactory(const Material::MaterialChild &t_Material, const WavelengthRange t_Range)
        : CWCELayerFactory(t_Material, t_Range)
    {
        m_CellFactory = std::make_shared<CWCEDiffuseShadeCellFactory>(t_Material);
    }

    void CWCEDiffuseShadeLayerFactory::createMaterialFactory()
    {
        m_MaterialFactory = std::make_shared<CWCEDiffuseShadeMaterialsFactory>(m_Material, m_Range);
    }

} // namespace WindowManager
} // namespace EnergyPlus
