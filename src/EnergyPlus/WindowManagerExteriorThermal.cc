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

// EnergyPlus headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowManager.hh>

// Windows library headers
#include <WCETarcog.hpp>

// EnergyPlus headers
#include <EnergyPlus/WindowManagerExteriorThermal.hh>

namespace EnergyPlus {

using namespace Tarcog;
using namespace Gases;
using namespace FenestrationCommon;

using namespace DataEnvironment;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace General;

namespace WindowManager {

    /////////////////////////////////////////////////////////////////////////////////////////
    void CalcWindowHeatBalanceExternalRoutines(EnergyPlusData &state,
                                               int const SurfNum,          // Surface number
                                               Real64 const HextConvCoeff, // Outside air film conductance coefficient
                                               Real64 &SurfInsideTemp,     // Inside window surface temperature
                                               Real64 &SurfOutsideTemp     // Outside surface temperature (C)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Main wrapper routine to pick-up data from EnergyPlus and then call Windows-CalcEngine routines
        // to obtain results

        auto &window(state.dataSurface->SurfaceWindow(SurfNum));
        auto &surface(state.dataSurface->Surface(SurfNum));
        auto ConstrNum = surface.Construction;
        auto &construction(state.dataConstruction->Construct(ConstrNum));

        auto const solutionTolerance = 0.02;

        // Tarcog thermal system for solving heat transfer through the window
        auto aFactory = CWCEHeatTransferFactory(state, surface, SurfNum);
        auto aSystem = aFactory.getTarcogSystem(state, HextConvCoeff);
        aSystem->setTolerance(solutionTolerance);

        // get previous timestep temperatures solution for faster iterations
        std::vector<Real64> Guess;
        auto totSolidLayers = construction.TotSolidLayers;

        // Interior and exterior shading layers have gas between them and IGU but that gas
        // was not part of construction so it needs to be increased by one
        if (ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(SurfNum)) || ANY_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
            ++totSolidLayers;
        }

        for (auto k = 1; k <= 2 * totSolidLayers; ++k) {
            Guess.push_back(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(k));
        }

        try {
            aSystem->setInitialGuess(Guess);
            aSystem->solve();
        } catch (const std::exception &ex) {
            ShowSevereError(state, "Error in Windows Calculation Engine Exterior Module.");
            ShowContinueError(state, ex.what());
        }

        auto aLayers = aSystem->getSolidLayers();
        auto i = 1;
        for (const auto &aLayer : aLayers) {
            Real64 aTemp = 0;
            for (auto aSide : EnumSide()) {
                aTemp = aLayer->getTemperature(aSide);
                state.dataWindowManager->thetas(i) = aTemp;
                if (i == 1) {
                    SurfOutsideTemp = aTemp - DataGlobalConstants::KelvinConv;
                }
                ++i;
            }
            SurfInsideTemp = aTemp - DataGlobalConstants::KelvinConv;
            if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                Real64 EffShBlEmiss;
                Real64 EffGlEmiss;
                if (state.dataSurface->SurfWinMovableSlats(SurfNum)) {
                    EffShBlEmiss =
                        General::InterpGeneral(window.EffShBlindEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                               window.EffShBlindEmiss(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                               state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                    EffGlEmiss =
                        General::InterpGeneral(window.EffGlassEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                                               window.EffGlassEmiss(std::min(MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                                               state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                } else {
                    EffShBlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                    EffGlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(1);
                }
                state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                    (EffShBlEmiss * SurfInsideTemp +
                     EffGlEmiss * (state.dataWindowManager->thetas(2 * totSolidLayers - 2) - state.dataWindowManager->TKelvin)) /
                    (EffShBlEmiss + EffGlEmiss);
            }
        }

        state.dataHeatBal->HConvIn(SurfNum) = aSystem->getHc(Environment::Indoor);
        if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)) || aFactory.isInteriorShade()) {
            // It is not clear why EnergyPlus keeps this interior calculations separately for interior shade. This does create different
            // solution from heat transfer from tarcog itself. Need to confirm with LBNL team about this approach. Note that heat flow
            // through shade (consider case when openings are zero) is different from heat flow obtained by these equations. Will keep
            // these calculations just to confirm that current exterior engine is giving close results to what is in here. (Simon)
            auto totLayers = aLayers.size();
            state.dataWindowManager->nglface = 2 * totLayers - 2;
            state.dataWindowManager->nglfacep = state.dataWindowManager->nglface + 2;
            auto aShadeLayer = aLayers[totLayers - 1];
            auto aGlassLayer = aLayers[totLayers - 2];
            auto ShadeArea = state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum);
            auto frontSurface = aShadeLayer->getSurface(Side::Front);
            auto backSurface = aShadeLayer->getSurface(Side::Back);
            auto EpsShIR1 = frontSurface->getEmissivity();
            auto EpsShIR2 = backSurface->getEmissivity();
            auto TauShIR = frontSurface->getTransmittance();
            auto RhoShIR1 = max(0.0, 1.0 - TauShIR - EpsShIR1);
            auto RhoShIR2 = max(0.0, 1.0 - TauShIR - EpsShIR2);
            auto glassEmiss = aGlassLayer->getSurface(Side::Back)->getEmissivity();
            auto RhoGlIR2 = 1.0 - glassEmiss;
            auto ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
            auto rmir = surface.getInsideIR(state, SurfNum);
            auto NetIRHeatGainShade =
                ShadeArea * EpsShIR2 *
                    (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas(state.dataWindowManager->nglfacep), 4) - rmir) +
                EpsShIR1 * (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas(state.dataWindowManager->nglfacep - 1), 4) - rmir) *
                    RhoGlIR2 * TauShIR / ShGlReflFacIR;
            auto NetIRHeatGainGlass =
                ShadeArea * (glassEmiss * TauShIR / ShGlReflFacIR) *
                (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas(state.dataWindowManager->nglface), 4) - rmir);
            auto tind = surface.getInsideAirTemperature(state, SurfNum) + DataGlobalConstants::KelvinConv;
            auto ConvHeatGainFrZoneSideOfShade =
                ShadeArea * state.dataHeatBal->HConvIn(SurfNum) * (state.dataWindowManager->thetas(state.dataWindowManager->nglfacep) - tind);
            state.dataSurface->SurfWinHeatGain(SurfNum) =
                state.dataSurface->SurfWinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfShade + NetIRHeatGainGlass + NetIRHeatGainShade;
            state.dataSurface->SurfWinHeatTransfer(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);

            state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;

            // Effective shade and glass emissivities that are used later for energy calculations.
            // This needs to be checked as well. (Simon)
            auto EffShBlEmiss = EpsShIR1 * (1.0 + RhoGlIR2 * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2));
            state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss = EffShBlEmiss;

            auto EffGlEmiss = glassEmiss * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2);
            state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss = EffGlEmiss;

            auto glassTemperature = aGlassLayer->getSurface(Side::Back)->getTemperature();
            state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (glassTemperature - DataGlobalConstants::KelvinConv)) / (EffShBlEmiss + EffGlEmiss);

        } else {
            // Another adoptation to old source that looks suspicious. Check if heat flow through
            // window is actually matching these values. (Simon)

            //
            auto totLayers = aLayers.size();
            auto aGlassLayer = aLayers[totLayers - 1];
            auto backSurface = aGlassLayer->getSurface(Side::Back);

            auto h_cin = aSystem->getHc(Environment::Indoor);
            auto ConvHeatGainFrZoneSideOfGlass =
                surface.Area * h_cin * (backSurface->getTemperature() - aSystem->getAirTemperature(Environment::Indoor));

            auto rmir = surface.getInsideIR(state, SurfNum);
            auto NetIRHeatGainGlass =
                surface.Area * backSurface->getEmissivity() * (state.dataWindowManager->sigma * pow(backSurface->getTemperature(), 4) - rmir);

            state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) = aLayers[totLayers - 1]->getTemperature(Side::Back) - state.dataWindowManager->TKelvin;
            state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss = aLayers[totLayers - 1]->getSurface(Side::Back)->getEmissivity();

            state.dataSurface->SurfWinHeatGain(SurfNum) =
                state.dataSurface->SurfWinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;
            state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) = ConvHeatGainFrZoneSideOfGlass;
            state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;

            state.dataSurface->SurfWinHeatTransfer(SurfNum) = state.dataSurface->SurfWinHeatGain(SurfNum);
        }

        auto TransDiff = construction.TransDiff;
        state.dataSurface->SurfWinHeatGain(SurfNum) -= state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) * surface.Area * TransDiff;
        state.dataSurface->SurfWinHeatTransfer(SurfNum) -= state.dataHeatBal->EnclSolQSWRad(surface.SolarEnclIndex) * surface.Area * TransDiff;
        state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum) =
            state.dataHeatBal->EnclSolQSWRad(state.dataSurface->Surface(SurfNum).SolarEnclIndex) * surface.Area * TransDiff;

        for (auto k = 1; k <= surface.getTotLayers(state); ++k) {
            state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * k - 1) = state.dataWindowManager->thetas(2 * k - 1);
            state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * k) = state.dataWindowManager->thetas(2 * k);

            // temperatures for reporting
            state.dataHeatBal->SurfWinFenLaySurfTempFront(SurfNum, k) = state.dataWindowManager->thetas(2 * k - 1) - DataGlobalConstants::KelvinConv;
            state.dataHeatBal->SurfWinFenLaySurfTempBack(SurfNum, k) = state.dataWindowManager->thetas(2 * k) - DataGlobalConstants::KelvinConv;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //  CWCEHeatTransferFactory
    /////////////////////////////////////////////////////////////////////////////////////////

    CWCEHeatTransferFactory::CWCEHeatTransferFactory(EnergyPlusData &state, SurfaceData const &surface, int const t_SurfNum)
        : m_Surface(surface), m_SurfNum(t_SurfNum), m_SolidLayerIndex(0), m_InteriorBSDFShade(false), m_ExteriorShade(false)
    {
        m_Window = state.dataSurface->SurfaceWindow(t_SurfNum);
        auto ShadeFlag = state.dataSurface->SurfWinShadingFlag(t_SurfNum);

        m_ConstructionNumber = m_Surface.Construction;
        m_ShadePosition = ShadePosition::NoShade;

        if (ANY_SHADE_SCREEN(ShadeFlag) || ANY_BLIND(ShadeFlag)) {
            m_ConstructionNumber = state.dataSurface->SurfWinActiveShadedConstruction(t_SurfNum);
        }

        m_TotLay = getNumOfLayers(state);

        if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Interior;
        }

        if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Exterior;
        }

        if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Between;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSingleSystem> CWCEHeatTransferFactory::getTarcogSystem(EnergyPlusData &state, Real64 const t_HextConvCoeff)
    {
        auto Indoor = getIndoor(state);
        auto Outdoor = getOutdoor(state, t_HextConvCoeff);
        auto aIGU = getIGU();

        // pick-up all layers and put them in IGU (this includes gap layers as well)
        for (auto i = 0; i < m_TotLay; ++i) {
            auto aLayer = getIGULayer(state, i + 1);
            assert(aLayer != nullptr);
            // IDF for "standard" windows do not insert gas between glass and shade. Tarcog needs that gas
            // and it will be created here
            if (m_ShadePosition == ShadePosition::Interior && i == m_TotLay - 1) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1);
                aIGU->addLayer(aAirLayer);
            }
            aIGU->addLayer(aLayer);
            if (m_ShadePosition == ShadePosition::Exterior && i == 0) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1);
                aIGU->addLayer(aAirLayer);
            }
        }

        auto aSystem = std::make_shared<CSingleSystem>(aIGU, Indoor, Outdoor);

        return aSystem;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    Material::MaterialProperties *CWCEHeatTransferFactory::getLayerMaterial(EnergyPlusData &state, int const t_Index) const
    {
        auto ConstrNum = m_Surface.Construction;

        if (ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(m_SurfNum)) || ANY_BLIND(state.dataSurface->SurfWinShadingFlag(m_SurfNum))) {
            ConstrNum = state.dataSurface->SurfWinActiveShadedConstruction(m_SurfNum);
        }

        auto &construction(state.dataConstruction->Construct(ConstrNum));
        auto LayPtr = construction.LayerPoint(t_Index);
        return &state.dataMaterial->Material(LayPtr);
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getIGULayer(EnergyPlusData &state, int const t_Index)
    {
        std::shared_ptr<CBaseIGULayer> aLayer = nullptr;

        auto material = getLayerMaterial(state, t_Index);

        auto matGroup = material->Group;

        if (matGroup == WindowGlass || matGroup == WindowSimpleGlazing || matGroup == WindowBlind || matGroup == Shade || matGroup == Screen ||
            matGroup == ComplexWindowShade) {
            ++m_SolidLayerIndex;
            aLayer = getSolidLayer(state, m_Surface, *material, m_SolidLayerIndex, m_SurfNum);
        } else if (matGroup == WindowGas || matGroup == WindowGasMixture) {
            aLayer = getGapLayer(*material);
        } else if (matGroup == ComplexWindowGap) {
            aLayer = getComplexGapLayer(state, *material);
        }

        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    int CWCEHeatTransferFactory::getNumOfLayers(EnergyPlusData &state) const
    {
        return state.dataConstruction->Construct(m_ConstructionNumber).TotLayers;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getSolidLayer(
        EnergyPlusData &state, SurfaceData const &surface, Material::MaterialProperties const &material, int const t_Index, int const t_SurfNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates solid layer object from material properties in EnergyPlus
        auto emissFront = 0.0;
        auto emissBack = 0.0;
        auto transThermalFront = 0.0;
        auto transThermalBack = 0.0;
        auto thickness = 0.0;
        auto conductivity = 0.0;
        auto createOpenness = false;
        auto Atop = 0.0;
        auto Abot = 0.0;
        auto Aleft = 0.0;
        auto Aright = 0.0;
        auto Afront = 0.0;

        if (material.Group == WindowGlass || material.Group == WindowSimpleGlazing) {
            emissFront = material.AbsorpThermalFront;
            emissBack = material.AbsorpThermalBack;
            transThermalFront = material.TransThermal;
            transThermalBack = material.TransThermal;
            thickness = material.Thickness;
            conductivity = material.Conductivity;
        }
        if (material.Group == WindowBlind) {
            auto blNum = state.dataSurface->SurfWinBlindNumber(m_SurfNum);
            auto blind = state.dataHeatBal->Blind(blNum);
            thickness = blind.SlatThickness;
            conductivity = blind.SlatConductivity;
            Atop = blind.BlindTopOpeningMult;
            Abot = blind.BlindBottomOpeningMult;
            Aleft = blind.BlindLeftOpeningMult;
            Aright = blind.BlindRightOpeningMult;
            Afront = state.dataSurface->SurfWinBlindAirFlowPermeability(m_SurfNum);
            emissFront = InterpSlatAng(
                state.dataSurface->SurfWinSlatAngThisTS(m_SurfNum), state.dataSurface->SurfWinMovableSlats(m_SurfNum), blind.IRFrontEmiss);
            emissBack = InterpSlatAng(
                state.dataSurface->SurfWinSlatAngThisTS(m_SurfNum), state.dataSurface->SurfWinMovableSlats(m_SurfNum), blind.IRBackEmiss);
            transThermalFront = InterpSlatAng(
                state.dataSurface->SurfWinSlatAngThisTS(m_SurfNum), state.dataSurface->SurfWinMovableSlats(m_SurfNum), blind.IRFrontTrans);
            transThermalBack = InterpSlatAng(
                state.dataSurface->SurfWinSlatAngThisTS(m_SurfNum), state.dataSurface->SurfWinMovableSlats(m_SurfNum), blind.IRBackTrans);
            if (t_Index == 1) {
                m_ExteriorShade = true;
            }
        }
        if (material.Group == Shade) {
            emissFront = material.AbsorpThermal;
            emissBack = material.AbsorpThermal;
            transThermalFront = material.TransThermal;
            transThermalBack = material.TransThermal;
            thickness = material.Thickness;
            conductivity = material.Conductivity;
            Atop = material.WinShadeTopOpeningMult;
            Abot = material.WinShadeBottomOpeningMult;
            Aleft = material.WinShadeLeftOpeningMult;
            Aright = material.WinShadeRightOpeningMult;
            Afront = material.WinShadeAirFlowPermeability;
            if (t_Index == 1) {
                m_ExteriorShade = true;
            }
        }
        if (material.Group == Screen) {
            // Simon: Existing code already takes into account geometry of Woven and scales down
            // emissivity for openning area.
            emissFront = material.AbsorpThermal;
            emissBack = material.AbsorpThermal;
            transThermalFront = material.TransThermal;
            transThermalBack = material.TransThermal;
            thickness = material.Thickness;
            conductivity = material.Conductivity;
            Atop = material.WinShadeTopOpeningMult;
            Abot = material.WinShadeBottomOpeningMult;
            Aleft = material.WinShadeLeftOpeningMult;
            Aright = material.WinShadeRightOpeningMult;
            Afront = material.WinShadeAirFlowPermeability;
            if (t_Index == 1) {
                m_ExteriorShade = true;
            }
        }
        if (material.Group == ComplexWindowShade) {
            auto shdPtr = material.ComplexShadePtr;
            auto &shade(state.dataHeatBal->ComplexShade(shdPtr));
            thickness = shade.Thickness;
            conductivity = shade.Conductivity;
            emissFront = shade.FrontEmissivity;
            emissBack = shade.BackEmissivity;
            transThermalFront = shade.IRTransmittance;
            transThermalBack = shade.IRTransmittance;
            Afront = shade.FrontOpeningMultiplier;
            Atop = shade.TopOpeningMultiplier;
            Abot = shade.BottomOpeningMultiplier;
            Aleft = shade.LeftOpeningMultiplier;
            Aright = shade.RightOpeningMultiplier;
            createOpenness = true;
            m_InteriorBSDFShade = ((2 * t_Index - 1) == m_TotLay);
        }

        std::shared_ptr<ISurface> frontSurface = std::make_shared<CSurface>(emissFront, transThermalFront);
        std::shared_ptr<ISurface> backSurface = std::make_shared<CSurface>(emissBack, transThermalBack);
        auto aSolidLayer = std::make_shared<CIGUSolidLayer>(thickness, conductivity, frontSurface, backSurface);
        if (createOpenness) {
            auto aOpenings = std::make_shared<CShadeOpenings>(Atop, Abot, Aleft, Aright, Afront);
            aSolidLayer = std::make_shared<CIGUShadeLayer>(aSolidLayer, aOpenings);
        }
        auto swRadiation = surface.getSWIncident(state, t_SurfNum);
        if (swRadiation > 0) {

            auto absCoeff = state.dataHeatBal->SurfWinQRadSWwinAbs(t_SurfNum, t_Index) / swRadiation;
            if ((2 * t_Index - 1) == m_TotLay) {
                absCoeff += state.dataHeatBal->SurfQRadThermInAbs(t_SurfNum) / swRadiation;
            }

            aSolidLayer->setSolarAbsorptance(absCoeff);
        }
        return aSolidLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getGapLayer(Material::MaterialProperties const &material) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        auto const pres = 1e5; // Old code uses this constant pressure
        auto thickness = material.Thickness;
        auto aGas = getGas(material);
        std::shared_ptr<CBaseIGULayer> aLayer = std::make_shared<CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getShadeToGlassLayer(EnergyPlusData &state, int const t_Index) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   August 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        auto const pres = 1e5; // Old code uses this constant pressure
        auto aGas = getAir();
        auto thickness = 0.0;

        if (state.dataSurface->SurfWinShadingFlag(m_SurfNum) == WinShadingType::IntBlind ||
            state.dataSurface->SurfWinShadingFlag(m_SurfNum) == WinShadingType::ExtBlind) {
            thickness = state.dataHeatBal->Blind(state.dataSurface->SurfWinBlindNumber(m_SurfNum)).BlindToGlassDist;
        }
        if (state.dataSurface->SurfWinShadingFlag(m_SurfNum) == WinShadingType::IntShade ||
            state.dataSurface->SurfWinShadingFlag(m_SurfNum) == WinShadingType::ExtShade ||
            state.dataSurface->SurfWinShadingFlag(m_SurfNum) == WinShadingType::ExtScreen) {
            auto material = getLayerMaterial(state, t_Index);
            thickness = material->WinShadeToGlassDist;
        }
        std::shared_ptr<CBaseIGULayer> aLayer = std::make_shared<CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getComplexGapLayer(EnergyPlusData &state,
                                                                               Material::MaterialProperties const &material) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        auto const pres = 1e5; // Old code uses this constant pressure
        auto thickness = material.Thickness;
        auto gasPointer = material.GasPointer;
        auto &gasMaterial(state.dataMaterial->Material(gasPointer));
        auto aGas = getGas(gasMaterial);
        std::shared_ptr<CBaseIGULayer> aLayer = std::make_shared<CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CGas> CWCEHeatTransferFactory::getGas(Material::MaterialProperties const &material) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        auto numGases = material.NumberOfGasesInMixture;
        auto const vacuumCoeff = 1.4; // Load vacuum coefficient once it is implemented (Simon).
        auto gasName = material.Name;
        auto aGas = std::make_shared<CGas>();
        for (auto i = 1; i <= numGases; ++i) {
            auto wght = material.GasWght(i);
            auto fract = material.GasFract(i);
            std::vector<double> gcon;
            std::vector<double> gvis;
            std::vector<double> gcp;
            for (auto j = 1; j <= 3; ++j) {
                gcon.push_back(material.GasCon(j, i));
                gvis.push_back(material.GasVis(j, i));
                gcp.push_back(material.GasCp(j, i));
            }
            auto aCon = CIntCoeff(gcon[0], gcon[1], gcon[2]);
            auto aCp = CIntCoeff(gcp[0], gcp[1], gcp[2]);
            auto aVis = CIntCoeff(gvis[0], gvis[1], gvis[2]);
            auto aData = CGasData(gasName, wght, vacuumCoeff, aCp, aCon, aVis);
            auto aGasItem = CGasItem(fract, aData);
            aGas->addGasItem(aGasItem);
        }
        return aGas;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CGas> CWCEHeatTransferFactory::getAir() const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   August 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates air gas layer for tarcog routines
        auto aGas = std::make_shared<CGas>();
        return aGas;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CEnvironment> CWCEHeatTransferFactory::getIndoor(EnergyPlusData &state) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates indoor environment object from surface properties in EnergyPlus
        auto tin = m_Surface.getInsideAirTemperature(state, m_SurfNum) + DataGlobalConstants::KelvinConv;
        auto hcin = state.dataHeatBal->HConvIn(m_SurfNum);

        auto IR = m_Surface.getInsideIR(state, m_SurfNum);

        std::shared_ptr<CEnvironment> Indoor = std::make_shared<CIndoorEnvironment>(tin, state.dataEnvrn->OutBaroPress);
        Indoor->setHCoeffModel(BoundaryConditionsCoeffModel::CalculateH, hcin);
        Indoor->setEnvironmentIR(IR);
        return Indoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CEnvironment> CWCEHeatTransferFactory::getOutdoor(EnergyPlusData &state, const Real64 t_Hext) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates outdoor environment object from surface properties in EnergyPlus
        double tout = m_Surface.getOutsideAirTemperature(state, m_SurfNum) + DataGlobalConstants::KelvinConv;
        double IR = m_Surface.getOutsideIR(state, m_SurfNum);
        // double dirSolRad = QRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
        double swRadiation = m_Surface.getSWIncident(state, m_SurfNum);
        double tSky = state.dataEnvrn->SkyTempKelvin;
        double airSpeed = 0.0;
        if (m_Surface.ExtWind) {
            airSpeed = state.dataSurface->SurfOutWindSpeed(m_SurfNum);
        }
        double fclr = 1 - state.dataEnvrn->CloudFraction;
        AirHorizontalDirection airDirection = AirHorizontalDirection::Windward;
        std::shared_ptr<CEnvironment> Outdoor = std::make_shared<COutdoorEnvironment>(
            tout, state.dataEnvrn->OutBaroPress, airSpeed, swRadiation, airDirection, tSky, SkyModel::AllSpecified, fclr);
        Outdoor->setHCoeffModel(BoundaryConditionsCoeffModel::HcPrescribed, t_Hext);
        Outdoor->setEnvironmentIR(IR);
        return Outdoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CIGU> CWCEHeatTransferFactory::getIGU()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates IGU object from surface properties in EnergyPlus

        std::shared_ptr<CIGU> aIGU = std::make_shared<CIGU>(m_Surface.Width, m_Surface.Height, m_Surface.Tilt);
        return aIGU;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    bool CWCEHeatTransferFactory::isInteriorShade() const
    {
        return m_InteriorBSDFShade;
    }

} // namespace WindowManager
} // namespace EnergyPlus
