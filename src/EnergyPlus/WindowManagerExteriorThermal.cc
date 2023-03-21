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

// EnergyPlus headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowManager.hh>

// Windows library headers
#include <WCEMultiLayerOptics.hpp>
#include <WCETarcog.hpp>

// EnergyPlus headers
#include <EnergyPlus/WindowManagerExteriorThermal.hh>

namespace EnergyPlus {

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

        auto &window = state.dataSurface->SurfaceWindow(SurfNum);
        auto &surface = state.dataSurface->Surface(SurfNum);
        int ConstrNum = surface.Construction;
        auto &construction = state.dataConstruction->Construct(ConstrNum);

        constexpr Real64 solutionTolerance = 0.02;

        // Tarcog thermal system for solving heat transfer through the window
        int activeConstrNum = CWCEHeatTransferFactory::getActiveConstructionNumber(state, surface, SurfNum);
        auto aFactory = CWCEHeatTransferFactory(state, surface, SurfNum, activeConstrNum); // (AUTO_OK)
        auto aSystem = aFactory.getTarcogSystem(state, HextConvCoeff);                     // (AUTO_OK_SHARED_PTR)
        aSystem->setTolerance(solutionTolerance);

        // get previous timestep temperatures solution for faster iterations
        std::vector<Real64> Guess;
        int totSolidLayers = construction.TotSolidLayers;

        // Interior and exterior shading layers have gas between them and IGU but that gas
        // was not part of construction so it needs to be increased by one
        if (ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(SurfNum)) || ANY_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
            ++totSolidLayers;
        }

        for (int k = 1; k <= 2 * totSolidLayers; ++k) {
            Guess.push_back(state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(k));
        }

        try {
            aSystem->setInitialGuess(Guess);
            aSystem->solve();
        } catch (const std::exception &ex) {
            ShowSevereError(state, "Error in Windows Calculation Engine Exterior Module.");
            ShowContinueError(state, ex.what());
        }

        auto aLayers = aSystem->getSolidLayers(); // (AUTO_OK_OBJ)
        int i = 1;
        for (const auto &aLayer : aLayers) { // (AUTO_OK_SHARED_PTR)
            Real64 aTemp = 0;
            for (auto aSide : FenestrationCommon::EnumSide()) { // (AUTO_OK) I don't understand what this construct is
                aTemp = aLayer->getTemperature(aSide);
                state.dataWindowManager->thetas[i - 1] = aTemp;
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
                    EffShBlEmiss = General::InterpGeneral(
                        window.EffShBlindEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                        window.EffShBlindEmiss(std::min(Material::MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                        state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                    EffGlEmiss = General::InterpGeneral(
                        window.EffGlassEmiss(state.dataSurface->SurfWinSlatsAngIndex(SurfNum)),
                        window.EffGlassEmiss(std::min(Material::MaxSlatAngs, state.dataSurface->SurfWinSlatsAngIndex(SurfNum) + 1)),
                        state.dataSurface->SurfWinSlatsAngInterpFac(SurfNum));
                } else {
                    EffShBlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss(1);
                    EffGlEmiss = state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss(1);
                }
                state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                    (EffShBlEmiss * SurfInsideTemp +
                     EffGlEmiss * (state.dataWindowManager->thetas[2 * totSolidLayers - 3] - state.dataWindowManager->TKelvin)) /
                    (EffShBlEmiss + EffGlEmiss);
            }
        }

        state.dataHeatBalSurf->SurfHConvInt(SurfNum) = aSystem->getHc(Tarcog::ISO15099::Environment::Indoor);
        if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum)) || aFactory.isInteriorShade()) {
            // It is not clear why EnergyPlus keeps this interior calculations separately for interior shade. This does create different
            // solution from heat transfer from tarcog itself. Need to confirm with LBNL team about this approach. Note that heat flow
            // through shade (consider case when openings are zero) is different from heat flow obtained by these equations. Will keep
            // these calculations just to confirm that current exterior engine is giving close results to what is in here. (Simon)
            int totLayers = aLayers.size();
            state.dataWindowManager->nglface = 2 * totLayers - 2;
            state.dataWindowManager->nglfacep = state.dataWindowManager->nglface + 2;
            auto aShadeLayer = aLayers[totLayers - 1]; // (AUTO_OK_SHARED_PTR)
            auto aGlassLayer = aLayers[totLayers - 2]; // (AUTO_OK_SHARED_PTR)
            Real64 ShadeArea = state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum);
            auto frontSurface = aShadeLayer->getSurface(FenestrationCommon::Side::Front); // (AUTO_OK_SHARED_PTR)
            auto backSurface = aShadeLayer->getSurface(FenestrationCommon::Side::Back);   // (AUTO_OK_SHARED_PTR)
            Real64 EpsShIR1 = frontSurface->getEmissivity();
            Real64 EpsShIR2 = backSurface->getEmissivity();
            Real64 TauShIR = frontSurface->getTransmittance();
            Real64 RhoShIR1 = max(0.0, 1.0 - TauShIR - EpsShIR1);
            Real64 RhoShIR2 = max(0.0, 1.0 - TauShIR - EpsShIR2);
            Real64 glassEmiss = aGlassLayer->getSurface(FenestrationCommon::Side::Back)->getEmissivity();
            Real64 RhoGlIR2 = 1.0 - glassEmiss;
            Real64 ShGlReflFacIR = 1.0 - RhoGlIR2 * RhoShIR1;
            Real64 rmir = state.dataSurface->SurfWinIRfromParentZone(SurfNum) + state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(SurfNum);
            Real64 NetIRHeatGainShade =
                ShadeArea * EpsShIR2 *
                    (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 1], 4) - rmir) +
                EpsShIR1 * (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 2], 4) - rmir) *
                    RhoGlIR2 * TauShIR / ShGlReflFacIR;
            Real64 NetIRHeatGainGlass =
                ShadeArea * (glassEmiss * TauShIR / ShGlReflFacIR) *
                (state.dataWindowManager->sigma * pow(state.dataWindowManager->thetas[state.dataWindowManager->nglface - 1], 4) - rmir);
            Real64 tind = surface.getInsideAirTemperature(state, SurfNum) + DataGlobalConstants::KelvinConv;
            Real64 ConvHeatGainFrZoneSideOfShade = ShadeArea * state.dataHeatBalSurf->SurfHConvInt(SurfNum) *
                                                   (state.dataWindowManager->thetas[state.dataWindowManager->nglfacep - 1] - tind);
            state.dataSurface->SurfWinHeatGain(SurfNum) =
                state.dataSurface->SurfWinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfShade + NetIRHeatGainGlass + NetIRHeatGainShade;

            state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;

            // Effective shade and glass emissivities that are used later for energy calculations.
            // This needs to be checked as well. (Simon)
            Real64 EffShBlEmiss = EpsShIR1 * (1.0 + RhoGlIR2 * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2));
            state.dataSurface->SurfaceWindow(SurfNum).EffShBlindEmiss = EffShBlEmiss;

            Real64 EffGlEmiss = glassEmiss * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2);
            state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss = EffGlEmiss;

            Real64 glassTemperature = aGlassLayer->getSurface(FenestrationCommon::Side::Back)->getTemperature();
            state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (glassTemperature - DataGlobalConstants::KelvinConv)) / (EffShBlEmiss + EffGlEmiss);

        } else {
            // Another adoptation to old source that looks suspicious. Check if heat flow through
            // window is actually matching these values. (Simon)

            //
            int totLayers = aLayers.size();
            auto aGlassLayer = aLayers[totLayers - 1];                                  // (AUTO_OK_SHARED_PTR)
            auto backSurface = aGlassLayer->getSurface(FenestrationCommon::Side::Back); // (AUTO_OK_SHARED_PTR)

            Real64 h_cin = aSystem->getHc(Tarcog::ISO15099::Environment::Indoor);
            Real64 ConvHeatGainFrZoneSideOfGlass =
                surface.Area * h_cin * (backSurface->getTemperature() - aSystem->getAirTemperature(Tarcog::ISO15099::Environment::Indoor));

            Real64 rmir = state.dataSurface->SurfWinIRfromParentZone(SurfNum) + state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(SurfNum);
            Real64 NetIRHeatGainGlass =
                surface.Area * backSurface->getEmissivity() * (state.dataWindowManager->sigma * pow(backSurface->getTemperature(), 4) - rmir);

            state.dataSurface->SurfWinEffInsSurfTemp(SurfNum) =
                aLayers[totLayers - 1]->getTemperature(FenestrationCommon::Side::Back) - state.dataWindowManager->TKelvin;
            state.dataSurface->SurfaceWindow(SurfNum).EffGlassEmiss =
                aLayers[totLayers - 1]->getSurface(FenestrationCommon::Side::Back)->getEmissivity();

            state.dataSurface->SurfWinHeatGain(SurfNum) =
                state.dataSurface->SurfWinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;
            state.dataSurface->SurfWinGainConvGlazToZoneRep(SurfNum) = ConvHeatGainFrZoneSideOfGlass;
            state.dataSurface->SurfWinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;
        }

        Real64 TransDiff = construction.TransDiff;
        state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum) =
            state.dataHeatBal->EnclSolQSWRad(state.dataSurface->Surface(SurfNum).SolarEnclIndex) * surface.Area * TransDiff;
        state.dataSurface->SurfWinHeatGain(SurfNum) -= state.dataSurface->SurfWinLossSWZoneToOutWinRep(SurfNum);

        for (int k = 1; k <= surface.getTotLayers(state); ++k) {
            state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * k - 1) = state.dataWindowManager->thetas[2 * k - 2];
            state.dataSurface->SurfaceWindow(SurfNum).ThetaFace(2 * k) = state.dataWindowManager->thetas[2 * k - 1];

            // temperatures for reporting
            state.dataHeatBal->SurfWinFenLaySurfTempFront(SurfNum, k) = state.dataWindowManager->thetas[2 * k - 2] - DataGlobalConstants::KelvinConv;
            state.dataHeatBal->SurfWinFenLaySurfTempBack(SurfNum, k) = state.dataWindowManager->thetas[2 * k - 1] - DataGlobalConstants::KelvinConv;
        }
    }

    Real64
    GetIGUUValueForNFRCReport(EnergyPlusData &state, const int surfNum, const int constrNum, const Real64 windowWidth, const Real64 windowHeight)
    {
        Real64 tilt = 90.0;

        auto &surface = state.dataSurface->Surface(surfNum);
        auto aFactory = CWCEHeatTransferFactory(state, surface, surfNum, constrNum); // (AUTO_OK)

        const auto winterGlassUnit = aFactory.getTarcogSystemForReporting(state, false, windowWidth, windowHeight, tilt); // (AUTO_OK_SHARED_PTR)

        return winterGlassUnit->getUValue();
    }

    Real64 GetSHGCValueForNFRCReporting(EnergyPlusData &state, int surfNum, int constrNum, Real64 windowWidth, Real64 windowHeight)
    {
        Real64 tilt = 90.0;

        auto &surface = state.dataSurface->Surface(surfNum);
        auto aFactory = CWCEHeatTransferFactory(state, surface, surfNum, constrNum); // (AUTO_OK)

        const auto summerGlassUnit = aFactory.getTarcogSystemForReporting(state, true, windowWidth, windowHeight, tilt); // (AUTO_OK_SHARED_PTR)
        return summerGlassUnit->getSHGC(state.dataConstruction->Construct(surface.Construction).SolTransNorm);
    }

    void GetWindowAssemblyNfrcForReport(EnergyPlusData &state,
                                        int const surfNum,
                                        int constrNum,
                                        Real64 windowWidth,
                                        Real64 windowHeight,
                                        EnergyPlus::DataSurfaces::NfrcVisionType vision,
                                        Real64 &uvalue,
                                        Real64 &shgc,
                                        Real64 &vt)
    {
        auto &surface = state.dataSurface->Surface(surfNum);
        auto &frameDivider = state.dataSurface->FrameDivider(surface.FrameDivider);

        auto aFactory = CWCEHeatTransferFactory(state, surface, surfNum, constrNum); // (AUTO_OK)

        for (bool isSummer : {false, true}) {
            constexpr Real64 framehExtConvCoeff = 30.0;
            constexpr Real64 framehIntConvCoeff = 8.0;
            constexpr Real64 tilt = 90.0;

            auto insulGlassUnit = aFactory.getTarcogSystemForReporting(state, isSummer, windowWidth, windowHeight, tilt); // (AUTO_OK_SHARED_PTR)

            const double centerOfGlassUvalue = insulGlassUnit->getUValue();

            auto winterGlassUnit = aFactory.getTarcogSystemForReporting(state, false, windowWidth, windowHeight, tilt); // (AUTO_OK_SHARED_PTR)

            const double frameUvalue = aFactory.overallUfactorFromFilmsAndCond(frameDivider.FrameConductance, framehIntConvCoeff, framehExtConvCoeff);
            const double frameEdgeUValue = winterGlassUnit->getUValue() * frameDivider.FrEdgeToCenterGlCondRatio; // not sure about this
            const double frameProjectedDimension = frameDivider.FrameWidth;
            const double frameWettedLength = frameProjectedDimension + frameDivider.FrameProjectionIn;
            const double frameAbsorptance = frameDivider.FrameSolAbsorp;

            Tarcog::ISO15099::FrameData frameData{frameUvalue, frameEdgeUValue, frameProjectedDimension, frameWettedLength, frameAbsorptance};

            const double dividerUvalue =
                aFactory.overallUfactorFromFilmsAndCond(frameDivider.DividerConductance, framehIntConvCoeff, framehExtConvCoeff);
            const double dividerEdgeUValue = centerOfGlassUvalue * frameDivider.DivEdgeToCenterGlCondRatio; // not sure about this
            const double dividerProjectedDimension = frameDivider.DividerWidth;
            const double dividerWettedLength = dividerProjectedDimension + frameDivider.DividerProjectionIn;
            const double dividerAbsorptance = frameDivider.DividerSolAbsorp;
            const int numHorizDividers = frameDivider.HorDividers;
            const int numVertDividers = frameDivider.VertDividers;

            Tarcog::ISO15099::FrameData dividerData{
                dividerUvalue, dividerEdgeUValue, dividerProjectedDimension, dividerWettedLength, dividerAbsorptance};

            const Real64 tVis = state.dataConstruction->Construct(constrNum).VisTransNorm;
            const Real64 tSol = state.dataConstruction->Construct(constrNum).SolTransNorm;

            if (vision == DataSurfaces::NfrcVisionType::Single) {
                Tarcog::ISO15099::WindowSingleVision window(windowWidth, windowHeight, tVis, tSol, insulGlassUnit);
                window.setFrameTop(frameData);
                window.setFrameBottom(frameData);
                window.setFrameLeft(frameData);
                window.setFrameRight(frameData);
                window.setDividers(dividerData, numHorizDividers, numVertDividers);

                if (isSummer) {
                    vt = window.vt();
                    shgc = window.shgc();
                } else {
                    uvalue = window.uValue();
                }
            } else if (vision == EnergyPlus::DataSurfaces::NfrcVisionType::DualHorizontal) {
                Tarcog::ISO15099::DualVisionHorizontal window(windowWidth, windowHeight, tVis, tSol, insulGlassUnit, tVis, tSol, insulGlassUnit);
                window.setFrameLeft(frameData);
                window.setFrameRight(frameData);
                window.setFrameBottomLeft(frameData);
                window.setFrameBottomRight(frameData);
                window.setFrameTopLeft(frameData);
                window.setFrameTopRight(frameData);
                window.setFrameMeetingRail(frameData);
                window.setDividers(dividerData, numHorizDividers, numVertDividers);

                if (isSummer) {
                    vt = window.vt();
                    shgc = window.shgc();
                } else {
                    uvalue = window.uValue();
                }
            } else if (vision == EnergyPlus::DataSurfaces::NfrcVisionType::DualVertical) {
                Tarcog::ISO15099::DualVisionVertical window(windowWidth, windowHeight, tVis, tSol, insulGlassUnit, tVis, tSol, insulGlassUnit);
                window.setFrameTop(frameData);
                window.setFrameBottom(frameData);
                window.setFrameTopLeft(frameData);
                window.setFrameTopRight(frameData);
                window.setFrameBottomLeft(frameData);
                window.setFrameBottomRight(frameData);
                window.setFrameMeetingRail(frameData);
                window.setDividers(dividerData, numHorizDividers, numVertDividers);

                if (isSummer) {
                    vt = window.vt();
                    shgc = window.shgc();
                } else {
                    uvalue = window.uValue();
                }
            } else {
                Tarcog::ISO15099::WindowSingleVision window(windowWidth, windowHeight, tVis, tSol, insulGlassUnit);
                window.setFrameTop(frameData);
                window.setFrameBottom(frameData);
                window.setFrameLeft(frameData);
                window.setFrameRight(frameData);
                window.setDividers(dividerData, numHorizDividers, numVertDividers);

                if (isSummer) {
                    vt = window.vt();
                    shgc = window.shgc();
                } else {
                    uvalue = window.uValue();
                }
            }
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //  CWCEHeatTransferFactory
    /////////////////////////////////////////////////////////////////////////////////////////

    CWCEHeatTransferFactory::CWCEHeatTransferFactory(EnergyPlusData &state, SurfaceData const &surface, int const t_SurfNum, int const t_ConstrNum)
        : m_Surface(surface), m_Window(state.dataSurface->SurfaceWindow(t_SurfNum)), m_ShadePosition(ShadePosition::NoShade), m_SurfNum(t_SurfNum),
          m_SolidLayerIndex(0), m_ConstructionNumber(t_ConstrNum), m_TotLay(getNumOfLayers(state)), m_InteriorBSDFShade(false), m_ExteriorShade(false)
    {
        if (!state.dataConstruction->Construct(m_ConstructionNumber).WindowTypeBSDF &&
            state.dataSurface->SurfWinShadingFlag.size() >= static_cast<size_t>(m_SurfNum)) {
            if (ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(m_SurfNum)) || ANY_BLIND(state.dataSurface->SurfWinShadingFlag(m_SurfNum))) {
                m_ConstructionNumber = state.dataSurface->SurfWinActiveShadedConstruction(m_SurfNum);
                m_TotLay = getNumOfLayers(state);
            }
        }
        const WinShadingType ShadeFlag = getShadeType(state, m_ConstructionNumber);

        if (ANY_INTERIOR_SHADE_BLIND(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Interior;
        }

        else if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Exterior;
        }

        else if (ANY_BETWEENGLASS_SHADE_BLIND(ShadeFlag)) {
            m_ShadePosition = ShadePosition::Between;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CSingleSystem> CWCEHeatTransferFactory::getTarcogSystem(EnergyPlusData &state, Real64 const t_HextConvCoeff)
    {
        auto Indoor = getIndoor(state);                    // (AUTO_OK_SHARED_PTR)
        auto Outdoor = getOutdoor(state, t_HextConvCoeff); // (AUTO_OK_SHARED_PTR)
        auto aIGU = getIGU();                              // (AUTO_OK_OBJ)

        // pick-up all layers and put them in IGU (this includes gap layers as well)
        for (int i = 0; i < m_TotLay; ++i) {
            auto aLayer = getIGULayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
            assert(aLayer != nullptr);
            // IDF for "standard" windows do not insert gas between glass and shade. Tarcog needs that gas
            // and it will be created here
            if (m_ShadePosition == ShadePosition::Interior && i == m_TotLay - 1) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
                aIGU.addLayer(aAirLayer);
            }
            aIGU.addLayer(aLayer);
            if (m_ShadePosition == ShadePosition::Exterior && i == 0) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
                aIGU.addLayer(aAirLayer);
            }
        }

        return std::make_shared<Tarcog::ISO15099::CSingleSystem>(aIGU, Indoor, Outdoor);
    }

    std::shared_ptr<Tarcog::ISO15099::IIGUSystem> CWCEHeatTransferFactory::getTarcogSystemForReporting(
        EnergyPlusData &state, bool const useSummerConditions, const Real64 width, const Real64 height, const Real64 tilt)
    {
        auto Indoor = getIndoorNfrc(useSummerConditions);   // (AUTO_OK_SHARED_PTR)
        auto Outdoor = getOutdoorNfrc(useSummerConditions); // (AUTO_OK_SHARED_PTR)
        auto aIGU = getIGU(width, height, tilt);            // (AUTO_OK_OBJ)

        m_SolidLayerIndex = 0;
        // pick-up all layers and put them in IGU (this includes gap layers as well)
        for (int i = 0; i < m_TotLay; ++i) {
            auto aLayer = getIGULayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
            assert(aLayer != nullptr);
            // IDF for "standard" windows do not insert gas between glass and shade. Tarcog needs that gas
            // and it will be created here
            if (m_ShadePosition == ShadePosition::Interior && i == m_TotLay - 1) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
                aIGU.addLayer(aAirLayer);
            }
            aIGU.addLayer(aLayer);
            if (m_ShadePosition == ShadePosition::Exterior && i == 0) {
                auto aAirLayer = getShadeToGlassLayer(state, i + 1); // (AUTO_OK_SHARED_PTR)
                aIGU.addLayer(aAirLayer);
            }
        }

        return std::make_shared<Tarcog::ISO15099::CSystem>(aIGU, Indoor, Outdoor);
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    Material::MaterialBase *CWCEHeatTransferFactory::getLayerMaterial(EnergyPlusData &state, int const t_Index) const
    {
        int ConstrNum = m_ConstructionNumber;

        // BSDF window do not have special shading flag
        if (!state.dataConstruction->Construct(ConstrNum).WindowTypeBSDF &&
            state.dataSurface->SurfWinShadingFlag.size() >= static_cast<size_t>(m_SurfNum)) {
            if (ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(m_SurfNum)) || ANY_BLIND(state.dataSurface->SurfWinShadingFlag(m_SurfNum))) {
                ConstrNum = state.dataSurface->SurfWinActiveShadedConstruction(m_SurfNum);
            }
        }

        auto &construction = state.dataConstruction->Construct(ConstrNum);
        const int LayPtr = construction.LayerPoint(t_Index);
        return state.dataMaterial->Material(LayPtr);
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> CWCEHeatTransferFactory::getIGULayer(EnergyPlusData &state, int const t_Index)
    {
        std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> aLayer = nullptr;

        auto *material = getLayerMaterial(state, t_Index);

        Material::Group matGroup = material->group;

        if ((matGroup == Material::Group::WindowGlass) || (matGroup == Material::Group::WindowSimpleGlazing) ||
            (matGroup == Material::Group::WindowBlind) || (matGroup == Material::Group::Shade) || (matGroup == Material::Group::Screen) ||
            (matGroup == Material::Group::ComplexWindowShade)) {
            ++m_SolidLayerIndex;
            aLayer = getSolidLayer(state, material, m_SolidLayerIndex);
        } else if (matGroup == Material::Group::WindowGas || matGroup == Material::Group::WindowGasMixture) {
            aLayer = getGapLayer(material);
        } else if (matGroup == Material::Group::ComplexWindowGap) {
            aLayer = getComplexGapLayer(state, material);
        }

        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    int CWCEHeatTransferFactory::getNumOfLayers(EnergyPlusData &state) const
    {
        return state.dataConstruction->Construct(m_ConstructionNumber).TotLayers;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer>
    CWCEHeatTransferFactory::getSolidLayer(EnergyPlusData &state, Material::MaterialBase const *materialBase, int const t_Index)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates solid layer object from material properties in EnergyPlus
        Real64 emissFront = 0.0;
        Real64 emissBack = 0.0;
        Real64 transThermalFront = 0.0;
        Real64 transThermalBack = 0.0;
        Real64 thickness = 0.0;
        Real64 conductivity = 0.0;
        Real64 createOpenness = false;
        Real64 Atop = 0.0;
        Real64 Abot = 0.0;
        Real64 Aleft = 0.0;
        Real64 Aright = 0.0;
        Real64 Afront = 0.0;

        auto const *material = dynamic_cast<const Material::MaterialChild *>(materialBase);
        assert(material != nullptr);
        if (material->group == Material::Group::WindowGlass || material->group == Material::Group::WindowSimpleGlazing) {
            emissFront = material->AbsorpThermalFront;
            emissBack = material->AbsorpThermalBack;
            transThermalFront = material->TransThermal;
            transThermalBack = material->TransThermal;
            thickness = material->Thickness;
            conductivity = material->Conductivity;
        }
        if (material->group == Material::Group::WindowBlind) {
            int blNum = state.dataSurface->SurfWinBlindNumber(m_SurfNum);
            auto const &blind = state.dataMaterial->Blind(blNum); // This was plain auto, did you mean to make a copy of blind?
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
        if (material->group == Material::Group::Shade) {
            emissFront = material->AbsorpThermal;
            emissBack = material->AbsorpThermal;
            transThermalFront = material->TransThermal;
            transThermalBack = material->TransThermal;
            thickness = material->Thickness;
            conductivity = material->Conductivity;
            Atop = material->WinShadeTopOpeningMult;
            Abot = material->WinShadeBottomOpeningMult;
            Aleft = material->WinShadeLeftOpeningMult;
            Aright = material->WinShadeRightOpeningMult;
            Afront = material->WinShadeAirFlowPermeability;
            if (t_Index == 1) {
                m_ExteriorShade = true;
            }
        }
        if (material->group == Material::Group::Screen) {
            // Simon: Existing code already takes into account geometry of Woven and scales down
            // emissivity for openning area.
            emissFront = material->AbsorpThermal;
            emissBack = material->AbsorpThermal;
            transThermalFront = material->TransThermal;
            transThermalBack = material->TransThermal;
            thickness = material->Thickness;
            conductivity = material->Conductivity;
            Atop = material->WinShadeTopOpeningMult;
            Abot = material->WinShadeBottomOpeningMult;
            Aleft = material->WinShadeLeftOpeningMult;
            Aright = material->WinShadeRightOpeningMult;
            Afront = material->WinShadeAirFlowPermeability;
            if (t_Index == 1) {
                m_ExteriorShade = true;
            }
        }

        if (material->group == Material::Group::ComplexWindowShade) {
            int shdPtr = material->ComplexShadePtr;
            auto const &shade = state.dataMaterial->ComplexShade(shdPtr);
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

        std::shared_ptr<Tarcog::ISO15099::ISurface> frontSurface = std::make_shared<Tarcog::ISO15099::CSurface>(emissFront, transThermalFront);
        std::shared_ptr<Tarcog::ISO15099::ISurface> backSurface = std::make_shared<Tarcog::ISO15099::CSurface>(emissBack, transThermalBack);
        auto aSolidLayer =
            std::make_shared<Tarcog::ISO15099::CIGUSolidLayer>(thickness, conductivity, frontSurface, backSurface); // (AUTO_OK_SHARED_PTR)
        if (createOpenness) {
            auto aOpenings = std::make_shared<Tarcog::ISO15099::CShadeOpenings>(Atop, Abot, Aleft, Aright, Afront, Afront); // (AUTO_OK_SHARED_PTR)
            aSolidLayer = std::make_shared<Tarcog::ISO15099::CIGUShadeLayer>(aSolidLayer, aOpenings);
        }
        static constexpr double standardizedRadiationIntensity = 783.0;
        if (state.dataWindowManager->inExtWindowModel->isExternalLibraryModel()) {
            auto &surface(state.dataSurface->Surface(m_SurfNum));
            const int ConstrNum = getActiveConstructionNumber(state, surface, m_SurfNum);
            std::shared_ptr<MultiLayerOptics::CMultiLayerScattered> aLayer =
                CWindowConstructionsSimplified::instance(state).getEquivalentLayer(state, FenestrationCommon::WavelengthRange::Solar, ConstrNum);

            // Report is done for normal incidence
            constexpr Real64 Theta = 0.0;
            constexpr Real64 Phi = 0.0;
            const Real64 absCoeff =
                aLayer->getAbsorptanceLayer(t_Index, FenestrationCommon::Side::Front, FenestrationCommon::ScatteringSimple::Diffuse, Theta, Phi);
            aSolidLayer->setSolarAbsorptance(absCoeff, standardizedRadiationIntensity);
        } else {
            const Real64 absCoeff{state.dataConstruction->Construct(state.dataSurface->Surface(m_SurfNum).Construction).AbsDiff(t_Index)};
            aSolidLayer->setSolarAbsorptance(absCoeff, standardizedRadiationIntensity);
        }
        return aSolidLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> CWCEHeatTransferFactory::getGapLayer(Material::MaterialBase const *material) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        Real64 constexpr pres = 1e5; // Old code uses this constant pressure
        Real64 thickness = material->Thickness;
        auto aGas = getGas(material); // (AUTO_OK_OBJ)
        std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> aLayer = std::make_shared<Tarcog::ISO15099::CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> CWCEHeatTransferFactory::getShadeToGlassLayer(EnergyPlusData &state, int const t_Index) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   August 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        Real64 constexpr pres = 1e5; // Old code uses this constant pressure
        auto aGas = getAir();        // (AUTO_OK_OBJ)
        Real64 thickness = 0.0;

        const WinShadingType ShadeFlag = getShadeType(state, m_ConstructionNumber);

        if (ShadeFlag == WinShadingType::IntBlind || ShadeFlag == WinShadingType::ExtBlind) {
            thickness = state.dataMaterial->Blind(state.dataSurface->SurfWinBlindNumber(m_SurfNum)).BlindToGlassDist;
        }
        if (ShadeFlag == WinShadingType::IntShade || ShadeFlag == WinShadingType::ExtShade || ShadeFlag == WinShadingType::ExtScreen) {
            const auto *material = dynamic_cast<Material::MaterialChild *>(getLayerMaterial(state, t_Index));
            assert(material != nullptr);
            thickness = material->WinShadeToGlassDist;
        }
        std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> aLayer = std::make_shared<Tarcog::ISO15099::CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CBaseIGULayer> CWCEHeatTransferFactory::getComplexGapLayer(EnergyPlusData &state,
                                                                                                 Material::MaterialBase const *materialBase) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        Real64 constexpr pres = 1e5; // Old code uses this constant pressure
        auto const *material = dynamic_cast<const Material::MaterialChild *>(materialBase);
        assert(material != nullptr);
        Real64 thickness = material->Thickness;
        Real64 gasPointer = material->GasPointer;
        auto *gasMaterial = state.dataMaterial->Material(gasPointer);
        auto aGas = getGas(gasMaterial); // (AUTO_OK_OBJ)
        return std::make_shared<Tarcog::ISO15099::CIGUGapLayer>(thickness, pres, aGas);
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    Gases::CGas CWCEHeatTransferFactory::getGas(Material::MaterialBase const *materialBase) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED
        //          April 2021: April 2021: Return of CGas instead of pointer to it

        // PURPOSE OF THIS SUBROUTINE:
        // Creates gap layer object from material properties in EnergyPlus
        auto const *material = dynamic_cast<const Material::MaterialChild *>(materialBase);
        assert(material != nullptr);
        const int numGases = material->NumberOfGasesInMixture;
        double constexpr vacuumCoeff = 1.4; // Load vacuum coefficient once it is implemented (Simon).
        std::string const &gasName = material->Name;
        Gases::CGas aGas;
        for (int i = 1; i <= numGases; ++i) {
            Real64 wght = material->GasWght(i);
            Real64 fract = material->GasFract(i);
            std::vector<double> gcon;
            std::vector<double> gvis;
            std::vector<double> gcp;
            for (int j = 1; j <= 3; ++j) {
                gcon.push_back(material->GasCon(j, i));
                gvis.push_back(material->GasVis(j, i));
                gcp.push_back(material->GasCp(j, i));
            }
            Gases::CIntCoeff aCon(gcon[0], gcon[1], gcon[2]);
            Gases::CIntCoeff aCp(gcp[0], gcp[1], gcp[2]);
            Gases::CIntCoeff aVis(gvis[0], gvis[1], gvis[2]);
            Gases::CGasData aData(gasName, wght, vacuumCoeff, aCp, aCon, aVis);
            aGas.addGasItem(fract, aData);
        }
        return aGas;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    Gases::CGas CWCEHeatTransferFactory::getAir()
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   August 2016
        //       MODIFIED       na
        //       RE-ENGINEERED
        //          April 2021: CGas return from the function instead of pointer to it

        // PURPOSE OF THIS SUBROUTINE:
        // Creates air gas layer for tarcog routines
        return {};
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> CWCEHeatTransferFactory::getIndoor(EnergyPlusData &state) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates indoor environment object from surface properties in EnergyPlus
        Real64 tin = m_Surface.getInsideAirTemperature(state, m_SurfNum) + DataGlobalConstants::KelvinConv;
        Real64 hcin = state.dataHeatBalSurf->SurfHConvInt(m_SurfNum);

        Real64 IR = state.dataSurface->SurfWinIRfromParentZone(m_SurfNum) + state.dataHeatBalSurf->SurfQdotRadHVACInPerArea(m_SurfNum);

        std::shared_ptr<Tarcog::ISO15099::CEnvironment> Indoor =
            std::make_shared<Tarcog::ISO15099::CIndoorEnvironment>(tin, state.dataEnvrn->OutBaroPress);
        Indoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH, hcin);
        Indoor->setEnvironmentIR(IR);
        return Indoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<Tarcog::ISO15099::CEnvironment> CWCEHeatTransferFactory::getOutdoor(EnergyPlusData &state, const Real64 t_Hext) const
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
        // double dirSolRad = SurfQRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
        double swRadiation = m_Surface.getSWIncident(state, m_SurfNum);
        double tSky = state.dataEnvrn->SkyTempKelvin;
        double airSpeed = 0.0;
        if (m_Surface.ExtWind) {
            airSpeed = state.dataSurface->SurfOutWindSpeed(m_SurfNum);
        }
        double fclr = 1 - state.dataEnvrn->CloudFraction;
        Tarcog::ISO15099::AirHorizontalDirection airDirection = Tarcog::ISO15099::AirHorizontalDirection::Windward;
        std::shared_ptr<Tarcog::ISO15099::CEnvironment> Outdoor = std::make_shared<Tarcog::ISO15099::COutdoorEnvironment>(
            tout, airSpeed, swRadiation, airDirection, tSky, Tarcog::ISO15099::SkyModel::AllSpecified, state.dataEnvrn->OutBaroPress, fclr);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::HcPrescribed, t_Hext);
        Outdoor->setEnvironmentIR(IR);
        return Outdoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    Tarcog::ISO15099::CIGU CWCEHeatTransferFactory::getIGU() const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED
        //          April 2021: Return CIGU object rather than pointer to it

        // PURPOSE OF THIS SUBROUTINE:
        // Creates IGU object from surface properties in EnergyPlus

        return {m_Surface.Width, m_Surface.Height, m_Surface.Tilt};
    }

    Tarcog::ISO15099::CIGU CWCEHeatTransferFactory::getIGU(double width, double height, double tilt)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   November 2021
        //       MODIFIED       na
        //       RE-ENGINEERED
        //          April 2021: Return CIGU object rather than pointer to it

        // PURPOSE OF THIS SUBROUTINE:
        // Creates IGU object for given width, height and tilt

        return {width, height, tilt};
    }

    int
    CWCEHeatTransferFactory::getActiveConstructionNumber(EnergyPlusData &state, EnergyPlus::DataSurfaces::SurfaceData const &surface, int t_SurfNum)
    {
        int result = surface.Construction;
        const WinShadingType ShadeFlag = state.dataSurface->SurfWinShadingFlag(t_SurfNum);

        if (ANY_SHADE_SCREEN(ShadeFlag) || ANY_BLIND(ShadeFlag)) {
            result = state.dataSurface->SurfWinActiveShadedConstruction(t_SurfNum);
        }

        return result;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    bool CWCEHeatTransferFactory::isInteriorShade() const
    {
        return m_InteriorBSDFShade;
    }

    std::shared_ptr<Tarcog::ISO15099::CEnvironment> CWCEHeatTransferFactory::getOutdoorNfrc(bool const useSummerConditions)
    {
        // NFRC 100 Section 4.3.1
        Real64 airTemperature = -18.0 + DataGlobalConstants::KelvinConv; // Kelvins
        Real64 airSpeed = 5.5;                                           // meters per second
        Real64 tSky = -18.0 + DataGlobalConstants::KelvinConv;           // Kelvins
        Real64 solarRadiation = 0.;                                      // W/m2
        if (useSummerConditions) {
            // NFRC 200 Section 4.3.1
            airTemperature = 32.0 + DataGlobalConstants::KelvinConv;
            airSpeed = 2.75;
            tSky = 32.0 + DataGlobalConstants::KelvinConv;
            solarRadiation = 783.;
        }
        auto Outdoor = // (AUTO_OK_SHARED_PTR)
            Tarcog::ISO15099::Environments::outdoor(airTemperature, airSpeed, solarRadiation, tSky, Tarcog::ISO15099::SkyModel::AllSpecified);
        Outdoor->setHCoeffModel(Tarcog::ISO15099::BoundaryConditionsCoeffModel::CalculateH);
        return Outdoor;
    }

    std::shared_ptr<Tarcog::ISO15099::CEnvironment> CWCEHeatTransferFactory::getIndoorNfrc(bool const useSummerConditions)
    {
        // NFRC 100 Section 4.3.1
        Real64 roomTemperature = 21. + DataGlobalConstants::KelvinConv;
        if (useSummerConditions) {
            // NFRC 200 Section 4.3.1
            roomTemperature = 24. + DataGlobalConstants::KelvinConv;
        }
        return Tarcog::ISO15099::Environments::indoor(roomTemperature);
    }

    WinShadingType CWCEHeatTransferFactory::getShadeType(EnergyPlusData &state, int ConstrNum)
    {
        WinShadingType ShadeFlag = WinShadingType::NoShade;

        const int TotLay = state.dataConstruction->Construct(ConstrNum).TotLayers;
        const int TotGlassLay = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
        const int MatOutside = state.dataConstruction->Construct(ConstrNum).LayerPoint(1);
        const int MatInside = state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLay);

        if (state.dataMaterial->Material(MatOutside)->group == Material::Group::Shade) { // Exterior shade present
            ShadeFlag = WinShadingType::ExtShade;
        } else if (state.dataMaterial->Material(MatOutside)->group == Material::Group::Screen) { // Exterior screen present
            const int MatShade = MatOutside;
            const int ScNum = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatShade))->ScreenDataPtr;
            // Orphaned constructs with exterior screen are ignored
            if (ScNum > 0) ShadeFlag = WinShadingType::ExtScreen;
        } else if (state.dataMaterial->Material(MatOutside)->group == Material::Group::WindowBlind) { // Exterior blind present
            ShadeFlag = WinShadingType::ExtBlind;
        } else if (state.dataMaterial->Material(MatInside)->group == Material::Group::Shade) { // Interior shade present
            ShadeFlag = WinShadingType::IntShade;
        } else if (state.dataMaterial->Material(MatInside)->group == Material::Group::WindowBlind) { // Interior blind present
            ShadeFlag = WinShadingType::IntBlind;
        } else if (TotGlassLay == 2) {
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3))->group == Material::Group::Shade)
                ShadeFlag = WinShadingType::BGShade;
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(3))->group == Material::Group::WindowBlind)
                ShadeFlag = WinShadingType::BGBlind;
        } else if (TotGlassLay == 3) {
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5))->group == Material::Group::Shade)
                ShadeFlag = WinShadingType::BGShade;
            if (state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(5))->group == Material::Group::WindowBlind)
                ShadeFlag = WinShadingType::BGBlind;
        }

        return ShadeFlag;
    }

    double CWCEHeatTransferFactory::overallUfactorFromFilmsAndCond(double conductance, double insideFilm, double outsideFilm)
    {
        double rOverall(0.);
        double uFactor(0.);
        if (insideFilm != 0 && outsideFilm != 0. && conductance != 0.) {
            rOverall = 1 / insideFilm + 1 / conductance + 1 / outsideFilm;
        }
        if (rOverall != 0.) {
            uFactor = 1 / rOverall;
        }
        return uFactor;
    }

} // namespace WindowManager
} // namespace EnergyPlus
