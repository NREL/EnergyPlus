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

// EnergyPlus headers
#include <DataEnvironment.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <General.hh>
#include <UtilityRoutines.hh>
#include <WindowManager.hh>

// Windows library headers
#include <WCETarcog.hpp>

#include "WindowManagerExteriorData.hh"
#include "WindowManagerExteriorThermal.hh"

namespace EnergyPlus {

using namespace Tarcog;
using namespace Gases;
using namespace FenestrationCommon;

using namespace DataEnvironment;
using namespace DataSurfaces;
using namespace DataHeatBalance;
using namespace DataHeatBalFanSys;
using namespace DataGlobals;
using namespace General;

namespace WindowManager {

    /////////////////////////////////////////////////////////////////////////////////////////
    void CalcWindowHeatBalanceExternalRoutines(int const SurfNum,          // Surface number
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

        auto &window(SurfaceWindow(SurfNum));
        auto &surface(Surface(SurfNum));
        auto ConstrNum = surface.Construction;
        auto &construction(Construct(ConstrNum));

        auto const solutionTolerance = 0.02;

        // Tarcog thermal system for solving heat transfer through the window
        auto aFactory = CWCEHeatTransferFactory(surface, SurfNum);
        auto aSystem = aFactory.getTarcogSystem(HextConvCoeff);
        aSystem->setTolerance(solutionTolerance);

        // get previous timestep temperatures solution for faster iterations
        std::vector<Real64> Guess;
        auto totSolidLayers = construction.TotSolidLayers;

        // Interior and exterior shading layers have gas between them and IGU but that gas
        // was not part of construction so it needs to be increased by one
        if (window.ShadingFlag == IntShadeOn || window.ShadingFlag == ExtShadeOn || window.ShadingFlag == IntBlindOn ||
            window.ShadingFlag == ExtBlindOn || window.ShadingFlag == ExtScreenOn || window.ShadingFlag == BGShadeOn ||
            window.ShadingFlag == BGBlindOn) {
            ++totSolidLayers;
        }

        for (auto k = 1; k <= 2 * totSolidLayers; ++k) {
            Guess.push_back(SurfaceWindow(SurfNum).ThetaFace(k));
        }

        try {
            aSystem->setInitialGuess(Guess);
            aSystem->solve();
        } catch(const std::exception& ex) {
            ShowSevereError("Error in Windows Calculation Engine Exterior Module.");
            ShowContinueError(ex.what());
        }

        auto aLayers = aSystem->getSolidLayers();
        auto i = 1;
        for (const auto & aLayer : aLayers) {
            Real64 aTemp = 0;
            for (auto aSide : EnumSide()) {
                aTemp = aLayer->getTemperature(aSide);
                thetas(i) = aTemp;
                if (i == 1) {
                    SurfOutsideTemp = aTemp - KelvinConv;
                }
                ++i;
            }
            SurfInsideTemp = aTemp - KelvinConv;
            if (window.ShadingFlag == IntShadeOn || window.ShadingFlag == IntBlindOn) {
                auto EffShBlEmiss = InterpSlatAng(window.SlatAngThisTS, window.MovableSlats, window.EffShBlindEmiss);
                auto EffGlEmiss = InterpSlatAng(window.SlatAngThisTS, window.MovableSlats, window.EffGlassEmiss);
                window.EffInsSurfTemp =
                    (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (thetas(2 * totSolidLayers - 2) - TKelvin)) / (EffShBlEmiss + EffGlEmiss);
            }
        }

        HConvIn(SurfNum) = aSystem->getHc(Environment::Indoor);
        if (window.ShadingFlag == IntShadeOn || window.ShadingFlag == IntBlindOn || aFactory.isInteriorShade()) {
            // It is not clear why EnergyPlus keeps this interior calculations separately for interior shade. This does create different
            // soltuion from heat transfer from tarcog itself. Need to confirm with LBNL team about this approach. Note that heat flow
            // through shade (consider case when openings are zero) is different from heat flow obtained by these equations. Will keep
            // these calculations just to confirm that current exterior engine is giving close results to what is in here. (Simon)
            auto totLayers = aLayers.size();
            nglface = 2 * totLayers - 2;
            nglfacep = nglface + 2;
            auto aShadeLayer = aLayers[totLayers - 1];
            auto aGlassLayer = aLayers[totLayers - 2];
            auto ShadeArea = Surface(SurfNum).Area + SurfaceWindow(SurfNum).DividerArea;
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
            auto rmir = surface.getInsideIR(SurfNum);
            auto NetIRHeatGainShade = ShadeArea * EpsShIR2 * (sigma * pow(thetas(nglfacep), 4) - rmir) +
                                      EpsShIR1 * (sigma * pow(thetas(nglfacep - 1), 4) - rmir) * RhoGlIR2 * TauShIR / ShGlReflFacIR;
            auto NetIRHeatGainGlass = ShadeArea * (glassEmiss * TauShIR / ShGlReflFacIR) * (sigma * pow(thetas(nglface), 4) - rmir);
            auto tind = surface.getInsideAirTemperature(SurfNum) + KelvinConv;
            auto ConvHeatGainFrZoneSideOfShade = ShadeArea * HConvIn(SurfNum) * (thetas(nglfacep) - tind);
            WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfShade + NetIRHeatGainGlass + NetIRHeatGainShade;
            WinHeatTransfer(SurfNum) = WinHeatGain(SurfNum);

            WinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;

            // Effective shade and glass emissivities that are used later for energy calculations.
            // This needs to be checked as well. (Simon)
            auto EffShBlEmiss = EpsShIR1 * (1.0 + RhoGlIR2 * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2));
            SurfaceWindow(SurfNum).EffShBlindEmiss = EffShBlEmiss;

            auto EffGlEmiss = glassEmiss * TauShIR / (1.0 - RhoGlIR2 * RhoShIR2);
            SurfaceWindow(SurfNum).EffGlassEmiss = EffGlEmiss;

            auto glassTemperature = aGlassLayer->getSurface(Side::Back)->getTemperature();
            SurfaceWindow(SurfNum).EffInsSurfTemp =
                (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (glassTemperature - KelvinConv)) / (EffShBlEmiss + EffGlEmiss);

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

            auto rmir = surface.getInsideIR(SurfNum);
            auto NetIRHeatGainGlass = surface.Area * backSurface->getEmissivity() * (sigma * pow(backSurface->getTemperature(), 4) - rmir);

            SurfaceWindow(SurfNum).EffInsSurfTemp = aLayers[totLayers - 1]->getTemperature(Side::Back) - TKelvin;
            SurfaceWindow(SurfNum).EffGlassEmiss = aLayers[totLayers - 1]->getSurface(Side::Back)->getEmissivity();

            WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass;
            WinGainConvGlazToZoneRep(SurfNum) = ConvHeatGainFrZoneSideOfGlass;
            WinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass;

            WinHeatTransfer(SurfNum) = WinHeatGain(SurfNum);
        }

        auto TransDiff = construction.TransDiff;
        WinHeatGain(SurfNum) -= QS(surface.Zone) * surface.Area * TransDiff;
        WinHeatTransfer(SurfNum) -= QS(surface.Zone) * surface.Area * TransDiff;
        WinLossSWZoneToOutWinRep(SurfNum) = QS(Surface(SurfNum).Zone) * surface.Area * TransDiff;

        for (auto k = 1; k <= surface.getTotLayers(); ++k) {
            SurfaceWindow(SurfNum).ThetaFace(2 * k - 1) = thetas(2 * k - 1);
            SurfaceWindow(SurfNum).ThetaFace(2 * k) = thetas(2 * k);

            // temperatures for reporting
            FenLaySurfTempFront(k, SurfNum) = thetas(2 * k - 1) - KelvinConv;
            FenLaySurfTempBack(k, SurfNum) = thetas(2 * k) - KelvinConv;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    //  CWCEHeatTransferFactory
    /////////////////////////////////////////////////////////////////////////////////////////

    CWCEHeatTransferFactory::CWCEHeatTransferFactory(SurfaceData const &surface, int const t_SurfNum)
        : m_Surface(surface), m_SurfNum(t_SurfNum), m_SolidLayerIndex(0), m_InteriorBSDFShade(false), m_ExteriorShade(false)
    {
        m_Window = SurfaceWindow(t_SurfNum);
        auto ShadeFlag = m_Window.ShadingFlag;

        m_ConstructionNumber = m_Surface.Construction;
        m_ShadePosition = ShadePosition::NoShade;

        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGShadeOn ||
            ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn) {
            m_ConstructionNumber = m_Surface.ShadedConstruction;
            if (m_Window.StormWinFlag > 0) m_ConstructionNumber = m_Surface.StormWinShadedConstruction;
        }

        m_TotLay = getNumOfLayers();

        if (ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn) {
            m_ShadePosition = ShadePosition::Interior;
        }

        if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn) {
            m_ShadePosition = ShadePosition::Exterior;
        }

        if (ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn) {
            m_ShadePosition = ShadePosition::Between;
        }
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CSingleSystem> CWCEHeatTransferFactory::getTarcogSystem(Real64 const t_HextConvCoeff)
    {
        auto Indoor = getIndoor();
        auto Outdoor = getOutdoor(t_HextConvCoeff);
        auto aIGU = getIGU();

        // pick-up all layers and put them in IGU (this includes gap layers as well)
        for (auto i = 0; i < m_TotLay; ++i) {
            auto aLayer = getIGULayer(i + 1);
            assert(aLayer != nullptr);
            // IDF for "standard" windows do not insert gas between glass and shade. Tarcog needs that gas
            // and it will be created here
            if (m_ShadePosition == ShadePosition::Interior && i == m_TotLay - 1) {
                auto aAirLayer = getShadeToGlassLayer(i + 1);
                aIGU->addLayer(aAirLayer);
            }
            aIGU->addLayer(aLayer);
            if (m_ShadePosition == ShadePosition::Exterior && i == 0) {
                auto aAirLayer = getShadeToGlassLayer(i + 1);
                aIGU->addLayer(aAirLayer);
            }
        }

        auto aSystem = std::make_shared<CSingleSystem>(aIGU, Indoor, Outdoor);

        return aSystem;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    MaterialProperties *CWCEHeatTransferFactory::getLayerMaterial(int const t_Index) const
    {
        auto ConstrNum = m_Surface.Construction;

        if (m_Window.ShadingFlag == IntShadeOn || m_Window.ShadingFlag == ExtShadeOn || m_Window.ShadingFlag == IntBlindOn ||
            m_Window.ShadingFlag == ExtBlindOn || m_Window.ShadingFlag == BGShadeOn || m_Window.ShadingFlag == BGBlindOn ||
            m_Window.ShadingFlag == ExtScreenOn) {
            ConstrNum = m_Surface.ShadedConstruction;
            if (m_Window.StormWinFlag > 0) ConstrNum = m_Surface.StormWinShadedConstruction;
        }

        auto &construction(Construct(ConstrNum));
        auto LayPtr = construction.LayerPoint(t_Index);
        return &Material(LayPtr);
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getIGULayer(int const t_Index)
    {
        std::shared_ptr<CBaseIGULayer> aLayer = nullptr;

        auto material = getLayerMaterial(t_Index);

        auto matGroup = material->Group;

        if (matGroup == WindowGlass || matGroup == WindowSimpleGlazing || matGroup == WindowBlind || matGroup == Shade || matGroup == Screen ||
            matGroup == ComplexWindowShade) {
            ++m_SolidLayerIndex;
            aLayer = getSolidLayer(m_Surface, *material, m_SolidLayerIndex, m_SurfNum);
        } else if (matGroup == WindowGas || matGroup == WindowGasMixture) {
            aLayer = getGapLayer(*material);
        } else if (matGroup == ComplexWindowGap) {
            aLayer = getComplexGapLayer(*material);
        }

        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    int CWCEHeatTransferFactory::getNumOfLayers() const
    {
        return Construct(m_ConstructionNumber).TotLayers;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer>
    CWCEHeatTransferFactory::getSolidLayer(SurfaceData const &surface, MaterialProperties const &material, int const t_Index, int const t_SurfNum)
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
            auto blNum = m_Window.BlindNumber;
            auto blind = Blind(blNum);
            thickness = blind.SlatThickness;
            conductivity = blind.SlatConductivity;
            Atop = blind.BlindTopOpeningMult;
            Abot = blind.BlindBottomOpeningMult;
            Aleft = blind.BlindLeftOpeningMult;
            Aright = blind.BlindRightOpeningMult;
            Afront = m_Window.BlindAirFlowPermeability;
            emissFront = InterpSlatAng(m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRFrontEmiss);
            emissBack = InterpSlatAng(m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRBackEmiss);
            transThermalFront = InterpSlatAng(m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRFrontTrans);
            transThermalBack = InterpSlatAng(m_Window.SlatAngThisTS, m_Window.MovableSlats, blind.IRBackTrans);
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
            auto &shade(ComplexShade(shdPtr));
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
        auto swRadiation = surface.getSWIncident(t_SurfNum);
        if (swRadiation > 0) {

            auto absCoeff = QRadSWwinAbs(t_Index, t_SurfNum) / swRadiation;
            if ((2 * t_Index - 1) == m_TotLay) {
                absCoeff += QRadThermInAbs(t_SurfNum) / swRadiation;
            }

            aSolidLayer->setSolarAbsorptance(absCoeff);
        }
        return aSolidLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getGapLayer(MaterialProperties const &material) const
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
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getShadeToGlassLayer(int const t_Index) const
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

        if (m_Window.ShadingFlag == IntBlindOn || m_Window.ShadingFlag == ExtBlindOn) {
            thickness = Blind(m_Window.BlindNumber).BlindToGlassDist;
        }
        if (m_Window.ShadingFlag == IntShadeOn || m_Window.ShadingFlag == ExtShadeOn || m_Window.ShadingFlag == ExtScreenOn) {
            auto material = getLayerMaterial(t_Index);
            thickness = material->WinShadeToGlassDist;
        }
        std::shared_ptr<CBaseIGULayer> aLayer = std::make_shared<CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CBaseIGULayer> CWCEHeatTransferFactory::getComplexGapLayer(MaterialProperties const &material) const
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
        auto &gasMaterial(Material(gasPointer));
        auto aGas = getGas(gasMaterial);
        std::shared_ptr<CBaseIGULayer> aLayer = std::make_shared<CIGUGapLayer>(thickness, pres, aGas);
        return aLayer;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CGas> CWCEHeatTransferFactory::getGas(MaterialProperties const &material) const
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
    std::shared_ptr<CEnvironment> CWCEHeatTransferFactory::getIndoor() const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates indoor environment object from surface properties in EnergyPlus
        auto tin = m_Surface.getInsideAirTemperature(m_SurfNum) + KelvinConv;
        auto hcin = HConvIn(m_SurfNum);

        auto IR = m_Surface.getInsideIR(m_SurfNum);

        std::shared_ptr<CEnvironment> Indoor = std::make_shared<CIndoorEnvironment>(tin, OutBaroPress);
        Indoor->setHCoeffModel(BoundaryConditionsCoeffModel::CalculateH, hcin);
        Indoor->setEnvironmentIR(IR);
        return Indoor;
    }

    /////////////////////////////////////////////////////////////////////////////////////////
    std::shared_ptr<CEnvironment> CWCEHeatTransferFactory::getOutdoor(const Real64 t_Hext) const
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   July 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates outdoor environment object from surface properties in EnergyPlus
        double tout = m_Surface.getOutsideAirTemperature(m_SurfNum) + KelvinConv;
        double IR = m_Surface.getOutsideIR(m_SurfNum);
        // double dirSolRad = QRadSWOutIncident( t_SurfNum ) + QS( Surface( t_SurfNum ).Zone );
        double swRadiation = m_Surface.getSWIncident(m_SurfNum);
        double tSky = SkyTempKelvin;
        double airSpeed = 0.0;
        if (m_Surface.ExtWind) {
            airSpeed = m_Surface.WindSpeed;
        }
        double fclr = 1 - CloudFraction;
        AirHorizontalDirection airDirection = AirHorizontalDirection::Windward;
        std::shared_ptr<CEnvironment> Outdoor =
            std::make_shared<COutdoorEnvironment>(tout, OutBaroPress, airSpeed, swRadiation, airDirection, tSky, SkyModel::AllSpecified, fclr);
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
