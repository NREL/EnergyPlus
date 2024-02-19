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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/ConvectionConstants.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/WindowManager.hh>

namespace EnergyPlus {

namespace SurfaceGeometry {

    // Module containing the routines dealing with the Surface Geometry

    // MODULE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   June 2000
    //       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module performs the functions required of the surface geometry.

    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using DataWindowEquivalentLayer::CFSMAXNL;

    static std::string const BlankString;

    int constexpr UnenteredAdjacentZoneSurface = -998; // allows users to enter one zone surface ("Zone")
                                                       // referencing another in adjacent zone
    int constexpr UnreconciledZoneSurface = -999;      // interim value between entering surfaces ("Surface") and reconciling

    void AllocateSurfaceWindows(EnergyPlusData &state, int NumSurfaces)
    {
        state.dataSurface->SurfWinA.dimension(state.dataSurface->TotSurfaces, CFSMAXNL + 1, 0.0);
        state.dataSurface->SurfWinADiffFront.dimension(state.dataSurface->TotSurfaces, CFSMAXNL + 1, 0.0);
        state.dataSurface->SurfWinACFOverlap.dimension(state.dataSurface->TotSurfaces, state.dataHeatBal->MaxSolidWinLayers, 0.0);

        state.dataSurface->SurfWinFrameQRadOutAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameQRadInAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerQRadOutAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerQRadInAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinExtBeamAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinExtDiffAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinIntBeamAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinIntSWAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInitialDifSolAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinIntLWAbsByShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinConvHeatFlowNatural.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinConvHeatGainToZoneAir.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinRetHeatGainToZoneAir.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerHeatGain.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlTsolBmBm.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlTsolBmDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlTsolDifDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlGlSysTsolBmBm.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlGlSysTsolDifDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinScTsolBmBm.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinScTsolBmDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinScTsolDifDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinScGlSysTsolBmBm.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinScGlSysTsolDifDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinGlTsolBmBm.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinGlTsolBmDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinGlTsolDifDif.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolTransThruIntWinRep.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolAbsdOutsReveal.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolRefldOutsRevealReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolAbsdInsReveal.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolRefldInsReveal.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolRefldInsRevealReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinOutsRevealDiffOntoGlazing.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffOntoGlazing.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffIntoZone.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinOutsRevealDiffOntoFrame.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffOntoFrame.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffOntoGlazingReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffIntoZoneReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsRevealDiffOntoFrameReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolAbsdInsRevealReport.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolTransThruIntWinRepEnergy.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolRefldOutsRevealRepEnergy.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmSolRefldInsRevealRepEnergy.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProfileAngHor.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProfileAngVert.dimension(NumSurfaces, 0);

        state.dataSurface->SurfWinShadingFlag.dimension(NumSurfaces, WinShadingType::ShadeOff);
        state.dataSurface->SurfWinShadingFlagEMSOn.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinShadingFlagEMSValue.dimension(NumSurfaces, 0.0);
        state.dataSurface->SurfWinStormWinFlag.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinStormWinFlagPrevDay.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFracTimeShadingDeviceOn.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinExtIntShadePrevTS.dimension(NumSurfaces, WinShadingType::ShadeOff);
        state.dataSurface->SurfWinHasShadeOrBlindLayer.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSurfDayLightInit.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDaylFacPoint.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinVisTransSelected.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSwitchingFactor.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinVisTransRatio.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinIRfromParentZone.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameArea.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameConductance.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameSolAbsorp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameVisAbsorp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameEmis.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrEdgeToCenterGlCondRatio.dimension(NumSurfaces, 1.0);
        state.dataSurface->SurfWinFrameEdgeArea.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameTempIn.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinFrameTempInOld.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinFrameTempSurfOut.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinProjCorrFrOut.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProjCorrFrIn.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerType.dimension(NumSurfaces, DataSurfaces::FrameDividerType::DividedLite);
        state.dataSurface->SurfWinDividerArea.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerConductance.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerSolAbsorp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerVisAbsorp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerEmis.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDivEdgeToCenterGlCondRatio.dimension(NumSurfaces, 1);
        state.dataSurface->SurfWinDividerEdgeArea.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerTempIn.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinDividerTempInOld.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinDividerTempSurfOut.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinProjCorrDivOut.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProjCorrDivIn.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinShadeAbsFacFace1.dimension(NumSurfaces, 0.5);
        state.dataSurface->SurfWinShadeAbsFacFace2.dimension(NumSurfaces, 0.5);
        state.dataSurface->SurfWinConvCoeffWithShade.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinOtherConvHeatGain.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlindNumber.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinEffInsSurfTemp.dimension(NumSurfaces, 23.0);
        state.dataSurface->SurfWinMovableSlats.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatAngThisTS.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatAngThisTSDeg.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatAngThisTSDegEMSon.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatAngThisTSDegEMSValue.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatsBlockBeam.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatsAngIndex.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSlatsAngInterpFac.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProfileAng.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProfAngIndex.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinProfAngInterpFac.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlindBmBmTrans.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBlindAirFlowPermeability.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinTotGlazingThickness.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinTanProfileAngHor.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinTanProfileAngVert.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsideSillDepth.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsideReveal.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsideSillSolAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsideRevealSolAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinOutsideRevealSolAbs.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinAirflowSource.dimension(NumSurfaces, DataSurfaces::WindowAirFlowSource::Invalid);
        state.dataSurface->SurfWinAirflowDestination.dimension(NumSurfaces, DataSurfaces::WindowAirFlowDestination::Invalid);
        state.dataSurface->SurfWinAirflowReturnNodePtr.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinMaxAirflow.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinAirflowControlType.dimension(NumSurfaces, DataSurfaces::WindowAirFlowControlType::Invalid);
        state.dataSurface->SurfWinAirflowHasSchedule.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinAirflowSchedulePtr.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinAirflowThisTS.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinTAirflowGapOutlet.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinWindowCalcIterationsRep.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinVentingOpenFactorMultRep.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinInsideTempForVentingRep.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinVentingAvailabilityRep.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSkyGndSolarInc.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinBmGndSolarInc.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSolarDiffusing.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameHeatGain.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinFrameHeatLoss.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinDividerHeatLoss.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinTCLayerTemp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinSpecTemp.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinWindowModelType.dimension(NumSurfaces, WindowModel::Detailed);
        state.dataSurface->SurfWinTDDPipeNum.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinStormWinConstr.dimension(NumSurfaces, 0);
        state.dataSurface->SurfActiveConstruction.dimension(NumSurfaces, 0);
        state.dataSurface->SurfWinActiveShadedConstruction.dimension(NumSurfaces, 0);
    }

    void SetupZoneGeometry(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   September 1977
        //       MODIFIED       April 2002 (FCW): add warning for Solar Distribution
        //                      = FullInteriorExterior when window has reveal
        //                      Add fatal error when triangular window has reveal
        //                      May 2002(FCW): Allow triangular windows to have reveal (subr SHDRVL
        //                      in SolarShading). Remove above warning and fatal error.
        //       RE-ENGINEERED  November 1997 (RKS,LKL)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine controls the processing of detached shadowing and
        // zone surfaces for computing their vertices.

        static constexpr std::string_view RoutineName("SetUpZoneGeometry: ");

        // Zones must have been "gotten" before this call
        // The RelNorth variables are used if "relative" coordinates are input as well
        // as setting up DaylightingCoords

        // these include building north axis and Building Rotation for Appendix G
        state.dataSurfaceGeometry->CosBldgRelNorth =
            std::cos(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * Constant::DegToRadians);
        state.dataSurfaceGeometry->SinBldgRelNorth =
            std::sin(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * Constant::DegToRadians);

        // these are only for Building Rotation for Appendix G when using world coordinate system
        state.dataSurfaceGeometry->CosBldgRotAppGonly = std::cos(-state.dataHeatBal->BuildingRotationAppendixG * Constant::DegToRadians);
        state.dataSurfaceGeometry->SinBldgRotAppGonly = std::sin(-state.dataHeatBal->BuildingRotationAppendixG * Constant::DegToRadians);

        state.dataSurfaceGeometry->CosZoneRelNorth.allocate(state.dataGlobal->NumOfZones);
        state.dataSurfaceGeometry->SinZoneRelNorth.allocate(state.dataGlobal->NumOfZones);

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            state.dataSurfaceGeometry->CosZoneRelNorth(ZoneNum) = std::cos(-state.dataHeatBal->Zone(ZoneNum).RelNorth * Constant::DegToRadians);
            state.dataSurfaceGeometry->SinZoneRelNorth(ZoneNum) = std::sin(-state.dataHeatBal->Zone(ZoneNum).RelNorth * Constant::DegToRadians);
        }
        GetSurfaceData(state, ErrorsFound);

        if (ErrorsFound) {
            state.dataSurfaceGeometry->CosZoneRelNorth.deallocate();
            state.dataSurfaceGeometry->SinZoneRelNorth.deallocate();
            return;
        }

        GetWindowGapAirflowControlData(state, ErrorsFound);

        GetStormWindowData(state, ErrorsFound);

        if (!ErrorsFound && state.dataSurface->TotStormWin > 0) CreateStormWindowConstructions(state);

        SetFlagForWindowConstructionWithShadeOrBlindLayer(state);

        state.dataSurfaceGeometry->CosZoneRelNorth.deallocate();
        state.dataSurfaceGeometry->SinZoneRelNorth.deallocate();

        state.dataHeatBal->CalcWindowRevealReflection = false; // Set to True in ProcessSurfaceVertices if beam solar reflection from window reveals
        // is requested for one or more exterior windows.
        state.dataSurface->BuildingShadingCount = 0;
        state.dataSurface->FixedShadingCount = 0;
        state.dataSurface->AttachedShadingCount = 0;
        state.dataSurface->ShadingSurfaceFirst = 0;
        state.dataSurface->ShadingSurfaceLast = -1;

        // Reserve space to avoid excess allocations
        state.dataSurface->AllExtSolAndShadingSurfaceList.reserve(state.dataSurface->TotSurfaces);

        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) { // Loop through all surfaces...
            auto &thisSurface = state.dataSurface->Surface(SurfNum);

            state.dataSurface->SurfAirSkyRadSplit(SurfNum) = std::sqrt(0.5 * (1.0 + thisSurface.CosTilt));

            // Set flag that determines whether a surface is a shadowing surface
            thisSurface.IsShadowing = false;
            if (thisSurface.Class == SurfaceClass::Shading || thisSurface.Class == SurfaceClass::Detached_F ||
                thisSurface.Class == SurfaceClass::Detached_B) {
                thisSurface.IsShadowing = true;
                if (state.dataSurface->ShadingSurfaceFirst == 0) state.dataSurface->ShadingSurfaceFirst = SurfNum;
                state.dataSurface->ShadingSurfaceLast = SurfNum;
            }
            if ((thisSurface.HeatTransSurf && thisSurface.ExtSolar) || thisSurface.IsShadowing) {
                // Some attached shading surfaces may be true for both
                state.dataSurface->AllExtSolAndShadingSurfaceList.push_back(SurfNum);
            }
            if (thisSurface.Class == SurfaceClass::Shading) ++state.dataSurface->AttachedShadingCount;
            if (thisSurface.Class == SurfaceClass::Detached_F) ++state.dataSurface->FixedShadingCount;
            if (thisSurface.Class == SurfaceClass::Detached_B) ++state.dataSurface->BuildingShadingCount;

            if (thisSurface.Class != SurfaceClass::IntMass) ProcessSurfaceVertices(state, SurfNum, ErrorsFound);
        }

        for (auto &e : state.dataHeatBal->Zone) {
            e.ExtWindowArea = 0.0;
            e.HasWindow = false;
            e.ExtGrossWallArea = 0.0;
            e.ExtNetWallArea = 0.0;
            e.TotalSurfArea = 0.0;
        }

        for (auto &s : state.dataHeatBal->space) {
            s.extWindowArea = 0.0;
            s.totalSurfArea = 0.0;
        }
        bool DetailedWWR = (state.dataInputProcessing->inputProcessor->getNumSectionsFound("DETAILEDWWR_DEBUG") > 0);
        if (DetailedWWR) {
            print(state.files.debug, "{}", "=======User Entered Classification =================");
            print(state.files.debug, "{}", "Surface,Class,Area,Tilt");
        }

        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) { // Loop through all surfaces to find windows...
            auto &thisSurface = state.dataSurface->Surface(SurfNum);

            if (!thisSurface.HeatTransSurf && !thisSurface.IsAirBoundarySurf) continue; // Skip shadowing (sub)surfaces
            auto &thisZone = state.dataHeatBal->Zone(thisSurface.Zone);
            auto &thisSpace = state.dataHeatBal->space(thisSurface.spaceNum);

            thisZone.TotalSurfArea += thisSurface.Area;
            thisSpace.totalSurfArea += thisSurface.Area;
            if (thisSurface.Class == SurfaceClass::Roof) {
                thisZone.geometricCeilingArea += thisSurface.GrossArea;
            } else if (thisSurface.Class == SurfaceClass::Floor) {
                thisZone.geometricFloorArea += thisSurface.GrossArea;
            }
            if (state.dataConstruction->Construct(thisSurface.Construction).TypeIsWindow) {
                thisZone.TotalSurfArea += state.dataSurface->SurfWinFrameArea(SurfNum);
                thisZone.HasWindow = true;
                thisSpace.totalSurfArea += state.dataSurface->SurfWinFrameArea(SurfNum);
                if (((thisSurface.ExtBoundCond == ExternalEnvironment) || (thisSurface.ExtBoundCond == OtherSideCondModeledExt)) &&
                    (thisSurface.Class != SurfaceClass::TDD_Dome)) {
                    thisZone.ExtWindowArea += thisSurface.GrossArea;
                    thisSpace.extWindowArea += thisSurface.GrossArea;
                    thisZone.ExtWindowArea_Multiplied =
                        thisZone.ExtWindowArea + thisSurface.GrossArea * thisSurface.Multiplier * thisZone.Multiplier * thisZone.ListMultiplier;
                    if (DetailedWWR) {
                        print(state.files.debug,
                              "{},Window,{:.2R},{:.1R}\n",
                              thisSurface.Name,
                              thisSurface.GrossArea * thisSurface.Multiplier * thisZone.Multiplier * thisZone.ListMultiplier,
                              thisSurface.Tilt);
                    }
                }
            } else {
                if (thisSurface.ExtBoundCond == ExternalEnvironment || thisSurface.ExtBoundCond == OtherSideCondModeledExt) {
                    thisZone.ExteriorTotalSurfArea += thisSurface.GrossArea;
                    thisSpace.ExteriorTotalSurfArea += thisSurface.GrossArea;
                    if (thisSurface.Class == SurfaceClass::Wall) {
                        thisZone.ExtNetWallArea += thisSurface.Area;
                        thisZone.ExtGrossWallArea += thisSurface.GrossArea;
                        thisSpace.ExtGrossWallArea += thisSurface.GrossArea;
                        thisZone.ExtGrossWallArea_Multiplied += thisSurface.GrossArea * thisZone.Multiplier * thisZone.ListMultiplier;
                        if (DetailedWWR) {
                            print(state.files.debug,
                                  "{},Wall,{:.2R},{:.1R}\n",
                                  thisSurface.Name,
                                  thisSurface.GrossArea * thisZone.Multiplier * thisZone.ListMultiplier,
                                  thisSurface.Tilt);
                        }
                    }
                } else if (thisSurface.ExtBoundCond == Ground || thisSurface.ExtBoundCond == GroundFCfactorMethod ||
                           thisSurface.ExtBoundCond == KivaFoundation) {
                    thisZone.ExteriorTotalGroundSurfArea += thisSurface.GrossArea;
                    if (thisSurface.Class == SurfaceClass::Wall) {
                        thisZone.ExtGrossGroundWallArea += thisSurface.GrossArea;
                        thisZone.ExtGrossGroundWallArea_Multiplied += thisSurface.GrossArea * thisZone.Multiplier * thisZone.ListMultiplier;
                        if (DetailedWWR) {
                            print(state.files.debug,
                                  "{},Wall-GroundContact,{:.2R},{:.1R}\n",
                                  thisSurface.Name,
                                  thisSurface.GrossArea * thisZone.Multiplier * thisZone.ListMultiplier,
                                  thisSurface.Tilt);
                        }
                    }
                }
            }

        } // ...end of surfaces windows DO loop

        if (DetailedWWR) {
            print(state.files.debug, "{}\n", "========================");
            print(state.files.debug, "{}\n", "Zone,ExtWallArea,ExtWindowArea");
        }

        for (auto &thisZone : state.dataHeatBal->Zone) {
            int CeilCount = 0;
            int FloorCount = 0;
            int WallCount = 0;
            Real64 AverageHeight = 0.0; // Used to keep track of average height of a surface/zone
            Real64 ZMax = -99999.0;     // Maximum Z of a surface (detailed outside coefficient calculation)
            Real64 ZMin = 99999.0;      // Minimum Z of a surface (detailed outside coefficient calculation)
            Real64 ZCeilAvg = 0.0;
            Real64 ZFlrAvg = 0.0;
            if (DetailedWWR) {
                print(state.files.debug, "{},{:.2R},{:.2R}\n", thisZone.Name, thisZone.ExtGrossWallArea, thisZone.ExtWindowArea);
            }
            for (int spaceNum : thisZone.spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                // Use AllSurfaceFirst which includes air boundaries
                for (int SurfNum = thisSpace.AllSurfaceFirst; SurfNum <= thisSpace.AllSurfaceLast; ++SurfNum) {
                    auto &thisSurface = state.dataSurface->Surface(SurfNum);

                    if (thisSurface.Class == SurfaceClass::Roof) {
                        // Use Average Z for surface, more important for roofs than floors...
                        ++CeilCount;
                        Real64 Z1 = minval(thisSurface.Vertex, &Vector::z);
                        Real64 Z2 = maxval(thisSurface.Vertex, &Vector::z);
                        //        ZCeilAvg=ZCeilAvg+(Z1+Z2)/2.d0
                        ZCeilAvg += ((Z1 + Z2) / 2.0) * (thisSurface.GrossArea / thisZone.geometricCeilingArea);
                    }
                    if (thisSurface.Class == SurfaceClass::Floor) {
                        // Use Average Z for surface, more important for roofs than floors...
                        ++FloorCount;
                        Real64 Z1 = minval(thisSurface.Vertex, &Vector::z);
                        Real64 Z2 = maxval(thisSurface.Vertex, &Vector::z);
                        //        ZFlrAvg=ZFlrAvg+(Z1+Z2)/2.d0
                        ZFlrAvg += ((Z1 + Z2) / 2.0) * (thisSurface.GrossArea / thisZone.geometricFloorArea);
                    }
                    if (thisSurface.Class == SurfaceClass::Wall) {
                        // Use Wall calculation in case no roof & floor in zone
                        ++WallCount;
                        if (WallCount == 1) {
                            ZMax = thisSurface.Vertex(1).z;
                            ZMin = ZMax;
                        }
                        ZMax = max(ZMax, maxval(thisSurface.Vertex, &Vector::z));
                        ZMin = min(ZMin, minval(thisSurface.Vertex, &Vector::z));
                    }
                }
            }
            if (CeilCount > 0 && FloorCount > 0) {
                AverageHeight = ZCeilAvg - ZFlrAvg;
            } else {
                AverageHeight = (ZMax - ZMin);
            }
            if (AverageHeight <= 0.0) {
                AverageHeight = (ZMax - ZMin);
            }

            if (thisZone.CeilingHeight > 0.0) {
                thisZone.ceilingHeightEntered = true;
                if (AverageHeight > 0.0) {
                    if (std::abs(AverageHeight - thisZone.CeilingHeight) / thisZone.CeilingHeight > 0.05) {
                        if (state.dataSurfaceGeometry->ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(
                                state,
                                format("{}Entered Ceiling Height for some zone(s) significantly different from calculated Ceiling Height",
                                       RoutineName));
                            ShowContinueError(state,
                                              "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             format("{}Entered Ceiling Height for Zone=\"{}\" significantly different from calculated Ceiling Height",
                                                    RoutineName,
                                                    thisZone.Name));
                            ShowContinueError(
                                state,
                                format("{}Entered Ceiling Height={:.2F}, Calculated Ceiling Height={:.2F}, entered height will be used in calculations.",
                                       RoutineName,
                                       thisZone.CeilingHeight,
                                       AverageHeight));
                        }
                    }
                }
            }
            if ((thisZone.CeilingHeight <= 0.0) && (AverageHeight > 0.0)) thisZone.CeilingHeight = AverageHeight;
            // Need to add check here - don't touch if already user-specified
        }

        CalculateZoneVolume(state); // Calculate Zone Volumes

        // Calculate zone centroid (and min/max x,y,z for zone)
        // Use AllSurfaceFirst which includes air boundaries
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            auto &thisZone = state.dataHeatBal->Zone(ZoneNum);
            bool nonInternalMassSurfacesPresent = false;
            bool internalMassSurfacesPresent = false;
            Real64 TotSurfArea = 0.0;
            thisZone.Centroid = Vector(0.0, 0.0, 0.0);
            if ((thisZone.AllSurfaceFirst > 0) && (state.dataSurface->Surface(thisZone.AllSurfaceFirst).Sides > 0)) {
                thisZone.MinimumX = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).x;
                thisZone.MaximumX = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).x;
                thisZone.MinimumY = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).y;
                thisZone.MaximumY = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).y;
                thisZone.MinimumZ = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).z;
                thisZone.MaximumZ = state.dataSurface->Surface(thisZone.AllSurfaceFirst).Vertex(1).z;
            }
            for (int spaceNum : thisZone.spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);

                for (int SurfNum = thisSpace.AllSurfaceFirst; SurfNum <= thisSpace.AllSurfaceLast; ++SurfNum) {
                    auto &thisSurface = state.dataSurface->Surface(SurfNum);
                    if (thisSurface.Class == SurfaceClass::IntMass) {
                        internalMassSurfacesPresent = true;
                        continue;
                    }
                    if (!thisSurface.IsAirBoundarySurf) nonInternalMassSurfacesPresent = true;
                    if (thisSurface.Class == SurfaceClass::Wall || (thisSurface.Class == SurfaceClass::Roof) ||
                        (thisSurface.Class == SurfaceClass::Floor)) {

                        thisZone.Centroid.x += thisSurface.Centroid.x * thisSurface.GrossArea;
                        thisZone.Centroid.y += thisSurface.Centroid.y * thisSurface.GrossArea;
                        thisZone.Centroid.z += thisSurface.Centroid.z * thisSurface.GrossArea;
                        TotSurfArea += thisSurface.GrossArea;
                    }
                    // The problem with doing things using these array reduction templates is that in cases like
                    // these, you are traversing the thisSurface.Vertex array six times.
                    thisZone.MinimumX = min(thisZone.MinimumX, minval(thisSurface.Vertex, &Vector::x));
                    thisZone.MaximumX = max(thisZone.MaximumX, maxval(thisSurface.Vertex, &Vector::x));
                    thisZone.MinimumY = min(thisZone.MinimumY, minval(thisSurface.Vertex, &Vector::y));
                    thisZone.MaximumY = max(thisZone.MaximumY, maxval(thisSurface.Vertex, &Vector::y));
                    thisZone.MinimumZ = min(thisZone.MinimumZ, minval(thisSurface.Vertex, &Vector::z));
                    thisZone.MaximumZ = max(thisZone.MaximumZ, maxval(thisSurface.Vertex, &Vector::z));
                }
            }
            if (TotSurfArea > 0.0) {
                thisZone.Centroid.x /= TotSurfArea;
                thisZone.Centroid.y /= TotSurfArea;
                thisZone.Centroid.z /= TotSurfArea;
            }
            if (internalMassSurfacesPresent && !nonInternalMassSurfacesPresent) {
                ShowSevereError(
                    state, format("{}Zone=\"{}\" has only internal mass surfaces.  Need at least one other surface.", RoutineName, thisZone.Name));
                ErrorsFound = true;
            }
        }

        state.dataSurface->SurfAdjacentZone.dimension(state.dataSurface->TotSurfaces, 0);
        // note -- adiabatic surfaces will show same zone as surface
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (state.dataSurface->Surface(SurfNum).ExtBoundCond <= 0) continue;
            state.dataSurface->SurfAdjacentZone(SurfNum) = state.dataSurface->Surface(state.dataSurface->Surface(SurfNum).ExtBoundCond).Zone;
        }

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                auto &thisSurface = state.dataSurface->Surface(SurfNum);
                if (!thisSurface.HeatTransSurf && thisSurface.ZoneName == state.dataHeatBal->Zone(ZoneNum).Name)
                    ++state.dataHeatBal->Zone(ZoneNum).NumShadingSurfaces;

                if (thisSurface.Zone != ZoneNum) continue;

                if (thisSurface.HeatTransSurf &&
                    (thisSurface.Class == SurfaceClass::Wall || thisSurface.Class == SurfaceClass::Roof || thisSurface.Class == SurfaceClass::Floor))
                    ++state.dataHeatBal->Zone(ZoneNum).NumSurfaces;

                if (thisSurface.HeatTransSurf && (thisSurface.Class == SurfaceClass::Window || thisSurface.Class == SurfaceClass::GlassDoor ||
                                                  thisSurface.Class == SurfaceClass::Door || thisSurface.Class == SurfaceClass::TDD_Dome ||
                                                  thisSurface.Class == SurfaceClass::TDD_Diffuser))
                    ++state.dataHeatBal->Zone(ZoneNum).NumSubSurfaces;

            } // surfaces
        }     // zones

        for (int const SurfNum : state.dataSurface->AllSurfaceListReportOrder) {
            auto &thisSurface = state.dataSurface->Surface(SurfNum);
            bool isWithConvCoefValid = false;
            Real64 NominalUwithConvCoeffs = 0.0;
            std::string cNominalUwithConvCoeffs;
            std::string cNominalU;
            if (thisSurface.Construction > 0 && thisSurface.Construction <= state.dataHeatBal->TotConstructs) {
                NominalUwithConvCoeffs = ComputeNominalUwithConvCoeffs(state, SurfNum, isWithConvCoefValid);
                if (isWithConvCoefValid) {
                    cNominalUwithConvCoeffs = format("{:.3R}", NominalUwithConvCoeffs);
                } else {
                    cNominalUwithConvCoeffs = "[invalid]";
                }
                if ((thisSurface.Class == SurfaceClass::Window) || (thisSurface.Class == SurfaceClass::TDD_Dome)) {
                    // SurfaceClass::Window also covers glass doors and TDD:Diffusers
                    cNominalU = "N/A";
                } else {
                    cNominalU = format("{:.3R}", state.dataHeatBal->NominalU(thisSurface.Construction));
                }
            } else {
                cNominalUwithConvCoeffs = "**";
                cNominalU = "**";
            }

            // populate the predefined report related to u-values with films
            // only exterior surfaces including underground
            DataSurfaces::SurfaceClass const SurfaceClass(thisSurface.Class);
            if ((thisSurface.ExtBoundCond == ExternalEnvironment) || (thisSurface.ExtBoundCond == Ground) ||
                (thisSurface.ExtBoundCond == KivaFoundation) || (thisSurface.ExtBoundCond == GroundFCfactorMethod)) {
                if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchOpUfactFilm, thisSurface.Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchDrUfactFilm, thisSurface.Name, NominalUwithConvCoeffs, 3);
                }
            } else {
                if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchIntOpUfactFilm, thisSurface.Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    OutputReportPredefined::PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchIntDrUfactFilm, thisSurface.Name, NominalUwithConvCoeffs, 3);
                }
            }
        } // surfaces

        // Write number of shadings to initialization output file
        print(state.files.eio,
              "! <Shading Summary>, Number of Fixed Detached Shades, Number of Building Detached Shades, Number of Attached Shades\n");

        print(state.files.eio,
              " Shading Summary,{},{},{}\n",
              state.dataSurface->FixedShadingCount,
              state.dataSurface->BuildingShadingCount,
              state.dataSurface->AttachedShadingCount);

        // Write number of zones header to initialization output file
        print(state.files.eio, "! <Zone Summary>, Number of Zones, Number of Zone Surfaces, Number of SubSurfaces\n");

        print(state.files.eio,
              " Zone Summary,{},{},{}\n",
              state.dataGlobal->NumOfZones,
              state.dataSurface->TotSurfaces - state.dataSurface->FixedShadingCount - state.dataSurface->BuildingShadingCount -
                  state.dataSurface->AttachedShadingCount,
              sum(state.dataHeatBal->Zone, &ZoneData::NumSubSurfaces));

        // Write Zone Information header to the initialization output file
        static constexpr std::string_view Format_721(
            "! <Zone Information>,Zone Name,North Axis {deg},Origin X-Coordinate {m},Origin Y-Coordinate {m},Origin Z-Coordinate "
            "{m},Centroid X-Coordinate {m},Centroid Y-Coordinate {m},Centroid Z-Coordinate {m},Type,Zone Multiplier,Zone List "
            "Multiplier,Minimum X {m},Maximum X {m},Minimum Y {m},Maximum Y {m},Minimum Z {m},Maximum Z {m},Ceiling Height {m},Volume "
            "{m3},Zone Inside Convection Algorithm {Simple-Detailed-CeilingDiffuser-TrombeWall},Zone Outside Convection Algorithm "
            "{Simple-Detailed-Tarp-MoWitt-DOE-2-BLAST}, Floor Area {m2},Exterior Gross Wall Area {m2},Exterior Net Wall Area {m2},Exterior "
            "Window "
            "Area {m2}, Number of Surfaces, Number of SubSurfaces, Number of Shading SubSurfaces,  Part of Total Building Area");
        print(state.files.eio, "{}\n", Format_721);

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            // Write Zone Information to the initialization output file
            std::string String1;
            std::string String2;
            std::string String3;

            switch (state.dataHeatBal->Zone(ZoneNum).IntConvAlgo) {
            case Convect::HcInt::ASHRAESimple: {
                String1 = "Simple";
            } break;
            case Convect::HcInt::ASHRAETARP: {
                String1 = "TARP";
            } break;
            case Convect::HcInt::CeilingDiffuser: {
                String1 = "CeilingDiffuser";
            } break;
            case Convect::HcInt::TrombeWall: {
                String1 = "TrombeWall";
            } break;
            case Convect::HcInt::AdaptiveConvectionAlgorithm: {
                String1 = "AdaptiveConvectionAlgorithm";
            } break;
            case Convect::HcInt::ASTMC1340: {
                String1 = "ASTMC1340";
            } break;
            default:
                break;
            }

            switch (state.dataHeatBal->Zone(ZoneNum).ExtConvAlgo) {
            case Convect::HcExt::ASHRAESimple: {
                String2 = "Simple";
            } break;
            case Convect::HcExt::ASHRAETARP: {
                String2 = "TARP";
            } break;
            case Convect::HcExt::TarpHcOutside: {
                String2 = "TARP";
            } break;
            case Convect::HcExt::MoWiTTHcOutside: {
                String2 = "MoWitt";
            } break;
            case Convect::HcExt::DOE2HcOutside: {
                String2 = "DOE-2";
            } break;
            case Convect::HcExt::AdaptiveConvectionAlgorithm: {
                String2 = "AdaptiveConvectionAlgorithm";
            } break;
            default:
                break;
            }

            String3 = (state.dataHeatBal->Zone(ZoneNum).isPartOfTotalArea) ? "Yes" : "No";

            static constexpr std::string_view Format_720(
                " Zone Information, "
                "{},{:.1R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{},{},{},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},"
                "{:.2R},{:.2R},{},{},{:.2R},{:.2R},{:.2R},{:.2R},{},{},{},{}\n");

            print(state.files.eio,
                  Format_720,
                  state.dataHeatBal->Zone(ZoneNum).Name,
                  state.dataHeatBal->Zone(ZoneNum).RelNorth,
                  state.dataHeatBal->Zone(ZoneNum).OriginX,
                  state.dataHeatBal->Zone(ZoneNum).OriginY,
                  state.dataHeatBal->Zone(ZoneNum).OriginZ,
                  state.dataHeatBal->Zone(ZoneNum).Centroid.x,
                  state.dataHeatBal->Zone(ZoneNum).Centroid.y,
                  state.dataHeatBal->Zone(ZoneNum).Centroid.z,
                  state.dataHeatBal->Zone(ZoneNum).OfType,
                  state.dataHeatBal->Zone(ZoneNum).Multiplier,
                  state.dataHeatBal->Zone(ZoneNum).ListMultiplier,
                  state.dataHeatBal->Zone(ZoneNum).MinimumX,
                  state.dataHeatBal->Zone(ZoneNum).MaximumX,
                  state.dataHeatBal->Zone(ZoneNum).MinimumY,
                  state.dataHeatBal->Zone(ZoneNum).MaximumY,
                  state.dataHeatBal->Zone(ZoneNum).MinimumZ,
                  state.dataHeatBal->Zone(ZoneNum).MaximumZ,
                  state.dataHeatBal->Zone(ZoneNum).CeilingHeight,
                  state.dataHeatBal->Zone(ZoneNum).Volume,
                  String1,
                  String2,
                  state.dataHeatBal->Zone(ZoneNum).FloorArea,
                  state.dataHeatBal->Zone(ZoneNum).ExtGrossWallArea,
                  state.dataHeatBal->Zone(ZoneNum).ExtNetWallArea,
                  state.dataHeatBal->Zone(ZoneNum).ExtWindowArea,
                  state.dataHeatBal->Zone(ZoneNum).NumSurfaces,
                  state.dataHeatBal->Zone(ZoneNum).NumSubSurfaces,
                  state.dataHeatBal->Zone(ZoneNum).NumShadingSurfaces,
                  String3);

        } // ZoneNum

        // Set up solar distribution enclosures allowing for any air boundaries
        SetupEnclosuresAndAirBoundaries(state, state.dataViewFactor->EnclSolInfo, SurfaceGeometry::enclosureType::SolarEnclosures, ErrorsFound);

        // Do the Stratosphere check
        SetZoneOutBulbTempAt(state);
        CheckZoneOutBulbTempAt(state);
    }

    void AllocateSurfaceArrays(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine allocates all of the arrays at the module level which
        // require allocation.

        // METHODOLOGY EMPLOYED:
        // Allocation is dependent on the user input file.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na
        state.dataSurface->ShadeV.allocate(state.dataSurface->TotSurfaces);
        for (auto &e : state.dataSurface->ShadeV)
            e.NVert = 0;
        // Individual components (XV,YV,ZV) allocated in routine ProcessSurfaceVertices
        state.dataSurface->T0.dimension(state.dataSurface->TotSurfaces, Vector3<Real64>(0.0, 0.0, 0.0));

        // Surface EMS arrays
        state.dataSurface->SurfEMSConstructionOverrideON.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfEMSConstructionOverrideValue.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfEMSOverrideIntConvCoef.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfEMSValueForIntConvCoef.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfEMSOverrideExtConvCoef.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfEMSValueForExtConvCoef.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutDryBulbTempEMSOverrideOn.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutDryBulbTempEMSOverrideValue.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutWetBulbTempEMSOverrideOn.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutWetBulbTempEMSOverrideValue.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfWindSpeedEMSOverrideOn.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfWindSpeedEMSOverrideValue.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfViewFactorGroundEMSOverrideOn.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfViewFactorGroundEMSOverrideValue.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfWindDirEMSOverrideOn.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfWindDirEMSOverrideValue.allocate(state.dataSurface->TotSurfaces);
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfEMSConstructionOverrideON(SurfNum) = false;
            state.dataSurface->SurfEMSConstructionOverrideValue(SurfNum) = 0.0;
            state.dataSurface->SurfEMSOverrideIntConvCoef(SurfNum) = false;
            state.dataSurface->SurfEMSValueForIntConvCoef(SurfNum) = 0.0;
            state.dataSurface->SurfEMSOverrideExtConvCoef(SurfNum) = false;
            state.dataSurface->SurfEMSValueForExtConvCoef(SurfNum) = 0.0;
            state.dataSurface->SurfOutDryBulbTempEMSOverrideOn(SurfNum) = false;
            state.dataSurface->SurfOutDryBulbTempEMSOverrideValue(SurfNum) = 0.0;
            state.dataSurface->SurfOutWetBulbTempEMSOverrideOn(SurfNum) = false;
            state.dataSurface->SurfOutWetBulbTempEMSOverrideValue(SurfNum) = 0.0;
            state.dataSurface->SurfWindSpeedEMSOverrideOn(SurfNum) = false;
            state.dataSurface->SurfWindSpeedEMSOverrideValue(SurfNum) = 0.0;
            state.dataSurface->SurfViewFactorGroundEMSOverrideOn(SurfNum) = false;
            state.dataSurface->SurfViewFactorGroundEMSOverrideValue(SurfNum) = 0.0;
            state.dataSurface->SurfWindDirEMSOverrideOn(SurfNum) = false;
            state.dataSurface->SurfWindDirEMSOverrideValue(SurfNum) = 0.0;
        }
        // Following are surface hb arrays
        state.dataSurface->SurfOutDryBulbTemp.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutWetBulbTemp.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutWindSpeed.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfOutWindDir.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfGenericContam.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfPenumbraID.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfAirSkyRadSplit.allocate(state.dataSurface->TotSurfaces);
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfOutDryBulbTemp(SurfNum) = 0.0;
            state.dataSurface->SurfOutWetBulbTemp(SurfNum) = 0.0;
            state.dataSurface->SurfOutWindSpeed(SurfNum) = 0.0;
            state.dataSurface->SurfOutWindDir(SurfNum) = 0.0;
            state.dataSurface->SurfGenericContam(SurfNum) = 0.0;
            state.dataSurface->SurfPenumbraID(SurfNum) = -1;
            state.dataSurface->SurfAirSkyRadSplit(SurfNum) = 0.0;
        }
        // Following are surface property arrays used in SurfaceGeometry
        state.dataSurface->SurfShadowRecSurfNum.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfShadowDisabledZoneList.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfShadowDiffuseSolRefl.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfShadowDiffuseVisRefl.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfShadowGlazingFrac.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfShadowGlazingConstruct.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfMaterialMovInsulExt.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfMaterialMovInsulInt.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfSchedMovInsulExt.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfSchedMovInsulInt.allocate(state.dataSurface->TotSurfaces);
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfShadowRecSurfNum(SurfNum) = 0;
            state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum) = 0.0;
            state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum) = 0.0;
            state.dataSurface->SurfShadowGlazingFrac(SurfNum) = 0.0;
            state.dataSurface->SurfShadowGlazingConstruct(SurfNum) = 0;
            state.dataSurface->SurfMaterialMovInsulExt(SurfNum) = 0;
            state.dataSurface->SurfMaterialMovInsulInt(SurfNum) = 0;
            state.dataSurface->SurfSchedMovInsulExt(SurfNum) = 0;
            state.dataSurface->SurfSchedMovInsulInt(SurfNum) = 0;
        }
        state.dataSurface->SurfExtEcoRoof.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfExtCavityPresent.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfExtCavNum.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfIsPV.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfIsICS.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfIsPool.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfICSPtr.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfIsRadSurfOrVentSlabOrPool.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfDaylightingShelfInd.allocate(state.dataSurface->TotSurfaces);
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfExtEcoRoof(SurfNum) = false;
            state.dataSurface->SurfExtCavityPresent(SurfNum) = false;
            state.dataSurface->SurfExtCavNum(SurfNum) = 0;
            state.dataSurface->SurfIsPV(SurfNum) = false;
            state.dataSurface->SurfIsICS(SurfNum) = false;
            state.dataSurface->SurfIsPool(SurfNum) = false;
            state.dataSurface->SurfICSPtr(SurfNum) = 0;
            state.dataSurface->SurfIsRadSurfOrVentSlabOrPool(SurfNum) = false;
            state.dataSurface->SurfDaylightingShelfInd(SurfNum) = 0;
        }
        state.dataSurface->SurfLowTempErrCount.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfHighTempErrCount.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->surfIntConv.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfTAirRef.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->SurfTAirRefRpt.allocate(state.dataSurface->TotSurfaces);
        state.dataSurface->surfExtConv.allocate(state.dataSurface->TotSurfaces);
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            state.dataSurface->SurfLowTempErrCount(SurfNum) = 0;
            state.dataSurface->SurfHighTempErrCount(SurfNum) = 0;
            state.dataSurface->surfIntConv(SurfNum) = SurfIntConv();
            state.dataSurface->surfExtConv(SurfNum) = SurfExtConv();
            state.dataSurface->SurfTAirRef(SurfNum) = DataSurfaces::RefAirTemp::Invalid;
            state.dataSurface->SurfTAirRefRpt(SurfNum) = (int)DataSurfaces::RefAirTemp::Invalid;
        }
    }

    void GetSurfaceData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   November 1997
        //       MODIFIED       April 1999, Linda Lawrie
        //                      Dec. 2000, FW (add "one-wall zone" checks)
        //       RE-ENGINEERED  May 2000, Linda Lawrie (breakout surface type gets)

        // PURPOSE OF THIS SUBROUTINE:
        // The purpose of this subroutine is to read in the surface information
        // from the input data file and interpret and put in the derived type

        // METHODOLOGY EMPLOYED:
        // The order of surfaces does not matter and the surfaces are resorted into
        // the hierarchical order:
        //  All Shading Surfaces
        //  Airwalls for space x1
        //  Base Surfaces for space x1
        //  Opaque Subsurfaces for space x1
        //  Window Subsurfaces for space x1
        //  TDD Dome Surfaces for space x1
        //  Airwalls for space x2
        //  Base Surfaces for space x2
        //  etc
        //  Pointers are set in the spaces (AllSurfaceFirst/Last, HTSurfaceFirst/Last, OpaqOrIntMassSurfaceFirst/Last, WindowSurfaceFirst/Last,
        //  OpaqOrWinSurfaceFirst/Last, TDDDomeFirst/Last)

        // REFERENCES:
        //   This routine manages getting the input for the following Objects:
        // SurfaceGeometry
        // Surface:Shading:Detached
        // Surface:HeatTransfer
        // Surface:HeatTransfer:Sub
        // Surface:Shading:Attached
        // Surface:InternalMass

        // Vertex input:
        //  N3 , \field Number of Surface Vertices -- Number of (X,Y,Z) groups in this surface
        //       \note currently limited 3 or 4, later?
        //       \min 3
        //       \max 4
        //       \memo vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
        //       \memo are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
        //       \memo for some internal calculations, but all coordinates are given in an "absolute" system.
        //  N4,  \field Vertex 1 X-coordinate
        //       \units m
        //       \type real
        //  N5 , \field Vertex 1 Y-coordinate
        //       \units m
        //       \type real
        //  N6 , \field Vertex 1 Z-coordinate
        //       \units m
        //       \type real
        //  N7,  \field Vertex 2 X-coordinate
        //       \units m
        //       \type real
        //  N8,  \field Vertex 2 Y-coordinate
        //       \units m
        //       \type real
        //  N9,  \field Vertex 2 Z-coordinate
        //       \units m
        //       \type real
        //  N10, \field Vertex 3 X-coordinate
        //       \units m
        //       \type real
        //  N11, \field Vertex 3 Y-coordinate
        //       \units m
        //       \type real
        //  N12, \field Vertex 3 Z-coordinate
        //       \units m
        //       \type real
        //  N13, \field Vertex 4 X-coordinate
        //       \units m
        //       \type real
        //  N14, \field Vertex 4 Y-coordinate
        //       \type real
        //       \units m
        //  N15; \field Vertex 4 Z-coordinate
        //       \units m
        //       \type real

        // The vertices are stored in the surface derived type.
        //      +(1)-------------------------(4)+
        //      |                               |
        //      |                               |
        //      |                               |
        //      +(2)-------------------------(3)+
        //  The above diagram shows the actual coordinate points of a typical wall
        //  (you're on the outside looking toward the wall) as stored into
        //  Surface%Vertex(1:<number-of-sides>)

        using namespace Vectors;
        using namespace DataErrorTracking;

        static constexpr std::string_view RoutineName("GetSurfaceData: ");

        int ConstrNum;            // Construction number
        int Found;                // For matching interzone surfaces
        int ConstrNumFound;       // Construction number of matching interzone surface
        bool NonMatch(false);     // Error for non-matching interzone surfaces
        int MovedSurfs;           // Number of Moved Surfaces (when sorting into hierarchical structure)
        bool SurfError(false);    // General Surface Error, causes fatal error at end of routine
        int TotLay;               // Total layers in a construction
        int TotLayFound;          // Total layers in the construction of a matching interzone surface
        int TotDetachedFixed;     // Total Shading:Site:Detailed entries
        int TotDetachedBldg;      // Total Shading:Building:Detailed entries
        int TotRectDetachedFixed; // Total Shading:Site entries
        int TotRectDetachedBldg;  // Total Shading:Building entries
        int TotHTSurfs;           // Number of BuildingSurface:Detailed items to obtain
        int TotDetailedWalls;     // Number of Wall:Detailed items to obtain
        int TotDetailedRoofs;     // Number of RoofCeiling:Detailed items to obtain
        int TotDetailedFloors;    // Number of Floor:Detailed items to obtain
        int TotHTSubs;            // Number of FenestrationSurface:Detailed items to obtain
        int TotShdSubs;           // Number of Shading:Zone:Detailed items to obtain
        int TotIntMassSurfaces;   // Number of InternalMass surfaces to obtain
        // Simple Surfaces (Rectangular)
        int TotRectExtWalls;   // Number of Exterior Walls to obtain
        int TotRectIntWalls;   // Number of Adiabatic Walls to obtain
        int TotRectIZWalls;    // Number of Interzone Walls to obtain
        int TotRectUGWalls;    // Number of Underground to obtain
        int TotRectRoofs;      // Number of Roofs to obtain
        int TotRectCeilings;   // Number of Adiabatic Ceilings to obtain
        int TotRectIZCeilings; // Number of Interzone Ceilings to obtain
        int TotRectGCFloors;   // Number of Floors with Ground Contact to obtain
        int TotRectIntFloors;  // Number of Adiabatic Walls to obtain
        int TotRectIZFloors;   // Number of Interzone Floors to obtain
        int TotRectWindows;
        int TotRectDoors;
        int TotRectGlazedDoors;
        int TotRectIZWindows;
        int TotRectIZDoors;
        int TotRectIZGlazedDoors;
        int TotOverhangs;
        int TotOverhangsProjection;
        int TotFins;
        int TotFinsProjection;
        bool RelWarning(false);
        int ConstrNumSh;      // Shaded construction number for a window
        int LayNumOutside;    // Outside material numbers for a shaded construction
        int BlNum;            // Blind number
        int AddedSubSurfaces; // Subsurfaces (windows) added when windows reference Window5 Data File
        // entries with two glazing systems
        int NeedToAddSurfaces;    // Surfaces that will be added due to "unentered" other zone surface
        int NeedToAddSubSurfaces; // SubSurfaces that will be added due to "unentered" other zone surface
        int CurNewSurf;
        int FirstTotalSurfaces;
        int Vert;
        int n;

        int MultFound;
        int MultSurfNum;
        std::string MultString;
        bool SubSurfaceSevereDisplayed;
        bool subSurfaceError(false);
        bool errFlag;

        int iTmp1;
        int iTmp2;
        // unused  INTEGER :: SchID
        int BlNumNew;
        int WinShadingControlPtr(0);
        int ErrCount;
        bool izConstDiff;    // differences in construction for IZ surfaces
        bool izConstDiffMsg; // display message about hb diffs only once.

        // Get the total number of surfaces to allocate derived type and for surface loops

        auto &sg = state.dataSurfaceGeometry;
        
        if (state.dataSurfaceGeometry->GetSurfaceDataOneTimeFlag) {
            return;
        } else {
            state.dataSurfaceGeometry->GetSurfaceDataOneTimeFlag = true;
        }

        GetGeometryParameters(state, ErrorsFound);

        if (state.dataSurface->WorldCoordSystem) {
            if (state.dataHeatBal->BuildingAzimuth != 0.0) RelWarning = true;
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (state.dataHeatBal->Zone(ZoneNum).RelNorth != 0.0) RelWarning = true;
            }
            if (RelWarning && !state.dataSurfaceGeometry->WarningDisplayed) {
                ShowWarningError(
                    state,
                    format("{}World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.",
                           RoutineName));
                ShowContinueError(state,
                                  "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                state.dataSurfaceGeometry->WarningDisplayed = true;
            }
            RelWarning = false;
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (state.dataHeatBal->Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (state.dataHeatBal->Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (state.dataHeatBal->Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !state.dataSurfaceGeometry->WarningDisplayed) {
                ShowWarningError(
                    state,
                    format("{}World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.",
                           RoutineName));
                ShowContinueError(state,
                                  "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                state.dataSurfaceGeometry->WarningDisplayed = true;
            }
        }

        TotDetachedFixed = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Site:Detailed");
        TotDetachedBldg = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Building:Detailed");
        TotRectDetachedFixed = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Site");
        TotRectDetachedBldg = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Building");
        TotHTSurfs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "BuildingSurface:Detailed");
        TotDetailedWalls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Wall:Detailed");
        TotDetailedRoofs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "RoofCeiling:Detailed");
        TotDetailedFloors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Floor:Detailed");
        TotHTSubs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "FenestrationSurface:Detailed");
        TotShdSubs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Zone:Detailed");
        TotOverhangs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Overhang");
        TotOverhangsProjection = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Overhang:Projection");
        TotFins = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Fin");
        TotFinsProjection = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Shading:Fin:Projection");
        TotRectWindows = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Window");
        TotRectDoors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Door");
        TotRectGlazedDoors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GlazedDoor");
        TotRectIZWindows = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Window:Interzone");
        TotRectIZDoors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Door:Interzone");
        TotRectIZGlazedDoors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "GlazedDoor:Interzone");
        TotRectExtWalls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Wall:Exterior");
        TotRectIntWalls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Wall:Adiabatic");
        TotRectIZWalls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Wall:Interzone");
        TotRectUGWalls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Wall:Underground");
        TotRectRoofs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Roof");
        TotRectCeilings = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Ceiling:Adiabatic");
        TotRectIZCeilings = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Ceiling:Interzone");
        TotRectGCFloors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Floor:GroundContact");
        TotRectIntFloors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Floor:Adiabatic");
        TotRectIZFloors = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Floor:Interzone");

        state.dataSurface->TotOSC = 0;

        TotIntMassSurfaces = GetNumIntMassSurfaces(state);

        state.dataSurface->TotSurfaces = (TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg) * 2 + TotHTSurfs +
                                         TotHTSubs + TotShdSubs * 2 + TotIntMassSurfaces + TotOverhangs * 2 + TotOverhangsProjection * 2 +
                                         TotFins * 4 + TotFinsProjection * 4 + TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors +
                                         TotRectWindows + TotRectDoors + TotRectGlazedDoors + TotRectIZWindows + TotRectIZDoors +
                                         TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls + TotRectRoofs +
                                         TotRectCeilings + TotRectIZCeilings + TotRectGCFloors + TotRectIntFloors + TotRectIZFloors;

        state.dataSurfaceGeometry->SurfaceTmp.allocate(state.dataSurface->TotSurfaces); // Allocate the Surface derived type appropriately
        // SurfaceTmp structure is allocated via derived type initialization.

        int NumSurfs = 0;
        AddedSubSurfaces = 0;
        state.dataErrTracking->AskForSurfacesReport = true;

        GetDetShdSurfaceData(state, ErrorsFound, NumSurfs, TotDetachedFixed, TotDetachedBldg);

        GetRectDetShdSurfaceData(state, ErrorsFound, NumSurfs, TotRectDetachedFixed, TotRectDetachedBldg);

        GetHTSurfaceData(state,
                         ErrorsFound,
                         NumSurfs,
                         TotHTSurfs,
                         TotDetailedWalls,
                         TotDetailedRoofs,
                         TotDetailedFloors,
                         NeedToAddSurfaces);

        GetRectSurfaces(state,
                        ErrorsFound,
                        NumSurfs,
                        TotRectExtWalls,
                        TotRectIntWalls,
                        TotRectIZWalls,
                        TotRectUGWalls,
                        TotRectRoofs,
                        TotRectCeilings,
                        TotRectIZCeilings,
                        TotRectGCFloors,
                        TotRectIntFloors,
                        TotRectIZFloors,
                        NeedToAddSurfaces);

        GetHTSubSurfaceData(state,
                            ErrorsFound,
                            NumSurfs,
                            TotHTSubs,
                            AddedSubSurfaces,
                            NeedToAddSubSurfaces);

        GetRectSubSurfaces(state,
                           ErrorsFound,
                           NumSurfs,
                           TotRectWindows,
                           TotRectDoors,
                           TotRectGlazedDoors,
                           TotRectIZWindows,
                           TotRectIZDoors,
                           TotRectIZGlazedDoors,
                           AddedSubSurfaces,
                           NeedToAddSubSurfaces);

        GetAttShdSurfaceData(state, ErrorsFound, NumSurfs, TotShdSubs);

        GetSimpleShdSurfaceData(state, ErrorsFound, NumSurfs, TotOverhangs, TotOverhangsProjection, TotFins, TotFinsProjection);

        GetIntMassSurfaceData(state, ErrorsFound, NumSurfs);

        state.dataSurface->TotSurfaces = NumSurfs + AddedSubSurfaces + NeedToAddSurfaces + NeedToAddSubSurfaces;

        if (ErrorsFound) {
            ShowFatalError(state, format("{}Errors discovered, program terminates.", RoutineName));
        }

        state.dataSurface->Surface.allocate(state.dataSurface->TotSurfaces); // Allocate the Surface derived type appropriately
        state.dataSurface->SurfaceWindow.allocate(state.dataSurface->TotSurfaces);
        AllocateSurfaceArrays(state);
        AllocateSurfaceWindows(state, state.dataSurface->TotSurfaces);

        // Have to make room for added surfaces, if needed
        FirstTotalSurfaces = NumSurfs + AddedSubSurfaces;
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) {
            state.dataSurfaceGeometry->SurfaceTmp.redimension(state.dataSurface->TotSurfaces);
        }

        // add the "need to add" surfaces
        // Debug    write(outputfiledebug,*) ' need to add ',NeedtoAddSurfaces+NeedToAddSubSurfaces
        // How is there not an unitialized variable warning/error on CurNewSurf?
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) CurNewSurf = FirstTotalSurfaces;
        for (int SurfNum = 1; SurfNum <= FirstTotalSurfaces; ++SurfNum) {
            auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
            if (surf.ExtBoundCond != UnenteredAdjacentZoneSurface) continue;
            // Need to add surface
            ++CurNewSurf;
            // Debug    write(outputfiledebug,*) ' adding surface=',curnewsurf
            sg->SurfaceTmp(CurNewSurf) = sg->SurfaceTmp(SurfNum);
            //  Basic parameters are the same for both surfaces.
            Found = Util::FindItemInList(surf.ExtBoundCondName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
            if (Found == 0) continue;

            auto &newSurf = sg->SurfaceTmp(CurNewSurf);
            
            newSurf.Zone = Found;
            newSurf.ZoneName = state.dataHeatBal->Zone(Found).Name;
            // Reverse Construction
            newSurf.Construction = AssignReverseConstructionNumber(state, surf.Construction, SurfError);
            newSurf.ConstructionStoredInputValue = newSurf.Construction;
            // Reverse Vertices
            for (int Vert = 1; Vert <= surf.Sides; ++Vert) {
                newSurf.Vertex(Vert) = surf.Vertex(surf.Sides + 1 - Vert);
            }
            if (newSurf.Sides > 2) {
                newSurf.NewellAreaVec = CalcNewellAreaVector(newSurf.Vertex, newSurf.Sides);
                newSurf.GrossArea = length(newSurf.NewellAreaVec);
                newSurf.Area = newSurf.GrossArea;
                newSurf.NetAreaShadowCalc = newSurf.Area;
                newSurf.NewellNormVec = CalcNewellNormalVector(newSurf.Vertex, newSurf.Sides);
                std::tie(newSurf.Azimuth, newSurf.Tilt) = CalcAzimuthAndTilt(newSurf.Vertex, newSurf.lcsx, newSurf.lcsy, newSurf.lcsz, newSurf.NewellNormVec);
                newSurf.convOrientation = Convect::GetSurfConvOrientation(newSurf.Tilt);

                // Sine and cosine of azimuth and tilt
                newSurf.SinAzim = std::sin(newSurf.Azimuth * Constant::DegToRadians);
                newSurf.CosAzim = std::cos(newSurf.Azimuth * Constant::DegToRadians);
                newSurf.SinTilt = std::sin(newSurf.Tilt * Constant::DegToRadians);
                newSurf.CosTilt = std::cos(newSurf.Tilt * Constant::DegToRadians);
                // Outward normal unit vector (pointing away from room)
                newSurf.OutNormVec = newSurf.NewellNormVec;
                
                auto &outNormVec = newSurf.OutNormVec;
                if (std::abs(outNormVec.x - 1.0) < 1.e-06) outNormVec.x = +1.0;
                if (std::abs(outNormVec.x + 1.0) < 1.e-06) outNormVec.x = -1.0;
                if (std::abs(outNormVec.x) < 1.e-06) outNormVec.x = 0.0;
                if (std::abs(outNormVec.y - 1.0) < 1.e-06) outNormVec.y = +1.0;
                if (std::abs(outNormVec.y + 1.0) < 1.e-06) outNormVec.y = -1.0;
                if (std::abs(outNormVec.y) < 1.e-06) outNormVec.y = 0.0;
                if (std::abs(outNormVec.z - 1.0) < 1.e-06) outNormVec.z = +1.0;
                if (std::abs(outNormVec.z + 1.0) < 1.e-06) outNormVec.z = -1.0;
                if (std::abs(outNormVec.z) < 1.e-06) outNormVec.z = 0.0;

                // Can perform tests on this surface here
                newSurf.ViewFactorSky = 0.5 * (1.0 + newSurf.CosTilt);
                newSurf.ViewFactorGround = 0.5 * (1.0 - newSurf.CosTilt);

                // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
                // surfaces
                newSurf.ViewFactorSkyIR = newSurf.ViewFactorSky;
                newSurf.ViewFactorGroundIR = 0.5 * (1.0 - newSurf.CosTilt);
            }

            // Change Name
            newSurf.Name = "iz-" + surf.Name;
            // Debug   write(outputfiledebug,*) ' new surf name=',TRIM(SurfaceTmp(CurNewSurf)%Name)
            // Debug   write(outputfiledebug,*) ' new surf in zone=',TRIM(surfacetmp(curnewsurf)%zoneName)
            newSurf.ExtBoundCond = UnreconciledZoneSurface;
            surf.ExtBoundCond = UnreconciledZoneSurface;
            newSurf.ExtBoundCondName = surf.Name;
            surf.ExtBoundCondName = newSurf.Name;
            if (newSurf.Class == SurfaceClass::Roof || newSurf.Class == SurfaceClass::Wall || newSurf.Class == SurfaceClass::Floor) {
                // base surface
                if (surf.Class == SurfaceClass::Roof) {
                    newSurf.Class = SurfaceClass::Floor;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a floor'
                } else if (surf.Class == SurfaceClass::Floor) {
                    newSurf.Class = SurfaceClass::Roof;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a roof'
                }
                newSurf.BaseSurf = CurNewSurf;
                newSurf.BaseSurfName = newSurf.Name;
                // Debug        write(outputfiledebug,*) ' basesurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
            } else {
                // subsurface
                Found = Util::FindItemInList("iz-" + surf.BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, FirstTotalSurfaces + CurNewSurf - 1);
                if (Found > 0) {
                    newSurf.BaseSurfName = "iz-" + surf.BaseSurfName;
                    newSurf.BaseSurf = Found;

                    auto &baseSurf = sg->SurfaceTmp(Found);
                    baseSurf.Area -= newSurf.Area;
                    if (newSurf.Class == SurfaceClass::Window || newSurf.Class == SurfaceClass::GlassDoor) {
                        baseSurf.NetAreaShadowCalc -= newSurf.Area / newSurf.Multiplier;
                    } else { // Door, TDD:Diffuser, TDD:DOME
                        baseSurf.NetAreaShadowCalc -= newSurf.Area;
                    }
                    newSurf.ExtBoundCond = baseSurf.ExtBoundCond;
                    newSurf.ExtBoundCondName = surf.Name;
                    newSurf.ExtSolar = baseSurf.ExtSolar;
                    newSurf.ExtWind = baseSurf.ExtWind;
                    newSurf.Zone = baseSurf.Zone;
                    newSurf.ZoneName = baseSurf.ZoneName;
                    newSurf.OSCPtr = baseSurf.OSCPtr;
                    // Debug        write(outputfiledebug,*) ' subsurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
                    // Debug        write(outputfiledebug,*) ' subsurf, basesurf=',TRIM('iz-'//SurfaceTmp(SurfNum)%BaseSurfName)
                } else {
                    ShowSevereError(state, format("{}Adding unentered subsurface, could not find base surface=iz-{}", RoutineName, surf.BaseSurfName));
                    SurfError = true;
                }
            }
        } // for (SurfNum)
        //**********************************************************************************
        // After all of the surfaces have been defined then the base surfaces for the
        // sub-surfaces can be defined.  Loop through surfaces and match with the sub-surface
        // names.
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                
            if (!surf.HeatTransSurf) continue;

            // why are we doing this again?  this should have already been done.
            if (Util::SameString(surf.BaseSurfName, surf.Name)) {
                surf.BaseSurf = SurfNum;
            } else {
                surf.BaseSurf = Util::FindItemInList(surf.BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp);
            }
            if (surf.BaseSurf > 0 && SurfNum != surf.BaseSurf) {
                if (surf.HeatTransSurf) ++state.dataSurfaceGeometry->SurfaceTmp(surf.BaseSurf).NumSubSurfaces;
                if (surf.Class < SurfaceClass::Window || surf.Class > SurfaceClass::TDD_Diffuser) {
                    if (surf.Class == SurfaceClass::None) {
                        ShowSevereError(state, format("{}Invalid SubSurface detected, Surface={}", RoutineName, surf.Name));
                    } else {
                        ShowSevereError(state,
                                        format("{}Invalid SubSurface detected, Surface={}, class={} invalid class for subsurface",
                                               RoutineName,
                                               surf.Name,
                                               sg->BaseSurfCls((int)surf.Class)));
                        SurfError = true;
                    }
                }
            } // if (Found > 0)
        } // ...end of the Surface DO loop for finding BaseSurf

        //**********************************************************************************
        // The surfaces need to be hierarchical by space.  Input is allowed to be in any order.  In
        // this section the surfaces are reordered into:
        //    All shadowing surfaces (if mirrored, Mir- surface follows immediately after original)
        //      Shading:Site
        //      Shading:Building
        //      Shading:space (and variants)
        //    For each space:
        //      Walls
        //      Floors
        //      Roofs/Ceilings
        //      Internal Mass
        //      Non-Window subsurfaces (including doors)
        //      Window subsurfaces (including TubularDaylightingDiffusers)
        //      TubularDaylightingDomes
        //    After reordering, MovedSurfs should equal TotSurfaces

        // For reporting purposes, the legacy surface order is also saved in DataSurfaces::AllSurfaceListReportOrder:
        //    All shadowing surfaces (if mirrored, Mir- surface follows immediately after original)
        //      Shading:Site
        //      Shading:Building
        //      Shading:Zone (and variants)
        //    For each zone:
        //      Walls
        //        subsurfaces for each wall (windows, doors, in input order, not sorted) follow the base surface
        //      Floors
        //        subsurfaces for each floor (windows, doors, in input order, not sorted) follow the base surface
        //      Roofs/Ceilings
        //        subsurfaces for each roof/ceiling (windows, doors, in input order, not sorted) follow the base surface
        //      Internal Mass
        //    After reordering, MovedSurfs should equal TotSurfaces

        MovedSurfs = 0;
        Array1D<bool> SurfaceTmpClassMoved; // Tmp class is moved
        SurfaceTmpClassMoved.dimension(state.dataSurface->TotSurfaces, false);
        state.dataSurface->AllSurfaceListReportOrder.reserve(state.dataSurface->TotSurfaces);

        CreateMissingSpaces(state, state.dataSurfaceGeometry->SurfaceTmp);

        // Old SurfNum to New SurfNum
        // Old = order in state.dataSurfaceGeometry->SurfaceTmp
        // New = order in state.dataSurface->Surface
        EPVector<int> oldToNewSurfNums;
        oldToNewSurfNums.resize(state.dataSurface->TotSurfaces, -1);

        // Move all shading Surfaces to Front
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
            if (surf.Class != SurfaceClass::Detached_F && surf.Class != SurfaceClass::Detached_B && surf.Class != SurfaceClass::Shading)
                continue;

            //  A shading surface
            ++MovedSurfs;
            // Store list of moved surface numbers in reporting order
            state.dataSurface->Surface(MovedSurfs) = surf;
            SurfaceTmpClassMoved(SurfNum) = true; //'Moved'
            state.dataSurface->AllSurfaceListReportOrder.push_back(SurfNum);
            oldToNewSurfNums(SurfNum) = MovedSurfs;
        }

        //  For each zone

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                // Group air boundary surfaces first within each space
                for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                    auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                    if (SurfaceTmpClassMoved(SurfNum)) continue;
                    if (surf.spaceNum != spaceNum) continue;
                    int constNum = surf.Construction;
                    if (constNum == 0) continue;
                    if (!state.dataConstruction->Construct(constNum).TypeIsAirBoundary) continue;

                    //  An air boundary surface
                    surf.IsAirBoundarySurf = true;
                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = surf;
                    //  If base Surface Type (Wall, Floor, Roof/Ceiling)
                    if ((surf.Class == state.dataSurfaceGeometry->BaseSurfIDs(1)) ||
                        (surf.Class == state.dataSurfaceGeometry->BaseSurfIDs(2)) ||
                        (surf.Class == state.dataSurfaceGeometry->BaseSurfIDs(3))) {
                        // Store list of moved surface numbers in reporting order. We use the old position, we'll reconcile later
                        // We don't do it for Air Door/Air Windows yet, we want them listed below each base surf they belong to
                        state.dataSurface->AllSurfaceListReportOrder.push_back(SurfNum);
                    }
                    oldToNewSurfNums(SurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SurfNum) = true; //'Moved'
                }

                //  For each Base Surface Type (Wall, Floor, Roof/Ceiling) - put these first

                for (const DataSurfaces::SurfaceClass Loop : state.dataSurfaceGeometry->BaseSurfIDs) {

                    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);

                        if (SurfaceTmpClassMoved(SurfNum)) continue;
                        if (surf.Zone == 0) continue;

                        if (surf.spaceNum != spaceNum) continue;
                        if (surf.Class != Loop) continue;

                        ++MovedSurfs;
                        state.dataSurface->Surface(MovedSurfs) = surf;
                        oldToNewSurfNums(SurfNum) = MovedSurfs;
                        SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                        // Store list of moved surface numbers in order reporting order (subsurfaces follow their base surface)
                        state.dataSurface->AllSurfaceListReportOrder.push_back(SurfNum);

                        //  Find all subsurfaces to this surface - just to update Report them in order
                        for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {
                            // Gotta avoid pushing myself again!
                            if (SubSurfNum == SurfNum) continue;
                            // We don't check if already moved, because we didn't add them to AllSurfaceListReportOrder above!
                            if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Zone == 0) continue;
                            if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).BaseSurf != SurfNum) continue;
                            // Add original sub-surface numbers as placeholders in surface list for reporting
                            state.dataSurface->AllSurfaceListReportOrder.push_back(SubSurfNum);
                        }
                    }
                }

                // Internal mass goes next
                for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                    auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);

                    if (SurfaceTmpClassMoved(SurfNum)) continue;
                    if (surf.spaceNum != spaceNum) continue;
                    if (surf.Class != SurfaceClass::IntMass) continue;
                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = surf;
                    oldToNewSurfNums(SurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                    // Store list of moved surface numbers in reporting order
                    state.dataSurface->AllSurfaceListReportOrder.push_back(SurfNum);
                }

                // Opaque door goes next
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).spaceNum != spaceNum) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::Door) continue;

                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                    oldToNewSurfNums(SubSurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                }

                // The exterior window subsurfaces (includes SurfaceClass::Window and SurfaceClass::GlassDoor) goes next
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).spaceNum != spaceNum) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).ExtBoundCond > 0) continue; // Exterior window
                    if ((state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::Window) &&
                        (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::GlassDoor))
                        continue;

                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                    oldToNewSurfNums(SubSurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                }

                // The interior window subsurfaces (includes SurfaceClass::Window and SurfaceClass::GlassDoor) goes next
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).spaceNum != spaceNum) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).ExtBoundCond <= 0) continue;
                    if ((state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::Window) &&
                        (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::GlassDoor))
                        continue;

                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                    oldToNewSurfNums(SubSurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                }

                // The SurfaceClass::TDD_Diffuser (OriginalClass = Window) goes next
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).spaceNum != spaceNum) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::TDD_Diffuser) continue;

                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                    oldToNewSurfNums(SubSurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                }

                // Last but not least, SurfaceClass::TDD_Dome
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).spaceNum != spaceNum) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::TDD_Dome) continue;

                    ++MovedSurfs;
                    state.dataSurface->Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                    oldToNewSurfNums(SubSurfNum) = MovedSurfs;
                    SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                }
            }
        }

        // Validity checking
        assert(state.dataSurface->TotSurfaces == MovedSurfs);
        assert(state.dataSurface->TotSurfaces == (int)state.dataSurface->AllSurfaceListReportOrder.size());
        assert(state.dataSurface->TotSurfaces == (int)oldToNewSurfNums.size());

        // Assert validity of indices
        assert(std::find_if(state.dataSurface->AllSurfaceListReportOrder.cbegin(), state.dataSurface->AllSurfaceListReportOrder.cend(), [](int i) {
                   return i < 1;
               }) == state.dataSurface->AllSurfaceListReportOrder.cend());

        assert(std::find_if(oldToNewSurfNums.cbegin(), oldToNewSurfNums.cend(), [](int i) { return i < 1; }) == oldToNewSurfNums.cend());

        if (MovedSurfs != state.dataSurface->TotSurfaces) {
            ShowSevereError(
                state,
                format("{}Reordered # of Surfaces ({}) not = Total # of Surfaces ({})", RoutineName, MovedSurfs, state.dataSurface->TotSurfaces));
            SurfError = true;
            for (int Loop = 1; Loop <= state.dataSurface->TotSurfaces; ++Loop) {
                if (!SurfaceTmpClassMoved(Loop) && state.dataSurfaceGeometry->SurfaceTmp(Loop).Class == SurfaceClass::Invalid) {
                    ShowSevereError(state,
                                    format("{}Error in Surface= \"{} indicated Zone=\"{}\"",
                                           RoutineName,
                                           state.dataSurfaceGeometry->SurfaceTmp(Loop).Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(Loop).ZoneName));
                }
            }
            ShowWarningError(
                state, format("{}Remaining surface checks will use \"reordered number of surfaces\", not number of original surfaces", RoutineName));
        }

        // Realign the relationship: surface to base surface
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto &movedSurf = state.dataSurface->Surface(SurfNum);
            if (movedSurf.BaseSurf > 0) {
                int newBaseSurfNum = oldToNewSurfNums(movedSurf.BaseSurf);
                movedSurf.BaseSurf = newBaseSurfNum;

                if (newBaseSurfNum < 1) {
                    ShowFatalError(
                        state,
                        format("{}Couldn't find the new Surface Number for surface index {} named '{}'. Looking for BaseSurf old index of {}",
                               RoutineName,
                               SurfNum,
                               movedSurf.Name,
                               movedSurf.BaseSurf));
                }
            }
            auto &reportOrderNum = state.dataSurface->AllSurfaceListReportOrder[SurfNum - 1];
            if (reportOrderNum > 0) {
                int newReportOrderNum = oldToNewSurfNums(reportOrderNum);
                reportOrderNum = newReportOrderNum;
            }
        }

        state.dataSurfaceGeometry->SurfaceTmp.deallocate(); // DeAllocate the Temp Surface derived type

        createSpaceSurfaceLists(state);

        //  For each Base Surface Type (Wall, Floor, Roof)

        for (const DataSurfaces::SurfaceClass Loop : state.dataSurfaceGeometry->BaseSurfIDs) {
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {

                if (state.dataSurface->Surface(SurfNum).Zone == 0) continue;

                if (state.dataSurface->Surface(SurfNum).Class != Loop) continue;

                //  Find all subsurfaces to this surface
                for (int SubSurfNum = 1; SubSurfNum <= state.dataSurface->TotSurfaces; ++SubSurfNum) {

                    if (SurfNum == SubSurfNum) continue;
                    if (state.dataSurface->Surface(SubSurfNum).Zone == 0) continue;
                    if (state.dataSurface->Surface(SubSurfNum).BaseSurf != SurfNum) continue;

                    // Check facing angle of Sub compared to base
                    checkSubSurfAzTiltNorm(state, state.dataSurface->Surface(SurfNum), state.dataSurface->Surface(SubSurfNum), subSurfaceError);
                    if (subSurfaceError) SurfError = true;
                }
            }
        }

        //**********************************************************************************
        // Now, match up interzone surfaces
        NonMatch = false;
        izConstDiffMsg = false;
        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            auto &surf = state.dataSurface->Surface(SurfNum);
            //  Clean up Shading Surfaces, make sure they don't go through here.
            if (!surf.HeatTransSurf) continue;
            //   If other surface, match it up
            //  Both interzone and "internal" surfaces have this pointer set
            //  Internal surfaces point to themselves, Interzone to another
            if (surf.ExtBoundCond == UnreconciledZoneSurface) {
                if (not_blank(surf.ExtBoundCondName)) {
                    if (surf.ExtBoundCondName == surf.Name) {
                        Found = SurfNum;
                    } else {
                        Found = Util::FindItemInList(state.dataSurface->Surface(SurfNum).ExtBoundCondName, state.dataSurface->Surface, MovedSurfs);
                    }
                    if (Found != 0) {
                        surf.ExtBoundCond = Found;
                        auto &osSurf = state.dataSurface->Surface(Found);
                        // Check that matching surface is also "OtherZoneSurface"
                        if (osSurf.ExtBoundCond <= 0 &&
                            osSurf.ExtBoundCond != UnreconciledZoneSurface) {
                            ShowSevereError(state, format("{}Potential \"OtherZoneSurface\" is not matched correctly:", RoutineName));
                            ShowContinueError(state, format("Surface={}, Zone={}", surf.Name, surf.ZoneName));
                            ShowContinueError(state, format("Nonmatched Other/InterZone Surface={}, Zone={}", osSurf.Name, osSurf.ZoneName));
                            SurfError = true;
                        }
                        // Check that matching interzone surface has construction with reversed layers
                        if (Found != SurfNum) { // Interzone surface
                            // Make sure different zones too (CR 4110)
                            if (surf.spaceNum == osSurf.spaceNum) {
                                ++state.dataSurfaceGeometry->ErrCount2;
                                if (state.dataSurfaceGeometry->ErrCount2 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state,
                                                     format("{}CAUTION -- Interspace surfaces are occuring in the same space(s).", RoutineName));
                                    ShowContinueError(
                                        state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual occurrences.");
                                }
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state, format("{}CAUTION -- Interspace surfaces are usually in different spaces", RoutineName));
                                    ShowContinueError(state,
                                                      format("Surface={}, Space={}, Zone={}",
                                                             surf.Name,
                                                             state.dataHeatBal->space(surf.spaceNum).Name,
                                                             surf.ZoneName));
                                    ShowContinueError(state,
                                                      format("Surface={}, Space={}, Zone={}",
                                                             osSurf.Name,
                                                             state.dataHeatBal->space(osSurf.spaceNum).Name,
                                                             osSurf.ZoneName));
                                }
                            }
                            ConstrNum = surf.Construction;
                            ConstrNumFound = osSurf.Construction;

                            if (ConstrNum <= 0 || ConstrNumFound <= 0) continue;

                            auto &constr = state.dataConstruction->Construct(ConstrNum);
                            auto &osConstr = state.dataConstruction->Construct(ConstrNumFound);
                            
                            if (constr.ReverseConstructionNumLayersWarning && osConstr.ReverseConstructionNumLayersWarning)
                                continue;
                            if (constr.ReverseConstructionLayersOrderWarning && osConstr.ReverseConstructionLayersOrderWarning)
                                continue;
                            TotLay = constr.TotLayers;
                            TotLayFound = osConstr.TotLayers;
                            if (TotLay != TotLayFound) { // Different number of layers
                                // match on like Uvalues (nominal)
                                if (std::abs(state.dataHeatBal->NominalU(ConstrNum) - state.dataHeatBal->NominalU(ConstrNumFound)) > 0.001) {
                                    ShowSevereError(state,
                                                    format("{}Construction {} of interzone surface {} does not have the same number of layers as the "
                                                           "construction {} of adjacent surface {}",
                                                           RoutineName,
                                                           constr.Name,
                                                           surf.Name,
                                                           osConstr.Name,
                                                           osSurf.Name));
                                    if (!constr.ReverseConstructionNumLayersWarning || !osConstr.ReverseConstructionNumLayersWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        constr.ReverseConstructionNumLayersWarning = true;
                                        osConstr.ReverseConstructionNumLayersWarning = true;
                                    }
                                    SurfError = true;
                                }
                            } else { // Same number of layers; check for reverse layers
                                // check layers as number of layers is the same
                                izConstDiff = false;
                                // ok if same nominal U
                                CheckForReversedLayers(state, izConstDiff, ConstrNum, ConstrNumFound, TotLay);
                                if (izConstDiff &&
                                    std::abs(state.dataHeatBal->NominalU(ConstrNum) - state.dataHeatBal->NominalU(ConstrNumFound)) > 0.001) {
                                    ShowSevereError(state,
                                                    format("{}Construction {} of interzone surface {} does not have the same materials in the "
                                                           "reverse order as the construction {} of adjacent surface {}",
                                                           RoutineName,
                                                           constr.Name,
                                                           surf.Name,
                                                           osConstr.Name,
                                                           osSurf.Name));
                                    ShowContinueError(state,
                                                      "or the properties of the reversed layers are not correct due to differing layer front and "
                                                      "back side values");
                                    if (!constr.ReverseConstructionLayersOrderWarning || !osConstr.ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        constr.ReverseConstructionLayersOrderWarning = osConstr.ReverseConstructionLayersOrderWarning = true;
                                    }
                                    SurfError = true;
                                } else if (izConstDiff) {
                                    ShowWarningError(state,
                                                     format("{}Construction {} of interzone surface {} does not have the same materials in the "
                                                            "reverse order as the construction {} of adjacent surface {}",
                                                            RoutineName,
                                                            constr.Name,
                                                            surf.Name,
                                                            osConstr.Name,
                                                            osSurf.Name));
                                    ShowContinueError(state,
                                                      "or the properties of the reversed layers are not correct due to differing layer front and "
                                                      "back side values");
                                    ShowContinueError(
                                        state,
                                        format("...but Nominal U values are similar, diff=[{:.4R}] ... simulation proceeds.",
                                               std::abs(state.dataHeatBal->NominalU(ConstrNum) - state.dataHeatBal->NominalU(ConstrNumFound))));
                                    if (!izConstDiffMsg) {
                                        ShowContinueError(state,
                                                          "...if the two zones are expected to have significantly different temperatures, the proper "
                                                          "\"reverse\" construction should be created.");
                                        izConstDiffMsg = true;
                                    }
                                    if (!constr.ReverseConstructionLayersOrderWarning || !osConstr.ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        constr.ReverseConstructionLayersOrderWarning = osConstr.ReverseConstructionLayersOrderWarning = true;
                                    }
                                }
                            }

                            // If significantly different areas -- this would not be good
                            MultFound = state.dataHeatBal->Zone(osSurf.Zone).Multiplier * state.dataHeatBal->Zone(osSurf.Zone).ListMultiplier;
                            MultSurfNum = state.dataHeatBal->Zone(surf.Zone).Multiplier * state.dataHeatBal->Zone(surf.Zone).ListMultiplier;
                            if (osSurf.Area > 0.0 && 
                                std::abs((osSurf.Area * MultFound - surf.Area * MultSurfNum) / osSurf.Area * MultFound) > 0.02) { // 2% difference in areas
                                ++state.dataSurfaceGeometry->ErrCount4;
                                if (state.dataSurfaceGeometry->ErrCount4 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state,
                                                     format("{}InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:",
                                                            RoutineName));
                                    ShowContinueError(
                                                      state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual mismatches.");
                                    }
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state,
                                                     format("{}InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:",
                                                            RoutineName));

                                    if (MultFound == 1 && MultSurfNum == 1) {
                                        ShowContinueError(state, format("  Area={:.1T} in Surface={}, Zone={}", surf.Area, surf.Name, surf.ZoneName));
                                        ShowContinueError(state, format("  Area={:.1T} in Surface={}, Zone={}", osSurf.Area, osSurf.Name, osSurf.ZoneName));
                                    } else { // Show multiplier info
                                        ShowContinueError(state, format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
                                                                        surf.Area, MultSurfNum, surf.Area * MultSurfNum, surf.Name, surf.ZoneName));

                                        ShowContinueError(state, format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
                                                                        osSurf.Area, MultFound, osSurf.Area * MultFound, osSurf.Name, osSurf.ZoneName));
                                    }
                                }
                            } // if (osSurf.Area > 0.0)
                            // Check opposites Azimuth and Tilt
                            // Tilt
                            if (std::abs(std::abs(osSurf.Tilt + surf.Tilt) - 180.0) > 1.0) {
                                ShowWarningError(state, format("{}InterZone Surface Tilts do not match as expected.", RoutineName));
                                ShowContinueError(state, format("  Tilt={:.1T} in Surface={}, Zone={}", surf.Tilt, surf.Name, surf.ZoneName));
                                ShowContinueError(state, format("  Tilt={:.1T} in Surface={}, Zone={}", osSurf.Tilt, osSurf.Name, osSurf.ZoneName));
                            }
                            // check surface class match.  interzone surface.

                            if ((surf.Class == SurfaceClass::Wall && osSurf.Class != SurfaceClass::Wall) ||
                                (surf.Class != SurfaceClass::Wall && osSurf.Class == SurfaceClass::Wall)) {
                                ShowWarningError(state, format("{}InterZone Surface Classes do not match as expected.", RoutineName));
                                ShowContinueError(state, format("Surface=\"{}\", surface class={}", surf.Name, surfaceClassStrings[(int)surf.Class]));
                                ShowContinueError(state, format("Adjacent Surface=\"{}\", surface class={}", osSurf.Name, surfaceClassStrings[(int)osSurf.Class]));
                                ShowContinueError(state, "Other errors/warnings may follow about these surfaces.");
                            }
                            if ((surf.Class == SurfaceClass::Roof && osSurf.Class != SurfaceClass::Floor) ||
                                (surf.Class != SurfaceClass::Roof && osSurf.Class == SurfaceClass::Floor)) {
                                ShowWarningError(state, format("{}InterZone Surface Classes do not match as expected.", RoutineName));
                                ShowContinueError(state, format("Surface=\"{}\", surface class={}", surf.Name, surfaceClassStrings[(int)surf.Class]));
                                ShowContinueError(state, format("Adjacent Surface=\"{}\", surface class={}", osSurf.Name, surfaceClassStrings[(int)osSurf.Class]));
                                ShowContinueError(state, "Other errors/warnings may follow about these surfaces.");
                            }
                            if (surf.Class != SurfaceClass::Roof && surf.Class != SurfaceClass::Floor) {
                                // Walls, Windows, Doors, Glass Doors
                                if (surf.Class != SurfaceClass::Wall) {
                                    // Surface is a Door, Window or Glass Door
                                    if (surf.BaseSurf == 0) continue; // error detected elsewhere
                                    if (state.dataSurface->Surface(surf.BaseSurf).Class == SurfaceClass::Roof ||
                                        state.dataSurface->Surface(surf.BaseSurf).Class == SurfaceClass::Floor)
                                        continue;
                                }
                                if (std::abs(std::abs(surf.Azimuth - osSurf.Azimuth) - 180.0) > 1.0) {
                                    if (std::abs(surf.SinTilt) > 0.5 || state.dataGlobal->DisplayExtraWarnings) {
                                        // if horizontal surfaces, then these are windows/doors/etc in those items.
                                        ShowWarningError(state, format("{}InterZone Surface Azimuths do not match as expected.", RoutineName));
                                        ShowContinueError(state, format("  Azimuth={:.1T}, Tilt={:.1T}, in Surface={}, Zone={}",
                                                                        surf.Azimuth, surf.Tilt, surf.Name, surf.ZoneName));
                                        ShowContinueError(state, format("  Azimuth={:.1T}, Tilt={:.1T}, in Surface={}, Zone={}",
                                                                        osSurf.Azimuth, osSurf.Tilt, osSurf.Name, osSurf.ZoneName));
                                        ShowContinueError(state, format("..surface class of first surface={}", surfaceClassStrings[(int)surf.Class]));
                                        ShowContinueError(state, format("..surface class of second surface={}", surfaceClassStrings[(int)osSurf.Class]));
                                    }
                                }
                            }

                            // Make sure exposures (Sun, Wind) are the same.....and are "not"
                            if (surf.ExtSolar || osSurf.ExtSolar) {
                                ShowWarningError(state, format("{}Interzone surfaces cannot be \"SunExposed\" -- removing SunExposed", RoutineName));
                                ShowContinueError(state, format("  Surface={}, Zone={}", surf.Name, surf.ZoneName));
                                ShowContinueError(state, format("  Surface={}, Zone={}", osSurf.Name, osSurf.ZoneName));
                                surf.ExtSolar = false;
                                osSurf.ExtSolar = false;
                            }
                            if (surf.ExtWind || osSurf.ExtWind) {
                                ShowWarningError(state, format("{}Interzone surfaces cannot be \"WindExposed\" -- removing WindExposed", RoutineName));
                                ShowContinueError(state, format("  Surface={}, Zone={}", surf.Name, surf.ZoneName));
                                ShowContinueError(state, format("  Surface={}, Zone={}", osSurf.Name, osSurf.ZoneName));
                                surf.ExtWind = false;
                                osSurf.ExtWind = false;
                            }
                        }
                        // Set opposing surface back to this one (regardless of error)
                        osSurf.ExtBoundCond = SurfNum;
                        // Check subsurfaces...  make sure base surface is also an interzone surface
                        if (surf.BaseSurf != SurfNum) { // Subsurface
                            if ((surf.ExtBoundCond != SurfNum) && not_blank(surf.ExtBoundCondName)) {
                                // if not internal subsurface
                                if (state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond == surf.BaseSurf) {
                                    // base surface is not interzone surface
                                    ShowSevereError(state, format("{}SubSurface=\"{}\" is an interzone subsurface.", RoutineName, surf.Name));
                                    ShowContinueError(state, format("..but the Base Surface is not an interzone surface, Surface=\"{}\".",
                                                                    state.dataSurface->Surface(surf.BaseSurf).Name));
                                    SurfError = true;
                                }
                            }
                        }
                    } else {
                        //  Seems unlikely that an internal surface would be missing itself, so this message
                        //  only indicates for adjacent (interzone) surfaces.
                        ShowSevereError(state, format("{}Adjacent Surface not found: {} adjacent to surface {}",
                                               RoutineName, surf.ExtBoundCondName, surf.Name));
                        NonMatch = true;
                        SurfError = true;
                    }
                } else if (surf.BaseSurf != SurfNum) { // Subsurface
                    if (state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond > 0 &&
                        state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond != surf.BaseSurf) { // If Interzone surface, subsurface must be also.
                        ShowSevereError(state, format("{}SubSurface on Interzone Surface must be an Interzone SubSurface.", RoutineName));
                        ShowContinueError(state, format("...OutsideFaceEnvironment is blank, in Surface={}", surf.Name));
                        SurfError = true;
                    } else {
                        ++state.dataSurfaceGeometry->ErrCount3;
                        if (state.dataSurfaceGeometry->ErrCount3 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, format("{}Blank name for Outside Boundary Condition Objects.", RoutineName));
                            ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, format("{}Blank name for Outside Boundary Condition Object, in surface={}", RoutineName, surf.Name));
                            ShowContinueError(state, format("Resetting this surface to be an internal zone surface, zone={}", surf.ZoneName));
                        }
                        surf.ExtBoundCondName = surf.Name;
                        surf.ExtBoundCond = SurfNum;
                    }
                } else {
                    ++state.dataSurfaceGeometry->ErrCount3;
                    if (state.dataSurfaceGeometry->ErrCount3 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                        ShowSevereError(state, format("{}Blank name for Outside Boundary Condition Objects.", RoutineName));
                        ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state, format("{}Blank name for Outside Boundary Condition Object, in surface={}", RoutineName, surf.Name));
                        ShowContinueError(state, format("Resetting this surface to be an internal zone (adiabatic) surface, zone={}", surf.ZoneName));
                    }
                    surf.ExtBoundCondName = surf.Name;
                    surf.ExtBoundCond = SurfNum;
                    SurfError = true;
                }
            }

        } // ...end of the Surface DO loop for finding BaseSurf
        if (NonMatch) {
            ShowSevereError(state, format("{}Non matching interzone surfaces found", RoutineName));
        }

        //**********************************************************************************
        // Warn about interzone surfaces that have adiabatic windows/vice versa
        SubSurfaceSevereDisplayed = false;
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto const &surf = state.dataSurface->Surface(SurfNum);
            if (!surf.HeatTransSurf) continue;
            if (surf.BaseSurf == SurfNum) continue; // base surface
            // not base surface.  Check it.
            if (state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond <= 0) { // exterior or other base surface
                if (surf.ExtBoundCond != state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond) { // should match base surface
                    if (surf.ExtBoundCond == SurfNum) {
                        ShowSevereError(
                            state,
                            format("{}Subsurface=\"{}\" exterior condition [adiabatic surface] in a base surface=\"{}\" with exterior condition [{}]",
                                   RoutineName,
                                   surf.Name,
                                   state.dataSurface->Surface(surf.BaseSurf).Name,
                                   cExtBoundCondition(state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond)));
                        SurfError = true;
                    } else if (surf.ExtBoundCond > 0) {
                        ShowSevereError(
                            state,
                            format("{}Subsurface=\"{}\" exterior condition [interzone surface] in a base surface=\"{}\" with exterior condition [{}]",
                                   RoutineName,
                                   surf.Name,
                                   state.dataSurface->Surface(surf.BaseSurf).Name,
                                   cExtBoundCondition(state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond)));
                        SurfError = true;
                    } else if (state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond == OtherSideCondModeledExt) {
                        ShowWarningError(
                            state,
                            format("{}Subsurface=\"{}\" exterior condition [{}] in a base surface=\"{}\" with exterior condition [{}]",
                                   RoutineName,
                                   surf.Name,
                                   cExtBoundCondition(surf.ExtBoundCond),
                                   state.dataSurface->Surface(surf.BaseSurf).Name,
                                   cExtBoundCondition(state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond)));
                        ShowContinueError(state, "...SubSurface will not use the exterior condition model of the base surface.");
                    } else {
                        ShowSevereError(
                            state,
                            format("{}Subsurface=\"{}\" exterior condition [{}] in a base surface=\"{}\" with exterior condition [{}]",
                                   RoutineName,
                                   surf.Name,
                                   cExtBoundCondition(surf.ExtBoundCond),
                                   state.dataSurface->Surface(surf.BaseSurf).Name,
                                   cExtBoundCondition(state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond)));
                        SurfError = true;
                    }
                    if (!SubSurfaceSevereDisplayed && SurfError) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                }
            } else if (state.dataSurface->Surface(surf.BaseSurf).BaseSurf == state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond) {
                // adiabatic surface. make sure subsurfaces match
                if (surf.ExtBoundCond != SurfNum) { // not adiabatic surface
                    if (surf.ExtBoundCond > 0) {
                        ShowSevereError(state,
                                        format("{}Subsurface=\"{}\" exterior condition [interzone surface] in a base surface=\"{}\" with exterior "
                                               "condition [adiabatic surface]",
                                               RoutineName,
                                               surf.Name,
                                               state.dataSurface->Surface(surf.BaseSurf).Name));
                    } else {
                        ShowSevereError(
                            state,
                            format("{}Subsurface=\"{}\" exterior condition [{}] in a base surface=\"{}\" with exterior condition [adiabatic surface]",
                                   RoutineName,
                                   surf.Name,
                                   cExtBoundCondition(surf.ExtBoundCond),
                                   state.dataSurface->Surface(surf.BaseSurf).Name));
                    }
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    SurfError = true;
                }
            } else if (state.dataSurface->Surface(surf.BaseSurf).ExtBoundCond > 0) { // interzone surface
                if (surf.ExtBoundCond == SurfNum) {
                    ShowSevereError(state,
                                    format("{}Subsurface=\"{}\" is an adiabatic surface in an Interzone base surface=\"{}\"",
                                           RoutineName,
                                           surf.Name,
                                           state.dataSurface->Surface(surf.BaseSurf).Name));
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    //        SurfError=.TRUE.
                }
            }
        } // for (SurfNum)

        setSurfaceFirstLast(state);

        // Set up Floor Areas for Zones and Spaces
        Real64 constexpr floorAreaTolerance(0.05);
        Real64 constexpr floorAreaPercentTolerance(floorAreaTolerance * 100.0);
        if (!SurfError) {
            for (auto &thisZone : state.dataHeatBal->Zone) {
                for (int spaceNum : thisZone.spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        auto &thisSurf = state.dataSurface->Surface(SurfNum);
                        if (thisSurf.Class == SurfaceClass::Floor) {
                            thisZone.HasFloor = true;
                            thisSpace.hasFloor = true;
                            thisSpace.calcFloorArea += thisSurf.Area;
                        }
                        if (thisSurf.Class == SurfaceClass::Roof) {
                            thisZone.CeilingArea += thisSurf.Area;
                            thisZone.HasRoof = true;
                        }
                    }
                }
            }
            ErrCount = 0;
            for (auto &thisSpace : state.dataHeatBal->space) {
                if (thisSpace.userEnteredFloorArea == Constant::AutoCalculate) {
                    thisSpace.FloorArea = thisSpace.calcFloorArea;
                    continue;
                }
                
                // Check entered vs calculated
                if (thisSpace.userEnteredFloorArea == 0.0) { // User entered Space floor area,
                    continue;
                }
                
                // produce message if not near calculated
                if (thisSpace.calcFloorArea > 0.0) {
                    Real64 diffp = std::abs(thisSpace.calcFloorArea - thisSpace.userEnteredFloorArea) / thisSpace.userEnteredFloorArea;
                    if (diffp > floorAreaTolerance) {
                        ++ErrCount;
                        if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(
                                             state,
                                             format("{}Entered Space Floor Area(s) differ more than {:.0R}% from calculated Space Floor Area(s).",
                                                    RoutineName,
                                                    floorAreaPercentTolerance));
                            ShowContinueError(state,
                                              "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual Spaces.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            // Warn user of using specified Space Floor Area
                            ShowWarningError(
                                             state,
                                             format("{}Entered Floor Area for Space=\"{}\" is {:.1R}% different from the calculated Floor Area.",
                                                    RoutineName,
                                                    thisSpace.Name,
                                                    diffp * 100.0));
                            ShowContinueError(state,
                                              format("Entered Space Floor Area={:.2R}, Calculated Space Floor Area={:.2R}, entered "
                                                     "Floor Area will be used.",
                                                     thisSpace.userEnteredFloorArea,
                                                     thisSpace.calcFloorArea));
                        }
                    }
                }
                thisSpace.FloorArea = thisSpace.userEnteredFloorArea;
                thisSpace.hasFloor = true;
            }

            ErrCount = 0;
            for (auto &thisZone : state.dataHeatBal->Zone) {
                // Calculate zone floor area as sum of space floor areas
                for (int spaceNum : thisZone.spaceIndexes) {
                    thisZone.CalcFloorArea += state.dataHeatBal->space(spaceNum).FloorArea;
                    thisZone.HasFloor |= state.dataHeatBal->space(spaceNum).hasFloor;
                }
                if (thisZone.UserEnteredFloorArea != Constant::AutoCalculate) {
                    // Check entered vs calculated
                    if (thisZone.UserEnteredFloorArea > 0.0) { // User entered zone floor area,
                        // produce message if not near calculated
                        if (thisZone.CalcFloorArea > 0.0) {
                            Real64 diffp = std::abs(thisZone.CalcFloorArea - thisZone.UserEnteredFloorArea) / thisZone.UserEnteredFloorArea;
                            if (diffp > 0.05) {
                                ++ErrCount;
                                if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(
                                        state,
                                        format("{}Entered Zone Floor Area(s) differ more than {:.0R}% from the sum of the Space Floor Area(s).",
                                               RoutineName,
                                               floorAreaPercentTolerance));
                                    ShowContinueError(state,
                                                      "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                                }
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    // Warn user of using specified Zone Floor Area
                                    ShowWarningError(state,
                                                     format("{}Entered Floor Area for Zone=\"{}\" is {:.1R}% different from the sum of the "
                                                            "Space Floor Area(s).",
                                                            RoutineName,
                                                            thisZone.Name,
                                                            diffp * 100.0));
                                    ShowContinueError(state,
                                                      format("Entered Zone Floor Area={:.2R}, Sum of Space Floor Area(s)={:.2R}",
                                                             thisZone.UserEnteredFloorArea,
                                                             thisZone.CalcFloorArea));
                                    ShowContinueError(
                                        state, "Entered Zone Floor Area will be used and Space Floor Area(s) will be adjusted proportionately.");
                                }
                            }
                        }
                        thisZone.FloorArea = thisZone.UserEnteredFloorArea;
                        thisZone.HasFloor = true;

                        // Adjust space floor areas to match zone floor area
                        if (thisZone.numSpaces == 1) {
                            // If the zone contains only one space, then set the Space area to the Zone area
                            int spaceNum = thisZone.spaceIndexes(1);
                            state.dataHeatBal->space(spaceNum).FloorArea = thisZone.FloorArea;
                        } else if (thisZone.CalcFloorArea > 0.0) {
                            // Adjust space areas proportionately
                            Real64 areaRatio = thisZone.FloorArea / thisZone.CalcFloorArea;
                            for (int spaceNum : thisZone.spaceIndexes) {
                                state.dataHeatBal->space(spaceNum).FloorArea *= areaRatio;
                            }
                        } else {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                // Warn if calculated floor area was zero and there is more than one Space
                                ShowWarningError(
                                    state,
                                    format("{}Entered Floor Area entered for Zone=\"{}\" significantly different from sum of Space Floor Areas",
                                           RoutineName,
                                           thisZone.Name));
                                ShowContinueError(state,
                                                  "But the sum of the Space Floor Areas is zero and there is more than one Space in the zone."
                                                  "Unable to apportion the zone floor area. Space Floor Areas are zero.");
                            }
                        }
                    } else {
                        if (thisZone.CalcFloorArea > 0.0) thisZone.FloorArea = thisZone.CalcFloorArea;
                    }
                } else {
                    thisZone.FloorArea = thisZone.CalcFloorArea;
                }
                Real64 totSpacesFloorArea = 0.0;
                for (int spaceNum : thisZone.spaceIndexes) {
                    totSpacesFloorArea += state.dataHeatBal->space(spaceNum).FloorArea;
                }
                if (totSpacesFloorArea > 0.0) {
                    for (int spaceNum : thisZone.spaceIndexes) {
                        state.dataHeatBal->space(spaceNum).fracZoneFloorArea = state.dataHeatBal->space(spaceNum).FloorArea / totSpacesFloorArea;
                    }
                } // else leave fractions at zero
            }
        } // if (!SurfError)

        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            auto const &surf = state.dataSurface->Surface(SurfNum);
            if (surf.Area < 1.e-06) {
                ShowSevereError(state, format("{}Zero or negative surface area[{:.5R}], Surface={}", RoutineName,surf.Area, surf.Name));
                SurfError = true;
            }
            if (state.dataSurface->Surface(SurfNum).Area >= 1.e-06 && state.dataSurface->Surface(SurfNum).Area < 0.001) {
                ShowWarningError(state, format("{}Very small surface area[{:.5R}], Surface={}", RoutineName, surf.Area, surf.Name));
            }
        }

        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            auto &surf = state.dataSurface->Surface(SurfNum);
            // GLASSDOORs and TDD:DIFFUSERs will be treated as windows in the subsequent heat transfer and daylighting
            // calculations. Reset class to 'Window' after saving the original designation in SurfaceWindow.

            surf.OriginalClass = surf.Class;

            if (surf.Class == SurfaceClass::GlassDoor || surf.Class == SurfaceClass::TDD_Diffuser) surf.Class = SurfaceClass::Window;

            if (surf.Class == SurfaceClass::TDD_Dome) {
                // Reset the TDD:DOME subsurface to act as a base surface that can shade and be shaded
                // NOTE: This must be set early so that subsequent shading calculations are done correctly
                surf.BaseSurf = SurfNum;
            }
        }

        errFlag = false;
        if (!SurfError) {
            for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
                auto &surf = state.dataSurface->Surface(SurfNum);
                if (surf.HasShadeControl) {
                    WinShadingControlPtr = surf.activeWindowShadingControl; // use first item since others should be identical
                    if (state.dataSurface->WindowShadingControl(WinShadingControlPtr).slatAngleControl != SlatAngleControl::Fixed) {
                        state.dataSurface->SurfWinMovableSlats(SurfNum) = true;
                        state.dataSurface->AnyMovableSlat = true;
                        state.dataHeatBalSurf->SurfMovSlatsIndexList.push_back(SurfNum);
                    }

                    ConstrNumSh = surf.activeShadedConstruction;
                    if (ConstrNumSh <= 0) continue;

                    WinShadingType ShadingType = state.dataSurface->WindowShadingControl(WinShadingControlPtr).ShadingType;

                    // only for blinds
                    if (ANY_BLIND(ShadingType)) {

                        // TH 1/7/2010. CR 7930
                        // The old code did not consider between-glass blind. Also there should not be two blinds - both interior and exterior
                        // Use the new generic code (assuming only one blind) as follows
                        for (iTmp1 = 1; iTmp1 <= state.dataConstruction->Construct(ConstrNumSh).TotLayers; ++iTmp1) {
                            iTmp2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(iTmp1);
                            auto *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(iTmp2));
                            assert(thisMaterial != nullptr);
                            if (thisMaterial->group == Material::Group::WindowBlind) {
                                BlNum = thisMaterial->BlindDataPtr;
                                state.dataSurface->SurfWinBlindNumber(SurfNum) = BlNum;
                                // TH 2/18/2010. CR 8010
                                // if it is a blind with movable slats, create one new blind and set it to VariableSlat if not done so yet.
                                //  the new blind is created only once, it can be shared by multiple windows though.
                                if (state.dataSurface->SurfWinMovableSlats(SurfNum) &&
                                    state.dataMaterial->Blind(BlNum).SlatAngleType != DataWindowEquivalentLayer::AngleType::Variable) {
                                    errFlag = false;
                                    AddVariableSlatBlind(state, BlNum, BlNumNew, errFlag);
                                    // point to the new blind
                                    thisMaterial->BlindDataPtr = BlNumNew;
                                    // window surface points to new blind
                                    state.dataSurface->SurfWinBlindNumber(SurfNum) = BlNumNew;
                                }
                                break;
                            }
                        }

                        if (errFlag) {
                            ErrorsFound = true;
                            ShowContinueError(state,
                                              format("WindowShadingControl {} has errors, program will terminate.",
                                                     state.dataSurface->WindowShadingControl(WinShadingControlPtr).Name));
                        }
                    }
                } // End of surface loop

                // final associate fenestration surfaces referenced in WindowShadingControl
                FinalAssociateWindowShadingControlFenestration(state, ErrorsFound);
                CheckWindowShadingControlSimilarForWindow(state, ErrorsFound);
            }

            // Check for zones with not enough surfaces
            for (auto &thisZone : state.dataHeatBal->Zone) {
                int OpaqueHTSurfs = 0;        // Number of floors, walls and roofs in a zone
                int OpaqueHTSurfsWithWin = 0; // Number of floors, walls and roofs with windows in a zone
                int InternalMassSurfs = 0;    // Number of internal mass surfaces in a zone
                int priorBaseSurfNum = 0;

                for (int spaceNum : thisZone.spaceIndexes) {
                    auto &thisSpace = state.dataHeatBal->space(spaceNum);
                    if (thisSpace.HTSurfaceFirst == 0) continue; // Zone with no surfaces
                    for (int SurfNum = thisSpace.HTSurfaceFirst; SurfNum <= thisSpace.HTSurfaceLast; ++SurfNum) {
                        auto &thisSurf = state.dataSurface->Surface(SurfNum);
                        if (thisSurf.Class == SurfaceClass::Floor || thisSurf.Class == SurfaceClass::Wall || thisSurf.Class == SurfaceClass::Roof)
                            ++OpaqueHTSurfs;
                        if (thisSurf.Class == SurfaceClass::IntMass) ++InternalMassSurfs;
                        if (thisSurf.Class == SurfaceClass::Window) {
                            // Count base surface only once for multiple windows on a wall
                            int thisBaseSurfNum = thisSurf.BaseSurf;
                            if (thisBaseSurfNum != priorBaseSurfNum) {
                                ++OpaqueHTSurfsWithWin;
                                priorBaseSurfNum = thisBaseSurfNum;
                            }
                        }
                    }
                }
                if (OpaqueHTSurfsWithWin == 1 && OpaqueHTSurfs == 1 && InternalMassSurfs == 0) {
                    SurfError = true;
                    ShowSevereError(state,
                                    format("{}Zone {} has only one floor, wall or roof, and this surface has a window.", RoutineName, thisZone.Name));
                    ShowContinueError(state, "Add more floors, walls or roofs, or an internal mass surface.");
                }
            }

            // set up vertex of centroid for each surface.
            CalcSurfaceCentroid(state);

            SetupShadeSurfacesForSolarCalcs(state); // if shading surfaces are solar collectors or PV, then we need full solar calc.

            GetMovableInsulationData(state, ErrorsFound);

            if (state.dataSurface->CalcSolRefl) GetShadingSurfReflectanceData(state, ErrorsFound);

            LayNumOutside = 0;
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                auto &surf = state.dataSurface->Surface(SurfNum);
                // Check for EcoRoof and only 1 allowed to be used.
                if (surf.Construction > 0)
                    state.dataSurface->SurfExtEcoRoof(SurfNum) = state.dataConstruction->Construct(surf.Construction).TypeIsEcoRoof;
                if (!state.dataSurface->SurfExtEcoRoof(SurfNum)) continue;
                if (LayNumOutside == 0) {
                    LayNumOutside = state.dataConstruction->Construct(surf.Construction).LayerPoint(1);
                    continue;
                }
                if (LayNumOutside != state.dataConstruction->Construct(surf.Construction).LayerPoint(1)) {
                    ShowSevereError(state, format("{}Only one EcoRoof Material is currently allowed for all constructions.", RoutineName));
                    ShowContinueError(state, format("... first material={}", state.dataMaterial->Material(LayNumOutside)->Name));
                    ShowContinueError(state,
                                      format("... conflicting Construction={} uses material={}",
                                             state.dataConstruction->Construct(surf.Construction).Name,
                                             state.dataMaterial->Material(state.dataConstruction->Construct(surf.Construction).LayerPoint(1))->Name));
                    ErrorsFound = true;
                }
            }

            // Reserve space to avoid excess allocations
            state.dataSurface->AllHTSurfaceList.reserve(state.dataSurface->TotSurfaces);
            state.dataSurface->AllExtSolarSurfaceList.reserve(state.dataSurface->TotSurfaces);
            state.dataSurface->AllShadowPossObstrSurfaceList.reserve(state.dataSurface->TotSurfaces);
            state.dataSurface->AllIZSurfaceList.reserve(state.dataSurface->TotSurfaces);
            state.dataSurface->AllHTNonWindowSurfaceList.reserve(state.dataSurface->TotSurfaces - state.dataSurface->TotWindows);
            state.dataSurface->AllHTWindowSurfaceList.reserve(state.dataSurface->TotWindows);
            state.dataSurface->AllExtSolWindowSurfaceList.reserve(state.dataSurface->TotWindows);
            state.dataSurface->AllExtSolWinWithFrameSurfaceList.reserve(state.dataSurface->TotWindows);
            state.dataSurface->AllHTKivaSurfaceList.reserve(state.dataSurface->TotSurfaces);

            // Set flag that determines whether a surface can be an exterior obstruction
            // Also set associated surfaces for Kiva foundations and build heat transfer surface lists
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                auto &surf = state.dataSurface->Surface(SurfNum);
                surf.IsShadowPossibleObstruction = false;
                if (surf.ExtSolar) {
                    // This may include some attached shading surfaces
                    state.dataSurface->AllExtSolarSurfaceList.push_back(SurfNum);
                }
                if (surf.HeatTransSurf) {
                    // Outside light shelves get tagged later as HeatTransSurf=true but they haven't been processed yet
                    state.dataSurface->AllHTSurfaceList.push_back(SurfNum);
                    int const zoneNum(surf.Zone);
                    auto &surfZone(state.dataHeatBal->Zone(zoneNum));
                    surfZone.ZoneHTSurfaceList.push_back(SurfNum);
                    // Sort window vs non-window surfaces
                    if (surf.Class == DataSurfaces::SurfaceClass::Window) {
                        state.dataSurface->AllHTWindowSurfaceList.push_back(SurfNum);
                        surfZone.ZoneHTWindowSurfaceList.push_back(SurfNum);
                        if (surf.ExtSolar) {
                            state.dataSurface->AllExtSolWindowSurfaceList.push_back(SurfNum);
                            if (surf.FrameDivider > 0) {
                                state.dataSurface->AllExtSolWinWithFrameSurfaceList.push_back(SurfNum);
                            }
                        }
                    } else {
                        state.dataSurface->AllHTNonWindowSurfaceList.push_back(SurfNum);
                        surfZone.ZoneHTNonWindowSurfaceList.push_back(SurfNum);
                    }
                    int const surfExtBoundCond(surf.ExtBoundCond);
                    // Build zone and interzone surface lists
                    if ((surfExtBoundCond > 0) && (surfExtBoundCond != SurfNum)) {
                        state.dataSurface->AllIZSurfaceList.push_back(SurfNum);
                        surfZone.ZoneIZSurfaceList.push_back(SurfNum);
                        auto &adjZone(state.dataHeatBal->Zone(state.dataSurface->Surface(surfExtBoundCond).Zone));
                        adjZone.ZoneHTSurfaceList.push_back(SurfNum);
                        adjZone.ZoneIZSurfaceList.push_back(SurfNum);
                        // Sort window vs non-window surfaces
                        if (surf.Class == DataSurfaces::SurfaceClass::Window) {
                            adjZone.ZoneHTWindowSurfaceList.push_back(SurfNum);
                        } else {
                            adjZone.ZoneHTNonWindowSurfaceList.push_back(SurfNum);
                        }
                    }
                }

                // Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
                if (surf.HeatTransSurf && surf.ExtBoundCond > 0) continue;
                if (surf.HeatTransSurf && surf.ExtBoundCond == Ground) continue;
                if (surf.HeatTransSurf && surf.ExtBoundCond == KivaFoundation) {
                    state.dataSurface->AllHTKivaSurfaceList.push_back(SurfNum);
                    if (!ErrorsFound) state.dataSurfaceGeometry->kivaManager.foundationInputs[surf.OSCPtr].surfaces.push_back(SurfNum);
                    continue;
                }
                if (surf.HeatTransSurf && surf.ExtBoundCond == OtherSideCoefNoCalcExt) continue;
                if (surf.HeatTransSurf && surf.ExtBoundCond == OtherSideCoefCalcExt) continue;
                // Exclude windows and doors, i.e., consider only their base surfaces as possible obstructions
                if (surf.Class == SurfaceClass::Window || surf.Class == SurfaceClass::Door) continue;
                // Exclude duplicate shading surfaces
                if (surf.MirroredSurf) continue;
                // Exclude air boundary surfaces
                if (surf.IsAirBoundarySurf) continue;

                surf.IsShadowPossibleObstruction = true;
                state.dataSurface->AllShadowPossObstrSurfaceList.push_back(SurfNum);
            }

            // Check for IRT surfaces in invalid places.
            iTmp1 = 0;
            if (std::any_of(state.dataConstruction->Construct.begin(),
                            state.dataConstruction->Construct.end(),
                            [](Construction::ConstructionProps const &e) { return e.TypeIsIRT; })) {
                for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                    auto &surf = state.dataSurface->Surface(SurfNum);
                    if (!surf.HeatTransSurf) continue;                                   // ignore shading surfaces
                    if (surf.ExtBoundCond > 0 && surf.ExtBoundCond != SurfNum) continue; // interzone, not adiabatic surface
                    if (!state.dataConstruction->Construct(surf.Construction).TypeIsIRT) {
                        continue;
                    }
                    if (!state.dataGlobal->DisplayExtraWarnings) {
                        ++iTmp1;
                    } else {
                        ShowWarningError(state,
                                         format("{}Surface=\"{}\" uses InfraredTransparent construction in a non-interzone surface. (illegal use)",
                                                RoutineName,
                                                surf.Name));
                    }
                }
                if (iTmp1 > 0) {
                    ShowWarningError(
                        state,
                        format("{}Surfaces use InfraredTransparent constructions {} in non-interzone surfaces. (illegal use)", RoutineName, iTmp1));
                    ShowContinueError(state, "For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
                }
            }

            // Populate SurfaceFilter lists
            for (int iSurfaceFilter = 1; iSurfaceFilter < (int)SurfaceFilter::Num; ++iSurfaceFilter)
                state.dataSurface->SurfaceFilterLists[iSurfaceFilter].reserve(state.dataSurface->TotSurfaces);

            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                auto const &surf = state.dataSurface->Surface(SurfNum);
                if (!surf.HeatTransSurf) continue;
                if (surf.ExtBoundCond > 0) {
                    state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorSurfaces].push_back(SurfNum);
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorWindows].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Wall) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorWalls].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Floor) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorFloors].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Roof) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorRoofs].push_back(SurfNum);
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorCeilings].push_back(SurfNum);
                    }
                } else {
                    state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllExteriorSurfaces].push_back(SurfNum);
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllExteriorWindows].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Wall) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllExteriorWalls].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Floor) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllExteriorFloors].push_back(SurfNum);
                    } else if (surf.Class == SurfaceClass::Roof) {
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllExteriorRoofs].push_back(SurfNum);
                        state.dataSurface->SurfaceFilterLists[(int)SurfaceFilter::AllInteriorCeilings].push_back(SurfNum);
                    }
                }
            }

            // Note, could do same for Window Area and detecting if Interzone Surface in Zone

            if (state.dataSurfaceGeometry->Warning1Count > 0) {
                ShowWarningMessage(state,
                                   format("{}Window dimensions differ from Window 5/6 data file dimensions, {} times.",
                                          RoutineName,
                                          state.dataSurfaceGeometry->Warning1Count));
                ShowContinueError(state, "This will affect the frame heat transfer calculation if the frame in the Data File entry");
                ShowContinueError(state, "is not uniform, i.e., has sections with different geometry and/or thermal properties.");
                ShowContinueError(state, "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
            }
            if (state.dataSurfaceGeometry->Warning2Count > 0) {
                ShowWarningMessage(state,
                                   format("{}Exterior Windows have been replaced with Window 5/6 two glazing systems, {} times.",
                                          RoutineName,
                                          state.dataSurfaceGeometry->Warning2Count));
                ShowContinueError(state, "Note that originally entered dimensions are overridden.");
                ShowContinueError(state, "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
            }
            if (state.dataSurfaceGeometry->Warning3Count > 0) {
                ShowWarningMessage(state,
                                   format("{}Interior Windows have been replaced with Window 5/6 two glazing systems, {} times.",
                                          RoutineName,
                                          state.dataSurfaceGeometry->Warning3Count));
                ShowContinueError(state, "Note that originally entered dimensions are overridden.");
                ShowContinueError(state, "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
            }

            if (state.dataErrTracking->TotalMultipliedWindows > 0) {
                ShowWarningMessage(state,
                                   format("{}There are {} window/glass door(s) that may cause inaccurate shadowing due to Solar Distribution.",
                                          RoutineName,
                                          state.dataErrTracking->TotalMultipliedWindows));
                ShowContinueError(state, "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
                state.dataErrTracking->TotalWarningErrors += state.dataErrTracking->TotalMultipliedWindows;
            }
            if (state.dataErrTracking->TotalCoincidentVertices > 0) {
                ShowWarningMessage(state,
                                   format("{}There are {} coincident/collinear vertices; These have been deleted unless the deletion would bring the "
                                          "number of surface sides < 3.",
                                          RoutineName,
                                          state.dataErrTracking->TotalCoincidentVertices));
                ShowContinueError(state, "For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;");
                state.dataErrTracking->TotalWarningErrors += state.dataErrTracking->TotalCoincidentVertices;
            }
            if (state.dataErrTracking->TotalDegenerateSurfaces > 0) {
                ShowSevereMessage(state,
                                  format("{}There are {} degenerate surfaces; Degenerate surfaces are those with number of sides < 3.",
                                         RoutineName,
                                         state.dataErrTracking->TotalDegenerateSurfaces));
                ShowContinueError(state, "These surfaces should be deleted.");
                ShowContinueError(state, "For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;");
                state.dataErrTracking->TotalSevereErrors += state.dataErrTracking->TotalDegenerateSurfaces;
            }

            GetHTSurfExtVentedCavityData(state, ErrorsFound);

            state.dataSurfaceGeometry->exposedFoundationPerimeter.getData(state, ErrorsFound);

            GetSurfaceHeatTransferAlgorithmOverrides(state, ErrorsFound);

            // Set up enclosures, process Air Boundaries if any
            SetupEnclosuresAndAirBoundaries(state, state.dataViewFactor->EnclRadInfo, SurfaceGeometry::enclosureType::RadiantEnclosures, ErrorsFound);

            GetSurfaceGroundSurfsData(state, ErrorsFound);

            GetSurfaceSrdSurfsData(state, ErrorsFound);

            GetSurfaceLocalEnvData(state, ErrorsFound);

            if (SurfError || ErrorsFound) {
                ErrorsFound = true;
                ShowFatalError(state, format("{}Errors discovered, program terminates.", RoutineName));
            }

            int TotShadSurf = TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg + TotShdSubs + TotOverhangs +
                              TotOverhangsProjection + TotFins + TotFinsProjection;
            int NumDElightCmplxFen = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Daylighting:DElight:ComplexFenestration");
            if (TotShadSurf > 0 && (NumDElightCmplxFen > 0 || Dayltg::doesDayLightingUseDElight(state))) {
                ShowWarningError(state,
                                 format("{}When using DElight daylighting the presence of exterior shading surfaces is ignored.", RoutineName));
            }

            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; SurfNum++) {
                auto &surf = state.dataSurface->Surface(SurfNum);
                // Initialize run time surface arrays
                state.dataSurface->SurfActiveConstruction(SurfNum) = surf.Construction;
                surf.RepresentativeCalcSurfNum = SurfNum;
            }

            // Representative surface calculations: Assign representative heat transfer surfaces
            if (state.dataSurface->UseRepresentativeSurfaceCalculations &&
                state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "ZoneProperty:UserViewFactors:BySurfaceName") == 0) {
                for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
                    for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                        auto &thisSpace = state.dataHeatBal->space(spaceNum);
                        for (int surfNum = thisSpace.HTSurfaceFirst; surfNum <= thisSpace.HTSurfaceLast; surfNum++) {
                            auto &surface(state.dataSurface->Surface(surfNum));
                            // Conditions where surface always needs to be unique
                            bool forceUniqueSurface =
                                surface.HasShadeControl ||
                                state.dataSurface->SurfWinAirflowSource(surfNum) != DataSurfaces::WindowAirFlowSource::Invalid ||
                                state.dataConstruction->Construct(surface.Construction).SourceSinkPresent ||
                                surface.Class == SurfaceClass::TDD_Dome ||
                                (surface.Class == SurfaceClass::Window &&
                                 (surface.OriginalClass == SurfaceClass::TDD_Diffuser ||
                                  state.dataSurface->SurfWinWindowModelType(surfNum) != WindowModel::Detailed ||
                                  state.dataWindowManager->inExtWindowModel->isExternalLibraryModel() ||
                                  state.dataConstruction->Construct(surface.Construction).TCFlag == 1));
                            if (!forceUniqueSurface) {
                                state.dataSurface->Surface(surfNum).set_representative_surface(state, surfNum);
                            }
                        }
                    }
                }
            }
            // Initialize surface with movable insulation index list
            for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; SurfNum++) {
                if (state.dataSurface->SurfMaterialMovInsulExt(SurfNum) > 0 || state.dataSurface->SurfMaterialMovInsulInt(SurfNum) > 0) {
                    state.dataHeatBalSurf->SurfMovInsulIndexList.push_back(SurfNum);
                }
            }
        }
        if (SurfError || ErrorsFound) {
            ErrorsFound = true;
            ShowFatalError(state, format("{}Errors discovered, program terminates.", RoutineName));
        }
    }

    void CreateMissingSpaces(EnergyPlusData &state, Array1D<SurfaceGeometry::SurfaceData> &Surfaces)
    {
        // Scan surfaces to see if Space was assigned in input
        EPVector<bool> anySurfacesWithSpace;    // True if any surfaces in a zone do not have a space assigned in input
        EPVector<bool> anySurfacesWithoutSpace; // True if any surfaces in a zone have a space assigned in input
        anySurfacesWithSpace.resize(state.dataGlobal->NumOfZones, false);
        anySurfacesWithoutSpace.resize(state.dataGlobal->NumOfZones, false);

        for (int surfNum = 1; surfNum <= state.dataSurface->TotSurfaces; ++surfNum) {
            auto &thisSurf = Surfaces(surfNum);
            if (!thisSurf.HeatTransSurf) continue; // ignore shading surfaces
            if (thisSurf.BaseSurf != surfNum) {
                // Set space for subsurfaces
                thisSurf.spaceNum = Surfaces(thisSurf.BaseSurf).spaceNum;
            }
            if (thisSurf.spaceNum > 0) {
                anySurfacesWithSpace(thisSurf.Zone) = true;
            } else {
                anySurfacesWithoutSpace(thisSurf.Zone) = true;
            }
        }

        // Create any missing Spaces
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            auto &thisZone = state.dataHeatBal->Zone(zoneNum);
            if (anySurfacesWithoutSpace(zoneNum)) {
                // If any surfaces in the zone are not assigned to a space, may need to create a new space
                // Every zone has at least one space, created in HeatBalanceManager::GetSpaceData
                // If no surfaces have a space assigned, then the default space will be used, otherwise, create a new space
                if (anySurfacesWithSpace(zoneNum)) {
                    // Add new space
                    ++state.dataGlobal->numSpaces;
                    state.dataHeatBal->space(state.dataGlobal->numSpaces).zoneNum = zoneNum;
                    // Add to zone's list of spaces
                    thisZone.spaceIndexes.emplace_back(state.dataGlobal->numSpaces);
                    ++state.dataHeatBal->Zone(zoneNum).numSpaces;
                    assert(state.dataHeatBal->Zone(zoneNum).numSpaces == int(state.dataHeatBal->Zone(zoneNum).spaceIndexes.size()));
                    // If some surfaces in the zone are assigned to a space, the new space is the remainder of the zone
                    state.dataHeatBal->space(state.dataGlobal->numSpaces).Name =
                        thisZone.Name + "-REMAINDER"; // Make UPPERcase so it can be referenced in input
                    state.dataHeatBal->space(state.dataGlobal->numSpaces).isRemainderSpace = true;
                    state.dataHeatBal->space(state.dataGlobal->numSpaces).spaceType = "GENERAL";
                    state.dataHeatBal->space(state.dataGlobal->numSpaces).spaceTypeNum = HeatBalanceManager::GetGeneralSpaceTypeNum(state);
                }
            }
        }

        // Assign Spaces to surfaces without one
        for (int surfNum = 1; surfNum <= state.dataSurface->TotSurfaces; ++surfNum) {
            auto &thisSurf = Surfaces(surfNum);
            if (!thisSurf.HeatTransSurf) continue; // ignore shading surfaces
            if (thisSurf.spaceNum == 0) {
                int const numSpaces = state.dataHeatBal->Zone(thisSurf.Zone).numSpaces;
                int const lastSpaceForZone = state.dataHeatBal->Zone(thisSurf.Zone).spaceIndexes(numSpaces);
                thisSurf.spaceNum = lastSpaceForZone;
            }
        }
    }

    void createSpaceSurfaceLists(EnergyPlusData &state)
    {
        static constexpr std::string_view RoutineName("createSpaceSurfaceLists: ");
        // Build Space surface lists now that all of the surface sorting is complete
        for (int surfNum = 1; surfNum <= state.dataSurface->TotSurfaces; ++surfNum) {
            auto &thisSurf = state.dataSurface->Surface(surfNum);
            if (!thisSurf.HeatTransSurf) continue; // ignore shading surfaces
            // Add to Space's list of surfaces
            state.dataHeatBal->space(thisSurf.spaceNum).surfaces.emplace_back(surfNum);
        }
        for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
            if (int(state.dataHeatBal->space(spaceNum).surfaces.size()) == 0) {
                ShowWarningError(state, format("{}Space={} has no surfaces.", RoutineName, state.dataHeatBal->space(spaceNum).Name));
            }
        }
    }

    void setSurfaceFirstLast(EnergyPlusData &state)
    {
        // Set Zone and Space Surface First/Last Pointers
        // Space surface lists have been built earlier in createSpaceSurfaceLists
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            for (int spaceNum : state.dataHeatBal->Zone(ZoneNum).spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                for (int SurfNum : thisSpace.surfaces) {
                    auto &surf = state.dataSurface->Surface(SurfNum);
                    if (thisSpace.AllSurfaceFirst == 0) {
                        thisSpace.AllSurfaceFirst = SurfNum;
                    }
                    thisSpace.AllSurfaceLast = SurfNum;

                    if (surf.IsAirBoundarySurf) {
                        surf.HeatTransSurf = false;
                        continue;
                    }
                    // Non window surfaces are grouped next within each space
                    if (thisSpace.HTSurfaceFirst == 0) {
                        thisSpace.HTSurfaceFirst = SurfNum;
                        thisSpace.OpaqOrIntMassSurfaceFirst = SurfNum;
                        thisSpace.OpaqOrWinSurfaceFirst = SurfNum;
                    }
                    thisSpace.HTSurfaceLast = SurfNum;

                    // Window surfaces are grouped next within each space
                    if ((surf.Class == DataSurfaces::SurfaceClass::Window) || (surf.Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                        (surf.Class == DataSurfaces::SurfaceClass::TDD_Diffuser)) {
                        if (thisSpace.WindowSurfaceFirst == 0) {
                            thisSpace.WindowSurfaceFirst = SurfNum;
                        }
                        thisSpace.WindowSurfaceLast = SurfNum;
                    } else if (surf.Class != DataSurfaces::SurfaceClass::TDD_Dome) {
                        thisSpace.OpaqOrIntMassSurfaceLast = SurfNum;
                    }

                    // TDDDome surfaces are grouped last within each space
                    if (surf.Class == DataSurfaces::SurfaceClass::TDD_Dome) {
                        if (thisSpace.TDDDomeFirst == 0) {
                            thisSpace.TDDDomeFirst = SurfNum;
                        }
                        thisSpace.TDDDomeLast = SurfNum;
                    } else {
                        thisSpace.OpaqOrWinSurfaceLast = SurfNum;
                    }
                }
                state.dataHeatBal->Zone(ZoneNum).AllSurfaceLast = thisSpace.AllSurfaceLast;
            }
            int firstSpaceNum = state.dataHeatBal->Zone(ZoneNum).spaceIndexes(1);
            state.dataHeatBal->Zone(ZoneNum).AllSurfaceFirst = state.dataHeatBal->space(firstSpaceNum).AllSurfaceFirst;
        }
    }

    void checkSubSurfAzTiltNorm(EnergyPlusData &state,
                                SurfaceData &surf, // Base surface data (in)
                                SurfaceData &subSurf,  // Subsurface data (in)
                                bool &surfaceError        // True if surface azimuths or tilts differ by more than error tolerance
    )
    {
        bool sameSurfNormal(false); // True if surface has the same surface normal within tolerance
        bool baseSurfHoriz(false);  // True if base surface is near horizontal
        Real64 constexpr warningTolerance(30.0);
        Real64 constexpr errorTolerance(90.0);

        std::string_view routineName = "checkSubSurfAzTiltNorm";
        
        surfaceError = false;

        // Check if base surface and subsurface have the same normal
        sameSurfNormal = Vectors::VecEqualTol(surf.NewellNormVec, subSurf.NewellNormVec, 0.001);
        if (sameSurfNormal) { // copy lcs vectors
                              // Prior logic tested for azimuth difference < 30 and then skipped this - this caused large diffs in
                              // CmplxGlz_MeasuredDeflectionAndShading Restoring that check here but will require further investigation (MJW Dec 2015)
                              // if (std::abs(baseSurface.Azimuth - subSurface.Azimuth) > warningTolerance) {
            subSurf.lcsx = surf.lcsx;
            subSurf.lcsy = surf.lcsy;
            subSurf.lcsz = surf.lcsz;
            return;
        }            
        // // Not sure what this does, but keeping for now (MJW Dec 2015)
        // if (std::abs(subSurface.Azimuth - 360.0) < 0.01) {
        //     subSurface.Azimuth = 360.0 - subSurface.Azimuth;
        // }
        // if (std::abs(baseSurface.Azimuth - 360.0) < 0.01) {
        //     baseSurface.Azimuth = 360.0 - baseSurface.Azimuth;
        // }

        // Is base surface horizontal? If so, ignore azimuth differences
        if (std::abs(surf.Tilt) <= 1.0e-5 || std::abs(surf.Tilt - 180.0) <= 1.0e-5) baseSurfHoriz = true;

        if (((General::rotAzmDiffDeg(surf.Azimuth, subSurf.Azimuth) > errorTolerance) && !baseSurfHoriz) ||
            (std::abs(surf.Tilt - subSurf.Tilt) > errorTolerance)) {
            surfaceError = true;
            ShowSevereError(state, format("{}: Outward facing angle of subsurface differs more than {:.1R} degrees from base surface.", routineName, errorTolerance));
            ShowContinueError(state, format("Subsurface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", subSurf.Name, subSurf.Tilt, subSurf.Azimuth));
            ShowContinueError(state, format("Base surface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", surf.Name, surf.Tilt, surf.Azimuth));
        } else if (((General::rotAzmDiffDeg(surf.Azimuth, subSurf.Azimuth) > warningTolerance) && !baseSurfHoriz) ||
                   (std::abs(surf.Tilt - subSurf.Tilt) > warningTolerance)) {
                ++state.dataSurfaceGeometry->checkSubSurfAzTiltNormErrCount;
            if (state.dataSurfaceGeometry->checkSubSurfAzTiltNormErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state, format("{}: Some Outward Facing angles of subsurfaces differ more than {:.1R} "
                                               "degrees from base surface.", routineName, warningTolerance));
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
            }
            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state, format("{}: Outward facing angle of subsurface differs more than {:.1R} degrees from base surface.",
                                               routineName, warningTolerance));
                ShowContinueError(state, format("Subsurface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", subSurf.Name, subSurf.Tilt, subSurf.Azimuth));
                ShowContinueError(state, format("Base surface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", surf.Name, surf.Tilt, surf.Azimuth));
            }
        }
    } // checkSubSurfAzTiltNorm()

    void GetGeometryParameters(EnergyPlusData &state, bool &ErrorsFound) // set to true if errors found during input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reads in the "Surface Geometry" parameters, verifies them,
        // and sets "global" variables that will tell other routines how the surface
        // vertices are expected in input.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // GlobalGeometryRules Definition
        // GlobalGeometryRules,
        //      \required-object
        //      \unique-object
        //  A1, \field Starting Vertex Position
        //      \required-field
        //      \note Specified as entry for a 4 sided surface/rectangle
        //      \note Surfaces are specified as viewed from outside the surface
        //      \note Shading surfaces as viewed from behind.  (towards what they are shading)
        //      \type choice
        //      \key UpperLeftCorner
        //      \key LowerLeftCorner
        //      \key UpperRightCorner
        //      \key LowerRightCorner
        //  A2, \field Vertex Entry Direction
        //      \required-field
        //      \type choice
        //      \key Counterclockwise
        //      \key Clockwise
        //  A3, \field Coordinate System
        //      \required-field
        //      \note relative -- coordinates are entered relative to zone origin
        //      \note world -- all coordinates entered are "absolute" for this facility
        //      \note absolute -- same as world
        //      \type choice
        //      \key Relative
        //      \key World
        //      \key Absolute
        //  A4, \field Daylighting Reference Point Coordinate System
        //      \type choice
        //      \key Relative
        //      \default Relative
        //      \note Relative -- coordinates are entered relative to zone origin
        //      \key World
        //      \note World -- all coordinates entered are "absolute" for this facility
        //      \key Absolute
        //      \note absolute -- same as world
        //  A5; \field Rectangular Surface Coordinate System
        //      \type choice
        //      \key Relative
        //      \default Relative
        //      \note Relative -- Starting corner is entered relative to zone origin
        //      \key World
        //      \note World -- Starting corner is entered in "absolute"
        //      \key Absolute
        //      \note absolute -- same as world

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const FlCorners(4, {"UpperLeftCorner", "LowerLeftCorner", "LowerRightCorner", "UpperRightCorner"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumStmt;
        Array1D_string GAlphas(5);
        int NAlphas;
        Array1D<Real64> GNum(1);
        int NNum;
        int IOStat;
        bool OK;
        int Found;
        std::string OutMsg;
        int ZoneNum; // For loop counter
        bool RelWarning(false);
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        cCurrentModuleObject = "GlobalGeometryRules";
        NumStmt = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        OutMsg = " Surface Geometry,";

        {
            int const SELECT_CASE_var = NumStmt;

            if (SELECT_CASE_var == 1) {
                // This is the valid case
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         1,
                                                                         GAlphas,
                                                                         NAlphas,
                                                                         GNum,
                                                                         NNum,
                                                                         IOStat,
                                                                         state.dataIPShortCut->lNumericFieldBlanks,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);

                // Even though these will be validated, set defaults in case error here -- wont
                // cause aborts in later surface gets (hopefully)
                state.dataSurface->Corner = UpperLeftCorner;
                state.dataSurface->WorldCoordSystem = true;
                state.dataSurface->CCW = true;

                OK = false;
                Found = Util::FindItem(GAlphas(1), FlCorners, 4);
                if (Found == 0) {
                    ShowSevereError(state, format("{}: Invalid {}={}", cCurrentModuleObject, state.dataIPShortCut->cAlphaFieldNames(1), GAlphas(1)));
                    ErrorsFound = true;
                } else {
                    state.dataSurface->Corner = Found;
                    OK = true;
                    OutMsg += FlCorners(state.dataSurface->Corner) + ',';
                }

                OK = false;
                if (Util::SameString(GAlphas(2), "CCW") || Util::SameString(GAlphas(2), "Counterclockwise")) {
                    state.dataSurface->CCW = true;
                    OutMsg += "Counterclockwise,";
                    OK = true;
                }
                if (Util::SameString(GAlphas(2), "CW") || Util::SameString(GAlphas(2), "Clockwise")) {
                    state.dataSurface->CCW = false;
                    OutMsg += "Clockwise,";
                    OK = true;
                }
                if (!OK) {
                    ShowSevereError(state, format("{}: Invalid {}={}", cCurrentModuleObject, state.dataIPShortCut->cAlphaFieldNames(2), GAlphas(2)));
                    ErrorsFound = true;
                }

                OK = false;
                if (Util::SameString(GAlphas(3), "World") || Util::SameString(GAlphas(3), "Absolute")) {
                    state.dataSurface->WorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                    OK = true;
                }
                if (Util::SameString(GAlphas(3), "Relative")) {
                    state.dataSurface->WorldCoordSystem = false;
                    OutMsg += "RelativeCoordinateSystem,";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, format("{}: Invalid {}={}", cCurrentModuleObject, state.dataIPShortCut->cAlphaFieldNames(3), GAlphas(3)));
                    ShowContinueError(state, format("{} defaults to \"WorldCoordinateSystem\"", state.dataIPShortCut->cAlphaFieldNames(3)));
                    state.dataSurface->WorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                }

                OK = false;
                if (Util::SameString(GAlphas(4), "World") || Util::SameString(GAlphas(4), "Absolute")) {
                    state.dataSurface->DaylRefWorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                    OK = true;
                }
                if (Util::SameString(GAlphas(4), "Relative") || GAlphas(4).empty()) {
                    state.dataSurface->DaylRefWorldCoordSystem = false;
                    OutMsg += "RelativeCoordinateSystem,";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, format("{}: Invalid {}={}", cCurrentModuleObject, state.dataIPShortCut->cAlphaFieldNames(4), GAlphas(4)));
                    ShowContinueError(state, format("{} defaults to \"RelativeToZoneOrigin\"", state.dataIPShortCut->cAlphaFieldNames(4)));
                    state.dataSurface->DaylRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin,";
                }

                OK = false;
                if (Util::SameString(GAlphas(5), "World") || Util::SameString(GAlphas(5), "Absolute")) {
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem";
                    OK = true;
                }
                if (Util::SameString(GAlphas(5), "Relative") || GAlphas(5).empty()) {
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, format("{}: Invalid {}={}", cCurrentModuleObject, state.dataIPShortCut->cAlphaFieldNames(5), GAlphas(5)));
                    ShowContinueError(state, format("{} defaults to \"RelativeToZoneOrigin\"", state.dataIPShortCut->cAlphaFieldNames(5)));
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                }

            } else if (SELECT_CASE_var == 0) {

                ShowSevereError(state, format("{}: Required object not found.", cCurrentModuleObject));
                OutMsg += "None found in input";
                ErrorsFound = true;

            } else {

                ShowSevereError(state, format("{}: Too many objects entered.  Only one allowed.", cCurrentModuleObject));
                ErrorsFound = true;
            }
        }

        if (!state.dataSurface->WorldCoordSystem) {
            if (state.dataSurface->DaylRefWorldCoordSystem) {
                ShowWarningError(state, format("{}: Potential mismatch of coordinate specifications.", cCurrentModuleObject));
                ShowContinueError(state, format("{}=\"{}\"; while ", state.dataIPShortCut->cAlphaFieldNames(3), GAlphas(3)));
                ShowContinueError(state, format("{}=\"{}\".", state.dataIPShortCut->cAlphaFieldNames(4), GAlphas(4)));
            }
            if (state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem) {
                ShowWarningError(state, format("{}: Potential mismatch of coordinate specifications.", cCurrentModuleObject));
                ShowContinueError(state, format("{}=\"{}\"; while ", state.dataIPShortCut->cAlphaFieldNames(3), GAlphas(3)));
                ShowContinueError(state, format("{}=\"{}\".", state.dataIPShortCut->cAlphaFieldNames(5), GAlphas(5)));
            }
        } else {
            RelWarning = false;
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (state.dataHeatBal->Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (state.dataHeatBal->Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (state.dataHeatBal->Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem) {
                ShowWarningError(state,
                                 format("{}: Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on the "
                                        "default SurfaceGeometry for 'Relative to zone' coordinate.",
                                        cCurrentModuleObject));
                ShowContinueError(state, format("{}=\"{}\"; while ", state.dataIPShortCut->cAlphaFieldNames(3), GAlphas(3)));
                if (GAlphas(5) == "RELATIVE") {
                    ShowContinueError(state, format("{}=\"{}\".", state.dataIPShortCut->cAlphaFieldNames(5), GAlphas(5)));
                } else if (GAlphas(5) != "ABSOLUTE") {
                    ShowContinueError(state, format("{}=\"defaults to RELATIVE\".", state.dataIPShortCut->cAlphaFieldNames(5)));
                }
            }
        }

        print(state.files.eio,
              "! <Surface Geometry>,Starting Corner,Vertex Input Direction,Coordinate System,Daylight Reference "
              "Point Coordinate System,Rectangular (Simple) Surface Coordinate System\n");
        print(state.files.eio, "{}\n", OutMsg);
    }

    void GetDetShdSurfaceData(EnergyPlusData &state,
                              bool &ErrorsFound,          // Error flag indicator (true if errors found)
                              int &SurfNum,               // Count of Current SurfaceNumber
                              int const TotDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
                              int const TotDetachedBldg   // Number of Building Detached Shading Surfaces to obtain
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the Detached Shading Surface Data,
        // checks it for errors, etc.

        // Using/Aliasing
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(2, {"Shading:Site:Detailed", "Shading:Building:Detailed"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;     // IO Status when calling get input subroutine
        int NumAlphas;  // Number of material alpha names being passed
        int NumNumbers; // Number of material properties being passed
        int Loop;
        int Item;
        int ItemsToGet;
        SurfaceClass ClassItem;
        int numSides;

        constexpr std::string_view routineName = "GetDetShdSurfaceData";
        
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        if ((TotDetachedFixed + TotDetachedBldg) > 0 && state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
            ShowWarningError(state, "Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
        }

        if ((TotDetachedFixed + TotDetachedBldg) == 0) return;

        for (Item = 1; Item <= 2; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotDetachedFixed;
                ClassItem = SurfaceClass::Detached_F;
            } else { // IF (Item == 2) THEN
                ItemsToGet = TotDetachedBldg;
                ClassItem = SurfaceClass::Detached_B;
            }

            ip->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 2) {
                ShowSevereError(
                    state, format("{}: Object Definition indicates not = 2 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, NumAlphas));
                ErrorsFound = true;
            }

            for (int Loop = 1; Loop <= ItemsToGet; ++Loop) {
                ipsc->cCurrentModuleObject = cModuleObjects(Item);
                ip->getObjectItem(state,
                                  ipsc->cCurrentModuleObject,
                                  Loop,
                                  ipsc->cAlphaArgs,
                                  NumAlphas,
                                  ipsc->rNumericArgs,
                                  NumNumbers,
                                  IOStat,
                                  ipsc->lNumericFieldBlanks,
                                  ipsc->lAlphaFieldBlanks,
                                  ipsc->cAlphaFieldNames,
                                  ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                surf.Class = ClassItem;
                surf.HeatTransSurf = false;
                // Base transmittance of a shadowing (sub)surface
                if (!ipsc->lAlphaFieldBlanks(2)) {
                    // Schedule for a shadowing (sub)surface
                    surf.SchedShadowSurfIndex =  GetScheduleIndex(state, ipsc->cAlphaArgs(2));
                    if (surf.SchedShadowSurfIndex == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                        ErrorsFound = true;
                    }
                }

                if (surf.SchedShadowSurfIndex != 0) {
                    std::tie(surf.SchedMinValue, surf.SchedMaxValue) = ScheduleManager::GetScheduleMinMaxValues(state, surf.SchedShadowSurfIndex);
                    if (surf.SchedMinValue == 1.0) {
                        // Set transparent for now, check for EMS actuators later in SolarShading::resetShadingSurfaceTransparency
                        surf.IsTransparent = true;
                    }
                    if (surf.SchedMinValue < 0.0) {
                        ShowSevereError(state, format("{}=\"{}\", {}=\"{}\", has schedule values < 0.",
                                               ipsc->cCurrentModuleObject, surf.Name, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                        ShowContinueError(state, "...Schedule values < 0 have no meaning for shading elements.");
                        ErrorsFound = true;
                    }
                    if (surf.SchedMaxValue > 0.0) {
                        state.dataSolarShading->anyScheduledShadingSurface = true;
                    }
                    if (surf.SchedMaxValue > 1.0) {
                        ShowSevereError(state, format("{}=\"{}\", {}=\"{}\", has schedule values > 1.",
                                                      ipsc->cCurrentModuleObject, surf.Name, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                        ShowContinueError(state, "...Schedule values > 1 have no meaning for shading elements.");
                    }
                    if (std::abs(surf.SchedMinValue - surf.SchedMaxValue) > 1.0e-6) {
                        state.dataSurface->ShadingTransmittanceVaries = true;
                    }
                }
                if (ipsc->lNumericFieldBlanks(1) || ipsc->rNumericArgs(1) == Constant::AutoCalculate) {
                    numSides = (NumNumbers - 1) / 3;
                    surf.Sides = numSides;
                    if (mod(NumNumbers - 1, 3) != 0) {
                        ShowWarningError(state, format("{}=\"{}\", {} not even multiple of 3. Will read in {}",
                                                       ipsc->cCurrentModuleObject, surf.Name, ipsc->cNumericFieldNames(1), surf.Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(state, format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                                      ipsc->cCurrentModuleObject, surf.Name, ipsc->cNumericFieldNames(1), surf.Sides));
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (NumNumbers - 1) / 3;
                    surf.Sides = ipsc->rNumericArgs(1);
                    if (numSides > surf.Sides) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", field {}={}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                ipsc->cNumericFieldNames(1),
                                                surf.Sides));
                        ShowContinueError(state,
                                          format("...but {} were entered. Only the indicated {} will be used.",
                                                 numSides,
                                                 ipsc->cNumericFieldNames(1)));
                    }
                }
                surf.Vertex.allocate(surf.Sides);
                GetVertices(state, SurfNum, surf.Sides, ipsc->rNumericArgs({2, _}));
                CheckConvexity(state, SurfNum, surf.Sides);
                if (state.dataReportFlag->MakeMirroredDetachedShading) {
                    SurfNum = MakeMirrorSurface(state, SurfNum);
                }
            }

        } // Item Loop
    } // GetDetShdSurfaceData()

    void GetRectDetShdSurfaceData(EnergyPlusData &state,
                                  bool &ErrorsFound,              // Error flag indicator (true if errors found)
                                  int &SurfNum,                   // Count of Current SurfaceNumber
                                  int const TotRectDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
                                  int const TotRectDetachedBldg   // Number of Building Detached Shading Surfaces to obtain
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets the simple, rectangular detached surfaces.

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(2, {"Shading:Site", "Shading:Building"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;     // IO Status when calling get input subroutine
        int NumAlphas;  // Number of material alpha names being passed
        int NumNumbers; // Number of material properties being passed
        int Loop;
        int Item;
        int ItemsToGet;
        SurfaceClass ClassItem;

        constexpr std::string_view routineName = "GetRectDetShdSurfaceData";
        
        if ((TotRectDetachedFixed + TotRectDetachedBldg) > 0 && state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
            ShowWarningError(state, "Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
        }

        if (TotRectDetachedFixed + TotRectDetachedBldg == 0) return;

        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        for (int Item = 1; Item <= 2; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotRectDetachedFixed;
                ClassItem = SurfaceClass::Detached_F;
            } else { // IF (Item == 2) THEN
                ItemsToGet = TotRectDetachedBldg;
                ClassItem = SurfaceClass::Detached_B;
            }

            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 1) {
                ShowSevereError(
                    state, format("{}: Object Definition indicates not = 1 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, NumAlphas));
                ErrorsFound = true;
            }

            for (int Loop = 1; Loop <= ItemsToGet; ++Loop) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                surf.Class = ClassItem;
                surf.HeatTransSurf = false;

                surf.Azimuth = ipsc->rNumericArgs(1);
                if (surf.Class == SurfaceClass::Detached_B && !state.dataSurface->WorldCoordSystem) {
                    surf.Azimuth += state.dataHeatBal->BuildingAzimuth;
                }
                if (surf.Class == SurfaceClass::Detached_B) {
                    surf.Azimuth += state.dataHeatBal->BuildingRotationAppendixG;
                }
                surf.Tilt = ipsc->rNumericArgs(2);
                surf.convOrientation =
                    Convect::GetSurfConvOrientation(surf.Tilt);

                surf.Sides = 4;
                surf.Vertex.allocate(surf.Sides);

                MakeRectangularVertices(state,
                                        SurfNum,
                                        ipsc->rNumericArgs(3),
                                        ipsc->rNumericArgs(4),
                                        ipsc->rNumericArgs(5),
                                        ipsc->rNumericArgs(6),
                                        ipsc->rNumericArgs(7),
                                        state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem);

                if (surf.Area <= 0.0) {
                    ShowSevereError(state, format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}", ipsc->cCurrentModuleObject, surf.Name, surf.Area));
                    ErrorsFound = true;
                }

                if (state.dataReportFlag->MakeMirroredDetachedShading) {
                    SurfNum = MakeMirrorSurface(state, SurfNum);
                }
            }

        } // Item Loop
    } // GetRectDetShdSurfaceData()

    void GetHTSurfaceData(EnergyPlusData &state,
                          bool &ErrorsFound,                 // Error flag indicator (true if errors found)
                          int &SurfNum,                      // Count of Current SurfaceNumber
                          int const TotHTSurfs,              // Number of Heat Transfer Base Surfaces to obtain
                          int const TotDetailedWalls,        // Number of Wall:Detailed items to obtain
                          int const TotDetailedRoofs,        // Number of RoofCeiling:Detailed items to obtain
                          int const TotDetailedFloors,       // Number of Floor:Detailed items to obtain
                          int &NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the HeatTransfer Surface Data,
        // checks it for errors, etc.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Heat Transfer Surface Definition
        // BuildingSurface:Detailed,
        //  \extensible:3 -- duplicate last set of x,y,z coordinates (last 3 fields), remembering to remove ; from "inner" fields.
        //  \format vertices
        //  A1 , \field Name
        //       \required-field
        //       \type alpha
        //       \reference SurfaceNames
        //       \reference SurfAndSubSurfNames
        //       \reference AllHeatTranSurfNames
        //       \reference HeatTranBaseSurfNames
        //       \reference OutFaceEnvNames
        //       \reference AllHeatTranAngFacNames
        //       \reference RadGroupAndSurfNames
        //       \reference SurfGroupAndHTSurfNames
        //       \reference AllShadingAndHTSurfNames
        //  A2 , \field Surface Type
        //       \required-field
        //       \type choice
        //       \key Floor
        //       \key Wall
        //       \key Ceiling
        //       \key Roof
        //  A3 , \field Construction Name
        //       \required-field
        //       \note To be matched with a construction in this input file
        //       \type object-list
        //       \object-list ConstructionNames
        //  A4 , \field Zone Name
        //       \required-field
        //       \note Zone the surface is a part of
        //       \type object-list
        //       \object-list ZoneNames
        //  A5 , \field Outside Boundary Condition
        //       \required-field
        //       \type choice
        //       \key Adiabatic
        //       \key Surface
        //       \key Zone
        //       \key Outdoors
        //       \key Ground
        //       \key GroundFCfactorMethod
        //       \key OtherSideCoefficients
        //       \key OtherSideConditionsModel
        //       \key GroundSlabPreprocessorAverage
        //       \key GroundSlabPreprocessorCore
        //       \key GroundSlabPreprocessorPerimeter
        //       \key GroundBasementPreprocessorAverageWall
        //       \key GroundBasementPreprocessorAverageFloor
        //       \key GroundBasementPreprocessorUpperWall
        //       \key GroundBasementPreprocessorLowerWall
        //  A6,  \field Outside Boundary Condition Object
        //       \type object-list
        //       \object-list OutFaceEnvNames
        //       \note Non-blank only if the field Outside Boundary Condition is Surface,
        //       \note Zone, OtherSideCoefficients or OtherSideConditionsModel
        //       \note If Surface, specify name of corresponding surface in adjacent zone or
        //       \note specify current surface name for internal partition separating like zones
        //       \note If Zone, specify the name of the corresponding zone and
        //       \note the program will generate the corresponding interzone surface
        //       \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
        //       \note If OtherSideConditionsModel, specify name of SurfaceProperty:OtherSideConditionsModel
        //  A7 , \field Sun Exposure
        //       \required-field
        //       \type choice
        //       \key SunExposed
        //       \key NoSun
        //       \default SunExposed
        //  A8,  \field Wind Exposure
        //       \required-field
        //       \type choice
        //       \key WindExposed
        //       \key NoWind
        //       \default WindExposed
        //  N1,  \field View Factor to Ground
        //       \type real
        //       \note From the exterior of the surface
        //       \note Unused if one uses the "reflections" options in Solar Distribution in Building input
        //       \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
        //       \note autocalculate will automatically calculate this value from the tilt of the surface
        //       \autocalculatable
        //       \minimum 0.0
        //       \maximum 1.0
        //       \default autocalculate
        //  N2 , \field Number of Vertices
        //       \note shown with 120 vertex coordinates -- extensible object
        //       \note  "extensible" -- duplicate last set of x,y,z coordinates (last 3 fields),
        //       \note remembering to remove ; from "inner" fields.
        //       \note for clarity in any error messages, renumber the fields as well.
        //       \note (and changing z terminator to a comma "," for all but last one which needs a semi-colon ";")
        //       \autocalculatable
        //       \minimum 3
        //       \default autocalculate
        //       \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
        //       \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
        //       \note for some internal calculations, but all coordinates are given in an "absolute" system.
        //  N3-xx as indicated by the N3 value

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr std::array<std::string_view, 4> cModuleObjects = {"BuildingSurface:Detailed", "Wall:Detailed", "Floor:Detailed", "RoofCeiling:Detailed"};

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;          // IO Status when calling get input subroutine
        int SurfaceNumAlpha; // Number of material alpha names being passed
        int SurfaceNumProp;  // Number of material properties being passed
        int ZoneNum;         // DO loop counter (zones)
        int Found;           // For matching interzone surfaces
        int Loop;
        int Item;
        int ItemsToGet;
        int ClassItem;
        int ArgPointer;
        int numSides;

        constexpr std::string_view routineName = "GetHTSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        GetOSCData(state, ErrorsFound);
        GetOSCMData(state, ErrorsFound);
        GetFoundationData(state, ErrorsFound);

        NeedToAddSurfaces = 0;
        
        for (int Item = 1; Item <= 4; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects[Item-1];
            if (Item == 1) {
                ItemsToGet = TotHTSurfs;
                ClassItem = 0;
            } else if (Item == 2) {
                ItemsToGet = TotDetailedWalls;
                ClassItem = 1;
            } else if (Item == 3) {
                ItemsToGet = TotDetailedFloors;
                ClassItem = 2;
            } else { // IF (Item == 4) THEN
                ItemsToGet = TotDetailedRoofs;
                ClassItem = 3;
            }

            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);
            if (Item == 1) {
                if (SurfaceNumAlpha != 9) {
                    ShowSevereError(
                        state,
                        format("{}: Object Definition indicates not = 9 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, SurfaceNumAlpha));
                    ErrorsFound = true;
                }
            } else {
                if (SurfaceNumAlpha != 8) {
                    ShowSevereError(
                        state,
                        format("{}: Object Definition indicates not = 8 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, SurfaceNumAlpha));
                    ErrorsFound = true;
                }
            }

            for (int Loop = 1; Loop <= ItemsToGet; ++Loop) {
                ipsc->cCurrentModuleObject = cModuleObjects[Item-1];
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         SurfaceNumAlpha,
                                                                         ipsc->rNumericArgs,
                                                                         SurfaceNumProp,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                
                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                
                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                ArgPointer = 2;
                if (Item == 1) {
                    if (ipsc->cAlphaArgs(2) == "CEILING") ipsc->cAlphaArgs(2) = "ROOF";
                    ClassItem = Util::FindItemInList(ipsc->cAlphaArgs(2), sg->BaseSurfCls, 3);
                    if (ClassItem == 0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", invalid {}=\"{}",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cAlphaFieldNames(2),
                                               ipsc->cAlphaArgs(2)));
                        ErrorsFound = true;
                    } else {
                        surf.Class = sg->BaseSurfIDs(ClassItem);
                    }
                    ++ArgPointer;
                } else {
                    surf.Class = sg->BaseSurfIDs(ClassItem);
                }

                surf.Construction = Util::FindItemInList(
                    ipsc->cAlphaArgs(ArgPointer), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);

                if (surf.Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer),ipsc->cAlphaArgs(ArgPointer));
                } else if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {}=\"{}\" - has Window materials.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(ArgPointer),
                                           ipsc->cAlphaArgs(ArgPointer)));
                    if (Item == 1) {
                        ShowContinueError(state, format("...because {}={}", ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                    } else {
                        ShowContinueError(state, format("...because Surface Type={}", sg->BaseSurfCls(ClassItem)));
                    }
                } else {
                    state.dataConstruction->Construct(surf.Construction).IsUsed = true;
                    surf.ConstructionStoredInputValue = surf.Construction;
                }
                surf.HeatTransSurf = true;
                surf.BaseSurf = SurfNum;
                surf.BaseSurfName = surf.Name;

                ++ArgPointer;
                surf.ZoneName = ipsc->cAlphaArgs(ArgPointer);
                ZoneNum = Util::FindItemInList(surf.ZoneName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);

                if (ZoneNum != 0) {
                    surf.Zone = ZoneNum;
                } else {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                    surf.Class = SurfaceClass::Invalid;
                    surf.ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }

                ++ArgPointer;
                if (!ipsc->lAlphaFieldBlanks(ArgPointer)) {
                    int spaceNum = Util::FindItemInList(ipsc->cAlphaArgs(ArgPointer), state.dataHeatBal->space);

                    if (spaceNum != 0) {
                        surf.spaceNum = spaceNum;
                        if (surf.Zone != state.dataHeatBal->space(spaceNum).zoneNum) {
                            ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer),
                                                         "is not in the same zone as the surface.");
                            surf.Class = SurfaceClass::Invalid;
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                        surf.Class = SurfaceClass::Invalid;
                        ErrorsFound = true;
                    }
                }
                // Get the ExteriorBoundaryCondition flag from input There are 4 conditions that
                // can take place. The conditions are set with a 0, -1, or -2, or all of the
                // zone names have to be looked at and generate the interzone array number
                ++ArgPointer;
                surf.ExtBoundCondName = ipsc->cAlphaArgs(ArgPointer + 1);

                if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Outdoors")) {
                    surf.ExtBoundCond = ExternalEnvironment;

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Adiabatic")) {
                    surf.ExtBoundCond = UnreconciledZoneSurface;
                    surf.ExtBoundCondName = surf.Name;

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Ground")) {
                    surf.ExtBoundCond = Ground;

                    if (state.dataSurfaceGeometry->NoGroundTempObjWarning) {
                        if (!state.dataEnvrn->GroundTempObjInput) {
                            ShowWarningError(state,
                                             "GetHTSurfaceData: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError(state, format("Found first in surface={}", ipsc->cAlphaArgs(1)));
                            ShowContinueError(
                                state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp));
                        }
                        state.dataSurfaceGeometry->NoGroundTempObjWarning = false;
                    }

                    // Added for FCfactor method
                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundFCfactorMethod")) {
                    surf.ExtBoundCond = GroundFCfactorMethod;
                    if (state.dataSurfaceGeometry->NoFCGroundTempObjWarning) {
                        if (!state.dataEnvrn->FCGroundTemps) {
                            ShowSevereError(state,
                                            "GetHTSurfaceData: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError(state, format("Found first in surface={}", ipsc->cAlphaArgs(1)));
                            ShowContinueError(state,
                                              "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with "
                                              "Ground Temperatures.");
                            ErrorsFound = true;
                            state.dataSurfaceGeometry->NoFCGroundTempObjWarning = false;
                        }
                    }
                    if (surf.Construction > 0) {
                        auto const &constr = state.dataConstruction->Construct(surf.Construction);
                        if (surf.Class == SurfaceClass::Wall && !constr.TypeIsCfactorWall) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", invalid {}",
                                                   ipsc->cCurrentModuleObject,
                                                   surf.Name,
                                                   ipsc->cAlphaFieldNames(ArgPointer)));
                            ShowContinueError(state, format("Construction=\"{}\" is not type Construction:CfactorUndergroundWall.", constr.Name));
                            ErrorsFound = true;
                        }
                        if (surf.Class == SurfaceClass::Floor && !constr.TypeIsFfactorFloor) {
                            ShowSevereError(state, format("{}=\"{}\", invalid {}", ipsc->cCurrentModuleObject, surf.Name, ipsc->cAlphaFieldNames(ArgPointer)));
                            ShowContinueError(state, format("Construction=\"{}\" is not type Construction:FfactorGroundFloor.", constr.Name));
                            ErrorsFound = true;
                        }
                    }

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "OtherSideCoefficients")) {
                    Found = Util::FindItemInList(surf.ExtBoundCondName, state.dataSurface->OSC, state.dataSurface->TotOSC);
                    if (Found == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer + 1), ipsc->cAlphaArgs(ArgPointer + 1));
                        ShowContinueError(state, " no OtherSideCoefficients of that name.");
                        ErrorsFound = true;
                    } else {
                        surf.OSCPtr = Found;
                        if (state.dataSurface->OSC(Found).SurfFilmCoef > 0.0) {
                            surf.ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            surf.ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Surface")) {
                    // it has to be another surface which needs to be found
                    // this will be found on the second pass through the surface input
                    // for flagging, set the value to UnreconciledZoneSurface
                    // name (ExtBoundCondName) will be validated later.
                    surf.ExtBoundCond = UnreconciledZoneSurface;
                    if (ipsc->lAlphaFieldBlanks(ArgPointer + 1)) {
                        surf.ExtBoundCondName = surf.Name;
                        ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(ArgPointer + 1));
                        ShowContinueError(state, format("..{}=\"Surface\" must be non-blank.", ipsc->cAlphaFieldNames(ArgPointer)));
                        ShowContinueError(state, "..This surface will become an adiabatic surface - no doors/windows allowed.");
                    }

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Zone")) {
                    // This is the code for an unmatched "other surface"
                    // will be set up later.
                    surf.ExtBoundCond = UnenteredAdjacentZoneSurface;
                    // check OutsideFaceEnvironment for legal zone
                    Found = Util::FindItemInList(
                        surf.ExtBoundCondName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
                    ++NeedToAddSurfaces;

                    if (Found == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                        ShowContinueError(state, "..Referenced as Zone for this surface.");
                        ErrorsFound = true;
                    }

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "Foundation")) {

                    if (!state.dataWeather->WeatherFileExists) {
                        ShowSevereError(
                            state,
                            format("{}=\"{}\", using \"Foundation\" type Outside Boundary Condition requires specification of a weather file",
                                   ipsc->cCurrentModuleObject,
                                   surf.Name));
                        ShowContinueError(state,
                                          "Either place in.epw in the working directory or specify a weather file on the command line using -w "
                                          "/path/to/weather.epw");
                        ErrorsFound = true;
                    }

                    // Find foundation object, if blank use default
                    if (ipsc->lAlphaFieldBlanks(ArgPointer + 1)) {

                        if (!sg->kivaManager.defaultAdded) {
                            // Add default foundation if no other foundation object specified
                            sg->kivaManager.addDefaultFoundation();
                        }
                        surf.OSCPtr = sg->kivaManager.defaultIndex; // Reuse OSC pointer...shouldn't be used for non OSC surfaces anyway.
                    } else {
                        surf.OSCPtr = sg->kivaManager.findFoundation(surf.ExtBoundCondName);
                        // This is not a good API, not found should be -1 for 0-based arrays and 0 (or preferably -1) for 1-based arrays
                        if (surf.OSCPtr == (int)sg->kivaManager.foundationInputs.size()) {
                            ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer + 1), ipsc->cAlphaArgs(ArgPointer + 1));
                            ErrorsFound = true;
                        }
                    }

                    if (state.dataConstruction->Construct(surf.Construction).SourceSinkPresent) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", construction may not have an internal source/sink",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                        ErrorsFound = true;
                    }
                    surf.ExtBoundCond = KivaFoundation;

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "OtherSideConditionsModel")) {
                    surf.OSCMPtr = Util::FindItemInList(surf.ExtBoundCondName, state.dataSurface->OSCM, state.dataSurface->TotOSCM);
                    if (surf.OSCMPtr == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ArgPointer + 1), ipsc->cAlphaArgs(ArgPointer + 1));
                        ErrorsFound = true;
                    }
                    surf.ExtBoundCond = OtherSideCondModeledExt;

                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundSlabPreprocessorAverage") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundSlabPreprocessorCore") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundSlabPreprocessorPerimeter") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageFloor") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageWall") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundBasementPreprocessorUpperWall") ||
                           Util::SameString(ipsc->cAlphaArgs(ArgPointer), "GroundBasementPreprocessorLowerWall")) {
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                    ShowContinueError(state, "The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder.");
                    ErrorsFound = true;

                } else {
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                    ShowContinueError(state,
                                      "Should be one of \"Outdoors\", \"Adiabatic\", Ground\", \"Surface\", \"OtherSideCoefficients\", "
                                      "\"OtherSideConditionsModel\" or \"Zone\"");
                    ErrorsFound = true;
                } // ... End of the ExtBoundCond logical IF Block

                ArgPointer += 2;
                // Set the logical flag for the exterior solar
                if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "SunExposed")) {
                    if ((surf.ExtBoundCond != ExternalEnvironment) && (surf.ExtBoundCond != OtherSideCondModeledExt)) {
                        ShowWarningFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ArgPointer),ipsc->cAlphaArgs(ArgPointer),
                                                      "..This surface is not exposed to External Environment.  Sun exposure has no effect.");
                    } else {
                        surf.ExtSolar = true;
                    }
                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "NoSun")) {
                    surf.ExtSolar = false;
                } else {
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                    ErrorsFound = true;
                }

                ++ArgPointer;
                // Set the logical flag for the exterior wind
                if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "WindExposed")) {
                    surf.ExtWind = true;
                } else if (Util::SameString(ipsc->cAlphaArgs(ArgPointer), "NoWind")) {
                    surf.ExtWind = false;
                } else {
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(ArgPointer), ipsc->cAlphaArgs(ArgPointer));
                    ErrorsFound = true;
                }

                // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                //                if (surf.Construction > 0)
                //                    surf.ExtEcoRoof =
                //                        state.dataConstruction->Construct(surf.Construction).TypeIsEcoRoof;

                surf.ViewFactorGround = ipsc->rNumericArgs(1);
                if (ipsc->lNumericFieldBlanks(1))
                    surf.ViewFactorGround = Constant::AutoCalculate;
                if (ipsc->lNumericFieldBlanks(2) || ipsc->rNumericArgs(2) == Constant::AutoCalculate) {
                    numSides = (SurfaceNumProp - 2) / 3;
                    surf.Sides = numSides;
                    if (mod(SurfaceNumProp - 2, 3) != 0) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", {} not even multiple of 3. Will read in {}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                ipsc->cNumericFieldNames(2),
                                                surf.Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cNumericFieldNames(2),
                                               surf.Sides));
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (SurfaceNumProp - 2) / 3;
                    surf.Sides = ipsc->rNumericArgs(2);
                    if (numSides > surf.Sides) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", field {}={}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                ipsc->cNumericFieldNames(2),
                                                surf.Sides));
                        ShowContinueError(state,
                                          format("...but {} were entered. Only the indicated {} will be used.",
                                                 numSides,
                                                 ipsc->cNumericFieldNames(2)));
                    }
                }
                surf.Vertex.allocate(surf.Sides);
                surf.NewVertex.allocate(surf.Sides);
                GetVertices(state, SurfNum, surf.Sides, ipsc->rNumericArgs({3, _}));
                if (surf.Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           surf.Area));
                    ErrorsFound = true;
                }

                CheckConvexity(state, SurfNum, surf.Sides);
                if (Util::SameString(ipsc->cAlphaArgs(5), "Surface")) {
                    if (surf.Sides != (int)surf.Vertex.size()) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", After CheckConvexity, mismatch between Sides ({}) and size of Vertex ({}).",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               surf.Sides,
                                               surf.Vertex.size()));
                        ShowContinueError(state, "CheckConvexity is used to verify the convexity of a surface and detect collinear points.");
                        ErrorsFound = true;
                    }
                }
                if (surf.Construction > 0) {
                    // Check wall height for the CFactor walls

                    if (surf.Class == SurfaceClass::Wall &&
                        state.dataConstruction->Construct(surf.Construction).TypeIsCfactorWall) {
                        if (std::abs(surf.Height -
                                     state.dataConstruction->Construct(surf.Construction).Height) > 0.05) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Wall Height = {:.2T}",
                                                    ipsc->cCurrentModuleObject,
                                                    surf.Name,
                                                    surf.Height));
                            ShowContinueError(state, "..which does not match its construction height.");
                        }
                    }

                    // Check area and perimeter for the FFactor floors
                    if (surf.Class == SurfaceClass::Floor &&
                        state.dataConstruction->Construct(surf.Construction).TypeIsFfactorFloor) {
                        if (std::abs(surf.Area -
                                     state.dataConstruction->Construct(surf.Construction).Area) > 0.1) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Floor Area = {:.2T}",
                                                    ipsc->cCurrentModuleObject,
                                                    surf.Name,
                                                    surf.Area));
                            ShowContinueError(state, "..which does not match its construction area.");
                        }
                        if (surf.Perimeter <
                            state.dataConstruction->Construct(surf.Construction).PerimeterExposed - 0.1) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Floor Perimeter = {:.2T}",
                                                    ipsc->cCurrentModuleObject,
                                                    surf.Name,
                                                    surf.Perimeter));
                            ShowContinueError(state, "..which is less than its construction exposed perimeter.");
                        }
                    }
                } // if (surf.Construction > 0)
            } // for (Loop)
        } // for (Item)
        
        // Check number of Vertex between base surface and Outside Boundary surface
        int ExtSurfNum;
        for (int i = 1; i <= SurfNum; i++) {
            auto &surf = sg->SurfaceTmp(i);
            if (surf.ExtBoundCond == UnreconciledZoneSurface && surf.ExtBoundCondName != "") {
                ExtSurfNum = Util::FindItemInList(surf.ExtBoundCondName, state.dataSurfaceGeometry->SurfaceTmp);
                // If we cannot find the referenced surface
                if (ExtSurfNum == 0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" references an outside boundary surface that cannot be found:{}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           surf.ExtBoundCondName));
                    ErrorsFound = true;
                    // If vertex size mistmatch
                } else if (surf.Vertex.size() !=
                           state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Vertex.size()) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Vertex size mismatch between base surface :{} and outside boundary surface: {}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           surf.Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Name));
                    ShowContinueError(state,
                                      format("The vertex sizes are {} for base surface and {} for outside boundary surface. Please check inputs.",
                                             surf.Vertex.size(),
                                             state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Vertex.size()));
                    ErrorsFound = true;
                }
            } // if (surf.ExtBoundCond)
        } // for (i)
    } // GetHTSurfaceDatA()

    void GetRectSurfaces(EnergyPlusData &state,
                         bool &ErrorsFound,                        // Error flag indicator (true if errors found)
                         int &SurfNum,                             // Count of Current SurfaceNumber
                         int const TotRectExtWalls,                // Number of Exterior Walls to obtain
                         int const TotRectIntWalls,                // Number of Adiabatic Walls to obtain
                         int const TotRectIZWalls,                 // Number of Interzone Walls to obtain
                         int const TotRectUGWalls,                 // Number of Underground to obtain
                         int const TotRectRoofs,                   // Number of Roofs to obtain
                         int const TotRectCeilings,                // Number of Adiabatic Ceilings to obtain
                         int const TotRectIZCeilings,              // Number of Interzone Ceilings to obtain
                         int const TotRectGCFloors,                // Number of Floors with Ground Contact to obtain
                         int const TotRectIntFloors,               // Number of Adiabatic Walls to obtain
                         int const TotRectIZFloors,                // Number of Interzone Floors to obtain
                         int &NeedToAddSurfaces                    // Number of surfaces to add, based on unentered IZ surfaces
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get simple (rectangular, LLC corner specified) walls

        // Using/Aliasing

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(10,
                                                   {"Wall:Exterior",
                                                    "Wall:Adiabatic",
                                                    "Wall:Interzone",
                                                    "Wall:Underground",
                                                    "Roof",
                                                    "Ceiling:Adiabatic",
                                                    "Ceiling:Interzone",
                                                    "Floor:GroundContact",
                                                    "Floor:Adiabatic",
                                                    "Floor:Interzone"});

        int Item;
        int ItemsToGet;
        int Loop;
        int NumAlphas;
        int NumNumbers;
        int IOStat; // IO Status when calling get input subroutine
        int Found;  // For matching base surfaces
        bool GettingIZSurfaces;
        int OtherSurfaceField;
        int ExtBoundCondition;
        int ClassItem;
        int ZoneNum;

        struct CMOParams {
            SurfaceClass Class;
            std::string_view CMO;
            int Items;
            bool IZSurfaces;
            int OtherSurfaceField;
            int ExtBoundCondition;
        };
#ifdef GET_OUT
        std::array<CMOParams, 10> cmoParams = {
            {SurfaceClass::Wall, "Wall:Exterior", TotRectExtWalls, false, 0, ExternalEnvironment},
            {SurfaceClass::Wall, "Wall:Adiabatic", TotRectIntWalls, false, 0, UnreconciledZoneSurface},
            {SurfaceClass::Wall, "Wall:Interzone", TotRectIZWalls, true, 5, UnreconciledZoneSurface},
            {SurfaceClass::Wall, "Wall:Underground", TotRectUGWalls, false, 0, Ground},
            {SurfaceClass::Roof, "Roof", TotRectRoofs, false, 0, ExternalEnvironment},
            {SurfaceClass::Roof, "Ceiling:Adiabatic", TotRectCeilings, false, 0, UnreconciledZoneSurface},
            {SurfaceClass::Roof, "Ceiling:Interzone", TotRectIZCeilings, true, 5, ExternalEnvironment},
            {SurfaceClass::Floor, "Floor:GroundContact", TotRectGCFloors, false, 0, Ground},
            {SurfaceClass::Floor, "Floor:Adiabatic", TotRectIntFloors, false, 0, UnreconciledZoneSurface},
            {SurfaceClass::Floor, "Floor:Interzone", TotRectIZFloors, true, 5, UnreconciledZoneSurface}
        };
#endif // GET_OUT
        
        constexpr std::string_view routineName = "GetRectSurfaces";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        for (int Item = 1; Item <= 10; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotRectExtWalls;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = ExternalEnvironment;
                ClassItem = 1;
            } else if (Item == 2) {
                ItemsToGet = TotRectIntWalls;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 1;
            } else if (Item == 3) {
                ItemsToGet = TotRectIZWalls;
                GettingIZSurfaces = true;
                OtherSurfaceField = 5;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 1;
            } else if (Item == 4) {
                ItemsToGet = TotRectUGWalls;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = Ground;
                ClassItem = 1;
            } else if (Item == 5) {
                ItemsToGet = TotRectRoofs;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = ExternalEnvironment;
                ClassItem = 3;
            } else if (Item == 6) {
                ItemsToGet = TotRectCeilings;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 3;
            } else if (Item == 7) {
                ItemsToGet = TotRectIZCeilings;
                GettingIZSurfaces = false;
                OtherSurfaceField = 5;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 3;
            } else if (Item == 8) {
                ItemsToGet = TotRectGCFloors;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = Ground;
                ClassItem = 2;
            } else if (Item == 9) {
                ItemsToGet = TotRectIntFloors;
                GettingIZSurfaces = false;
                OtherSurfaceField = 0;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 2;
            } else { // IF (Item == 10) THEN
                ItemsToGet = TotRectIZFloors;
                GettingIZSurfaces = true;
                OtherSurfaceField = 5;
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 2;
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                if (NumNumbers < 7) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Too few number of numeric args=[{}].",
                                           ipsc->cCurrentModuleObject,
                                           ipsc->cAlphaArgs(1),
                                           NumNumbers));
                    ErrorsFound = true;
                }

                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);

                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                surf.Class = sg->BaseSurfIDs(ClassItem);             // Set class number

                surf.Construction = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);

                if (surf.Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                } else if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {}=\"{}\" - has Window materials.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaArgs(2)));
                    ShowContinueError(state,
                                      format("...because {}={}", ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                } else {
                    state.dataConstruction->Construct(surf.Construction).IsUsed = true;
                    surf.ConstructionStoredInputValue = surf.Construction;
                }
                surf.HeatTransSurf = true;
                surf.BaseSurf = SurfNum;
                surf.BaseSurfName = surf.Name;

                surf.ZoneName = ipsc->cAlphaArgs(3);
                ZoneNum = Util::FindItemInList(surf.ZoneName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);

                if (ZoneNum != 0) {
                    surf.Zone = ZoneNum;
                } else {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    surf.Class = SurfaceClass::Invalid;
                    surf.ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }

                if (!ipsc->lAlphaFieldBlanks(4)) {
                    int spaceNum = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataHeatBal->space);

                    if (spaceNum != 0) {
                        surf.spaceNum = spaceNum;
                    } else {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                        surf.Class = SurfaceClass::Invalid;
                        ErrorsFound = true;
                    }
                }

                surf.ExtBoundCond = ExtBoundCondition;
                if (surf.Construction > 0) {
                    if (surf.Class == SurfaceClass::Wall &&
                        state.dataConstruction->Construct(surf.Construction).TypeIsCfactorWall &&
                        surf.ExtBoundCond == Ground) {
                        surf.ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(surf.Construction).TypeIsCfactorWall) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Construction type is \"Construction:CfactorUndergroundWall\" but invalid for this object.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                    }
                    if (surf.Class == SurfaceClass::Floor &&
                        state.dataConstruction->Construct(surf.Construction).TypeIsFfactorFloor &&
                        surf.ExtBoundCond == Ground) {
                        surf.ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(surf.Construction).TypeIsFfactorFloor) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Construction type is \"Construction:FfactorGroundFloor\" but invalid for this object.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                    }
                }
                surf.ExtSolar = false;
                surf.ExtWind = false;
                surf.ViewFactorGround = Constant::AutoCalculate;

                if (surf.ExtBoundCond == ExternalEnvironment) {
                    surf.ExtSolar = true;
                    surf.ExtWind = true;

                    // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                    //                    if (surf.Construction > 0)
                    //                        surf.ExtEcoRoof =
                    //                            state.dataConstruction->Construct(surf.Construction).TypeIsEcoRoof;

                } else if (surf.ExtBoundCond == UnreconciledZoneSurface) {
                    if (GettingIZSurfaces) {
                        surf.ExtBoundCondName = ipsc->cAlphaArgs(OtherSurfaceField);
                        Found = Util::FindItemInList(
                            surf.ExtBoundCondName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
                        // see if match to zone, then it's an unentered other surface, else reconciled later
                        if (Found > 0) {
                            ++NeedToAddSurfaces;
                            surf.ExtBoundCond = UnenteredAdjacentZoneSurface;
                        }
                    } else {
                        surf.ExtBoundCondName = surf.Name;
                    }

                } else if (surf.ExtBoundCond == Ground) {

                    if (state.dataSurfaceGeometry->NoGroundTempObjWarning) {
                        if (!state.dataEnvrn->GroundTempObjInput) {
                            ShowWarningError(state,
                                             "GetRectSurfaces: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError(state, format("Found first in surface={}", ipsc->cAlphaArgs(1)));
                            ShowContinueError(
                                state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp));
                        }
                        state.dataSurfaceGeometry->NoGroundTempObjWarning = false;
                    }

                } else if (surf.ExtBoundCond == GroundFCfactorMethod) {
                    if (state.dataSurfaceGeometry->NoFCGroundTempObjWarning) {
                        if (!state.dataEnvrn->FCGroundTemps) {
                            ShowSevereError(state,
                                            "GetRectSurfaces: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError(state, format("Found first in surface={}", ipsc->cAlphaArgs(1)));
                            ShowContinueError(state,
                                              "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with "
                                              "Ground Temperatures.");
                            ErrorsFound = true;
                            state.dataSurfaceGeometry->NoFCGroundTempObjWarning = false;
                        }
                    }

                } // ... End of the ExtBoundCond logical IF Block

                surf.Azimuth = ipsc->rNumericArgs(1);
                surf.Tilt = ipsc->rNumericArgs(2);
                surf.convOrientation =
                    Convect::GetSurfConvOrientation(surf.Tilt);
                if (!state.dataSurface->WorldCoordSystem) {
                    if (ZoneNum != 0) {
                        surf.Azimuth +=
                            state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->Zone(ZoneNum).RelNorth;
                    }
                }
                if (ZoneNum != 0) {
                    surf.Azimuth += state.dataHeatBal->BuildingRotationAppendixG;
                }

                surf.Sides = 4;
                surf.Vertex.allocate(surf.Sides);

                MakeRectangularVertices(state,
                                        SurfNum,
                                        ipsc->rNumericArgs(3),
                                        ipsc->rNumericArgs(4),
                                        ipsc->rNumericArgs(5),
                                        ipsc->rNumericArgs(6),
                                        ipsc->rNumericArgs(7),
                                        state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem);

                if (surf.Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           surf.Area));
                    ErrorsFound = true;
                }

                // Check wall height for the CFactor walls
                if (surf.Class == SurfaceClass::Wall &&
                    surf.ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(surf.Height -
                                 state.dataConstruction->Construct(surf.Construction).Height) > 0.05) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Wall Height = {:.2T}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                surf.Height));
                        ShowContinueError(state, "..which deos not match its construction height.");
                    }
                }

                // Check area and perimeter for the FFactor floors
                if (surf.Class == SurfaceClass::Floor &&
                    surf.ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(surf.Area -
                                 state.dataConstruction->Construct(surf.Construction).Area) > 0.1) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Floor Area = {:.2T}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                surf.Area));
                        ShowContinueError(state, "..which does not match its construction area.");
                    }
                    if (surf.Perimeter <
                        state.dataConstruction->Construct(surf.Construction).PerimeterExposed - 0.1) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Floor Perimeter = {:.2T}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                surf.Perimeter));
                        ShowContinueError(state, "..which is less than its construction exposed perimeter.");
                    }
                }
            } // Getting Items
        }
    }

    void MakeRectangularVertices(EnergyPlusData &state,
                                 int const SurfNum,
                                 Real64 const XCoord,
                                 Real64 const YCoord,
                                 Real64 const ZCoord,
                                 Real64 const Length,
                                 Real64 const Height,
                                 bool const SurfWorldCoordSystem)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine creates world/3d coordinates for rectangular surfaces using azimuth, tilt, LLC (X,Y,Z), length & height.

        Vector3<Real64>LLC;
        
        Array1D<Vector2<Real64>> VV(4);
        Real64 Xb;
        Real64 Yb;
        Real64 Perimeter;
        int n;
        int Vrt;

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
        if (surf.Zone == 0 &&
            (surf.Class != SurfaceClass::Detached_F &&
             surf.Class != SurfaceClass::Detached_B))
            return;

        surf.Height = Height;
        surf.Width = Length;

        surf.CosAzim = std::cos(surf.Azimuth * Constant::DegToRadians);
        surf.SinAzim = std::sin(surf.Azimuth * Constant::DegToRadians);
        surf.CosTilt = std::cos(surf.Tilt * Constant::DegToRadians);
        surf.SinTilt = std::sin(surf.Tilt * Constant::DegToRadians);
        if (!SurfWorldCoordSystem) {
            if (surf.Zone > 0) {
                Xb = XCoord * state.dataSurfaceGeometry->CosZoneRelNorth(surf.Zone) -
                     YCoord * state.dataSurfaceGeometry->SinZoneRelNorth(surf.Zone) +
                     state.dataHeatBal->Zone(surf.Zone).OriginX;
                Yb = XCoord * state.dataSurfaceGeometry->SinZoneRelNorth(surf.Zone) +
                     YCoord * state.dataSurfaceGeometry->CosZoneRelNorth(surf.Zone) +
                     state.dataHeatBal->Zone(surf.Zone).OriginY;
                LLC.x = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                LLC.y = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                LLC.z = ZCoord + state.dataHeatBal->Zone(surf.Zone).OriginZ;
            } else {
                if (surf.Class == SurfaceClass::Detached_B) {
                    Xb = XCoord;
                    Yb = YCoord;
                    LLC.x = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                    LLC.y = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                    LLC.z = ZCoord;
                } else {
                    LLC.x = XCoord;
                    LLC.y = YCoord;
                    LLC.z = ZCoord;
                }
            }
        } else {
            // for world coordinates, only rotate for appendix G
            Xb = XCoord;
            Yb = YCoord;
            LLC.z = ZCoord;
            if (surf.Class != SurfaceClass::Detached_F) {
                LLC.x = Xb * state.dataSurfaceGeometry->CosBldgRotAppGonly - Yb * state.dataSurfaceGeometry->SinBldgRotAppGonly;
                LLC.y = Xb * state.dataSurfaceGeometry->SinBldgRotAppGonly + Yb * state.dataSurfaceGeometry->CosBldgRotAppGonly;
            } else {
                LLC.x = Xb;
                LLC.y = Yb;
            }
        }

        VV(1) = {0.0, Height};
        VV(2) = {0.0, 0.0};
        VV(3) = {Length, 0.0};
        VV(4) = {Length, Height};

        for (n = 1; n <= surf.Sides; ++n) {
            Vrt = n;
            surf.Vertex(Vrt).x = LLC.x - VV(n).x * surf.CosAzim - VV(n).y * surf.CosTilt * surf.SinAzim;
            surf.Vertex(Vrt).y = LLC.y + VV(n).x * surf.SinAzim - VV(n).y * surf.CosTilt * surf.CosAzim;
            surf.Vertex(Vrt).z = LLC.z + VV(n).y * surf.SinTilt;
        }

        surf.NewellAreaVec = Vectors::CalcNewellAreaVector(surf.Vertex, surf.Sides);
        surf.GrossArea = length(surf.NewellAreaVec);
        surf.Area = surf.GrossArea;
        surf.NetAreaShadowCalc = surf.Area;
        surf.NewellNormVec = Vectors::CalcNewellNormalVector(surf.Vertex, surf.Sides);
        std::tie(surf.Azimuth, surf.Tilt) = Vectors::CalcAzimuthAndTilt(surf.Vertex, surf.lcsx, surf.lcsy, surf.lcsz, surf.NewellNormVec);
        surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
        // Sine and cosine of azimuth and tilt
        surf.ViewFactorGround = 0.5 * (1.0 - surf.CosTilt);
        // Outward normal unit vector (pointing away from room)
        surf.OutNormVec = surf.NewellNormVec;
        auto &outNormVec = surf.OutNormVec;

        if (std::abs(outNormVec.x - 1.0) < 1.e-06) outNormVec.x = +1.0;
        if (std::abs(outNormVec.x + 1.0) < 1.e-06) outNormVec.x = -1.0;
        if (std::abs(outNormVec.x) < 1.e-06) outNormVec.x = 0.0;
        if (std::abs(outNormVec.y - 1.0) < 1.e-06) outNormVec.y = +1.0;
        if (std::abs(outNormVec.y + 1.0) < 1.e-06) outNormVec.y = -1.0;
        if (std::abs(outNormVec.y) < 1.e-06) outNormVec.y = 0.0;
        if (std::abs(outNormVec.z - 1.0) < 1.e-06) outNormVec.z = +1.0;
        if (std::abs(outNormVec.z + 1.0) < 1.e-06) outNormVec.z = -1.0;
        if (std::abs(outNormVec.z) < 1.e-06) outNormVec.z = 0.0;

        // Can perform tests on this surface here
        surf.ViewFactorSky = 0.5 * (1.0 + surf.CosTilt);
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        surf.ViewFactorSkyIR = surf.ViewFactorSky;
        surf.ViewFactorGroundIR = 0.5 * (1.0 - surf.CosTilt);

        Perimeter = distance(surf.Vertex(surf.Sides),
                             surf.Vertex(1));
        for (Vrt = 2; Vrt <= surf.Sides; ++Vrt) {
            Perimeter +=
                distance(surf.Vertex(Vrt), surf.Vertex(Vrt - 1));
        }
        surf.Perimeter = Perimeter;

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, surf.Sides);
    }

    void GetHTSubSurfaceData(EnergyPlusData &state,
                             bool &ErrorsFound,                       // Error flag indicator (true if errors found)
                             int &SurfNum,                            // Count of Current SurfaceNumber
                             int const TotHTSubs,                     // Number of Heat Transfer SubSurfaces to obtain
                             int &AddedSubSurfaces,                   // Subsurfaces added when windows reference Window5
                             int &NeedToAddSurfaces                   // Number of surfaces to add, based on unentered IZ surfaces
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       August 2012 - line up subsurfaces with base surface types
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the HeatTransfer Sub Surface Data,
        // checks it for errors, etc.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Heat Transfer Subsurface Definition
        // FenestrationSurface:Detailed,
        //        \min-fields 19
        //        \memo Used for windows, doors, glass doors, tubular daylighting devices
        //        \format vertices
        //   A1 , \field Name
        //        \required-field
        //        \type alpha
        //   A2 , \field Surface Type
        //        \required-field
        //        \type choice
        //        \key Window
        //        \key Door
        //        \key GlassDoor
        //        \key TubularDaylightDome
        //        \key TubularDaylightDiffuser
        //   A3 , \field Construction Name
        //        \required-field
        //        \note To be matched with a construction in this input file
        //        \type object-list
        //        \object-list ConstructionNames
        //   A4 , \field Building Surface Name
        //        \required-field
        //        \type object-list
        //        \object-list SurfaceNames
        //   A5,  \field Outside Boundary Condition Object
        //        \type object-list
        //        \object-list OutFaceEnvNames
        //        \note Non-blank only if base surface field Outside Boundary Condition is
        //        \note Surface or OtherSideCoefficients
        //        \note If Base Surface's Surface, specify name of corresponding subsurface in adjacent zone or
        //        \note specify current subsurface name for internal partition separating like zones
        //        \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
        //        \note  or leave blank to inherit Base Surface's OtherSide Coefficients
        //   N1, \field View Factor to Ground
        //        \type real
        //        \note From the exterior of the surface
        //        \note Unused if one uses the "reflections" options in Solar Distribution in Building input
        //        \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
        //        \note autocalculate will automatically calculate this value from the tilt of the surface
        //        \autocalculatable
        //        \minimum 0.0
        //        \maximum 1.0
        //        \default autocalculate
        //   A6, \field Frame and Divider Name
        //        \note Enter the name of a WindowProperty:FrameAndDivider object
        //        \type object-list
        //        \object-list WindowFrameAndDividerNames
        //        \note Used only for exterior windows (rectangular) and glass doors.
        //        \note Unused for triangular windows.
        //        \note If not specified (blank), window or glass door has no frame or divider
        //        \note and no beam solar reflection from reveal surfaces.
        //   N2 , \field Multiplier
        //        \note Used only for Surface Type = WINDOW, GLASSDOOR or DOOR
        //        \note Non-integer values will be truncated to integer
        //        \default 1.0
        //        \minimum 1.0
        //   N3 , \field Number of Vertices
        //        \minimum 3
        //        \maximum 4
        //        \autocalculatable
        //        \default autocalculate
        //        \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
        //        \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
        //        \note for some internal calculations, but all coordinates are given in an "absolute" system.
        //  N4-15 as indicated by the N3 value

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //  data file entry with two glazing systems

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;          // IO Status when calling get input subroutine
        int SurfaceNumAlpha; // Number of material alpha names being passed
        int SurfaceNumProp;  // Number of material properties being passed
        int Found;           // For matching interzone surfaces
        int Loop;
        int ValidChk;
        int numSides;

        constexpr std::string_view routineName = "GetHTSubSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        GetWindowShadingControlData(state, ErrorsFound);
        ipsc->cCurrentModuleObject = "FenestrationSurface:Detailed";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);

        if (SurfaceNumAlpha != 6) {
            ShowSevereError(
                state, format("{}: Object Definition indicates not = 6 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, SurfaceNumAlpha));
            ErrorsFound = true;
        }

        if (SurfaceNumProp != 15) {
            ShowSevereError(
                state, format("{}: Object Definition indicates > 15 Numeric Objects, Number Indicated={}", ipsc->cCurrentModuleObject, SurfaceNumAlpha));
            ErrorsFound = true;
        }
        NeedToAddSurfaces = 0;

        for (int Loop = 1; Loop <= TotHTSubs; ++Loop) {
            ipsc->cCurrentModuleObject = "FenestrationSurface:Detailed";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     SurfaceNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     SurfaceNumProp,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
            }
                
            ++SurfNum;
            sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

            auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
            if (SurfaceNumProp < 12) {
                ShowSevereError(state,
                                format("{}=\"{}\", Too few number of numeric args=[{}].",
                                       ipsc->cCurrentModuleObject,
                                       surf.Name,
                                       SurfaceNumProp));
                ErrorsFound = true;
            }

            surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
            ValidChk = Util::FindItemInList(ipsc->cAlphaArgs(2), sg->SubSurfCls, 6);
            if (ValidChk == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            } else {
                surf.Class = sg->SubSurfIDs(ValidChk); // Set class number
            }

            surf.Construction = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);

            if (surf.Construction == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
                continue;
            } else {
                state.dataConstruction->Construct(surf.Construction).IsUsed = true;
                surf.ConstructionStoredInputValue =
                    surf.Construction;
            }

            if (surf.Class == SurfaceClass::Window ||
                surf.Class == SurfaceClass::GlassDoor ||
                surf.Class == SurfaceClass::TDD_Diffuser ||
                surf.Class == SurfaceClass::TDD_Dome) {

                if (surf.Construction != 0) {
                    auto &construction = state.dataConstruction->Construct(surf.Construction);
                    if (!construction.TypeIsWindow && !construction.TypeIsAirBoundary) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\" has an opaque surface construction; it should have a window construction.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                    }
                    if (state.dataConstruction->Construct(surf.Construction).SourceSinkPresent) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\": Windows are not allowed to have embedded sources/sinks",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                    }
                }

            } else if (surf.Construction != 0) {
                if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {}=\"{}\" - has Window materials.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(state,
                                      format("...because {}={}", ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                }
            }

            surf.HeatTransSurf = true;

            surf.BaseSurfName = ipsc->cAlphaArgs(4);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            if (auto found = state.dataSurfaceGeometry->surfaceMap.find(surf.BaseSurfName); found != sg->surfaceMap.end()) {
                surf.BaseSurf = found->second;
                auto &baseSurf = state.dataSurfaceGeometry->SurfaceTmp(surf.BaseSurf);
                surf.ExtBoundCond = baseSurf.ExtBoundCond;
                surf.ExtBoundCondName = baseSurf.ExtBoundCondName;
                surf.ExtSolar = baseSurf.ExtSolar;
                surf.ExtWind = baseSurf.ExtWind;
                surf.Zone = baseSurf.Zone;
                surf.ZoneName = baseSurf.ZoneName;
                surf.OSCPtr = baseSurf.OSCPtr;
                if (baseSurf.ExtBoundCond == UnreconciledZoneSurface &&
                    baseSurf.ExtBoundCondName ==
                        baseSurf.Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {}=\"{}\".",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(4),
                                           ipsc->cAlphaArgs(4)));
                    ShowContinueError(state, "... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError(state,
                                      "... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {}=\"{}",
                                       ipsc->cCurrentModuleObject,
                                       surf.Name,
                                       ipsc->cAlphaFieldNames(4),
                                       ipsc->cAlphaArgs(4)));
                surf.ZoneName = "Unknown Zone";
                ErrorsFound = true;
            }
            if (surf.Class == SurfaceClass::TDD_Dome ||
                surf.Class == SurfaceClass::TDD_Diffuser) {
                surf.ExtBoundCond = ExternalEnvironment;
            }

            if (surf.ExtBoundCond == ExternalEnvironment) {
                if (!ipsc->lAlphaFieldBlanks(5)) {
                    ShowWarningError(state,
                                     format("{}=\"{}\", invalid field {}",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            ipsc->cAlphaFieldNames(5)));
                    ShowContinueError(
                        state,
                        format("...when Base surface uses \"Outdoors\" as {}, subsurfaces need to be blank to inherit the outdoor characteristics.",
                               ipsc->cAlphaFieldNames(5)));
                    ShowContinueError(state, "...Surface external characteristics changed to reflect base surface.");
                }
            }

            if (surf.ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                if (!ipsc->lAlphaFieldBlanks(5)) {
                    surf.ExtBoundCondName = ipsc->cAlphaArgs(5);
                } else {
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid blank {}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(5)));
                    ShowContinueError(
                        state,
                        format("...when Base surface uses \"Surface\" as {}, subsurfaces must also specify specific surfaces in the adjacent zone.",
                               ipsc->cAlphaFieldNames(5)));
                    surf.ExtBoundCondName =
                        ipsc->cAlphaArgs(5); // putting it as blank will not confuse things later.
                    ErrorsFound = true;
                }
            }

            if (surf.ExtBoundCond == UnenteredAdjacentZoneSurface) { // "Zone" - unmatched interior surface
                ++NeedToAddSurfaces;
                // ignoring window5datafiles for now -- will need to add.
            }

            if (surf.ExtBoundCond == OtherSideCoefNoCalcExt ||
                surf.ExtBoundCond == OtherSideCoefCalcExt) {
                if (!ipsc->lAlphaFieldBlanks(5)) { // Otherside Coef special Name
                    Found = Util::FindItemInList(ipsc->cAlphaArgs(5), state.dataSurface->OSC, state.dataSurface->TotOSC);
                    if (Found == 0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", invalid {}=\"{}\".",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cAlphaFieldNames(5),
                                               ipsc->cAlphaArgs(5)));
                        ShowContinueError(state, "...base surface requires that this subsurface have OtherSideCoefficients -- not found.");
                        ErrorsFound = true;
                    } else { // found
                        // The following allows for a subsurface that has different characteristics than
                        // the base surface with OtherSide Coeff -- do we want that or is it an error?
                        surf.OSCPtr = Found;
                        surf.ExtBoundCondName = ipsc->cAlphaArgs(5);
                        if (state.dataSurface->OSC(Found).SurfFilmCoef > 0.0) {
                            surf.ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            surf.ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }
                }
            }

            if (surf.ExtBoundCond == OtherSideCondModeledExt) {
                surf.ExtBoundCond = ExternalEnvironment;
            }

            if (surf.ExtBoundCondName == BlankString) {
                surf.ExtBoundCondName = surf.Name;
            }
            surf.ViewFactorGround = ipsc->rNumericArgs(1);
            if (ipsc->lNumericFieldBlanks(1))
                surf.ViewFactorGround = Constant::AutoCalculate;

            if (ipsc->lNumericFieldBlanks(3) || ipsc->rNumericArgs(3) == Constant::AutoCalculate) {
                ipsc->rNumericArgs(3) = (SurfaceNumProp - 3) / 3;
                surf.Sides = ipsc->rNumericArgs(3);
                if (mod(SurfaceNumProp - 3, 3) != 0) {
                    ShowWarningError(state,
                                     format("{}=\"{}\", {}",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            format("{} not even multiple of 3. Will read in {}",
                                                   ipsc->cNumericFieldNames(3),
                                                   surf.Sides)));
                }
                if (ipsc->rNumericArgs(3) < 3) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cNumericFieldNames(3),
                                           surf.Sides));
                    ErrorsFound = true;
                    continue;
                }
            } else {
                numSides = (SurfaceNumProp - 2) / 3;
                surf.Sides = ipsc->rNumericArgs(3);
                if (numSides > surf.Sides) {
                    ShowWarningError(state,
                                     format("{}=\"{}\", field {}={}",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            ipsc->cNumericFieldNames(3),
                                            fmt::to_string(surf.Sides)));
                    ShowContinueError(
                        state,
                        format("...but {} were entered. Only the indicated {} will be used.", numSides, ipsc->cNumericFieldNames(3)));
                }
            }
            surf.Vertex.allocate(surf.Sides);
            if (surf.Class == SurfaceClass::Window ||
                surf.Class == SurfaceClass::GlassDoor ||
                surf.Class == SurfaceClass::Door)
                surf.Multiplier = int(ipsc->rNumericArgs(2));
            // Only windows, glass doors and doors can have Multiplier > 1:
            if ((surf.Class != SurfaceClass::Window &&
                 surf.Class != SurfaceClass::GlassDoor &&
                 surf.Class != SurfaceClass::Door) &&
                ipsc->rNumericArgs(2) > 1.0) {
                ShowWarningError(state,
                                 format("{}=\"{}\", invalid {}=[{:.1T}].",
                                        ipsc->cCurrentModuleObject,
                                        surf.Name,
                                        ipsc->cNumericFieldNames(2),
                                        ipsc->rNumericArgs(2)));
                ShowContinueError(state,
                                  format("...because {}={} multiplier will be set to 1.0.",
                                         ipsc->cAlphaFieldNames(2),
                                         ipsc->cAlphaArgs(2)));
                surf.Multiplier = 1.0;
            }

            GetVertices(state, SurfNum, surf.Sides, ipsc->rNumericArgs({4, _}));

            CheckConvexity(state, SurfNum, surf.Sides);
            surf.windowShadingControlList.clear();
            surf.activeWindowShadingControl = 0;
            surf.HasShadeControl = false;

            surf.shadedConstructionList.clear();
            surf.activeShadedConstruction = 0;
            surf.shadedStormWinConstructionList.clear();

            if (surf.Class == SurfaceClass::Window ||
                surf.Class == SurfaceClass::GlassDoor ||
                surf.Class == SurfaceClass::TDD_Diffuser ||
                surf.Class == SurfaceClass::TDD_Dome) {

                if (surf.ExtBoundCond == OtherSideCoefNoCalcExt ||
                    surf.ExtBoundCond == OtherSideCoefCalcExt) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Other side coefficients are not allowed with windows.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name));
                    ErrorsFound = true;
                }

                if (surf.ExtBoundCond == Ground) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Exterior boundary condition = Ground is not allowed with windows.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name));
                    ErrorsFound = true;
                }

                if (surf.ExtBoundCond == KivaFoundation) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Exterior boundary condition = Foundation is not allowed with windows.",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name));
                    ErrorsFound = true;
                }

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                CheckWindowShadingControlFrameDivider(state, "GetHTSubSurfaceData", ErrorsFound, SurfNum, 6);

                if (surf.Sides == 3) { // Triangular window
                    if (!ipsc->cAlphaArgs(6).empty()) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", invalid {}=\"{}\".",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                ipsc->cAlphaFieldNames(6),
                                                ipsc->cAlphaArgs(6)));
                        ShowContinueError(state, ".. because it is a triangular window and cannot have a frame or divider or reveal reflection.");
                        ShowContinueError(state, "Frame, divider and reveal reflection will be ignored for this window.");
                    }
                    surf.FrameDivider = 0;
                } // End of check if window is triangular or rectangular

            } // check on non-opaquedoor subsurfaces

            CheckSubSurfaceMiscellaneous(state,
                                         "GetHTSubSurfaceData",
                                         ErrorsFound,
                                         SurfNum,
                                         ipsc->cAlphaArgs(1),
                                         ipsc->cAlphaArgs(3),
                                         AddedSubSurfaces);

        } // End of main loop over subsurfaces
    }

    void GetRectSubSurfaces(EnergyPlusData &state,
                            bool &ErrorsFound,                       // Error flag indicator (true if errors found)
                            int &SurfNum,                            // Count of Current SurfaceNumber
                            int const TotWindows,                    // Number of Window SubSurfaces to obtain
                            int const TotDoors,                      // Number of Door SubSurfaces to obtain
                            int const TotGlazedDoors,                // Number of Glass Door SubSurfaces to obtain
                            int const TotIZWindows,                  // Number of Interzone Window SubSurfaces to obtain
                            int const TotIZDoors,                    // Number of Interzone Door SubSurfaces to obtain
                            int const TotIZGlazedDoors,              // Number of Interzone Glass Door SubSurfaces to obtain
                            int &AddedSubSurfaces,                   // Subsurfaces added when windows reference Window5
                            int &NeedToAddSubSurfaces                // Number of surfaces to add, based on unentered IZ surfaces
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get simple (rectangular, relative origin to base surface) windows, doors, glazed doors.

        // Using/Aliasing

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //  data file entry with two glazing systems

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(6, {"Window", "Door", "GlazedDoor", "Window:Interzone", "Door:Interzone", "GlazedDoor:Interzone"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;
        int ItemsToGet;
        int Loop;
        int NumAlphas;
        int NumNumbers;
        int IOStat; // IO Status when calling get input subroutine
        int Found;  // For matching base surfaces
        bool GettingIZSurfaces;
        int WindowShadingField;
        int FrameField;
        int OtherSurfaceField;
        int ClassItem;
        int IZFound;

        constexpr std::string_view routineName = "GetRectSubSurfaces";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        for (int Item = 1; Item <= 6; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotWindows;
                GettingIZSurfaces = false;
                WindowShadingField = 4;
                FrameField = 5;
                OtherSurfaceField = 0;
                ClassItem = 1;
            } else if (Item == 2) {
                ItemsToGet = TotDoors;
                GettingIZSurfaces = false;
                WindowShadingField = 0;
                FrameField = 0;
                OtherSurfaceField = 0;
                ClassItem = 2;
            } else if (Item == 3) {
                ItemsToGet = TotGlazedDoors;
                GettingIZSurfaces = false;
                WindowShadingField = 4;
                FrameField = 5;
                OtherSurfaceField = 0;
                ClassItem = 3;
            } else if (Item == 4) {
                ItemsToGet = TotIZWindows;
                GettingIZSurfaces = true;
                WindowShadingField = 0;
                FrameField = 0;
                OtherSurfaceField = 4;
                ClassItem = 1;
            } else if (Item == 5) {
                ItemsToGet = TotIZDoors;
                GettingIZSurfaces = true;
                WindowShadingField = 0;
                FrameField = 0;
                OtherSurfaceField = 4;
                ClassItem = 2;
            } else { // Item = 6
                ItemsToGet = TotIZGlazedDoors;
                GettingIZSurfaces = true;
                WindowShadingField = 0;
                FrameField = 0;
                OtherSurfaceField = 4;
                ClassItem = 3;
            }

            for (int Loop = 1; Loop <= ItemsToGet; ++Loop) {
                ipsc->cCurrentModuleObject = cModuleObjects(Item);
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                if (NumNumbers < 5) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Too few number of numeric args=[{}].",
                                           ipsc->cCurrentModuleObject,
                                           ipsc->cAlphaArgs(1),
                                           NumNumbers));
                    ErrorsFound = true;
                }

                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                
                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                surf.Class = sg->SubSurfIDs(ClassItem);              // Set class number

                surf.Construction = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);

                if (surf.Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                } else {
                    state.dataConstruction->Construct(surf.Construction).IsUsed = true;
                    surf.ConstructionStoredInputValue = surf.Construction;
                }

                if (surf.Class == SurfaceClass::Window ||
                    surf.Class == SurfaceClass::GlassDoor) {

                    if (surf.Construction != 0) {
                        auto &construction = state.dataConstruction->Construct(surf.Construction);
                        if (!construction.TypeIsWindow && !construction.TypeIsAirBoundary) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" has an opaque surface construction; it should have a window construction.",
                                                   ipsc->cCurrentModuleObject,
                                                   surf.Name));
                        }
                        if (state.dataConstruction->Construct(surf.Construction).SourceSinkPresent) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\": Windows are not allowed to have embedded sources/sinks",
                                                   ipsc->cCurrentModuleObject,
                                                   surf.Name));
                        }
                    }

                } else if (surf.Construction != 0) {
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", invalid {}=\"{}\" - has Window materials.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cAlphaFieldNames(2),
                                               ipsc->cAlphaArgs(2)));
                    }
                }

                surf.HeatTransSurf = true;

                surf.BaseSurfName = ipsc->cAlphaArgs(3);
                //  The subsurface inherits properties from the base surface
                //  Exterior conditions, Zone, etc.
                //  We can figure out the base surface though, because they've all been entered
                if (auto found = state.dataSurfaceGeometry->surfaceMap.find(surf.BaseSurfName); found != state.dataSurfaceGeometry->surfaceMap.end()) {
                    surf.BaseSurf = found->second;
                    auto &baseSurf = state.dataSurfaceGeometry->SurfaceTmp(surf.BaseSurf);
                        
                    surf.ExtBoundCond = baseSurf.ExtBoundCond;
                    surf.ExtBoundCondName = baseSurf.ExtBoundCondName;
                    surf.ExtSolar = baseSurf.ExtSolar;
                    surf.ExtWind = baseSurf.ExtWind;
                    surf.Tilt = baseSurf.Tilt;
                    surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
                    surf.Azimuth = baseSurf.Azimuth;
                    surf.Zone = baseSurf.Zone;
                    surf.ZoneName = baseSurf.ZoneName;
                    surf.OSCPtr = baseSurf.OSCPtr;
                    surf.ViewFactorGround = baseSurf.ViewFactorGround;
                    surf.ViewFactorSky = baseSurf.ViewFactorSky;
                } else {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    surf.ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                    continue;
                }

                auto &baseSurf = state.dataSurfaceGeometry->SurfaceTmp(surf.BaseSurf);
                
                if (baseSurf.ExtBoundCond == UnreconciledZoneSurface &&
                    baseSurf.ExtBoundCondName == baseSurf.Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(state,
                                    format("{}=\"{}\", invalid {}=\"{}\".",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(3),
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(state, "... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError(state,
                                      "... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }

                if (surf.ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (!GettingIZSurfaces) {
                        ShowSevereError(
                            state,
                            format("{}=\"{}\", invalid use of object", ipsc->cCurrentModuleObject, surf.Name));
                        ShowContinueError(
                            state,
                            format(
                                "...when Base surface uses \"Surface\" as {}, subsurfaces must also specify specific surfaces in the adjacent zone.",
                                ipsc->cAlphaFieldNames(5)));
                        ShowContinueError(state, format("...Please use {}:Interzone to enter this surface.", ipsc->cCurrentModuleObject));
                        surf.ExtBoundCondName =
                            BlankString; // putting it as blank will not confuse things later.
                        ErrorsFound = true;
                    }
                }

                if (surf.ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (GettingIZSurfaces) {
                        surf.ExtBoundCondName = ipsc->cAlphaArgs(OtherSurfaceField);
                        IZFound = Util::FindItemInList(
                            surf.ExtBoundCondName, state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
                        if (IZFound > 0) surf.ExtBoundCond = UnenteredAdjacentZoneSurface;
                    } else { // Interior Window
                        surf.ExtBoundCondName = surf.Name;
                    }
                }

                // This is the parent's property:
                if (surf.ExtBoundCond ==
                    UnenteredAdjacentZoneSurface) { // OtherZone - unmatched interior surface
                    if (GettingIZSurfaces) {
                        ++NeedToAddSubSurfaces;
                    } else { // Interior Window
                        ShowSevereError(state,
                                        format("{}=\"{}\", invalid Interzone Surface, specify {}:InterZone",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cCurrentModuleObject));
                        ShowContinueError(state, "...when base surface is an interzone surface, subsurface must also be an interzone surface.");
                        ++NeedToAddSubSurfaces;
                        ErrorsFound = true;
                    }
                }

                if (GettingIZSurfaces) {
                    if (ipsc->lAlphaFieldBlanks(OtherSurfaceField)) {
                        // blank -- set it up for unentered adjacent zone
                        if (surf.ExtBoundCond ==
                            UnenteredAdjacentZoneSurface) { // already set but need Zone
                            surf.ExtBoundCondName =
                                baseSurf.ExtBoundCondName; // base surface has it
                        } else if (surf.ExtBoundCond == UnreconciledZoneSurface) {
                            surf.ExtBoundCondName =
                                baseSurf.ZoneName; // base surface has it
                            surf.ExtBoundCond = UnenteredAdjacentZoneSurface;
                        } else { // not correct boundary condition for interzone subsurface
                            ShowSevereError(state,
                                            format("{}=\"{}\", invalid Base Surface type for Interzone Surface",
                                                   ipsc->cCurrentModuleObject,
                                                   surf.Name));
                            ShowContinueError(state,
                                              "...when base surface is not an interzone surface, subsurface must also not be an interzone surface.");
                            ErrorsFound = true;
                        }
                    }
                }

                if (surf.ExtBoundCond == OtherSideCondModeledExt) {
                    surf.ExtBoundCond = ExternalEnvironment;
                }

                //      SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

                surf.Sides = 4;
                surf.Vertex.allocate(surf.Sides);
                if (surf.Class == SurfaceClass::Window ||
                    surf.Class == SurfaceClass::GlassDoor ||
                    surf.Class == SurfaceClass::Door)
                    surf.Multiplier = int(ipsc->rNumericArgs(1));
                // Only windows, glass doors and doors can have Multiplier > 1:
                if ((surf.Class != SurfaceClass::Window &&
                     surf.Class != SurfaceClass::GlassDoor &&
                     surf.Class != SurfaceClass::Door) &&
                    ipsc->rNumericArgs(1) > 1.0) {
                    ShowWarningError(state,
                                     format("{}=\"{}\", invalid {}=[{:.1T}].",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            ipsc->cNumericFieldNames(1),
                                            ipsc->rNumericArgs(1)));
                    ShowContinueError(state,
                                      format("...because {}={} multiplier will be set to 1.0.",
                                             ipsc->cAlphaFieldNames(1),
                                             ipsc->cAlphaArgs(1)));
                    surf.Multiplier = 1.0;
                }

                MakeRelativeRectangularVertices(state,
                                                surf.BaseSurf,
                                                SurfNum,
                                                ipsc->rNumericArgs(2),
                                                ipsc->rNumericArgs(3),
                                                ipsc->rNumericArgs(4),
                                                ipsc->rNumericArgs(5));

                if (surf.Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           surf.Area));
                    ErrorsFound = true;
                }

                surf.windowShadingControlList.clear();
                surf.activeWindowShadingControl = 0;
                surf.HasShadeControl = false;

                surf.shadedConstructionList.clear();
                surf.activeShadedConstruction = 0;
                surf.shadedStormWinConstructionList.clear();

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                if (!GettingIZSurfaces && (surf.Class == SurfaceClass::Window ||
                                           surf.Class == SurfaceClass::GlassDoor)) {

                    if (surf.ExtBoundCond == OtherSideCoefNoCalcExt ||
                        surf.ExtBoundCond == OtherSideCoefCalcExt) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", Other side coefficients are not allowed with windows.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                        ErrorsFound = true;
                    }

                    if (surf.ExtBoundCond == Ground) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", Exterior boundary condition = Ground is not allowed with windows.",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name));
                        ErrorsFound = true;
                    }

                    CheckWindowShadingControlFrameDivider(state, "GetRectSubSurfaces", ErrorsFound, SurfNum, FrameField);

                } // check on non-opaquedoor subsurfaces

                CheckSubSurfaceMiscellaneous(state,
                                             "GetRectSubSurfaces",
                                             ErrorsFound,
                                             SurfNum,
                                             ipsc->cAlphaArgs(1),
                                             ipsc->cAlphaArgs(2),
                                             AddedSubSurfaces);

            } // Getting Items
        }
    }

    void CheckWindowShadingControlFrameDivider(EnergyPlusData &state,
                                               std::string_view const cRoutineName, // routine name calling this one (for error messages)
                                               bool &ErrorsFound,                   // true if errors have been found or are found here
                                               int const SurfNum,                   // current surface number
                                               int const FrameField                 // field number for frame/divider
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine performs checks on WindowShadingControl settings and Frame/Divider Settings.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstrNumSh;    // Construction number with Shade
        int ConstrNum;      // Construction number
        int ShDevNum;       // Shading Device number
        int Lay;            // Layer number
        int TotGlassLayers; // Number of glass layers in window construction
        int TotLayers;      // Number of layers in unshaded construction
        int TotShLayers;    // Number of layers in shaded construction
        int MatGap;         // Gap material number
        int MatGap1;        // Material number of gap to left (outer side) of between-glass shade/blind
        int MatGap2;        // Material number of gap to right (inner side) of between-glass shade/blind
        int MatSh;          // Between-glass shade/blind material number
        Real64 MatGapCalc;  // Calculated MatGap diff for shaded vs non-shaded constructions

        constexpr std::string_view routineName = "CheckWindowShadingControlFrameDivider";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        // If WindowShadingControl has been specified for this window --
        // Set shaded construction number if shaded construction was specified in WindowShadingControl.
        // Otherwise, create shaded construction if WindowShadingControl for this window has
        // interior or exterior shade/blind (but not between-glass shade/blind) specified.

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
        
        for (std::size_t shadeControlIndex = 0; shadeControlIndex < surf.windowShadingControlList.size();
             ++shadeControlIndex) {
            int WSCPtr = surf.windowShadingControlList[shadeControlIndex];
            ConstrNumSh = 0;
            if (!ErrorsFound && surf.HasShadeControl) {
                ConstrNumSh = surf.shadedConstructionList[shadeControlIndex];
                if (ConstrNumSh > 0) {
                    surf.activeShadedConstruction = ConstrNumSh;
                } else {
                    if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType) ||
                        ANY_EXTERIOR_SHADE_BLIND_SCREEN(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                        ShDevNum = state.dataSurface->WindowShadingControl(WSCPtr).ShadingDevice;
                        if (ShDevNum > 0) {
                            CreateShadedWindowConstruction(state, SurfNum, WSCPtr, ShDevNum, shadeControlIndex);
                            ConstrNumSh = surf.activeShadedConstruction;
                        }
                    }
                }
            }

            // Error checks for shades and blinds

            ConstrNum = surf.Construction;
            if (!ErrorsFound && WSCPtr > 0 && ConstrNum > 0 && ConstrNumSh > 0) {

                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 1 != TotLayers) {
                        ShowWarningError(
                            state,
                            "WindowShadingControl: Interior shade or blind: Potential problem in match of unshaded/shaded constructions, "
                            "shaded should have 1 more layers than unshaded.");
                        ShowContinueError(state, format("Unshaded construction={}", state.dataConstruction->Construct(ConstrNum).Name));
                        ShowContinueError(state, format("Shaded construction={}", state.dataConstruction->Construct(ConstrNumSh).Name));
                        ShowContinueError(state,
                                          "If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) !=
                            state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay)) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format(" The glass and gas layers in the shaded and unshaded constructions do not match for window={}",
                                                   surf.Name));
                            ShowContinueError(state, format("Unshaded construction={}", state.dataConstruction->Construct(ConstrNum).Name));
                            ShowContinueError(state, format("Shaded construction={}", state.dataConstruction->Construct(ConstrNumSh).Name));
                            break;
                        }
                    }
                }

                if (ANY_EXTERIOR_SHADE_BLIND_SCREEN(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 1 != TotLayers) {
                        ShowWarningError(state,
                                         "WindowShadingControl: Exterior shade, screen or blind: Potential problem in match of unshaded/shaded "
                                         "constructions, shaded should have 1 more layer than unshaded.");
                        ShowContinueError(state, format("Unshaded construction={}", state.dataConstruction->Construct(ConstrNum).Name));
                        ShowContinueError(state, format("Shaded construction={}", state.dataConstruction->Construct(ConstrNumSh).Name));
                        ShowContinueError(
                            state,
                            "If preceding two constructions have the same name, you have likely specified a WindowShadingControl (Field "
                            "#3) with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) !=
                            state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay + 1)) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format(" The glass and gas layers in the shaded and unshaded constructions do not match for window={}",
                                                   surf.Name));
                            ShowContinueError(state, format("Unshaded construction={}", state.dataConstruction->Construct(ConstrNum).Name));
                            ShowContinueError(state, format("Shaded construction={}", state.dataConstruction->Construct(ConstrNumSh).Name));
                            break;
                        }
                    }
                }

                if (ANY_BETWEENGLASS_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                    // Divider not allowed with between-glass shade or blind
                    if (surf.FrameDivider > 0) {
                        if (state.dataSurface->FrameDivider(surf.FrameDivider).DividerWidth > 0.0) {
                            ShowWarningError(
                                state, format("A divider cannot be specified for window {}", surf.Name));
                            ShowContinueError(state, ", which has a between-glass shade or blind.");
                            ShowContinueError(state, "Calculation will proceed without the divider for this window.");
                            state.dataSurface->FrameDivider(surf.FrameDivider).DividerWidth = 0.0;
                        }
                    }
                    // Check consistency of gap widths between unshaded and shaded constructions
                    TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 2 != TotLayers) {
                        ShowWarningError(
                            state,
                            "WindowShadingControl: Between Glass Shade/Blind: Potential problem in match of unshaded/shaded constructions, "
                            "shaded should have 2 more layers than unshaded.");
                        ShowContinueError(state, format("Unshaded construction={}", state.dataConstruction->Construct(ConstrNum).Name));
                        ShowContinueError(state, format("Shaded construction={}", state.dataConstruction->Construct(ConstrNumSh).Name));
                        ShowContinueError(state,
                                          "If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers) !=
                        state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers)) {
                        ShowSevereError(state, format("{}: Mis-match in unshaded/shaded inside layer materials.  These should match.", cRoutineName));
                        ShowContinueError(
                            state,
                            format("Unshaded construction={}, Material={}",
                                   state.dataConstruction->Construct(ConstrNum).Name,
                                   state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers))->Name));
                        ShowContinueError(
                            state,
                            format("Shaded construction={}, Material={}",
                                   state.dataConstruction->Construct(ConstrNumSh).Name,
                                   state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers))->Name));
                        ErrorsFound = true;
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(1) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)) {
                        ShowSevereError(state, format("{}: Mis-match in unshaded/shaded inside layer materials.  These should match.", cRoutineName));
                        ShowContinueError(state,
                                          format("Unshaded construction={}, Material={}",
                                                 state.dataConstruction->Construct(ConstrNum).Name,
                                                 state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1))->Name));
                        ShowContinueError(state,
                                          format("Shaded construction={}, Material={}",
                                                 state.dataConstruction->Construct(ConstrNumSh).Name,
                                                 state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1))->Name));
                        ErrorsFound = true;
                    }
                    if (TotGlassLayers == 2 || TotGlassLayers == 3) {
                        MatGap = state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers);
                        MatSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 1);
                        if (state.dataSurface->WindowShadingControl(WSCPtr).ShadingType == WinShadingType::BGBlind) {
                            MatGapCalc =
                                std::abs(state.dataMaterial->Material(MatGap)->Thickness -
                                         (state.dataMaterial->Material(MatGap1)->Thickness + state.dataMaterial->Material(MatGap2)->Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(state,
                                                format("{}: The gap width(s) for the unshaded window construction {}",
                                                       cRoutineName,
                                                       state.dataConstruction->Construct(ConstrNum).Name));
                                ShowContinueError(state,
                                                  "are inconsistent with the gap widths for shaded window construction " +
                                                      state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError(state,
                                                  "for window " + surf.Name +
                                                      ", which has a between-glass blind.");
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} -",
                                                         state.dataMaterial->Material(MatGap)->Name,
                                                         state.dataMaterial->Material(MatGap)->Thickness));
                                ShowContinueError(state,
                                                  format("..( Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap1)->Name,
                                                         state.dataMaterial->Material(MatGap1)->Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} )=[{:.3R}] >.001",
                                                         state.dataMaterial->Material(MatGap2)->Name,
                                                         state.dataMaterial->Material(MatGap2)->Thickness,
                                                         MatGapCalc));
                                ErrorsFound = true;
                            }
                        } else { // Between-glass shade
                            MatGapCalc =
                                std::abs(state.dataMaterial->Material(MatGap)->Thickness -
                                         (state.dataMaterial->Material(MatGap1)->Thickness + state.dataMaterial->Material(MatGap2)->Thickness +
                                          state.dataMaterial->Material(MatSh)->Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(state,
                                                format("{}: The gap width(s) for the unshaded window construction {}",
                                                       cRoutineName,
                                                       state.dataConstruction->Construct(ConstrNum).Name));
                                ShowContinueError(state,
                                                  "are inconsistent with the gap widths for shaded window construction " +
                                                      state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError(state,
                                                  "for window " + surf.Name +
                                                      ", which has a between-glass shade.");
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} -",
                                                         state.dataMaterial->Material(MatGap)->Name,
                                                         state.dataMaterial->Material(MatGap)->Thickness));
                                ShowContinueError(state,
                                                  format("...( Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap1)->Name,
                                                         state.dataMaterial->Material(MatGap1)->Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap2)->Name,
                                                         state.dataMaterial->Material(MatGap2)->Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} )=[{:.3R}] >.001",
                                                         state.dataMaterial->Material(MatSh)->Name,
                                                         state.dataMaterial->Material(MatSh)->Thickness,
                                                         MatGapCalc));
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
        }
        
        auto &cCurrentModuleObject = ipsc->cCurrentModuleObject;
        if (surf.Sides != 3) { // Rectangular Window
            // Initialize the FrameDivider number for this window. W5FrameDivider will be positive if
            // this window's construction came from the Window5 data file and that construction had an
            // associated frame or divider. It will be zero if the window's construction is not from the
            // Window5 data file, or the construction is from the data file, but the construction has no
            // associated frame or divider. Note that if there is a FrameDivider candidate for this
            // window from the Window5 data file it is used instead of the window's input FrameDivider.

            if (surf.Construction != 0) {
                surf.FrameDivider =
                    state.dataConstruction->Construct(surf.Construction).W5FrameDivider;

                // Warning if FrameAndDivider for this window is over-ridden by one from Window5 Data File
                if (surf.FrameDivider > 0 && !ipsc->lAlphaFieldBlanks(FrameField)) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", {}=\"{}\"",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           ipsc->cAlphaFieldNames(FrameField),
                                           ipsc->cAlphaArgs(FrameField)));
                    ShowContinueError(state,
                                      format("will be replaced with FrameAndDivider from Window5 Data File entry {}",
                                             state.dataConstruction->Construct(surf.Construction).Name));
                }

                if (!ipsc->lAlphaFieldBlanks(FrameField) && surf.FrameDivider == 0) {
                    surf.FrameDivider =
                        Util::FindItemInList(ipsc->cAlphaArgs(FrameField), state.dataSurface->FrameDivider);
                    if (surf.FrameDivider == 0) {
                        if (!state.dataConstruction->Construct(surf.Construction).WindowTypeEQL) {
                            ShowSevereError(state,
                                            format("{}=\"{}\", invalid {}=\"{}\"",
                                                   cCurrentModuleObject,
                                                   surf.Name,
                                                   ipsc->cAlphaFieldNames(FrameField),
                                                   ipsc->cAlphaArgs(FrameField)));
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(state,
                                            format("{}=\"{}\", invalid {}=\"{}\"",
                                                   cCurrentModuleObject,
                                                   surf.Name,
                                                   ipsc->cAlphaFieldNames(FrameField),
                                                   ipsc->cAlphaArgs(FrameField)));
                            ShowContinueError(state, "...Frame/Divider is not supported in Equivalent Layer Window model.");
                        }
                    }
                    // Divider not allowed with between-glass shade or blind
                    for (int WSCPtr : surf.windowShadingControlList) {
                        if (!ErrorsFound && WSCPtr > 0 && ConstrNumSh > 0) {
                            if (ANY_BETWEENGLASS_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                                if (surf.FrameDivider > 0) {
                                    if (state.dataSurface->FrameDivider(surf.FrameDivider).DividerWidth >
                                        0.0) {
                                        ShowSevereError(state,
                                                        format("{}=\"{}\", invalid {}=\"{}\"",
                                                               cCurrentModuleObject,
                                                               surf.Name,
                                                               ipsc->cAlphaFieldNames(FrameField),
                                                               ipsc->cAlphaArgs(FrameField)));
                                        ShowContinueError(state,
                                                          "Divider cannot be specified because the construction has a between-glass shade or blind.");
                                        ShowContinueError(state, "Calculation will proceed without the divider for this window.");
                                        ShowContinueError(
                                            state,
                                            format("Divider width = [{:.2R}].",
                                                   state.dataSurface->FrameDivider(surf.FrameDivider)
                                                       .DividerWidth));
                                        state.dataSurface->FrameDivider(surf.FrameDivider).DividerWidth =
                                            0.0;
                                    }
                                } // End of check if window has divider
                            }     // End of check if window has a between-glass shade or blind
                        }         // End of check if window has a shaded construction
                    }             // end of looping through window shading controls of window
                }                 // End of check if window has an associated FrameAndDivider
            }                     // End of check if window has a construction
        }

        if (state.dataConstruction->Construct(surf.Construction).WindowTypeEQL) {
            if (surf.FrameDivider > 0) {
                // Equivalent Layer window does not have frame/divider model
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {}=\"{}\"",
                                       cCurrentModuleObject,
                                       surf.Name,
                                       ipsc->cAlphaFieldNames(FrameField),
                                       ipsc->cAlphaArgs(FrameField)));
                ShowContinueError(state, "Frame/Divider is not supported in Equivalent Layer Window model.");
                surf.FrameDivider = 0;
            }
        }
    }

    void CheckSubSurfaceMiscellaneous(EnergyPlusData &state,
                                      std::string_view const cRoutineName,       // routine name calling this one (for error messages)
                                      bool &ErrorsFound,                         // true if errors have been found or are found here
                                      int const SurfNum,                         // current surface number
                                      std::string const &SubSurfaceName,         // name of the surface
                                      std::string const &SubSurfaceConstruction, // name of the construction
                                      int &AddedSubSurfaces)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine performs miscellaneous checks on subsurfaces: Windows, GlassDoors, Doors, Tubular Devices.

        // Using/Aliasing

        using namespace DataErrorTracking;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumShades; // count on number of shading layers
        int Lay;       // Layer number
        int LayerPtr;  // Layer pointer
        int ConstrNum; // Construction number
        int Found;     // when item is found

        auto &sg = state.dataSurfaceGeometry;
        auto &surf = sg->SurfaceTmp(SurfNum);
        // Warning if window has multiplier > 1 and SolarDistribution = FullExterior or FullInteriorExterior

        if ((surf.Class == SurfaceClass::Window || surf.Class == SurfaceClass::GlassDoor) &&
            (state.dataHeatBal->SolarDistribution != DataHeatBalance::Shadowing::Minimal) && 
            surf.Multiplier > 1.0) {
            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(
                    state,
                    format("{}: A Multiplier > 1.0 for window/glass door {}", cRoutineName, surf.Name));
                ShowContinueError(state, "in conjunction with SolarDistribution = FullExterior or FullInteriorExterior");
                ShowContinueError(state, "can cause inaccurate shadowing on the window and/or");
                ShowContinueError(state, "inaccurate interior solar distribution from the window.");
            }
            ++state.dataErrTracking->TotalMultipliedWindows;
        }

        //  Require that a construction referenced by a surface that is a window
        //  NOT have a shading device layer; use WindowShadingControl to specify a shading device.
        ConstrNum = surf.Construction;
        if (ConstrNum > 0) {
            NumShades = 0;
            for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                if (LayerPtr == 0) continue; // Error is caught already, will terminate later
                if (state.dataMaterial->Material(LayerPtr)->group == Material::Group::Shade ||
                    state.dataMaterial->Material(LayerPtr)->group == Material::Group::WindowBlind ||
                    state.dataMaterial->Material(LayerPtr)->group == Material::Group::Screen)
                    ++NumShades;
            }
            if (NumShades != 0) {
                ShowSevereError(state, format("{}: Window \"{}\" must not directly reference", cRoutineName, SubSurfaceName));
                ShowContinueError(state, format("a Construction (i.e, \"{}\") with a shading device.", SubSurfaceConstruction));
                ShowContinueError(state, "Use WindowShadingControl to specify a shading device for a window.");
                ErrorsFound = true;
            }
        }

        // Disallow glass transmittance dirt factor for interior windows and glass doors

        if (surf.ExtBoundCond != ExternalEnvironment &&
            (surf.Class == SurfaceClass::Window ||
             surf.Class == SurfaceClass::GlassDoor)) {
            ConstrNum = surf.Construction;
            if (ConstrNum > 0) {
                for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                    LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                    auto const *thisMaterial = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(LayerPtr));
                    assert(thisMaterial != nullptr);
                    if (thisMaterial->group == Material::Group::WindowGlass && thisMaterial->GlassTransDirtFactor < 1.0) {
                        ShowSevereError(state, format("{}: Interior Window or GlassDoor {} has a glass layer with", cRoutineName, SubSurfaceName));
                        ShowContinueError(state, "Dirt Correction Factor for Solar and Visible Transmittance < 1.0");
                        ShowContinueError(state, "A value less than 1.0 for this factor is only allowed for exterior windows and glass doors.");
                        ErrorsFound = true;
                    }
                }
            }
        }

        // If this is a window with a construction from the Window5DataFile, call routine that will
        // (1) if one glazing system on Data File, give warning message if window height or width
        //     differ by more than 10% from those of the glazing system on the Data File;
        // (2) if two glazing systems (separated by a mullion) on Data File, create a second window
        //     and adjust the dimensions of the original and second windows to those on the Data File

        if (surf.Construction != 0) {

            if (state.dataConstruction->Construct(surf.Construction).FromWindow5DataFile) {

                ModifyWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else {
                // Calculate net area for base surface (note that ModifyWindow, above, adjusts net area of
                // base surface for case where window construction is from Window5 Data File
                // In case there is in error in this window's base surface (i.e. none)..
                if (surf.BaseSurf > 0) {
                    auto &baseSurf = state.dataSurfaceGeometry->SurfaceTmp(surf.BaseSurf);
                    baseSurf.Area -= surf.Area;

                    // Subtract TDD:DIFFUSER area from other side interzone surface
                    if (surf.Class == SurfaceClass::TDD_Diffuser &&
                        !baseSurf.ExtBoundCondName.empty()) { // Base surface is an interzone surface
                        // Lookup interzone surface of the base surface
                        // (Interzone surfaces have not been assigned yet, but all base surfaces should already be loaded.)
                        if (auto found = sg->surfaceMap.find(baseSurf.ExtBoundCondName); found != sg->surfaceMap.end()) {
                            sg->SurfaceTmp(found->second).Area -= surf.Area;
                        }
                    }
                    if (baseSurf.Area <= 0.0) {
                        ShowSevereError(state,
                                        format("{}: Surface Openings have too much area for base surface={}",
                                               cRoutineName,
                                               baseSurf.Name));
                        ShowContinueError(state, format("Opening Surface creating error={}", surf.Name));
                        ErrorsFound = true;
                    }
                    // Net area of base surface with unity window multipliers (used in shadowing checks)
                    // For Windows, Glass Doors and Doors, just one area is subtracted.  For the rest, should be
                    // full area.
                    if (surf.Class == SurfaceClass::Window ||
                        surf.Class == SurfaceClass::GlassDoor) {
                        baseSurf.NetAreaShadowCalc -= surf.Area / surf.Multiplier;
                    } else if (surf.Class == SurfaceClass::Door) { // Door, TDD:Diffuser, TDD:DOME
                        baseSurf.NetAreaShadowCalc -= surf.Area / surf.Multiplier;
                    } else {
                        baseSurf.NetAreaShadowCalc -= surf.Area;
                    }
                }
            }
        } // if (surf.Construction != 0)
    } // CheckSubSurfaceMiscellaneous()

    void MakeRelativeRectangularVertices(EnergyPlusData &state,
                                         int const BaseSurfNum, // Base surface
                                         int const SurfNum,
                                         Real64 const XCoord,
                                         Real64 const ZCoord,
                                         Real64 const Length,
                                         Real64 const Height)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   December 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine creates world/3d coordinates for rectangular surfaces using relative X and Z, length & height.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace Vectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Vector3<Real64> LLC;
        Array1D<Vector2<Real64>> VV(4);
        Real64 Perimeter;
        int n;
        int Vrt;

        if (BaseSurfNum == 0) return; // invalid base surface, don't bother

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);;
        auto &baseSurf = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum);
        // Tilt and Facing (Azimuth) will be same as the Base Surface

        surf.Height = Height;
        surf.Width = Length;

        surf.CosAzim = std::cos(surf.Azimuth * Constant::DegToRadians);
        surf.SinAzim = std::sin(surf.Azimuth * Constant::DegToRadians);
        surf.CosTilt = std::cos(surf.Tilt * Constant::DegToRadians);
        surf.SinTilt = std::sin(surf.Tilt * Constant::DegToRadians);

        LLC.x = baseSurf.Vertex(2).x - XCoord * baseSurf.CosAzim - ZCoord * baseSurf.CosTilt * baseSurf.SinAzim;
        LLC.y = baseSurf.Vertex(2).y + XCoord * baseSurf.SinAzim - ZCoord * baseSurf.CosTilt * baseSurf.CosAzim;
        LLC.z = baseSurf.Vertex(2).z + ZCoord * baseSurf.SinTilt;

        VV(1) = {0.0, Height};
        VV(2) = {0.0, 0.0};
        VV(3) = {Length, 0.0};
        VV(4) = {Length, Height};

        for (int n = 1; n <= surf.Sides; ++n) {
            Vrt = n;
            surf.Vertex(Vrt).x = LLC.x - VV(n).x * surf.CosAzim - VV(n).y * surf.CosTilt * surf.SinAzim;
            surf.Vertex(Vrt).y = LLC.y + VV(n).x * surf.SinAzim - VV(n).y * surf.CosTilt * surf.CosAzim;
            surf.Vertex(Vrt).z = LLC.z + VV(n).y * surf.SinTilt;
        }

        surf.NewellAreaVec = CalcNewellAreaVector(surf.Vertex, surf.Sides);
        surf.GrossArea = length(surf.NewellAreaVec);
        surf.Area = surf.GrossArea;
        surf.NetAreaShadowCalc = surf.Area;
        surf.NewellNormVec = CalcNewellNormalVector(surf.Vertex, surf.Sides);

        std::tie(surf.Azimuth, surf.Tilt) = CalcAzimuthAndTilt(surf.Vertex, surf.lcsx, surf.lcsy, surf.lcsz, surf.NewellNormVec);
        surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);

        if (surf.Class != SurfaceClass::Window &&
            surf.Class != SurfaceClass::GlassDoor &&
            surf.Class != SurfaceClass::Door)
            surf.ViewFactorGround = 0.5 * (1.0 - surf.CosTilt);
        // Outward normal unit vector (pointing away from room)
        surf.OutNormVec = surf.NewellNormVec;
        auto &outNormVec = surf.OutNormVec;
        if (std::abs(outNormVec.x - 1.0) < 1.e-06) outNormVec.x = +1.0;
        if (std::abs(outNormVec.x + 1.0) < 1.e-06) outNormVec.x = -1.0;
        if (std::abs(outNormVec.x) < 1.e-06) outNormVec.x = 0.0;
        if (std::abs(outNormVec.y - 1.0) < 1.e-06) outNormVec.y = +1.0;
        if (std::abs(outNormVec.y + 1.0) < 1.e-06) outNormVec.y = -1.0;
        if (std::abs(outNormVec.y) < 1.e-06) outNormVec.y = 0.0;
        if (std::abs(outNormVec.z - 1.0) < 1.e-06) outNormVec.z = +1.0;
        if (std::abs(outNormVec.z + 1.0) < 1.e-06) outNormVec.z = -1.0;
        if (std::abs(outNormVec.z) < 1.e-06) outNormVec.z = 0.0;

        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Roof .and. SurfTilt > 80.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError(state, format("Roof/Ceiling Tilt={}{}{}{}{}{}{}{}{}{} for Surface={}{}{}, in
        //    Zone={}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}{}", //TRIM(TiltString)//',,
        //    much, greater, than, expected, tilt, of, 0,'//, &, //, //TRIM(SurfaceTmp(SurfNum)%Name)//, &, //, //TRIM(SurfaceTmp(SurfNum)%ZoneName)),
        //    //, ENDIF, //, IF, (SurfaceTmp(SurfNum)%Class, ==, SurfaceClass::Floor, .and., SurfTilt, <, 170.), THEN, //, WRITE(TiltString,'(F5.1)'),
        //    SurfTilt, //, TiltString=ADJUSTL(TiltString), //, CALL, ShowWarningError(state, 'Floor Tilt='//TRIM(TiltString)//', much less than
        //    expected tilt of 180,'//   &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName)), //, ENDIF, if,
        //                          (surf.Class, ==, SurfaceClass::Window, ||,
        //                          surf.Class, ==, SurfaceClass::GlassDoor, ||,
        //                          surf.Class, ==, SurfaceClass::Door),
        //                          surf.Area, *=,
        //                          surf.Multiplier;, //, Can, perform, tests, on, this, surface, here,
        //                          surf.ViewFactorSky, =, 0.5, *, (1.0,
        //                          surf.CosTilt));
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        surf.ViewFactorSkyIR = surf.ViewFactorSky;
        surf.ViewFactorGroundIR = 0.5 * (1.0 - surf.CosTilt);

        surf.Perimeter = distance(surf.Vertex(surf.Sides), surf.Vertex(1));
        for (Vrt = 2; Vrt <= surf.Sides; ++Vrt) {
            surf.Perimeter += distance(surf.Vertex(Vrt), surf.Vertex(Vrt - 1));
        }

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, surf.Sides);
    }

    void GetAttShdSurfaceData(EnergyPlusData &state,
                              bool &ErrorsFound,   // Error flag indicator (true if errors found)
                              int &SurfNum,        // Count of Current SurfaceNumber
                              int const TotShdSubs // Number of Attached Shading SubSurfaces to obtain
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the HeatTransfer Surface Data,
        // checks it for errors, etc.

        // Using/Aliasing
        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;     // IO Status when calling get input subroutine
        int NumAlphas;  // Number of alpha names being passed
        int NumNumbers; // Number of properties being passed
        int Found;      // For matching interzone surfaces
        int Loop;

        constexpr std::string_view routineName = "GetAttShdSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        if (TotShdSubs > 0 && state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
            ShowWarningError(state, "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
        }

        ipsc->cCurrentModuleObject = "Shading:Zone:Detailed";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
        if (NumAlphas != 3) {
            ShowSevereError(state,
                            format("{}: Object Definition indicates not = 3 Alpha Objects, Number Indicated={}", ipsc->cCurrentModuleObject, NumAlphas));
            ErrorsFound = true;
        }

        for (int Loop = 1; Loop <= TotShdSubs; ++Loop) {
            ipsc->cCurrentModuleObject = "Shading:Zone:Detailed";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }
            
            ++SurfNum;
            sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

            auto &surf = sg->SurfaceTmp(SurfNum);
            
            surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
            surf.Class = SurfaceClass::Shading;
            surf.HeatTransSurf = false;
            surf.BaseSurfName = ipsc->cAlphaArgs(2);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            if (auto found = sg->surfaceMap.find(surf.BaseSurfName); found != sg->surfaceMap.end()) { 
                surf.BaseSurf = found->second;
                auto &baseSurf = sg->SurfaceTmp(surf.BaseSurf);
                // SurfaceTmp(SurfNum)%BaseSurf=Found
                surf.ExtBoundCond = baseSurf.ExtBoundCond;
                surf.ExtSolar = baseSurf.ExtSolar;
                surf.ExtWind = baseSurf.ExtWind;
                surf.Zone = baseSurf.Zone; // Necessary to do relative coordinates in GetVertices below
                surf.ZoneName = baseSurf.ZoneName; // Necessary to have surface drawn in OutputReports
            } else {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }
            if (surf.ExtBoundCond == UnenteredAdjacentZoneSurface) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {}=\"{}",
                                       ipsc->cCurrentModuleObject,
                                       surf.Name,
                                       ipsc->cAlphaFieldNames(2),
                                       ipsc->cAlphaArgs(2)));
                ShowContinueError(state, "...trying to attach a shading device to an interzone surface.");
                ErrorsFound = true;
                surf.ExtBoundCond =
                    ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }
            if (surf.ExtBoundCond == UnreconciledZoneSurface) {
                ShowSevereError(state,
                                format("{}=\"{}\", invalid {}=\"{}",
                                       ipsc->cCurrentModuleObject,
                                       surf.Name,
                                       ipsc->cAlphaFieldNames(2),
                                       ipsc->cAlphaArgs(2)));
                ShowContinueError(state, "...trying to attach a shading device to an interior surface.");
                ErrorsFound = true;
                surf.ExtBoundCond =
                    ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }

            if (!ipsc->lAlphaFieldBlanks(3)) {
                surf.SchedShadowSurfIndex = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
                if (surf.SchedShadowSurfIndex == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }
            if (surf.SchedShadowSurfIndex != 0) {
                std::tie(surf.SchedMinValue, surf.SchedMaxValue) = ScheduleManager::GetScheduleMinMaxValues(state, surf.SchedShadowSurfIndex);
                if (surf.SchedMinValue == 1.0) {
                    // Set transparent for now, check for EMS actuators later in SolarShading::resetShadingSurfaceTransparency
                    surf.IsTransparent = true;
                }
                if (surf.SchedMinValue < 0.0) {
                    ShowSevereError(state, format("{}=\"{}\", {}=\"{}\", has schedule values < 0.",
                                                  ipsc->cCurrentModuleObject, surf.Name, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                    ShowContinueError(state, "...Schedule values < 0 have no meaning for shading elements.");
                }
                if (surf.SchedMaxValue > 0.0) {
                    state.dataSolarShading->anyScheduledShadingSurface = true;
                }
                if (surf.SchedMaxValue > 1.0) {
                    ShowSevereError(state, format("{}=\"{}\", {}=\"{}\", has schedule values > 1.",
                                                  ipsc->cCurrentModuleObject, surf.Name, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2)));
                    ShowContinueError(state, "...Schedule values > 1 have no meaning for shading elements.");
                }
                if (std::abs(surf.SchedMinValue - surf.SchedMaxValue) > 1.0e-6) {
                    state.dataSurface->ShadingTransmittanceVaries = true;
                }
            }
            if (ipsc->lNumericFieldBlanks(1) || ipsc->rNumericArgs(1) == Constant::AutoCalculate) {
                ipsc->rNumericArgs(1) = (NumNumbers - 1) / 3;
                surf.Sides = ipsc->rNumericArgs(1);
                if (mod(NumNumbers - 1, 3) != 0) {
                    ShowWarningError(state, format("{}=\"{}\", {} not even multiple of 3. Will read in {}",
                                                   ipsc->cCurrentModuleObject, surf.Name, ipsc->cNumericFieldNames(1), surf.Sides));
                }
                if (ipsc->rNumericArgs(1) < 3) {
                    ShowSevereError(state, format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                                  ipsc->cCurrentModuleObject, surf.Name, ipsc->cNumericFieldNames(1), surf.Sides));
                    ErrorsFound = true;
                    continue;
                }
            } else {
                surf.Sides = ipsc->rNumericArgs(1);
            }
            surf.Vertex.allocate(surf.Sides);
            GetVertices(state, SurfNum, surf.Sides, ipsc->rNumericArgs({2, _}));
            CheckConvexity(state, SurfNum, surf.Sides);
            //    IF (SurfaceTmp(SurfNum)%Sides == 3) THEN
            //      CALL ShowWarningError(state, TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            //                        ' should not be triangular.')
            //      CALL ShowContinueError(state, '...Check results carefully.')
            //      ErrorsFound=.TRUE.
            //    ENDIF
            // Reset surface to be "detached"
            surf.BaseSurf = 0;
            //    SurfaceTmp(SurfNum)%BaseSurfName='  '
            surf.Zone = 0;
            // SurfaceTmp(SurfNum)%ZoneName='  '
            if (state.dataReportFlag->MakeMirroredAttachedShading) {
                SurfNum = MakeMirrorSurface(state, SurfNum);
            }
        }
    } // GetAttShdSurfaceData()

    void GetSimpleShdSurfaceData(EnergyPlusData &state,
                                 bool &ErrorsFound,                // Error flag indicator (true if errors found)
                                 int &SurfNum,                     // Count of Current SurfaceNumber
                                 int const TotOverhangs,           // Number of Overhangs to obtain
                                 int const TotOverhangsProjection, // Number of Overhangs (projection) to obtain
                                 int const TotFins,                // Number of Fins to obtain
                                 int const TotFinsProjection       // Number of Fins (projection) to obtain
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   January 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get simple overhang and fin descriptions.

        // Using/Aliasing
        using namespace Vectors;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(4, {"Shading:Overhang", "Shading:Overhang:Projection", "Shading:Fin", "Shading:Fin:Projection"});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Item;
        int ItemsToGet;
        int Loop;
        int NumAlphas;
        int NumNumbers;
        int IOStat; // IO Status when calling get input subroutine
        int Found;  // For matching base surfaces
        Real64 Depth;
        Real64 Length;
        Vector3<Real64> p;
        Vector3<Real64> LLC;

        constexpr std::string_view routineName = "GetSimpleShdSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        if ((TotOverhangs + TotOverhangsProjection + TotFins + TotFinsProjection) > 0 &&
            state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
            ShowWarningError(state, "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
        }

        for (int Item = 1; Item <= 4; ++Item) {

            ipsc->cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotOverhangs;
            } else if (Item == 2) {
                ItemsToGet = TotOverhangsProjection;
            } else if (Item == 3) {
                ItemsToGet = TotFins;
            } else { // ! (Item == 4) THEN
                ItemsToGet = TotFinsProjection;
            }

            for (int Loop = 1; Loop <= ItemsToGet; ++Loop) {
                ipsc->cCurrentModuleObject = cModuleObjects(Item);
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                    ShowSevereDuplicateName(state, eoh);
                    ErrorsFound = true;
                    continue;
                }
                
                ++SurfNum;
                sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);
            
                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                surf.Name = ipsc->cAlphaArgs(1); // Set the Surface Name in the Derived Type
                surf.Class = SurfaceClass::Shading;
                surf.HeatTransSurf = false;
                // this object references a window or door....

                auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(2)); 
                if (found == sg->surfaceMap.end()) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ErrorsFound = true;
                    continue;
                }

                auto &winSurf = sg->SurfaceTmp(found->second);
                auto &baseSurf = sg->SurfaceTmp(winSurf.BaseSurf);
                
                surf.BaseSurf = winSurf.BaseSurf;
                surf.BaseSurfName = winSurf.BaseSurfName;
                surf.ExtBoundCond = winSurf.ExtBoundCond;
                surf.ExtSolar = winSurf.ExtSolar;
                surf.ExtWind = winSurf.ExtWind;
                surf.Zone = winSurf.Zone; // Necessary to do relative coordinates in GetVertices below
                surf.ZoneName = winSurf.ZoneName; // Necessary to have surface drawn in OutputReports

                if (surf.ExtBoundCond == UnenteredAdjacentZoneSurface) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ShowContinueError(state, "...trying to attach a shading device to an interzone surface.");
                    ErrorsFound = true;
                    surf.ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }
                if (surf.ExtBoundCond == UnreconciledZoneSurface) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ShowContinueError(state, "...trying to attach a shading device to an interior surface.");
                    ErrorsFound = true;
                    surf.ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }

                surf.SchedShadowSurfIndex = 0;

                //===== Overhang =====

                if (Item < 3) {
                    //  Found is the surface window or door.
                    //   N1,  \field Height above Window or Door
                    //        \units m
                    //   N2,  \field Tilt Angle from Window/Door
                    //        \units deg
                    //        \default 90
                    //        \minimum 0
                    //        \maximum 180
                    //   N3,  \field Left extension from Window/Door Width
                    //        \units m
                    //   N4,  \field Right extension from Window/Door Width
                    //        \note N3 + N4 + Window/Door Width is Overhang Length
                    //        \units m
                    //   N5;  \field Depth
                    //        \units m
                    // for projection option:
                    //   N5;  \field Depth as Fraction of Window/Door Height
                    //        \units m
                    Length = ipsc->rNumericArgs(3) + ipsc->rNumericArgs(4) + winSurf.Width;
                    if (Item == 1) {
                        Depth = ipsc->rNumericArgs(5);
                    } else if (Item == 2) {
                        Depth = ipsc->rNumericArgs(5) * winSurf.Height;
                    }

                    if (Length * Depth <= 0.0) {
                        ShowSevereError(state, format("{}=\"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                                      ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1), Length * Depth));
                        continue;
                    }

                    surf.Tilt = winSurf.Tilt + ipsc->rNumericArgs(2);
                    surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
                    surf.Azimuth = winSurf.Azimuth;

                    // Make it relative to surface origin.....
                    Vector3<Real64> p = winSurf.Vertex(2) - baseSurf.Vertex(2);

                    LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                    LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;

                    surf.Sides = 4;
                    surf.Vertex.allocate(surf.Sides);

                    MakeRelativeRectangularVertices(state,
                                                    surf.BaseSurf,
                                                    SurfNum,
                                                    LLC.x - ipsc->rNumericArgs(3),
                                                    LLC.y + winSurf.Height + ipsc->rNumericArgs(1),
                                                    Length,
                                                    Depth);

                    // Reset surface to be "detached"
                    //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                    //    SurfaceTmp(SurfNum)%ZoneName='  '

                    surf.BaseSurf = 0;
                    surf.Zone = 0;

                    // and mirror
                    if (state.dataReportFlag->MakeMirroredAttachedShading) {
                        SurfNum = MakeMirrorSurface(state, SurfNum);
                    }

                } else { // Fins

                    //===== Fins =====

                    //===== Left Fin =====

                    //   N1,  \field Left Extension from Window/Door
                    //        \units m
                    //   N2,  \field Left Distance Above Top of Window
                    //        \units m
                    //   N3,  \field Left Distance Below Bottom of Window
                    //        \units m
                    //        \note N2 + N3 + height of Window/Door is height of Fin
                    //   N4,  \field Left Tilt Angle from Window/Door
                    //        \units deg
                    //        \default 90
                    //        \minimum 0
                    //        \maximum 180
                    //   N5,  \field Left Depth
                    //        \units m
                    // for projection option:
                    //   N5,  \field Left Depth as Fraction of Window/Door Width
                    //        \units m
                    surf.Name = surf.Name + " Left";
                    Length = ipsc->rNumericArgs(2) + ipsc->rNumericArgs(3) + winSurf.Height;
                    if (Item == 3) {
                        Depth = ipsc->rNumericArgs(5);
                    } else if (Item == 4) {
                        Depth = ipsc->rNumericArgs(5) * winSurf.Width;
                    }

                    if (Length * Depth <= 0.0) {
                        ShowWarningError(state,
                                         format("{}=Left Fin of \"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                                ipsc->cCurrentModuleObject,
                                                ipsc->cAlphaArgs(1),
                                                Length * Depth));
                        --SurfNum;
                    } else {
                        surf.Tilt = winSurf.Tilt;
                        surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
                        surf.Azimuth = winSurf.Azimuth - (180.0 - ipsc->rNumericArgs(4));

                        // Make it relative to surface origin.....
                        Vector3<Real64> p = winSurf.Vertex(2) - baseSurf.Vertex(2);

                        LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                        LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;

                        surf.CosAzim = std::cos(surf.Azimuth * Constant::DegToRadians);
                        surf.SinAzim = std::sin(surf.Azimuth * Constant::DegToRadians);
                        surf.CosTilt = std::cos(surf.Tilt * Constant::DegToRadians);
                        surf.SinTilt = std::sin(surf.Tilt * Constant::DegToRadians);

                        surf.Sides = 4;
                        surf.Vertex.allocate(surf.Sides);

                        MakeRelativeRectangularVertices(state,
                                                        surf.BaseSurf,
                                                        SurfNum,
                                                        LLC.x - ipsc->rNumericArgs(1),
                                                        LLC.y - ipsc->rNumericArgs(3),
                                                        -Depth,
                                                        Length);

                        // Reset surface to be "detached"
                        //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                        //    SurfaceTmp(SurfNum)%ZoneName='  '

                        surf.BaseSurf = 0;
                        surf.Zone = 0;

                        // and mirror
                        if (state.dataReportFlag->MakeMirroredAttachedShading) {
                            SurfNum = MakeMirrorSurface(state, SurfNum);
                        }
                    }
                    
                    //===== Right Fin =====

                    //   N6,  \field Right Extension from Window/Door
                    //        \units m
                    //   N7,  \field Right Distance Above Top of Window
                    //        \units m
                    //   N8,  \field Right Distance Below Bottom of Window
                    //        \note N7 + N8 + height of Window/Door is height of Fin
                    //        \units m
                    //   N9,  \field Right Tilt Angle from Window/Door
                    //        \units deg
                    //        \default 90
                    //        \minimum 0
                    //        \maximum 180
                    //   N10; \field Right Depth
                    //        \units m
                    // for projection option:
                    //   N10; \field Right Depth as Fraction of Window/Door Width
                    //        \units m

                    ++SurfNum;
                    auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                    surf.Name = ipsc->cAlphaArgs(1) + " Right"; // Set the Surface Name in the Derived Type
                    surf.Class = SurfaceClass::Shading;
                    surf.HeatTransSurf = false;
                    surf.BaseSurfName = winSurf.BaseSurfName;
                    surf.ExtBoundCond = winSurf.ExtBoundCond;
                    surf.ExtSolar = winSurf.ExtSolar;
                    surf.ExtWind = winSurf.ExtWind;
                    surf.Zone = winSurf.Zone; // Necessary to do relative coordinates in GetVertices below
                    surf.ZoneName = winSurf.ZoneName; // Necessary to have surface drawn in OutputReports

                    surf.SchedShadowSurfIndex = 0;
                    Length = ipsc->rNumericArgs(7) + ipsc->rNumericArgs(8) + winSurf.Height;
                    if (Item == 3) {
                        Depth = ipsc->rNumericArgs(10);
                    } else if (Item == 4) {
                        Depth = ipsc->rNumericArgs(10) * winSurf.Width;
                    }

                    if (Length * Depth <= 0.0) {
                        ShowWarningError(state,
                                         format("{}=Right Fin of \"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                                ipsc->cCurrentModuleObject,
                                                ipsc->cAlphaArgs(1),
                                                Length * Depth));
                        --SurfNum;
                    } else {
                        // Make it relative to surface origin.....
                        Vector3<Real64> p = winSurf.Vertex(2) - baseSurf.Vertex(2);

                        LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                        LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;

                        surf.Tilt = winSurf.Tilt;
                        surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
                        surf.Azimuth = winSurf.Azimuth - (180.0 - ipsc->rNumericArgs(9));
                        surf.CosAzim = std::cos(surf.Azimuth * Constant::DegToRadians);
                        surf.SinAzim = std::sin(surf.Azimuth * Constant::DegToRadians);
                        surf.CosTilt = std::cos(surf.Tilt * Constant::DegToRadians);
                        surf.SinTilt = std::sin(surf.Tilt * Constant::DegToRadians);

                        surf.Sides = 4;
                        surf.Vertex.allocate(surf.Sides);

                        MakeRelativeRectangularVertices(state,
                                                        surf.BaseSurf,
                                                        SurfNum,
                                                        LLC.x + winSurf.Width + ipsc->rNumericArgs(6),
                                                        LLC.y - ipsc->rNumericArgs(8),
                                                        -Depth,
                                                        Length);

                        surf.BaseSurf = 0;
                        surf.Zone = 0;

                        // and mirror
                        if (state.dataReportFlag->MakeMirroredAttachedShading) {
                            SurfNum = MakeMirrorSurface(state, SurfNum);
                        }
                    } 
                }
            } // for (Loop)
        } // for (Item)
    } // GetSimpleShdSurfaceData()

    void GetIntMassSurfaceData(EnergyPlusData &state,
                               bool &ErrorsFound, // Error flag indicator (true if errors found)
                               int &SurfNum       // Count of Current SurfaceNumber
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the Internal Surface Data,
        // checks it for errors, etc.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Internal Mass Surface Definition
        // Surface:HeatTransfer:InternalMass,
        //       \note used to describe internal zone surface area that does not need to be part of geometric representation
        //  A1 , \field User Supplied Surface Name
        //       \type alpha
        //       \reference SurfaceNames
        //  A2 , \field Construction Name of the Surface
        //       \note To be matched with a construction in this input file
        //       \type object-list
        //       \object-list ConstructionNames
        //  A3 , \field Interior Environment
        //       \note Zone the surface is a part of
        //       \type object-list
        //       \object-list ZoneNames
        //  N1,  \field View factor to Person (to people?)
        //       \type real
        //       \note from the interior of the surface
        //  N2 ; \field Surface area
        //       \units m2

        // Using/Aliasing
        using namespace Vectors;
        using General::CheckCreatedZoneItemName;

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;                // IO Status when calling get input subroutine
        int SurfaceNumAlpha;       // Number of material alpha names being passed
        int SurfaceNumArg;         // Number of material properties being passed
        int ZoneNum;               // index to a zone
        int NumIntMassSurfaces(0); // total count of internal mass surfaces
        bool errFlag;              //  local error flag

        constexpr std::string_view routineName = "GetIntMassSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        ipsc->cCurrentModuleObject = "InternalMass";
        
        int TotIntMass = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        if (TotIntMass == 0) return;

        state.dataSurface->IntMassObjects.allocate(TotIntMass);

        // scan for use of Zone lists in InternalMass objects
        errFlag = false;
        NumIntMassSurfaces = 0;
        for (int Item = 1; Item <= TotIntMass; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     SurfaceNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     SurfaceNumArg,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            if (auto found = sg->surfaceMap.find(ipsc->cAlphaArgs(1)); found != sg->surfaceMap.end()) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            // Don't insert this into the surfaceMap?
            // sg->surfaceMap.insert_or_assign(ipsc->cAlphaArgs(1), SurfNum);

            auto &intMass = state.dataSurface->IntMassObjects(Item);
            
            intMass.Name = ipsc->cAlphaArgs(1);
            intMass.GrossArea = ipsc->rNumericArgs(1);
            intMass.Construction = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);
            intMass.ZoneOrZoneListName = ipsc->cAlphaArgs(3);
            int Item1 = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
            int ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                if (ipsc->lAlphaFieldBlanks(4)) {
                    ++NumIntMassSurfaces;
                }
                intMass.NumOfZones = 1;
                intMass.ZoneListActive = false;
                intMass.ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                NumIntMassSurfaces += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                intMass.NumOfZones = state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
                intMass.ZoneListActive = true;
                intMass.ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ++SurfNum;
                auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                surf.Class = SurfaceClass::Invalid;
                surf.ZoneName = "Unknown Zone";
                ErrorsFound = true;
                errFlag = true;
            }

            if (!ipsc->lAlphaFieldBlanks(4)) {
                intMass.spaceOrSpaceListName = ipsc->cAlphaArgs(4);
                int Item1 = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataHeatBal->space);
                int SLItem = 0;
                if (Item1 == 0 && int(state.dataHeatBal->spaceList.size()) > 0)
                    SLItem = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataHeatBal->spaceList);
                if (Item1 > 0) {
                    ++NumIntMassSurfaces;
                    intMass.numOfSpaces = 1;
                    intMass.spaceListActive = false;
                    intMass.spaceOrSpaceListPtr = Item1;
                    if (!intMass.ZoneListActive) {
                        if (state.dataHeatBal->space(Item1).zoneNum != intMass.ZoneOrZoneListPtr) {
                            ShowSevereError(state,
                                            format("{}=\"{}\" invalid {}=\"{}\" is not part of Zone =\"{}\".",
                                                   ipsc->cCurrentModuleObject,
                                                   ipsc->cAlphaArgs(1),
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4),
                                                   ipsc->cAlphaArgs(3)));
                            ErrorsFound = true;
                            errFlag = true;
                        }
                    }
                } else if (SLItem > 0) {
                    int numOfSpaces = int(state.dataHeatBal->spaceList(SLItem).numListSpaces);
                    NumIntMassSurfaces += numOfSpaces;
                    intMass.numOfSpaces = numOfSpaces;
                    intMass.spaceListActive = true;
                    intMass.spaceOrSpaceListPtr = SLItem;
                } else {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                    ++SurfNum;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::Invalid;
                    ErrorsFound = true;
                    errFlag = true;
                }
            }

            if (errFlag) {
                ShowSevereError(state, format("{}: Errors with invalid names in {} objects.", routineName, ipsc->cCurrentModuleObject));
                ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
                NumIntMassSurfaces = 0;
            }

            if (intMass.Construction == 0) {
                ErrorsFound = true;
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
            } else if (state.dataConstruction->Construct(intMass.Construction).TypeIsWindow) {
                ErrorsFound = true;
                ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2), "has Window materials.");
            } else {
                state.dataConstruction->Construct(intMass.Construction).IsUsed = true;
            }
        }

        if (NumIntMassSurfaces > 0) {
            int spaceNum = 0;
            for (int Loop = 1; Loop <= TotIntMass; ++Loop) {
                int numberOfZonesOrSpaces = 1;
                auto const &intMass = state.dataSurface->IntMassObjects(Loop);
                if (intMass.ZoneListActive) {
                    numberOfZonesOrSpaces = intMass.NumOfZones;
                } else if (intMass.spaceListActive) {
                    numberOfZonesOrSpaces = intMass.numOfSpaces;
                }

                for (int Item1 = 1; Item1 <= numberOfZonesOrSpaces; ++Item1) {

                    ++SurfNum;
                    auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                    surf.Construction = intMass.Construction;
                    if (!intMass.ZoneListActive && !intMass.spaceListActive) {
                        surf.Zone = intMass.ZoneOrZoneListPtr;
                        surf.spaceNum = intMass.spaceOrSpaceListPtr;
                        surf.Name = intMass.Name;
                        surf.Class = SurfaceClass::IntMass;
                        surf.ZoneName = intMass.ZoneOrZoneListName;
                        surf.HeatTransSurf = true;
                    } else {
                        if (intMass.ZoneListActive) {
                            CheckCreatedZoneItemName(
                                state,
                                routineName,
                                ipsc->cCurrentModuleObject,
                                state.dataHeatBal->Zone(state.dataHeatBal->ZoneList(intMass.ZoneOrZoneListPtr).Zone(Item1)).Name,
                                state.dataHeatBal->ZoneList(intMass.ZoneOrZoneListPtr).MaxZoneNameLength,
                                intMass.Name,
                                state.dataSurfaceGeometry->SurfaceTmp,
                                SurfNum - 1,
                                surf.Name,
                                errFlag);

                            ZoneNum = state.dataHeatBal->ZoneList(intMass.ZoneOrZoneListPtr).Zone(Item1);
                        } else if (intMass.spaceListActive) {
                            spaceNum = state.dataHeatBal->spaceList(intMass.spaceOrSpaceListPtr).spaces(Item1);
                            ZoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
                            const std::string spaceName = state.dataHeatBal->space(spaceNum).Name;
                            surf.Name = spaceName + ' ' + intMass.Name;
                            surf.spaceNum = spaceNum;
                        }
                        surf.Zone = ZoneNum;
                        surf.Class = SurfaceClass::IntMass;
                        surf.ZoneName = state.dataHeatBal->Zone(ZoneNum).Name;
                        surf.HeatTransSurf = true;
                        if (errFlag) ErrorsFound = true;
                    }

                    if (intMass.Construction > 0) {
                        if (state.dataConstruction->Construct(intMass.Construction).IsUsed) {
                            surf.ConstructionStoredInputValue = intMass.Construction;
                        }
                    }
                    surf.GrossArea = intMass.GrossArea;
                    surf.Area = surf.GrossArea;
                    surf.NetAreaShadowCalc = surf.Area;
                    surf.Width = surf.Area;
                    surf.Height = 1.0;
                    surf.Tilt = 90.0;
                    surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
                    surf.CosTilt = 0.0; // Tuned Was std::cos( 90.0 * DegToRadians )
                    surf.SinTilt = 1.0; // Tuned Was std::sin( 90.0 * DegToRadians )
                    surf.Azimuth = 0.0;
                    surf.CosAzim = 1.0; // Tuned Was std::cos( 0.0 )
                    surf.SinAzim = 0.0; // Tuned Was std::sin( 0.0 )
                    // Outward normal unit vector (pointing away from room)
                    surf.OutNormVec = surf.lcsz;
                    surf.ViewFactorSky = 0.5;
                    surf.ExtSolar = false;
                    surf.ExtWind = false;
                    surf.BaseSurf = SurfNum;
                    surf.BaseSurfName = surf.Name;
                    surf.ExtBoundCondName = surf.Name;
                    surf.ExtBoundCond = UnreconciledZoneSurface;
                } // for (Item)
            } // for (Loop)
        } // if (NumIntMassSurfaces > 0)
    } // GetIntMassSurfaceData()

    int GetNumIntMassSurfaces(EnergyPlusData &state) // Number of Internal Mass Surfaces to obtain

    {
        // Counts internal mass surfaces applied to zones and zone lists

        // Using/Aliasing

        int IOStat;          // IO Status when calling get input subroutine
        int SurfaceNumAlpha; // Number of material alpha names being passed
        int SurfaceNumArg;   // Number of material properties being passed
        int NumIntMassSurf;  // total count of internal mass surfaces

        constexpr std::string_view routineName = "GetNumIntMassSurfaceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        NumIntMassSurf = 0;
        int TotIntMass = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "InternalMass");

        if (TotIntMass == 0) return NumIntMassSurf;

        ipsc->cCurrentModuleObject = "InternalMass";
        // scan for zones and zone lists in InternalMass objects
        for (int Item = 1; Item <= TotIntMass; ++Item) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     SurfaceNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     SurfaceNumArg,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            int Item1 = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataHeatBal->Zone, state.dataGlobal->NumOfZones);
            int ZLItem = 0;
            if (Item1 == 0 && state.dataHeatBal->NumOfZoneLists > 0)
                ZLItem = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataHeatBal->ZoneList);
            if (Item1 > 0) {
                if (ipsc->lAlphaFieldBlanks(4)) {
                    ++NumIntMassSurf;
                }
            } else if (ZLItem > 0) {
                NumIntMassSurf += state.dataHeatBal->ZoneList(ZLItem).NumOfZones;
            }

            if (!ipsc->lAlphaFieldBlanks(4)) {
                int Item1 = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataHeatBal->space);
                int SLItem = 0;
                if (Item1 == 0 && int(state.dataHeatBal->spaceList.size()) > 0)
                    SLItem = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataHeatBal->spaceList);
                if (Item1 > 0) {
                    ++NumIntMassSurf;
                } else if (SLItem > 0) {
                    int numOfSpaces = int(state.dataHeatBal->spaceList(SLItem).numListSpaces);
                    NumIntMassSurf += numOfSpaces;
                }
            }
        }
        return max(NumIntMassSurf, TotIntMass);
    } // GetNumIntMassSurfaces()

    void GetShadingSurfReflectanceData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Sept 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Gets data for a Shading Surface Reflectance object.  This is only called when the
        // Solar Distribution is to be calculated for reflectances.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;                       // IO Status when calling get input subroutine
        int NumAlpha;                     // Number of alpha names being passed
        int NumProp;                      // Number of properties being passed
        int TotShadingSurfaceReflectance; // Total Shading Surface Refleftance statements
        int Loop;                         // DO loop index
        int SurfNum;                      // Surface number
        int GlConstrNum;                  // Glazing construction number
        bool WrongSurfaceType;

        constexpr std::string_view routineName = "GetShadingSurfReflectanceData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        // For shading surfaces, initialize value of reflectance values to default values. These values
        // may be overridden below for shading surfaces with an associated Shading Surface Reflectance object.
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto const &surf = state.dataSurface->Surface(SurfNum);
            if (!(surf.Class == SurfaceClass::Shading || surf.Class == SurfaceClass::Detached_F || surf.Class == SurfaceClass::Detached_B ||
                  surf.Class == SurfaceClass::Overhang || surf.Class == SurfaceClass::Fin))
                continue;
            state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum) = 0.2;
            state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum) = 0.2;
            state.dataSurface->SurfShadowGlazingFrac(SurfNum) = 0.0;
            state.dataSurface->SurfShadowGlazingConstruct(SurfNum) = 0;
        }

        // Get the total number of Shading Surface Reflectance objects

        ipsc->cCurrentModuleObject = "ShadingProperty:Reflectance";
        TotShadingSurfaceReflectance = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        //  IF(TotShadingSurfaceReflectance.EQ.0) RETURN

        for (int Loop = 1; Loop <= TotShadingSurfaceReflectance; ++Loop) {
            ipsc->cCurrentModuleObject = "ShadingProperty:Reflectance";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumProp,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            SurfNum = Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
            if (SurfNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                //      ErrorsFound =.TRUE.
                continue;
            }

            // Check that associated surface is a shading surface
            auto const &surf = state.dataSurface->Surface(SurfNum);

            if (surf.Class != SurfaceClass::Shading && surf.Class != SurfaceClass::Detached_F && surf.Class != SurfaceClass::Detached_B &&
                surf.Class != SurfaceClass::Overhang && surf.Class != SurfaceClass::Fin) {
                ShowSevereCustomMessage(state, eoh, "surface is not a shading surface.");
                ErrorsFound = true;
                continue;
            }

            // If associated surface is a shading surface, set reflectance values
            state.dataSurface->SurfShadowGlazingFrac(SurfNum) = ipsc->rNumericArgs(3);
            state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum) = (1.0 - ipsc->rNumericArgs(3)) * ipsc->rNumericArgs(1);
            state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum) = (1.0 - ipsc->rNumericArgs(3)) * ipsc->rNumericArgs(2);
            if (ipsc->rNumericArgs(3) > 0.0) {
                GlConstrNum = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);
                if (GlConstrNum == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ErrorsFound = true;
                } else {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                state.dataSurface->SurfShadowGlazingConstruct(SurfNum) = GlConstrNum;
            }
            SurfNum = Util::FindItemInList("Mir-" + ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
            if (SurfNum == 0) continue;
            state.dataSurface->SurfShadowGlazingFrac(SurfNum) = ipsc->rNumericArgs(3);
            state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum) = (1.0 - ipsc->rNumericArgs(3)) * ipsc->rNumericArgs(1);
            state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum) = (1.0 - ipsc->rNumericArgs(3)) * ipsc->rNumericArgs(2);
            if (ipsc->rNumericArgs(3) > 0.0) {
                GlConstrNum = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);
                if (GlConstrNum != 0) {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                state.dataSurface->SurfShadowGlazingConstruct(SurfNum) = GlConstrNum;
            }

        } // End of loop over Shading Surface Reflectance objects

        // Write reflectance values to .eio file.
        print(state.files.eio,
              "! <ShadingProperty Reflectance>,Shading Surface Name,Shading Type,Diffuse Solar Reflectance, Diffuse "
              "Visible Reflectance,Surface Glazing Fraction,Surface Glazing Contruction\n");

        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto const &surf = state.dataSurface->Surface(SurfNum);
            if (!(surf.Class == SurfaceClass::Shading || surf.Class == SurfaceClass::Detached_F || surf.Class == SurfaceClass::Detached_B ||
                  surf.Class == SurfaceClass::Overhang || surf.Class == SurfaceClass::Fin))
                continue;

            constexpr std::string_view fmt = "ShadingProperty Reflectance,{},{},{:.2R},{:.2R},{:.2R}, {}\n";
            if (state.dataSurface->SurfShadowGlazingConstruct(SurfNum) != 0) {
                print(state.files.eio,
                      fmt,
                      surf.Name,
                      surfaceClassStrings[(int)surf.Class],
                      state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum),
                      state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum),
                      state.dataSurface->SurfShadowGlazingFrac(SurfNum),
                      state.dataConstruction->Construct(state.dataSurface->SurfShadowGlazingConstruct(SurfNum)).Name);
            } else {
                print(state.files.eio,
                      fmt,
                      surf.Name,
                      surfaceClassStrings[(int)surf.Class],
                      state.dataSurface->SurfShadowDiffuseSolRefl(SurfNum),
                      state.dataSurface->SurfShadowDiffuseVisRefl(SurfNum),
                      state.dataSurface->SurfShadowGlazingFrac(SurfNum),
                      "N/A");
            }
        } // for (SurfNum)
    } // GetShadingSurfReflectanceData()

    void GetHTSurfExtVentedCavityData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         BGriffith
        //       DATE WRITTEN   January 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for Exterior Vented Cavity Special case for heat transfer surfaces

        // METHODOLOGY EMPLOYED:
        // usual E+ input processes

        // REFERENCES:
        // derived from SUBROUTINE GetTranspiredCollectorInput

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int Item;          // Item to be "gotten"
        int NumAlphas;     // Number of Alphas for each GetObjectItem call
        int NumNumbers;    // Number of Numbers for each GetObjectItem call
        int MaxNumAlphas;  // argument for call to GetObjectDefMaxArgs
        int MaxNumNumbers; // argument for call to GetObjectDefMaxArgs
        int Dummy;         // argument for call to GetObjectDefMaxArgs
        int IOStatus;      // Used in GetObjectItem
        int Found;
        int AlphaOffset; // local temp var
        std::string Roughness;
        int ThisSurf;                   // do loop counter
        Real64 AvgAzimuth;              // temp for error checking
        Real64 AvgTilt;                 // temp for error checking
        constexpr Real64 AZITOL = 15.0; // Degree Azimuth Angle Tolerance
        constexpr Real64 TILTOL = 10.0; // Degree Tilt Angle Tolerance
        int SurfID;                     // local surface "pointer"
        bool IsBlank;
        bool ErrorInName;

        constexpr std::string_view routineName = "GetHTSurfExtVentedCavityData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        ipsc->cCurrentModuleObject = "SurfaceProperty:ExteriorNaturalVentedCavity";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ipsc->cCurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers);

        if (MaxNumNumbers != 8) {
            ShowSevereError(
                state, format("{}: Object Definition indicates not = 8 Number Objects, Number Indicated={}", ipsc->cCurrentModuleObject, MaxNumNumbers));
            ErrorsFound = true;
        }

        state.dataSurface->TotExtVentCav = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        state.dataHeatBal->ExtVentedCavity.allocate(state.dataSurface->TotExtVentCav);

        for (int Item = 1; Item <= state.dataSurface->TotExtVentCav; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:ExteriorNaturalVentedCavity";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            if (ipsc->lAlphaFieldBlanks(1)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                ErrorsFound = true;
                continue;
            }
            
            if (Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataHeatBal->ExtVentedCavity, Item - 1) > 0) { 
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            auto &cavity = state.dataHeatBal->ExtVentedCavity(Item);
            cavity.Name = ipsc->cAlphaArgs(1);

            if (ipsc->lAlphaFieldBlanks(2)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(2));
                ErrorsFound = true;
                continue;
            }
            
            cavity.OSCMName = ipsc->cAlphaArgs(2);
            cavity.OSCMPtr = Util::FindItemInList(cavity.OSCMName, state.dataSurface->OSCM, state.dataSurface->TotOSCM);
            if (cavity.OSCMPtr == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                continue;
            }

            // Select the correct Number for the associated ascii name for the roughness type
            cavity.BaffleRoughness = static_cast<Material::Roughness>(getEnumValue(Material::roughnessNamesUC, ipsc->cAlphaArgs(3)));
            if (cavity.BaffleRoughness == Material::Roughness::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
                continue;
            }

            AlphaOffset = 3;
            cavity.NumSurfs = NumAlphas - AlphaOffset;
            if (cavity.NumSurfs == 0) {
                ShowSevereCustomMessage(state, eoh, "no underlying surfaces specified. Must have at least one.");
                ErrorsFound = true;
                continue;
            }
            
            cavity.SurfPtrs.allocate(cavity.NumSurfs);
            cavity.SurfPtrs = 0;
            for (int ThisSurf = AlphaOffset + 1; ThisSurf <= AlphaOffset + cavity.NumSurfs; ++ThisSurf) {
                Found = Util::FindItemInList(ipsc->cAlphaArgs(ThisSurf), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
                if (Found == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(ThisSurf), ipsc->cAlphaArgs(ThisSurf));
                    ErrorsFound = true;
                    continue;
                }

                auto const &surf = state.dataSurface->Surface(Found);
                // check that surface is appropriate, Heat transfer, Sun, Wind,
                if (!surf.HeatTransSurf) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ThisSurf), ipsc->cAlphaArgs(ThisSurf), "is not a Heat Transfer Surface.");
                    ErrorsFound = true;
                    continue;
                }
                if (!surf.ExtSolar) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ThisSurf), ipsc->cAlphaArgs(ThisSurf), "is not exposed to Sun.");
                    ErrorsFound = true;
                    continue;
                }
                if (!surf.ExtWind) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ThisSurf), ipsc->cAlphaArgs(ThisSurf), "is not exposed to Wind.");
                    ErrorsFound = true;
                    continue;
                }
                if (surf.ExtBoundCond != OtherSideCondModeledExt) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(ThisSurf), ipsc->cAlphaArgs(ThisSurf), "is not an OtherSideConditionedModel surface.");
                    ErrorsFound = true;
                    continue;
                }
                cavity.SurfPtrs(ThisSurf) = Found;

                // now set info in Surface structure
                state.dataSurface->SurfExtCavNum(Found) = Item;
                state.dataSurface->SurfExtCavityPresent(Found) = true;
            }

            if (ErrorsFound) continue; // previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

            // now that we should have all the surfaces, do some preperations and checks.

            // are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
            Real64 area = 0.0, areaWeightedAzimuth = 0.0, areaWeightedTilt = 0.0, areaWeightedCentroidZ = 0.0;
            for (int surfNum : cavity.SurfPtrs) {
                auto const &surf = state.dataSurface->Surface(surfNum);
                area += surf.Area;
                areaWeightedAzimuth += surf.Area * surf.Azimuth;
                areaWeightedTilt += surf.Area * surf.Tilt;
                areaWeightedCentroidZ += surf.Area * surf.Centroid.z;
            }
            
            cavity.Azimuth = areaWeightedAzimuth / area;
            cavity.Tilt = areaWeightedTilt / area;
            cavity.Centroid.z = areaWeightedCentroidZ / area;
                                       
            for (int surfNum : cavity.SurfPtrs) {
                auto const &surf = state.dataSurface->Surface(surfNum);
                if (General::rotAzmDiffDeg(surf.Azimuth, AvgAzimuth) > AZITOL) {
                    ShowWarningError(state, format("{}=\"{}, Surface {} has Azimuth different from others in the associated group.",
                                                   ipsc->cCurrentModuleObject, cavity.Name, surf.Name));
                }
                if (std::abs(surf.Tilt - AvgTilt) > TILTOL) {
                    ShowWarningError(state, format("{}=\"{}, Surface {} has Tilt different from others in the associated group.",
                                                   ipsc->cCurrentModuleObject, cavity.Name, surf.Name));
                }

                // test that there are no windows.  Now allow windows
                // If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
                //      Call ShowWarningError(state, 'Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
                //         //'subtracted in the group of surfaces associated with '//TRIM(ExtVentedCavity(Item)%Name))
                // endif
            }

            // find area weighted centroid.
            //            ExtVentedCavity( Item ).Centroid.z = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Centroid.z * Surface(
            // ExtVentedCavity(  Item
            //).SurfPtrs ).Area ) / sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced
            // by below


            // now handle rNumericArgs from input object
            cavity.Porosity = ipsc->rNumericArgs(1);
            cavity.LWEmitt = ipsc->rNumericArgs(2);
            cavity.SolAbsorp = ipsc->rNumericArgs(3);
            cavity.HdeltaNPL = ipsc->rNumericArgs(4);
            cavity.PlenGapThick = ipsc->rNumericArgs(5);
            if (cavity.PlenGapThick <= 0.0) {
                ShowSevereError(state, format("{}=\"{}\", invalid .", ipsc->cCurrentModuleObject, cavity.Name));
                ErrorsFound = true;
                ShowContinueError(state,
                                  format("...because field \"{}\" must be greater than Zero=[{:.2T}].",
                                         ipsc->cNumericFieldNames(5),
                                         ipsc->rNumericArgs(5)));
                continue;
            }
            cavity.AreaRatio = ipsc->rNumericArgs(6);
            cavity.Cv = ipsc->rNumericArgs(7);
            cavity.Cd = ipsc->rNumericArgs(8);

            // Fill out data we now know
            // sum areas of HT surface areas
            //            ExtVentedCavity( Item ).ProjArea = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array
            // subscript  usage: Replaced by below
            cavity.ProjArea = area;
            if (cavity.ProjArea <= 0.0) {
                ShowSevereError(state, format("{}=\"{}\", invalid .", ipsc->cCurrentModuleObject, cavity.Name));
                ShowContinueError(state, format("...because gross area of underlying surfaces must be greater than Zero=[{:.2T}].", cavity.ProjArea));
                ErrorsFound = true;
                continue;
            }
            cavity.ActualArea = cavity.ProjArea * cavity.AreaRatio;

            SetupOutputVariable(state,
                                "Surface Exterior Cavity Baffle Surface Temperature",
                                Constant::Units::C,
                                cavity.Tbaffle,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
            SetupOutputVariable(state,
                                "Surface Exterior Cavity Air Drybulb Temperature",
                                Constant::Units::C,
                                cavity.TAirCav,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
            SetupOutputVariable(state,
                                "Surface Exterior Cavity Total Natural Ventilation Air Change Rate",
                                Constant::Units::ach,
                                cavity.PassiveACH,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
            SetupOutputVariable(state,
                                "Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate",
                                Constant::Units::kg_s,
                                cavity.PassiveMdotVent,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
            SetupOutputVariable(state,
                                "Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate",
                                Constant::Units::kg_s,
                                cavity.PassiveMdotWind,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
            SetupOutputVariable(state,
                                "Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate",
                                Constant::Units::kg_s,
                                cavity.PassiveMdotTherm,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                cavity.Name);
        } // for (Item)
    } // GetHTSurfExtVentedCavityData()

    void ExposedFoundationPerimeter::getData(EnergyPlusData &state, bool &ErrorsFound)
    {

        int IOStatus; // Used in GetObjectItem
        int NumAlphas;
        int NumNumbers;

        Real64 constexpr tolerance = 1e-6;

        constexpr std::string_view routineName = "ExposedFoundationPerimeter::getData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        ipsc->cCurrentModuleObject = "SurfaceProperty:ExposedFoundationPerimeter";
        int numObjects = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        for (int obj = 1; obj <= numObjects; ++obj) {
            int alpF = 1;
            int numF = 1;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     obj,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            int Found = Util::FindItemInList(ipsc->cAlphaArgs(alpF), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
            if (Found == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
            }
            alpF++;
            auto const &surf = state.dataSurface->Surface(Found);
            if (surf.Class != SurfaceClass::Floor) {
                ShowWarningError(state, format("{}: {}, surface is not a floor surface", ipsc->cCurrentModuleObject, surf.Name));
                ShowContinueError(state, format("{} will not be used", ipsc->cCurrentModuleObject));
                continue;
            }

            // Choose calculation method
            enum class CalculationMethod
            {
                Invalid = -1,
                TotalExposedPerimeter,
                ExposedPerimeterFraction,
                Bysegment,
                Num
            };

            constexpr std::array<std::string_view, (int)CalculationMethod::Num> CalculationMethodNamesUC = {
                "TOTALEXPOSEDPERIMETER", "EXPOSEDPERIMETERFRACTION", "BYSEGMENT"};
            CalculationMethod calculationMethod =
                static_cast<CalculationMethod>(getEnumValue(CalculationMethodNamesUC, ipsc->cAlphaArgs(alpF)));
            if (calculationMethod == CalculationMethod::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                ErrorsFound = true;
            }
            alpF++;

            Data data;
            data.useDetailedExposedPerimeter = true;

            if (!ipsc->lNumericFieldBlanks(numF)) {
                if (calculationMethod == CalculationMethod::TotalExposedPerimeter) {
                    data.exposedFraction = ipsc->rNumericArgs(numF) / surf.Perimeter;
                    if (data.exposedFraction > 1 + tolerance) {
                        ShowWarningError(state,
                                         format("{}: {}, {} is greater than the perimeter of {}",
                                                ipsc->cCurrentModuleObject,
                                                surf.Name,
                                                ipsc->cNumericFieldNames(numF),
                                                surf.Name));
                        ShowContinueError(state,
                                          format("{} perimeter = {}, {} exposed perimeter = {}",
                                                 surf.Name,
                                                 surf.Perimeter,
                                                 ipsc->cCurrentModuleObject,
                                                 ipsc->rNumericArgs(numF)));
                        ShowContinueError(state,
                                          format("{} will be set equal to {} perimeter",
                                                 ipsc->cNumericFieldNames(numF),
                                                 surf.Name));
                        data.exposedFraction = 1.0;
                    }

                    data.useDetailedExposedPerimeter = false;
                } else {
                    ShowWarningError(state,
                                     format("{}: {}, {} set as calculation method, but a value has been set for {}. This value will be ignored.",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            calculationMethod,
                                            ipsc->cNumericFieldNames(numF)));
                }
            } else {
                if (calculationMethod == CalculationMethod::TotalExposedPerimeter) {
                    ShowSevereError(state,
                                    format("{}: {}, {} set as calculation method, but no value has been set for {}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           calculationMethod,
                                           ipsc->cNumericFieldNames(numF)));
                    ErrorsFound = true;
                }
            }
            numF++;

            if (!ipsc->lNumericFieldBlanks(numF)) {
                if (calculationMethod == CalculationMethod::ExposedPerimeterFraction) {
                    data.exposedFraction = ipsc->rNumericArgs(numF);
                    data.useDetailedExposedPerimeter = false;
                } else {
                    ShowWarningError(state,
                                     format("{}: {}, {} set as calculation method, but a value has been set for {}. This value will be ignored.",
                                            ipsc->cCurrentModuleObject,
                                            surf.Name,
                                            calculationMethod,
                                            ipsc->cNumericFieldNames(numF)));
                }
            } else {
                if (calculationMethod == CalculationMethod::ExposedPerimeterFraction) {
                    ShowSevereError(state,
                                    format("{}: {}, {} set as calculation method, but no value has been set for {}",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           calculationMethod,
                                           ipsc->cNumericFieldNames(numF)));
                    ErrorsFound = true;
                }
            }
            numF++;

            int numRemainingFields = NumAlphas - (alpF - 1) + NumNumbers - (numF - 1);
            if (numRemainingFields > 0) {
                if (calculationMethod == CalculationMethod::Bysegment) {
                    if (numRemainingFields != (int)surf.Vertex.size()) {
                        ShowSevereError(state,
                                        format("{}: {}, must have equal number of segments as the floor has vertices.{}\" and \"{}\"",
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cAlphaFieldNames(alpF),
                                               ipsc->cNumericFieldNames(numF - 1)));
                        ShowContinueError(state,
                                          format("{} number of vertices = {}, {} number of segments = {}",
                                                 surf.Name,
                                                 surf.Vertex.size(),
                                                 ipsc->cCurrentModuleObject,
                                                 numRemainingFields));
                        ErrorsFound = true;
                    }
                    for (int segNum = 0; segNum < numRemainingFields; segNum++) {
                        BooleanSwitch bs;
                        if (ipsc->lAlphaFieldBlanks(alpF)) {
                            ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                            ErrorsFound = true;
                        } else if ((bs = getYesNoValue(ipsc->cAlphaArgs(alpF))) == BooleanSwitch::Invalid) {
                            ShowSevereInvalidBool(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                            ErrorsFound = true;
                        } else {
                            data.isExposedPerimeter.push_back(static_cast<bool>(bs));
                        } 
                        alpF++;
                    }
                }
            } else {
                if (calculationMethod == CalculationMethod::Bysegment) {
                    ShowSevereError(state,
                                    format("{}: {}, {} set as calculation method, but no values have been set for Surface Segments Exposed",
                                           ipsc->cCurrentModuleObject,
                                           surf.Name,
                                           calculationMethod));
                    ErrorsFound = true;
                }
            }
            surfaceMap[Found] = data;
        } // for (obj)
    } // ExposedFoundationPerimeter::getData()

    void GetSurfaceLocalEnvData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         X LUO
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for Outdoor Air Node for exterior surfaces

        // Using/Aliasing
        using namespace DataErrorTracking;
        using DataLoopNode::ObjectIsParent;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSurfaceLocalEnvData: ");

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumeric;
        int IOStat;

        constexpr std::string_view routineName = "GetSurfaceLocalEnvData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        //-----------------------------------------------------------------------
        //                SurfaceProperty:LocalEnvironment
        //-----------------------------------------------------------------------
        
        ipsc->cCurrentModuleObject = "SurfaceProperty:LocalEnvironment";
        state.dataSurface->TotSurfLocalEnv = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (state.dataSurface->TotSurfLocalEnv > 0) {

            state.dataGlobal->AnyLocalEnvironmentsInModel = true;

            if (!allocated(state.dataSurface->SurfLocalEnvironment)) {
                state.dataSurface->SurfLocalEnvironment.allocate(state.dataSurface->TotSurfLocalEnv);
            }

            for (int Loop = 1; Loop <= state.dataSurface->TotSurfLocalEnv; ++Loop) {

                auto &SurfLocalEnv = state.dataSurface->SurfLocalEnvironment(Loop);

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlpha,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumeric,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                if (ipsc->lAlphaFieldBlanks(1)) {
                    ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                    ErrorsFound = true;
                    continue;
                }

                SurfLocalEnv.Name = ipsc->cAlphaArgs(1);

                // Assign surface number
                SurfLocalEnv.SurfPtr = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataSurface->Surface);
                if (SurfLocalEnv.SurfPtr == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ErrorsFound = true;
                    continue;
                }

                // Assign Sunlit Fraction Schedule number
                if (!ipsc->lAlphaFieldBlanks(3)) {
                    SurfLocalEnv.SunlitFracSchedPtr = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
                    if (SurfLocalEnv.SunlitFracSchedPtr == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                        ErrorsFound = true;
                        continue;
                    }
                }

                // Assign surrounding surfaces object number;
                if (!ipsc->lAlphaFieldBlanks(4)) {
                    SurfLocalEnv.SurroundingSurfsPtr = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataSurface->SurroundingSurfsProperty);
                    if (SurfLocalEnv.SurroundingSurfsPtr == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                        ErrorsFound = true;
                        continue;
                    }
                }

                // Assign outdoor air node number;
                if (!ipsc->lAlphaFieldBlanks(5)) {
                    SurfLocalEnv.OutdoorAirNodePtr = GetOnlySingleNode(state,
                                                                       ipsc->cAlphaArgs(5),
                                                                       ErrorsFound,
                                                                       DataLoopNode::ConnectionObjectType::SurfacePropertyLocalEnvironment,
                                                                       SurfLocalEnv.Name,
                                                                       DataLoopNode::NodeFluidType::Air,
                                                                       DataLoopNode::ConnectionType::Inlet,
                                                                       NodeInputManager::CompFluidStream::Primary,
                                                                       ObjectIsParent);
                    if (SurfLocalEnv.OutdoorAirNodePtr == 0 && CheckOutAirNodeNumber(state, SurfLocalEnv.OutdoorAirNodePtr)) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(5), ipsc->cAlphaArgs(5));
                        ErrorsFound = true;
                        continue;
                    }
                }

                // get ground surfaces object number;
                if (!ipsc->lAlphaFieldBlanks(6)) {
                    SurfLocalEnv.GroundSurfsPtr = Util::FindItemInList(ipsc->cAlphaArgs(6), state.dataSurface->GroundSurfsProperty);
                    if (SurfLocalEnv.GroundSurfsPtr == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(6), ipsc->cAlphaArgs(6));
                        ErrorsFound = true;
                        continue;
                    }
                }
            }
        } // if (TotalLocalEnv > 0)
        
        // Link surface properties to surface object
        for (int SurfLoop = 1; SurfLoop <= state.dataSurface->TotSurfaces; ++SurfLoop) {
            for (int Loop = 1; Loop <= state.dataSurface->TotSurfLocalEnv; ++Loop) {
                auto &SurfLocalEnv = state.dataSurface->SurfLocalEnvironment(Loop);
                if (SurfLocalEnv.SurfPtr == SurfLoop) {
                    auto &surface = state.dataSurface->Surface(SurfLoop);
                    if (SurfLocalEnv.OutdoorAirNodePtr != 0) {
                        surface.SurfLinkedOutAirNode = SurfLocalEnv.OutdoorAirNodePtr;
                    }
                    if (SurfLocalEnv.SunlitFracSchedPtr != 0) {
                        surface.SurfSchedExternalShadingFrac = true;
                        surface.SurfExternalShadingSchInd = SurfLocalEnv.SunlitFracSchedPtr;
                    }
                    if (SurfLocalEnv.SurroundingSurfsPtr != 0) {
                        surface.SurfHasSurroundingSurfProperty = true;
                        surface.SurfSurroundingSurfacesNum = SurfLocalEnv.SurroundingSurfsPtr;
                        surface.ViewFactorSrdSurfs =
                            state.dataSurface->SurroundingSurfsProperty(surface.SurfSurroundingSurfacesNum).SurfsViewFactorSum;
                        if (surface.ViewFactorSrdSurfs == 0.0) {
                            surface.SurfHasSurroundingSurfProperty = false;
                        }
                    }
                    if (SurfLocalEnv.GroundSurfsPtr != 0) {
                        surface.IsSurfPropertyGndSurfacesDefined = true;
                        surface.UseSurfPropertyGndSurfTemp = true;
                        surface.UseSurfPropertyGndSurfRefl = true;
                        surface.SurfPropertyGndSurfIndex = SurfLocalEnv.GroundSurfsPtr;
                    }
                }
            }
        }
    } // GetSurfaceLocalEnvData()

    void GetSurfaceSrdSurfsData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         X LUO
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for surrounding surfaces properties for exterior surfaces

        // Using/Aliasing
        using namespace DataErrorTracking;
        using DataLoopNode::ObjectIsParent;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumeric;
        int IOStat;
        int TotSrdSurfProperties;

        constexpr std::string_view routineName = "GetSurfaceSrdSurfsData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        //-----------------------------------------------------------------------
        //                SurfaceProperty:SurroundingSurfaces
        //-----------------------------------------------------------------------

        ipsc->cCurrentModuleObject = "SurfaceProperty:SurroundingSurfaces";
        TotSrdSurfProperties = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (TotSrdSurfProperties == 0) return;
                

        if (!allocated(state.dataSurface->SurroundingSurfsProperty)) {
            state.dataSurface->SurroundingSurfsProperty.allocate(TotSrdSurfProperties);
        }
        
        for (int Loop = 1; Loop <= TotSrdSurfProperties; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumeric,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            if (ipsc->lAlphaFieldBlanks(1)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                ErrorsFound = true;
                continue;
            }
                        
            auto &SrdSurfsProp = state.dataSurface->SurroundingSurfsProperty(Loop);
            
            SrdSurfsProp.Name = ipsc->cAlphaArgs(1);
            if (!ipsc->lNumericFieldBlanks(1)) {
                SrdSurfsProp.SkyViewFactor = ipsc->rNumericArgs(1);
                SrdSurfsProp.IsSkyViewFactorSet = true;
            }

            if (!ipsc->lAlphaFieldBlanks(2)) {
                SrdSurfsProp.SkyTempSchNum = GetScheduleIndex(state, ipsc->cAlphaArgs(2));
                // These schedules are not checked for existence for some reason
                // if (SrdSurfsProp.SkyTempSchNum == 0) {
                //    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                //    ErrorsFound = true;
                // }
            }

                // N2: ground view factor
            if (!ipsc->lNumericFieldBlanks(2)) {
                SrdSurfsProp.GroundViewFactor = ipsc->rNumericArgs(2);
                SrdSurfsProp.IsGroundViewFactorSet = true;
            }
            
            // A3: ground temp sch name
            if (!ipsc->lAlphaFieldBlanks(3)) {
                SrdSurfsProp.GroundTempSchNum = GetScheduleIndex(state, ipsc->cAlphaArgs(3));
                // These schedules are not checked for existence for some reason
                // if (SrdSurfsProp.GroundTempSchNum == 0) {
                //    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                //    ErrorsFound = true;
                // }
            }

            // The object requires at least one srd surface input, each surface requires a set of 3 fields (2 Alpha fields Name and Temp
            // Sch Name and 1 Num fields View Factor)
            if (NumAlpha < 5) {
                ShowSevereError(state, format("{} = \"{}\" is not defined correctly.", ipsc->cCurrentModuleObject, SrdSurfsProp.Name));
                ShowContinueError(state, "At lease one set of surrounding surface properties should be defined.");
                ErrorsFound = true;
                continue;
            }
            if ((NumAlpha - 3) / 2 != (NumNumeric - 2)) {
                ShowSevereError(state, format("{} = \"{}\" is not defined correctly.", ipsc->cCurrentModuleObject, SrdSurfsProp.Name));
                ShowContinueError(state, "Check number of input fields for each surrounding surface.");
                ErrorsFound = true;
                continue;
            }
            // Read surrounding surfaces properties
            SrdSurfsProp.TotSurroundingSurface = NumNumeric - 2;
            SrdSurfsProp.SurroundingSurfs.allocate(SrdSurfsProp.TotSurroundingSurface);
            for (int SurfLoop = 1; SurfLoop <= SrdSurfsProp.TotSurroundingSurface; ++SurfLoop) {
                auto &surSurf = SrdSurfsProp.SurroundingSurfs(SurfLoop);
                surSurf.Name = ipsc->cAlphaArgs(SurfLoop * 2 + 2);
                surSurf.ViewFactor = ipsc->rNumericArgs(SurfLoop + 2);
                surSurf.TempSchNum = GetScheduleIndex(state, ipsc->cAlphaArgs(SurfLoop * 2 + 3));
                SrdSurfsProp.SurfsViewFactorSum += surSurf.ViewFactor;
            }
        }
    } // GetSurfaceSrdSurfsData()

    void GetSurfaceGroundSurfsData(EnergyPlusData &state, bool &ErrorsFound)
    {
        std::string const cCurrentModuleObject = "SurfaceProperty:GroundSurfaces";
        state.dataSurface->TotSurfPropGndSurfs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            if (state.dataSurface->TotSurfPropGndSurfs > 0) ErrorsFound = true;
            return;
        }
        
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            std::string const &thisObjectName = instance.key();
            GroundSurfacesProperty thisGndSurfsObj;
            thisGndSurfsObj.Name = Util::makeUPPER(thisObjectName);
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
            auto groundSurfaces = fields.find("ground_surfaces");
            if (groundSurfaces != fields.end()) {
                auto &groundSurfacesArray = groundSurfaces.value();
                thisGndSurfsObj.NumGndSurfs = groundSurfacesArray.size();
                for (auto &groundSurface : groundSurfacesArray) {
                    GroundSurfacesData thisGndSurf;
                    auto GndSurfName = groundSurface.find("ground_surface_name");
                    if (GndSurfName != groundSurface.end()) {
                        std::string ground_surf_name = GndSurfName.value().get<std::string>();
                        if (!ground_surf_name.empty()) {
                            thisGndSurf.Name = Util::makeUPPER(ground_surf_name);
                        }
                    }
                    auto groundSurfViewFactor = groundSurface.find("ground_surface_view_factor");
                    if (groundSurfViewFactor != groundSurface.end()) {
                        thisGndSurf.ViewFactor = groundSurfViewFactor.value().get<Real64>();
                        thisGndSurfsObj.IsGroundViewFactorSet = true;
                    }
                    auto TempSchName = groundSurface.find("ground_surface_temperature_schedule_name");
                    if (TempSchName != groundSurface.end()) {
                        std::string gnd_surf_TempSchName = TempSchName.value().get<std::string>();
                        if (!gnd_surf_TempSchName.empty()) {
                            thisGndSurf.TempSchPtr = ScheduleManager::GetScheduleIndex(state, Util::makeUPPER(gnd_surf_TempSchName));
                        }
                    }
                    auto ReflSchName = groundSurface.find("ground_surface_reflectance_schedule_name");
                    if (ReflSchName != groundSurface.end()) {
                        std::string gnd_surf_ReflSchName = ReflSchName.value().get<std::string>();
                        if (!gnd_surf_ReflSchName.empty()) {
                                thisGndSurf.ReflSchPtr = ScheduleManager::GetScheduleIndex(state, Util::makeUPPER(gnd_surf_ReflSchName));
                        }
                    }
                    thisGndSurfsObj.GndSurfs.push_back(thisGndSurf);
                }
            }
            for (int gSurfNum = 1; gSurfNum <= thisGndSurfsObj.NumGndSurfs; gSurfNum++) {
                thisGndSurfsObj.SurfsViewFactorSum += thisGndSurfsObj.GndSurfs(gSurfNum).ViewFactor;
            }
            state.dataSurface->GroundSurfsProperty.push_back(thisGndSurfsObj);
        }
        
        // set report variables
        if (state.dataSurface->TotSurfPropGndSurfs == 0) return;

        for (int Loop = 1; Loop <= state.dataSurface->TotSurfPropGndSurfs; Loop++) {
            bool SetTempSchReportVar = true;
            bool SetReflSchReportVar = true;
            auto &thisGndSurfsObj = state.dataSurface->GroundSurfsProperty(Loop);
            for (int gSurfNum = 1; gSurfNum <= thisGndSurfsObj.NumGndSurfs; gSurfNum++) {
                if (thisGndSurfsObj.GndSurfs(gSurfNum).TempSchPtr != 0 && SetTempSchReportVar) {
                    SetupOutputVariable(state,
                                        "Surfaces Property Ground Surfaces Average Temperature",
                                        Constant::Units::C,
                                        thisGndSurfsObj.SurfsTempAvg,
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::State,
                                        thisGndSurfsObj.Name);
                    SetTempSchReportVar = false;
                }
                if (thisGndSurfsObj.GndSurfs(gSurfNum).ReflSchPtr != 0 && SetReflSchReportVar) {
                    SetupOutputVariable(state,
                                        "Surfaces Property Ground Surfaces Average Reflectance",
                                        Constant::Units::None,
                                        thisGndSurfsObj.SurfsReflAvg,
                                        OutputProcessor::SOVTimeStepType::Zone,
                                        OutputProcessor::SOVStoreType::State,
                                        thisGndSurfsObj.Name);
                    SetReflSchReportVar = false;
                }
            }
        } // for (Loop)
    } // GetSurfaceGroundSurfsData()

    void GetSurfaceHeatTransferAlgorithmOverrides(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith, portions from ApplyConvectionValue by Linda Lawrie
        //       DATE WRITTEN   July 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // <description>

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CountHTAlgoObjectsSingleSurf;
        int CountHTAlgoObjectsMultiSurf;
        int CountHTAlgoObjectsSurfList;
        int IOStatus; // Used in GetObjectItem
        bool ErrorsFoundSingleSurf(false);
        bool ErrorsFoundMultiSurf(false);
        bool ErrorsFoundSurfList(false);
        bool ErrorsFoundByConstruct(false);
        DataSurfaces::HeatTransferModel tmpAlgoInput;
        int Item;
        int Item1;
        int NumAlphas;
        int NumNumbers;
        int Found;
        bool SurfacesOfType;
        int SurfNum;
        //  INTEGER :: Index
        int NumEMPDMat;
        int NumPCMat;
        int NumVTCMat;
        int NumHAMTMat1;
        int NumHAMTMat2;
        int NumHAMTMat3;
        int NumHAMTMat4;
        int NumHAMTMat5;
        int NumHAMTMat6;
        int SumHAMTMat;
        bool msgneeded;

        constexpr std::string_view routineName = "GetSurfaceHeatTransferAlgorithmOverrides";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;
        
        ipsc->cCurrentModuleObject = "SurfaceProperty:HeatBalanceSourceTerm";
        int CountAddHeatSourceSurf = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        for (int Item = 1; Item <= CountAddHeatSourceSurf; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:HeatBalanceSourceTerm";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            Found = Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);

            if (Found == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFound = true;
                continue;
            }                

            auto &surf = state.dataSurface->Surface(Found);
            if (surf.InsideHeatSourceTermSchedule || surf.OutsideHeatSourceTermSchedule) {
                ShowSevereError(state,
                                format("{}=\"{}\", multiple SurfaceProperty:HeatBalanceSourceTerm objects applied to the same surface.",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }

            if (!ipsc->lAlphaFieldBlanks(2)) {
                surf.InsideHeatSourceTermSchedule = ScheduleManager::GetScheduleIndex(state, ipsc->cAlphaArgs(2));
                if (surf.InsideHeatSourceTermSchedule == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                    ErrorsFound = true;
                } else {
                    state.dataSurface->allInsideSourceSurfaceList.emplace_back(Found);
                }
            }
            
            if (!ipsc->lAlphaFieldBlanks(3)) {
                surf.OutsideHeatSourceTermSchedule = ScheduleManager::GetScheduleIndex(state, ipsc->cAlphaArgs(3));
                if (surf.OutsideHeatSourceTermSchedule == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    ErrorsFound = true;
                } else if (surf.OSCPtr > 0) {
                    ShowSevereError(state,
                                    format("{}=\"SurfaceProperty:HeatBalanceSourceTerm\", cannot be specified for OtherSideCoefficient Surface={}",
                                           ipsc->cCurrentModuleObject,
                                           ipsc->cAlphaArgs(1)));
                    ErrorsFound = true;
                } else {
                    state.dataSurface->allOutsideSourceSurfaceList.emplace_back(Found);
                }
            }

            if (surf.OutsideHeatSourceTermSchedule == 0 && surf.InsideHeatSourceTermSchedule == 0) {
                ShowSevereError(
                    state,
                    format("{}=\"{}\", no schedule defined for additional heat source.", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        // first initialize each heat transfer surface with the overall model type, array assignment
        for (auto &surf : state.dataSurface->Surface)
            surf.HeatTransferAlgorithm = state.dataHeatBal->OverallHeatTransferSolutionAlgo;

        ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
        CountHTAlgoObjectsSingleSurf = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        for (int Item = 1; Item <= CountHTAlgoObjectsSingleSurf; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            ErrorsFoundSingleSurf = false;
            Found = Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);

            if (Found == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(1), ipsc->cAlphaArgs(1));
                ErrorsFoundSingleSurf = true;
            }

            auto &surf = state.dataSurface->Surface(Found);
            surf.HeatTransferAlgorithm = static_cast<HeatTransferModel>(getEnumValue(heatTransferModelNamesUC, ipsc->cAlphaArgs(2)));
            switch (surf.HeatTransferAlgorithm) {
            case HeatTransferModel::CTF: {
                state.dataHeatBal->AnyCTF = true;
            } break;
            case HeatTransferModel::EMPD: {
                state.dataHeatBal->AnyEMPD = true;
            } break;
            case HeatTransferModel::HAMT: {
                state.dataHeatBal->AnyHAMT = true;
            } break;
            case HeatTransferModel::CondFD: {
                    state.dataHeatBal->AnyCondFD = true;
            } break;
            default: {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFoundSingleSurf = true;
            } break;
            }

            if (ErrorsFoundSingleSurf) {
                ErrorsFound = true;
            }
        } // single surface heat transfer algorithm override

        ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
        CountHTAlgoObjectsMultiSurf = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        for (int Item = 1; Item <= CountHTAlgoObjectsMultiSurf; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ""};
            
            ErrorsFoundMultiSurf = false;

            HeatTransferModel htAlgo = static_cast<HeatTransferModel>(getEnumValue(heatTransferModelNamesUC, ipsc->cAlphaArgs(3)));

            switch (htAlgo) {
            case HeatTransferModel::CTF: {
                state.dataHeatBal->AnyCTF = true;
            } break;
            case HeatTransferModel::EMPD: {
                state.dataHeatBal->AnyEMPD = true;
            } break;
            case HeatTransferModel::HAMT: {
                state.dataHeatBal->AnyHAMT = true;
            } break;
            case HeatTransferModel::CondFD: {
                state.dataHeatBal->AnyCondFD = true;
            } break;
            default: {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFoundMultiSurf = true;
            } break;
            }

            SurfaceFilter surfFilter = static_cast<SurfaceFilter>(getEnumValue(SurfaceFilterNamesUC, ipsc->cAlphaArgs(2)));
            switch (surfFilter) {
            case SurfaceFilter::AllExteriorSurfaces: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond > 0) continue; // Interior surfaces
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = htAlgo;
                }
            } break;

            case SurfaceFilter::AllExteriorWalls: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond > 0) continue; // Interior surfaces
                    if (surf.Class != SurfaceClass::Wall) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = htAlgo;
                }
            } break;

            case SurfaceFilter::AllExteriorRoofs: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond > 0) continue; // Interior surfaces
                    if (surf.Class != SurfaceClass::Roof) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = htAlgo;
                }
            } break;
                    
            case SurfaceFilter::AllExteriorFloors: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond > 0) continue; // Interior surfaces
                    if (surf.Class != SurfaceClass::Floor) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = htAlgo;
                }
            } break;
                    
            case SurfaceFilter::AllGroundContactSurfaces: { 
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond != Ground) continue; // ground BC
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = htAlgo;
                }
            } break;

            case SurfaceFilter::AllInteriorSurfaces: { 
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond <= 0) continue; // Exterior surfaces
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = tmpAlgoInput;
                }
            } break;

            case SurfaceFilter::AllInteriorWalls: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond <= 0) continue; // Exterior surfaces
                    if (surf.Class != SurfaceClass::Wall) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = tmpAlgoInput;
                }
            } break;

            case SurfaceFilter::AllInteriorRoofs:
            case SurfaceFilter::AllInteriorCeilings: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond <= 0) continue; // Exterior surfaces
                    if (surf.Class != SurfaceClass::Roof) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = tmpAlgoInput;
                }
            } break;

            case SurfaceFilter::AllInteriorFloors: {
                SurfacesOfType = false;
                for (auto &surf : state.dataSurface->Surface) {
                    if (!surf.HeatTransSurf) continue;
                    if (surf.ExtBoundCond <= 0) continue; // Exterior surfaces
                    if (surf.Class != SurfaceClass::Floor) continue;
                    if (state.dataConstruction->Construct(surf.Construction).TypeIsWindow) continue;
                    SurfacesOfType = true;
                    surf.HeatTransferAlgorithm = tmpAlgoInput;
                }
            } break;

            default: {
                SurfacesOfType = false;
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFoundMultiSurf = true;
            } break;
            } // switch (surfFilter)

            if (!SurfacesOfType) {
                ShowWarningError(
                    state,
                    format("In {}=\"{}\", for Multiple Surface Assignment=\"{}\", there were no surfaces of that type found for assignment.",
                           ipsc->cCurrentModuleObject,
                           ipsc->cAlphaArgs(1),
                           ipsc->cAlphaArgs(2)));
            }
            if (ErrorsFoundMultiSurf) ErrorsFound = true;

        } // multi surface heat transfer algo override

        ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
        CountHTAlgoObjectsSurfList = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (int Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ""};
            ErrorsFoundSurfList = false;

            HeatTransferModel htAlgo = static_cast<HeatTransferModel>(getEnumValue(heatTransferModelNamesUC, ipsc->cAlphaArgs(3)));

            switch (htAlgo) {
            case HeatTransferModel::CTF: {
                state.dataHeatBal->AnyCTF = true;
            } break;
            case HeatTransferModel::EMPD: {
                state.dataHeatBal->AnyEMPD = true;
            } break;
            case HeatTransferModel::HAMT: {
                state.dataHeatBal->AnyHAMT = true;
            } break;
            case HeatTransferModel::CondFD: {
                state.dataHeatBal->AnyCondFD = true;
            } break;
            default: {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFoundMultiSurf = true;
            } break;
            }

            for (Item1 = 3; Item1 <= NumAlphas; ++Item1) {

                Found = Util::FindItemInList(ipsc->cAlphaArgs(Item1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);

                if (Found == 0) {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(Item1), ipsc->cAlphaArgs(Item1)); 
                    ErrorsFoundSurfList = true;
                }

                if (!ErrorsFoundSurfList) {
                    state.dataSurface->Surface(Found).HeatTransferAlgorithm = htAlgo;
                } else {
                    ErrorsFound = true;
                }
            }
        }

        ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
        CountHTAlgoObjectsSurfList = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item) {
            ipsc->cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Item,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ""};
            ErrorsFoundByConstruct = false;

            HeatTransferModel htAlgo = static_cast<HeatTransferModel>(getEnumValue(heatTransferModelNamesUC, ipsc->cAlphaArgs(2)));

            switch (htAlgo) {
            case HeatTransferModel::CTF: {
                state.dataHeatBal->AnyCTF = true;
            } break;
            case HeatTransferModel::EMPD: {
                state.dataHeatBal->AnyEMPD = true;
            } break;
            case HeatTransferModel::HAMT: {
                state.dataHeatBal->AnyHAMT = true;
            } break;
            case HeatTransferModel::CondFD: {
                state.dataHeatBal->AnyCondFD = true;
            } break;
            default: {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFoundMultiSurf = true;
            } break;
            }

            Found = Util::FindItemInList(ipsc->cAlphaArgs(3), state.dataConstruction->Construct, state.dataHeatBal->TotConstructs);
            if (Found == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFoundByConstruct = true;
            }

            if (!ErrorsFoundByConstruct) {
                for (auto &surf : state.dataSurface->Surface) { 
                    if (surf.Construction == Found) {
                        surf.HeatTransferAlgorithm = htAlgo;
                    }
                }
            }
        } // for (Item)

        // Change algorithm for Kiva and air boundary foundation surfaces
        for (auto &surf : state.dataSurface->Surface) {
            if (surf.ExtBoundCond == KivaFoundation) {
                surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::Kiva;
                state.dataHeatBal->AnyKiva = true;
            }
        }

        // test for missing materials for algorithms selected
        NumEMPDMat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:MoisturePenetrationDepth:Settings");
        NumPCMat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:PhaseChange") +
                   state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:PhaseChangeHysteresis");
        NumVTCMat = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:VariableThermalConductivity");
        NumHAMTMat1 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Settings");
        NumHAMTMat2 =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm");
        NumHAMTMat3 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Suction");
        NumHAMTMat4 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Redistribution");
        NumHAMTMat5 = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Diffusion");
        NumHAMTMat6 =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity");
        SumHAMTMat = NumHAMTMat1 + NumHAMTMat2 + NumHAMTMat3 + NumHAMTMat4 + NumHAMTMat5 + NumHAMTMat6;
        msgneeded = false;

        if (NumEMPDMat > 0 && !state.dataHeatBal->AnyEMPD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:MoisturePenetrationDepth:Settings objects but the moisture "
                                    "penetration depth algorithm is not used anywhere.",
                                    NumEMPDMat));
            msgneeded = true;
        }
        if (NumPCMat > 0 && !state.dataHeatBal->AnyCondFD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:PhaseChange objects but the conduction finite difference algorithm "
                                    "is not used anywhere.",
                                    NumPCMat));
            msgneeded = true;
        }
        if (NumVTCMat > 0 && !state.dataHeatBal->AnyCondFD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:VariableThermalConductivity objects but the conduction finite "
                                    "difference algorithm is not used anywhere.",
                                    NumVTCMat));
            msgneeded = true;
        }
        if (SumHAMTMat > 0 && !state.dataHeatBal->AnyHAMT) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:HeatAndMoistureTransfer:* objects but the combined heat and "
                                    "moisture finite difference algorithm is not used anywhere.",
                                    SumHAMTMat));
            msgneeded = true;
        }
        if (msgneeded) {
            ShowContinueError(state, "Previous materials will be ignored due to HeatBalanceAlgorithm choice.");
        }
        msgneeded = false;
        if (NumEMPDMat == 0 && state.dataHeatBal->AnyEMPD) {
            ShowWarningError(state,
                             "The moisture penetration depth conduction transfer function algorithm is used but the input file includes no "
                             "MaterialProperty:MoisturePenetrationDepth:Settings objects.");
            msgneeded = true;
        }
        if (SumHAMTMat == 0 && state.dataHeatBal->AnyHAMT) {
            ShowWarningError(state,
                             "The combined heat and moisture finite element algorithm is used but the input file includes no "
                             "MaterialProperty:HeatAndMoistureTransfer:* objects.");
            msgneeded = true;
        }
        if (msgneeded) {
            ShowContinueError(state,
                              "Certain materials objects are necessary to achieve proper results with the heat transfer algorithm(s) selected.");
        }

        // Write Solution Algorithm to the initialization output file for User Verification
        print(state.files.eio,
              "{}\n",
              "! <Surface Heat Transfer Algorithm>, Value {CTF - ConductionTransferFunction | EMPD - "
              "MoisturePenetrationDepthConductionTransferFunction | CondFD - ConductionFiniteDifference | HAMT - "
              "CombinedHeatAndMoistureFiniteElement} - Description,Inside Surface Max Temperature Limit{C}, Surface "
              "Convection Coefficient Lower Limit {W/m2-K}, Surface Convection Coefficient Upper Limit {W/m2-K}");

        int numberOfHeatTransferAlgosUsed = 0;
        // Formats
        static constexpr std::string_view Format_725("Surface Heat Transfer Algorithm, {},{:.0R},{:.2R},{:.1R}\n");

        if (state.dataHeatBal->AnyCTF) {
            constexpr std::string_view AlgoName = "CTF - ConductionTransferFunction";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio,
                  Format_725,
                  AlgoName,
                  state.dataHeatBalSurf->MaxSurfaceTempLimit,
                  state.dataHeatBal->LowHConvLimit,
                  state.dataHeatBal->HighHConvLimit);
        }
        if (state.dataHeatBal->AnyEMPD) {
            state.dataHeatBal->AllCTF = false;
            constexpr std::string_view AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio,
                  Format_725,
                  AlgoName,
                  state.dataHeatBalSurf->MaxSurfaceTempLimit,
                  state.dataHeatBal->LowHConvLimit,
                  state.dataHeatBal->HighHConvLimit);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation || state.dataHeatBal->doSpaceHeatBalanceSizing) {
                ShowSevereError(
                    state,
                    "MoisturePenetrationDepthConductionTransferFunction is not supported with ZoneAirHeatBalanceAlgorithm Space Heat Balance.");
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->AnyCondFD) {
            state.dataHeatBal->AllCTF = false;
            constexpr std::string_view AlgoName = "CondFD - ConductionFiniteDifference";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio,
                  Format_725,
                  AlgoName,
                  state.dataHeatBalSurf->MaxSurfaceTempLimit,
                  state.dataHeatBal->LowHConvLimit,
                  state.dataHeatBal->HighHConvLimit);
        }
        if (state.dataHeatBal->AnyHAMT) {
            state.dataHeatBal->AllCTF = false;
            constexpr std::string_view AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio,
                  Format_725,
                  AlgoName,
                  state.dataHeatBalSurf->MaxSurfaceTempLimit,
                  state.dataHeatBal->LowHConvLimit,
                  state.dataHeatBal->HighHConvLimit);
            if (state.dataHeatBal->doSpaceHeatBalanceSimulation || state.dataHeatBal->doSpaceHeatBalanceSizing) {
                ShowSevereError(state, "CombinedHeatAndMoistureFiniteElement is not supported with ZoneAirHeatBalanceAlgorithm Space Heat Balance.");
                ErrorsFound = true;
            }
        }
        if (state.dataHeatBal->AnyKiva) {
            state.dataHeatBal->AllCTF = false;
            constexpr std::string_view AlgoName = "KivaFoundation - TwoDimensionalFiniteDifference";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio,
                  Format_725,
                  AlgoName,
                  state.dataHeatBalSurf->MaxSurfaceTempLimit,
                  state.dataHeatBal->LowHConvLimit,
                  state.dataHeatBal->HighHConvLimit);
        }

        // Check HeatTransferAlgorithm for interior surfaces
        if (numberOfHeatTransferAlgosUsed > 1) {
            int ExtSurfNum;
            for (Item = 1; Item <= state.dataSurface->TotSurfaces; ++Item) {
                auto &surf = state.dataSurface->Surface(Item);
                if (surf.ExtBoundCond > 0) {
                    if ((surf.HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::Invalid) ||
                        (surf.HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::None))
                        continue;
                    ExtSurfNum = surf.ExtBoundCond;
                    auto &extSurf = state.dataSurface->Surface(ExtSurfNum);
                    if (surf.HeatTransferAlgorithm != extSurf.HeatTransferAlgorithm) {
                        ShowWarningError(state,
                                         "An interior surface is defined as two surfaces with reverse constructions. The HeatTransferAlgorithm in "
                                         "both constructions should be same.");
                        ShowContinueError(state,
                                          format("The HeatTransferAlgorithm of Surface: {}, is {}",
                                                 surf.Name,
                                                 DataSurfaces::HeatTransAlgoStrs[(int)surf.HeatTransferAlgorithm]));
                        ShowContinueError(state,
                                          format("The HeatTransferAlgorithm of Surface: {}, is {}",
                                                 extSurf.Name,
                                                 DataSurfaces::HeatTransAlgoStrs[(int)extSurf.HeatTransferAlgorithm]));
                        if (surf.HeatTransferAlgorithm > extSurf.HeatTransferAlgorithm) {
                            ShowContinueError(state,
                                              format("The HeatTransferAlgorithm of Surface: {}, is assigned to {}. Simulation continues.",
                                                     extSurf.Name,
                                                     DataSurfaces::HeatTransAlgoStrs[(int)surf.HeatTransferAlgorithm]));
                            extSurf.HeatTransferAlgorithm = surf.HeatTransferAlgorithm;
                        } else {
                            ShowContinueError(state,
                                              format("The HeatTransferAlgorithm of Surface: {}, is assigned to {}. Simulation continues.",
                                                     surf.Name,
                                                     DataSurfaces::HeatTransAlgoStrs[(int)extSurf.HeatTransferAlgorithm]));
                            surf.HeatTransferAlgorithm = extSurf.HeatTransferAlgorithm;
                        }
                    }
                }
            }
        }

        // Assign model type to windows, shading surfaces, and TDDs
        for (int Item = 1; Item <= state.dataSurface->TotSurfaces; ++Item) {
            auto &surf = state.dataSurface->Surface(Item);
            if (surf.Class == SurfaceClass::Window || surf.Class == SurfaceClass::GlassDoor) {
                // todo, add complex fenestration switch  HeatTransferModel_ComplexFenestration
                if (state.dataSurface->SurfWinWindowModelType(Item) == WindowModel::BSDF) {
                    surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::ComplexFenestration;
                } else {
                    surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::Window5;
                }
            }
            if (surf.Class == SurfaceClass::Detached_B ||
                surf.Class == SurfaceClass::Detached_F ||
                surf.Class == SurfaceClass::Shading || surf.Class == SurfaceClass::Overhang ||
                surf.Class == SurfaceClass::Fin) {
                surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::None;
            }
            if (surf.Class == SurfaceClass::TDD_Diffuser ||
                surf.Class == SurfaceClass::TDD_Dome) {
                surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::TDD;
            }

            if (surf.HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::CTF ||
                surf.HeatTransferAlgorithm == DataSurfaces::HeatTransferModel::EMPD) {
                state.dataConstruction->Construct(surf.Construction).IsUsedCTF = true;
            }
        }
    } // GetSurfaceHeatTransferAlgorithmOverrides()

    struct PopCoincidentVertexReturn
    {
        double perimeter;
        int poppedVertexPos = -1; // This is a STL vector position, 0-indexed
        int keptVertexPos = -1;
    };

    PopCoincidentVertexReturn checkPopCoincidentVertex(const Array1D<Vector3<Real64>> &vertices)
    {
        constexpr double tolerance = 0.01;

        size_t const nSides = vertices.size();

        // Pass one: Vector of distance from this vertex to the next one
        std::vector<Real64> distances(nSides);
        size_t index = 0;
        double min_distance = std::numeric_limits<Real64>::max();
        double perimeter = 0.0;
        for (auto it = vertices.begin(); it != vertices.end(); ++it) {
            auto itnext = std::next(it);
            if (itnext == std::end(vertices)) {
                itnext = std::begin(vertices);
            }
            const auto dist = distance(*it, *itnext);
            distances[index++] = dist;
            min_distance = std::min(min_distance, dist);
            perimeter += dist;
        }
        // Return early if nothing to be popped
        if (min_distance >= tolerance) {
            return {perimeter};
        }

        // Pass two: figure out the vertex that is coincident with its previous and/or next vertex and
        // that minimizes the (distanceThisToNext + distanceThisToPrev).
        Real64 min_weight = std::numeric_limits<Real64>::max();
        int poppedVertexPos = -1;
        int keptVertexPos = -1;

        for (size_t index = 0; index < nSides; ++index) {
            size_t const prevIndex = (index == 0) ? nSides - 1 : index - 1;
            Real64 distanceThisToNext = distances[index];
            Real64 distanceThisToPrev = distances[prevIndex];
            if ((distanceThisToNext >= tolerance) && (distanceThisToPrev >= tolerance)) {
                continue;
            }
            Real64 const weight = distanceThisToNext + distanceThisToPrev;
            if (weight < min_weight) {
                min_weight = weight;
                poppedVertexPos = static_cast<int>(index);
                if (distanceThisToPrev < distanceThisToNext) {
                    keptVertexPos = prevIndex;
                } else {
                    keptVertexPos = static_cast<int>((index == nSides - 1) ? 0 : index + 1);
                }
            }
        }

        // Return the keptVertexPos (which can be the previous or the next), so we can print the displayExtraWarning correctly
        return {perimeter, poppedVertexPos, keptVertexPos};
    }

    void GetVertices(EnergyPlusData &state,
                     int const SurfNum,             // Current surface number
                     int const NSides,              // Number of sides to figure
                     Array1S<Real64> const Vertices // Vertices, in specified order
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the surface vertices from the arrays
        // passed by the calling routine.  These had previously been obtained
        // from the InputProcessor (GetObjectItem).  This routine will provide
        // a standard place for determining various properties of the surface
        // from the vertices.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace Vectors;

        using namespace DataErrorTracking;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetVertices: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ptr;  // Pointer into Vertice array
        int n;    // Loop counter
        int NSrc; // Used for CW -> CCW transformation
        int NTar; // Used for CW -> CCW transformation
        Real64 Perimeter; // Perimeter length of the surface
        Real64 Xb;        // Intermediate calculation
        Real64 Yb;        // Intermediate calculation
        int ZoneNum;
        int ThisCorner;
        // unused    REAL(r64) :: ccwtest
        // unused    LOGICAL   :: SurfaceCCW
        Real64 dotp;

        // Object Data
        Vector3<Real64> const TestVector(0.0, 0.0, 1.0);
        Vector3<Real64> temp;

        auto &sg = state.dataSurfaceGeometry;
        auto &surf = sg->SurfaceTmp(SurfNum);
        
        if (NSides > state.dataSurface->MaxVerticesPerSurface) state.dataSurface->MaxVerticesPerSurface = NSides;
        Ptr = 1;
        for (n = 1; n <= NSides; ++n) {
            surf.Vertex(n).x = Vertices(Ptr);
            ++Ptr;
            surf.Vertex(n).y = Vertices(Ptr);
            ++Ptr;
            surf.Vertex(n).z = Vertices(Ptr);
            ++Ptr;
        }

        // Address changing vertices if they were put in in CW order rather than CCW
        if (!state.dataSurface->CCW) {
            // If even number of sides, this will transfer appropriately
            // If odd number, will leave the "odd" one, which is what you want.
            NSrc = NSides;
            NTar = 2;
            for (n = 1; n <= (NSides - 1) / 2; ++n) {
                temp = surf.Vertex(NSrc);
                surf.Vertex(NSrc) = surf.Vertex(NTar);
                surf.Vertex(NTar) = temp;
                --NSrc;
                ++NTar;
            }
        }
        // Now address which "Corner" has been put in first.  Note: the azimuth and tilt and area
        // calculations do not care which corner is put in first.
        // 2/2011 - don't think the shading calculations have a corner preference.  Will keep this for
        // consistency (for now)
        ThisCorner = state.dataSurface->Corner;
        while (ThisCorner != UpperLeftCorner) {
            if (NSides < 4) {
                if (ThisCorner == UpperRightCorner) {
                    ThisCorner = UpperLeftCorner;
                    break;
                }
            }
            NTar = ThisCorner;
            NSrc = ThisCorner + 1;
            if (NSrc > NSides) NSrc = 1;
            for (n = 1; n <= NSides - 1; ++n) {
                temp = surf.Vertex(NTar);
                surf.Vertex(NTar) = surf.Vertex(NSrc);
                surf.Vertex(NSrc) = temp;
                ++NTar;
                ++NSrc;
                if (NTar > NSides) NTar = 1;
                if (NSrc > NSides) NSrc = 1;
            }
            ++ThisCorner;
            if (ThisCorner > NSides) ThisCorner = 1;
        } // Corners
        if (!state.dataSurface->WorldCoordSystem) {
            // Input in "relative" coordinates, use Building and Zone North Axes and Origins
            //                                  to translate each point (including rotation for Appendix G)
            ZoneNum = surf.Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = surf.Vertex(n).x * sg->CosZoneRelNorth(ZoneNum) - surf.Vertex(n).y * sg->SinZoneRelNorth(ZoneNum) + state.dataHeatBal->Zone(ZoneNum).OriginX;
                    Yb = surf.Vertex(n).x * sg->SinZoneRelNorth(ZoneNum) + surf.Vertex(n).y * sg->CosZoneRelNorth(ZoneNum) + state.dataHeatBal->Zone(ZoneNum).OriginY;
                    surf.Vertex(n).x = Xb * sg->CosBldgRelNorth - Yb * sg->SinBldgRelNorth;
                    surf.Vertex(n).y = Xb * sg->SinBldgRelNorth + Yb * sg->CosBldgRelNorth;
                    surf.Vertex(n).z += state.dataHeatBal->Zone(ZoneNum).OriginZ;
                }
            } else if (surf.Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = surf.Vertex(n).x;
                    Yb = surf.Vertex(n).y;
                    surf.Vertex(n).x = Xb * sg->CosBldgRelNorth - Yb * sg->SinBldgRelNorth;
                    surf.Vertex(n).y = Xb * sg->SinBldgRelNorth + Yb * sg->CosBldgRelNorth;
                }
            }
        } else {
            // if world coordinate only need to rotate for Appendix G
            ZoneNum = surf.Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = surf.Vertex(n).x;
                    Yb = surf.Vertex(n).y;
                    surf.Vertex(n).x = Xb * sg->CosBldgRotAppGonly - Yb * sg->SinBldgRotAppGonly;
                    surf.Vertex(n).y = Xb * sg->SinBldgRotAppGonly + Yb * sg->CosBldgRotAppGonly;
                }
            } else if (surf.Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = surf.Vertex(n).x;
                    Yb = surf.Vertex(n).y;
                    surf.Vertex(n).x = Xb * sg->CosBldgRotAppGonly - Yb * sg->SinBldgRotAppGonly;
                    surf.Vertex(n).y = Xb * sg->SinBldgRotAppGonly + Yb * sg->CosBldgRotAppGonly;
                }
            }
        }

        if (NSides > 2) {
            while (true) {
                PopCoincidentVertexReturn const popResult = checkPopCoincidentVertex(surf.Vertex);
                Perimeter = popResult.perimeter;
                if (popResult.poppedVertexPos < 0) {
                    // No pop needed, we're done
                    break;
                }

                // Grab the popped one, and the kept one (regardless of whether it's previous or next)
                auto it = surf.Vertex.begin();
                std::advance(it, popResult.poppedVertexPos);
                int const poppedVertexIndex = popResult.poppedVertexPos + 1;

                auto itKept = surf.Vertex.begin();
                std::advance(itKept, popResult.keptVertexPos);
                int const keptVertexIndex = popResult.keptVertexPos + 1;

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     format("{}Distance between two vertices < .01, possibly coincident. for Surface={}, in Zone={}",
                                            RoutineName,
                                            surf.Name,
                                            surf.ZoneName));

                    bool const printPoppedFirst = (poppedVertexIndex < keptVertexIndex) ? !(poppedVertexIndex == 1 && keptVertexIndex == surf.Sides)
                                                                                        : (poppedVertexIndex == surf.Sides && keptVertexIndex == 1);

                    if (printPoppedFirst) {
                        ShowContinueError(state, format("Vertex [{}]=({:.2R},{:.2R},{:.2R})", poppedVertexIndex, it->x, it->y, it->z));
                        ShowContinueError(state, format("Vertex [{}]=({:.2R},{:.2R},{:.2R})", keptVertexIndex, itKept->x, itKept->y, itKept->z));
                    } else {
                        ShowContinueError(state, format("Vertex [{}]=({:.2R},{:.2R},{:.2R})", keptVertexIndex, itKept->x, itKept->y, itKept->z));
                        ShowContinueError(state, format("Vertex [{}]=({:.2R},{:.2R},{:.2R})", poppedVertexIndex, it->x, it->y, it->z));
                    }
                }
                ++state.dataErrTracking->TotalCoincidentVertices;
                if (surf.Sides <= 3) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state,
                                          format("Cannot Drop Vertex [{}]; Number of Surface Sides at minimum. This surface is now a "
                                                 "degenerate surface.",
                                                 poppedVertexIndex));
                    }
                    ++state.dataErrTracking->TotalDegenerateSurfaces;
                    // If degenerate, we won't be able to pop now nor later, so exit
                    // mark degenerate surface?
                    break;
                }

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowContinueError(state, format("Dropping Vertex [{}].", poppedVertexIndex));
                }
                --surf.Sides;
                surf.Vertex.erase(it);
                // No need to recompute perimeter, because it'll be done in the next iteration, until no popping or degenerate happens
            }

            surf.Perimeter = Perimeter;

            surf.NewellNormVec = CalcNewellNormalVector(surf.Vertex, surf.Sides);
            surf.NewellAreaVec = CalcNewellAreaVector(surf.Vertex, surf.Sides);
            // For surfaces with subsurfaces, the following two areas are turned into net areas later by
            // subtracting subsurface areas
            surf.GrossArea = length(surf.NewellAreaVec);
            surf.Area = surf.GrossArea;
            surf.NetAreaShadowCalc = surf.Area;
            std::tie(surf.Azimuth, surf.Tilt) = CalcAzimuthAndTilt(surf.Vertex, surf.lcsx, surf.lcsy, surf.lcsz, surf.NewellNormVec);
            dotp = dot(surf.NewellNormVec, TestVector);
            if (surf.Class == SurfaceClass::Roof && dotp < -0.000001) {
                ShowWarningError(state,
                                 format("{}Roof/Ceiling is upside down! Tilt angle=[{:.1R}], should be near 0, Surface=\"{}\", in Zone=\"{}\".",
                                        RoutineName,
                                        surf.Tilt,
                                        surf.Name,
                                        surf.ZoneName));
                ShowContinueError(state, "Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, surf.Sides, surf.Azimuth, surf.Tilt);
            } else if (surf.Class == SurfaceClass::Roof && surf.Tilt > 80.0) {
                ShowWarningError(
                    state,
                    format("{}Roof/Ceiling is not oriented correctly! Tilt angle=[{:.1R}], should be near 0, Surface=\"{}\", in Zone=\"{}\".",
                           RoutineName,
                           surf.Tilt,
                           surf.Name,
                           surf.ZoneName));
            }
            if (surf.Class == SurfaceClass::Floor && dotp > 0.000001) {
                ShowWarningError(state,
                                 format("{}Floor is upside down! Tilt angle=[{:.1R}], should be near 180, Surface=\"{}\", in Zone=\"{}\".",
                                        RoutineName,
                                        surf.Tilt,
                                        surf.Name,
                                        surf.ZoneName));
                ShowContinueError(state, "Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, surf.Sides, surf.Azimuth, surf.Tilt);
            } else if (surf.Class == SurfaceClass::Floor && surf.Tilt < 158.2) { // slope/grade = 40%!
                ShowWarningError(state,
                                 format("{}Floor is not oriented correctly! Tilt angle=[{:.1R}], should be near 180, Surface=\"{}\", in Zone=\"{}\".",
                                        RoutineName,
                                        surf.Tilt,
                                        surf.Name,
                                        surf.ZoneName));
            }
            surf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);

            // Sine and cosine of azimuth and tilt
            surf.SinAzim = std::sin(surf.Azimuth * Constant::DegToRadians);
            surf.CosAzim = std::cos(surf.Azimuth * Constant::DegToRadians);
            surf.SinTilt = std::sin(surf.Tilt * Constant::DegToRadians);
            surf.CosTilt = std::cos(surf.Tilt * Constant::DegToRadians);
            if (surf.ViewFactorGround == Constant::AutoCalculate) {
                surf.ViewFactorGround = 0.5 * (1.0 - surf.CosTilt);
            }
            // Outward normal unit vector (pointing away from room)
            surf.OutNormVec = surf.NewellNormVec;
            auto &outNormVec = surf.OutNormVec;
            if (std::abs(outNormVec.x - 1.0) < 1.e-06) outNormVec.x = +1.0;
            if (std::abs(outNormVec.x + 1.0) < 1.e-06) outNormVec.x = -1.0;
            if (std::abs(outNormVec.x) < 1.e-06) outNormVec.x = 0.0;
            if (std::abs(outNormVec.y - 1.0) < 1.e-06) outNormVec.y = +1.0;
            if (std::abs(outNormVec.y + 1.0) < 1.e-06) outNormVec.y = -1.0;
            if (std::abs(outNormVec.y) < 1.e-06) outNormVec.y = 0.0;
            if (std::abs(outNormVec.z - 1.0) < 1.e-06) outNormVec.z = +1.0;
            if (std::abs(outNormVec.z + 1.0) < 1.e-06) outNormVec.z = -1.0;
            if (std::abs(outNormVec.z) < 1.e-06) outNormVec.z = 0.0;

            if (surf.Class == SurfaceClass::Window || surf.Class == SurfaceClass::GlassDoor || surf.Class == SurfaceClass::Door)
                surf.Area *= surf.Multiplier;
            // Can perform tests on this surface here
            surf.ViewFactorSky = 0.5 * (1.0 + surf.CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            surf.ViewFactorSkyIR = surf.ViewFactorSky;
            surf.ViewFactorGroundIR = 0.5 * (1.0 - surf.CosTilt);

            // Call to transform vertices

            TransformVertsByAspect(state, SurfNum, surf.Sides);

        } else {
            ShowFatalError(state, format("{}Called with less than 2 sides, Surface={}", RoutineName, surf.Name));
        }

        // Preliminary Height/Width
        surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
        surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
    } // GetVertices()

    void ReverseAndRecalculate(EnergyPlusData &state,
                               int const SurfNum,   // Surface number for the surface
                               int const NSides,    // number of sides to surface
                               Real64 &SurfAzimuth, // Surface Facing angle (will be 0 for roofs/floors)
                               Real64 &SurfTilt     // Surface tilt (
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   February 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine reverses the vertices for a surface (needed when roof/floor is upside down)
        // and recalculates the azimuth, etc for the surface.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace Vectors;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("ReverseAndRecalculate: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int n;      // Loop Control
        int RevPtr; // pointer for reversing vertices

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);

        // Reverse vertex order
        for (int n = 1; n <= NSides / 2; ++n) {
            Vector3<Real64> tmp = surf.Vertex(n);
            surf.Vertex(n) = surf.Vertex(NSides + 1 - n);
            surf.Vertex(NSides + 1 - n) = tmp;
        }

        print(state.files.debug, "Reversing Surface Name={}\n", surf.Name);
        for (n = 1; n <= NSides; ++n) {
            print(state.files.debug,
                  "side={:5} abs coord vertex= {:18.13F} {:18.13F} {:18.13F}\n",
                  n,
                  surf.Vertex(n).x,
                  surf.Vertex(n).y,
                  surf.Vertex(n).z);
        }

        surf.NewellNormVec = CalcNewellNormalVector(surf.Vertex, surf.Sides);
        std::tie(surf.Azimuth, surf.Tilt) = CalcAzimuthAndTilt(surf.Vertex, surf.lcsx, surf.lcsy, surf.lcsz, surf.NewellNormVec);
        if (surf.Class == SurfaceClass::Roof && surf.Tilt > 80.0) {
            ShowWarningError(
                state,
                format("{}Roof/Ceiling is still upside down! Tilt angle=[{:.1R}], should be near 0, please fix manually.", RoutineName, surf.Tilt));
        }
        if (surf.Class == SurfaceClass::Floor && surf.Tilt < 158.2) { // 40% grade!
            ShowWarningError(
                state, format("{}Floor is still upside down! Tilt angle=[{:.1R}], should be near 180, please fix manually.", RoutineName, surf.Tilt));
        }
    }

    int MakeMirrorSurface(EnergyPlusData &state, int SurfNum) // In=>Surface to Mirror, Out=>new Surface index
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   June 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine creates a "mirror" surface using the indicated surface.
        // This is the simple approach for bi-directional shading devices.  If, perchance,
        // the user has already taken care of this (e.g. fins in middle of wall), there will
        // be extra shading devices shown.

        // METHODOLOGY EMPLOYED:
        // Reverse the vertices in the original surface.  Add "bi" to name.

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
        auto &surf1 = state.dataSurfaceGeometry->SurfaceTmp(SurfNum+1);

        surf1.Sides = surf.Sides;
        surf1.Vertex.allocate(surf1.Sides);
        // doesn't work when Vertex are pointers  SurfaceTmp(SurfNum+1)=SurfaceTmp(SurfNum)
        surf1.Name = "Mir-" + surf.Name;
        surf1.Construction = surf.Construction;
        surf1.ConstructionStoredInputValue = surf.ConstructionStoredInputValue;
        surf1.Class = surf.Class;
        surf1.GrossArea = surf.GrossArea;
        surf1.Area = surf.Area;
        surf1.Azimuth = surf.Azimuth;
        surf1.Height = surf.Height;
        surf1.Reveal = surf.Reveal;
        surf1.Shape = surf.Shape;
        surf1.Sides = surf.Sides;
        surf1.Tilt = surf.Tilt;
        surf1.convOrientation = Convect::GetSurfConvOrientation(surf1.Tilt);
        surf1.Width = surf.Width;
        surf1.HeatTransSurf = surf.HeatTransSurf;
        surf1.BaseSurfName = surf.BaseSurfName;
        surf1.BaseSurf = surf.BaseSurf;
        surf1.ZoneName = surf.ZoneName;
        surf1.Zone = surf.Zone;
        surf1.ExtBoundCondName = surf.ExtBoundCondName;
        surf1.ExtBoundCond = surf.ExtBoundCond;
        surf1.ExtSolar = surf.ExtSolar;
        surf1.ExtWind = surf.ExtWind;
        surf1.ViewFactorGround = surf.ViewFactorGround;
        surf1.ViewFactorSky = surf.ViewFactorSky;
        surf1.ViewFactorGroundIR = surf.ViewFactorGroundIR;
        surf1.ViewFactorSkyIR = surf.ViewFactorSkyIR;
        surf1.SchedShadowSurfIndex = surf.SchedShadowSurfIndex;
        surf1.SchedMinValue = surf.SchedMinValue;
        surf1.SchedMaxValue = surf.SchedMaxValue;
        surf1.IsTransparent = surf.IsTransparent;
        surf1.activeWindowShadingControl = surf.activeWindowShadingControl;
        surf1.windowShadingControlList = surf.windowShadingControlList;
        surf1.HasShadeControl = surf.HasShadeControl;
        surf1.activeShadedConstruction = surf.activeShadedConstruction;
        surf1.FrameDivider = surf.FrameDivider;
        surf1.Multiplier = surf.Multiplier;
        surf1.NetAreaShadowCalc = surf.NetAreaShadowCalc;
        surf1.Perimeter = surf.Perimeter;

        for (int i = 1; i <= surf.Sides; ++i) {
            surf1.Vertex(surf.Sides + 1 - i) = surf.Vertex(i);
        }

        // TH 3/26/2010
        surf1.MirroredSurf = true;

        if (surf1.Sides > 2) {
            surf1.NewellAreaVec = Vectors::CalcNewellAreaVector(surf1.Vertex, surf1.Sides);
            surf1.GrossArea = length(surf.NewellAreaVec);
            surf1.Area = surf1.GrossArea;
            surf1.NetAreaShadowCalc = surf1.Area;
            surf1.NewellNormVec = Vectors::CalcNewellNormalVector(surf1.Vertex, surf1.Sides);
            std::tie(surf1.Azimuth, surf1.Tilt) = Vectors::CalcAzimuthAndTilt(surf1.Vertex, surf1.lcsx, surf1.lcsy, surf1.lcsz, surf1.NewellNormVec);
            surf1.convOrientation = Convect::GetSurfConvOrientation(surf1.Tilt);

            // Sine and cosine of azimuth and tilt
            surf1.SinAzim = std::sin(surf1.Azimuth * Constant::DegToRadians);
            surf1.CosAzim = std::cos(surf1.Azimuth * Constant::DegToRadians);
            surf1.SinTilt = std::sin(surf1.Tilt * Constant::DegToRadians);
            surf1.CosTilt = std::cos(surf1.Tilt * Constant::DegToRadians);
            // Outward normal unit vector (pointing away from room)
            surf1.OutNormVec = surf1.NewellNormVec;
            auto &outNormVec = surf1.OutNormVec;
            if (std::abs(outNormVec.x - 1.0) < 1.e-06) outNormVec.x = +1.0;
            if (std::abs(outNormVec.x + 1.0) < 1.e-06) outNormVec.x = -1.0;
            if (std::abs(outNormVec.x) < 1.e-06) outNormVec.x = 0.0;
            if (std::abs(outNormVec.y - 1.0) < 1.e-06) outNormVec.y = +1.0;
            if (std::abs(outNormVec.y + 1.0) < 1.e-06) outNormVec.y = -1.0;
            if (std::abs(outNormVec.y) < 1.e-06) outNormVec.y = 0.0;
            if (std::abs(outNormVec.z - 1.0) < 1.e-06) outNormVec.z = +1.0;
            if (std::abs(outNormVec.z + 1.0) < 1.e-06) outNormVec.z = -1.0;
            if (std::abs(outNormVec.z) < 1.e-06) outNormVec.z = 0.0;

            // Can perform tests on this surface here
            surf1.ViewFactorSky = 0.5 * (1.0 + surf1.CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            surf1.ViewFactorSkyIR = surf1.ViewFactorSky;
            surf1.ViewFactorGroundIR = 0.5 * (1.0 - surf1.CosTilt);
        }
        return SurfNum+1;
    } // MakeMirrorSurface()

    void GetWindowShadingControlData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   November 1998
        //       MODIFIED       Aug 2001 (FW): add handling of new ShadingControlIsScheduled
        //                      and GlareControlIsActive fields
        //                      Nov 2001 (FW): add ShadingDevice as alternative to ShadedConstruction
        //                      Dec 2001 (FW): add slat angle controls for blinds
        //                      Aug 2002 (FW): add Setpoint2; check that specified control type is legal
        //                      Feb 2003 (FW): add error if Material Name of Shading Device is used with
        //                        Shading Type = BetweenGlassShade or BetweenGlassBlind
        //                      Dec 2003 (FW): improve BetweenGlassBlind error messages
        //                      Feb 2009 (BG): improve error checking for OnIfScheduleAllows
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads in the window shading control information
        // from the input data file, interprets it and puts it in the derived type

        // Using/Aliasing
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:

        int constexpr NumValidShadingTypes(9);
        static Array1D_string const cValidShadingTypes(NumValidShadingTypes,
                                                       {
                                                           "SHADEOFF",          // 1
                                                           "INTERIORSHADE",     // 2
                                                           "SWITCHABLEGLAZING", // 3
                                                           "EXTERIORSHADE",     // 4
                                                           "EXTERIORSCREEN",    // 5
                                                           "INTERIORBLIND",     // 6
                                                           "EXTERIORBLIND",     // 7
                                                           "BETWEENGLASSSHADE", // 8
                                                           "BETWEENGLASSBLIND"  // 9
                                                       });

        constexpr std::array<std::string_view, (int)WindowShadingControlType::Num> WindowShadingControlTypeNamesUC{
            "ALWAYSON",
            "ALWAYSOFF",
            "ONIFSCHEDULEALLOWS",
            "ONIFHIGHSOLARONWINDOW",
            "ONIFHIGHHORIZONTALSOLAR",
            "ONIFHIGHOUTDOORAIRTEMPERATURE",
            "ONIFHIGHZONEAIRTEMPERATURE",
            "ONIFHIGHZONECOOLING",
            "ONIFHIGHGLARE",
            "MEETDAYLIGHTILLUMINANCESETPOINT",
            "ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY",
            "ONNIGHTIFLOWINSIDETEMPANDOFFDAY",
            "ONNIGHTIFHEATINGANDOFFDAY",
            "ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING",
            "ONNIGHTIFHEATINGANDONDAYIFCOOLING",
            "OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW",
            "ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW",
            "ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW",
            "ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR",
            "ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW",
            "ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR",
            "ONIFHIGHSOLARORHIGHLUMINANCETILLMIDNIGHT",
            "ONIFHIGHSOLARORHIGHLUMINANCETILLSUNSET",
            "ONIFHIGHSOLARORHIGHLUMINANCETILLNEXTMORNING"};

        constexpr std::array<std::string_view, (int)SlatAngleControl::Num> SlatAngleNamesUC{
            "FIXEDSLATANGLE", "SCHEDULEDSLATANGLE", "BLOCKBEAMSOLAR"};

        constexpr std::array<std::string_view, (int)MultiSurfaceControl::Num> MultiSurfaceControlNamesUC = {"SEQUENTIAL", "GROUP"};

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;          // IO Status when calling get input subroutine
        int ControlNumAlpha; // Number of control alpha names being passed
        int ControlNumProp;  // Number of control properties being passed
        int ControlNum;      // DO loop counter/index for window shading control number
        int IShadedConst;    // Construction number of shaded construction
        int IShadingDevice;  // Material number of shading device
        int NLayers;         // Layers in shaded construction
        bool ErrorInName;
        bool IsBlank;
        int Loop;
        bool BGShadeBlindError; // True if problem with construction that is supposed to have between-glass
        // shade or blind
        int Found;

        constexpr std::string_view routineName = "GetWindowShadingControlData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        // Get the total number of window shading control blocks
        ipsc->cCurrentModuleObject = "WindowShadingControl";
        state.dataSurface->TotWinShadingControl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        if (state.dataSurface->TotWinShadingControl == 0) return;

        state.dataSurface->WindowShadingControl.allocate(state.dataSurface->TotWinShadingControl);

        ControlNum = 0;
        for (int Loop = 1; Loop <= state.dataSurface->TotWinShadingControl; ++Loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     ControlNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     ControlNumProp,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            if (ipsc->lAlphaFieldBlanks(1)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                ErrorsFound = true;
                continue;
            }

            if (Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->WindowShadingControl) > 0) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }
            
            ++ControlNum;

            auto &shadeCtrl = state.dataSurface->WindowShadingControl(ControlNum);

            shadeCtrl.Name = ipsc->cAlphaArgs(1); // Set the Control Name in the Derived Type

            shadeCtrl.ZoneIndex = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataHeatBal->Zone);
            if (shadeCtrl.ZoneIndex == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
            }

            shadeCtrl.SequenceNumber = int(ipsc->rNumericArgs(1));
            // WindowShadingControl().getInputShadedConstruction is only used during GetInput process and is ultimately stored in
            // Surface().shadedConstructionList
            shadeCtrl.getInputShadedConstruction = Util::FindItemInList(ipsc->cAlphaArgs(4), state.dataConstruction->Construct);
            shadeCtrl.ShadingDevice = Util::FindItemInPtrList(ipsc->cAlphaArgs(9), state.dataMaterial->Material);
            // Are these not checked for existence?
            
            shadeCtrl.Schedule = GetScheduleIndex(state, ipsc->cAlphaArgs(6));
            shadeCtrl.SetPoint = ipsc->rNumericArgs(2);
            shadeCtrl.SetPoint2 = ipsc->rNumericArgs(3);
            shadeCtrl.ShadingControlIsScheduled = getYesNoValue(ipsc->cAlphaArgs(7)) == BooleanSwitch::Yes;
            shadeCtrl.GlareControlIsActive = getYesNoValue(ipsc->cAlphaArgs(8)) == BooleanSwitch::Yes;
            shadeCtrl.SlatAngleSchedule = GetScheduleIndex(state, ipsc->cAlphaArgs(11));
            // Not checked for validity?
            
            // store the string for now and associate it after daylighting control objects are read
            shadeCtrl.DaylightingControlName = ipsc->cAlphaArgs(12);

            shadeCtrl.multiSurfaceControl =
                static_cast<MultiSurfaceControl>(getEnumValue(MultiSurfaceControlNamesUC, Util::makeUPPER(ipsc->cAlphaArgs(13))));

            if (shadeCtrl.multiSurfaceControl == MultiSurfaceControl::Invalid) {
                shadeCtrl.multiSurfaceControl = MultiSurfaceControl::Sequential;
                ShowWarningInvalidKey(state, eoh, ipsc->cAlphaFieldNames(13), ipsc->cAlphaArgs(13),
                                      MultiSurfaceControlNamesUC[(int)MultiSurfaceControl::Sequential]);
            }

            if (ControlNumAlpha >= 14) {
                shadeCtrl.FenestrationCount = ControlNumAlpha - 13;
                shadeCtrl.FenestrationName.allocate(shadeCtrl.FenestrationCount);
                shadeCtrl.FenestrationIndex.allocate(shadeCtrl.FenestrationCount);
                for (int i = 1; i <= shadeCtrl.FenestrationCount; i++) {
                    shadeCtrl.FenestrationName(i) = ipsc->cAlphaArgs(i + 13);
                }
            } else {
                ShowSevereError(state,
                                format("{}=\"{}\" invalid. Must reference at least one Fenestration Surface object name.",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1)));
            }

            shadeCtrl.shadingControlType = static_cast<WindowShadingControlType>(
                getEnumValue(WindowShadingControlTypeNamesUC, Util::makeUPPER(ipsc->cAlphaArgs(5))));

            if (shadeCtrl.ShadingDevice > 0) {
                if (state.dataMaterial->Material(shadeCtrl.ShadingDevice)->group == Material::Group::Screen &&
                    !(shadeCtrl.shadingControlType == WindowShadingControlType::AlwaysOn ||
                      shadeCtrl.shadingControlType == WindowShadingControlType::AlwaysOff ||
                      shadeCtrl.shadingControlType == WindowShadingControlType::OnIfScheduled)) {
                    ErrorsFound = true;
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(5), ipsc->cAlphaArgs(5));
                    ShowContinueError(state,
                                      "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                }
            } else {
                if (shadeCtrl.getInputShadedConstruction > 0) {
                    state.dataConstruction->Construct(shadeCtrl.getInputShadedConstruction).IsUsed = true;
                    if (state.dataMaterial->Material(state.dataConstruction->Construct(shadeCtrl.getInputShadedConstruction).LayerPoint(1))
                                ->group == Material::Group::Screen &&
                        !(shadeCtrl.shadingControlType == WindowShadingControlType::AlwaysOn ||
                          shadeCtrl.shadingControlType == WindowShadingControlType::AlwaysOff ||
                          shadeCtrl.shadingControlType == WindowShadingControlType::OnIfScheduled)) {
                        ErrorsFound = true;
                        ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(5), ipsc->cAlphaArgs(5));
                        ShowContinueError(state,
                                          "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                    }
                } else if (ipsc->lAlphaFieldBlanks(4)) {
                    ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(4));
                    ShowContinueError(state, "A valid construction is required.");
                    ErrorsFound = true;
                } else {
                    ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                    ErrorsFound = true;
                }
            }

            // Warning if setpoint is unintentionally zero
            if (shadeCtrl.SetPoint == 0 && shadeCtrl.shadingControlType != WindowShadingControlType::AlwaysOn &&
                shadeCtrl.shadingControlType != WindowShadingControlType::AlwaysOff &&
                shadeCtrl.shadingControlType != WindowShadingControlType::OnIfScheduled &&
                shadeCtrl.shadingControlType != WindowShadingControlType::HiGlare) {
                ShowWarningError(state, format("{}=\"{}\", The first SetPoint is zero.", ipsc->cCurrentModuleObject, shadeCtrl.Name));
                ShowContinueError(state, "..You may have forgotten to specify that setpoint.");
            }

            // Error checks
            if (ipsc->cAlphaArgs(7) != "YES" && ipsc->cAlphaArgs(7) != "NO") { // Shading Control is Schedule field
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\".",
                                       ipsc->cCurrentModuleObject,
                                       shadeCtrl.Name,
                                       ipsc->cAlphaFieldNames(7),
                                       ipsc->cAlphaArgs(7)));
            }
            if (ipsc->cAlphaArgs(8) != "YES" && ipsc->cAlphaArgs(8) != "NO") { // Glare Control is Active field
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\".",
                                       ipsc->cCurrentModuleObject,
                                       shadeCtrl.Name,
                                       ipsc->cAlphaFieldNames(8),
                                       ipsc->cAlphaArgs(8)));
            }

            if ((shadeCtrl.shadingControlType == WindowShadingControlType::OnIfScheduled) &&
                (!shadeCtrl.ShadingControlIsScheduled)) { // CR 7709 BG
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{} = \"{}\" invalid, {} must be set to \"Yes\" for {} = OnIfScheduleAllows",
                                       ipsc->cCurrentModuleObject,
                                       shadeCtrl.Name,
                                       ipsc->cAlphaFieldNames(7),
                                       ipsc->cAlphaFieldNames(5)));
            }
            shadeCtrl.slatAngleControl =
                static_cast<SlatAngleControl>(getEnumValue(SlatAngleNamesUC, Util::makeUPPER(ipsc->cAlphaArgs(10))));

            // For upward compatibility change old "noninsulating" and "insulating" shade types to
            // INTERIORSHADE or EXTERIORSHADE
            if (ipsc->cAlphaArgs(3) == "INTERIORNONINSULATINGSHADE" ||
                ipsc->cAlphaArgs(3) == "INTERIORINSULATINGSHADE") {
                ShowWarningError(state,
                                 format("{}=\"{}\" is using obsolete {}=\"{}\", changing to \"InteriorShade\"",
                                        ipsc->cCurrentModuleObject,
                                        shadeCtrl.Name,
                                        ipsc->cAlphaFieldNames(3),
                                        ipsc->cAlphaArgs(3)));
                shadeCtrl.ShadingType = WinShadingType::IntShade;
                ipsc->cAlphaArgs(3) = "INTERIORSHADE";
            }
            if (ipsc->cAlphaArgs(3) == "EXTERIORNONINSULATINGSHADE" ||
                ipsc->cAlphaArgs(3) == "EXTERIORINSULATINGSHADE") {
                ShowWarningError(state,
                                 format("{}=\"{}\" is using obsolete {}=\"{}\", changing to \"ExteriorShade\"",
                                        ipsc->cCurrentModuleObject,
                                        shadeCtrl.Name,
                                        ipsc->cAlphaFieldNames(3),
                                        ipsc->cAlphaArgs(3)));
                shadeCtrl.ShadingType = WinShadingType::ExtShade;
                ipsc->cAlphaArgs(3) = "EXTERIORSHADE";
            }

            if (shadeCtrl.shadingControlType == WindowShadingControlType::MeetDaylIlumSetp &&
                ipsc->cAlphaArgs(3) != "SWITCHABLEGLAZING") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\".",
                                       ipsc->cCurrentModuleObject,
                                       shadeCtrl.Name,
                                       ipsc->cAlphaFieldNames(3),
                                       ipsc->cAlphaArgs(3)));
                ShowContinueError(state,
                                  format("...{} must be SwitchableGlazing for this control, but entered type=\"{}\".",
                                         ipsc->cAlphaFieldNames(3),
                                         ipsc->cAlphaArgs(3)));
            }

            // Check for illegal shading type name
            Found = Util::FindItemInList(ipsc->cAlphaArgs(3), cValidShadingTypes, NumValidShadingTypes);
            if (Found <= 1) {
                ErrorsFound = true;
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
            } else {
                shadeCtrl.ShadingType = WinShadingType(Found);
            }

            WinShadingType ShTyp = shadeCtrl.ShadingType;
            IShadedConst = shadeCtrl.getInputShadedConstruction;
            IShadingDevice = shadeCtrl.ShadingDevice;

            if (IShadedConst == 0 && IShadingDevice == 0) {
                ShowSevereError(
                    state,
                    format("{}=\"{}\" has no matching shaded construction or shading device.", ipsc->cCurrentModuleObject, shadeCtrl.Name));
                ErrorsFound = true;
            } else if (IShadedConst == 0 && IShadingDevice > 0) {
                if (ShTyp == WinShadingType::SwitchableGlazing) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" has {}= SwitchableGlazing but no matching shaded construction",
                                           ipsc->cCurrentModuleObject,
                                           shadeCtrl.Name,
                                           ipsc->cAlphaArgs(3)));
                    ErrorsFound = true;
                }
                if ((ShTyp == WinShadingType::IntShade || ShTyp == WinShadingType::ExtShade) &&
                    state.dataMaterial->Material(IShadingDevice)->group != Material::Group::Shade) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" has {}= InteriorShade or ExteriorShade but matching shading device is not a window shade",
                                           ipsc->cCurrentModuleObject,
                                           shadeCtrl.Name,
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(
                        state,
                        format("{} in error=\"{}\".", ipsc->cAlphaFieldNames(8), state.dataMaterial->Material(IShadingDevice)->Name));
                    ErrorsFound = true;
                }
                if ((ShTyp == WinShadingType::ExtScreen) && state.dataMaterial->Material(IShadingDevice)->group != Material::Group::Screen) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" has {}= ExteriorScreen but matching shading device is not a window screen",
                                           ipsc->cCurrentModuleObject,
                                           shadeCtrl.Name,
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(
                        state,
                        format("{} in error=\"{}\".", ipsc->cAlphaFieldNames(8), state.dataMaterial->Material(IShadingDevice)->Name));
                    ErrorsFound = true;
                }
                if ((ShTyp == WinShadingType::IntBlind || ShTyp == WinShadingType::ExtBlind) &&
                    state.dataMaterial->Material(IShadingDevice)->group != Material::Group::WindowBlind) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" has {}= InteriorBlind or ExteriorBlind but matching shading device is not a window blind",
                                           ipsc->cCurrentModuleObject,
                                           shadeCtrl.Name,
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(
                        state,
                        format("{} in error=\"{}\".", ipsc->cAlphaFieldNames(8), state.dataMaterial->Material(IShadingDevice)->Name));
                    ErrorsFound = true;
                }
                if (ShTyp == WinShadingType::BGShade || ShTyp == WinShadingType::BGBlind) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" has {}= BetweenGlassShade or BetweenGlassBlind and",
                                           ipsc->cCurrentModuleObject,
                                           shadeCtrl.Name,
                                           ipsc->cAlphaArgs(3)));
                    ShowContinueError(
                        state,
                        format("{} is specified. This is illegal. Specify shaded construction instead.", ipsc->cAlphaFieldNames(8)));
                    ErrorsFound = true;
                }
            } else if (IShadedConst > 0 && IShadingDevice > 0) {
                IShadingDevice = 0;
                ShowWarningError(state,
                                 format("{}=\"{}\" Both {} and {} are specified.",
                                        ipsc->cCurrentModuleObject,
                                        shadeCtrl.Name,
                                        ipsc->cAlphaFieldNames(4),
                                        ipsc->cAlphaFieldNames(9)));
                ShowContinueError(state,
                                  format("The {}=\"{}\" will be used.",
                                         ipsc->cAlphaFieldNames(4),
                                         state.dataConstruction->Construct(IShadedConst).Name));
            }

            // If type = interior or exterior shade or blind require that the shaded construction
            // have a shade layer in the correct position
            if (IShadedConst != 0) {

                NLayers = state.dataConstruction->Construct(IShadedConst).TotLayers;
                BGShadeBlindError = false;
                IShadingDevice = 0;
                if (state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers) != 0) {
                    if (shadeCtrl.ShadingType == WinShadingType::IntShade) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers))->group !=
                            Material::Group::Shade) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have a shade layer on the inside of the window.",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::ExtShade) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1))->group !=
                            Material::Group::Shade) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(43),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have a shade layer on the outside of the window.",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::ExtScreen) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1))->group !=
                            Material::Group::Screen) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have a screen layer on the outside of the window.",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::IntBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers))->group !=
                            Material::Group::WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have a blind layer on the inside of the window.",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::ExtBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1))->group !=
                            Material::Group::WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have a blind layer on the outside of the window.",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::BGShade) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3))->group !=
                                Material::Group::Shade)
                                BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5))->group !=
                                Material::Group::Shade)
                                BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have two or three glass layers and a",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(32)));
                            ShowContinueError(state, "between-glass shade layer with a gas layer on each side.");
                        }
                    } else if (shadeCtrl.ShadingType == WinShadingType::BGBlind) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3))->group !=
                                Material::Group::WindowBlind)
                                BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5))->group !=
                                Material::Group::WindowBlind)
                                BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(state,
                                            format("{}=\"{}\" the {}=\"{}\"",
                                                   ipsc->cCurrentModuleObject,
                                                   shadeCtrl.Name,
                                                   ipsc->cAlphaFieldNames(4),
                                                   ipsc->cAlphaArgs(4)));
                            ShowContinueError(state,
                                              format("of {}=\"{}\" should have two or three glass layers and a",
                                                     ipsc->cAlphaFieldNames(3),
                                                     ipsc->cAlphaArgs(3)));
                            ShowContinueError(state, "between-glass blind layer with a gas layer on each side.");
                        }
                    }
                }
                if (IShadingDevice > 0) {
                    if ((ShTyp == WinShadingType::IntShade || ShTyp == WinShadingType::ExtShade) &&
                        state.dataMaterial->Material(IShadingDevice)->group != Material::Group::Shade) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" has {}= InteriorShade or ExteriorShade but matching shading device is not a window shade",
                                               ipsc->cCurrentModuleObject,
                                               shadeCtrl.Name,
                                               ipsc->cAlphaFieldNames(3)));
                        ShowContinueError(state, format("Shading Device in error=\"{}\".", state.dataMaterial->Material(IShadingDevice)->Name));
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WinShadingType::ExtScreen) && state.dataMaterial->Material(IShadingDevice)->group != Material::Group::Screen) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" has {}= ExteriorScreen but matching shading device is not an exterior window screen.",
                                               ipsc->cCurrentModuleObject,
                                               shadeCtrl.Name,
                                               ipsc->cAlphaFieldNames(3)));
                        ShowContinueError(state, format("Shading Device in error=\"{}\".", state.dataMaterial->Material(IShadingDevice)->Name));
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WinShadingType::IntBlind || ShTyp == WinShadingType::ExtBlind) &&
                        state.dataMaterial->Material(IShadingDevice)->group != Material::Group::WindowBlind) {
                        ShowSevereError(state,
                                        format("{}=\"{}\" has {}= InteriorBlind or ExteriorBlind but matching shading device is not a window blind.",
                                               ipsc->cCurrentModuleObject,
                                               shadeCtrl.Name,
                                               ipsc->cAlphaFieldNames(3)));
                        ShowContinueError(state, format("Shading Device in error=\"{}\".", state.dataMaterial->Material(IShadingDevice)->Name));
                        ErrorsFound = true;
                    }
                }
            }
        } // End of loop over window shading controls
    } // GetWindowShadingControlData()

    void InitialAssociateWindowShadingControlFenestration(EnergyPlusData &state, bool &ErrorsFound, int &SurfNum)
    {
        // J.Glazer 2018 - operates on SurfaceTmp array before final indices are known for windows and sets the activeWindowShadingControl
        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
        for (int iShadeCtrl = 1; iShadeCtrl <= state.dataSurface->TotWinShadingControl; ++iShadeCtrl) {
            auto const &shadeCtrl = state.dataSurface->WindowShadingControl(iShadeCtrl);
            int curShadedConstruction = shadeCtrl.getInputShadedConstruction;
            for (int jFeneRef = 1; jFeneRef <= shadeCtrl.FenestrationCount; ++jFeneRef) {
                if (Util::SameString(shadeCtrl.FenestrationName(jFeneRef), surf.Name)) {
                    state.dataGlobal->AndShadingControlInModel = true;
                    surf.HasShadeControl = true;
                    surf.windowShadingControlList.push_back(iShadeCtrl);
                    surf.activeWindowShadingControl = iShadeCtrl;
                    surf.shadedConstructionList.push_back(curShadedConstruction);
                    surf.activeShadedConstruction = curShadedConstruction;

                    // check to make the window refenced is an exterior window
                    if (surf.ExtBoundCond != ExternalEnvironment) {
                        ErrorsFound = true;
                        ShowSevereError(
                            state,
                            format("InitialAssociateWindowShadingControlFenestration: \"{}\", invalid  because it is not an exterior window.",
                                   surf.Name));
                        ShowContinueError(
                            state,
                            format(".. It appears on WindowShadingControl object: \"{}", state.dataSurface->WindowShadingControl(iShadeCtrl).Name));
                    }
                    // check to make sure the window is not using equivalent layer window construction
                    if (state.dataConstruction->Construct(surf.Construction).WindowTypeEQL) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("InitialAssociateWindowShadingControlFenestration: =\"{}\", invalid \".",
                                               surf.Name));
                        ShowContinueError(state, ".. equivalent layer window model does not use shading control object.");
                        ShowContinueError(state, ".. Shading control is set to none or zero, and simulation continues.");
                        ShowContinueError(
                            state,
                            format(".. It appears on WindowShadingControl object: \"{}", state.dataSurface->WindowShadingControl(iShadeCtrl).Name));
                        surf.activeWindowShadingControl = 0;
                    }
                }
            }
        } // for (iShadeCtrl)
    } // InitialAssociateWindowShadingControlFenestration()

    void FinalAssociateWindowShadingControlFenestration(EnergyPlusData &state, bool &ErrorsFound)
    {
        // J.Glazer 2018 - operates on Surface array after final indices are known for windows and checks to make sure it is correct
        for (int iShadeCtrl = 1; iShadeCtrl <= state.dataSurface->TotWinShadingControl; ++iShadeCtrl) {
            auto &shadeCtrl = state.dataSurface->WindowShadingControl(iShadeCtrl);
            for (int jFeneRef = 1; jFeneRef <= shadeCtrl.FenestrationCount; ++jFeneRef) {
                int fenestrationIndex = Util::FindItemInList(shadeCtrl.FenestrationName(jFeneRef), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
                auto const &surfFen = state.dataSurface->Surface(fenestrationIndex);
                if (std::find(surfFen.windowShadingControlList.begin(), surfFen.windowShadingControlList.end(), iShadeCtrl) != surfFen.windowShadingControlList.end()) {
                    shadeCtrl.FenestrationIndex(jFeneRef) = fenestrationIndex;
                } else {
                    // this error condition should not occur since the rearrangement of Surface() from SurfureTmp() is reliable.
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("FinalAssociateWindowShadingControlFenestration: Fenestration surface named \"{}\" has "
                                           "WindowShadingContol index that does not match the initial index assigned.",
                                           surfFen.Name));
                    ShowContinueError(state,
                                      format("This occurs while WindowShadingControl object: \"{}\" is being evaluated. ",
                                             shadeCtrl.Name));
                }
            }
        }
    } // FinalAssociateWindowShadingControlFenestration()

    void CheckWindowShadingControlSimilarForWindow(EnergyPlusData &state, bool &ErrorsFound)
    {
        // For each window check if all window shading controls on list are the same except for name, schedule name, construction, and
        // material
        for (auto &surf : state.dataSurface->Surface) {
            if (!surf.HasShadeControl) continue;
            if (surf.windowShadingControlList.size() == 1) continue;
            
            int firstWindowShadingControl = surf.windowShadingControlList.front();
            for (auto wsc = std::next(surf.windowShadingControlList.begin()); wsc != surf.windowShadingControlList.end(); ++wsc) {
                if (!isWindowShadingControlSimilar(state, firstWindowShadingControl, *wsc)) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("CheckWindowShadingControlSimilarForWindow: Fenestration surface named \"{}\" has multiple "
                                           "WindowShadingContols that are not similar.",
                                           surf.Name));
                    ShowContinueError(state,
                                      format("for: \"{} and: {}",
                                             state.dataSurface->WindowShadingControl(firstWindowShadingControl).Name,
                                             state.dataSurface->WindowShadingControl(*wsc).Name));
                }
            }
        }
    } // CheckWindowShadingControlSimilarForWindow()

    bool isWindowShadingControlSimilar(EnergyPlusData &state, int a, int b)
    {
        // Compares two window shading controls are the same except for the name, schedule name, construction, and material
        auto &ctrlA = state.dataSurface->WindowShadingControl(a);
        auto &ctrlB = state.dataSurface->WindowShadingControl(b);
        return (ctrlA.ZoneIndex == ctrlB.ZoneIndex &&
                ctrlA.ShadingType == ctrlB.ShadingType &&
                ctrlA.shadingControlType == ctrlB.shadingControlType &&
                ctrlA.SetPoint == ctrlB.SetPoint &&
                ctrlA.ShadingControlIsScheduled == ctrlB.ShadingControlIsScheduled &&
                ctrlA.GlareControlIsActive == ctrlB.GlareControlIsActive &&
                ctrlA.slatAngleControl == ctrlB.slatAngleControl &&
                ctrlA.SetPoint2 == ctrlB.SetPoint2 &&
                ctrlA.DaylightingControlName == ctrlB.DaylightingControlName &&
                ctrlA.DaylightControlIndex == ctrlB.DaylightControlIndex &&
                ctrlA.multiSurfaceControl == ctrlB.multiSurfaceControl);
    }

    void GetStormWindowData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   December 2003
        //       MODIFIED       na

        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads in the storm window data from the input file,
        // interprets it and puts it in the derived type

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;           // IO Status when calling get input subroutine
        int StormWinNumAlpha; // Number of alpha names being passed
        int StormWinNumProp;  // Number of properties being passed
        int StormWinNum;      // Index for storm window number
        int loop;             // Do loop counter
        int SurfNum;          // Surface number
        int MatNum;           // Material number

        constexpr std::string_view routineName = "GetStormWindowData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        // Get the total number of storm window input objects
        ipsc->cCurrentModuleObject = "WindowProperty:StormWindow";
        state.dataSurface->TotStormWin = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        if (state.dataSurface->TotStormWin == 0) return;

        state.dataSurface->StormWindow.allocate(state.dataSurface->TotStormWin);

        StormWinNum = 0;
        for (int loop = 1; loop <= state.dataSurface->TotStormWin; ++loop) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     loop,
                                                                     ipsc->cAlphaArgs,
                                                                     StormWinNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     StormWinNumProp,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            ++StormWinNum;

            auto &stormWin = state.dataSurface->StormWindow(StormWinNum);
            
            stormWin.BaseWindowNum =
                Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
            stormWin.StormWinMaterialNum =
                Util::FindItemInPtrList(ipsc->cAlphaArgs(2), state.dataMaterial->Material, state.dataMaterial->TotMaterials);
            stormWin.StormWinDistance = ipsc->rNumericArgs(1);
            stormWin.MonthOn = ipsc->rNumericArgs(2);
            stormWin.DayOfMonthOn = ipsc->rNumericArgs(3);
            stormWin.DateOn =
                General::OrdinalDay(stormWin.MonthOn, stormWin.DayOfMonthOn, 1);
            stormWin.MonthOff = ipsc->rNumericArgs(4);
            stormWin.DayOfMonthOff = ipsc->rNumericArgs(5);
            stormWin.DateOff = General::OrdinalDay(
                stormWin.MonthOff, stormWin.DayOfMonthOff, 1);

            if (stormWin.DateOn == stormWin.DateOff) {
                ShowSevereError(state,
                                format("{}: Date On = Date Off -- not allowed, occurred in WindowProperty:StormWindow Input #{}",
                                       ipsc->cCurrentModuleObject,
                                       StormWinNum));
                ErrorsFound = true;
            }

            enum Month
            {
                January = 1,
                February,
                March,
                April,
                May,
                June,
                July,
                August,
                September,
                October,
                November,
                December
            };
            constexpr std::array<int, 13> oneBasedDaysInMonth = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

            int const monthOn = stormWin.MonthOn;
            if (monthOn >= January && monthOn <= December) {
                if (stormWin.DayOfMonthOn >
                    oneBasedDaysInMonth[stormWin.MonthOn]) {
                    ShowSevereError(state,
                                    format("{}: Date On (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                           ipsc->cCurrentModuleObject,
                                           stormWin.DayOfMonthOn,
                                           StormWinNum));
                    ErrorsFound = true;
                }
                break;
            } else {
                ShowSevereError(state,
                                format("{}: Date On Month [{}], invalid for WindowProperty:StormWindow Input #{}",
                                       ipsc->cCurrentModuleObject,
                                       stormWin.MonthOn,
                                       StormWinNum));
                ErrorsFound = true;
            }

            int const monthOff = stormWin.MonthOff;
            if (monthOff >= January && monthOff <= December) {
                if (stormWin.DayOfMonthOff >
                    oneBasedDaysInMonth[stormWin.MonthOff]) {
                    ShowSevereError(state,
                                    format("{}: Date Off (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                           ipsc->cCurrentModuleObject,
                                           stormWin.DayOfMonthOff,
                                           StormWinNum));
                    ErrorsFound = true;
                }
                break;
            } else {
                ShowSevereError(state,
                                format("{}: Date Off Month [{}], invalid for WindowProperty:StormWindow Input #{}",
                                       ipsc->cCurrentModuleObject,
                                       stormWin.MonthOff,
                                       StormWinNum));
                ErrorsFound = true;
            }
        }

        // Error checks

        for (StormWinNum = 1; StormWinNum <= state.dataSurface->TotStormWin; ++StormWinNum) {
            auto &stormWin = state.dataSurface->StormWindow(StormWinNum);
            // Require BaseWindowNum be that of an exterior window
            SurfNum = stormWin.BaseWindowNum;
            if (SurfNum == 0) {
                ShowSevereError(state, format("{}=\"{}\" invalid.", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            } else {
                auto const &surf = state.dataSurface->Surface(SurfNum);
                if (surf.Class != SurfaceClass::Window || surf.ExtBoundCond != 0) {
                    ShowSevereError(state, format("{}=\"{}\"", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowSevereError(state, format("cannot be used with surface={}", surf.Name));
                    ShowContinueError(state, "because that surface is not an exterior window.");
                    ErrorsFound = true;
                }
            }

            // Require that storm window material be glass
            MatNum = stormWin.StormWinMaterialNum;
            if (SurfNum > 0) {
                if (MatNum == 0) {
                    ShowSevereError(state, format("{}=\"{}\"", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state,
                                      format("{}=\"{}\" not found as storm window layer.",
                                             ipsc->cAlphaFieldNames(2),
                                             ipsc->cAlphaArgs(2)));
                    ErrorsFound = true;
                } else {
                    if (state.dataMaterial->Material(MatNum)->group != Material::Group::WindowGlass) {
                        ShowSevereError(state, format("{}=\"{}\"", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                        ShowContinueError(state,
                                          format("{}=\"{}must be a WindowMaterial:Glazing or WindowMaterial:Glazing:RefractionExtinctionMethod",
                                                 ipsc->cAlphaFieldNames(2),
                                                 ipsc->cAlphaArgs(2)));
                        ErrorsFound = true;
                    }
                }
            }

            // Error if base window has airflow control
            if (SurfNum > 0) {
                if (state.dataSurface->SurfWinAirflowControlType(SurfNum) != DataSurfaces::WindowAirFlowControlType::Invalid) {
                    ShowSevereError(
                        state,
                        format("{}=\"{} cannot be used because it is an airflow window (i.e., has WindowProperty:AirflowControl specified)",
                               ipsc->cCurrentModuleObject,
                               ipsc->cAlphaArgs(1)));
                    ErrorsFound = true;
                }
            }

            // Check for reversal of on and off times
            if (SurfNum > 0) {
                if ((state.dataEnvrn->Latitude > 0.0 &&
                     (stormWin.MonthOn < stormWin.MonthOff)) ||
                    (state.dataEnvrn->Latitude <= 0.0 &&
                     (stormWin.MonthOn > stormWin.MonthOff))) {
                    ShowWarningError(state,
                                     format("{}=\"{}\" check times that storm window", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                    ShowContinueError(state,
                                      format("is put on (month={}, day={}) and taken off (month={}, day={});",
                                             stormWin.MonthOn,
                                             stormWin.DayOfMonthOn,
                                             stormWin.MonthOff,
                                             stormWin.DayOfMonthOff));
                    ShowContinueError(state, format("these times may be reversed for your building latitude={:.2R} deg.", state.dataEnvrn->Latitude));
                }
            }
        }
    } // GetStormWindowData()

    void GetWindowGapAirflowControlData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Feb 2003
        //       MODIFIED       June 2003, FCW: add destination = return air;
        //                        more error messages
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Reads in the window airflow control information from the input data file,
        // interprets it and puts it in the SurfaceWindow derived type

        // Using/Aliasing
        using ScheduleManager::GetScheduleIndex;

        int IOStat;               // IO Status when calling get input subroutine
        int ControlNumAlpha;      // Number of control alpha names being passed
        int ControlNumProp;       // Number of control properties being passed
        int TotWinAirflowControl; // Total window airflow control statements
        bool WrongSurfaceType;    // True if associated surface is not 2- or 3-pane exterior window
        int Loop;
        int SurfNum;      // Surface number
        int ConstrNum(0); // Construction number
        int ConstrNumSh;  // Shaded Construction number
        int MatGapFlow;   // Material number of gas in airflow gap of window's construction
        int MatGapFlow1;  // Material number of gas on either side of a between-glass shade/blind
        int MatGapFlow2;

        constexpr std::array<std::string_view, (int)WindowAirFlowSource::Num> WindowAirFlowSourceNamesUC = {"INDOORAIR", "OUTDOORAIR"};
        constexpr std::array<std::string_view, (int)WindowAirFlowDestination::Num> WindowAirFlowDestinationNamesUC = {
            "INDOORAIR", "OUTDOORAIR", "RETURNAIR"};

        constexpr std::string_view routineName = "GetWindowGapAirflowControlData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        // of the shaded construction of airflow window
        // Get the total number of window airflow control statements
        ipsc->cCurrentModuleObject = "WindowProperty:AirflowControl";
        TotWinAirflowControl = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        if (TotWinAirflowControl == 0) return;

        for (int Loop = 1; Loop <= TotWinAirflowControl; ++Loop) { // Loop through all surfaces in the input...
            ipsc->cCurrentModuleObject = "WindowProperty:AirflowControl";
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     ControlNumAlpha,
                                                                     ipsc->rNumericArgs,
                                                                     ControlNumProp,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            SurfNum = Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->Surface, state.dataSurface->TotSurfaces);
            if (SurfNum == 0) {
                ShowSevereError(state, format("{}=\"{}\" not found.", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ErrorsFound = true;
            }

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
            
            // Check that associated surface is a 2- or 3-pane exterior window
            WrongSurfaceType = false;
            if (SurfNum != 0) {
                auto const &surf = state.dataSurface->Surface(SurfNum);
                if (surf.Class != SurfaceClass::Window) WrongSurfaceType = true;
                if (surf.Class == SurfaceClass::Window) {
                    ConstrNum = surf.Construction;
                    if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers != 2 &&
                        state.dataConstruction->Construct(ConstrNum).TotGlassLayers != 3)
                        WrongSurfaceType = true;
                    if (surf.ExtBoundCond != ExternalEnvironment) WrongSurfaceType = true;
                }
                if (WrongSurfaceType) {
                    ShowSevereError(state,
                                    format("{}=\"{}\" is not an exterior window with 2 or 3 glass layers.",
                                           ipsc->cCurrentModuleObject,
                                           ipsc->cAlphaArgs(1)));
                    ErrorsFound = true;
                }
            }

            // Error if illegal airflow source
            if (ipsc->cAlphaArgs(2) != "INDOORAIR" && ipsc->cAlphaArgs(2) != "OUTDOORAIR") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(2),
                                       ipsc->cAlphaArgs(2)));
            }

            // Error if illegal airflow destination
            if (ipsc->cAlphaArgs(3) != "INDOORAIR" && ipsc->cAlphaArgs(3) != "OUTDOORAIR" &&
                ipsc->cAlphaArgs(3) != "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(3),
                                       ipsc->cAlphaArgs(3)));
            }

            // Error if source = OutsideAir and destination = ReturnAir
            if (ipsc->cAlphaArgs(2) == "OUTDOORAIR" && ipsc->cAlphaArgs(3) == "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(2),
                                       ipsc->cAlphaArgs(2)));
                ShowContinueError(state, format("..when {}=\"{}\"", ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3)));
            }

            // Error if illegal airflow control type
            if (ipsc->cAlphaArgs(4) != "ALWAYSONATMAXIMUMFLOW" && ipsc->cAlphaArgs(4) != "ALWAYSOFF" &&
                ipsc->cAlphaArgs(4) != "SCHEDULEDONLY") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(4),
                                       ipsc->cAlphaArgs(4)));
            }

            // Error if illegal value for Airflow Has Multiplier Schedule
            if (ipsc->cAlphaArgs(5) != "YES" && ipsc->cAlphaArgs(5) != "NO") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(5),
                                       ipsc->cAlphaArgs(5)));
            }

            // Error if Airflow Control Type = ScheduledOnly and Airflow Has Multiplier Schedule = No
            if (ipsc->cAlphaArgs(4) == "SCHEDULEDONLY" && ipsc->cAlphaArgs(5) == "NO") {
                ErrorsFound = true;
                ShowSevereError(state,
                                format("{}=\"{}\" invalid {}=\"{}\"",
                                       ipsc->cCurrentModuleObject,
                                       ipsc->cAlphaArgs(1),
                                       ipsc->cAlphaFieldNames(4),
                                       ipsc->cAlphaArgs(4)));
                ShowContinueError(state, format("..when {}=\"{}\"", ipsc->cAlphaFieldNames(5), ipsc->cAlphaArgs(5)));
            }

            // Warning if Airflow Control Type = AlwaysOnAtMaxFlow and Airflow Has Multiplier Schedule = Yes
            if (ipsc->cAlphaArgs(4) == "ALWAYSONATMAXIMUMFLOW" && ipsc->cAlphaArgs(5) == "YES") {
                ShowWarningError(state,
                                 format("{}=\"{}has {}=\"{}\"",
                                        ipsc->cCurrentModuleObject,
                                        ipsc->cAlphaArgs(1),
                                        ipsc->cAlphaFieldNames(4),
                                        ipsc->cAlphaArgs(4)));
                ShowContinueError(state,
                                  format("..but {}=\"{}If specified, the {} will be ignored.",
                                         ipsc->cAlphaFieldNames(5),
                                         ipsc->cAlphaArgs(5),
                                         ipsc->cAlphaFieldNames(5)));
            }

            // Warning if Airflow Control Type = AlwaysOff and Airflow Has Multiplier Schedule = Yes
            if (ipsc->cAlphaArgs(4) == "ALWAYSOFF" && ipsc->cAlphaArgs(5) == "YES") {
                ShowWarningError(state,
                                 format("{}=\"{}has {}=\"{}\"",
                                        ipsc->cCurrentModuleObject,
                                        ipsc->cAlphaArgs(1),
                                        ipsc->cAlphaFieldNames(4),
                                        ipsc->cAlphaArgs(4)));
                ShowContinueError(state,
                                  format("..but {}=\"{}\". If specified, the {} will be ignored.",
                                         ipsc->cAlphaFieldNames(5),
                                         ipsc->cAlphaArgs(5),
                                         ipsc->cAlphaFieldNames(5)));
            }

            if (SurfNum > 0) {
                auto const &surf = state.dataSurface->Surface(SurfNum);
                state.dataSurface->AirflowWindows = true;
                state.dataSurface->SurfWinAirflowSource(SurfNum) =
                    static_cast<WindowAirFlowSource>(getEnumValue(WindowAirFlowSourceNamesUC, ipsc->cAlphaArgs(2)));

                state.dataSurface->SurfWinAirflowDestination(SurfNum) =
                    static_cast<WindowAirFlowDestination>(getEnumValue(WindowAirFlowDestinationNamesUC, ipsc->cAlphaArgs(3)));

                if (state.dataSurface->SurfWinAirflowDestination(SurfNum) == WindowAirFlowDestination::Return) {
                    int controlledZoneNum = DataZoneEquipment::GetControlledZoneIndex(state, surf.ZoneName);
                    if (controlledZoneNum > 0) {
                        state.dataHeatBal->Zone(surf.Zone).HasAirFlowWindowReturn = true;
                    }

                    // Set return air node number
                    state.dataSurface->SurfWinAirflowReturnNodePtr(SurfNum) = 0;
                    std::string retNodeName = "";
                    if (!ipsc->lAlphaFieldBlanks(7)) {
                        retNodeName = ipsc->cAlphaArgs(7);
                    }
                    std::string callDescription = ipsc->cCurrentModuleObject + "=" + surf.Name;
                    state.dataSurface->SurfWinAirflowReturnNodePtr(SurfNum) =
                        DataZoneEquipment::GetReturnAirNodeForZone(state, surf.Zone, retNodeName, callDescription);
                    if (state.dataSurface->SurfWinAirflowReturnNodePtr(SurfNum) == 0) {
                        ShowSevereError(state,
                                        format("{}: {}=\"{}\", airflow window return air node not found for {} = {}",
                                               routineName,
                                               ipsc->cCurrentModuleObject,
                                               surf.Name,
                                               ipsc->cAlphaFieldNames(3),
                                               ipsc->cAlphaArgs(3)));
                        if (!ipsc->lAlphaFieldBlanks(7))
                            ShowContinueError(state,
                                              format("{}=\"{}\" did not find a matching return air node.",
                                                     ipsc->cAlphaFieldNames(7),
                                                     ipsc->cAlphaArgs(7)));
                        ShowContinueError(state,
                                          "..Airflow windows with Airflow Destination = ReturnAir must reference a controlled Zone (appear in a "
                                          "ZoneHVAC:EquipmentConnections object) with at least one return air node.");
                        ErrorsFound = true;
                    }
                }
                if (Util::SameString(ipsc->cAlphaArgs(4), "AlwaysOnAtMaximumFlow")) {
                    state.dataSurface->SurfWinAirflowControlType(SurfNum) = WindowAirFlowControlType::MaxFlow;
                } else if (Util::SameString(ipsc->cAlphaArgs(4), "AlwaysOff")) {
                    state.dataSurface->SurfWinAirflowControlType(SurfNum) = WindowAirFlowControlType::AlwaysOff;
                } else if (Util::SameString(ipsc->cAlphaArgs(4), "ScheduledOnly")) {
                    state.dataSurface->SurfWinAirflowControlType(SurfNum) = WindowAirFlowControlType::Schedule;
                }
                state.dataSurface->SurfWinMaxAirflow(SurfNum) = ipsc->rNumericArgs(1);
                if (ipsc->cAlphaArgs(4) == "SCHEDULEDONLY" && ipsc->cAlphaArgs(5) == "YES") {
                    if (ipsc->lAlphaFieldBlanks(6)) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", has {}=\"{}\"",
                                               ipsc->cCurrentModuleObject,
                                               ipsc->cAlphaArgs(1),
                                               ipsc->cAlphaFieldNames(4),
                                               ipsc->cAlphaArgs(4)));
                        ShowContinueError(state,
                                          format("..and {}=\"{}\", but no {} specified.",
                                                 ipsc->cAlphaFieldNames(5),
                                                 ipsc->cAlphaArgs(5),
                                                 ipsc->cAlphaFieldNames(6)));
                    } else {
                        state.dataSurface->SurfWinAirflowHasSchedule(SurfNum) = true;
                        state.dataSurface->SurfWinAirflowSchedulePtr(SurfNum) = GetScheduleIndex(state, ipsc->cAlphaArgs(6));
                        if (state.dataSurface->SurfWinAirflowSchedulePtr(SurfNum) == 0) {
                            ErrorsFound = true;
                            ShowSevereItemNotFound(state, eoh,  ipsc->cAlphaFieldNames(6), ipsc->cAlphaArgs(6));
                        }
                    }
                }
                // Warning if associated window is an interior window
                if (surf.ExtBoundCond != ExternalEnvironment && !ErrorsFound)
                    ShowWarningError(state,
                                     format("{}=\"{}\", is an Interior window; cannot be an airflow window.",
                                            ipsc->cCurrentModuleObject,
                                            ipsc->cAlphaArgs(1)));
                if (!ErrorsFound) {
                    // Require that gas in airflow gap has type = air
                    MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(2);
                    if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers == 3)
                        MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(4);
                    if (dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatGapFlow))->gasTypes(1) != Material::GasType::Air) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        format("{}=\"{}\", Gas type not air in airflow gap of construction {}",
                                               ipsc->cCurrentModuleObject,
                                               ipsc->cAlphaArgs(1),
                                               state.dataConstruction->Construct(ConstrNum).Name));
                    }
                    // Require that gas be air in airflow gaps on either side of a between glass shade/blind
                    if (surf.HasShadeControl) {
                        for (std::size_t listIndex = 0; listIndex < surf.windowShadingControlList.size(); ++listIndex) {
                            int WSCPtr = surf.windowShadingControlList[listIndex];
                            if (ANY_BETWEENGLASS_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
                                ConstrNumSh = surf.shadedConstructionList[listIndex];
                                if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers == 2) {
                                    MatGapFlow1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2);
                                    MatGapFlow2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(4);
                                } else {
                                    MatGapFlow1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(4);
                                    MatGapFlow2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(6);
                                }
                                if (dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatGapFlow1))->gasTypes(1) !=
                                        Material::GasType::Air ||
                                    dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(MatGapFlow2))->gasTypes(1) !=
                                        Material::GasType::Air) {
                                    ErrorsFound = true;
                                    ShowSevereError(state,
                                                    format("{}=\"{}\", gas type must be air on either side of the shade/blind",
                                                           ipsc->cCurrentModuleObject,
                                                           ipsc->cAlphaArgs(1)));
                                }
                                break; // only need the first window shading control since they should be the same
                            }
                        }
                    }
                }
            }

        } // End of loop over window airflow controls
    } // GetWindowGapAirFlowControlData()

    void GetFoundationData(EnergyPlusData &state, bool &ErrorsFound)
    {

        int NumAlphas;
        int NumProps;
        int IOStat;
 
        constexpr std::string_view routineName = "GetFoundationData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        // Read Kiva Settings
        ipsc->cCurrentModuleObject = "Foundation:Kiva:Settings";
        int TotKivaStgs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (TotKivaStgs > 1) {
            ErrorsFound = true;
            ShowSevereError(state, format("Multiple {} objects found. Only one is allowed.", ipsc->cCurrentModuleObject));
        }

        if (TotKivaStgs == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     1,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumProps,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ""};
            int numF = 1;
            int alpF = 1;

            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.soilK = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.soilRho = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.soilCp = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.groundSolarAbs = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.groundThermalAbs = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.groundRoughness = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.farFieldWidth = ipsc->rNumericArgs(numF);
            }
            numF++;

            if (!ipsc->lAlphaFieldBlanks(alpF)) {
                sg->kivaManager.settings.deepGroundBoundary =
                    static_cast<HeatBalanceKivaManager::DGType>(getEnumValue(HeatBalanceKivaManager::dgTypeNamesUC, ipsc->cAlphaArgs(alpF)));
                if (sg->kivaManager.settings.deepGroundBoundary == HeatBalanceKivaManager::DGType::Invalid) {
                    ErrorsFound = true;
                    ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                }
            }
            alpF++;

            if (ipsc->lNumericFieldBlanks(numF) || ipsc->rNumericArgs(numF) == Constant::AutoCalculate) {
                // Autocalculate deep-ground depth (see KivaManager::defineDefaultFoundation() for actual calculation)
                sg->kivaManager.settings.deepGroundDepth = 40.0;
                sg->kivaManager.settings.autocalculateDeepGroundDepth = true;
                if (sg->kivaManager.settings.deepGroundBoundary != HeatBalanceKivaManager::DGType::AUTO) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    format("{}, {} should not be set to Autocalculate unless {} is set to Autoselect",
                                           ipsc->cCurrentModuleObject,
                                           ipsc->cNumericFieldNames(numF),
                                           ipsc->cAlphaFieldNames(alpF - 1)));
                }
            } else {
                sg->kivaManager.settings.deepGroundDepth = ipsc->rNumericArgs(numF);
                sg->kivaManager.settings.autocalculateDeepGroundDepth = false;
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.minCellDim = ipsc->rNumericArgs(numF);
            }
            numF++;
            if (!ipsc->lNumericFieldBlanks(numF)) {
                sg->kivaManager.settings.maxGrowthCoeff = ipsc->rNumericArgs(numF);
            }
            numF++;

            if (!ipsc->lAlphaFieldBlanks(alpF)) {
                if (Util::SameString(ipsc->cAlphaArgs(alpF), "Hourly")) {
                    sg->kivaManager.settings.timestepType = HeatBalanceKivaManager::TSType::HOURLY;
                    sg->kivaManager.timestep = 3600.; // seconds
                } else { // if (Util::SameString(ipsc->cAlphaArgs( alpF ), "Timestep"))
                    sg->kivaManager.settings.timestepType = HeatBalanceKivaManager::TSType::TIMESTEP;
                    sg->kivaManager.timestep = state.dataGlobal->MinutesPerTimeStep * 60.;
                }
            }
            alpF++;
        }

        // Set default foundation (probably doesn't need to be called if there are no Kiva
        // surfaces, but we don't know that yet). We call this here so that the default
        // foundation is available for 1) the starting copy for user-defined Foundation:Kiva
        // object default inputs, and 2) the actual default Foundation object if a
        // user-defined Foundation:Kiva name is not referenced by a surface.
        sg->kivaManager.defineDefaultFoundation(state);

        // Read Foundation objects
        ipsc->cCurrentModuleObject = "Foundation:Kiva";
        int TotKivaFnds = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        if (TotKivaFnds > 0) {
            for (int Loop = 1; Loop <= TotKivaFnds; ++Loop) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         Loop,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumProps,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);

                int numF = 1;
                int alpF = 1;

                ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};
                bool ErrorInName = false;

                HeatBalanceKivaManager::FoundationKiva fndInput;

                if (ipsc->lAlphaFieldBlanks(1)) {
                    ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                    ErrorsFound = true;
                    continue;
                }

                fndInput.name = ipsc->cAlphaArgs(alpF);
                alpF++;

                // Start with copy of default
                auto &fnd = fndInput.foundation;
                fnd = sg->kivaManager.defaultFoundation.foundation;

                // Indoor temperature
                if (!ipsc->lNumericFieldBlanks(numF)) {
                    fndInput.assumedIndoorTemperature = ipsc->rNumericArgs(numF);
                } else {
                    fndInput.assumedIndoorTemperature = -9999;
                }
                numF++;

                // Interior horizontal insulation
                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto *m = state.dataMaterial->Material(index);
                    if (m->group != Material::Group::Regular || m->ROnly) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                        continue;
                    }
                    fndInput.intHIns.x = 0.0;
                    fndInput.intHIns.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                    fndInput.intHIns.depth = m->Thickness;
                }
                alpF++;

                if (!ipsc->lAlphaFieldBlanks(alpF - 1)) {
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        fndInput.intHIns.z = 0.0;
                    } else {
                        fndInput.intHIns.z = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaFieldNames(alpF - 1), ipsc->cAlphaArgs(alpF - 1));
                        continue;
                    } else {
                        fndInput.intHIns.width = -ipsc->rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                }

                // Interior vertical insulation
                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto *m = state.dataMaterial->Material(index);
                    if (m->group != Material::Group::Regular || m->ROnly) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                        continue;
                    }
                    fndInput.intVIns.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                    fndInput.intVIns.width = -m->Thickness;
                    fndInput.intVIns.x = 0.0;
                    fndInput.intVIns.z = 0.0;
                }
                alpF++;

                if (!ipsc->lAlphaFieldBlanks(alpF - 1)) {
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaArgs(alpF - 1), ipsc->cAlphaFieldNames(alpF - 1));
                        continue;
                    } else {
                        fndInput.intVIns.depth = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                }

                // Exterior horizontal insulation
                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto *m = state.dataMaterial->Material(index);
                    if (m->group != Material::Group::Regular || m->ROnly) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                        continue;
                    }
                    fndInput.extHIns.x = 0.0;
                    fndInput.extHIns.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                    fndInput.extHIns.depth = m->Thickness;
                }
                alpF++;

                if (!ipsc->lAlphaFieldBlanks(alpF - 1)) {
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        fndInput.extHIns.z = 0.0;
                    } else {
                        fndInput.extHIns.z = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaFieldNames(alpF - 1), ipsc->cAlphaArgs(alpF - 1));
                        continue;
                    } else {
                        fndInput.extHIns.width = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                }

                // Exterior vertical insulation
                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto *m = state.dataMaterial->Material(index);
                    if (m->group != Material::Group::Regular || m->ROnly) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                        continue;
                    }
                    fndInput.extVIns.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                    fndInput.extVIns.width = m->Thickness;
                    fndInput.extVIns.x = 0.0;
                    fndInput.extVIns.z = 0.0;
                }
                alpF++;

                if (!ipsc->lAlphaFieldBlanks(alpF - 1)) {
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaFieldNames(alpF - 1), ipsc->cAlphaArgs(alpF - 1));
                        continue;
                    } else {
                        fndInput.extVIns.depth = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                }

                // Foundation wall
                if (!ipsc->lNumericFieldBlanks(numF)) {
                    fnd.wall.heightAboveGrade = ipsc->rNumericArgs(numF);
                }
                numF++;

                if (!ipsc->lNumericFieldBlanks(numF)) {
                    fnd.wall.depthBelowSlab = ipsc->rNumericArgs(numF);
                }
                numF++;

                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    fndInput.wallConstructionIndex = Util::FindItemInList(ipsc->cAlphaArgs(alpF), state.dataConstruction->Construct);
                    if (fndInput.wallConstructionIndex == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto &c = state.dataConstruction->Construct(fndInput.wallConstructionIndex);
                    c.IsUsed = true;
                    if (c.TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "cannot be a window construction.");
                        continue;
                    }
                } else {
                    fndInput.wallConstructionIndex = 0; // Use default wall construction
                }
                alpF++;

                // Footing
                if (!ipsc->lAlphaFieldBlanks(alpF)) {
                    int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                        continue;
                    }
                    auto *m = state.dataMaterial->Material(index);
                    if (m->group != Material::Group::Regular || m->ROnly) {
                        ErrorsFound = true;
                        ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                        continue;
                    }
                    fndInput.footing.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                    fndInput.footing.width = m->Thickness;
                    fndInput.footing.x = 0.0;
                    fndInput.footing.z = 0.0;
                }
                alpF++;

                if (!ipsc->lAlphaFieldBlanks(alpF - 1)) {
                    if (ipsc->lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaFieldNames(alpF - 1), ipsc->cAlphaArgs(alpF - 1));
                        continue;
                    } else {
                        fndInput.footing.depth = ipsc->rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!ipsc->lNumericFieldBlanks(numF)) {
                        ShowWarningError(
                            state,
                            format(
                                "{}=\"{}\", no {} defined", ipsc->cCurrentModuleObject, fndInput.name, ipsc->cAlphaFieldNames(alpF - 1)));
                        ShowContinueError(state, format("{} will not be used.", ipsc->cNumericFieldNames(numF)));
                    }
                    numF++;
                }

                // General Blocks
                int numRemainingFields = NumAlphas - (alpF - 1) + NumProps - (numF - 1);
                if (numRemainingFields > 0) {
                    int numBlocks = numRemainingFields / 4;
                    if (mod(numRemainingFields, 4) != 0) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", number of Block fields not even multiple of 4. Will read in {}",
                                                ipsc->cCurrentModuleObject,
                                                fndInput.name,
                                                numBlocks));
                    }
                    for (int blockNum = 0; blockNum < numBlocks; blockNum++) {
                        Kiva::InputBlock block;
                        if (!ipsc->lAlphaFieldBlanks(alpF)) {
                            int index = Util::FindItemInPtrList(ipsc->cAlphaArgs(alpF), state.dataMaterial->Material);
                            if (index == 0) {
                                ErrorsFound = true;
                                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF));
                                continue;
                            }
                            auto *m = state.dataMaterial->Material(index);
                            if (m->group != Material::Group::Regular || m->ROnly) {
                                ErrorsFound = true;
                                ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(alpF), ipsc->cAlphaArgs(alpF), "must be of type \"Material\".");
                                continue;
                            }
                            block.material = Kiva::Material(m->Conductivity, m->Density, m->SpecHeat);
                            block.width = m->Thickness;
                        } else {
                            ErrorsFound = true;
                            ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(alpF));
                            continue;
                        }
                        alpF++;

                        if (ipsc->lNumericFieldBlanks(numF)) {
                            block.depth = 0.0; // Temporary indicator to default to foundation depth
                        } else {
                            block.depth = ipsc->rNumericArgs(numF);
                        }
                        numF++;

                        if (ipsc->lNumericFieldBlanks(numF)) {
                            ErrorsFound = true;
                            ShowSevereEmptyField(state, eoh, ipsc->cNumericFieldNames(numF), ipsc->cAlphaFieldNames(alpF - 1), ipsc->cAlphaArgs(alpF - 1));
                            continue;
                        } else {
                            block.x = ipsc->rNumericArgs(numF);
                        }
                        numF++;

                        if (ipsc->lNumericFieldBlanks(numF)) {
                            block.z = 0.0;
                        } else {
                            block.z = ipsc->rNumericArgs(numF);
                        }
                        numF++;

                        fnd.inputBlocks.push_back(block);
                    }
                }

                state.dataSurfaceGeometry->kivaManager.foundationInputs.push_back(fndInput);
            } // for (Loop)
        } // if (TotKivaFnds > 0)
    } // GetFoundationData()

    void GetOSCData(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the OtherSideCoefficient data.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Other Side Coefficient Definition
        // OtherSideCoefficients,
        //       \memo This object sets the other side conditions for a surface in a variety of ways.
        //   A1, \field OtherSideCoeff Name
        //       \required-field
        //       \reference OSCNames
        //       \reference OutFaceEnvNames
        //   N1, \field Combined convective/radiative film coefficient
        //       \required-field
        //       \type real
        //       \note if>0, N1 becomes exterior convective/radiative film coefficient and other fields
        //       \note are used to calc outside air temp then exterior surface temp based on outside air
        //       \note and specified coefficient
        //       \note if<=0, then remaining fields calculate the outside surface temperature(?)
        //       \note following fields are used in the equation:
        //       \note SurfTemp=N7*TempZone + N4*OutsideDryBulb + N2*N3 + GroundTemp*N5 + WindSpeed*N6*OutsideDryBulb
        //   N2, \field User selected Constant Temperature
        //       \units C
        //       \type real
        //       \note This parameter will be overwritten by the values from the schedule(A2 below) if one is present
        //   N3, \field Coefficient modifying the user selected constant temperature
        //       \note This coefficient is used even with a schedule.  It should normally be 1.0 in that case
        //   N4, \field Coefficient modifying the external dry bulb temperature
        //       \type real
        //   N5, \field Coefficient modifying the ground temperature
        //       \type real
        //   N6, \field Coefficient modifying the wind speed term (s/m)
        //       \type real
        //   N7, \field Coefficient modifying the zone air temperature part of the equation
        //       \type real
        //   A2, \field ScheduleName for constant temperature
        //       \note Name of Schedule for values of "const" temperature.
        //       \note Schedule values replace N2 - User selected constant temperature.
        //       \type object-list
        //       \object-list ScheduleNames
        //   A3, \field Sinusoidal Variation of Constant Temperature Coefficient
        //       \note Optionally used to vary Constant Temperature Coefficient with unitary sine wave
        //       \type choice
        //       \key Yes
        //       \key No
        //       \default No
        //   N8; \field Period of Sinusoidal Variation
        //       \note Use with sinusoidal variation to define the time period
        //       \type real
        //       \units hr
        //       \default 24
        //  N9, \field Previous Other Side Temperature Coefficient
        //      \note This coeffient multiplies the other side temperature result from the
        //      \note previous zone timestep
        //      \type real
        //      \default 0
        // N10, \field Minimum Other Side Temperature
        //      \type real
        //      \units C
        //      \default -100
        // N11; \field Maximum Other Side Temperature
        //      \type real
        //      \units C
        //      \default 200

        // Using/Aliasing

        using ScheduleManager::GetScheduleIndex;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumProps;
        int Loop;
        int IOStat;
        int OSCNum;
        bool ErrorInName;
        bool IsBlank;
        std::string cOSCLimitsString;

        constexpr std::string_view routineName = "GetOSCData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        ipsc->cCurrentModuleObject = "SurfaceProperty:OtherSideCoefficients";
        state.dataSurface->TotOSC = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        state.dataSurface->OSC.allocate(state.dataSurface->TotOSC);

        OSCNum = 0;
        for (int Loop = 1; Loop <= state.dataSurface->TotOSC; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NumAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NumProps,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            if (ipsc->lAlphaFieldBlanks(1)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                ErrorsFound = true;
                continue;
            }

            if (Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->OSC)) {
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            ++OSCNum;

            auto &osc = state.dataSurface->OSC(OSCNum);
            osc.Name = ipsc->cAlphaArgs(1);
            osc.SurfFilmCoef = ipsc->rNumericArgs(1);
            osc.ConstTemp = ipsc->rNumericArgs(2); //  This will be replaced if  schedule is used
            osc.ConstTempCoef = ipsc->rNumericArgs(3); //  This multiplier is used (even with schedule).  It should normally be 1.0
            osc.ExtDryBulbCoef = ipsc->rNumericArgs(4);
            osc.GroundTempCoef = ipsc->rNumericArgs(5);
            osc.WindSpeedCoef = ipsc->rNumericArgs(6);
            osc.ZoneAirTempCoef = ipsc->rNumericArgs(7);
            osc.SinusoidPeriod = ipsc->rNumericArgs(8);

            if ((!ipsc->lAlphaFieldBlanks(2)) && (NumAlphas != 1)) { //  Const temp will come from schedule specified below.
                osc.ConstTempScheduleName = ipsc->cAlphaArgs(2);
                if (!osc.ConstTempScheduleName.empty()) {
                    osc.ConstTempScheduleIndex = GetScheduleIndex(state, osc.ConstTempScheduleName);
                    if (osc.ConstTempScheduleIndex == 0) {
                        ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                        ErrorsFound = true;
                    }
                }
            }

            if (!ipsc->lAlphaFieldBlanks(3)) {
                BooleanSwitch bs = getYesNoValue(ipsc->cAlphaArgs(3));
                if (bs == BooleanSwitch::Invalid) {
                    ShowSevereInvalidBool(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                    ErrorsFound = true;
                } else {
                    osc.SinusoidalConstTempCoef = static_cast<bool>(bs);
                }
            }

            if (ipsc->rNumericArgs(1) > 0.0 && !any_ne(ipsc->rNumericArgs({3, 7}), 0.0) &&
                (!osc.SinusoidalConstTempCoef)) {
                ShowSevereError(state,
                                format("{}=\"{}\" has zeros for all coefficients.", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state, "...The outdoor air temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            if (ipsc->rNumericArgs(1) <= 0.0 && !any_ne(ipsc->rNumericArgs({3, 7}), 0.0) &&
                (!osc.SinusoidalConstTempCoef)) {
                ShowSevereError(state,
                                format("{}=\"{}\" has zeros for all coefficients.", ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)));
                ShowContinueError(state,
                                  "...The outside surface temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            osc.TPreviousCoef = ipsc->rNumericArgs(9);

            if (!ipsc->lNumericFieldBlanks(10)) {
                osc.MinLimitPresent = true;
                osc.MinTempLimit = ipsc->rNumericArgs(10);
                cOSCLimitsString = format("{:.3R}", ipsc->rNumericArgs(10));
            } else {
                cOSCLimitsString = "N/A";
            }
            if (!ipsc->lNumericFieldBlanks(11)) {
                osc.MaxLimitPresent = true;
                osc.MaxTempLimit = ipsc->rNumericArgs(11);
                cOSCLimitsString += format(",{:.3R}", ipsc->rNumericArgs(10));
            } else {
                cOSCLimitsString += ",N/A";
            }
        }

        for (int Loop = 1; Loop <= state.dataSurface->TotOSC; ++Loop) {
            if (Loop == 1) {
                static constexpr std::string_view OSCFormat1(
                    "! <Other Side Coefficients>,Name,Combined convective/radiative film coefficient {W/m2-K},User selected "
                    "Constant Temperature {C},Coefficient modifying the constant temperature term,Coefficient modifying the external "
                    "dry bulb temperature term,Coefficient modifying the ground temperature term,Coefficient modifying the wind speed "
                    "term {s/m},Coefficient modifying the zone air temperature term,Constant Temperature Schedule Name,Sinusoidal "
                    "Variation,Period of Sinusoidal Variation,Previous Other Side Temperature Coefficient,Minimum Other Side "
                    "Temperature {C},Maximum Other Side Temperature {C}");
                print(state.files.eio, "{}\n", OSCFormat1);
            }

            auto &osc = state.dataSurface->OSC(Loop);
            if (osc.SurfFilmCoef > 0.0) {
                ipsc->cAlphaArgs(1) = format("{:.3R}", osc.SurfFilmCoef);
                SetupOutputVariable(state,
                                    "Surface Other Side Coefficients Exterior Air Drybulb Temperature",
                                    Constant::Units::C,
                                    osc.OSCTempCalc,
                                    OutputProcessor::SOVTimeStepType::System,
                                    OutputProcessor::SOVStoreType::Average,
                                    osc.Name);
            } else {
                ipsc->cAlphaArgs(1) = "N/A";
            }
            if (osc.ConstTempScheduleIndex != 0) {
                ipsc->cAlphaArgs(2) = osc.ConstTempScheduleName;
                constexpr std::string_view format = "Other Side Coefficients,{},{},{},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{},{},{:.3R},{:.3R},{}\n";
                print(state.files.eio,
                      format,
                      osc.Name,
                      ipsc->cAlphaArgs(1),
                      "N/A",
                      osc.ConstTempCoef,
                      osc.ExtDryBulbCoef,
                      osc.GroundTempCoef,
                      osc.WindSpeedCoef,
                      osc.ZoneAirTempCoef,
                      ipsc->cAlphaArgs(2),
                      ipsc->cAlphaArgs(3),
                      osc.SinusoidPeriod,
                      osc.TPreviousCoef,
                      cOSCLimitsString);
            } else {
                ipsc->cAlphaArgs(2) = "N/A";
                constexpr std::string_view format =
                    "Other Side Coefficients,{},{},{:.2R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{},{},{:.3R},{:.3R},{}\n";
                print(state.files.eio,
                      format,
                      osc.Name,
                      ipsc->cAlphaArgs(1),
                      osc.ConstTemp,
                      osc.ConstTempCoef,
                      osc.ExtDryBulbCoef,
                      osc.GroundTempCoef,
                      osc.WindSpeedCoef,
                      osc.ZoneAirTempCoef,
                      ipsc->cAlphaArgs(2),
                      ipsc->cAlphaArgs(3),
                      osc.SinusoidPeriod,
                      osc.TPreviousCoef,
                      cOSCLimitsString);
            }
        } // for (Loop)
    } // GetOSCData()

    void GetOSCMData(EnergyPlusData &state, bool &ErrorsFound)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the OtherSideConditionsModel data.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // derived from GetOSCData subroutine by Linda Lawrie

        //  OtherSideConditionsModel,
        //      \memo This object sets up modifying the other side conditions for a surface from other model results.
        //  A1, \field OtherSideConditionsModel Name
        //      \required-field
        //      \reference OSCMNames
        //      \reference OutFaceEnvNames
        //  A2; \field Type of Model to determine Boundary Conditions
        //      \type choice
        //      \key Transpired Collector
        //      \key Vented PV Cavity
        //      \key Hybrid PV Transpired Collector

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumProps;
        int Loop;
        int IOStat;
        int OSCMNum;
        bool ErrorInName;
        bool IsBlank;

        constexpr std::string_view routineName = "GetOSCMData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        ipsc->cCurrentModuleObject = "SurfaceProperty:OtherSideConditionsModel";
        state.dataSurface->TotOSCM = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        state.dataSurface->OSCM.allocate(state.dataSurface->TotOSCM);
        // OSCM is already initialized in derived type defn.

        OSCMNum = 0;
        for (int Loop = 1; Loop <= state.dataSurface->TotOSCM; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state,
                ipsc->cCurrentModuleObject,
                Loop,
                ipsc->cAlphaArgs,
                NumAlphas,
                ipsc->rNumericArgs,
                NumProps,
                IOStat,
                ipsc->lNumericFieldBlanks,
                ipsc->lAlphaFieldBlanks,
                ipsc->cAlphaFieldNames,
                ipsc->cNumericFieldNames);
            
            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            if (ipsc->lAlphaFieldBlanks(1)) {
                ShowSevereEmptyField(state, eoh, ipsc->cAlphaFieldNames(1));
                ErrorsFound = true;
                continue;
            }

            if (Util::FindItemInList(ipsc->cAlphaArgs(1), state.dataSurface->OSCM) > 0) { 
                ShowSevereDuplicateName(state, eoh);
                ErrorsFound = true;
                continue;
            }

            ++OSCMNum;
            auto &oscm = state.dataSurface->OSCM(OSCMNum);
            oscm.Name = ipsc->cAlphaArgs(1);
            // Note no validation of the below at this time:
            oscm.Class = ipsc->cAlphaArgs(2);
            // setup output vars for modeled coefficients
            SetupOutputVariable(state,
                                "Surface Other Side Conditions Modeled Convection Air Temperature",
                                Constant::Units::C,
                                oscm.TConv,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                oscm.Name);
            SetupOutputVariable(state,
                                "Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient",
                                Constant::Units::W_m2K,
                                oscm.HConv,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                oscm.Name);
            SetupOutputVariable(state,
                                "Surface Other Side Conditions Modeled Radiation Temperature",
                                Constant::Units::C,
                                oscm.TRad,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                oscm.Name);
            SetupOutputVariable(state,
                                "Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient",
                                Constant::Units::W_m2K,
                                oscm.HRad,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                oscm.Name);

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "Other Side Boundary Conditions",
                                 oscm.Name,
                                 "Convection Bulk Air Temperature",
                                 "[C]",
                                 oscm.EMSOverrideOnTConv,
                                 oscm.EMSOverrideTConvValue);
                SetupEMSActuator(state,
                                 "Other Side Boundary Conditions",
                                 oscm.Name,
                                 "Convection Heat Transfer Coefficient",
                                 "[W/m2-K]",
                                 oscm.EMSOverrideOnHConv,
                                 oscm.EMSOverrideHConvValue);
                SetupEMSActuator(state,
                                 "Other Side Boundary Conditions",
                                 oscm.Name,
                                 "Radiation Effective Temperature",
                                 "[C]",
                                 oscm.EMSOverrideOnTRad,
                                 oscm.EMSOverrideTRadValue);
                SetupEMSActuator(state,
                                 "Other Side Boundary Conditions",
                                 oscm.Name,
                                 "Radiation Linear Heat Transfer Coefficient",
                                 "[W/m2-K]",
                                 oscm.EMSOverrideOnHrad,
                                 oscm.EMSOverrideHradValue);
            }
        } // for (Loop)

        for (Loop = 1; Loop <= state.dataSurface->TotOSCM; ++Loop) {
            if (Loop == 1) {
                static constexpr std::string_view OSCMFormat1("! <Other Side Conditions Model>,Name,Class\n");
                print(state.files.eio, OSCMFormat1);
            }
            print(state.files.eio, "Other Side Conditions Model,{},{}\n", state.dataSurface->OSCM(Loop).Name, state.dataSurface->OSCM(Loop).Class);
        }
    } // GetOSCMData()

    void GetMovableInsulationData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the movable insulation data that can be associated with
        // a surface.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Movable Insulation Definition
        // SurfaceControl:MovableInsulation,
        //       \memo Exterior or Interior Insulation on opaque surfaces
        //   A1, \field Insulation Type
        //       \required-field
        //       \type choice
        //       \key Outside
        //       \key Inside
        //   A2, \field Surface Name
        //       \required-field
        //       \type object-list
        //       \object-list SurfaceNames
        //   A3, \field Material Name
        //       \required-field
        //       \object-list MaterialName
        //   A4; \field Schedule Name
        //        \required-field
        //        \type object-list
        //        \object-list ScheduleNames

        // Using/Aliasing

        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NAlphas;
        int NNums;
        int IOStat;
        int Loop;
        int NMatInsul;
        int SurfNum;
        int MaterNum;
        int SchNum;

        enum class InsulationType
        {
            Invalid = -1,
            Outside,
            Inside,
            Num
        };
        constexpr std::array<std::string_view, (int)InsulationType::Num> insulationTypeNamesUC = {"OUTSIDE", "INSIDE"};

        constexpr std::string_view routineName = "GetMovableInsulationData";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        ipsc->cCurrentModuleObject = "SurfaceControl:MovableInsulation";
        NMatInsul = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        for (Loop = 1; Loop <= NMatInsul; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     ipsc->cCurrentModuleObject,
                                                                     Loop,
                                                                     ipsc->cAlphaArgs,
                                                                     NAlphas,
                                                                     ipsc->rNumericArgs,
                                                                     NNums,
                                                                     IOStat,
                                                                     ipsc->lNumericFieldBlanks,
                                                                     ipsc->lAlphaFieldBlanks,
                                                                     ipsc->cAlphaFieldNames,
                                                                     ipsc->cNumericFieldNames);

            ErrorObjectHeader eoh{routineName, ipsc->cCurrentModuleObject, ipsc->cAlphaArgs(1)};

            InsulationType insulationType = static_cast<InsulationType>(getEnumValue(insulationTypeNamesUC, ipsc->cAlphaArgs(1)));
            if (insulationType == InsulationType::Invalid) {
                ShowSevereInvalidKey(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                continue;
            }
            
            SurfNum = Util::FindItemInList(ipsc->cAlphaArgs(2), state.dataSurface->Surface);
            if (SurfNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2));
                ErrorsFound = true;
                continue; 
            }
                                                                        
            if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {
                ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2), "surface is Window, use WindowShadingControl instead.");
                ErrorsFound = true;
                continue; 
            }

            MaterNum = Util::FindItemInPtrList(ipsc->cAlphaArgs(3), state.dataMaterial->Material);
            if (MaterNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3));
                ErrorsFound = true;
                continue; 
            }
            
            auto *mat = state.dataMaterial->Material(MaterNum);

            if ((mat->group == Material::Group::WindowSimpleGlazing) ||
                (mat->group == Material::Group::ShadeEquivalentLayer) ||
                (mat->group == Material::Group::DrapeEquivalentLayer) ||
                (mat->group == Material::Group::BlindEquivalentLayer) ||
                (mat->group == Material::Group::ScreenEquivalentLayer) ||
                (mat->group == Material::Group::GapEquivalentLayer)) {
                ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(3), ipsc->cAlphaArgs(3),
                                             format("Invalid movable insulation material of type {}.", Material::groupNames[(int)mat->group]));
                ErrorsFound = true;
                // continue; // Why not continue here?
            }

            if (mat->Resistance <= 0.0 && mat->Conductivity <= 0.0) {
                ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2),
                                             "material Resistance and/or Conductivity must be > 0.");
                ErrorsFound = true;
                continue;
            } else if (mat->Resistance <= 0.0) {
                mat->Resistance = mat->Thickness / mat->Conductivity;
            } else if (mat->Conductivity <= 0.0) {
                mat->Conductivity = mat->Thickness / mat->Resistance;
            }

            SchNum = GetScheduleIndex(state, ipsc->cAlphaArgs(4));
            if (SchNum == 0) {
                ShowSevereItemNotFound(state, eoh, ipsc->cAlphaFieldNames(4), ipsc->cAlphaArgs(4));
                ErrorsFound = true;
                continue;
            }
            
            switch (insulationType) {
            case InsulationType::Outside: {
                if (state.dataSurface->SurfMaterialMovInsulExt(SurfNum) > 0) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2),
                                                 format("\"Outside\", was already assigned Material=\"{}\".", state.dataMaterial->Material(state.dataSurface->SurfMaterialMovInsulExt(SurfNum))->Name));
                    ErrorsFound = true;
                    continue;
                }
                state.dataSurface->SurfMaterialMovInsulExt(SurfNum) = MaterNum;
                state.dataSurface->SurfSchedMovInsulExt(SurfNum) = SchNum;
                state.dataSurface->AnyMovableInsulation = true;
            } break;

            case InsulationType::Inside: {
                if (state.dataSurface->SurfMaterialMovInsulInt(SurfNum) > 0) {
                    ShowSevereFieldCustomMessage(state, eoh, ipsc->cAlphaFieldNames(2), ipsc->cAlphaArgs(2),
                                                 format("\"Outside\", was already assigned Material=\"{}\".", state.dataMaterial->Material(state.dataSurface->SurfMaterialMovInsulInt(SurfNum))->Name));
                    ErrorsFound = true;
                    continue;
                }
                state.dataSurface->SurfMaterialMovInsulInt(SurfNum) = MaterNum;
                state.dataSurface->SurfSchedMovInsulInt(SurfNum) = SchNum;
                state.dataSurface->AnyMovableInsulation = true;
            } break;

            default: {
                assert(false);
            } break;
            } // switch (insulationType)
        } // for (Loop)
    } // GetMovableInsulationData()

    // Calculates the volume (m3) of a zone using the surfaces as possible.
    void CalculateZoneVolume(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN   1992-1994
        //       MODIFIED       Sep 2007, Mar 2017

        // METHODOLOGY EMPLOYED:
        // Uses surface area information for calculations.  Modified to use the
        // user-entered ceiling height (x floor area, if applicable) instead of using
        // the calculated volume when the user enters the ceiling height.

        // REFERENCES:
        // Legacy Code (IBLAST)

        Vectors::Polyhedron ZoneStruct;
        bool initmsg = true;
        bool ShowZoneSurfaces = (state.dataInputProcessing->inputProcessor->getNumSectionsFound("SHOWZONESURFACES_DEBUG") > 0);
        EPVector<int> surfacenotused;

        enum class ZoneVolumeCalcMethod
        {
            Invalid = -1,
            Enclosed,
            FloorAreaTimesHeight1,
            FloorAreaTimesHeight2,
            CeilingAreaTimesHeight,
            OpWallAreaTimesDistance,
            UserProvided,
            Num
        };

        int countNotFullyEnclosedZones = 0;
        for (auto &thisZone : state.dataHeatBal->Zone) {
            if (!thisZone.HasFloor) {
                ShowWarningError(state,
                                 format("No floor exists in Zone=\"{}\", zone floor area is zero. All values for this zone that are entered per "
                                        "floor area will be zero.",
                                        thisZone.Name));
            }

            Real64 SumAreas = 0.0;
            Real64 CalcVolume = 0.0;
            // Use AllSurfaceFirst which includes air boundaries
            int NFaces = thisZone.AllSurfaceLast - thisZone.AllSurfaceFirst + 1;
            int notused = 0;
            ZoneStruct.NumFaces = NFaces;
            ZoneStruct.Faces.allocate(NFaces);
            int NActFaces = 0;
            surfacenotused.dimension(NFaces, 0);

            for (int SurfNum = thisZone.AllSurfaceFirst; SurfNum <= thisZone.AllSurfaceLast; ++SurfNum) {
                auto &thisSurface = state.dataSurface->Surface(SurfNum);
                // Only include Base Surfaces in Calc.

                if (thisSurface.Class != SurfaceClass::Wall && thisSurface.Class != SurfaceClass::Floor && thisSurface.Class != SurfaceClass::Roof) {
                    ++notused;
                    surfacenotused(notused) = SurfNum;
                    continue;
                }

                ++NActFaces;
                auto &thisFace = ZoneStruct.Faces(NActFaces);
                thisFace.Points.allocate(thisSurface.Sides);
                thisFace.NumSides = thisSurface.Sides;
                thisFace.SurfNum = SurfNum;
                thisFace.Points({1, thisSurface.Sides}) = thisSurface.Vertex({1, thisSurface.Sides});
                thisFace.NewellAreaVector = Vectors::CalcNewellAreaVector(thisFace.Points, thisFace.NumSides);
                SumAreas += length(thisFace.NewellAreaVector);
            }
            ZoneStruct.NumFaces = NActFaces;

            bool isFloorHorizontal = false;
            bool isCeilingHorizontal = false;
            bool areWallsVertical = false;
            std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(state, ZoneStruct);
            Real64 oppositeWallArea = 0.0;
            Real64 distanceBetweenOppositeWalls = 0.0;

            bool areWallsSameHeight = areWallHeightSame(state, ZoneStruct);

            std::vector<EdgeOfSurf> listOfedgeNotUsedTwice;
            bool isZoneEnclosed = isEnclosedVolume(ZoneStruct, listOfedgeNotUsedTwice);
            ZoneVolumeCalcMethod volCalcMethod;

            Real64 floorAreaForVolume = (thisZone.FloorArea > 0.0) ? thisZone.FloorArea : thisZone.geometricFloorArea;
            Real64 ceilingAreaForVolume = (thisZone.CeilingArea > 0.0) ? thisZone.CeilingArea : thisZone.geometricCeilingArea;

            if (isZoneEnclosed) {
                CalcVolume = Vectors::CalcPolyhedronVolume(state, ZoneStruct);
                volCalcMethod = ZoneVolumeCalcMethod::Enclosed;
            } else if (floorAreaForVolume > 0.0 && thisZone.CeilingHeight > 0.0 && areFloorAndCeilingSame(state, ZoneStruct)) {
                CalcVolume = floorAreaForVolume * thisZone.CeilingHeight;
                volCalcMethod = ZoneVolumeCalcMethod::FloorAreaTimesHeight1;
            } else if (isFloorHorizontal && areWallsVertical && areWallsSameHeight && floorAreaForVolume > 0.0 && thisZone.CeilingHeight > 0.0) {
                CalcVolume = floorAreaForVolume * thisZone.CeilingHeight;
                volCalcMethod = ZoneVolumeCalcMethod::FloorAreaTimesHeight2;
            } else if (isCeilingHorizontal && areWallsVertical && areWallsSameHeight && ceilingAreaForVolume > 0.0 && thisZone.CeilingHeight > 0.0) {
                CalcVolume = ceilingAreaForVolume * thisZone.CeilingHeight;
                volCalcMethod = ZoneVolumeCalcMethod::CeilingAreaTimesHeight;
            } else if (areOppositeWallsSame(state, ZoneStruct, oppositeWallArea, distanceBetweenOppositeWalls)) {
                CalcVolume = oppositeWallArea * distanceBetweenOppositeWalls;
                volCalcMethod = ZoneVolumeCalcMethod::OpWallAreaTimesDistance;
            } else if (thisZone.Volume == Constant::AutoCalculate) { // no user entered zone volume
                ShowSevereError(state,
                                format("For zone: {} it is not possible to calculate the volume from the surrounding surfaces so either provide the "
                                       "volume value or define all the surfaces to fully enclose the zone.",
                                       thisZone.Name));
                CalcVolume = 0.;
                volCalcMethod = ZoneVolumeCalcMethod::Invalid;
            } else {
                CalcVolume = 0.;
                volCalcMethod = ZoneVolumeCalcMethod::UserProvided;
            }
            if (!isZoneEnclosed) {
                ++countNotFullyEnclosedZones;
                if (state.dataGlobal->DisplayExtraWarnings) { // report missing
                    ShowWarningError(state,
                                     format("CalculateZoneVolume: The Zone=\"{}\" is not fully enclosed. To be fully enclosed, each edge of a "
                                            "surface must also be an edge on one other surface.",
                                            thisZone.Name));
                    switch (volCalcMethod) {
                    case ZoneVolumeCalcMethod::FloorAreaTimesHeight1:
                        ShowContinueError(state,
                                          "  The zone volume was calculated using the floor area times ceiling height method where the floor and "
                                          "ceiling are the same except for the z-coordinates.");
                        break;
                    case ZoneVolumeCalcMethod::FloorAreaTimesHeight2:
                        ShowContinueError(state,
                                          "  The zone volume was calculated using the floor area times ceiling height method where the floor is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case ZoneVolumeCalcMethod::CeilingAreaTimesHeight:
                        ShowContinueError(state,
                                          "  The zone volume was calculated using the ceiling area times ceiling height method where the ceiling is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case ZoneVolumeCalcMethod::OpWallAreaTimesDistance:
                        ShowContinueError(state,
                                          "  The zone volume was calculated using the opposite wall area times the distance between them method ");
                        break;
                    case ZoneVolumeCalcMethod::UserProvided:
                        ShowContinueError(state, "  The zone volume was provided as an input to the ZONE object ");
                        break;
                    case ZoneVolumeCalcMethod::Invalid:
                        ShowContinueError(state, "  The zone volume was not calculated and an error exists. ");
                        break;
                    case ZoneVolumeCalcMethod::Enclosed: // should not be called but completes enumeration
                        ShowContinueError(state, "  The zone volume was calculated using multiple pyramids and was fully enclosed. ");
                        break;
                    default:
                        assert(false);
                    }
                    for (auto &edge : listOfedgeNotUsedTwice) {
                        if (edge.count < 2) {
                            ShowContinueError(
                                state,
                                fmt::format("  The surface \"{}\" has an edge that was used only once: it is not an edge on another surface",
                                            state.dataSurface->Surface(edge.surfNum).Name));

                        } else {
                            ShowContinueError(
                                state,
                                fmt::format("  The surface \"{}\" has an edge that was used {} times: it is an edge on three or more surfaces: ",
                                            state.dataSurface->Surface(edge.surfNum).Name,
                                            edge.count));
                            std::string surfaceNames = "    It was found on the following Surfaces: ";
                            for (int surfNum : edge.otherSurfNums) {
                                surfaceNames += fmt::format("'{}' ", state.dataSurface->Surface(surfNum).Name);
                            }
                            ShowContinueError(state, surfaceNames);
                        }
                        ShowContinueError(state, format("    Vertex start {{ {:.4R}, {:.4R}, {:.4R}}}", edge.start.x, edge.start.y, edge.start.z));
                        ShowContinueError(state, format("    Vertex end   {{ {:.4R}, {:.4R}, {:.4R}}}", edge.end.x, edge.end.y, edge.end.z));
                    }
                }
            }
            if (thisZone.Volume > 0.0) { // User entered zone volume, produce message if not near calculated
                if (CalcVolume > 0.0) {
                    if (std::abs(CalcVolume - thisZone.Volume) / thisZone.Volume > 0.05) {
                        ++state.dataSurfaceGeometry->ErrCount5;
                        if (state.dataSurfaceGeometry->ErrCount5 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage(state,
                                            "Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            ShowWarningError(state, "Entered Zone Volumes differ from calculated zone volume(s).");
                            ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage(state,
                                            "Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            // Warn user of using specified Zone Volume
                            ShowWarningError(
                                state,
                                format("Entered Volume entered for Zone=\"{}\" significantly different from calculated Volume", thisZone.Name));
                            ShowContinueError(state,
                                              format("Entered Zone Volume value={:.2R}, Calculated Zone Volume value={:.2R}, entered volume will be "
                                                     "used in calculations.",
                                                     thisZone.Volume,
                                                     CalcVolume));
                        }
                    }
                }
            } else if (thisZone.ceilingHeightEntered) { // User did not enter zone volume, but entered ceiling height
                if (floorAreaForVolume > 0.0) {
                    thisZone.Volume = floorAreaForVolume * thisZone.CeilingHeight;
                } else { // ceiling height entered but floor area zero
                    thisZone.Volume = CalcVolume;
                }
            } else { // Neither ceiling height nor volume entered
                thisZone.Volume = CalcVolume;
            }

            if (thisZone.Volume <= 0.0) {
                ShowWarningError(state, format("Indicated Zone Volume <= 0.0 for Zone={}", thisZone.Name));
                ShowContinueError(state, format("The calculated Zone Volume was={:.2R}", thisZone.Volume));
                ShowContinueError(state, "The simulation will continue with the Zone Volume set to 10.0 m3. ");
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                thisZone.Volume = 10.;
            }
            // For now - pro-rate space volumes by floor area, if not entered
            for (int spaceNum : thisZone.spaceIndexes) {
                auto &thisSpace = state.dataHeatBal->space(spaceNum);
                // don't touch if already user-specified
                if (thisSpace.Volume > 0.0) continue;
                if (thisZone.numSpaces == 1) {
                    thisSpace.Volume = thisZone.Volume;
                } else if (thisZone.geometricFloorArea > 0.0) {
                    thisSpace.Volume = thisZone.Volume * thisSpace.FloorArea / thisZone.geometricFloorArea;
                }
            }
            Real64 totSpacesVolume = 0.0;
            for (int spaceNum : thisZone.spaceIndexes) {
                totSpacesVolume += state.dataHeatBal->space(spaceNum).Volume;
            }
            if (totSpacesVolume > 0.0) {
                for (int spaceNum : thisZone.spaceIndexes) {
                    state.dataHeatBal->space(spaceNum).fracZoneVolume = state.dataHeatBal->space(spaceNum).Volume / totSpacesVolume;
                }
            } // else leave fractions at zero

            if (ShowZoneSurfaces) {
                if (state.dataSurfaceGeometry->ShowZoneSurfaceHeaders) {
                    print(state.files.debug, "{}\n", "===================================");
                    print(state.files.debug, "{}\n", "showing zone surfaces used and not used in volume calculation");
                    print(state.files.debug, "{}\n", "for volume calculation, only floors, walls and roofs/ceilings are used");
                    print(state.files.debug, "{}\n", "surface class, 1=wall, 2=floor, 3=roof/ceiling");
                    print(state.files.debug, "{}\n", "unused surface class(es), 5=internal mass, 11=window, 12=glass door");
                    print(state.files.debug, "{}\n", "                          13=door, 14=shading, 15=overhang, 16=fin");
                    print(state.files.debug, "{}\n", "                          17=TDD Dome, 18=TDD Diffuser");
                    state.dataSurfaceGeometry->ShowZoneSurfaceHeaders = false;
                }
                print(state.files.debug, "{}\n", "===================================");
                print(state.files.debug, "zone={} calc volume={}\n", thisZone.Name, CalcVolume);
                print(state.files.debug, " nsurfaces={} nactual={}\n", NFaces, NActFaces);
            }
            for (int faceNum = 1; faceNum <= ZoneStruct.NumFaces; ++faceNum) {
                auto &thisFace = ZoneStruct.Faces(faceNum);
                if (ShowZoneSurfaces) {
                    if (faceNum <= NActFaces) {
                        auto &thisSurface = state.dataSurface->Surface(thisFace.SurfNum);
                        print(state.files.debug, "surface={} nsides={}\n", thisFace.SurfNum, thisFace.NumSides);
                        print(state.files.debug, "surface name={} class={}\n", thisSurface.Name, thisSurface.Class);
                        print(state.files.debug, "area={}\n", thisSurface.GrossArea);
                        for (auto const &FacePoint : thisFace.Points) {
                            print(state.files.debug, "{} {} {}\n", FacePoint.x, FacePoint.y, FacePoint.z);
                        }
                    }
                }
                thisFace.Points.deallocate();
            }
            if (ShowZoneSurfaces) {
                for (int SurfNum = 1; SurfNum <= notused; ++SurfNum) {
                    print(state.files.debug,
                          "notused:surface={} name={} class={}\n",
                          surfacenotused(SurfNum),
                          state.dataSurface->Surface(surfacenotused(SurfNum)).Name,
                          state.dataSurface->Surface(surfacenotused(SurfNum)).Class);
                }
            }

            ZoneStruct.Faces.deallocate();
            surfacenotused.deallocate();

        } // zone loop
        if (!state.dataGlobal->DisplayExtraWarnings) {
            if (countNotFullyEnclosedZones == 1) {
                ShowWarningError(
                    state, "CalculateZoneVolume: 1 zone is not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; ");
            } else if (countNotFullyEnclosedZones > 1) {
                ShowWarningError(state,
                                 format("CalculateZoneVolume: {} zones are not fully enclosed. For more details use:  "
                                        "Output:Diagnostics,DisplayExtrawarnings; ",
                                        countNotFullyEnclosedZones));
            }
        }
    } // CalculateZoneVolume()

    // test if the volume described by the polyhedron if full enclosed (would not leak)
    bool isEnclosedVolume(DataVectorTypes::Polyhedron const &zonePoly, std::vector<EdgeOfSurf> &edgeNot2)
    {
        // J. Glazer - March 2017

        std::vector<Vector3<Real64>> uniqueVertices = makeListOfUniqueVertices(zonePoly);

        std::vector<EdgeOfSurf> edgeNot2orig = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);
        // if all edges had two counts then it is fully enclosed
        if (edgeNot2orig.empty()) {
            edgeNot2 = edgeNot2orig;
            return true;
        } else { // if the count is three or greater it is likely that a vertex that is colinear was counted on the faces on one edge and not
                 // on the "other side" of the edge Go through all the points looking for the number that are colinear and see if that is
                 // consistent with the number of edges found that didn't have a count of two
            DataVectorTypes::Polyhedron updatedZonePoly = updateZonePolygonsForMissingColinearPoints(
                zonePoly, uniqueVertices); // this is done after initial test since it is computationally intensive.
            std::vector<EdgeOfSurf> edgeNot2again = edgesNotTwoForEnclosedVolumeTest(updatedZonePoly, uniqueVertices);
            if (edgeNot2again.empty()) {
                return true;
            } else {
                edgeNot2 = edgesInBoth(edgeNot2orig,
                                       edgeNot2again); // only return a list of those edges that appear in both the original edge and the
                                                       // revised edges this eliminates added edges that will confuse users and edges that
                                                       // were caught by the updateZonePoly routine
                return false;
            }
        }
    } // isEnclosedVolume()

    // returns a vector of edges that are in both vectors
    std::vector<EdgeOfSurf> edgesInBoth(std::vector<EdgeOfSurf> const &edges1, std::vector<EdgeOfSurf> const &edges2)
    {
        // J. Glazer - June 2017
        // this is not optimized but the number of edges for a typical polyhedron is 12 and is probably rarely bigger than 20.

        std::vector<EdgeOfSurf> inBoth;
        for (const auto &e1 : edges1) {
            for (const auto &e2 : edges2) {
                if (edgesEqualOnSameSurface(e1, e2)) {
                    inBoth.push_back(e1);
                    break;
                }
            }
        }
        return inBoth;
    }

    // returns true if the edges match - including the surface number
    bool edgesEqualOnSameSurface(EdgeOfSurf const &a, EdgeOfSurf const &b)
    {
        if (a.surfNum != b.surfNum) {
            return false;
        }

        // vertex comparison (we compare indices, so absolute equal)
        return ((a.start == b.start && a.end == b.end) || (a.start == b.end && a.end == b.start));
    }

    // returns the number of times the edges of the polyhedron of the zone are not used twice by the sides
    std::vector<EdgeOfSurf> edgesNotTwoForEnclosedVolumeTest(DataVectorTypes::Polyhedron const &zonePoly, std::vector<Vector3<Real64>> const &uniqueVertices)
    {
        // J. Glazer - March 2017

        using DataVectorTypes::Vector;

        struct EdgeByPts
        {
            int start;
            int end;
            int count;
            int firstSurfNum;
            std::vector<int> otherSurfNums;
            EdgeByPts() : start(0), end(0), count(0), firstSurfNum(0)
            {
            }
        };
        std::vector<EdgeByPts> uniqueEdges;
        uniqueEdges.reserve(zonePoly.NumFaces * 6);

        // construct list of unique edges
        Vector3<Real64> curVertex;
        int curVertexIndex;
        for (auto const &face : zonePoly.Faces) {
            Vector3<Real64> prevVertex;
            int prevVertexIndex;
            for (int jVertex = 1; jVertex <= face.NumSides; ++jVertex) {
                if (jVertex == 1) {
                    prevVertex = face.Points(face.NumSides); // the last point
                    prevVertexIndex = findIndexOfVertex(prevVertex, uniqueVertices);
                } else {
                    prevVertex = curVertex;
                    prevVertexIndex = curVertexIndex;
                }
                curVertex = face.Points(jVertex);
                curVertexIndex = findIndexOfVertex(curVertex, uniqueVertices); // uses isAlmostEqual3dPt
                auto it = std::find_if(uniqueEdges.begin(), uniqueEdges.end(), [&curVertexIndex, &prevVertexIndex](const auto &edge) {
                    return ((edge.start == curVertexIndex && edge.end == prevVertexIndex) ||
                            (edge.start == prevVertexIndex && edge.end == curVertexIndex));
                });
                if (it == uniqueEdges.end()) {
                    EdgeByPts curEdge;
                    curEdge.start = prevVertexIndex;
                    curEdge.end = curVertexIndex;
                    curEdge.count = 1;
                    curEdge.firstSurfNum = face.SurfNum;
                    uniqueEdges.emplace_back(curEdge);
                } else {
                    ++(it->count);
                    it->otherSurfNums.push_back(face.SurfNum);
                }
            }
        }
        // All edges for an enclosed polyhedron should be shared by two (and only two) sides.
        // So if the count is not two for all edges, the polyhedron is not enclosed
        std::vector<EdgeOfSurf> edgesNotTwoCount;
        for (const auto &anEdge : uniqueEdges) {
            if (anEdge.count != 2) {
                EdgeOfSurf curEdgeOne;
                curEdgeOne.surfNum = anEdge.firstSurfNum;
                curEdgeOne.start = uniqueVertices[anEdge.start];
                curEdgeOne.end = uniqueVertices[anEdge.end];
                curEdgeOne.count = anEdge.count;
                curEdgeOne.otherSurfNums = anEdge.otherSurfNums;
                edgesNotTwoCount.push_back(curEdgeOne);
            }
        }
        return edgesNotTwoCount;
    }

    // create a list of unique vertices given the polyhedron describing the zone
    std::vector<Vector3<Real64>> makeListOfUniqueVertices(DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017
        std::vector<Vector3<Real64>> uniqVertices;
        uniqVertices.reserve(zonePoly.NumFaces * 6);

        for (auto const &face : zonePoly.Faces) { 
            for (Vector3<Real64> curVertex : face.Points) { 
                if (uniqVertices.size() == 0) {
                    uniqVertices.emplace_back(curVertex);
                } else {
                    bool found = false;
                    for (const auto &unqV : uniqVertices) {
                        if (Vectors::VecEqualTol(curVertex, unqV, 0.0127)) { // 0.0127 m == 1/2 in
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        uniqVertices.emplace_back(curVertex);
                    }
                }
            }
        }
        return uniqVertices;
    } // makeListOfUniqueVertices()

    // updates the polyhedron used to describe a zone to include points on an edge that are between and collinear to points already describing
    // the edge
    DataVectorTypes::Polyhedron updateZonePolygonsForMissingColinearPoints(DataVectorTypes::Polyhedron const &zonePoly,
                                                                           std::vector<Vector3<Real64>> const &uniqVertices)
    {
        // J. Glazer - March 2017
        DataVectorTypes::Polyhedron updZonePoly = zonePoly; // set the return value to the original polyhedron describing the zone

        for (auto &updFace : updZonePoly.Faces) {
            bool insertedVertext = true;
            while (insertedVertext) {
                insertedVertext = false;
                auto &vertices = updFace.Points;
                for (auto it = vertices.begin(); it != vertices.end(); ++it) {

                    auto itnext = std::next(it);
                    if (itnext == std::end(vertices)) {
                        itnext = std::begin(vertices);
                    }

                    auto curVertex = *it;      // (AUTO_OK_OBJ) can't tell if a copy is the intended behavior here
                    auto nextVertex = *itnext; // (AUTO_OK_OBJ)

                    // now go through all the vertices and see if they are colinear with start and end vertices
                    for (const auto &testVertex : uniqVertices) {
                        if (!Vectors::VecEqualTol(curVertex, testVertex, 0.0127) && !Vectors::VecEqualTol(nextVertex, testVertex, 0.0127)) {
                            if (isPointOnLineBetweenPoints(curVertex, nextVertex, testVertex)) {
                                vertices.insert(itnext, testVertex);
                                ++updFace.NumSides;
                                insertedVertext = true;
                                break;
                            }
                        }
                    }
                    // Break out of the loop on vertices of the surface too, and start again at the while
                    if (insertedVertext) {
                        break;
                    }
                }
            }
        }
        return updZonePoly;
    }

    // test if the ceiling and floor are the same except for their height difference by looking at the corners
    bool areFloorAndCeilingSame(EnergyPlusData &state, DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // check if the floor and ceiling are the same
        // this is almost equivalent to saying, if you ignore the z-coordinate, are the vertices the same
        // so if you could all the unique vertices of the floor and ceiling, ignoring the z-coordinate, they
        // should always be even (they would be two but you might define multiple surfaces that meet in a corner)

        struct Vector2Count : Vector2<Real64> {
            int count = 0;
        };

        std::vector<Vector2Count> floorCeilingXY;
        floorCeilingXY.reserve(zonePoly.NumFaces * 6);

        // make list of x and y coordinates for all faces that are on the floor or ceiling
        for (int iFace = 1; iFace <= zonePoly.NumFaces; ++iFace) {
            auto const &face = zonePoly.Faces(iFace);
            int curSurfNum = face.SurfNum;
            if (state.dataSurface->Surface(curSurfNum).Class == SurfaceClass::Floor ||
                state.dataSurface->Surface(curSurfNum).Class == SurfaceClass::Roof) {
                for (int jVertex = 1; jVertex <= face.NumSides; ++jVertex) {
                    Vector3<Real64> curVertex = face.Points(jVertex);
                    Vector2Count curXYc;
                    curXYc.x = curVertex.x;
                    curXYc.y = curVertex.y;
                    curXYc.count = 1;
                    bool found = false;
                    for (Vector2Count &curFloorCeiling : floorCeilingXY) { // can't use just "auto" because updating floorCeilingXY
                        if (Vectors::VecEqualTol(curXYc, curFloorCeiling, 0.0127)) {   // 0.0127 m == 1/2 in 
                            ++curFloorCeiling.count;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        floorCeilingXY.emplace_back(curXYc);
                    }
                }
            }
        }
        // now make sure every point has been counted and even number of times (usually twice)
        // if they are then the ceiling and floor are (almost certainly) the same x and y coordinates.
        bool areFlrAndClgSame = true;
        if (floorCeilingXY.size() > 0) {
            for (auto const &curFloorCeiling : floorCeilingXY) {
                if (curFloorCeiling.count % 2 != 0) {
                    areFlrAndClgSame = false;
                    break;
                }
            }
        } else {
            areFlrAndClgSame = false;
        }
        return areFlrAndClgSame;
    }

    // test if the walls of a zone are all the same height using the polyhedron describing the zone geometry
    bool areWallHeightSame(EnergyPlusData &state, DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // test if all the wall heights are the same (all walls have the same maximum z-coordinate

        bool areWlHgtSame = true;
        Real64 wallHeightZ = -1.0E50;
        bool foundWallHeight = false;
        // Can't use iterator loop because zonePoly.NumFaces can be less than zonePoly.Faces.size()
        for (int iFace = 1; iFace <= zonePoly.NumFaces; ++iFace) {
            auto const &face = zonePoly.Faces(iFace);
            if (state.dataSurface->Surface(face.SurfNum).Class == SurfaceClass::Wall) {
                Real64 maxZ = -1.0E50;
                for (auto const &vertex : face.Points) {
                    if (maxZ < vertex.z) {
                        maxZ = vertex.z;
                    }
                }
                if (foundWallHeight) {
                    if (std::abs(maxZ - wallHeightZ) > 0.0254) { //  2.54 cm = 1 inch
                        areWlHgtSame = false;
                        break;
                    }
                } else {
                    wallHeightZ = maxZ;
                    foundWallHeight = true;
                }
            }
        }
        return areWlHgtSame;
    } // areWallHeightsSame()

    // tests if the floor is horizontal, ceiling is horizontal, and walls are vertical and returns all three as a tuple of booleans
    std::tuple<bool, bool, bool> areSurfaceHorizAndVert(EnergyPlusData &state, DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // check if floors and ceilings are horizontal and walls are vertical
        bool isFlrHoriz = true;
        bool isClgHoriz = true;
        bool areWlVert = true;
        // Can't use iterator loop because NumFaces may be smaller than Faces.size()
        for (int iFace = 1; iFace < zonePoly.NumFaces; ++iFace) { 
            auto const &face = zonePoly.Faces(iFace);
            auto const &surf = state.dataSurface->Surface(face.SurfNum);
            if (surf.Class == SurfaceClass::Floor) {
                if (std::abs(surf.Tilt - 180.) > 1.) { // with 1 degree angle
                    isFlrHoriz = false;
                }
            } else if (surf.Class == SurfaceClass::Roof) { // includes ceilings
                if (std::abs(surf.Tilt) > 1.) {            // with 1 degree angle of
                    isClgHoriz = false;
                }
            } else if (surf.Class == SurfaceClass::Wall) {
                if (std::abs(surf.Tilt - 90) > 1.) { // with 1 degree angle
                    areWlVert = false;
                }
            }
        }
        return std::make_tuple(isFlrHoriz, isClgHoriz, areWlVert);
    } // areSurfaceHorizAndVert()

    // tests whether a pair of walls in the zone are the same except offset from one another and facing the opposite direction and also
    // returns the wall area and distance between
    bool areOppositeWallsSame(EnergyPlusData &state,
                              DataVectorTypes::Polyhedron const &zonePoly,
                              Real64 &oppositeWallArea,            // return the area of the wall that has an opposite wall
                              Real64 &distanceBetweenOppositeWalls // returns distance
    )
    {
        // J. Glazer - March 2017

        // approach: if opposite surfaces have opposite azimuth and same area, then check the distance between the
        // vertices( one counting backwards ) and if it is the same distance than assume that it is the same.
        bool foundOppEqual = false;
        for (int iFace = 1; iFace <= zonePoly.NumFaces; ++iFace) {
            auto const &face = zonePoly.Faces(iFace);
            int curSurfNum = face.SurfNum;
            if (state.dataSurface->Surface(curSurfNum).Class == SurfaceClass::Wall) {
                std::vector<int> facesAtAz = listOfFacesFacingAzimuth(state, zonePoly, state.dataSurface->Surface(curSurfNum).Azimuth);
                bool allFacesEquidistant = true;
                oppositeWallArea = 0.;
                for (int curFace : facesAtAz) {
                    int possOppFace = findPossibleOppositeFace(state, zonePoly, curFace);
                    if (possOppFace > 0) { // an opposite fact was found
                        oppositeWallArea += state.dataSurface->Surface(zonePoly.Faces(curFace).SurfNum).Area;
                        if (!areCornersEquidistant(zonePoly, curFace, possOppFace, distanceBetweenOppositeWalls)) {
                            allFacesEquidistant = false;
                            break;
                        }
                    } else {
                        allFacesEquidistant = false;
                        break;
                    }
                }
                if (allFacesEquidistant) {
                    foundOppEqual = true;
                    break; // only need to find the first case where opposite walls are the same
                }
            }
        }
        return foundOppEqual;
    }

    // provides a list of indices of polyhedron faces that are facing a specific azimuth
    std::vector<int> listOfFacesFacingAzimuth(EnergyPlusData &state, DataVectorTypes::Polyhedron const &zonePoly, Real64 const azimuth)
    {
        // J. Glazer - March 2017

        std::vector<int> facingAzimuth;
        facingAzimuth.reserve(zonePoly.NumFaces);

        for (int iFace = 1; iFace <= zonePoly.NumFaces; ++iFace) {
            if (General::rotAzmDiffDeg(state.dataSurface->Surface(zonePoly.Faces(iFace).SurfNum).Azimuth, azimuth) < 1.) {
                facingAzimuth.emplace_back(iFace);
            }
        }
        return facingAzimuth;
    } // listOfFacesFacingAzimuth

    // returns the index of the face of a polyhedron that is probably opposite of the face index provided
    int findPossibleOppositeFace(EnergyPlusData &state, DataVectorTypes::Polyhedron const &zonePoly, int const faceIndex)
    {
        // J. Glazer - March 2017

        int selectedSurNum = zonePoly.Faces(faceIndex).SurfNum;
        Real64 selectedAzimuth = state.dataSurface->Surface(selectedSurNum).Azimuth;
        Real64 oppositeAzimuth = fmod(selectedAzimuth + 180., 360.);
        Real64 selectedArea = state.dataSurface->Surface(selectedSurNum).Area;
        int selectedNumCorners = zonePoly.Faces(faceIndex).NumSides;

        for (int iFace = 1; iFace <= zonePoly.NumFaces; ++iFace) {
            int curSurfNum = zonePoly.Faces(iFace).SurfNum;
            if ((zonePoly.Faces(iFace).NumSides == selectedNumCorners) &&
                (std::abs(state.dataSurface->Surface(curSurfNum).Area - selectedArea) < 0.01) &&
                (std::abs(state.dataSurface->Surface(curSurfNum).Azimuth - oppositeAzimuth) < 1.)) {
                return iFace;
            }
        }
        return -1;
    } // findPossibleOppositeFace()

    // tests if the corners of one face of the polyhedron are the same distance from corners of another face
    bool areCornersEquidistant(DataVectorTypes::Polyhedron const &zonePoly, int const faceIndex, int const opFaceIndex, Real64 &dist)
    {
        // J. Glazer - March 2017

        constexpr Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        bool allAreEquidistant = true;

        dist = -99.;

        auto const &face = zonePoly.Faces(faceIndex);
        auto const &opFace = zonePoly.Faces(opFaceIndex);
        
        if (face.NumSides != opFace.NumSides) // double check that the number of sides match
            return false;
        
        for (int iVertex = 1; iVertex <= face.NumSides; ++iVertex) {
            int iVertexOpp = 1 + face.NumSides - iVertex; // count backwards for opposite face
            Real64 curDistBetwCorners = distance(face.Points(iVertex), opFace.Points(iVertexOpp));
            if (iVertex == 1) {
                dist = curDistBetwCorners;
            } else if (std::abs(curDistBetwCorners - dist) > tol) {
                return false;
            }
        }

        return true;
    } // areCornersEquidistant()

    // returns the index of vertex in a list that is in the same position in space as the given vertex
    int findIndexOfVertex(Vector3<Real64> const &vertexToFind, std::vector<Vector3<Real64>> const &listOfVertices)
    {
        // J. Glazer - March 2017
        for (std::size_t i = 0; i < listOfVertices.size(); i++) {
            if (Vectors::VecEqualTol(listOfVertices[i], vertexToFind, 0.0127)) {
                return i;
            }
        }
        return -1;
    } // findIndexOfVertex()

    Real64 distanceFromPointToLine(Vector3<Real64> const &start, Vector3<Real64> const &end, Vector3<Real64> const &test)
    {
        // np.linalg.norm(np.cross(e-s,p-s)/np.linalg.norm(e-s))
        Vector3<Real64> t = end - start;
        t.normalize(); // Unit vector of start to end

        Vector3<Real64> other = test - start;

        Vector3<Real64> projection = cross(t, other); // normal unit vector, that's the distance component
        return projection.length();
    }

    // tests if a point in space lies on the line segment defined by two other points
    bool isPointOnLineBetweenPoints(Vector3<Real64> const &start, Vector3<Real64> const &end, Vector3<Real64> const &test)
    {
        // J. Glazer - March 2017
        // The tolerance has to be low enough. Take for eg a plenum that has an edge that's 30meters long, you risk adding point from the
        // floor to the roof, cf #7383 compute the shortest distance from the point to the line first to avoid false positive
        Real64 tol = 0.0127;
        if (distanceFromPointToLine(start, end, test) < tol) { // distanceFromPointToLine always positive, it's calculated as norml_L2
            return (std::abs((distance(start, end) - (distance(start, test) + distance(test, end)))) < tol);
        }
        return false;
    }

    bool EdgeOfSurf::operator==(const EdgeOfSurf &other) const
    {
        return ((Vectors::VecEqualTol(this->start, other.start, 0.0127) && Vectors::VecEqualTol(this->end, other.end, 0.0127)) ||
                (Vectors::VecEqualTol(this->start, other.end, 0.0127) && Vectors::VecEqualTol(this->end, other.start, 0.0127)));
    }

    bool EdgeOfSurf::operator!=(const EdgeOfSurf &other) const
    {
        return !(*this == other);
    }

    bool EdgeOfSurf::containsPoints(const Vector3<Real64> &vertex) const
    {
        return (!Vectors::VecEqualTol(this->start, vertex, 0.0127) && !Vectors::VecEqualTol(this->end, vertex, 0.0127) &&
                isPointOnLineBetweenPoints(this->start, this->end, vertex));
    }

    double EdgeOfSurf::length() const
    {
        return distance(this->start, this->end);
    }

    void ProcessSurfaceVertices(EnergyPlusData &state, int const ThisSurf, bool &ErrorsFound)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code (Walton)
        //       DATE WRITTEN   1976
        //       MODIFIED        FW, Mar 2002: Add triangular windows
        //                       FW, May 2002: modify test for 4-sided but non-rectangular subsurfaces
        //                       FW, Sep 2002: add shape for base surfaces (walls and detached shading surfaces)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine processes each surface into the vertex representation used
        // by the shading procedures.

        // METHODOLOGY EMPLOYED:
        // Detached Shading, Base Surfaces, Attached Shading surfaces are represented in the
        // same manner as original.  Subsurfaces (windows, doors) are a "relative coordinate".

        static constexpr std::string_view RoutineName("ProcessSurfaceVertices: ");

        Vector3<Real64> p1;           // Intermediate Result
        Vector3<Real64> LLC;
        int n;               // Vertex Number in Loop
        int ThisBaseSurface; // Current base surface
        SurfaceShape ThisShape(SurfaceShape::None);
        bool BaseSurface; // True if a base surface or a detached shading surface
        Real64 ThisSurfAz;
        Real64 ThisSurfTilt;
        Real64 ThisReveal;
        int FrDivNum;        // Frame/divider number
        Real64 FrWidth;      // Frame width for exterior windows (m)
        Real64 FrArea;       // Frame area for exterior windows(m2)
        Real64 DivWidth;     // Divider width for exterior windows (m)
        Real64 DivArea;      // Divider area for exterior windows (m2)
        Real64 DivFrac;      // Fraction of divider area without overlaps
        bool ErrorInSurface; // false/true, depending on pass through routine
        bool SError;         // Bool used for return value of calls to PlaneEquation
        bool HeatTransSurf;
        bool IsCoPlanar;
        Real64 OutOfLine;
        int LastVertexInError;

        // Object Data
        Vector4<Real64> BasePlane;
        Vector3<Real64> CoordinateTransVector;

        auto &sg = state.dataSurfaceGeometry;
        if (state.dataSurface->Surface(ThisSurf).VerticesProcessed) {
            return;
        }

        ErrorInSurface = false;

        if (sg->ProcessSurfaceVerticesOneTimeFlag) {
            sg->psv.allocate(state.dataSurface->MaxVerticesPerSurface);
            sg->psv = Vector3<Real64>(0.0, 0.0, 0.0);
            sg->ProcessSurfaceVerticesOneTimeFlag = false;
        }

        // Categorize this surface
        auto &surf = state.dataSurface->Surface(ThisSurf);
        BaseSurface = (surf.BaseSurf == 0 || surf.BaseSurf == ThisSurf);

        ThisBaseSurface = surf.BaseSurf; // Dont know if this is still needed or not
        HeatTransSurf = surf.HeatTransSurf;

        // Kludge for daylighting shelves
        if (surf.IsShadowing) {
            ThisBaseSurface = ThisSurf;
            HeatTransSurf = true;
        }

        // IF (Surface(ThisSurf)%Name(1:3) /= 'Mir') THEN
        if (!surf.MirroredSurf) {
            std::tie(IsCoPlanar, OutOfLine) = Vectors::CalcCoPlanarNess(surf.Vertex, surf.Sides, LastVertexInError);
            if (!IsCoPlanar) {
                if (OutOfLine > 0.01) {
                    ShowSevereError(state,
                                    format("{}Suspected non-planar surface:\"{}\", Max \"out of line\"={:.5T} at Vertex # {}",
                                           RoutineName,
                                           surf.Name,
                                           OutOfLine,
                                           LastVertexInError));
                } else {
                    ShowWarningError(state,
                                     format("{}Possible non-planar surface:\"{}\", Max \"out of line\"={:.5T} at Vertex # {}",
                                            RoutineName,
                                            surf.Name,
                                            OutOfLine,
                                            LastVertexInError));
                }
                //       ErrorInSurface=.TRUE.
            }
        }

        if (BaseSurface) {
            for (n = 1; n <= surf.Sides; ++n) {
                sg->psv(n) = surf.Vertex(n);
            }
            surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
            surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
            if (surf.Sides == 3) {
                surf.Shape = SurfaceShape::Triangle;
            } else if (surf.Sides == 4) {
                // Test for rectangularity
                if (isRectangle(state, ThisSurf)) {
                    surf.Shape = SurfaceShape::Rectangle;
                } else {
                    surf.Shape = SurfaceShape::Quadrilateral;
                }
            } else { // Surface( ThisSurf ).Sides > 4
                surf.Shape = SurfaceShape::Polygonal;
                if (std::abs(surf.Height * surf.Width - surf.GrossArea) > 0.001) {
                    surf.Width = std::sqrt(surf.GrossArea);
                    surf.Height = surf.Width;
                }
            }

        } else { // It's a subsurface to previous basesurface in this set of calls

            ThisSurfAz = surf.Azimuth;
            ThisSurfTilt = surf.Tilt;

            // Retrieve base surface info
            auto const &baseSurf = state.dataSurface->Surface(ThisBaseSurface);
            Vector3<Real64> const BaseLLC = baseSurf.Vertex(2);

            if (HeatTransSurf) {

                if (surf.Sides == 4) {
                    ThisShape = SurfaceShape::RectangularDoorWindow;
                } else if (surf.Sides == 3 && surf.Class == SurfaceClass::Window) {
                    ThisShape = SurfaceShape::TriangularWindow;
                } else if (surf.Sides == 3 && surf.Class == SurfaceClass::Door) {
                    ThisShape = SurfaceShape::TriangularDoor;
                } else {
                    assert(false);
                }

            } else { //  this is a shadowing subsurface

                if (std::abs(state.dataSurface->Surface(surf.BaseSurf).Tilt - ThisSurfTilt) <= 0.01) {
                    // left or right fin
                    if (ThisSurfAz < 0.0) ThisSurfAz += 360.0;
                    if (ThisSurfAz > state.dataSurface->Surface(surf.BaseSurf).Azimuth) {
                        ThisShape = SurfaceShape::RectangularLeftFin;
                    } else {
                        ThisShape = SurfaceShape::RectangularRightFin;
                    }
                } else {
                    ThisShape = SurfaceShape::RectangularOverhang;
                }
            }

            // Setting relative coordinates for shadowing calculations for subsurfaces
            switch (ThisShape) {
            case SurfaceShape::RectangularDoorWindow: { // Rectangular heat transfer subsurface
                BasePlane = Vectors::CalcPlaneEquation(state.dataSurface->Surface(surf.BaseSurf).Vertex, state.dataSurface->Surface(surf.BaseSurf).Sides, SError);
                if (SError) {
                    ShowSevereError(state, format("{}Degenerate surface (likely two vertices equal):\"{}\".", RoutineName, surf.Name));
                    ErrorInSurface = true;
                }
                ThisReveal = -Vectors::Pt2Plane(surf.Vertex(2), BasePlane);
                if (std::abs(ThisReveal) < 0.0002) ThisReveal = 0.0;
                surf.Reveal = ThisReveal;

                Vector3<Real64> p = surf.Vertex(2) - BaseLLC;

                LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                LLC.z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
                surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
                surf.Height = length(surf.Vertex(2) - surf.Vertex(1));

                // Processing of 4-sided but non-rectangular Window, Door or GlassDoor, for use in calc of convective air flow.
                if (!isRectangle(state, ThisSurf)) {

                    // Transform the surface into an equivalent rectangular surface with the same area and aspect ratio.
                    MakeEquivalentRectangle(state, ThisSurf, ErrorsFound);

                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state, format("{}Suspected 4-sided but non-rectangular Window, Door or GlassDoor:", RoutineName));
                        ShowContinueError(
                            state,
                            format("Surface={} is transformed into an equivalent rectangular surface with the same area and aspect ratio. ",
                                   surf.Name));
                    }
                }

                sg->psv(1) = {LLC.x, LLC.y + surf.Height, LLC.z};
                sg->psv(2) = {LLC.x, LLC.y, LLC.z};
                sg->psv(3) = {LLC.x + surf.Width, LLC.y, LLC.z};
                sg->psv(4) = {LLC.x + surf.Width, LLC.y + surf.Height, LLC.z};

                if (surf.Class == SurfaceClass::Window && surf.ExtBoundCond == ExternalEnvironment && surf.FrameDivider > 0) {
                    FrDivNum = surf.FrameDivider;
                    // Set flag for calculating beam solar reflection from outside and/or inside window reveal
                    if ((surf.Reveal > 0.0 && state.dataSurface->FrameDivider(FrDivNum).OutsideRevealSolAbs > 0.0) ||
                        (state.dataSurface->FrameDivider(FrDivNum).InsideSillDepth > 0.0 &&
                         state.dataSurface->FrameDivider(FrDivNum).InsideSillSolAbs > 0.0) ||
                        (state.dataSurface->FrameDivider(FrDivNum).InsideReveal > 0.0 &&
                         state.dataSurface->FrameDivider(FrDivNum).InsideRevealSolAbs > 0.0))
                        state.dataHeatBal->CalcWindowRevealReflection = true;

                    // For exterior window with frame, subtract frame area from base surface
                    // (only rectangular windows are allowed to have a frame and/or divider;
                    // Surface(ThisSurf)%FrameDivider will be 0 for triangular windows)
                    FrWidth = state.dataSurface->FrameDivider(FrDivNum).FrameWidth;
                    if (FrWidth > 0.0) {
                        FrArea = (surf.Height + 2.0 * FrWidth) * (surf.Width + 2.0 * FrWidth) - surf.Area / surf.Multiplier;
                        state.dataSurface->SurfWinFrameArea(ThisSurf) = FrArea * surf.Multiplier;
                        if ((state.dataSurface->Surface(surf.BaseSurf).Area - state.dataSurface->SurfWinFrameArea(ThisSurf)) <= 0.0) {
                            ShowSevereError(state, format("{}Base Surface=\"{}\", ", RoutineName, state.dataSurface->Surface(surf.BaseSurf).Name));
                            ShowContinueError(state,
                                              format("Window Surface=\"{}\" area (with frame) is too large to fit on the surface.", surf.Name));
                            ShowContinueError(state,
                                              format("Base surface area (-windows and doors)=[{:.2T}] m2, frame area=[{:.2T}] m2.",
                                                     state.dataSurface->Surface(surf.BaseSurf).Area,
                                                     state.dataSurface->SurfWinFrameArea(ThisSurf)));
                            ErrorInSurface = true;
                        }
                        state.dataSurface->Surface(surf.BaseSurf).Area -= state.dataSurface->SurfWinFrameArea(ThisSurf);
                    }
                    // If exterior window has divider, subtract divider area to get glazed area
                    DivWidth = state.dataSurface->FrameDivider(surf.FrameDivider).DividerWidth;
                    if (DivWidth > 0.0 && !ErrorInSurface) {
                        DivArea = DivWidth * (state.dataSurface->FrameDivider(FrDivNum).HorDividers * surf.Width +
                                              state.dataSurface->FrameDivider(FrDivNum).VertDividers * surf.Height -
                                              state.dataSurface->FrameDivider(FrDivNum).HorDividers *
                                                  state.dataSurface->FrameDivider(FrDivNum).VertDividers * DivWidth);
                        state.dataSurface->SurfWinDividerArea(ThisSurf) = DivArea * surf.Multiplier;
                        if ((surf.Area - state.dataSurface->SurfWinDividerArea(ThisSurf)) <= 0.0) {
                            ShowSevereError(state, format("{}Divider area exceeds glazed opening for window {}", RoutineName, surf.Name));
                            ShowContinueError(state,
                                              format("Window surface area=[{:.2T}] m2, divider area=[{:.2T}] m2.",
                                                     surf.Area,
                                                     state.dataSurface->SurfWinDividerArea(ThisSurf)));
                            ErrorInSurface = true;
                        }
                        surf.Area -= state.dataSurface->SurfWinDividerArea(ThisSurf); // Glazed area
                        if (DivArea <= 0.0) {
                            ShowWarningError(state, format("{}Calculated Divider Area <= 0.0 for Window={}", RoutineName, surf.Name));
                            if (state.dataSurface->FrameDivider(FrDivNum).HorDividers == 0) {
                                ShowContinueError(state, "..Number of Horizontal Dividers = 0.");
                            }
                            if (state.dataSurface->FrameDivider(FrDivNum).VertDividers == 0) {
                                ShowContinueError(state, "..Number of Vertical Dividers = 0.");
                            }
                        } else {
                            auto &surfWin = state.dataSurface->SurfaceWindow(ThisSurf);
                            surfWin.glazedFrac = surf.Area / (surf.Area + state.dataSurface->SurfWinDividerArea(ThisSurf));
                            // Correction factor for portion of divider subject to divider projection correction
                            DivFrac = (1.0 - state.dataSurface->FrameDivider(FrDivNum).HorDividers *
                                                 state.dataSurface->FrameDivider(FrDivNum).VertDividers * pow_2(DivWidth) / DivArea);
                            state.dataSurface->SurfWinProjCorrDivOut(ThisSurf) =
                                DivFrac * state.dataSurface->FrameDivider(FrDivNum).DividerProjectionOut / DivWidth;
                            state.dataSurface->SurfWinProjCorrDivIn(ThisSurf) =
                                DivFrac * state.dataSurface->FrameDivider(FrDivNum).DividerProjectionIn / DivWidth;
                            // Correction factor for portion of frame subject to frame projection correction
                            if (FrWidth > 0.0) {
                                state.dataSurface->SurfWinProjCorrFrOut(ThisSurf) =
                                    (state.dataSurface->FrameDivider(FrDivNum).FrameProjectionOut / FrWidth) *
                                    (surf.Height + surf.Width -
                                     (state.dataSurface->FrameDivider(FrDivNum).HorDividers +
                                      state.dataSurface->FrameDivider(FrDivNum).VertDividers) *
                                         DivWidth) /
                                    (surf.Height + surf.Width + 2 * FrWidth);
                                state.dataSurface->SurfWinProjCorrFrIn(ThisSurf) =
                                    (state.dataSurface->FrameDivider(FrDivNum).FrameProjectionIn / FrWidth) *
                                    (surf.Height + surf.Width -
                                     (state.dataSurface->FrameDivider(FrDivNum).HorDividers +
                                      state.dataSurface->FrameDivider(FrDivNum).VertDividers) *
                                         DivWidth) /
                                    (surf.Height + surf.Width + 2 * FrWidth);
                            }
                        }
                    }
                }
            } break;
            case SurfaceShape::TriangularWindow:
            case SurfaceShape::TriangularDoor: {
                BasePlane = Vectors::CalcPlaneEquation(state.dataSurface->Surface(surf.BaseSurf).Vertex, state.dataSurface->Surface(surf.BaseSurf).Sides, SError);
                if (SError) {
                    ShowSevereError(state, format("{}Degenerate surface (likely two vertices equal):\"{}\".", RoutineName, surf.Name));
                    ErrorInSurface = true;
                }
                ThisReveal = -Vectors::Pt2Plane(surf.Vertex(2), BasePlane);
                if (std::abs(ThisReveal) < 0.0002) ThisReveal = 0.0;
                surf.Reveal = ThisReveal;

                Vector3<Real64> p = surf.Vertex(2) - BaseLLC;

                sg->psv(2).x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                sg->psv(2).y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                sg->psv(2).z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
                surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
                surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
                // Effective height and width of a triangular window for use in calc of convective air flow
                // in gap between glass and shading device when shading device is present
                surf.Height = 4.0 * surf.Area / (3.0 * surf.Width);
                surf.Width *= 0.75;

                p = surf.Vertex(1) - BaseLLC;

                sg->psv(1).x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                sg->psv(1).y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                sg->psv(1).z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;

                p = surf.Vertex(3) - BaseLLC;

                sg->psv(3).x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                sg->psv(3).y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                sg->psv(3).z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
            } break;
            case SurfaceShape::RectangularOverhang: {
                Vector3<Real64> p = surf.Vertex(2) - BaseLLC;
                LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                LLC.z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
                surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
                surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
                sg->psv(1) = {LLC.x, LLC.y, surf.Height};
                sg->psv(2) = {LLC.x, LLC.y, 0.0};
                sg->psv(3) = {LLC.x + surf.Width, LLC.y, 0.0};
                sg->psv(4) = {LLC.x + surf.Width, LLC.y, surf.Height};
            } break;
            case SurfaceShape::RectangularLeftFin: {
                Vector3<Real64> p = surf.Vertex(2) - BaseLLC;
                LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                LLC.z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
                surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
                surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
                sg->psv(1) = {LLC.x, LLC.y, surf.Height};
                sg->psv(2) = {LLC.x, LLC.y, 0.0};
                sg->psv(3) = {LLC.x, LLC.y + surf.Width, 0.0};
                sg->psv(4) = {LLC.x, LLC.y + surf.Width, surf.Height};
            } break;
            case SurfaceShape::RectangularRightFin: {
                Vector3<Real64> p = surf.Vertex(2) - BaseLLC;
                LLC.x = -p.x * baseSurf.CosAzim + p.y * baseSurf.SinAzim;
                LLC.y = -p.x * baseSurf.SinAzim * baseSurf.CosTilt - p.y * baseSurf.CosAzim * baseSurf.CosTilt + p.z * baseSurf.SinTilt;
                LLC.z = p.x * baseSurf.SinAzim * baseSurf.SinTilt + p.y * baseSurf.CosAzim * baseSurf.SinTilt + p.z * baseSurf.CosTilt;
                surf.Width = length(surf.Vertex(3) - surf.Vertex(2));
                surf.Height = length(surf.Vertex(2) - surf.Vertex(1));
                sg->psv(1) = {LLC.x, LLC.y + surf.Width, surf.Height};
                sg->psv(2) = {LLC.x, LLC.y + surf.Width, 0.0};
                sg->psv(3) = {LLC.x, LLC.y, 0.0};
                sg->psv(4) = {LLC.x, LLC.y, surf.Height};
            } break;
            default: {
                // Error Condition
                ShowSevereError(state, format("{}Incorrect surface shape number.", RoutineName), OptionalOutputFileRef{state.files.eso});
                ShowContinueError(state, "Please notify EnergyPlus support of this error and send input file.");
                ErrorInSurface = true;
            } break;
            }

            for (n = 1; n <= surf.Sides; ++n) {
                // if less than 1/10 inch
                auto &psv = sg->psv(n);
                    
                psv.x = nint64(10000.0 * psv.x) / 10000.0;
                if (std::abs(psv.x) < 0.0025) psv.x = 0.0;
                psv.y = nint64(10000.0 * psv.y) / 10000.0;
                if (std::abs(psv.y) < 0.0025) psv.y = 0.0;
                psv.z = nint64(10000.0 * psv.y) / 10000.0;
                if (std::abs(psv.z) < 0.0025) psv.z = 0.0;
            }

            surf.Shape = ThisShape;

        } // End of check if ThisSurf is a base surface

        if (ErrorInSurface) {
            ErrorsFound = true;
            return;
        }

        // Transfer to XV,YV,ZV arrays
        auto &shadeV = state.dataSurface->ShadeV(ThisSurf);
        
        shadeV.NVert = surf.Sides;
        shadeV.V.allocate(surf.Sides);

        for (int n = 1; n <= surf.Sides; ++n) {
            shadeV.V(n) = sg->psv(n);
        }

        // Process Surfaces According to Type of Coordinate Origin.
        if (BaseSurface) {

            // General Surfaces:
            CalcCoordinateTransformation(state, ThisSurf, CoordinateTransVector); // X00,Y00,Z00,X,Y,Z,A)    ! Compute Coordinate Transformation

            // RECORD DIRECTION COSINES.
            if (HeatTransSurf) { // This is a general surface but not detached shading surface

                auto &baseSurf = state.dataSurface->Surface(ThisBaseSurface);
                // RECORD COORDINATE TRANSFORMATION FOR BASE SURFACES.
                state.dataSurface->T0(ThisBaseSurface) = CoordinateTransVector;
                // COMPUTE INVERSE TRANSFORMATION.
                Vector3<Real64> p1 = sg->psv(2) - CoordinateTransVector;

                // Store the relative coordinate shift values for later use by any subsurfaces
                baseSurf.XShift = dot(baseSurf.lcsx, p1);
                baseSurf.YShift = dot(baseSurf.lcsy, p1);
                baseSurf.VerticesProcessed = true;
            }

            // SUBSURFACES: (Surface(ThisSurf)%BaseSurf /= ThisSurf)
        } else {
            // WINDOWS OR DOORS:
            auto &baseSurf = state.dataSurface->Surface(ThisBaseSurface);
                
            // SHIFT RELATIVE COORDINATES FROM LOWER LEFT CORNER TO ORIGIN DEFINED
            // BY CTRAN AND SET DIRECTION COSINES SAME AS BASE SURFACE.
            if (!baseSurf.VerticesProcessed) {

                if (surf.IsAirBoundarySurf) {
                    ProcessSurfaceVertices(state, ThisBaseSurface, ErrorsFound);
                } else {

                    ShowSevereError(state, format("{}Developer error for Subsurface={}", RoutineName, surf.Name));
                    ShowContinueError(state,
                                      format("Base surface={} vertices must be processed before any subsurfaces.",
                                             baseSurf.Name));
                    ShowFatalError(state, std::string(RoutineName));
                }
            }

            for (int n = 1; n <= surf.Sides; ++n) {
                shadeV.V(n).x += baseSurf.XShift;
                shadeV.V(n).y += baseSurf.YShift;
            }
        }

        if (ErrorInSurface) {
            ErrorsFound = true;
        }
    } // ProcessSurfaceVertices()

    void CalcCoordinateTransformation(EnergyPlusData &state,
                                      int const SurfNum,            // Surface Number
                                      Vector3<Real64> &CompCoordTranslVector // Coordinate Translation Vector
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton, BLAST
        //       DATE WRITTEN   August 1976
        //       MODIFIED       LKL, May 2004 -- >4 sided polygons
        //       RE-ENGINEERED  Yes

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine develops a coordinate transformation such that the X-axis goes
        // through points 2 and 3 and the Y-axis goes through point 1
        // of a plane figure in 3-d space.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // 'NECAP' - NASA'S Energy-Cost Analysis Program

        // Using/Aliasing
        using namespace Vectors;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Determine Components of the Coordinate Translation Vector.
        auto const &surf = state.dataSurface->Surface(SurfNum);

        Vector3<Real64> x21 = surf.Vertex(2) - surf.Vertex(1);
        Vector3<Real64> x23 = surf.Vertex(2) - surf.Vertex(3);

        Real64 DotSelfX23 = magnitude_squared(x23);

        if (DotSelfX23 <= .1e-6) {
            ShowSevereError(state, format("CalcCoordinateTransformation: Invalid dot product, surface=\"{}\":", surf.Name));
            for (int I = 1; I <= surf.Sides; ++I) {
                auto const &point = surf.Vertex(I);
                ShowContinueError(state, format(" ({:8.3F},{:8.3F},{:8.3F})", point.x, point.y, point.z));
            }
            ShowFatalError(
                state, "CalcCoordinateTransformation: Program terminates due to preceding condition.", OptionalOutputFileRef{state.files.eso});
            return;
        }

        Real64 Gamma = dot(x21, x23) / magnitude_squared(x23);

        CompCoordTranslVector = surf.Vertex(2) + Gamma * (surf.Vertex(3) - surf.Vertex(2));
    }

    void CreateShadedWindowConstruction(EnergyPlusData &state,
                                        int const SurfNum,          // Surface number
                                        int const WSCPtr,           // Pointer to WindowShadingControl for SurfNum
                                        int const ShDevNum,         // Shading device material number for WSCptr
                                        int const shadeControlIndex // index to the Surface().windowShadingControlList,
                                                                    // Surface().shadedConstructionList, and Surface().shadedStormWinConstructionList
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Nov 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Creates a shaded window construction for windows whose WindowShadingControl
        // has a shading device specified instead of a shaded construction

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string ShDevName;    // Shading device material name
        std::string ConstrNameSh; // Shaded construction name
        int TotLayersOld;         // Total layers in old (unshaded) construction
        int TotLayersNew;         // Total layers in new (shaded) construction
        //  INTEGER :: loop                            ! DO loop index

        auto &sg = state.dataSurfaceGeometry;
        auto &surf = sg->SurfaceTmp(SurfNum);
        ShDevName = state.dataMaterial->Material(ShDevNum)->Name;
        int ConstrNum = surf.Construction;
        auto const &constr = state.dataConstruction->Construct(ConstrNum);

        if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->WindowShadingControl(WSCPtr).ShadingType)) {
            ConstrNameSh = format("{}:{}:INT", constr.Name, ShDevName);
        } else {
            ConstrNameSh = format("{}:{}:EXT", constr.Name, ShDevName);
        }

        // If this construction name already exists, set the surface's shaded construction number to it

        int ConstrNewSh = Util::FindItemInList(ConstrNameSh, state.dataConstruction->Construct);

        if (ConstrNewSh > 0) {
            surf.shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            surf.activeShadedConstruction = ConstrNewSh; // set the active to the current for now
        } else {
            // Create new construction
            ConstrNewSh = state.dataHeatBal->TotConstructs + 1;
            surf.shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            surf.activeShadedConstruction = ConstrNewSh; // set the active to the current for now
            state.dataHeatBal->TotConstructs = ConstrNewSh;
            state.dataConstruction->Construct.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalRforNominalUCalculation.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalRforNominalUCalculation(state.dataHeatBal->TotConstructs) = 0.0;
            state.dataHeatBal->NominalU.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalU(state.dataHeatBal->TotConstructs) = 0.0;
            state.dataHeatBal->NominalUBeforeAdjusted.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->CoeffAdjRatio.redimension(state.dataHeatBal->TotConstructs) = 1.0;

            state.dataConstruction->Construct(state.dataHeatBal->TotConstructs).setArraysBasedOnMaxSolidWinLayers(state);

            TotLayersOld = constr.TotLayers;
            TotLayersNew = TotLayersOld + 1;

            state.dataConstruction->Construct(ConstrNewSh).LayerPoint = 0;

            auto const *thisMaterialSh = dynamic_cast<const Material::MaterialChild *>(state.dataMaterial->Material(ShDevNum));
            auto &thisConstructNewSh = state.dataConstruction->Construct(ConstrNewSh);
            if (state.dataSurface->WindowShadingControl(WSCPtr).ShadingType == WinShadingType::IntShade ||
                state.dataSurface->WindowShadingControl(WSCPtr).ShadingType == WinShadingType::IntBlind) {
                // Interior shading device
                thisConstructNewSh.LayerPoint({1, TotLayersOld}) = state.dataConstruction->Construct(ConstrNum).LayerPoint({1, TotLayersOld});
                thisConstructNewSh.LayerPoint(TotLayersNew) = ShDevNum;
                thisConstructNewSh.InsideAbsorpSolar = thisMaterialSh->AbsorpSolar;
                auto const *thisMaterialShLayer1 = dynamic_cast<const Material::MaterialChild *>(
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1)));
                thisConstructNewSh.OutsideAbsorpSolar = thisMaterialShLayer1->AbsorpSolar;
                thisConstructNewSh.OutsideAbsorpThermal = thisMaterialShLayer1->AbsorpThermalFront;
            } else {
                // Exterior shading device
                thisConstructNewSh.LayerPoint(1) = ShDevNum;
                thisConstructNewSh.LayerPoint({2, TotLayersNew}) = constr.LayerPoint({1, TotLayersOld});
                auto const *thisMaterialShInside = dynamic_cast<const Material::MaterialChild *>(
                    state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(TotLayersNew)));
                thisConstructNewSh.InsideAbsorpSolar = thisMaterialShInside->AbsorpSolar;
                thisConstructNewSh.OutsideAbsorpSolar = thisMaterialSh->AbsorpSolar;
                thisConstructNewSh.OutsideAbsorpThermal = thisMaterialSh->AbsorpThermalFront;
            }
            // The following InsideAbsorpThermal applies only to inside glass; it is corrected
            //  later in InitGlassOpticalCalculations if construction has inside shade or blind.
            thisConstructNewSh.InsideAbsorpThermal =
                dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(constr.LayerPoint(TotLayersOld)))->AbsorpThermalBack;
            thisConstructNewSh.OutsideRoughness = Material::Roughness::VerySmooth;
            thisConstructNewSh.DayltPropPtr = 0;
            thisConstructNewSh.CTFCross.fill(0.0);
            thisConstructNewSh.CTFFlux.fill(0.0);
            thisConstructNewSh.CTFInside.fill(0.0);
            thisConstructNewSh.CTFOutside.fill(0.0);
            thisConstructNewSh.CTFSourceIn.fill(0.0);
            thisConstructNewSh.CTFSourceOut.fill(0.0);
            thisConstructNewSh.CTFTimeStep = 0.0;
            thisConstructNewSh.CTFTSourceOut.fill(0.0);
            thisConstructNewSh.CTFTSourceIn.fill(0.0);
            thisConstructNewSh.CTFTSourceQ.fill(0.0);
            thisConstructNewSh.CTFTUserOut.fill(0.0);
            thisConstructNewSh.CTFTUserIn.fill(0.0);
            thisConstructNewSh.CTFTUserSource.fill(0.0);
            thisConstructNewSh.NumHistories = 0;
            thisConstructNewSh.NumCTFTerms = 0;
            thisConstructNewSh.UValue = 0.0;
            thisConstructNewSh.SourceSinkPresent = false;
            thisConstructNewSh.SolutionDimensions = 0;
            thisConstructNewSh.SourceAfterLayer = 0;
            thisConstructNewSh.TempAfterLayer = 0;
            thisConstructNewSh.ThicknessPerpend = 0.0;
            thisConstructNewSh.AbsDiff = 0.0;
            thisConstructNewSh.AbsDiffBack = 0.0;
            thisConstructNewSh.AbsDiffShade = 0.0;
            thisConstructNewSh.AbsDiffBackShade = 0.0;
            thisConstructNewSh.ShadeAbsorpThermal = 0.0;
            thisConstructNewSh.AbsBeamShadeCoef = 0.0;
            thisConstructNewSh.TransDiff = 0.0;
            thisConstructNewSh.TransDiffVis = 0.0;
            thisConstructNewSh.ReflectSolDiffBack = 0.0;
            thisConstructNewSh.ReflectSolDiffFront = 0.0;
            thisConstructNewSh.ReflectVisDiffBack = 0.0;
            thisConstructNewSh.ReflectVisDiffFront = 0.0;
            thisConstructNewSh.TransSolBeamCoef = 0.0;
            thisConstructNewSh.TransVisBeamCoef = 0.0;
            thisConstructNewSh.ReflSolBeamFrontCoef = 0.0;
            thisConstructNewSh.ReflSolBeamBackCoef = 0.0;
            thisConstructNewSh.W5FrameDivider = 0;
            thisConstructNewSh.FromWindow5DataFile = false;

            thisConstructNewSh.Name = ConstrNameSh;
            thisConstructNewSh.TotLayers = TotLayersNew;
            thisConstructNewSh.TotSolidLayers = constr.TotSolidLayers + 1;
            thisConstructNewSh.TotGlassLayers = constr.TotGlassLayers;
            thisConstructNewSh.TypeIsWindow = true;
            thisConstructNewSh.IsUsed = true;

            for (int Layer = 1; Layer <= state.dataHeatBal->MaxSolidWinLayers; ++Layer) {
                for (int index = 1; index <= DataSurfaces::MaxPolyCoeff; ++index) {
                    thisConstructNewSh.AbsBeamCoef(Layer)(index) = 0.0;
                    thisConstructNewSh.AbsBeamBackCoef(Layer)(index) = 0.0;
                }
            }
        }
    } // CreateShadedWindowConstruction()

    void CreateStormWindowConstructions(EnergyPlusData &state)
    {
        // For windows with an associated StormWindow object, creates a construction
        // consisting of the base construction plus a storm window and air gap on the outside.
        // If the window has an interior or between-glass shade/blind, also creates a
        // construction consisting of the storm window added to the shaded construction.
        DisplayString(state, "Creating Storm Window Constructions");

        for (int StormWinNum = 1; StormWinNum <= state.dataSurface->TotStormWin; ++StormWinNum) {
            auto &stormWin = state.dataSurface->StormWindow(StormWinNum);
                
            int SurfNum = stormWin.BaseWindowNum; // Surface number
            auto &surf = state.dataSurface->Surface(SurfNum);
            int ConstrNum = surf.Construction; // Number of unshaded construction
            // Fatal error if base construction has more than three glass layers
            if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers > 3) {
                ShowFatalError(state, format("Window={} has more than 3 glass layers; a storm window cannot be applied.", surf.Name));
            }

            // create unshaded construction with storm window
            std::string ConstrNameSt = format("BARECONSTRUCTIONWITHSTORMWIN:{}", StormWinNum); // Name of unshaded construction with storm window
            // If this construction name already exists, set the surface's storm window construction number to it
            int ConstrNewSt = Util::FindItemInList(ConstrNameSt,
                                                   state.dataConstruction->Construct,
                                                   state.dataHeatBal->TotConstructs); // Number of unshaded storm window construction that is created
            // If necessary, create new material corresponding to the air layer between the storm winddow and the rest of the window
            int MatNewStAir = createAirMaterialFromDistance(state, stormWin.StormWinDistance, "AIR:STORMWIN:");
            if (ConstrNewSt == 0) {
                ConstrNewSt = createConstructionWithStorm(state, ConstrNum, ConstrNameSt, stormWin.StormWinMaterialNum, MatNewStAir);
            }
            state.dataSurface->SurfWinStormWinConstr(SurfNum) = ConstrNewSt;

            // create shaded constructions with storm window
            surf.shadedStormWinConstructionList.resize(surf.shadedConstructionList.size(),
                                                       0); // make the shaded storm window size the same size as the number of shaded constructions
            for (std::size_t iConstruction = 0; iConstruction < surf.shadedConstructionList.size(); ++iConstruction) {
                int curConstrNum = surf.shadedConstructionList[iConstruction];
                auto &curConstr = state.dataConstruction->Construct(curConstrNum);
                // Set ShAndSt, which is true if the window has a shaded construction to which a storm window
                // can be added. (A storm window can be added if there is an interior shade or blind and up to three
                // glass layers, or there is a between-glass shade or blind and two glass layers.)
                bool ShAndSt = false; // True if unshaded and shaded window can have a storm window
                std::string ConstrNameSh = curConstr.Name; // Name of original shaded window construction
                int TotLayers = curConstr.TotLayers;       // Total layers in a construction
                int MatIntSh = curConstr.LayerPoint(TotLayers); // Material number of interior shade or blind
                int MatBetweenGlassSh = 0; // Material number of between-glass shade or blind
                if (TotLayers == 5) MatBetweenGlassSh = curConstr.LayerPoint(3);
                if (curConstr.TotGlassLayers <= 3 &&
                    (state.dataMaterial->Material(MatIntSh)->group == Material::Group::Shade ||
                     state.dataMaterial->Material(MatIntSh)->group == Material::Group::WindowBlind))
                    ShAndSt = true;
                if (MatBetweenGlassSh > 0) {
                    if (state.dataMaterial->Material(MatBetweenGlassSh)->group == Material::Group::Shade ||
                        state.dataMaterial->Material(MatBetweenGlassSh)->group == Material::Group::WindowBlind) {
                        ShAndSt = true;
                    } else {
                        ShowContinueError(state, format("Window={} has a shaded construction to which a storm window cannot be applied.", surf.Name));
                        ShowContinueError(state, "Storm windows can only be applied to shaded constructions that:");
                        ShowContinueError(state, "have an interior shade or blind and up to three glass layers, or");
                        ShowContinueError(state, "have a between-glass shade or blind and two glass layers.");
                        ShowFatalError(state, "EnergyPlus is exiting due to reason stated above.");
                    }
                }
                if (ShAndSt) {
                    auto &surf = state.dataSurface->Surface(SurfNum);
                    // Shouldn't this be curConstrNum instead of iConstruction?
                    std::string ConstrNameStSh = format("SHADEDCONSTRUCTIONWITHSTORMWIN:{}:{}", state.dataConstruction->Construct(iConstruction).Name, StormWinNum); // Name of shaded construction with storm window
                    int ConstrNewStSh = createConstructionWithStorm(state, ConstrNum, ConstrNameStSh, stormWin.StormWinMaterialNum, MatNewStAir);
                    surf.shadedStormWinConstructionList[iConstruction] = ConstrNewStSh; // put in same index as the shaded constuction
                }
            } // end of loop for shaded constructions
        }     // end of loop over storm window objects
    } // CreateStormWindowConstructions()

    int createAirMaterialFromDistance(EnergyPlusData &state, Real64 distance, std::string_view namePrefix)
    {
        int mmDistance = int(1000 * distance); // Thickness of air gap in mm (usually between storm window and rest of window)
        std::string MatNameStAir = format("{}{}MM", namePrefix, mmDistance); // Name of created air layer material
        int newAirMaterial = Util::FindItemInPtrList(MatNameStAir, state.dataMaterial->Material, state.dataMaterial->TotMaterials);
        if (newAirMaterial == 0) {
            // Create new material
            state.dataMaterial->TotMaterials = state.dataMaterial->TotMaterials + 1;
            newAirMaterial = state.dataMaterial->TotMaterials;
            auto *thisMaterial = new Material::MaterialChild;
            state.dataMaterial->Material.push_back(thisMaterial);
            state.dataHeatBal->NominalR.redimension(state.dataMaterial->TotMaterials);
            thisMaterial->Name = MatNameStAir;
            thisMaterial->group = Material::Group::WindowGas;
            thisMaterial->roughness = Material::Roughness::MediumRough;
            thisMaterial->Conductivity = 0.0;
            thisMaterial->Density = 0.0;
            thisMaterial->IsoMoistCap = 0.0;
            thisMaterial->Porosity = 0.0;
            thisMaterial->Resistance = 0.0;
            thisMaterial->SpecHeat = 0.0;
            thisMaterial->ThermGradCoef = 0.0;
            thisMaterial->Thickness = distance;
            thisMaterial->VaporDiffus = 0.0;
            thisMaterial->gasTypes = Material::GasType::Custom;
            thisMaterial->GasCon = 0.0;
            thisMaterial->GasVis = 0.0;
            thisMaterial->GasCp = 0.0;
            thisMaterial->GasWght = 0.0;
            thisMaterial->GasFract = 0.0;
            thisMaterial->gasTypes(1) = Material::GasType::Air;
            thisMaterial->GlassSpectralDataPtr = 0;
            thisMaterial->NumberOfGasesInMixture = 1;
            thisMaterial->GasCon(1, 1) = 2.873e-3;
            thisMaterial->GasCon(2, 1) = 7.760e-5;
            thisMaterial->GasVis(1, 1) = 3.723e-6;
            thisMaterial->GasVis(2, 1) = 4.940e-8;
            thisMaterial->GasCp(1, 1) = 1002.737;
            thisMaterial->GasCp(2, 1) = 1.2324e-2;
            thisMaterial->GasWght(1) = 28.97;
            thisMaterial->GasFract(1) = 1.0;
            thisMaterial->AbsorpSolar = 0.0;
            thisMaterial->AbsorpThermal = 0.0;
            thisMaterial->AbsorpVisible = 0.0;
            thisMaterial->Trans = 0.0;
            thisMaterial->TransVis = 0.0;
            thisMaterial->GlassTransDirtFactor = 0.0;
            thisMaterial->ReflectShade = 0.0;
            thisMaterial->ReflectShadeVis = 0.0;
            thisMaterial->AbsorpThermalBack = 0.0;
            thisMaterial->AbsorpThermalFront = 0.0;
            thisMaterial->ReflectSolBeamBack = 0.0;
            thisMaterial->ReflectSolBeamFront = 0.0;
            thisMaterial->ReflectSolDiffBack = 0.0;
            thisMaterial->ReflectSolDiffFront = 0.0;
            thisMaterial->ReflectVisBeamBack = 0.0;
            thisMaterial->ReflectVisBeamFront = 0.0;
            thisMaterial->ReflectVisDiffBack = 0.0;
            thisMaterial->ReflectVisDiffFront = 0.0;
            thisMaterial->TransSolBeam = 0.0;
            thisMaterial->TransThermal = 0.0;
            thisMaterial->TransVisBeam = 0.0;
            thisMaterial->BlindDataPtr = 0;
            thisMaterial->WinShadeToGlassDist = 0.0;
            thisMaterial->WinShadeTopOpeningMult = 0.0;
            thisMaterial->WinShadeBottomOpeningMult = 0.0;
            thisMaterial->WinShadeLeftOpeningMult = 0.0;
            thisMaterial->WinShadeRightOpeningMult = 0.0;
            thisMaterial->WinShadeAirFlowPermeability = 0.0;
        }
        return (newAirMaterial);
    } // createAirMaterialFromDistance()

    // create a new construction with storm based on an old construction and storm and gap materials
    int createConstructionWithStorm(EnergyPlusData &state, int oldConstruction, std::string const &name, int stormMaterial, int gapMaterial)
    {
        int newConstruct = Util::FindItemInList(name, state.dataConstruction->Construct); // Number of shaded storm window construction that is created
        if (newConstruct == 0) {
            state.dataHeatBal->TotConstructs = state.dataHeatBal->TotConstructs + 1;
            newConstruct = state.dataHeatBal->TotConstructs;
            state.dataConstruction->Construct.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalRforNominalUCalculation.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalU.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->NominalUBeforeAdjusted.redimension(state.dataHeatBal->TotConstructs);
            state.dataHeatBal->CoeffAdjRatio.redimension(state.dataHeatBal->TotConstructs) = 1.0;

            auto &thisConstruct = state.dataConstruction->Construct(state.dataHeatBal->TotConstructs);
            // these Construct arrays dimensioned based on MaxSolidWinLayers
            thisConstruct.setArraysBasedOnMaxSolidWinLayers(state);

            int TotLayersOld = state.dataConstruction->Construct(oldConstruction).TotLayers;
            thisConstruct.LayerPoint({1, Construction::MaxLayersInConstruct}) = 0;
            thisConstruct.LayerPoint(1) = stormMaterial;
            thisConstruct.LayerPoint(2) = gapMaterial;
            thisConstruct.LayerPoint({3, TotLayersOld + 2}) = state.dataConstruction->Construct(oldConstruction).LayerPoint({1, TotLayersOld});
            thisConstruct.Name = name;
            thisConstruct.TotLayers = TotLayersOld + 2;
            thisConstruct.TotSolidLayers = state.dataConstruction->Construct(oldConstruction).TotSolidLayers + 1;
            thisConstruct.TotGlassLayers = state.dataConstruction->Construct(oldConstruction).TotGlassLayers + 1;
            thisConstruct.TypeIsWindow = true;
            thisConstruct.InsideAbsorpVis = 0.0;
            thisConstruct.OutsideAbsorpVis = 0.0;
            thisConstruct.InsideAbsorpSolar = 0.0;
            thisConstruct.OutsideAbsorpSolar = 0.0;
            thisConstruct.InsideAbsorpThermal = state.dataConstruction->Construct(oldConstruction).InsideAbsorpThermal;
            thisConstruct.OutsideAbsorpThermal =
                dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(stormMaterial))->AbsorpThermalFront;
            thisConstruct.OutsideRoughness = Material::Roughness::VerySmooth;
            thisConstruct.DayltPropPtr = 0;
            thisConstruct.CTFCross.fill(0.0);
            thisConstruct.CTFFlux.fill(0.0);
            thisConstruct.CTFInside.fill(0.0);
            thisConstruct.CTFOutside.fill(0.0);
            thisConstruct.CTFSourceIn.fill(0.0);
            thisConstruct.CTFSourceOut.fill(0.0);
            thisConstruct.CTFTimeStep = 0.0;
            thisConstruct.CTFTSourceOut.fill(0.0);
            thisConstruct.CTFTSourceIn.fill(0.0);
            thisConstruct.CTFTSourceQ.fill(0.0);
            thisConstruct.CTFTUserOut.fill(0.0);
            thisConstruct.CTFTUserIn.fill(0.0);
            thisConstruct.CTFTUserSource.fill(0.0);
            thisConstruct.NumHistories = 0;
            thisConstruct.NumCTFTerms = 0;
            thisConstruct.UValue = 0.0;
            thisConstruct.SourceSinkPresent = false;
            thisConstruct.SolutionDimensions = 0;
            thisConstruct.SourceAfterLayer = 0;
            thisConstruct.TempAfterLayer = 0;
            thisConstruct.ThicknessPerpend = 0.0;
            thisConstruct.AbsDiffIn = 0.0;
            thisConstruct.AbsDiffOut = 0.0;
            thisConstruct.AbsDiff = 0.0;
            thisConstruct.AbsDiffBack = 0.0;
            thisConstruct.AbsDiffShade = 0.0;
            thisConstruct.AbsDiffBackShade = 0.0;
            thisConstruct.ShadeAbsorpThermal = 0.0;
            thisConstruct.AbsBeamShadeCoef = 0.0;
            thisConstruct.TransDiff = 0.0;
            thisConstruct.TransDiffVis = 0.0;
            thisConstruct.ReflectSolDiffBack = 0.0;
            thisConstruct.ReflectSolDiffFront = 0.0;
            thisConstruct.ReflectVisDiffBack = 0.0;
            thisConstruct.ReflectVisDiffFront = 0.0;
            thisConstruct.TransSolBeamCoef = 0.0;
            thisConstruct.TransVisBeamCoef = 0.0;
            thisConstruct.ReflSolBeamFrontCoef = 0.0;
            thisConstruct.ReflSolBeamBackCoef = 0.0;
            thisConstruct.W5FrameDivider = 0;
            thisConstruct.FromWindow5DataFile = false;
            thisConstruct.W5FileMullionWidth = 0.0;
            thisConstruct.W5FileMullionOrientation = DataWindowEquivalentLayer::Orientation::Invalid;
            thisConstruct.W5FileGlazingSysWidth = 0.0;
            thisConstruct.W5FileGlazingSysHeight = 0.0;
            for (int Layer = 1; Layer <= state.dataHeatBal->MaxSolidWinLayers; ++Layer) {
                for (int index = 1; index <= DataSurfaces::MaxPolyCoeff; ++index) {
                    thisConstruct.AbsBeamCoef(Layer)(index) = 0.0;
                    thisConstruct.AbsBeamBackCoef(Layer)(index) = 0.0;
                }
            }
        }
        return (newConstruct);
    } // createConstructionWithStorm()

    void ModifyWindow(EnergyPlusData &state,
                      int const SurfNum,    // SurfNum has construction of glazing system from Window5 Data File;
                      bool &ErrorsFound,    // Set to true if errors found
                      int &AddedSubSurfaces // Subsurfaces added when window references a
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Feb 2002
        //       MODIFIED       June 2004, FCW: SinAzim, CosAzim, SinTilt, CosTilt, OutNormVec, GrossArea
        //                       and Perimeter weren't being set for created window for case when
        //                       window from Window5DataFile had two different glazing systems. Also,
        //                       GrossArea and Perimeter of original window were not being recalculated.
        //                      October 2007, LKL: Net area for shading calculations was not being
        //                       recalculated.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // If a window from the Window5DataFile has one glazing system, modify the
        // vertex coordinates of the original window to correspond to the dimensions
        // of the glazing system on the Data File.
        // If a window from the Window5DataFile has two different glazing systems, split
        // the window into two separate windows with different properties and adjust the
        // vertices of these windows taking into account the dimensions of the glazing systems
        // on the Data File and the width and orientation of the mullion that separates
        // the glazing systems.

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // If there is a second glazing systme on the Data File, SurfNum+1
        // has the construction of the second glazing system.

        // 2-glazing system Window5 data file entry
        auto &sg = state.dataSurfaceGeometry;
        auto &surf = sg->SurfaceTmp(SurfNum);
        // Object Data
        Array1D<Vector3<Real64>> OriginalCoord(surf.Sides);

        int IConst = surf.Construction;

        // Height and width of original window
        Real64 W = length(surf.Vertex(3) - surf.Vertex(2));
        Real64 H = length(surf.Vertex(2) - surf.Vertex(1));

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord({1, surf.Sides}) = surf.Vertex({1, surf.Sides});

        auto const &constr = state.dataConstruction->Construct(IConst);
        // Height and width of first glazing system
        Real64 h1 = constr.W5FileGlazingSysHeight;
        Real64 w1 = constr.W5FileGlazingSysWidth;

        std::string Const2Name = constr.Name + ":2";
        int IConst2 = Util::FindItemInList(Const2Name, state.dataConstruction->Construct);

        if (IConst2 == 0) { // Only one glazing system on Window5 Data File for this window.

            // So... original dimensions and area of window are used (entered in IDF)
            // Warning if dimensions of original window differ from those on Data File by more than 10%

            if (std::abs((H - h1) / H) > 0.10 || std::abs((W - w1) / W) > 0.10) {

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     format("SurfaceGeometry: ModifyWindow: Window {} uses the Window5 Data File Construction {}",
                                            surf.Name,
                                            constr.Name));
                    ShowContinueError(state, format("The height {:.3R}(m) or width  (m) of this window differs by more than 10%{:.3R}", H, W));
                    ShowContinueError(state,
                                      format("from the corresponding height {:.3R} (m) or width  (m) on the Window5 Data file.{:.3R}", h1, w1));
                    ShowContinueError(state, "This will affect the frame heat transfer calculation if the frame in the Data File entry");
                    ShowContinueError(state, "is not uniform, i.e., has sections with different geometry and/or thermal properties.");
                } else {
                    ++sg->Warning1Count;
                }
            }

            auto &baseSurf = sg->SurfaceTmp(surf.BaseSurf);
            // Calculate net area for base surface
            baseSurf.Area -= surf.Area;
            if (baseSurf.Area <= 0.0) {
                ShowSevereError(state, format("Subsurfaces have too much area for base surface={}", baseSurf.Name));
                ShowContinueError(state, format("Subsurface creating error={}", surf.Name));
                ErrorsFound = true;
            }

            // Net area of base surface with unity window multipliers (used in shadowing checks)
            baseSurf.NetAreaShadowCalc -= surf.Area / surf.Multiplier;

        } else { // Two glazing systems on Window5 data file for this window

            // if exterior window, okay.

            if (surf.ExtBoundCond == ExternalEnvironment) {
                // There are two glazing systems (separated by a vertical or horizontal mullion) on the Window5 Data File.
                // Fill in geometry data for the second window (corresponding to the second glazing system on the data file.
                // The first glazing system is assumed to be at left for vertical mullion, at bottom for horizontal mullion.
                // The second glazing system is assumed to be at right for vertical mullion, at top for horizontal mullion.
                // The lower left-hand corner of the original window (its vertex #2) is assumed to coincide with
                // vertex #2 of the first glazing system.

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowMessage(state,
                                format("SurfaceGeometry: ModifyWindow: Window {} has been replaced with the Window 5/6 two glazing system=\"{}\".",
                                       surf.Name,
                                       constr.Name));
                    ShowContinueError(state, "Note that originally entered dimensions are overridden.");
                } else {
                    ++sg->Warning2Count;
                }

                // Allocate another window
                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else if (surf.ExtBoundCond > 0) { // Interior window, specified  ! not external environment

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(
                        state,
                        format("SurfaceGeometry: ModifyWindow: Interior Window {} has been replaced with the Window 5/6 two glazing system=\"{}\".",
                               surf.Name,
                               constr.Name));
                    ShowContinueError(
                        state, "Please check to make sure interior window is correct. Note that originally entered dimensions are overridden.");
                } else {
                    ++sg->Warning3Count;
                }

                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else { // Interior window, specified not entered

                ShowSevereError(state,
                                format("SurfaceGeometry: ModifyWindow: Interior Window {} is a window in an adjacent zone.",
                                       surf.Name));
                ShowContinueError(state, format("Attempted to add/reverse Window 5/6 multiple glazing system=\"{}\".", constr.Name));
                ShowContinueError(state, "Cannot use these Window 5/6 constructs for these Interior Windows. Program will terminate.");
                ErrorsFound = true;
            }

        } // End of check if one or two glazing systems are on the Window5 Data File
    } // ModifyWindow()

    void AddWindow(EnergyPlusData &state,
                   int const SurfNum,    // SurfNum has construction of glazing system from Window5 Data File;
                   bool &ErrorsFound,    // Set to true if errors found
                   int &AddedSubSurfaces // Subsurfaces added when window references a
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   Nov 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine is called from ModifyWindow to add a window.  Allows it to be
        // called in more than one place in the calling routine so as to be able to have
        // specific warnings or errors issued.

        // Using/Aliasing

        using namespace Vectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // If there is a second glazing systme on the Data File, SurfNum+1
        // has the construction of the second glazing system.

        // 2-glazing system Window5 data file entry

        // Object Data
        Array1D<Vector3<Real64>> NewCoord(4);
        Array1D<Vector3<Real64>> OriginalCoord(4);

        auto &sg = state.dataSurfaceGeometry;
        auto &surf = sg->SurfaceTmp(SurfNum);

        int IConst = surf.Construction;

        // Height and width of original window
        Real64 W = length(surf.Vertex(3) - surf.Vertex(2));
        Real64 H = length(surf.Vertex(2) - surf.Vertex(1));

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord({1, surf.Sides}) = surf.Vertex({1, surf.Sides});

        // Height and width of first glazing system
        auto const &constr = state.dataConstruction->Construct(IConst);
        Real64 h1 = constr.W5FileGlazingSysHeight;
        Real64 w1 = constr.W5FileGlazingSysWidth;

        std::string Const2Name = constr.Name + ":2";
        int IConst2 = Util::FindItemInList(Const2Name, state.dataConstruction->Construct);

        ++AddedSubSurfaces;
        sg->SurfaceTmp.redimension(++state.dataSurface->TotSurfaces);

        auto &newSurf = sg->SurfaceTmp(state.dataSurface->TotSurfaces);
        newSurf.Vertex.allocate(4);

        newSurf.Name = surf.Name + ":2";
        newSurf.Construction = IConst2;
        newSurf.ConstructionStoredInputValue = IConst2;
        newSurf.Class = surf.Class;
        newSurf.Azimuth = surf.Azimuth;
        // Sine and cosine of azimuth and tilt
        newSurf.SinAzim = surf.SinAzim;
        newSurf.CosAzim = surf.CosAzim;
        newSurf.SinTilt = surf.SinTilt;
        newSurf.CosTilt = surf.CosTilt;
        // Outward normal unit vector (pointing away from room)
        newSurf.Centroid = surf.Centroid;
        newSurf.lcsx = surf.lcsx;
        newSurf.lcsy = surf.lcsy;
        newSurf.lcsz = surf.lcsz;
        newSurf.NewellAreaVec = surf.NewellAreaVec;
        newSurf.OutNormVec = surf.OutNormVec;
        newSurf.Reveal = surf.Reveal;
        newSurf.Shape = surf.Shape;
        newSurf.Sides = surf.Sides;
        newSurf.Tilt = surf.Tilt;
        newSurf.convOrientation = Convect::GetSurfConvOrientation(surf.Tilt);
        newSurf.HeatTransSurf = surf.HeatTransSurf;
        newSurf.BaseSurfName = surf.BaseSurfName;
        newSurf.BaseSurf = surf.BaseSurf;
        newSurf.ZoneName = surf.ZoneName;
        newSurf.Zone = surf.Zone;
        newSurf.ExtBoundCondName = surf.ExtBoundCondName;
        newSurf.ExtBoundCond = surf.ExtBoundCond;
        newSurf.ExtSolar = surf.ExtSolar;
        newSurf.ExtWind = surf.ExtWind;
        newSurf.ViewFactorGround = surf.ViewFactorGround;
        newSurf.ViewFactorSky = surf.ViewFactorSky;
        newSurf.ViewFactorGroundIR = surf.ViewFactorGroundIR;
        newSurf.ViewFactorSkyIR = surf.ViewFactorSkyIR;
        newSurf.OSCPtr = surf.OSCPtr;
        newSurf.SchedShadowSurfIndex = surf.SchedShadowSurfIndex;
        newSurf.activeWindowShadingControl = surf.activeWindowShadingControl;
        newSurf.windowShadingControlList = surf.windowShadingControlList;
        newSurf.HasShadeControl = surf.HasShadeControl;
        newSurf.activeShadedConstruction = surf.activeShadedConstruction;
        newSurf.windowShadingControlList = surf.windowShadingControlList;
        newSurf.shadedStormWinConstructionList = surf.shadedStormWinConstructionList;
        newSurf.FrameDivider = surf.FrameDivider;
        newSurf.Multiplier = surf.Multiplier;
        newSurf.NetAreaShadowCalc = surf.NetAreaShadowCalc;

        Real64 MulWidth = constr.W5FileMullionWidth;
        Real64 w2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysWidth;
        Real64 h2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysHeight;

        // Correction to net area of base surface. Add back in the original glazing area and subtract the
        // area of the two glazing systems. Note that for Surface(SurfNum)%Class = 'Window' the effect
        // of a window multiplier is included in the glazing area. Note that frame areas are subtracted later.

        Real64 AreaNew = surf.Multiplier * (h1 * w1 + h2 * w2); // both glazing systems


        auto &baseSurf = sg->SurfaceTmp(surf.BaseSurf);
        // Adjust net area for base surface
        baseSurf.Area -= AreaNew;

        // Net area of base surface with unity window multipliers (used in shadowing checks)
        baseSurf.NetAreaShadowCalc -= AreaNew / surf.Multiplier;

        // Reset area, etc. of original window
        surf.Area = surf.Multiplier * (h1 * w1);
        surf.GrossArea = surf.Area;
        surf.NetAreaShadowCalc = h1 * w1;
        surf.Perimeter = 2 * (h1 + w1);
        surf.Height = h1;
        surf.Width = w1;
        // Set area, etc. of new window
        newSurf.Area = newSurf.Multiplier * (h2 * w2);
        newSurf.GrossArea = newSurf.Area;
        newSurf.NetAreaShadowCalc = h2 * w2;
        newSurf.Perimeter = 2 * (h2 + w2);
        newSurf.Height = h2;
        newSurf.Width = w2;

        if (baseSurf.Area <= 0.0) {
            ShowSevereError(state, format("SurfaceGeometry: ModifyWindow: Subsurfaces have too much area for base surface={}", baseSurf.Name));
            ShowContinueError(state, format("Subsurface (window) creating error={}", surf.Name));
            ShowContinueError(state, format("This window has been replaced by two windows from the Window5 Data File of total area {:.2R} m2", AreaNew));
            ErrorsFound = true;
        }

        // Assign vertices to the new window; modify vertices of original window.
        // In the following, vertices are numbered counter-clockwise with vertex #1 at the upper left.

        Vector3<Real64> orig2minus1 = OriginalCoord(2) - OriginalCoord(1);
        Vector3<Real64> orig3minus4 = OriginalCoord(3) - OriginalCoord(4);
        Vector3<Real64> a, b;
            
        if (constr.W5FileMullionOrientation == DataWindowEquivalentLayer::Orientation::Vertical) {

            // VERTICAL MULLION: original window is modified to become left-hand glazing (system #1);
            // new window is created to become right-hand glazing (system #2)

            // Left-hand glazing
            
            // Vertex 1
            Real64 dx = 0.0;
            Real64 dy = H - h1;
            Vector3<Real64> a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            Vector3<Real64> b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(1) = a + (dx / W) * (b - a);
            // NewCoord(1).x = xa + (dx / W) * (xb - xa);
            // NewCoord(1).y = ya + (dx / W) * (yb - ya);
            // NewCoord(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = 0.0;
            dy = H;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(2) = a + (dx / W) * (b - a);
            // NewCoord(2).x = xa + (dx / W) * (xb - xa);
            // NewCoord(2).y = ya + (dx / W) * (yb - ya);
            // NewCoord(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1;
            dy = H;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(3) = a + (dx / W) * (b - a);
            // NewCoord(3).x = xa + (dx / W) * (xb - xa);
            // NewCoord(3).y = ya + (dx / W) * (yb - ya);
            // NewCoord(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1;
            dy = H - h1;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(4) = a + (dx / W) * (b - a);
            // NewCoord(4).x = xa + (dx / W) * (xb - xa);
            // NewCoord(4).y = ya + (dx / W) * (yb - ya);
            // NewCoord(4).z = za + (dx / W) * (zb - za);

            for (int loop = 1; loop <= surf.Sides; ++loop) {
                surf.Vertex(loop) = NewCoord(loop);
            }

            // Right-hand glazing

            // Vertex 1
            dx = w1 + MulWidth;
            dy = H - (h1 + h2) / 2.0;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(1) = a + (dx / W) * (b - a);
            // NewCoord(1).x = xa + (dx / W) * (xb - xa);
            // NewCoord(1).y = ya + (dx / W) * (yb - ya);
            // NewCoord(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = w1 + MulWidth;
            dy = H + (h2 - h1) / 2.0;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(2) = a + (dx / W) * (b - a);
            // NewCoord(2).x = xa + (dx / W) * (xb - xa);
            // NewCoord(2).y = ya + (dx / W) * (yb - ya);
            // NewCoord(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1 + MulWidth + w2;
            dy = H + (h2 - h1) / 2.0;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(3) = a + (dx / W) * (b - a);
            // NewCoord(3).x = xa + (dx / W) * (xb - xa);
            // NewCoord(3).y = ya + (dx / W) * (yb - ya);
            // NewCoord(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1 + MulWidth + w2;
            dy = H - (h1 + h2) / 2.0;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(4) = a + (dx / W) * (b - a);
            // NewCoord(4).x = xa + (dx / W) * (xb - xa);
            // NewCoord(4).y = ya + (dx / W) * (yb - ya);
            // NewCoord(4).z = za + (dx / W) * (zb - za);

            for (int loop = 1; loop <= newSurf.Sides; ++loop) {
                newSurf.Vertex(loop) = NewCoord(loop);
            }

        } else { // Horizontal mullion

            // HORIZONTAL MULLION: original window is modified to become bottom glazing (system #1);
            // new window is created to become top glazing (system #2)

            // Bottom glazing

            // Vertex 1
            Real64 dx = 0.0;
            Real64 dy = H - h1;
            Vector3<Real64> a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            Vector3<Real64> b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(1) = a + (dx / W) * (b - a);
            // NewCoord(1).x = xa + (dx / W) * (xb - xa);
            // NewCoord(1).y = ya + (dx / W) * (yb - ya);
            // NewCoord(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = 0.0;
            dy = H;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(2) = a + (dx / W) * (b - a);
            // NewCoord(2).x = xa + (dx / W) * (xb - xa);
            // NewCoord(2).y = ya + (dx / W) * (yb - ya);
            // NewCoord(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1;
            dy = H;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(3) = a + (dx / W) * (b - a);
            // NewCoord(3).x = xa + (dx / W) * (xb - xa);
            // NewCoord(3).y = ya + (dx / W) * (yb - ya);
            // NewCoord(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1;
            dy = H - h1;
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(4) = a + (dx / W) * (b - a);
            // NewCoord(4).x = xa + (dx / W) * (xb - xa);
            // NewCoord(4).y = ya + (dx / W) * (yb - ya);
            // NewCoord(4).z = za + (dx / W) * (zb - za);

            for (int loop = 1; loop <= surf.Sides; ++loop) {
                surf.Vertex(loop) = NewCoord(loop);
            }

            // Top glazing

            // Vertex 1
            dx = (w1 - w2) / 2.0;
            dy = H - (h1 + h2 + MulWidth);
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(1) = a + (dx / W) * (b - a);
            // NewCoord(1).x = xa + (dx / W) * (xb - xa);
            // NewCoord(1).y = ya + (dx / W) * (yb - ya);
            // NewCoord(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = (w1 - w2) / 2.0;
            dy = H - (h1 + MulWidth);
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(2) = a + (dx / W) * (b - a);
            // NewCoord(2).x = xa + (dx / W) * (xb - xa);
            // NewCoord(2).y = ya + (dx / W) * (yb - ya);
            // NewCoord(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = (w1 + w2) / 2.0;
            dy = H - (h1 + MulWidth);
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(3) = a + (dx / W) * (b - a);
            // NewCoord(3).x = xa + (dx / W) * (xb - xa);
            // NewCoord(3).y = ya + (dx / W) * (yb - ya);
            // NewCoord(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = (w1 + w2) / 2.0;
            dy = H - (h1 + h2 + MulWidth);
            a = OriginalCoord(1) + (dy / H) * orig2minus1;
            // xa = OriginalCoord(1).x + (dy / H) * (OriginalCoord(2).x - OriginalCoord(1).x);
            // ya = OriginalCoord(1).y + (dy / H) * (OriginalCoord(2).y - OriginalCoord(1).y);
            // za = OriginalCoord(1).z + (dy / H) * (OriginalCoord(2).z - OriginalCoord(1).z);
            b = OriginalCoord(4) + (dy / H) * orig3minus4;
            // xb = OriginalCoord(4).x + (dy / H) * (OriginalCoord(3).x - OriginalCoord(4).x);
            // yb = OriginalCoord(4).y + (dy / H) * (OriginalCoord(3).y - OriginalCoord(4).y);
            // zb = OriginalCoord(4).z + (dy / H) * (OriginalCoord(3).z - OriginalCoord(4).z);
            NewCoord(4) = a + (dx / W) * (b - a);
            // NewCoord(4).x = xa + (dx / W) * (xb - xa);
            // NewCoord(4).y = ya + (dx / W) * (yb - ya);
            // NewCoord(4).z = za + (dx / W) * (zb - za);

            for (int loop = 1; loop <= newSurf.Sides; ++loop) {
                newSurf.Vertex(loop) = NewCoord(loop);
            }

        } // End of check if vertical or horizontal mullion
    } // AddWindow()

    void TransformVertsByAspect(EnergyPlusData &state,
                                int const SurfNum, // Current surface number
                                int const NSides   // Number of sides to figure
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent T Griffith
        //       DATE WRITTEN   April 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Alter input for surface geometry
        // Optimizing building design for energy can involve
        //  altering building geometry.  Rather than assemble routines to transform
        //  geometry through pre-processing on input, it may be simpler to change
        //  vertices within EnergyPlus since it already reads the data from the input
        //  file and there would no longer be a need to rewrite the text data.
        //  This is essentially a crude hack to allow adjusting geometry with
        //  a single parameter...

        // METHODOLOGY EMPLOYED:
        // once vertices have been converted to WCS, change them to reflect a different aspect
        // ratio for the entire building based on user input.
        // This routine is called once for each surface by subroutine GetVertices

        Array1D_string cAlphas(1);
        Array1D<Real64> rNumerics(2);
        int NAlphas;
        int NNum;
        int IOStat;
        Real64 OldAspectRatio;
        Real64 NewAspectRatio;
        // begin execution
        // get user input...

        constexpr std::string_view routineName = "TransformVertsByAspect";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        auto &surf = sg->SurfaceTmp(SurfNum);
        
        ipsc->cCurrentModuleObject = "GeometryTransform";
                
        if (sg->firstTime) {
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject) == 1) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         1,
                                                                         cAlphas,
                                                                         NAlphas,
                                                                         rNumerics,
                                                                         NNum,
                                                                         IOStat,
                                                                         ipsc->lNumericFieldBlanks,
                                                                         ipsc->lAlphaFieldBlanks,
                                                                         ipsc->cAlphaFieldNames,
                                                                         ipsc->cNumericFieldNames);
                OldAspectRatio = rNumerics(1);
                NewAspectRatio = rNumerics(2);
                if (cAlphas(1) != "XY") {
                    ShowWarningError(
                        state, format("{}: invalid {}=\"{}...ignored.", ipsc->cCurrentModuleObject, ipsc->cAlphaFieldNames(1), cAlphas(1)));
                }
                sg->firstTime = false;
                sg->noTransform = false;
                state.dataSurface->AspectTransform = true;
                if (state.dataSurface->WorldCoordSystem) {
                    ShowWarningError(state, format("{}: must use Relative Coordinate System.  Transform request ignored.", ipsc->cCurrentModuleObject));
                    sg->noTransform = true;
                    state.dataSurface->AspectTransform = false;
                }
            } else {
                sg->firstTime = false;
            }
        }
        if (sg->noTransform) return;

        // check surface type.
        if (!surf.HeatTransSurf) {
            // Site Shading do not get transformed.
            if (surf.Class == SurfaceClass::Detached_F) return;
        }

        // testing method of transforming  x and y coordinates as follows

        // this works if not rotated wrt north axis ... but if it is, then trouble
        // try to first derotate it , transform by aspect and then rotate back.

        for (int n = 1; n <= NSides; ++n) {
            Real64 Xo = surf.Vertex(n).x; // world coordinates.... shifted by relative north angle...
            Real64 Yo = surf.Vertex(n).y;
            // next derotate the building
            Real64 XnoRot = Xo * sg->CosBldgRelNorth + Yo * sg->SinBldgRelNorth;
            Real64 YnoRot = Yo * sg->CosBldgRelNorth - Xo * sg->SinBldgRelNorth;
            // translate
            Real64 Xtrans = XnoRot * std::sqrt(NewAspectRatio / OldAspectRatio);
            Real64 Ytrans = YnoRot * std::sqrt(OldAspectRatio / NewAspectRatio);
            // rerotate
            surf.Vertex(n).x = Xtrans * sg->CosBldgRelNorth - Ytrans * sg->SinBldgRelNorth;
            surf.Vertex(n).y = Xtrans * sg->SinBldgRelNorth + Ytrans * sg->CosBldgRelNorth;
        }
    } // TransformVertsByAspect()

    void CalcSurfaceCentroid(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Feb. 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // compute centroid of all the surfaces in the main
        // surface structure. Store the vertex coordinates of
        // the centroid in the 'SURFACE' derived type.

        // METHODOLOGY EMPLOYED:
        // The centroid of triangle is easily computed by averaging the vertices
        // The centroid of a quadrilateral is computed by area weighting the centroids
        // of two triangles.
        // (Algorithm would need to be changed for higher order
        // polygons with more than four sides).

            
        static Vector3<Real64> const zero_vector(0.0);
        Vector3<Real64> centroid;

        int negZcount(0); // for warning error in surface centroids

        auto &sg = state.dataSurfaceGeometry;

        // loop through all the surfaces
        for (int ThisSurf = 1; ThisSurf <= state.dataSurface->TotSurfaces; ++ThisSurf) {
            auto &surface = state.dataSurface->Surface(ThisSurf);

            if (surface.Class == SurfaceClass::IntMass) continue;

            auto const &vertex = surface.Vertex;

            if (surface.Sides == 3) { // 3-sided polygon

                centroid = cen(vertex(1), vertex(2), vertex(3));

            } else if (surface.Sides == 4) { // 4-sided polygon

                // split into 2 3-sided polygons (Triangle 1 and Triangle 2)
                sg->Triangle1(1) = vertex(1);
                sg->Triangle1(2) = vertex(2);
                sg->Triangle1(3) = vertex(3);
                sg->Triangle2(1) = vertex(1);
                sg->Triangle2(2) = vertex(3);
                sg->Triangle2(3) = vertex(4);

                // get total Area of quad.
                Real64 TotalArea(surface.GrossArea);
                if (TotalArea <= 0.0) {
                    // catch a problem....
                    ShowWarningError(state, format("CalcSurfaceCentroid: zero area surface, for surface={}", surface.Name));
                    continue;
                }

                // get area fraction of triangles.
                Real64 Tri1Area(Vectors::CalcPolygonArea(sg->Triangle1, 3) / TotalArea);
                Real64 Tri2Area(Vectors::CalcPolygonArea(sg->Triangle2, 3) / TotalArea);

                // check if sum of fractions are slightly greater than 1.0 which is a symptom of the triangles for a non-convex
                // quadralateral using the wrong two triangles
                if ((Tri1Area + Tri2Area) > 1.05) {

                    // if so repeat the process with the other two possible triangles (notice the vertices are in a different order this
                    // time) split into 2 3-sided polygons (Triangle 1 and Triangle 2)
                    sg->Triangle1(1) = vertex(1);
                    sg->Triangle1(2) = vertex(2);
                    sg->Triangle1(3) = vertex(4);
                    sg->Triangle2(1) = vertex(2);
                    sg->Triangle2(2) = vertex(3);
                    sg->Triangle2(3) = vertex(4);

                    // get area fraction of triangles.
                    Real64 AreaTriangle1 = Vectors::CalcPolygonArea(sg->Triangle1, 3);
                    Real64 AreaTriangle2 = Vectors::CalcPolygonArea(sg->Triangle2, 3);
                    TotalArea = AreaTriangle1 + AreaTriangle2;
                    Tri1Area = AreaTriangle1 / TotalArea;
                    Tri2Area = AreaTriangle2 / TotalArea;
                }

                // get centroid of Triangle 1
                Vector3<Real64> cen1(cen(sg->Triangle1(1), sg->Triangle1(2), sg->Triangle1(3)));

                // get centroid of Triangle 2
                Vector3<Real64> cen2(cen(sg->Triangle2(1), sg->Triangle2(2), sg->Triangle2(3)));

                // find area weighted combination of the two centroids (coded to avoid temporary Vectors)
                cen1 *= Tri1Area;
                cen2 *= Tri2Area;
                centroid = cen1;
                centroid += cen2;

            } else if (surface.Sides >= 5) { // multi-sided polygon
                // (Maybe triangulate?  For now, use old "z" average method")
                // and X and Y -- straight average

                //        X1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%x)
                //        X2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%x)
                //        Y1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%y)
                //        Y2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%y)
                //        Z1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%z)
                //        Z2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%z)
                //        Xcm=(X1+X2)/2.0d0
                //        Ycm=(Y1+Y2)/2.0d0
                //        Zcm=(Z1+Z2)/2.0d0

                // Calc centroid as average of surfaces
                centroid = 0.0;
                for (int vert = 1; vert <= surface.Sides; ++vert) {
                    centroid += vertex(vert);
                }
                centroid /= double(surface.Sides);

            } else {

                if (!surface.Name.empty()) {
                    ShowWarningError(state, format("CalcSurfaceCentroid: caught problem with # of sides, for surface={}", surface.Name));
                    ShowContinueError(state, format("... number of sides must be >= 3, this surface # sides={}", surface.Sides));
                } else {
                    ShowWarningError(state, format("CalcSurfaceCentroid: caught problem with # of sides, for surface=#{}", ThisSurf));
                    ShowContinueError(state,
                                      "...surface name is blank. Examine surfaces -- this may be a problem with ill-formed interzone surfaces.");
                    ShowContinueError(state, format("... number of sides must be >= 3, this surface # sides={}", surface.Sides));
                }
                centroid = 0.0;
            }

            // store result in the surface structure in DataSurfaces
            surface.Centroid = centroid;

            if (centroid.z < 0.0) {
                if (surface.ExtWind || surface.ExtBoundCond == ExternalEnvironment) ++negZcount;
            }

        } // loop through surfaces

        if (negZcount > 0) {
            ShowWarningError(state, format("CalcSurfaceCentroid: {} Surfaces have the Z coordinate < 0.", negZcount));
            ShowContinueError(state, "...in any calculations, Wind Speed will be 0.0 for these surfaces.");
            ShowContinueError(state,
                              format("...in any calculations, Outside temperatures will be the outside temperature + {:.3R} for these surfaces.",
                                     state.dataEnvrn->WeatherFileTempModCoeff));
            ShowContinueError(state, "...that is, these surfaces will have conditions as though at ground level.");
        }
    } // CalcSurfaceCentroid()

    void SetupShadeSurfacesForSolarCalcs(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Dec. 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // determine if Shading surfaces need full solar calcs because they
        // are also used to define geometry of an active solar component.
        // Normally, a shading surface is not included in calculations of incident solar, only shading

        // METHODOLOGY EMPLOYED:
        // Mine solar renewables input and collect surface names.
        // find shading surfaces with names that match those in solar objects.
        // setup flags for shading surfaces so that the solar renewables can resuse incident solar calcs
        // new solar component models that use shading surfaces will have to extend the code here.

        // Using/Aliasing

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string TmpCandidateSurfaceNames;
        Array1D_string TmpCandidateICSSurfaceNames;
        Array1D_string TmpCandidateICSBCTypeNames;
        int NumCandidateNames;
        int NumOfCollectors;
        int NumOfICSUnits;
        int NumOfFlatPlateUnits;
        int NumPVTs;
        int NumPVs;
        int SurfNum;
        int Found;
        int CollectorNum;
        int PVTnum;
        int PVnum;
        int NumAlphas;  // Number of alpha names being passed
        int NumNumbers; // Number of numeric parameters being passed
        int IOStatus;

        constexpr std::string_view routineName = "SetupShadeSurfaceForSolarCalcs";
        auto &ipsc = state.dataIPShortCut;
        auto &sg = state.dataSurfaceGeometry;

        // First collect names of surfaces referenced by active solar components
        ipsc->cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
        NumOfFlatPlateUnits = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        ipsc->cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        NumPVTs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        ipsc->cCurrentModuleObject = "Generator:Photovoltaic";
        NumPVs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);
        ipsc->cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
        NumOfICSUnits = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, ipsc->cCurrentModuleObject);

        NumCandidateNames = NumOfFlatPlateUnits + NumPVTs + NumPVs + NumOfICSUnits;
        NumOfCollectors = NumOfFlatPlateUnits + NumOfICSUnits;

        TmpCandidateSurfaceNames.allocate(NumCandidateNames);
        TmpCandidateICSSurfaceNames.allocate(NumOfCollectors);
        TmpCandidateICSBCTypeNames.allocate(NumOfCollectors);

        if (NumOfCollectors > 0) {
            ipsc->cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
            for (int CollectorNum = 1; CollectorNum <= NumOfFlatPlateUnits; ++CollectorNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         CollectorNum,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus);

                TmpCandidateSurfaceNames(CollectorNum) = ipsc->cAlphaArgs(3);
                TmpCandidateICSBCTypeNames(CollectorNum) = "";
            }
        }

        if (NumPVTs > 0) {
            ipsc->cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
            for (PVTnum = 1; PVTnum <= NumPVTs; ++PVTnum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         PVTnum,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus);

                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + PVTnum) = ipsc->cAlphaArgs(2);
            }
        }

        if (NumPVs > 0) {
            ipsc->cCurrentModuleObject = "Generator:Photovoltaic";
            for (PVnum = 1; PVnum <= NumPVs; ++PVnum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         PVnum,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus);
                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + PVnum) = ipsc->cAlphaArgs(2);
            }
        }

        if (NumOfICSUnits > 0) {
            ipsc->cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
            for (CollectorNum = 1; CollectorNum <= NumOfICSUnits; ++CollectorNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         ipsc->cCurrentModuleObject,
                                                                         CollectorNum,
                                                                         ipsc->cAlphaArgs,
                                                                         NumAlphas,
                                                                         ipsc->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus);
                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + NumPVs + CollectorNum) = ipsc->cAlphaArgs(3);
                TmpCandidateICSSurfaceNames(NumOfFlatPlateUnits + CollectorNum) = ipsc->cAlphaArgs(3);
                TmpCandidateICSBCTypeNames(NumOfFlatPlateUnits + CollectorNum) = ipsc->cAlphaArgs(4);
            }
        }

        // loop through all the surfaces
        for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            auto &surf = state.dataSurface->Surface(SurfNum);
            Found = Util::FindItemInList(surf.Name, TmpCandidateSurfaceNames, NumCandidateNames);
            if (Found > 0) {
                if (!surf.HeatTransSurf) { // not BIPV, must be a shading surf with solar device
                    // Setup missing values to allow shading surfaces to model incident solar and wind
                    surf.ExtSolar = true;
                    surf.ExtWind = true;
                    surf.ViewFactorGround = 0.5 * (1.0 - surf.CosTilt);
                }
                // check if this surface is used for ICS collector mounting and has OthersideCondictionsModel as its
                // boundary condition
                if (NumOfICSUnits > 0) {
                    for (CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum) {
                        if (Util::SameString(surf.Name, TmpCandidateICSSurfaceNames(CollectorNum)) &&
                            Util::SameString(TmpCandidateICSBCTypeNames(CollectorNum), "OTHERSIDECONDITIONSMODEL")) {
                            state.dataSurface->SurfIsICS(SurfNum) = true;
                            state.dataSurface->SurfICSPtr(SurfNum) = CollectorNum;
                        }
                    }
                }

            } // end of IF (Found > 0) Then
        }
    }

    void
    SetupEnclosuresAndAirBoundaries(EnergyPlusData &state,
                                    EPVector<DataViewFactorInformation::EnclosureViewFactorInformation> &Enclosures, // Radiant or Solar Enclosures
                                    SurfaceGeometry::enclosureType const EnclosureType,                              // Radiant or Solar
                                    bool &ErrorsFound)                                                               // Set to true if errors found
    {
        static constexpr std::string_view RoutineName = "SetupEnclosuresAndAirBoundaries";
        bool anyGroupedSpaces = false;
        bool radiantSetup = false;
        bool solarSetup = false;
        std::string RadiantOrSolar = "";
        int enclosureNum = 0;
        if (EnclosureType == RadiantEnclosures) {
            radiantSetup = true;
            RadiantOrSolar = "Radiant";
            state.dataViewFactor->EnclRadInfo.allocate(state.dataGlobal->numSpaces);
        } else if (EnclosureType == SolarEnclosures) {
            solarSetup = true;
            RadiantOrSolar = "Solar";
            state.dataViewFactor->EnclSolInfo.allocate(state.dataGlobal->numSpaces);
        } else {
            ShowFatalError(
                state, format("{}: Illegal call to this function. Second argument must be 'RadiantEnclosures' or 'SolarEnclosures'", RoutineName));
        }
        if (std::any_of(state.dataConstruction->Construct.begin(),
                        state.dataConstruction->Construct.end(),
                        [](Construction::ConstructionProps const &e) { return e.TypeIsAirBoundary; })) {
            int errorCount = 0;
            for (int surfNum = 1; surfNum <= state.dataSurface->TotSurfaces; ++surfNum) {
                auto &surf = state.dataSurface->Surface(surfNum);
                if (surf.Construction == 0) continue;
                auto &constr = state.dataConstruction->Construct(surf.Construction);
                if (!constr.TypeIsAirBoundary) continue;
                surf.IsAirBoundarySurf = true;

                // Check for invalid air boundary surfaces - valid only on non-adiabatic interzone surfaces
                // Only check this once during radiant setup, skip for solar setup
                if (radiantSetup && (surf.ExtBoundCond <= 0 || surf.ExtBoundCond == surfNum)) {
                    ErrorsFound = true;
                    if (!state.dataGlobal->DisplayExtraWarnings) {
                        ++errorCount;
                    } else {
                        ShowSevereError(
                            state, format("{}: Surface=\"{}\" uses Construction:AirBoundary in a non-interzone surface.", RoutineName, surf.Name));
                    }
                } else {
                    // Process air boundary - set surface properties and set up enclosures
                    // Radiant exchange
                    if (surf.IsAirBoundarySurf) {
                        // Boundary is grouped - assign enclosure
                        state.dataHeatBal->AnyAirBoundary = true;
                        int thisSideEnclosureNum = 0;
                        int otherSideEnclosureNum = 0;
                        if (radiantSetup) {
                            // Radiant enclosure setup
                            constr.IsUsedCTF = false;
                            surf.HeatTransSurf = false;
                            surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel::AirBoundaryNoHT;
                            thisSideEnclosureNum = state.dataHeatBal->space(surf.spaceNum).radiantEnclosureNum;
                            otherSideEnclosureNum =
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).radiantEnclosureNum;
                        } else {
                            // Solar enclosure setup
                            thisSideEnclosureNum = state.dataHeatBal->space(surf.spaceNum).solarEnclosureNum;
                            otherSideEnclosureNum =
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).solarEnclosureNum;
                        }
                        anyGroupedSpaces = true;
                        if ((thisSideEnclosureNum == 0) && (otherSideEnclosureNum == 0)) {
                            // Neither zone is assigned to an enclosure, so increment the counter and assign to both
                            ++enclosureNum;
                            auto &thisEnclosure = Enclosures(enclosureNum);
                            thisSideEnclosureNum = enclosureNum;
                            thisEnclosure.Name = format("{} Enclosure {}", RadiantOrSolar, enclosureNum);
                            thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(surf.spaceNum).Name);
                            thisEnclosure.spaceNums.push_back(surf.spaceNum);
                            thisEnclosure.FloorArea += state.dataHeatBal->space(surf.spaceNum).FloorArea;
                            otherSideEnclosureNum = enclosureNum;
                            int otherSideSpaceNum = state.dataSurface->Surface(surf.ExtBoundCond).spaceNum;
                            if (otherSideSpaceNum != surf.spaceNum) {
                                thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(otherSideSpaceNum).Name);
                                thisEnclosure.spaceNums.push_back(otherSideSpaceNum);
                                thisEnclosure.FloorArea += state.dataHeatBal->space(otherSideSpaceNum).FloorArea;
                            }
                            if (radiantSetup) {
                                state.dataHeatBal->space(surf.spaceNum).radiantEnclosureNum = thisSideEnclosureNum;
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).radiantEnclosureNum =
                                    otherSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea += state.dataHeatBal->space(surf.spaceNum).extWindowArea;
                                thisEnclosure.TotalSurfArea += state.dataHeatBal->space(surf.spaceNum).totalSurfArea;
                                if (otherSideSpaceNum != surf.spaceNum) {
                                    thisEnclosure.ExtWindowArea += state.dataHeatBal->space(otherSideSpaceNum).extWindowArea;
                                    thisEnclosure.TotalSurfArea += state.dataHeatBal->space(otherSideSpaceNum).totalSurfArea;
                                }
                                state.dataHeatBal->space(surf.spaceNum).solarEnclosureNum = thisSideEnclosureNum;
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).solarEnclosureNum =
                                    otherSideEnclosureNum;
                            }
                        } else if (thisSideEnclosureNum == 0) {
                            // Other side is assigned, so use that one for both
                            thisSideEnclosureNum = otherSideEnclosureNum;
                            auto &thisEnclosure = Enclosures(thisSideEnclosureNum);
                            thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(surf.spaceNum).Name);
                            thisEnclosure.spaceNums.push_back(surf.spaceNum);
                            thisEnclosure.FloorArea += state.dataHeatBal->space(surf.spaceNum).FloorArea;
                            if (radiantSetup) {
                                state.dataHeatBal->space(surf.spaceNum).radiantEnclosureNum = thisSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea += state.dataHeatBal->space(surf.spaceNum).extWindowArea;
                                thisEnclosure.TotalSurfArea += state.dataHeatBal->space(surf.spaceNum).totalSurfArea;
                                state.dataHeatBal->space(surf.spaceNum).solarEnclosureNum = thisSideEnclosureNum;
                            }
                        } else if (otherSideEnclosureNum == 0) {
                            // This side is assigned, so use that one for both
                            otherSideEnclosureNum = thisSideEnclosureNum;
                            auto &thisEnclosure = Enclosures(thisSideEnclosureNum);
                            thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).Name);
                            thisEnclosure.spaceNums.push_back(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum);
                            thisEnclosure.FloorArea += state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).FloorArea;
                            if (radiantSetup) {
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).radiantEnclosureNum =
                                    otherSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea +=
                                    state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).extWindowArea;
                                thisEnclosure.TotalSurfArea +=
                                    state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).totalSurfArea;
                                state.dataHeatBal->space(state.dataSurface->Surface(surf.ExtBoundCond).spaceNum).solarEnclosureNum =
                                    otherSideEnclosureNum;
                            }
                        } else if (thisSideEnclosureNum != otherSideEnclosureNum) {
                            // If both sides are already assigned to an enclosure, then merge the two enclosures
                            auto &thisEnclosure = Enclosures(thisSideEnclosureNum);
                            auto &otherEnclosure = Enclosures(otherSideEnclosureNum);
                            for (const auto &zName : thisEnclosure.spaceNames) {
                                otherEnclosure.spaceNames.push_back(zName);
                            }
                            for (int zNum : thisEnclosure.spaceNums) {
                                otherEnclosure.spaceNums.push_back(zNum);
                                if (radiantSetup) {
                                    state.dataHeatBal->space(zNum).radiantEnclosureNum = otherSideEnclosureNum;
                                } else {
                                    state.dataHeatBal->space(zNum).solarEnclosureNum = otherSideEnclosureNum;
                                }
                            }
                            otherEnclosure.FloorArea += thisEnclosure.FloorArea;
                            otherEnclosure.ExtWindowArea += thisEnclosure.ExtWindowArea;
                            otherEnclosure.TotalSurfArea += thisEnclosure.TotalSurfArea;
                            // Move any enclosures beyond thisEnclosure down one slot - at this point all enclosures are named "Radiant
                            // Enclosure N"
                            for (int enclNum = thisSideEnclosureNum; enclNum < enclosureNum; ++enclNum) {
                                std::string saveName = Enclosures(enclNum).Name;
                                Enclosures(enclNum) = Enclosures(enclNum + 1);
                                Enclosures(enclNum).Name = saveName;
                                for (int sNum : Enclosures(enclNum).spaceNums) {
                                    if (radiantSetup) {
                                        state.dataHeatBal->space(sNum).radiantEnclosureNum = enclNum;
                                    } else {
                                        state.dataHeatBal->space(sNum).solarEnclosureNum = enclNum;
                                    }
                                }
                            }
                            // Clear the last rad enclosure and reduce the total number of enclosures by 1
                            Enclosures(enclosureNum).Name.clear();
                            Enclosures(enclosureNum).spaceNames.clear();
                            Enclosures(enclosureNum).spaceNums.clear();
                            Enclosures(enclosureNum).FloorArea = 0;
                            Enclosures(enclosureNum).ExtWindowArea = 0;
                            Enclosures(enclosureNum).TotalSurfArea = 0;
                            enclosureNum -= 1;
                        }
                    } else {
                        ErrorsFound = true;
                        ShowSevereError(state, format("{}: Surface={} uses Construction:AirBoundary with illegal option:", RoutineName, surf.Name));
                        if (radiantSetup) {
                            ShowContinueError(state, "Radiant Exchange Method must be either GroupedSpaces or IRTSurface.");
                        } else {
                            ShowContinueError(state, "Solar and Daylighting Method must be either GroupedSpaces or InteriorWindow");
                        }
                    }
                    if (solarSetup && constr.TypeIsAirBoundaryMixing) {
                        // Set up mixing air boundaries only once, during solar setup
                        int spaceNum1 = min(surf.spaceNum, state.dataSurface->Surface(surf.ExtBoundCond).spaceNum);
                        int spaceNum2 = max(surf.spaceNum, state.dataSurface->Surface(surf.ExtBoundCond).spaceNum);
                        // This pair already saved?
                        bool found = false;
                        for (auto const &thisAirBoundaryMixing : state.dataHeatBal->airBoundaryMixing) {
                            if ((spaceNum1 == thisAirBoundaryMixing.space1) && (spaceNum2 == thisAirBoundaryMixing.space2)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            // Store the space pairs, schedule, and flow rate to use later to create cross mixing objects
                            DataHeatBalance::AirBoundaryMixingSpecs newAirBoundaryMixing;
                            newAirBoundaryMixing.space1 = spaceNum1;
                            newAirBoundaryMixing.space2 = spaceNum2;
                            newAirBoundaryMixing.scheduleIndex = state.dataConstruction->Construct(surf.Construction).AirBoundaryMixingSched;
                            Real64 mixingVolume = state.dataConstruction->Construct(surf.Construction).AirBoundaryACH *
                                                  min(state.dataHeatBal->space(spaceNum1).Volume, state.dataHeatBal->space(spaceNum2).Volume) /
                                                  Constant::SecInHour;
                            newAirBoundaryMixing.mixingVolumeFlowRate = mixingVolume;
                            state.dataHeatBal->airBoundaryMixing.push_back(newAirBoundaryMixing);
                        }
                    }
                }
            }
            if (errorCount > 0) {
                ShowSevereError(state, format("{}: {} surfaces use Construction:AirBoundary in non-interzone surfaces.", RoutineName, errorCount));
                ShowContinueError(state, "For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
            }
        }
        // Check for any spaces defined only by floor surface(s) and group them
        for (auto const &zone : state.dataHeatBal->Zone) {
            int newEnclosureNum = 0;
            for (int const spaceNum : zone.spaceIndexes) {
                int spaceEnclosureNum = 0;
                bool spaceHasOnlyFloors = false;
                if (radiantSetup) {
                    spaceEnclosureNum = state.dataHeatBal->space(spaceNum).radiantEnclosureNum;
                } else {
                    spaceEnclosureNum = state.dataHeatBal->space(spaceNum).solarEnclosureNum;
                }
                if (spaceEnclosureNum == 0) {
                    spaceHasOnlyFloors = true;
                    for (int const surfNum : state.dataHeatBal->space(spaceNum).surfaces) {
                        if (state.dataSurface->Surface(surfNum).Class == SurfaceClass::IntMass) continue;
                        if (state.dataSurface->Surface(surfNum).Class != SurfaceClass::Floor) {
                            spaceHasOnlyFloors = false;
                            break;
                        }
                    }
                }
                if (spaceEnclosureNum == 0 && spaceHasOnlyFloors) {
                    anyGroupedSpaces = true;
                    if (newEnclosureNum == 0) {
                        // Assign one new enclosure for all loose floors in this zone
                        ++enclosureNum;
                        newEnclosureNum = enclosureNum;
                    }
                    if (radiantSetup) {
                        state.dataHeatBal->space(spaceNum).radiantEnclosureNum = enclosureNum;
                    } else {
                        state.dataHeatBal->space(spaceNum).solarEnclosureNum = enclosureNum;
                    }
                    auto &thisEnclosure = Enclosures(enclosureNum);
                    // Give this enclosure the zone name and assign this to the zone-remainder space if it exists
                    thisEnclosure.Name = zone.Name;
                    thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(spaceNum).Name);
                    thisEnclosure.spaceNums.push_back(spaceNum);
                    thisEnclosure.FloorArea = state.dataHeatBal->space(spaceNum).FloorArea;
                    thisEnclosure.ExtWindowArea = state.dataHeatBal->space(spaceNum).extWindowArea;
                    thisEnclosure.TotalSurfArea = state.dataHeatBal->space(spaceNum).totalSurfArea;
                }
            }
        }

        if (anyGroupedSpaces) {
            // All grouped spaces have been assigned to an enclosure, now assign remaining spaces
            for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
                auto &curSpace = state.dataHeatBal->space(spaceNum);
                int spaceEnclosureNum = 0;
                if (radiantSetup) {
                    spaceEnclosureNum = curSpace.radiantEnclosureNum;
                } else {
                    spaceEnclosureNum = curSpace.solarEnclosureNum;
                }
                if (spaceEnclosureNum == 0) {
                    if (Util::SameString(curSpace.Name, state.dataHeatBal->Zone(curSpace.zoneNum).Name + "-REMAINDER")) {
                        // Search for existing enclosure with same name as the zone
                        spaceEnclosureNum = Util::FindItemInList(state.dataHeatBal->Zone(curSpace.zoneNum).Name, Enclosures);
                    }
                    if (spaceEnclosureNum == 0) {
                        // Otherwise add a new one named for the space
                        ++enclosureNum;
                        spaceEnclosureNum = enclosureNum;
                        Enclosures(spaceEnclosureNum).Name = curSpace.Name;
                    }
                    if (radiantSetup) {
                        curSpace.radiantEnclosureNum = spaceEnclosureNum;
                    } else {
                        curSpace.solarEnclosureNum = spaceEnclosureNum;
                    }
                    auto &thisEnclosure = Enclosures(spaceEnclosureNum);
                    thisEnclosure.spaceNames.push_back(curSpace.Name);
                    thisEnclosure.spaceNums.push_back(spaceNum);
                    thisEnclosure.FloorArea += curSpace.FloorArea;
                    thisEnclosure.ExtWindowArea += curSpace.extWindowArea;
                    thisEnclosure.TotalSurfArea += curSpace.totalSurfArea;
                }
            }
            if (radiantSetup) {
                state.dataViewFactor->NumOfRadiantEnclosures = enclosureNum;
            } else {
                state.dataViewFactor->NumOfSolarEnclosures = enclosureNum;
            }
        } else {
            // There are no grouped radiant air boundaries, assign each space to it's own radiant enclosure
            for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
                auto &thisEnclosure = Enclosures(spaceNum);
                thisEnclosure.Name = state.dataHeatBal->space(spaceNum).Name;
                thisEnclosure.spaceNames.push_back(state.dataHeatBal->space(spaceNum).Name);
                thisEnclosure.spaceNums.push_back(spaceNum);
                thisEnclosure.FloorArea = state.dataHeatBal->space(spaceNum).FloorArea;
                if (radiantSetup) {
                    state.dataHeatBal->space(spaceNum).radiantEnclosureNum = spaceNum;
                } else {
                    state.dataHeatBal->space(spaceNum).solarEnclosureNum = spaceNum;
                    thisEnclosure.ExtWindowArea = state.dataHeatBal->space(spaceNum).extWindowArea;
                    thisEnclosure.TotalSurfArea = state.dataHeatBal->space(spaceNum).totalSurfArea;
                }
            }
            if (radiantSetup) {
                state.dataViewFactor->NumOfRadiantEnclosures = state.dataGlobal->numSpaces;
            } else {
                state.dataViewFactor->NumOfSolarEnclosures = state.dataGlobal->numSpaces;
            }
        }
        if (radiantSetup) {
            assert(state.dataViewFactor->NumOfRadiantEnclosures <= int(Enclosures.size()));
            Enclosures.resize(state.dataViewFactor->NumOfRadiantEnclosures);
        } else {
            assert(state.dataViewFactor->NumOfSolarEnclosures <= int(Enclosures.size()));
            Enclosures.resize(state.dataViewFactor->NumOfSolarEnclosures);
        }

        for (auto &thisEnclosure : state.dataViewFactor->EnclRadInfo) {
            SetupOutputVariable(state,
                                "Enclosure Mean Radiant Temperature",
                                Constant::Units::C,
                                thisEnclosure.MRT,
                                OutputProcessor::SOVTimeStepType::Zone,
                                OutputProcessor::SOVStoreType::State,
                                thisEnclosure.Name);
        }

        // TODO MJW: For now, set the max and min enclosure numbers for each zone to be used in CalcInteriorRadExchange with ZoneToResimulate
        for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
            for (int spaceNum : state.dataHeatBal->Zone(zoneNum).spaceIndexes) {
                if (state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst == -1) { // initial value
                    state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst = state.dataHeatBal->space(spaceNum).radiantEnclosureNum;
                } else {
                    state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst =
                        min(state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst, state.dataHeatBal->space(spaceNum).radiantEnclosureNum);
                }
                state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureLast =
                    max(state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureLast, state.dataHeatBal->space(spaceNum).radiantEnclosureNum);
            }
            assert(state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst != -1);
            assert(state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureLast != -1);
            assert(state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureFirst <= state.dataHeatBal->Zone(zoneNum).zoneRadEnclosureLast);
        }
    }

    void CheckConvexity(EnergyPlusData &state,
                        int const SurfNum, // Current surface number
                        int const NSides   // Number of sides to figure
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tyler Hoyt
        //       DATE WRITTEN   December 2010
        //       MODIFIED       CR8752 - incorrect note of non-convex polygons
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE: This subroutine verifies the convexity of a
        // surface that is exposed to the sun in the case that full shading calculations
        // are required. The calculation conveniently detects collinear points as well,
        // and returns a list of indices that are collinear within the plane of the surface.

        // METHODOLOGY EMPLOYED: First the surface is determined to have dimension 2 in
        // either the xy, yz, or xz plane. That plane is selected to do the testing.
        // Vectors representing the edges of the polygon and the perpendicular dot product
        // between adjacent edges are computed. This allows the turning angle to be determined.
        // If the turning angle is greater than pi/2, it turns to the right, and if it is
        // less than pi/2, it turns left. The direction of the turn is stored, and if it
        // changes as the edges are iterated the surface is not convex. Meanwhile it stores
        // the indices of vertices that are collinear and are later removed.

        // REFERENCES:
        // http://mathworld.wolfram.com/ConvexPolygon.html

        // Using/Aliasing

        using namespace DataErrorTracking;

        constexpr Real64 TurnThreshold(0.000001); // Sensitivity of convexity test, in radians

        enum class Axis { Invalid = -1, X, Y, Z };

        Real64 LastTheta = 0.0;                 // Angle between edge vectors
        bool SignFlag;                          // Direction of edge turn : true is right, false is left
        bool PrevSignFlag(false);               // Container for the sign of the previous iteration's edge turn
        bool PrevSignFlagInitialized(false);    // Whether we picked a PrevSignFlag already or not

        Array1D<Vector3<Real64>> V(NSides+2);
        Axis axisA = Axis::Invalid;
        Axis axisB = Axis::Invalid;
        
        std::vector<int> surfCollinearVerts; // index of vertices to remove, 1-indexed
        surfCollinearVerts.reserve(NSides + 2);

        auto &surf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);

        for (int n = 1; n <= NSides; ++n) {
            V(n) = surf.Vertex(n);
        }
        
        V(NSides + 1) = surf.Vertex(1);
        V(NSides + 2) = surf.Vertex(2);

        // Determine a suitable plane in which to do the tests
        Real64 Det = 0.0;
        for (int n = 1; n <= NSides; ++n) {
            Det += V(n).x * V(n + 1).y - V(n + 1).x * V(n).y;
        }
        if (std::abs(Det) > 1.e-4) {
            axisA = Axis::X;
            axisB = Axis::Y;
        } else {
            Det = 0.0;
            for (int n = 1; n <= NSides; ++n) {
                Det += V(n).x * V(n + 1).z - V(n + 1).x * V(n).z;
            }
            if (std::abs(Det) > 1.e-4) {
                axisA = Axis::X;
                axisB = Axis::Z;
            } else {
                Det = 0.0;
                for (int n = 1; n <= NSides; ++n) {
                    Det += V(n).y * V(n + 1).z - V(n + 1).y * V(n).z;
                }
                if (std::abs(Det) > 1.e-4) {
                    axisA = Axis::Y;
                    axisB = Axis::Z;
                } else {
                    // This condition should not be reached if the surfaces are guaranteed to be planar already
                    ShowSevereError(state, format("CheckConvexity: Surface=\"{}\" is non-planar.", surf.Name));
                    ShowContinueError(state, "Coincident Vertices will be removed as possible.");
                    for (int n = 1; n <= surf.Sides; ++n) {
                        auto const &point = surf.Vertex(n);
                        static constexpr std::string_view ErrFmt = " ({:8.3F},{:8.3F},{:8.3F})";
                        ShowContinueError(state, format(ErrFmt, point.x, point.y, point.z));
                    }
                }
            }
        }

        for (int n = 1; n <= NSides; ++n) { // perform convexity test in the plane determined above.

            Vector2<Real64> pt0, pt1, pt2;
            if (axisA == Axis::X) { pt0.x = V(n).x; pt1.x = V(n+1).x; pt2.x = V(n+2).x; }
            else { pt0.x = V(n).y, pt1.x = V(n+1).y; pt2.x = V(n+2).y; }
            
            if (axisB == Axis::Y) { pt0.y = V(n).y; pt1.y = V(n+1).y; pt2.y = V(n+2).y; }
            else { pt0.y = V(n).z, pt1.y = V(n+1).z; pt2.y = V(n+2).z; }

            Vector2<Real64> V1 = pt1 - pt0;
            Vector2<Real64> V2 = pt2 - pt1;

            Real64 V1len = V1.length(); // = norm_L2()
            Real64 V2len = V2.length();
            if (V1len <= 1.e-8 || V2len <= 1.e-8) {
                // At least two points are coincident. Should this happen? GetVertices is supposed to pop these vertices
                continue;
            }
            Real64 CrossProd = V1.cross(V2);
            Real64 sinarg = CrossProd / (V1len * V2len);
            if (sinarg < -1.0) {
                sinarg = -1.0;
            } else if (sinarg > 1.0) {
                sinarg = 1.0;
            }
            Real64 Theta = std::asin(sinarg);
            if (Theta > TurnThreshold) {
                SignFlag = true;
            } else if (Theta < -TurnThreshold) {
                SignFlag = false;
            } else { // std::abs(Theta) < TurnThreshold
                // Store the index of the collinear vertex for removal
                int colinearIndex = n + 1;
                if (colinearIndex > NSides) {
                    colinearIndex -= NSides;
                }
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(
                        state,
                        format("CheckConvexity: Surface=\"{}\", vertex {} is colinear with previous and next.", surf.Name, colinearIndex));
                }
                ++state.dataErrTracking->TotalCoincidentVertices;
                surfCollinearVerts.push_back(colinearIndex);
                continue;
            }

            if (!PrevSignFlagInitialized) {
                PrevSignFlag = SignFlag;
                LastTheta = Theta;
                PrevSignFlagInitialized = true;
                continue;
            }

            if (SignFlag != PrevSignFlag) {
                if (state.dataGlobal->DisplayExtraWarnings && surf.ExtSolar &&
                    (state.dataHeatBal->SolarDistribution != DataHeatBalance::Shadowing::Minimal) &&
                    // Warn only once
                    surf.IsConvex) {
                    ShowWarningError(state,
                                     format("CheckConvexity: Zone=\"{}\", Surface=\"{}\" is non-convex.",
                                            state.dataHeatBal->Zone(surf.Zone).Name,
                                            surf.Name));
                    int Np1 = n + 1;
                    if (Np1 > NSides) {
                        Np1 -= NSides;
                    }
                    int Np2 = n + 2;
                    if (Np2 > NSides) {
                        Np2 -= NSides;
                    }
                    ShowContinueError(state, format("...vertex {} to vertex {} to vertex {}", n, Np1, Np2));
                    ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", n, V(n).x, V(n).y, V(n).z));
                    ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", Np1, V(n + 1).x, V(n + 1).y, V(n + 1).z));
                    ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", Np2, V(n + 2).x, V(n + 2).y, V(n + 2).z));
                    // ShowContinueError(state, format("...theta angle=[{:.6R}]", Theta));
                    // ShowContinueError(state, format("...last theta angle=[{:.6R}]", LastTheta));
                }
                surf.IsConvex = false;
                // #10103 - We do not want to break early, because we do want to consistently remove colinear vertices
                // to avoid potential vertex size mismatch fatal errors
                // break;
            }
            PrevSignFlag = SignFlag;
            LastTheta = Theta;
        }

        // must check to make sure don't remove NSides below 3
        int M = surfCollinearVerts.size();
        if (M > 0) { // Remove the collinear points determined above
            if (NSides - M >= 3) {
                surf.Sides = NSides - M;
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     format("CheckConvexity: Surface=\"{}\" has [{}] collinear points that have been removed.", surf.Name, M));
                }
            } else { // too many
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, format("CheckConvexity: Surface=\"{}\" has [{}] collinear points.", surf.Name, M));
                    ShowContinueError(state, "...too many to remove all.  Will leave the surface with 3 sides. But this is now a degenerate surface");
                }
                ++state.dataErrTracking->TotalDegenerateSurfaces;
                surf.Sides = 3; // max(NSides - M, 3) = 3 since NSide - M is < 3;
                surfCollinearVerts.resize(NSides - 3);
            }

            // remove duplicated points: For that we construct a new array of vertices, only copying indices that aren't in SurfCollinearVerts
            // Then we move that array into the original one
            Array1D<Vector3<Real64>> newVertices;
            newVertices.allocate(surf.Sides);

            int n = 0;
            for (int i = 1; i <= NSides; ++i) {
                if (std::find(surfCollinearVerts.cbegin(), surfCollinearVerts.cend(), i) == surfCollinearVerts.cend()) {
                    newVertices(++n) = surf.Vertex(i);
                }
            }
            surf.Vertex = std::move(newVertices);

            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state,
                                 format("CheckConvexity: Surface=\"{}\": The vertex points has been reprocessed as Sides = {}",
                                        surf.Name,
                                        surf.Sides));
            }
        }
    } // CheckConvexity()

    bool isRectangle(EnergyPlusData &state, int const ThisSurf // Surface number
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         M.J. Witte
        //       DATE WRITTEN   October 2015

        // PURPOSE: Check if a 4-sided surface is a rectangle
        Real64 const cos89deg = std::cos(89.0 * Constant::DegToRadians); // tolerance for right angle

        auto &surf = state.dataSurface->Surface(ThisSurf);
        Real64 Diagonal1 = length(surf.Vertex(1) - surf.Vertex(3));
        Real64 Diagonal2 = length(surf.Vertex(2) - surf.Vertex(4));
        // Test for rectangularity
        if (std::abs(Diagonal1 - Diagonal2) >= 0.020)  // This tolerance based on coincident vertex tolerance of 0.01
            return false;
        
        Vector3<Real64> Vect32 = surf.Vertex(3) - surf.Vertex(2);
        Vector3<Real64> Vect21 = surf.Vertex(2) - surf.Vertex(1);
        Vect32.normalize_zero();
        Vect21.normalize_zero();
        Real64 DotProd = dot(Vect32, Vect21);
        return (std::abs(DotProd) <= cos89deg);
    } // isRectangle()

    void MakeEquivalentRectangle(EnergyPlusData &state,
                                 int const SurfNum, // Surface number
                                 bool &ErrorsFound  // Error flag indicator (true if errors found)
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Zhang, LBNL
        //       DATE WRITTEN   September 2016
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Processing of 4-sided but non-rectangular Window, Door or GlassDoor.
        // Calculate the effective height and width of the surface.
        //
        // METHODOLOGY EMPLOYED:
        // Transform the surface into an equivalent rectangular surface with the same area and aspect ratio.

        if (SurfNum == 0) {
            // invalid surface
            ErrorsFound = true;
            return;
        }

        auto &surf = state.dataSurface->Surface(SurfNum);
        if (surf.Sides != 4 || isRectangle(state, SurfNum)) 
            // no need to transform
            return;
 
        // Calculate WidthMax and HeightMax
        Real64 WidthMax = 0.0;
        Real64 HeightMax = 0.0;
        for (int i = 1; i < surf.Sides; ++i) {
            for (int j = i + 1; j <= surf.Sides; ++j) {

                Vector3<Real64> p = surf.Vertex(j) - surf.Vertex(i);
                Vector3<Real64> LLC;

                LLC.x = -p.x * surf.CosAzim + p.y * surf.SinAzim;
                LLC.y = -p.x * surf.SinAzim * surf.CosTilt - p.y * surf.CosAzim * surf.CosTilt + p.z * surf.SinTilt;
                LLC.z = p.x * surf.SinAzim * surf.SinTilt + p.y * surf.CosAzim * surf.SinTilt + p.z * surf.CosTilt;

                if (std::abs(LLC.x) > WidthMax) WidthMax = std::abs(LLC.x);
                if (std::abs(LLC.y) > WidthMax) HeightMax = std::abs(LLC.y);
            }
        }

        // Perform transformation by calculating WidthEff and HeightEff
        Real64 AspectRatio = ((WidthMax > 0) && (HeightMax > 0)) ? WidthMax / HeightMax : 1;

        surf.Width = std::sqrt(surf.Area * AspectRatio);
        surf.Height = std::sqrt(surf.Area / AspectRatio);
    } // MakeEquivalentRectangle()

    void CheckForReversedLayers(EnergyPlusData &state,
                                bool &RevLayerDiffs,    // true when differences are discovered in interzone constructions
                                int const ConstrNum,    // construction index
                                int const ConstrNumRev, // construction index for reversed construction
                                int const TotalLayers   // total layers for construction definition
    )
    {
        RevLayerDiffs = false;

        for (int LayerNo = 1; LayerNo <= TotalLayers; ++LayerNo) {

            int thisConstLayer = state.dataConstruction->Construct(ConstrNum).LayerPoint(LayerNo);
            int revConstLayer = state.dataConstruction->Construct(ConstrNumRev).LayerPoint(TotalLayers - LayerNo + 1);

            auto *thisMatLay = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(thisConstLayer));
            assert(thisMatLay != nullptr);
            auto *revMatLay = dynamic_cast<Material::MaterialChild *>(state.dataMaterial->Material(revConstLayer));
            assert(revMatLay != nullptr);
            if ((thisConstLayer != revConstLayer) ||                   // Not pointing to the same layer
                (thisMatLay->group == Material::Group::WindowGlass) || // Not window glass or glass equivalent layer which have
                (revMatLay->group == Material::Group::WindowGlass) ||  // to have certain properties flipped from front to back
                (thisMatLay->group == Material::Group::GlassEquivalentLayer) || (revMatLay->group == Material::Group::GlassEquivalentLayer)) {
                // If not point to the same layer, check to see if this is window glass which might need to have
                // front and back material properties reversed.
                Real64 constexpr SmallDiff = 0.0001;
                if ((thisMatLay->group == Material::Group::WindowGlass) && (revMatLay->group == Material::Group::WindowGlass)) {
                    // Both layers are window glass, so need to check to see if the properties are reversed
                    if ((abs(thisMatLay->Thickness - revMatLay->Thickness) > SmallDiff) ||
                        (abs(thisMatLay->ReflectSolBeamBack - revMatLay->ReflectSolBeamFront) > SmallDiff) ||
                        (abs(thisMatLay->ReflectSolBeamFront - revMatLay->ReflectSolBeamBack) > SmallDiff) ||
                        (abs(thisMatLay->TransVis - revMatLay->TransVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflectVisBeamBack - revMatLay->ReflectVisBeamFront) > SmallDiff) ||
                        (abs(thisMatLay->ReflectVisBeamFront - revMatLay->ReflectVisBeamBack) > SmallDiff) ||
                        (abs(thisMatLay->TransThermal - revMatLay->TransThermal) > SmallDiff) ||
                        (abs(thisMatLay->AbsorpThermalBack - revMatLay->AbsorpThermalFront) > SmallDiff) ||
                        (abs(thisMatLay->AbsorpThermalFront - revMatLay->AbsorpThermalBack) > SmallDiff) ||
                        (abs(thisMatLay->Conductivity - revMatLay->Conductivity) > SmallDiff) ||
                        (abs(thisMatLay->GlassTransDirtFactor - revMatLay->GlassTransDirtFactor) > SmallDiff) ||
                        (thisMatLay->SolarDiffusing != revMatLay->SolarDiffusing) ||
                        (abs(thisMatLay->YoungModulus - revMatLay->YoungModulus) > SmallDiff) ||
                        (abs(thisMatLay->PoissonsRatio - revMatLay->PoissonsRatio) > SmallDiff)) {
                        RevLayerDiffs = true;
                        break; // exit when diff
                    }          // If none of the above conditions is met, then these should be the same layers in reverse (RevLayersDiffs = false)
                } else if ((thisMatLay->group == Material::Group::GlassEquivalentLayer) &&
                           (revMatLay->group == Material::Group::GlassEquivalentLayer)) {
                    if ((abs(thisMatLay->TausBackBeamBeam - revMatLay->TausFrontBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay->TausFrontBeamBeam - revMatLay->TausBackBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackBeamBeam - revMatLay->ReflFrontBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontBeamBeam - revMatLay->ReflBackBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay->TausBackBeamBeamVis - revMatLay->TausFrontBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay->TausFrontBeamBeamVis - revMatLay->TausBackBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackBeamBeamVis - revMatLay->ReflFrontBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontBeamBeamVis - revMatLay->ReflBackBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay->TausBackBeamDiff - revMatLay->TausFrontBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay->TausFrontBeamDiff - revMatLay->TausBackBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackBeamDiff - revMatLay->ReflFrontBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontBeamDiff - revMatLay->ReflBackBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay->TausBackBeamDiffVis - revMatLay->TausFrontBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->TausFrontBeamDiffVis - revMatLay->TausBackBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackBeamDiffVis - revMatLay->ReflFrontBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontBeamDiffVis - revMatLay->ReflBackBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->TausDiffDiff - revMatLay->TausDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackDiffDiff - revMatLay->ReflFrontDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontDiffDiff - revMatLay->ReflBackDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay->TausDiffDiffVis - revMatLay->TausDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflBackDiffDiffVis - revMatLay->ReflFrontDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->ReflFrontDiffDiffVis - revMatLay->ReflBackDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay->TausThermal - revMatLay->TausThermal) > SmallDiff) ||
                        (abs(thisMatLay->EmissThermalBack - revMatLay->EmissThermalFront) > SmallDiff) ||
                        (abs(thisMatLay->EmissThermalFront - revMatLay->EmissThermalBack) > SmallDiff) ||
                        (abs(thisMatLay->Resistance - revMatLay->Resistance) > SmallDiff)) {
                        RevLayerDiffs = true;
                        break; // exit when diff
                    }          // If none of the above conditions is met, then these should be the same layers in reverse (RevLayersDiffs = false)
                } else {
                    // Other material types do not have reversed constructions so if they are not the same layer there is a problem
                    // (RevLayersDiffs = true)
                    RevLayerDiffs = true;
                    break; // exit when diff
                }          // End check of whether or not these are WindowGlass
            }              // else: thisConstLayer is the same as revConstLayer--so there is no problem (RevLayersDiffs = false)
        }
    }

    void GetGeoSummaryRoof(EnergyPlusData &state, GeoSummary &geoSummaryRoof)
    {
        std::vector<Vector3<Real64>> uniqueRoofVertices;
        std::vector<SurfaceGeometry::EdgeOfSurf> uniqEdgeOfSurfs; // I'm only partially using this
        for (const auto &surface : state.dataSurface->Surface) {

            if (surface.ExtBoundCond != ExternalEnvironment) {
                continue;
            }
            if (!surface.HeatTransSurf) {
                continue;
            }

            if (surface.Tilt > 45.0) { // TODO Double check tilt wrt outside vs inside?
                continue;
            }

            Real64 const z_min(minval(surface.Vertex, &Vector::z));
            Real64 const z_max(maxval(surface.Vertex, &Vector::z));
            Real64 const verticalHeight = z_max - z_min;
            geoSummaryRoof.Height += verticalHeight * surface.Area;
            geoSummaryRoof.Tilt += surface.Tilt * surface.Area;
            geoSummaryRoof.Azimuth += surface.Azimuth * surface.Area;
            geoSummaryRoof.Area += surface.Area;

            for (auto it = surface.Vertex.begin(); it != surface.Vertex.end(); ++it) {

                auto itnext = std::next(it);
                if (itnext == std::end(surface.Vertex)) {
                    itnext = std::begin(surface.Vertex);
                }

                auto &curVertex = *it;
                auto &nextVertex = *itnext;
                auto it2 = std::find_if(uniqueRoofVertices.begin(), uniqueRoofVertices.end(), [&curVertex](const auto &unqV) {
                    return Vectors::VecEqualTol(curVertex, unqV, 0.0127);
                });
                if (it2 == std::end(uniqueRoofVertices)) {
                    uniqueRoofVertices.emplace_back(curVertex);
                }

                SurfaceGeometry::EdgeOfSurf thisEdge;
                thisEdge.start = std::move(curVertex);
                thisEdge.end = std::move(nextVertex);
                thisEdge.count = 1;

                // Uses the custom operator== that uses isAlmostEqual3dPt internally and doesn't care about order of the start/end
                auto itEdge = std::find(uniqEdgeOfSurfs.begin(), uniqEdgeOfSurfs.end(), thisEdge);
                if (itEdge == uniqEdgeOfSurfs.end()) {
                    uniqEdgeOfSurfs.emplace_back(std::move(thisEdge));
                } else {
                    ++(itEdge->count);
                }
            }
        }

        if (geoSummaryRoof.Area > 0) {
            geoSummaryRoof.Height /= geoSummaryRoof.Area;
            geoSummaryRoof.Tilt /= geoSummaryRoof.Area;
            geoSummaryRoof.Azimuth /= geoSummaryRoof.Area;
        } else {
            geoSummaryRoof.Height = 0.0;
            geoSummaryRoof.Tilt = 0.0;
            geoSummaryRoof.Azimuth = 0.0;
        }

        // Remove the ones that are already used twice
        uniqEdgeOfSurfs.erase(
            std::remove_if(uniqEdgeOfSurfs.begin(), uniqEdgeOfSurfs.end(), [](const auto &edge) -> bool { return edge.count == 2; }),
            uniqEdgeOfSurfs.end());

        // Intersect with unique vertices as much as needed
        bool insertedVertext = true;
        while (insertedVertext) {
            insertedVertext = false;

            for (auto &edge : uniqEdgeOfSurfs) {

                // now go through all the vertices and see if they are colinear with start and end vertices
                for (const auto &testVertex : uniqueRoofVertices) {
                    if (edge.containsPoints(testVertex)) {
                        SurfaceGeometry::EdgeOfSurf newEdgeOfSurface;
                        newEdgeOfSurface.start = testVertex;
                        newEdgeOfSurface.end = edge.end;
                        edge.end = testVertex;
                        uniqEdgeOfSurfs.emplace_back(std::move(newEdgeOfSurface));
                        insertedVertext = true;
                        break;
                    }
                }
                // Break out of the loop on edges, and start again at the while
                if (insertedVertext) {
                    break;
                }
            }
        }

        // recount
        for (auto &edge : uniqEdgeOfSurfs) {
            edge.count = std::count(uniqEdgeOfSurfs.begin(), uniqEdgeOfSurfs.end(), edge);
        }

        uniqEdgeOfSurfs.erase(
            std::remove_if(uniqEdgeOfSurfs.begin(), uniqEdgeOfSurfs.end(), [](const auto &edge) -> bool { return edge.count == 2; }),
            uniqEdgeOfSurfs.end());

        geoSummaryRoof.Perimeter =
            std::accumulate(uniqEdgeOfSurfs.cbegin(), uniqEdgeOfSurfs.cend(), 0.0, [](const double &sum, const SurfaceGeometry::EdgeOfSurf &edge) {
                return sum + edge.length();
            });
    }

} // namespace SurfaceGeometry

} // namespace EnergyPlus
