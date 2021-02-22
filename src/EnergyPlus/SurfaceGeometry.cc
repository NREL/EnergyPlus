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

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <string>
#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobals.hh>
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
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/WeatherManager.hh>

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

    void AllocateSurfaceWindows(int NumSurfaces) {
        SurfWinFrameQRadOutAbs.dimension(NumSurfaces, 0);
        SurfWinFrameQRadInAbs.dimension(NumSurfaces, 0);
        SurfWinDividerQRadOutAbs.dimension(NumSurfaces, 0);
        SurfWinDividerQRadInAbs.dimension(NumSurfaces, 0);
        SurfWinExtBeamAbsByShade.dimension(NumSurfaces, 0);
        SurfWinExtDiffAbsByShade.dimension(NumSurfaces, 0);
        SurfWinIntBeamAbsByShade.dimension(NumSurfaces, 0);
        SurfWinIntSWAbsByShade.dimension(NumSurfaces, 0);
        SurfWinInitialDifSolAbsByShade.dimension(NumSurfaces, 0);
        SurfWinIntLWAbsByShade.dimension(NumSurfaces, 0);
        SurfWinConvHeatFlowNatural.dimension(NumSurfaces, 0);
        SurfWinConvHeatGainToZoneAir.dimension(NumSurfaces, 0);
        SurfWinRetHeatGainToZoneAir.dimension(NumSurfaces, 0);
        SurfWinDividerHeatGain.dimension(NumSurfaces, 0);
        SurfWinBlTsolBmBm.dimension(NumSurfaces, 0);
        SurfWinBlTsolBmDif.dimension(NumSurfaces, 0);
        SurfWinBlTsolDifDif.dimension(NumSurfaces, 0);
        SurfWinBlGlSysTsolBmBm.dimension(NumSurfaces, 0);
        SurfWinBlGlSysTsolDifDif.dimension(NumSurfaces, 0);
        SurfWinScTsolBmBm.dimension(NumSurfaces, 0);
        SurfWinScTsolBmDif.dimension(NumSurfaces, 0);
        SurfWinScTsolDifDif.dimension(NumSurfaces, 0);
        SurfWinScGlSysTsolBmBm.dimension(NumSurfaces, 0);
        SurfWinScGlSysTsolDifDif.dimension(NumSurfaces, 0);
        SurfWinGlTsolBmBm.dimension(NumSurfaces, 0);
        SurfWinGlTsolBmDif.dimension(NumSurfaces, 0);
        SurfWinGlTsolDifDif.dimension(NumSurfaces, 0);
        SurfWinBmSolTransThruIntWinRep.dimension(NumSurfaces, 0);
        SurfWinBmSolAbsdOutsReveal.dimension(NumSurfaces, 0);
        SurfWinBmSolRefldOutsRevealReport.dimension(NumSurfaces, 0);
        SurfWinBmSolAbsdInsReveal.dimension(NumSurfaces, 0);
        SurfWinBmSolRefldInsReveal.dimension(NumSurfaces, 0);
        SurfWinBmSolRefldInsRevealReport.dimension(NumSurfaces, 0);
        SurfWinOutsRevealDiffOntoGlazing.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffOntoGlazing.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffIntoZone.dimension(NumSurfaces, 0);
        SurfWinOutsRevealDiffOntoFrame.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffOntoFrame.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffOntoGlazingReport.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffIntoZoneReport.dimension(NumSurfaces, 0);
        SurfWinInsRevealDiffOntoFrameReport.dimension(NumSurfaces, 0);
        SurfWinBmSolAbsdInsRevealReport.dimension(NumSurfaces, 0);
        SurfWinBmSolTransThruIntWinRepEnergy.dimension(NumSurfaces, 0);
        SurfWinBmSolRefldOutsRevealRepEnergy.dimension(NumSurfaces, 0);
        SurfWinBmSolRefldInsRevealRepEnergy.dimension(NumSurfaces, 0);
        SurfWinProfileAngHor.dimension(NumSurfaces, 0);
        SurfWinProfileAngVert.dimension(NumSurfaces, 0);

        SurfWinShadingFlag.dimension(NumSurfaces, 0);
        SurfWinShadingFlagEMSOn.dimension(NumSurfaces, 0);
        SurfWinShadingFlagEMSValue.dimension(NumSurfaces, 0);
        SurfWinStormWinFlag.dimension(NumSurfaces, 0);
        SurfWinStormWinFlagPrevDay.dimension(NumSurfaces, 0);
        SurfWinFracTimeShadingDeviceOn.dimension(NumSurfaces, 0);
        SurfWinExtIntShadePrevTS.dimension(NumSurfaces, 0);
        SurfWinHasShadeOrBlindLayer.dimension(NumSurfaces, 0);
        SurfWinSurfDayLightInit.dimension(NumSurfaces, 0);
        SurfWinDaylFacPoint.dimension(NumSurfaces, 0);
        SurfWinVisTransSelected.dimension(NumSurfaces, 0);
        SurfWinSwitchingFactor.dimension(NumSurfaces, 0);
        SurfWinTheta.dimension(NumSurfaces, 0);
        SurfWinPhi.dimension(NumSurfaces, 0);
        SurfWinRhoCeilingWall.dimension(NumSurfaces, 0);
        SurfWinRhoFloorWall.dimension(NumSurfaces, 0);
        SurfWinFractionUpgoing.dimension(NumSurfaces, 0);
        SurfWinVisTransRatio.dimension(NumSurfaces, 0);
        SurfWinIRfromParentZone.dimension(NumSurfaces, 0);
        SurfWinFrameArea.dimension(NumSurfaces, 0);
        SurfWinFrameConductance.dimension(NumSurfaces, 0);
        SurfWinFrameSolAbsorp.dimension(NumSurfaces, 0);
        SurfWinFrameVisAbsorp.dimension(NumSurfaces, 0);
        SurfWinFrameEmis.dimension(NumSurfaces, 0);
        SurfWinFrEdgeToCenterGlCondRatio.dimension(NumSurfaces, 1.0);
        SurfWinFrameEdgeArea.dimension(NumSurfaces, 0);
        SurfWinFrameTempSurfIn.dimension(NumSurfaces, 23.0);
        SurfWinFrameTempSurfInOld.dimension(NumSurfaces, 23.0);
        SurfWinFrameTempSurfOut.dimension(NumSurfaces, 23.0);
        SurfWinProjCorrFrOut.dimension(NumSurfaces, 0);
        SurfWinProjCorrFrIn.dimension(NumSurfaces, 0);
        SurfWinDividerType.dimension(NumSurfaces, 0);
        SurfWinDividerArea.dimension(NumSurfaces, 0);
        SurfWinDividerConductance.dimension(NumSurfaces, 0);
        SurfWinDividerSolAbsorp.dimension(NumSurfaces, 0);
        SurfWinDividerVisAbsorp.dimension(NumSurfaces, 0);
        SurfWinDividerEmis.dimension(NumSurfaces, 0);
        SurfWinDivEdgeToCenterGlCondRatio.dimension(NumSurfaces, 1);
        SurfWinDividerEdgeArea.dimension(NumSurfaces, 0);
        SurfWinDividerTempSurfIn.dimension(NumSurfaces, 23.0);
        SurfWinDividerTempSurfInOld.dimension(NumSurfaces, 23.0);
        SurfWinDividerTempSurfOut.dimension(NumSurfaces, 23.0);
        SurfWinProjCorrDivOut.dimension(NumSurfaces, 0);
        SurfWinProjCorrDivIn.dimension(NumSurfaces, 0);
        SurfWinGlazedFrac.dimension(NumSurfaces, 1);
        SurfWinCenterGlArea.dimension(NumSurfaces, 0);
        SurfWinEdgeGlCorrFac.dimension(NumSurfaces, 1);
        SurfWinOriginalClass.dimension(NumSurfaces, SurfaceClass::None);
        SurfWinShadeAbsFacFace1.dimension(NumSurfaces, 0.5);
        SurfWinShadeAbsFacFace2.dimension(NumSurfaces, 0.5);
        SurfWinConvCoeffWithShade.dimension(NumSurfaces, 0);
        SurfWinOtherConvHeatGain.dimension(NumSurfaces, 0);
        SurfWinBlindNumber.dimension(NumSurfaces, 0);
        SurfWinEffInsSurfTemp.dimension(NumSurfaces, 23.0);
        SurfWinMovableSlats.dimension(NumSurfaces, 0);
        SurfWinSlatAngThisTS.dimension(NumSurfaces, 0);
        SurfWinSlatAngThisTSDeg.dimension(NumSurfaces, 0);
        SurfWinSlatAngThisTSDegEMSon.dimension(NumSurfaces, 0);
        SurfWinSlatAngThisTSDegEMSValue.dimension(NumSurfaces, 0);
        SurfWinSlatsBlockBeam.dimension(NumSurfaces, 0);
        SurfWinBlindAirFlowPermeability.dimension(NumSurfaces, 0);
        SurfWinTotGlazingThickness.dimension(NumSurfaces, 0);
        SurfWinTanProfileAngHor.dimension(NumSurfaces, 0);
        SurfWinTanProfileAngVert.dimension(NumSurfaces, 0);
        SurfWinInsideSillDepth.dimension(NumSurfaces, 0);
        SurfWinInsideReveal.dimension(NumSurfaces, 0);
        SurfWinInsideSillSolAbs.dimension(NumSurfaces, 0);
        SurfWinInsideRevealSolAbs.dimension(NumSurfaces, 0);
        SurfWinOutsideRevealSolAbs.dimension(NumSurfaces, 0);
        SurfWinScreenNumber.dimension(NumSurfaces, 0);
        SurfWinAirflowSource.dimension(NumSurfaces, 0);
        SurfWinAirflowDestination.dimension(NumSurfaces, 0);
        SurfWinAirflowReturnNodePtr.dimension(NumSurfaces, 0);
        SurfWinMaxAirflow.dimension(NumSurfaces, 0);
        SurfWinAirflowControlType.dimension(NumSurfaces, 0);
        SurfWinAirflowHasSchedule.dimension(NumSurfaces, 0);
        SurfWinAirflowSchedulePtr.dimension(NumSurfaces, 0);
        SurfWinAirflowThisTS.dimension(NumSurfaces, 0);
        SurfWinTAirflowGapOutlet.dimension(NumSurfaces, 0);
        SurfWinWindowCalcIterationsRep.dimension(NumSurfaces, 0);
        SurfWinVentingOpenFactorMultRep.dimension(NumSurfaces, 0);
        SurfWinInsideTempForVentingRep.dimension(NumSurfaces, 0);
        SurfWinVentingAvailabilityRep.dimension(NumSurfaces, 0);
        SurfWinSkyGndSolarInc.dimension(NumSurfaces, 0);
        SurfWinBmGndSolarInc.dimension(NumSurfaces, 0);
        SurfWinLightWellEff.dimension(NumSurfaces, 1);
        SurfWinSolarDiffusing.dimension(NumSurfaces, 0);
        SurfWinFrameHeatGain.dimension(NumSurfaces, 0);
        SurfWinFrameHeatLoss.dimension(NumSurfaces, 0);
        SurfWinDividerHeatLoss.dimension(NumSurfaces, 0);
        SurfWinTCLayerTemp.dimension(NumSurfaces, 0);
        SurfWinSpecTemp.dimension(NumSurfaces, 0);
        SurfWinWindowModelType.dimension(NumSurfaces, Window5DetailedModel);
        SurfWinTDDPipeNum.dimension(NumSurfaces, 0);
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

        using namespace DataVectorTypes;
        using namespace OutputReportPredefined;

        using namespace DataReportingFlags;


        static std::string const RoutineName("SetUpZoneGeometry: ");

        Real64 AverageHeight; // Used to keep track of average height of a surface/zone
        int SurfNum;          // Surface number (DO loop counter)
        int ZoneNum;          // Zone number for current surface and DO loop counter
        Real64 ZMax;          // Maximum Z of a surface (detailed outside coefficient calculation)
        Real64 ZMin;          // Minimum Z of a surface (detailed outside coefficient calculation)
        Real64 ZCeilAvg;
        Real64 CeilCount;
        Real64 ZFlrAvg;
        Real64 FloorCount;
        Real64 TotSurfArea;
        Real64 Z1;
        Real64 Z2;
        std::string String1;
        std::string String2;
        std::string String3;
        int Count; // To count wall surfaces for ceiling height calculation
        Array1D_bool ZoneCeilingHeightEntered;
        Array1D<Real64> ZoneCeilingArea;
        static int ErrCount(0);
        Real64 NominalUwithConvCoeffs;
        std::string cNominalU;
        std::string cNominalUwithConvCoeffs;
        bool isWithConvCoefValid;
        bool nonInternalMassSurfacesPresent;
        bool DetailedWWR;


        // Zones must have been "gotten" before this call
        // The RelNorth variables are used if "relative" coordinates are input as well
        // as setting up DaylightingCoords

        // these include building north axis and Building Rotation for Appendix G
        state.dataSurfaceGeometry->CosBldgRelNorth = std::cos(-(BuildingAzimuth + BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
        state.dataSurfaceGeometry->SinBldgRelNorth = std::sin(-(BuildingAzimuth + BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);

        // these are only for Building Rotation for Appendix G when using world coordinate system
        state.dataSurfaceGeometry->CosBldgRotAppGonly = std::cos(-BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);
        state.dataSurfaceGeometry->SinBldgRotAppGonly = std::sin(-BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);

        state.dataSurfaceGeometry->CosZoneRelNorth.allocate(state.dataGlobal->NumOfZones);
        state.dataSurfaceGeometry->SinZoneRelNorth.allocate(state.dataGlobal->NumOfZones);

        ZoneCeilingHeightEntered.dimension(state.dataGlobal->NumOfZones, false);
        ZoneCeilingArea.dimension(state.dataGlobal->NumOfZones, 0.0);

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            state.dataSurfaceGeometry->CosZoneRelNorth(ZoneNum) = std::cos(-Zone(ZoneNum).RelNorth * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SinZoneRelNorth(ZoneNum) = std::sin(-Zone(ZoneNum).RelNorth * DataGlobalConstants::DegToRadians);
        }
        GetSurfaceData(state, ErrorsFound);

        if (ErrorsFound) {
            state.dataSurfaceGeometry->CosZoneRelNorth.deallocate();
            state.dataSurfaceGeometry->SinZoneRelNorth.deallocate();
            return;
        }

        GetWindowGapAirflowControlData(state, ErrorsFound);

        GetStormWindowData(state, ErrorsFound);

        if (!ErrorsFound && TotStormWin > 0) CreateStormWindowConstructions(state);

        SetFlagForWindowConstructionWithShadeOrBlindLayer(state);

        state.dataSurfaceGeometry->CosZoneRelNorth.deallocate();
        state.dataSurfaceGeometry->SinZoneRelNorth.deallocate();

        AllocateModuleArrays(state); // This needs to be moved to the main manager routine of SSG at a later date

        AirSkyRadSplit.dimension(TotSurfaces, 0.0);

        CalcWindowRevealReflection = false; // Set to True in ProcessSurfaceVertices if beam solar reflection from window reveals
        // is requested for one or more exterior windows.
        BuildingShadingCount = 0;
        FixedShadingCount = 0;
        AttachedShadingCount = 0;
        ShadingSurfaceFirst = -1;
        ShadingSurfaceLast = -1;

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) { // Loop through all surfaces...

            AirSkyRadSplit(SurfNum) = std::sqrt(0.5 * (1.0 + Surface(SurfNum).CosTilt));

            // Set flag that determines whether a surface is a shadowing surface
            Surface(SurfNum).ShadowingSurf = false;
            if (Surface(SurfNum).Class == SurfaceClass::Shading || Surface(SurfNum).Class == SurfaceClass::Detached_F ||
                Surface(SurfNum).Class == SurfaceClass::Detached_B) {
                Surface(SurfNum).ShadowingSurf = true;
                if (ShadingSurfaceFirst == -1) ShadingSurfaceFirst = SurfNum;
                ShadingSurfaceLast = SurfNum;
            }
            if (Surface(SurfNum).Class == SurfaceClass::Shading) ++AttachedShadingCount;
            if (Surface(SurfNum).Class == SurfaceClass::Detached_F) ++FixedShadingCount;
            if (Surface(SurfNum).Class == SurfaceClass::Detached_B) ++BuildingShadingCount;

            if (Surface(SurfNum).Class != SurfaceClass::IntMass) ProcessSurfaceVertices(state, SurfNum, ErrorsFound);
        }

        for (auto &e : Zone) {
            e.ExtWindowArea = 0.0;
            e.HasInterZoneWindow = false;
            e.HasWindow = false;
            e.ExtGrossWallArea = 0.0;
            e.ExtNetWallArea = 0.0;
            e.TotalSurfArea = 0.0;
        }

        DetailedWWR = (inputProcessor->getNumSectionsFound("DETAILEDWWR_DEBUG") > 0);
        if (DetailedWWR) {
            print(state.files.debug, "{}", "=======User Entered Classification =================");
            print(state.files.debug, "{}", "Surface,Class,Area,Tilt");
        }

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) { // Loop through all surfaces to find windows...

            if (!Surface(SurfNum).HeatTransSurf && !Surface(SurfNum).IsAirBoundarySurf) continue; // Skip shadowing (sub)surfaces
            ZoneNum = Surface(SurfNum).Zone;
            Zone(ZoneNum).TotalSurfArea += Surface(SurfNum).Area;
            if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) {
                Zone(ZoneNum).TotalSurfArea += SurfWinFrameArea(SurfNum);
                Zone(ZoneNum).HasWindow = true;
            }
            if (Surface(SurfNum).Class == SurfaceClass::Roof) ZoneCeilingArea(ZoneNum) += Surface(SurfNum).Area;
            if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) {
                if (Surface(SurfNum).ExtBoundCond == ExternalEnvironment || Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                    Zone(ZoneNum).ExteriorTotalSurfArea += Surface(SurfNum).GrossArea;
                    if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                        Zone(ZoneNum).ExtNetWallArea += Surface(SurfNum).Area;
                        Zone(ZoneNum).ExtGrossWallArea += Surface(SurfNum).GrossArea;
                        Zone(ZoneNum).ExtGrossWallArea_Multiplied +=
                            Surface(SurfNum).GrossArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
                        if (DetailedWWR) {
                            print(state.files.debug,
                                  "{},Wall,{:.2R},{:.1R}\n",
                                  Surface(SurfNum).Name,
                                  Surface(SurfNum).GrossArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier,
                                  Surface(SurfNum).Tilt);
                        }
                    }
                } else if (Surface(SurfNum).ExtBoundCond == Ground || Surface(SurfNum).ExtBoundCond == GroundFCfactorMethod ||
                           Surface(SurfNum).ExtBoundCond == KivaFoundation) {
                    Zone(ZoneNum).ExteriorTotalGroundSurfArea += Surface(SurfNum).GrossArea;
                    if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                        Zone(ZoneNum).ExtGrossGroundWallArea += Surface(SurfNum).GrossArea;
                        Zone(ZoneNum).ExtGrossGroundWallArea_Multiplied +=
                            Surface(SurfNum).GrossArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
                        if (DetailedWWR) {
                            print(state.files.debug,
                                  "{},Wall-GroundContact,{:.2R},{:.1R}\n",
                                  Surface(SurfNum).Name,
                                  Surface(SurfNum).GrossArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier,
                                  Surface(SurfNum).Tilt);
                        }
                    }
                }

            } else { // For Windows

                if ((Surface(SurfNum).ExtBoundCond > 0) && (Surface(SurfNum).BaseSurf != SurfNum)) { // Interzone window present
                    Zone(Surface(SurfNum).Zone).HasInterZoneWindow = true;
                } else {
                    if (((Surface(SurfNum).ExtBoundCond == ExternalEnvironment) || (Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt)) &&
                        (Surface(SurfNum).Class != SurfaceClass::TDD_Dome)) {
                        Zone(Surface(SurfNum).Zone).ExtWindowArea += Surface(SurfNum).GrossArea;
                        Zone(Surface(SurfNum).Zone).ExtWindowArea_Multiplied =
                            Zone(Surface(SurfNum).Zone).ExtWindowArea +
                            Surface(SurfNum).GrossArea * Surface(SurfNum).Multiplier * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
                        if (DetailedWWR) {
                            print(state.files.debug,
                                  "{},Window,{:.2R},{:.1R}\n",
                                  Surface(SurfNum).Name,
                                  Surface(SurfNum).GrossArea * Surface(SurfNum).Multiplier * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier,
                                  Surface(SurfNum).Tilt);
                        }
                    }
                }
            }

        } // ...end of surfaces windows DO loop

        if (DetailedWWR) {
            print(state.files.debug, "{}\n", "========================");
            print(state.files.debug, "{}\n", "Zone,ExtWallArea,ExtWindowArea");
        }

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            CeilCount = 0.0;
            FloorCount = 0.0;
            Count = 0;
            AverageHeight = 0.0;
            ZCeilAvg = 0.0;
            ZFlrAvg = 0.0;
            ZMax = -99999.0;
            ZMin = 99999.0;
            if (DetailedWWR) {
                print(state.files.debug, "{},{:.2R},{:.2R}\n", Zone(ZoneNum).Name, Zone(ZoneNum).ExtGrossWallArea,
                                                                    Zone(ZoneNum).ExtWindowArea);
            }
            // Use AllSurfaceFirst which includes air boundaries
            for (SurfNum = Zone(ZoneNum).AllSurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                    // Use Average Z for surface, more important for roofs than floors...
                    ++CeilCount;
                    Z1 = minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    Z2 = maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    //        ZCeilAvg=ZCeilAvg+(Z1+Z2)/2.d0
                    ZCeilAvg += ((Z1 + Z2) / 2.0) * (Surface(SurfNum).Area / ZoneCeilingArea(ZoneNum));
                }
                if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                    // Use Average Z for surface, more important for roofs than floors...
                    ++FloorCount;
                    Z1 = minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    Z2 = maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z);
                    //        ZFlrAvg=ZFlrAvg+(Z1+Z2)/2.d0
                    ZFlrAvg += ((Z1 + Z2) / 2.0) * (Surface(SurfNum).Area / Zone(ZoneNum).FloorArea);
                }
                if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                    // Use Wall calculation in case no roof & floor in zone
                    ++Count;
                    if (Count == 1) {
                        ZMax = Surface(SurfNum).Vertex(1).z;
                        ZMin = ZMax;
                    }
                    ZMax = max(ZMax, maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z));
                    ZMin = min(ZMin, minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z));
                }
            }
            if (CeilCount > 0.0 && FloorCount > 0.0) {
                AverageHeight = ZCeilAvg - ZFlrAvg;
            } else {
                AverageHeight = (ZMax - ZMin);
            }
            if (AverageHeight <= 0.0) {
                AverageHeight = (ZMax - ZMin);
            }

            if (Zone(ZoneNum).CeilingHeight > 0.0) {
                ZoneCeilingHeightEntered(ZoneNum) = true;
                if (AverageHeight > 0.0) {
                    if (std::abs(AverageHeight - Zone(ZoneNum).CeilingHeight) / Zone(ZoneNum).CeilingHeight > 0.05) {
                        if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, RoutineName +
                                             "Entered Ceiling Height for some zone(s) significantly different from calculated Ceiling Height");
                            ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, RoutineName + "Entered Ceiling Height for Zone=\"" + Zone(ZoneNum).Name +
                                             "\" significantly different from calculated Ceiling Height");
                            static constexpr auto ValFmt("{:.2F}");
                            String1 = format(ValFmt, Zone(ZoneNum).CeilingHeight);
                            String2 = format(ValFmt, AverageHeight);
                            ShowContinueError(state, RoutineName + "Entered Ceiling Height=" + String1 + ", Calculated Ceiling Height=" + String2 +
                                              ", entered height will be used in calculations.");
                        }
                    }
                }
            }
            if ((Zone(ZoneNum).CeilingHeight <= 0.0) && (AverageHeight > 0.0)) Zone(ZoneNum).CeilingHeight = AverageHeight;
        }

        CalculateZoneVolume(state, ZoneCeilingHeightEntered); // Calculate Zone Volumes

        // Calculate zone centroid (and min/max x,y,z for zone)
        // Use AllSurfaceFirst which includes air boundaries
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            nonInternalMassSurfacesPresent = false;
            TotSurfArea = 0.0;
            Zone(ZoneNum).Centroid = Vector(0.0, 0.0, 0.0);
            if (Surface(Zone(ZoneNum).AllSurfaceFirst).Sides > 0) {
                Zone(ZoneNum).MinimumX = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).x;
                Zone(ZoneNum).MaximumX = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).x;
                Zone(ZoneNum).MinimumY = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).y;
                Zone(ZoneNum).MaximumY = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).y;
                Zone(ZoneNum).MinimumZ = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).z;
                Zone(ZoneNum).MaximumZ = Surface(Zone(ZoneNum).AllSurfaceFirst).Vertex(1).z;
            }
            for (SurfNum = Zone(ZoneNum).AllSurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                if (Surface(SurfNum).Class == SurfaceClass::IntMass) continue;
                nonInternalMassSurfacesPresent = true;
                if (Surface(SurfNum).Class == SurfaceClass::Wall || (Surface(SurfNum).Class == SurfaceClass::Roof) ||
                    (Surface(SurfNum).Class == SurfaceClass::Floor)) {

                    Zone(ZoneNum).Centroid.x += Surface(SurfNum).Centroid.x * Surface(SurfNum).GrossArea;
                    Zone(ZoneNum).Centroid.y += Surface(SurfNum).Centroid.y * Surface(SurfNum).GrossArea;
                    Zone(ZoneNum).Centroid.z += Surface(SurfNum).Centroid.z * Surface(SurfNum).GrossArea;
                    TotSurfArea += Surface(SurfNum).GrossArea;
                }
                Zone(ZoneNum).MinimumX = min(Zone(ZoneNum).MinimumX, minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::x));
                Zone(ZoneNum).MaximumX = max(Zone(ZoneNum).MaximumX, maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::x));
                Zone(ZoneNum).MinimumY = min(Zone(ZoneNum).MinimumY, minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::y));
                Zone(ZoneNum).MaximumY = max(Zone(ZoneNum).MaximumY, maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::y));
                Zone(ZoneNum).MinimumZ = min(Zone(ZoneNum).MinimumZ, minval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z));
                Zone(ZoneNum).MaximumZ = max(Zone(ZoneNum).MaximumZ, maxval(Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides}), &Vector::z));
            }
            if (TotSurfArea > 0.0) {
                Zone(ZoneNum).Centroid.x /= TotSurfArea;
                Zone(ZoneNum).Centroid.y /= TotSurfArea;
                Zone(ZoneNum).Centroid.z /= TotSurfArea;
            }
            if (!nonInternalMassSurfacesPresent) {
                ShowSevereError(state, RoutineName + "Zone=\"" + Zone(ZoneNum).Name +
                                "\" has only internal mass surfaces.  Need at least one other surface.");
                ErrorsFound = true;
            }
        }

        ZoneCeilingHeightEntered.deallocate();
        ZoneCeilingArea.deallocate();

        AdjacentZoneToSurface.dimension(TotSurfaces, 0);
        // note -- adiabatic surfaces will show same zone as surface
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (Surface(SurfNum).ExtBoundCond <= 0) continue;
            AdjacentZoneToSurface(SurfNum) = Surface(Surface(SurfNum).ExtBoundCond).Zone;
        }

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ZoneName == Zone(ZoneNum).Name) ++Zone(ZoneNum).NumShadingSurfaces;

                if (Surface(SurfNum).Zone != ZoneNum) continue;

                if (Surface(SurfNum).HeatTransSurf && (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Roof ||
                                                       Surface(SurfNum).Class == SurfaceClass::Floor))
                    ++Zone(ZoneNum).NumSurfaces;

                if (Surface(SurfNum).HeatTransSurf &&
                    (Surface(SurfNum).Class == SurfaceClass::Window || Surface(SurfNum).Class == SurfaceClass::GlassDoor ||
                     Surface(SurfNum).Class == SurfaceClass::Door || Surface(SurfNum).Class == SurfaceClass::TDD_Dome ||
                     Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser))
                    ++Zone(ZoneNum).NumSubSurfaces;

            } // surfaces
        }     // zones

        for (int SurfNum : DataSurfaces::AllSurfaceListReportOrder) {
            if (Surface(SurfNum).Construction > 0 && Surface(SurfNum).Construction <= TotConstructs) {
                NominalUwithConvCoeffs = ComputeNominalUwithConvCoeffs(SurfNum, isWithConvCoefValid);
                if (isWithConvCoefValid) {
                    cNominalUwithConvCoeffs = format("{:.3R}", NominalUwithConvCoeffs);
                } else {
                    cNominalUwithConvCoeffs = "[invalid]";
                }
                if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::TDD_Dome)) {
                    // SurfaceClass::Window also covers glass doors and TDD:Diffusers
                    cNominalU = "N/A";
                } else {
                    cNominalU = format("{:.3R}", NominalU(Surface(SurfNum).Construction));
                }
            } else {
                cNominalUwithConvCoeffs = "**";
                cNominalU = "**";
            }

            // save the U-value nominal for use later in tabular report
            Surface(SurfNum).UNomWOFilm = cNominalU;
            Surface(SurfNum).UNomFilm = cNominalUwithConvCoeffs;
            // populate the predefined report related to u-values with films
            // only exterior surfaces including underground
            auto const SurfaceClass(Surface(SurfNum).Class);
            if ((Surface(SurfNum).ExtBoundCond == ExternalEnvironment) || (Surface(SurfNum).ExtBoundCond == Ground) ||
                (Surface(SurfNum).ExtBoundCond == KivaFoundation) || (Surface(SurfNum).ExtBoundCond == GroundFCfactorMethod)) {
                if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchOpUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDrUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                }
            }else{
                if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntOpUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchIntDrUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                }
            }
        } // surfaces

        // Write number of shadings to initialization output file
        print(state.files.eio, "! <Shading Summary>, Number of Fixed Detached Shades, Number of Building Detached Shades, Number of Attached Shades\n");

        print(state.files.eio, " Shading Summary,{},{},{}\n", FixedShadingCount, BuildingShadingCount, AttachedShadingCount);

        // Write number of zones header to initialization output file
        print(state.files.eio, "! <Zone Summary>, Number of Zones, Number of Zone Surfaces, Number of SubSurfaces\n");

        print(state.files.eio,
              " Zone Summary,{},{},{}\n",
              state.dataGlobal->NumOfZones,
              TotSurfaces - FixedShadingCount - BuildingShadingCount - AttachedShadingCount,
              sum(Zone, &ZoneData::NumSubSurfaces));

        // Write Zone Information header to the initialization output file
        static constexpr auto Format_721(
            "! <Zone Information>,Zone Name,North Axis {deg},Origin X-Coordinate {m},Origin Y-Coordinate {m},Origin Z-Coordinate "
            "{m},Centroid X-Coordinate {m},Centroid Y-Coordinate {m},Centroid Z-Coordinate {m},Type,Zone Multiplier,Zone List "
            "Multiplier,Minimum X {m},Maximum X {m},Minimum Y {m},Maximum Y {m},Minimum Z {m},Maximum Z {m},Ceiling Height {m},Volume "
            "{m3},Zone Inside Convection Algorithm {Simple-Detailed-CeilingDiffuser-TrombeWall},Zone Outside Convection Algorithm "
            "{Simple-Detailed-Tarp-MoWitt-DOE-2-BLAST}, Floor Area {m2},Exterior Gross Wall Area {m2},Exterior Net Wall Area {m2},Exterior Window "
            "Area {m2}, Number of Surfaces, Number of SubSurfaces, Number of Shading SubSurfaces,  Part of Total Building Area");
        print(state.files.eio, "{}\n", Format_721);

        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            // Write Zone Information to the initialization output file

            {
                auto const SELECT_CASE_var(Zone(ZoneNum).InsideConvectionAlgo);
                if (SELECT_CASE_var == ASHRAESimple) {
                    String1 = "Simple";
                } else if (SELECT_CASE_var == ASHRAETARP) {
                    String1 = "TARP";
                } else if (SELECT_CASE_var == CeilingDiffuser) {
                    String1 = "CeilingDiffuser";
                } else if (SELECT_CASE_var == TrombeWall) {
                    String1 = "TrombeWall";
                } else if (SELECT_CASE_var == AdaptiveConvectionAlgorithm) {
                    String1 = "AdaptiveConvectionAlgorithm";
                } else if (SELECT_CASE_var == ASTMC1340) {
                    String1 = "ASTMC1340";
                }
            }

            {
                auto const SELECT_CASE_var(Zone(ZoneNum).OutsideConvectionAlgo);
                if (SELECT_CASE_var == ASHRAESimple) {
                    String2 = "Simple";
                } else if (SELECT_CASE_var == ASHRAETARP) {
                    String2 = "TARP";
                } else if (SELECT_CASE_var == TarpHcOutside) {
                    String2 = "TARP";
                } else if (SELECT_CASE_var == MoWiTTHcOutside) {
                    String2 = "MoWitt";
                } else if (SELECT_CASE_var == DOE2HcOutside) {
                    String2 = "DOE-2";
                    //      CASE (BLASTHcOutside)
                    //        String2='BLAST'
                } else if (SELECT_CASE_var == AdaptiveConvectionAlgorithm) {
                    String2 = "AdaptiveConvectionAlgorithm";
                }
            }

            if (Zone(ZoneNum).isPartOfTotalArea) {
                String3 = "Yes";
            } else {
                String3 = "No";
            }

            static constexpr auto Format_720(" Zone Information, "
                                             "{},{:.1R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{},{},{},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},"
                                             "{:.2R},{:.2R},{},{},{:.2R},{:.2R},{:.2R},{:.2R},{},{},{},{}\n");

            print(state.files.eio,
                  Format_720,
                  Zone(ZoneNum).Name,
                  Zone(ZoneNum).RelNorth,
                  Zone(ZoneNum).OriginX,
                  Zone(ZoneNum).OriginY,
                  Zone(ZoneNum).OriginZ,
                  Zone(ZoneNum).Centroid.x,
                  Zone(ZoneNum).Centroid.y,
                  Zone(ZoneNum).Centroid.z,
                  Zone(ZoneNum).OfType,
                  Zone(ZoneNum).Multiplier,
                  Zone(ZoneNum).ListMultiplier,
                  Zone(ZoneNum).MinimumX,
                  Zone(ZoneNum).MaximumX,
                  Zone(ZoneNum).MinimumY,
                  Zone(ZoneNum).MaximumY,
                  Zone(ZoneNum).MinimumZ,
                  Zone(ZoneNum).MaximumZ,
                  Zone(ZoneNum).CeilingHeight,
                  Zone(ZoneNum).Volume,
                  String1,
                  String2,
                  Zone(ZoneNum).FloorArea,
                  Zone(ZoneNum).ExtGrossWallArea,
                  Zone(ZoneNum).ExtNetWallArea,
                  Zone(ZoneNum).ExtWindowArea,
                  Zone(ZoneNum).NumSurfaces,
                  Zone(ZoneNum).NumSubSurfaces,
                  Zone(ZoneNum).NumShadingSurfaces,
                  String3);

        } // ZoneNum

        // Set up solar distribution enclosures allowing for any air boundaries
        SetupEnclosuresAndAirBoundaries(state, DataViewFactorInformation::ZoneSolarInfo, SurfaceGeometry::enclosureType::SolarEnclosures, ErrorsFound);

        // Do the Stratosphere check
        SetZoneOutBulbTempAt(state);
        CheckZoneOutBulbTempAt(state);
    }

    void AllocateModuleArrays(EnergyPlusData &state)
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



        ShadeV.allocate(TotSurfaces);
        for (auto &e : ShadeV)
            e.NVert = 0;
        // Individual components (XV,YV,ZV) allocated in routine ProcessSurfaceVertices
        X0.dimension(TotSurfaces, 0.0);
        Y0.dimension(TotSurfaces, 0.0);
        Z0.dimension(TotSurfaces, 0.0);

        EnclSolDB.dimension(state.dataGlobal->NumOfZones, 0.0);
        EnclSolDBSSG.dimension(state.dataGlobal->NumOfZones, 0.0);
        QSDifSol.dimension(state.dataGlobal->NumOfZones, 0.0);
        SurfOpaqAI.dimension(TotSurfaces, 0.0);
        SurfOpaqAO.dimension(TotSurfaces, 0.0);
        SurfBmToBmReflFacObs.dimension(TotSurfaces, 0.0);
        SurfBmToDiffReflFacObs.dimension(TotSurfaces, 0.0);
        SurfBmToDiffReflFacGnd.dimension(TotSurfaces, 0.0);
        SurfSkyDiffReflFacGnd.dimension(TotSurfaces, 0.0);
        SurfWinA.dimension(CFSMAXNL + 1, TotSurfaces, 0.0);
        SurfWinADiffFront.dimension(CFSMAXNL + 1, TotSurfaces, 0.0);
        SurfWinADiffBack.dimension(CFSMAXNL + 1, TotSurfaces, 0.0);
        SurfWinACFOverlap.dimension(DataHeatBalance::MaxSolidWinLayers, TotSurfaces, 0.0);
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
        //  Detached Surfaces
        //  Base Surface for zone x
        //    Subsurfaces for base surface
        //  Base Surface for zone x
        //    etc
        //  Heat Transfer Surfaces and Shading surfaces are mixed in the list
        //  Pointers are set in the zones (First, Last)

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

        using namespace DataIPShortCuts;

        using namespace Vectors;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;
        using namespace DataErrorTracking;

        static std::string const RoutineName("GetSurfaceData: ");

        int ConstrNum;                // Construction number
        int Found;                    // For matching interzone surfaces
        int ConstrNumFound;           // Construction number of matching interzone surface
        static bool NonMatch(false);  // Error for non-matching interzone surfaces
        int MovedSurfs;               // Number of Moved Surfaces (when sorting into hierarchical structure)
        static bool SurfError(false); // General Surface Error, causes fatal error at end of routine
        int BaseSurfNum;
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
        int OpaqueHTSurfs;        // Number of floors, walls and roofs in a zone
        int OpaqueHTSurfsWithWin; // Number of floors, walls and roofs with windows in a zone
        int InternalMassSurfs;    // Number of internal mass surfaces in a zone
        static bool RelWarning(false);
        int ConstrNumSh;      // Shaded construction number for a window
        int LayNumOutside;    // Outside material numbers for a shaded construction
        int BlNum;            // Blind number
        int AddedSubSurfaces; // Subsurfaces (windows) added when windows reference Window5 Data File
        // entries with two glazing systems
        int NeedToAddSurfaces;    // Surfaces that will be added due to "unentered" other zone surface
        int NeedToAddSubSurfaces; // SubSurfaces that will be added due to "unentered" other zone surface
        int CurNewSurf;
        int FirstTotalSurfaces;
        int NVert;
        int Vert;
        int n;
        Real64 SurfWorldAz;
        Real64 SurfTilt;

        int MultFound;
        int MultSurfNum;
        std::string MultString;
        static bool WarningDisplayed(false);
        static int ErrCount2(0);
        static int ErrCount3(0);
        static int ErrCount4(0); // counts of interzone area mismatches.
        bool SubSurfaceSevereDisplayed;
        bool subSurfaceError(false);
        bool errFlag;

        int iTmp1;
        int iTmp2;
        // unused  INTEGER :: SchID
        int BlNumNew;
        int WinShadingControlPtr(0);
        int ShadingType;
        int ErrCount;
        Real64 diffp;
        bool izConstDiff;    // differences in construction for IZ surfaces
        bool izConstDiffMsg; // display message about hb diffs only once.

        // Get the total number of surfaces to allocate derived type and for surface loops

        if (state.dataSurfaceGeometry->GetSurfaceDataOneTimeFlag) {
            return;
        } else {
            state.dataSurfaceGeometry->GetSurfaceDataOneTimeFlag = true;
        }

        GetGeometryParameters(state, ErrorsFound);

        if (WorldCoordSystem) {
            if (BuildingAzimuth != 0.0) RelWarning = true;
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).RelNorth != 0.0) RelWarning = true;
            }
            if (RelWarning && !WarningDisplayed) {
                ShowWarningError(state, RoutineName +
                                 "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.");
                ShowContinueError(state, "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                WarningDisplayed = true;
            }
            RelWarning = false;
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !WarningDisplayed) {
                ShowWarningError(state, RoutineName +
                                 "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.");
                ShowContinueError(state, "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                WarningDisplayed = true;
            }
        }

        TotDetachedFixed = inputProcessor->getNumObjectsFound(state, "Shading:Site:Detailed");
        TotDetachedBldg = inputProcessor->getNumObjectsFound(state, "Shading:Building:Detailed");
        TotRectDetachedFixed = inputProcessor->getNumObjectsFound(state, "Shading:Site");
        TotRectDetachedBldg = inputProcessor->getNumObjectsFound(state, "Shading:Building");
        TotHTSurfs = inputProcessor->getNumObjectsFound(state, "BuildingSurface:Detailed");
        TotDetailedWalls = inputProcessor->getNumObjectsFound(state, "Wall:Detailed");
        TotDetailedRoofs = inputProcessor->getNumObjectsFound(state, "RoofCeiling:Detailed");
        TotDetailedFloors = inputProcessor->getNumObjectsFound(state, "Floor:Detailed");
        TotHTSubs = inputProcessor->getNumObjectsFound(state, "FenestrationSurface:Detailed");
        TotShdSubs = inputProcessor->getNumObjectsFound(state, "Shading:Zone:Detailed");
        TotOverhangs = inputProcessor->getNumObjectsFound(state, "Shading:Overhang");
        TotOverhangsProjection = inputProcessor->getNumObjectsFound(state, "Shading:Overhang:Projection");
        TotFins = inputProcessor->getNumObjectsFound(state, "Shading:Fin");
        TotFinsProjection = inputProcessor->getNumObjectsFound(state, "Shading:Fin:Projection");
        TotRectWindows = inputProcessor->getNumObjectsFound(state, "Window");
        TotRectDoors = inputProcessor->getNumObjectsFound(state, "Door");
        TotRectGlazedDoors = inputProcessor->getNumObjectsFound(state, "GlazedDoor");
        TotRectIZWindows = inputProcessor->getNumObjectsFound(state, "Window:Interzone");
        TotRectIZDoors = inputProcessor->getNumObjectsFound(state, "Door:Interzone");
        TotRectIZGlazedDoors = inputProcessor->getNumObjectsFound(state, "GlazedDoor:Interzone");
        TotRectExtWalls = inputProcessor->getNumObjectsFound(state, "Wall:Exterior");
        TotRectIntWalls = inputProcessor->getNumObjectsFound(state, "Wall:Adiabatic");
        TotRectIZWalls = inputProcessor->getNumObjectsFound(state, "Wall:Interzone");
        TotRectUGWalls = inputProcessor->getNumObjectsFound(state, "Wall:Underground");
        TotRectRoofs = inputProcessor->getNumObjectsFound(state, "Roof");
        TotRectCeilings = inputProcessor->getNumObjectsFound(state, "Ceiling:Adiabatic");
        TotRectIZCeilings = inputProcessor->getNumObjectsFound(state, "Ceiling:Interzone");
        TotRectGCFloors = inputProcessor->getNumObjectsFound(state, "Floor:GroundContact");
        TotRectIntFloors = inputProcessor->getNumObjectsFound(state, "Floor:Adiabatic");
        TotRectIZFloors = inputProcessor->getNumObjectsFound(state, "Floor:Interzone");

        TotOSC = 0;

        TotIntMassSurfaces = GetNumIntMassSurfaces(state);

        TotSurfaces = (TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg) * 2 + TotHTSurfs + TotHTSubs +
                      TotShdSubs * 2 + TotIntMassSurfaces + TotOverhangs * 2 + TotOverhangsProjection * 2 + TotFins * 4 + TotFinsProjection * 4 +
                      TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors + TotRectWindows + TotRectDoors + TotRectGlazedDoors +
                      TotRectIZWindows + TotRectIZDoors + TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls +
                      TotRectRoofs + TotRectCeilings + TotRectIZCeilings + TotRectGCFloors + TotRectIntFloors + TotRectIZFloors;

        state.dataSurfaceGeometry->SurfaceTmp.allocate(TotSurfaces); // Allocate the Surface derived type appropriately
        state.dataSurfaceGeometry->UniqueSurfaceNames.reserve(TotSurfaces);
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
                         state.dataSurfaceGeometry->BaseSurfCls,
                         state.dataSurfaceGeometry->BaseSurfIDs,
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
                        state.dataSurfaceGeometry->BaseSurfIDs,
                        NeedToAddSurfaces);

        GetHTSubSurfaceData(state, ErrorsFound, NumSurfs, TotHTSubs, state.dataSurfaceGeometry->SubSurfCls, state.dataSurfaceGeometry->SubSurfIDs, AddedSubSurfaces, NeedToAddSubSurfaces);

        GetRectSubSurfaces(state,
                           ErrorsFound,
                           NumSurfs,
                           TotRectWindows,
                           TotRectDoors,
                           TotRectGlazedDoors,
                           TotRectIZWindows,
                           TotRectIZDoors,
                           TotRectIZGlazedDoors,
                           state.dataSurfaceGeometry->SubSurfIDs,
                           AddedSubSurfaces,
                           NeedToAddSubSurfaces);

        GetAttShdSurfaceData(state, ErrorsFound, NumSurfs, TotShdSubs);

        GetSimpleShdSurfaceData(state, ErrorsFound, NumSurfs, TotOverhangs, TotOverhangsProjection, TotFins, TotFinsProjection);

        GetIntMassSurfaceData(state, ErrorsFound, NumSurfs);

        GetMovableInsulationData(state, ErrorsFound);

        if (CalcSolRefl) GetShadingSurfReflectanceData(state, ErrorsFound);

        TotSurfaces = NumSurfs + AddedSubSurfaces + NeedToAddSurfaces + NeedToAddSubSurfaces;

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors discovered, program terminates.");
        }

        // Have to make room for added surfaces, if needed
        FirstTotalSurfaces = NumSurfs + AddedSubSurfaces;
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) {
            state.dataSurfaceGeometry->SurfaceTmp.redimension(TotSurfaces);
        }

        SurfaceWindow.allocate(TotSurfaces);

        AllocateSurfaceWindows(TotSurfaces);

        // add the "need to add" surfaces
        // Debug    write(outputfiledebug,*) ' need to add ',NeedtoAddSurfaces+NeedToAddSubSurfaces
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) CurNewSurf = FirstTotalSurfaces;
        for (int SurfNum = 1; SurfNum <= FirstTotalSurfaces; ++SurfNum) {
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond != state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) continue;
            // Need to add surface
            ++CurNewSurf;
            // Debug    write(outputfiledebug,*) ' adding surface=',curnewsurf
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
            //  Basic parameters are the same for both surfaces.
            Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, Zone, state.dataGlobal->NumOfZones);
            if (Found == 0) continue;
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Zone = Found;
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ZoneName = Zone(Found).Name;
            // Reverse Construction
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Construction = AssignReverseConstructionNumber(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction, SurfError);
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Construction;
            // Reverse Vertices
            NVert = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
            for (Vert = 1; Vert <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++Vert) {
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Vertex(Vert) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NVert);
                --NVert;
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Sides > 2) {
                CreateNewellAreaVector(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Vertex, state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Sides, state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NewellAreaVector);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).GrossArea = VecLength(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NewellAreaVector);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Area = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).GrossArea;
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Area;
                CreateNewellSurfaceNormalVector(
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Vertex, state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Sides, state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector);
                DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Vertex,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Sides,
                                        SurfWorldAz,
                                        SurfTilt,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).lcsx,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).lcsy,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).lcsz,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).GrossArea,
                                        state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Azimuth = SurfWorldAz;
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Tilt = SurfTilt;

                // Sine and cosine of azimuth and tilt
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
                // Outward normal unit vector (pointing away from room)
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector;
                for (n = 1; n <= 3; ++n) {
                    if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n) - 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n) = +1.0;
                    if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n) + 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n) = -1.0;
                    if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n)) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OutNormVec(n) = 0.0;
                }

                // Can perform tests on this surface here
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ViewFactorSky = 0.5 * (1.0 + state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).CosTilt);
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ViewFactorGround = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).CosTilt);

                // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
                // surfaces
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ViewFactorSky;
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ViewFactorGroundIR = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).CosTilt);
            }

            // Change Name
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Name = "iz-" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
            // Debug   write(outputfiledebug,*) ' new surf name=',TRIM(SurfaceTmp(CurNewSurf)%Name)
            // Debug   write(outputfiledebug,*) ' new surf in zone=',TRIM(surfacetmp(curnewsurf)%zoneName)
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtBoundCond = state.dataSurfaceGeometry->UnreconciledZoneSurface;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnreconciledZoneSurface;
            state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Name;
            if (state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class == SurfaceClass::Roof || state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class == SurfaceClass::Wall ||
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class == SurfaceClass::Floor) {
                // base surface
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Roof) {
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class = SurfaceClass::Floor;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a floor'
                } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor) {
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class = SurfaceClass::Roof;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a roof'
                }
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).BaseSurf = CurNewSurf;
                state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Name;
                // Debug        write(outputfiledebug,*) ' basesurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
            } else {
                // subsurface
                Found = UtilityRoutines::FindItemInList("iz-" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, FirstTotalSurfaces + CurNewSurf - 1);
                if (Found > 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).BaseSurfName = "iz-" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).BaseSurf = Found;
                    state.dataSurfaceGeometry->SurfaceTmp(Found).Area -= state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Area;
                    if (state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Class == SurfaceClass::GlassDoor) {
                        state.dataSurfaceGeometry->SurfaceTmp(Found).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Area / state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Multiplier;
                    } else { // Door, TDD:Diffuser, TDD:DOME
                        state.dataSurfaceGeometry->SurfaceTmp(Found).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Area;
                    }
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName;
                    state.dataSurfaceGeometry->SurfaceTmp(CurNewSurf).OSCPtr = state.dataSurfaceGeometry->SurfaceTmp(Found).OSCPtr;
                    // Debug        write(outputfiledebug,*) ' subsurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
                    // Debug        write(outputfiledebug,*) ' subsurf, basesurf=',TRIM('iz-'//SurfaceTmp(SurfNum)%BaseSurfName)
                } else {
                    ShowSevereError(state, RoutineName + "Adding unentered subsurface, could not find base surface=" + "iz-" +
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName);
                    SurfError = true;
                }
            }
        }
        //**********************************************************************************
        // After all of the surfaces have been defined then the base surfaces for the
        // sub-surfaces can be defined.  Loop through surfaces and match with the sub-surface
        // names.
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf) continue;

            // why are we doing this again?  this should have already been done.
            if (UtilityRoutines::SameString(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name)) {
                Found = SurfNum;
            } else {
                Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            }
            if (Found > 0) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = Found;
                if (SurfNum != Found) { // for subsurfaces
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf) ++state.dataSurfaceGeometry->SurfaceTmp(Found).NumSubSurfaces;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class < SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class > SurfaceClass::TDD_Diffuser) {
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::None) {
                            ShowSevereError(state, RoutineName + "Invalid SubSurface detected, Surface=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                        } else {
                            ShowSevereError(state, RoutineName + "Invalid SubSurface detected, Surface=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                            ", class=" + state.dataSurfaceGeometry->BaseSurfCls(int(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class)) + " invalid class for subsurface");
                            SurfError = true;
                        }
                    }
                }
            }

        } // ...end of the Surface DO loop for finding BaseSurf
        //**********************************************************************************

        // The surfaces need to be hierarchical by zone.  Input is allowed to be in any order.  In
        // this section the surfaces are reordered into:
        //    All shadowing surfaces (if mirrored, Mir- surface follows immediately after original)
        //      Shading:Site
        //      Shading:Building
        //      Shading:Zone (and variants)
        //    For each zone:
        //      Walls
        //      Floors
        //      Roofs/Ceilings
        //      Internal Mass
        //      Non-Window subsurfaces (doors and TubularDaylightingDomes)
        //      Window subsurfaces (including TubularDaylightingDiffusers)
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
        Surface.allocate(TotSurfaces); // Allocate the Surface derived type appropriately
        Array1D<bool> SurfaceTmpClassMoved; // Tmp class is moved
        SurfaceTmpClassMoved.dimension(TotSurfaces, false);

        // Move all shading Surfaces to Front
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_B &&
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Shading)
                continue;

            //  A shading surface
            ++MovedSurfs;
            // Store list of moved surface numbers in reporting order
            Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
            SurfaceTmpClassMoved(SurfNum) = true; //'Moved'
            DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);
        }

        //  For each zone

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            // Group air boundary surfaces first within each zone
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (SurfaceTmpClassMoved(SurfNum)) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone != ZoneNum) continue;
                int constNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
                if (constNum == 0) continue;
                if (!state.dataConstruction->Construct(constNum).TypeIsAirBoundary) continue;

                //  An air boundary surface
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).IsAirBoundarySurf = true;
                ++MovedSurfs;
                Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                //  If base Surface Type (Wall, Floor, Roof/Ceiling)
                if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == state.dataSurfaceGeometry->BaseSurfIDs(1)) ||
                    (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == state.dataSurfaceGeometry->BaseSurfIDs(2)) ||
                    (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == state.dataSurfaceGeometry->BaseSurfIDs(3))) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = -1; // Default has base surface = base surface
                    Surface(MovedSurfs).BaseSurf = MovedSurfs;
                }
                SurfaceTmpClassMoved(SurfNum) = true; //'Moved'
                // Store list of moved surface numbers in reporting order
                DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);
            }

            //  For each Base Surface Type (Wall, Floor, Roof/Ceiling) - put these first

            for (int Loop = 1; Loop <= 3; ++Loop) {

                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                    if (SurfaceTmpClassMoved(SurfNum)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone == 0) continue;

                    if (!UtilityRoutines::SameString(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName, Zone(ZoneNum).Name)) continue;
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != state.dataSurfaceGeometry->BaseSurfIDs(Loop)) continue;

                    ++MovedSurfs;
                    Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                    SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = -1;              // Default has base surface = base surface
                    BaseSurfNum = MovedSurfs;
                    Surface(MovedSurfs).BaseSurf = BaseSurfNum;
                    // Store list of moved surface numbers in order reporting order (subsurfaces follow their base surface)
                    DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);

                    //  Find all subsurfaces to this surface - just to update the base surface number - don't move these yet
                    for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                        if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Zone == 0) continue;
                        if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).BaseSurf != SurfNum) continue;
                        // Set BaseSurf to negative of new BaseSurfNum (to avoid confusion with other base surfaces)
                        state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).BaseSurf = -BaseSurfNum;
                        // Add original sub-surface numbers as placeholders in surface list for reporting
                        DataSurfaces::AllSurfaceListReportOrder.push_back(-SubSurfNum);
                    }
                }
            }

            // Internal mass goes next
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                if (SurfaceTmpClassMoved(SurfNum)) continue;
                if (!UtilityRoutines::SameString(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName, Zone(ZoneNum).Name)) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::IntMass) continue;
                ++MovedSurfs;
                Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum);
                Surface(MovedSurfs).BaseSurf = MovedSurfs;
                SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                // Store list of moved surface numbers in reporting order
                DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);
            }

            // Non-window) subsurfaces are next (anything left in this zone that's not a window or a glass door)
            // includes SurfaceClass::TDD_Dome which transmits light but is not a window for heat balance purposes
            for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Zone != ZoneNum) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class == SurfaceClass::Window) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class == SurfaceClass::GlassDoor) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class == SurfaceClass::TDD_Diffuser) continue;


                ++MovedSurfs;
                Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                // Reset BaseSurf to it's positive value (set to negative earlier)
                Surface(MovedSurfs).BaseSurf = -Surface(MovedSurfs).BaseSurf;
                state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).BaseSurf = -1;
                // Find and replace negative SubSurfNum with new MovedSurfs num in surface list for reporting
                std::replace(DataSurfaces::AllSurfaceListReportOrder.begin(), DataSurfaces::AllSurfaceListReportOrder.end(), -SubSurfNum, MovedSurfs);
            }

            // Last but not least, the window subsurfaces (includes SurfaceClass::TDD_Diffuser)
            for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                if (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Zone != ZoneNum) continue;
                if ((state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::Window) && (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::GlassDoor) &&
                    (state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).Class != SurfaceClass::TDD_Diffuser))
                    continue;

                ++MovedSurfs;
                Surface(MovedSurfs) = state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum);
                SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                // Reset BaseSurf to it's positive value (set to negative earlier)
                Surface(MovedSurfs).BaseSurf = -Surface(MovedSurfs).BaseSurf;
                state.dataSurfaceGeometry->SurfaceTmp(SubSurfNum).BaseSurf = -1;
                // Find and replace negative SubSurfNum with new MovedSurfs num in surface list for reporting
                std::replace(DataSurfaces::AllSurfaceListReportOrder.begin(), DataSurfaces::AllSurfaceListReportOrder.end(), -SubSurfNum, MovedSurfs);
            }
        }

        if (MovedSurfs != TotSurfaces) {
            ShowSevereError(state, format("{}Reordered # of Surfaces ({}) not = Total # of Surfaces ({})", RoutineName, MovedSurfs, TotSurfaces));
            SurfError = true;
            for (int Loop = 1; Loop <= TotSurfaces; ++Loop) {
                if (!SurfaceTmpClassMoved(Loop) && state.dataSurfaceGeometry->SurfaceTmp(Loop).Class == SurfaceClass::INVALID) {
                    ShowSevereError(state, RoutineName + "Error in Surface= \"" + state.dataSurfaceGeometry->SurfaceTmp(Loop).Name +
                                    " indicated Zone=\"" + state.dataSurfaceGeometry->SurfaceTmp(Loop).ZoneName + "\"");
                }
            }
            ShowWarningError(state, RoutineName + "Remaining surface checks will use \"reordered number of surfaces\", not number of original surfaces");
        }

        state.dataSurfaceGeometry->SurfaceTmp.deallocate(); // DeAllocate the Temp Surface derived type

        //  For each Base Surface Type (Wall, Floor, Roof)

        for (int Loop = 1; Loop <= 3; ++Loop) {

            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                if (Surface(SurfNum).Zone == 0) continue;

                if (Surface(SurfNum).Class != state.dataSurfaceGeometry->BaseSurfIDs(Loop)) continue;

                //  Find all subsurfaces to this surface
                for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                    if (SurfNum == SubSurfNum) continue;
                    if (Surface(SubSurfNum).Zone == 0) continue;
                    if (Surface(SubSurfNum).BaseSurf != SurfNum) continue;

                    // Check facing angle of Sub compared to base
                    checkSubSurfAzTiltNorm(state, Surface(SurfNum), Surface(SubSurfNum), subSurfaceError);
                    if (subSurfaceError) SurfError = true;
                }
            }
        }

        //**********************************************************************************
        // Now, match up interzone surfaces
        NonMatch = false;
        izConstDiffMsg = false;
        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            //  Clean up Shading Surfaces, make sure they don't go through here.
            if (!Surface(SurfNum).HeatTransSurf) continue;
            //   If other surface, match it up
            //  Both interzone and "internal" surfaces have this pointer set
            //  Internal surfaces point to themselves, Interzone to another
            if (Surface(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                if (not_blank(Surface(SurfNum).ExtBoundCondName)) {
                    if (Surface(SurfNum).ExtBoundCondName == Surface(SurfNum).Name) {
                        Found = SurfNum;
                    } else {
                        Found = UtilityRoutines::FindItemInList(Surface(SurfNum).ExtBoundCondName, Surface, MovedSurfs);
                    }
                    if (Found != 0) {
                        Surface(SurfNum).ExtBoundCond = Found;
                        // Check that matching surface is also "OtherZoneSurface"
                        if (Surface(Found).ExtBoundCond <= 0 && Surface(Found).ExtBoundCond != state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                            ShowSevereError(state, RoutineName + "Potential \"OtherZoneSurface\" is not matched correctly:");

                            ShowContinueError(state, "Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                            ShowContinueError(state, "Nonmatched Other/InterZone Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                            SurfError = true;
                        }
                        // Check that matching interzone surface has construction with reversed layers
                        if (Found != SurfNum) { // Interzone surface
                            // Make sure different zones too (CR 4110)
                            if (Surface(SurfNum).Zone == Surface(Found).Zone) {
                                ++ErrCount2;
                                if (ErrCount2 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state, RoutineName + "CAUTION -- Interzone surfaces are occuring in the same zone(s).");
                                    ShowContinueError(state,
                                        "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual occurrences.");
                                }
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state, RoutineName + "CAUTION -- Interzone surfaces are usually in different zones");
                                    ShowContinueError(state, "Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                    ShowContinueError(state, "Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                                }
                            }
                            ConstrNum = Surface(SurfNum).Construction;
                            ConstrNumFound = Surface(Found).Construction;
                            if (ConstrNum <= 0 || ConstrNumFound <= 0) continue;
                            if (state.dataConstruction->Construct(ConstrNum).ReverseConstructionNumLayersWarning &&
                                state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionNumLayersWarning)
                                continue;
                            if (state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning &&
                                state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning)
                                continue;
                            TotLay = state.dataConstruction->Construct(ConstrNum).TotLayers;
                            TotLayFound = state.dataConstruction->Construct(ConstrNumFound).TotLayers;
                            if (TotLay != TotLayFound) { // Different number of layers
                                // match on like Uvalues (nominal)
                                if (std::abs(NominalU(ConstrNum) - NominalU(ConstrNumFound)) > 0.001) {
                                    ShowSevereError(state, RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                    Surface(SurfNum).Name + " does not have the same number of layers as the construction " +
                                                    state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionNumLayersWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionNumLayersWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        state.dataConstruction->Construct(ConstrNum).ReverseConstructionNumLayersWarning = true;
                                        state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionNumLayersWarning = true;
                                    }
                                    SurfError = true;
                                }
                            } else { // Same number of layers; check for reverse layers
                                // check layers as number of layers is the same
                                izConstDiff = false;
                                // ok if same nominal U
                                CheckForReversedLayers(state, izConstDiff, ConstrNum, ConstrNumFound, TotLay);
                                if (izConstDiff && std::abs(NominalU(ConstrNum) - NominalU(ConstrNumFound)) > 0.001) {
                                    ShowSevereError(state, RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                    Surface(SurfNum).Name +
                                                    " does not have the same materials in the reverse order as the construction " +
                                                    state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    ShowContinueError(state, "or the properties of the reversed layers are not correct due to differing layer front and back side values");
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning = true;
                                        state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning = true;
                                    }
                                    SurfError = true;
                                } else if (izConstDiff) {
                                    ShowWarningError(state, RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                     Surface(SurfNum).Name +
                                                     " does not have the same materials in the reverse order as the construction " +
                                                     state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    ShowContinueError(state, "or the properties of the reversed layers are not correct due to differing layer front and back side values");
                                    ShowContinueError(state,
                                                      format("...but Nominal U values are similar, diff=[{:.4R}] ... simulation proceeds.",
                                                             std::abs(NominalU(ConstrNum) - NominalU(ConstrNumFound))));
                                    if (!izConstDiffMsg) {
                                        ShowContinueError(state, "...if the two zones are expected to have significantly different temperatures, the proper "
                                                          "\"reverse\" construction should be created.");
                                        izConstDiffMsg = true;
                                    }
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError(state, "...this problem for this pair will not be reported again.");
                                        state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning = true;
                                        state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning = true;
                                    }
                                }
                            }

                            // If significantly different areas -- this would not be good
                            MultFound = Zone(Surface(Found).Zone).Multiplier * Zone(Surface(Found).Zone).ListMultiplier;
                            MultSurfNum = Zone(Surface(SurfNum).Zone).Multiplier * Zone(Surface(SurfNum).Zone).ListMultiplier;
                            if (Surface(Found).Area > 0.0) {
                                if (std::abs((Surface(Found).Area * MultFound - Surface(SurfNum).Area * MultSurfNum) / Surface(Found).Area *
                                             MultFound) > 0.02) { // 2% difference in areas
                                    ++ErrCount4;
                                    if (ErrCount4 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                        ShowWarningError(state,
                                            RoutineName +
                                            "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:");
                                        ShowContinueError(state,
                                            "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual mismatches.");
                                    }
                                    if (state.dataGlobal->DisplayExtraWarnings) {
                                        ShowWarningError(state,
                                            RoutineName +
                                            "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:");

                                        if (MultFound == 1 && MultSurfNum == 1) {
                                            ShowContinueError(state,
                                                              format("  Area={:.1T} in Surface={}, Zone={}",
                                                                     Surface(SurfNum).Area,
                                                                     Surface(SurfNum).Name,
                                                                     Surface(SurfNum).ZoneName));
                                            ShowContinueError(state,
                                                              format("  Area={:.1T} in Surface={}, Zone={}",
                                                                     Surface(Found).Area,
                                                                     Surface(Found).Name,
                                                                     Surface(Found).ZoneName));
                                        } else { // Show multiplier info
                                            ShowContinueError(state, format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
                                                                     Surface(SurfNum).Area,
                                                                     MultSurfNum,
                                                                    Surface(SurfNum).Area * MultSurfNum,
                                                                     Surface(SurfNum).Name,
                                                                     Surface(SurfNum).ZoneName));

                                            ShowContinueError(state, format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
                                                                     Surface(Found).Area,
                                                                     MultFound,
                                                                     Surface(Found).Area * MultFound,
                                                                     Surface(Found).Name,
                                                                     Surface(Found).ZoneName));
                                        }
                                    }
                                }
                            }
                            // Check opposites Azimuth and Tilt
                            // Tilt
                            if (std::abs(std::abs(Surface(Found).Tilt + Surface(SurfNum).Tilt) - 180.0) > 1.0) {
                                ShowWarningError(state, RoutineName + "InterZone Surface Tilts do not match as expected.");
                                ShowContinueError(state,
                                                  format("  Tilt={:.1T} in Surface={}, Zone={}",
                                                         Surface(SurfNum).Tilt,
                                                         Surface(SurfNum).Name,
                                                         Surface(SurfNum).ZoneName));
                                ShowContinueError(
                                    state,
                                    format(
                                        "  Tilt={:.1T} in Surface={}, Zone={}", Surface(Found).Tilt, Surface(Found).Name, Surface(Found).ZoneName));
                            }
                            // check surface class match.  interzone surface.

                            if ((Surface(SurfNum).Class == SurfaceClass::Wall && Surface(Found).Class != SurfaceClass::Wall) ||
                                (Surface(SurfNum).Class != SurfaceClass::Wall && Surface(Found).Class == SurfaceClass::Wall)) {
                                ShowWarningError(state, RoutineName + "InterZone Surface Classes do not match as expected.");
                                ShowContinueError(state, "Surface=\"" + Surface(SurfNum).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(SurfNum).Class));
                                ShowContinueError(state, "Adjacent Surface=\"" + Surface(Found).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(Found).Class));
                                ShowContinueError(state, "Other errors/warnings may follow about these surfaces.");
                            }
                            if ((Surface(SurfNum).Class == SurfaceClass::Roof && Surface(Found).Class != SurfaceClass::Floor) ||
                                (Surface(SurfNum).Class != SurfaceClass::Roof && Surface(Found).Class == SurfaceClass::Floor)) {
                                ShowWarningError(state, RoutineName + "InterZone Surface Classes do not match as expected.");
                                ShowContinueError(state, "Surface=\"" + Surface(SurfNum).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(SurfNum).Class));
                                ShowContinueError(state, "Adjacent Surface=\"" + Surface(Found).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(Found).Class));
                                ShowContinueError(state, "Other errors/warnings may follow about these surfaces.");
                            }
                            if (Surface(SurfNum).Class != SurfaceClass::Roof && Surface(SurfNum).Class != SurfaceClass::Floor) {
                                // Walls, Windows, Doors, Glass Doors
                                if (Surface(SurfNum).Class != SurfaceClass::Wall) {
                                    // Surface is a Door, Window or Glass Door
                                    if (Surface(SurfNum).BaseSurf == 0) continue; // error detected elsewhere
                                    if (Surface(Surface(SurfNum).BaseSurf).Class == SurfaceClass::Roof ||
                                        Surface(Surface(SurfNum).BaseSurf).Class == SurfaceClass::Floor)
                                        continue;
                                }
                                if (std::abs(std::abs(Surface(SurfNum).Azimuth - Surface(Found).Azimuth) - 180.0) > 1.0) {
                                    if (std::abs(Surface(SurfNum).SinTilt) > 0.5 || state.dataGlobal->DisplayExtraWarnings) {
                                        // if horizontal surfaces, then these are windows/doors/etc in those items.
                                        ShowWarningError(state, RoutineName + "InterZone Surface Azimuths do not match as expected.");
                                        ShowContinueError(state,
                                                          format("  Azimuth={:.1T}, Tilt={:.1T}, in Surface={}, Zone={}",
                                                                 Surface(SurfNum).Azimuth,
                                                                 Surface(SurfNum).Tilt,
                                                                 Surface(SurfNum).Name,
                                                                 Surface(SurfNum).ZoneName));
                                        ShowContinueError(state,
                                                          format("  Azimuth={:.1T}, Tilt={:.1T}, in Surface={}, Zone={}",
                                                                 Surface(Found).Azimuth,
                                                                 Surface(Found).Tilt,
                                                                 Surface(Found).Name,
                                                                 Surface(Found).ZoneName));
                                        ShowContinueError(state, "..surface class of first surface=" + cSurfaceClass(Surface(SurfNum).Class));
                                        ShowContinueError(state, "..surface class of second surface=" + cSurfaceClass(Surface(Found).Class));
                                    }
                                }
                            }

                            // Make sure exposures (Sun, Wind) are the same.....and are "not"
                            if (Surface(SurfNum).ExtSolar || Surface(Found).ExtSolar) {
                                ShowWarningError(state, RoutineName + "Interzone surfaces cannot be \"SunExposed\" -- removing SunExposed");
                                ShowContinueError(state, "  Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                ShowContinueError(state, "  Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                                Surface(SurfNum).ExtSolar = false;
                                Surface(Found).ExtSolar = false;
                            }
                            if (Surface(SurfNum).ExtWind || Surface(Found).ExtWind) {
                                ShowWarningError(state, RoutineName + "Interzone surfaces cannot be \"WindExposed\" -- removing WindExposed");
                                ShowContinueError(state, "  Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                ShowContinueError(state, "  Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                                Surface(SurfNum).ExtWind = false;
                                Surface(Found).ExtWind = false;
                            }
                        }
                        // Set opposing surface back to this one (regardless of error)
                        Surface(Found).ExtBoundCond = SurfNum;
                        // Check subsurfaces...  make sure base surface is also an interzone surface
                        if (Surface(SurfNum).BaseSurf != SurfNum) { // Subsurface
                            if ((Surface(SurfNum).ExtBoundCond != SurfNum) && not_blank(Surface(SurfNum).ExtBoundCondName)) {
                                // if not internal subsurface
                                if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond == Surface(SurfNum).BaseSurf) {
                                    // base surface is not interzone surface
                                    ShowSevereError(state, RoutineName + "SubSurface=\"" + Surface(SurfNum).Name + "\" is an interzone subsurface.");
                                    ShowContinueError(state, "..but the Base Surface is not an interzone surface, Surface=\"" +
                                                      Surface(Surface(SurfNum).BaseSurf).Name + "\".");
                                    SurfError = true;
                                }
                            }
                        }
                    } else {
                        //  Seems unlikely that an internal surface would be missing itself, so this message
                        //  only indicates for adjacent (interzone) surfaces.
                        ShowSevereError(state, RoutineName + "Adjacent Surface not found: " + Surface(SurfNum).ExtBoundCondName + " adjacent to surface " +
                                        Surface(SurfNum).Name);
                        NonMatch = true;
                        SurfError = true;
                    }
                } else if (Surface(SurfNum).BaseSurf != SurfNum) { // Subsurface
                    if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond > 0 &&
                        Surface(Surface(SurfNum).BaseSurf).ExtBoundCond !=
                            Surface(SurfNum).BaseSurf) { // If Interzone surface, subsurface must be also.
                        ShowSevereError(state, RoutineName + "SubSurface on Interzone Surface must be an Interzone SubSurface.");
                        ShowContinueError(state, "...OutsideFaceEnvironment is blank, in Surface=" + Surface(SurfNum).Name);
                        SurfError = true;
                    } else {
                        ++ErrCount3;
                        if (ErrCount3 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, RoutineName + "Blank name for Outside Boundary Condition Objects.");
                            ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface(SurfNum).Name);
                            ShowContinueError(state, "Resetting this surface to be an internal zone surface, zone=" + Surface(SurfNum).ZoneName);
                        }
                        Surface(SurfNum).ExtBoundCondName = Surface(SurfNum).Name;
                        Surface(SurfNum).ExtBoundCond = SurfNum;
                    }
                } else {
                    ++ErrCount3;
                    if (ErrCount3 == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                        ShowSevereError(state, RoutineName + "Blank name for Outside Boundary Condition Objects.");
                        ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state, RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface(SurfNum).Name);
                        ShowContinueError(state, "Resetting this surface to be an internal zone (adiabatic) surface, zone=" + Surface(SurfNum).ZoneName);
                    }
                    Surface(SurfNum).ExtBoundCondName = Surface(SurfNum).Name;
                    Surface(SurfNum).ExtBoundCond = SurfNum;
                    SurfError = true;
                }
            }

        } // ...end of the Surface DO loop for finding BaseSurf
        if (NonMatch) {
            ShowSevereError(state, RoutineName + "Non matching interzone surfaces found");
        }

        //**********************************************************************************
        // Warn about interzone surfaces that have adiabatic windows/vice versa
        SubSurfaceSevereDisplayed = false;
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue;
            if (Surface(SurfNum).BaseSurf == SurfNum) continue; // base surface
            // not base surface.  Check it.
            if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond <= 0) {                                 // exterior or other base surface
                if (Surface(SurfNum).ExtBoundCond != Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) { // should match base surface
                    if (Surface(SurfNum).ExtBoundCond == SurfNum) {
                        ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [adiabatic surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [" + cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    } else if (Surface(SurfNum).ExtBoundCond > 0) {
                        ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [interzone surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [" + cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    } else if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond == OtherSideCondModeledExt) {
                        ShowWarningError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                         cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                         Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [" +
                                         cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        ShowContinueError(state, "...SubSurface will not use the exterior condition model of the base surface.");
                    } else {
                        ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                        cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                        Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [" +
                                        cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    }
                    if (!SubSurfaceSevereDisplayed && SurfError) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                }
            } else if (Surface(Surface(SurfNum).BaseSurf).BaseSurf == Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) {
                // adiabatic surface. make sure subsurfaces match
                if (Surface(SurfNum).ExtBoundCond != SurfNum) { // not adiabatic surface
                    if (Surface(SurfNum).ExtBoundCond > 0) {
                        ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [interzone surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [adiabatic surface]");
                    } else {
                        ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                        cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                        Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [adiabatic surface]");
                    }
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    SurfError = true;
                }
            } else if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond > 0) { // interzone surface
                if (Surface(SurfNum).ExtBoundCond == SurfNum) {
                    ShowSevereError(state, RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                    "\" is an adiabatic surface in an Interzone base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name + "\"");
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError(state, "...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    //        SurfError=.TRUE.
                }
            }
        }

        //**********************************************************************************
        //   Set up Zone Surface Pointers
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
                if (Surface(SurfNum).Zone == ZoneNum) {
                    if (Zone(ZoneNum).AllSurfaceFirst == 0) {
                        Zone(ZoneNum).AllSurfaceFirst = SurfNum;
                    }
                    if (Surface(SurfNum).IsAirBoundarySurf) continue;
                    if (Zone(ZoneNum).SurfaceFirst == 0) {
                        Zone(ZoneNum).SurfaceFirst = SurfNum;
                        // Non window surfaces are grouped next within each zone
                        Zone(ZoneNum).NonWindowSurfaceFirst = SurfNum;
                    }
                    if ((Zone(ZoneNum).WindowSurfaceFirst == 0) && ((Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) ||
                                                                    (Surface(SurfNum).Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                                                                    (Surface(SurfNum).Class == DataSurfaces::SurfaceClass::TDD_Diffuser))) {
                        // Window surfaces are grouped last within each zone
                        Zone(ZoneNum).WindowSurfaceFirst = SurfNum;
                        Zone(ZoneNum).NonWindowSurfaceLast = SurfNum - 1;
                        break;
                    }
                }
            }
        }
        //  Surface First pointers are set, set last
        if (state.dataGlobal->NumOfZones > 0) {
            Zone(state.dataGlobal->NumOfZones).SurfaceLast = TotSurfaces;
            if ((Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::Window) ||
                (Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                (Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::TDD_Diffuser)) {
                Zone(state.dataGlobal->NumOfZones).WindowSurfaceLast = TotSurfaces;
            } else {
                // If there are no windows in the zone, then set this to -1 so any for loops on WindowSurfaceFirst to WindowSurfaceLast will not
                // execute
                Zone(state.dataGlobal->NumOfZones).WindowSurfaceLast = -1;
                Zone(state.dataGlobal->NumOfZones).NonWindowSurfaceLast = TotSurfaces;
            }
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones - 1; ++ZoneNum) {
            Zone(ZoneNum).SurfaceLast = Zone(ZoneNum + 1).AllSurfaceFirst - 1;
            if ((Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::Window) ||
                (Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                (Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::TDD_Diffuser)) {
                Zone(ZoneNum).WindowSurfaceLast = Zone(ZoneNum + 1).AllSurfaceFirst - 1;
            } else {
                // If there are no windows in the zone, then set this to -1 so any for loops on WindowSurfaceFirst to WindowSurfaceLast will not
                // execute
                Zone(ZoneNum).WindowSurfaceLast = -1;
                Zone(ZoneNum).NonWindowSurfaceLast = Zone(ZoneNum).SurfaceLast;
            }
        }

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (Zone(ZoneNum).SurfaceFirst == 0) {
                ShowSevereError(state, RoutineName + "Zone has no surfaces, Zone=" + Zone(ZoneNum).Name);
                SurfError = true;
            }
        }

        // Set up Floor Areas for Zones
        if (!SurfError) {
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                for (int SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                    if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                        Zone(ZoneNum).FloorArea += Surface(SurfNum).Area;
                        Zone(ZoneNum).HasFloor = true;
                    }
                    if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                        Zone(ZoneNum).CeilingArea += Surface(SurfNum).Area;
                        Zone(ZoneNum).HasRoof = true;
                    }
                }
            }
            ErrCount = 0;
            for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                Zone(ZoneNum).CalcFloorArea = Zone(ZoneNum).FloorArea;
                if (Zone(ZoneNum).UserEnteredFloorArea != DataGlobalConstants::AutoCalculate) {
                    // Check entered vs calculated
                    if (Zone(ZoneNum).UserEnteredFloorArea > 0.0) { // User entered zone floor area,
                        // produce message if not near calculated
                        if (Zone(ZoneNum).CalcFloorArea > 0.0) {
                            diffp = std::abs(Zone(ZoneNum).CalcFloorArea - Zone(ZoneNum).UserEnteredFloorArea) / Zone(ZoneNum).UserEnteredFloorArea;
                            if (diffp > 0.05) {
                                ++ErrCount;
                                if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                                    ShowWarningError(state, RoutineName + "Entered Zone Floor Areas differ from calculated Zone Floor Area(s).");
                                    ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                                }
                                if (state.dataGlobal->DisplayExtraWarnings) {
                                    // Warn user of using specified Zone Floor Area
                                    ShowWarningError(state, RoutineName + "Entered Floor Area entered for Zone=\"" + Zone(ZoneNum).Name +
                                                     "\" significantly different from calculated Floor Area");
                                    ShowContinueError(state,
                                                      format("Entered Zone Floor Area value={:.2R}, Calculated Zone Floor Area value={:.2R}, entered "
                                                             "Floor Area will be used in calculations.",
                                                             Zone(ZoneNum).UserEnteredFloorArea,
                                                             Zone(ZoneNum).CalcFloorArea));
                                }
                            }
                        }
                        Zone(ZoneNum).FloorArea = Zone(ZoneNum).UserEnteredFloorArea;
                        Zone(ZoneNum).HasFloor = true;
                    }
                } else {
                    Zone(ZoneNum).FloorArea = Zone(ZoneNum).CalcFloorArea; // redundant, already done.
                }
            }
        }

        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            if (Surface(SurfNum).Area < 1.e-06) {
                ShowSevereError(
                    state, format("{}Zero or negative surface area[{:.5R}], Surface={}", RoutineName, Surface(SurfNum).Area, Surface(SurfNum).Name));
                SurfError = true;
            }
            if (Surface(SurfNum).Area >= 1.e-06 && Surface(SurfNum).Area < 0.001) {
                ShowWarningError(state,
                                 format("{}Very small surface area[{:.5R}], Surface={}", RoutineName, Surface(SurfNum).Area, Surface(SurfNum).Name));
            }
        }

        for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
            // GLASSDOORs and TDD:DIFFUSERs will be treated as windows in the subsequent heat transfer and daylighting
            // calculations. Reset class to 'Window' after saving the original designation in SurfaceWindow.

            SurfWinOriginalClass(SurfNum) = Surface(SurfNum).Class;

            if (Surface(SurfNum).Class == SurfaceClass::GlassDoor || Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)
                Surface(SurfNum).Class = SurfaceClass::Window;

            if (Surface(SurfNum).Class == SurfaceClass::TDD_Dome) {
                // Reset the TDD:DOME subsurface to act as a base surface that can shade and be shaded
                // NOTE: This must be set early so that subsequent shading calculations are done correctly
                Surface(SurfNum).BaseSurf = SurfNum;
            }
        }

        errFlag = false;
        if (!SurfError) {
            for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
                if (Surface(SurfNum).HasShadeControl) {
                    WinShadingControlPtr = Surface(SurfNum).activeWindowShadingControl; // use first item since others should be identical
                    if (WindowShadingControl(WinShadingControlPtr).SlatAngleControlForBlinds != WSC_SAC_FixedSlatAngle)
                        SurfWinMovableSlats(SurfNum) = true;
                }

                ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                if (ConstrNumSh <= 0) continue;

                ShadingType = WindowShadingControl(WinShadingControlPtr).ShadingType;

                // only for blinds
                if (ShadingType == WSC_ST_ExteriorBlind || ShadingType == WSC_ST_InteriorBlind || ShadingType == WSC_ST_BetweenGlassBlind) {

                    // TH 1/7/2010. CR 7930
                    // The old code did not consider between-glass blind. Also there should not be two blinds - both interior and exterior
                    // Use the new generic code (assuming only one blind) as follows
                    for (iTmp1 = 1; iTmp1 <= state.dataConstruction->Construct(ConstrNumSh).TotLayers; ++iTmp1) {
                        iTmp2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(iTmp1);
                        if (state.dataMaterial->Material(iTmp2).Group == WindowBlind) {
                            BlNum = state.dataMaterial->Material(iTmp2).BlindDataPtr;
                            SurfWinBlindNumber(SurfNum) = BlNum;
                            // TH 2/18/2010. CR 8010
                            // if it is a blind with movable slats, create one new blind and set it to VariableSlat if not done so yet.
                            //  the new blind is created only once, it can be shared by multiple windows though.
                            if (SurfWinMovableSlats(SurfNum) && Blind(BlNum).SlatAngleType != VariableSlats) {
                                errFlag = false;
                                AddVariableSlatBlind(state, BlNum, BlNumNew, errFlag);
                                // point to the new blind
                                state.dataMaterial->Material(iTmp2).BlindDataPtr = BlNumNew;
                                // window surface points to new blind
                                SurfWinBlindNumber(SurfNum) = BlNumNew;
                            }
                            break;
                        }
                    }

                    if (errFlag) {
                        ErrorsFound = true;
                        ShowContinueError(state, "WindowShadingControl " + WindowShadingControl(WinShadingControlPtr).Name +
                                          " has errors, program will terminate.");
                    }
                }
            } // End of surface loop

            // final associate fenestration surfaces referenced in WindowShadingControl
            FinalAssociateWindowShadingControlFenestration(state, ErrorsFound);
            CheckWindowShadingControlSimilarForWindow(state, ErrorsFound);
        }

        // Check for zones with not enough surfaces
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            OpaqueHTSurfs = 0;
            OpaqueHTSurfsWithWin = 0;
            InternalMassSurfs = 0;
            if (Zone(ZoneNum).SurfaceFirst == 0) continue; // Zone with no surfaces
            for (int SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                if (Surface(SurfNum).Class == SurfaceClass::Floor || Surface(SurfNum).Class == SurfaceClass::Wall ||
                    Surface(SurfNum).Class == SurfaceClass::Roof)
                    ++OpaqueHTSurfs;
                if (Surface(SurfNum).Class == SurfaceClass::IntMass) ++InternalMassSurfs;
                if (Surface(SurfNum).Class == SurfaceClass::Window) {
                    // Count base surface only once for multiple windows on a wall
                    if (SurfNum > 1 && Surface(SurfNum - 1).Class != SurfaceClass::Window) ++OpaqueHTSurfsWithWin;
                }
            }
            if (OpaqueHTSurfsWithWin == 1 && OpaqueHTSurfs == 1 && InternalMassSurfs == 0) {
                SurfError = true;
                ShowSevereError(state, RoutineName + "Zone " + Zone(ZoneNum).Name + " has only one floor, wall or roof, and this surface has a window.");
                ShowContinueError(state, "Add more floors, walls or roofs, or an internal mass surface.");
            }
        }

        // set up vertex of centroid for each surface.
        CalcSurfaceCentroid(state);

        SetupShadeSurfacesForSolarCalcs(state); // if shading surfaces are solar collectors or PV, then we need full solar calc.

        LayNumOutside = 0;
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            // Check for EcoRoof and only 1 allowed to be used.
            if (!Surface(SurfNum).ExtEcoRoof) continue;
            if (LayNumOutside == 0) {
                LayNumOutside = state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1);
                continue;
            }
            if (LayNumOutside != state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1)) {
                ShowSevereError(state, RoutineName + "Only one EcoRoof Material is currently allowed for all constructions.");
                ShowContinueError(state, "... first material=" + state.dataMaterial->Material(LayNumOutside).Name);
                ShowContinueError(state, "... conflicting Construction=" + state.dataConstruction->Construct(Surface(SurfNum).Construction).Name +
                                  " uses material=" + state.dataMaterial->Material(state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1)).Name);
                ErrorsFound = true;
            }
        }

        // Set flag that determines whether a surface can be an exterior obstruction
        // Also set associated surfaces for Kiva foundations and build heat transfer surface lists
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            Surface(SurfNum).ShadowSurfPossibleObstruction = false;
            if (Surface(SurfNum).HeatTransSurf) {
                DataSurfaces::AllHTSurfaceList.push_back(SurfNum);
                int const zoneNum(Surface(SurfNum).Zone);
                auto &surfZone(Zone(zoneNum));
                surfZone.ZoneHTSurfaceList.push_back(SurfNum);
                // Sort window vs non-window surfaces
                if (Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) {
                    DataSurfaces::AllHTWindowSurfaceList.push_back(SurfNum);
                    surfZone.ZoneHTWindowSurfaceList.push_back(SurfNum);
                } else {
                    DataSurfaces::AllHTNonWindowSurfaceList.push_back(SurfNum);
                    surfZone.ZoneHTNonWindowSurfaceList.push_back(SurfNum);
                }
                if (Surface(SurfNum).ExtSolar) {
                    surfZone.ZoneExtSolarSurfaceList.push_back(SurfNum);
                }
                int const surfExtBoundCond(Surface(SurfNum).ExtBoundCond);
                // Build zone and interzone surface lists
                if ((surfExtBoundCond > 0) && (surfExtBoundCond != SurfNum)) {
                    DataSurfaces::AllIZSurfaceList.push_back(SurfNum);
                    surfZone.ZoneIZSurfaceList.push_back(SurfNum);
                    auto &adjZone(Zone(Surface(surfExtBoundCond).Zone));
                    adjZone.ZoneHTSurfaceList.push_back(SurfNum);
                    adjZone.ZoneIZSurfaceList.push_back(SurfNum);
                    // Sort window vs non-window surfaces
                    if (Surface(SurfNum).Class == DataSurfaces::SurfaceClass::Window) {
                        adjZone.ZoneHTWindowSurfaceList.push_back(SurfNum);
                    } else {
                        adjZone.ZoneHTNonWindowSurfaceList.push_back(SurfNum);
                    }
                }
            }
            // Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond > 0) continue;
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == Ground) continue;
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == KivaFoundation) {
                if (!ErrorsFound) state.dataSurfaceGeometry->kivaManager.foundationInputs[Surface(SurfNum).OSCPtr].surfaces.push_back(SurfNum);
                continue;
            }
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt) continue;
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) continue;
            // Exclude windows and doors, i.e., consider only their base surfaces as possible obstructions
            if (Surface(SurfNum).Class == SurfaceClass::Window || Surface(SurfNum).Class == SurfaceClass::Door) continue;
            // Exclude duplicate shading surfaces
            if (Surface(SurfNum).MirroredSurf) continue;
            // Exclude air boundary surfaces
            if (Surface(SurfNum).IsAirBoundarySurf) continue;

            Surface(SurfNum).ShadowSurfPossibleObstruction = true;
        }

        // Check for IRT surfaces in invalid places.
        iTmp1 = 0;
        if (std::any_of(state.dataConstruction->Construct.begin(), state.dataConstruction->Construct.end(), [](Construction::ConstructionProps const &e) { return e.TypeIsIRT; })) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;                                               // ignore shading surfaces
                if (Surface(SurfNum).ExtBoundCond > 0 && Surface(SurfNum).ExtBoundCond != SurfNum) continue; // interzone, not adiabatic surface
                if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsIRT) {
                    continue;
                }
                if (!state.dataGlobal->DisplayExtraWarnings) {
                    ++iTmp1;
                } else {
                    ShowWarningError(state, RoutineName + "Surface=\"" + Surface(SurfNum).Name +
                                     "\" uses InfraredTransparent construction in a non-interzone surface. (illegal use)");
                }
            }
            if (iTmp1 > 0) {
                ShowWarningError(
                    state,
                    format("{}Surfaces use InfraredTransparent constructions {} in non-interzone surfaces. (illegal use)", RoutineName, iTmp1));
                ShowContinueError(state, "For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
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
        SetupEnclosuresAndAirBoundaries(state, DataViewFactorInformation::ZoneRadiantInfo, SurfaceGeometry::enclosureType::RadiantEnclosures, ErrorsFound);

        GetSurfaceSrdSurfsData(state, ErrorsFound);

        GetSurfaceLocalEnvData(state, ErrorsFound);

        if (SurfError || ErrorsFound) {
            ErrorsFound = true;
            ShowFatalError(state, RoutineName + "Errors discovered, program terminates.");
        }

        int TotShadSurf = TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg + TotShdSubs + TotOverhangs +
                          TotOverhangsProjection + TotFins + TotFinsProjection;
        int NumDElightCmplxFen = inputProcessor->getNumObjectsFound(state, "Daylighting:DElight:ComplexFenestration");
        if (TotShadSurf > 0 && (NumDElightCmplxFen > 0 || DaylightingManager::doesDayLightingUseDElight(state))) {
            ShowWarningError(state, RoutineName + "When using DElight daylighting the presence of exterior shading surfaces is ignored.");
        }
    }

    void checkSubSurfAzTiltNorm(EnergyPlusData &state, SurfaceData &baseSurface, // Base surface data (in)
                                SurfaceData &subSurface,  // Subsurface data (in)
                                bool &surfaceError        // True if surface azimuths or tilts differ by more than error tolerance
    )
    {
        bool sameSurfNormal(false); // True if surface has the same surface normal within tolerance
        bool baseSurfHoriz(false);  // True if base surface is near horizontal
        Real64 const warningTolerance(30.0);
        Real64 const errorTolerance(90.0);

        surfaceError = false;

        // Check if base surface and subsurface have the same normal
        Vectors::CompareTwoVectors(baseSurface.NewellSurfaceNormalVector, subSurface.NewellSurfaceNormalVector, sameSurfNormal, 0.001);
        if (sameSurfNormal) { // copy lcs vectors
            // Prior logic tested for azimuth difference < 30 and then skipped this - this caused large diffs in CmplxGlz_MeasuredDeflectionAndShading
            // Restoring that check here but will require further investigation (MJW Dec 2015)
            if (std::abs(baseSurface.Azimuth - subSurface.Azimuth) > warningTolerance) {
                subSurface.lcsx = baseSurface.lcsx;
                subSurface.lcsy = baseSurface.lcsy;
                subSurface.lcsy = baseSurface.lcsy;
            }
        } else {
            // Not sure what this does, but keeping for now (MJW Dec 2015)
            if (std::abs(subSurface.Azimuth - 360.0) < 0.01) {
                subSurface.Azimuth = 360.0 - subSurface.Azimuth;
            }
            if (std::abs(baseSurface.Azimuth - 360.0) < 0.01) {
                baseSurface.Azimuth = 360.0 - baseSurface.Azimuth;
            }

            // Is base surface horizontal? If so, ignore azimuth differences
            if (std::abs(baseSurface.Tilt) <= 1.0e-5 || std::abs(baseSurface.Tilt - 180.0) <= 1.0e-5) baseSurfHoriz = true;

            if (((std::abs(baseSurface.Azimuth - subSurface.Azimuth) > errorTolerance) && !baseSurfHoriz) ||
                (std::abs(baseSurface.Tilt - subSurface.Tilt) > errorTolerance)) {
                surfaceError = true;
                ShowSevereError(
                    state,
                    format("checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than {:.1R} degrees from base surface.",
                           errorTolerance));
                ShowContinueError(state,
                                  format("Subsurface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", subSurface.Name, subSurface.Tilt, subSurface.Azimuth));
                ShowContinueError(
                    state, format("Base surface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", baseSurface.Name, baseSurface.Tilt, baseSurface.Azimuth));
            } else if (((std::abs(baseSurface.Azimuth - subSurface.Azimuth) > warningTolerance) && !baseSurfHoriz) ||
                       (std::abs(baseSurface.Tilt - subSurface.Tilt) > warningTolerance)) {
                ++state.dataSurfaceGeometry->checkSubSurfAzTiltNormErrCount;
                if (state.dataSurfaceGeometry->checkSubSurfAzTiltNormErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(
                        state,
                        format("checkSubSurfAzTiltNorm: Some Outward Facing angles of subsurfaces differ more than {:.1R} degrees from base surface.",
                               warningTolerance));
                    ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                }
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(
                        state,
                        format("checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than {:.1R} degrees from base surface.",
                               warningTolerance));
                    ShowContinueError(
                        state, format("Subsurface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", subSurface.Name, subSurface.Tilt, subSurface.Azimuth));
                    ShowContinueError(
                        state,
                        format("Base surface=\"{}\" Tilt = {:.1R}  Azimuth = {:.1R}", baseSurface.Name, baseSurface.Tilt, baseSurface.Azimuth));
                }
            }
        }
    }

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
        using namespace DataIPShortCuts;

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
        int ZoneNum; //For loop counter
        static bool RelWarning(false);

        cCurrentModuleObject = "GlobalGeometryRules";
        NumStmt = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        OutMsg = " Surface Geometry,";

        {
            auto const SELECT_CASE_var(NumStmt);

            if (SELECT_CASE_var == 1) {
                // This is the valid case
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              1,
                                              GAlphas,
                                              NAlphas,
                                              GNum,
                                              NNum,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                // Even though these will be validated, set defaults in case error here -- wont
                // cause aborts in later surface gets (hopefully)
                Corner = UpperLeftCorner;
                WorldCoordSystem = true;
                CCW = true;

                OK = false;
                Found = UtilityRoutines::FindItem(GAlphas(1), FlCorners, 4);
                if (Found == 0) {
                    ShowSevereError(state, cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(1) + '=' + GAlphas(1));
                    ErrorsFound = true;
                } else {
                    Corner = Found;
                    OK = true;
                    OutMsg += FlCorners(Corner) + ',';
                }

                OK = false;
                if (UtilityRoutines::SameString(GAlphas(2), "CCW") || UtilityRoutines::SameString(GAlphas(2), "Counterclockwise")) {
                    CCW = true;
                    OutMsg += "Counterclockwise,";
                    OK = true;
                }
                if (UtilityRoutines::SameString(GAlphas(2), "CW") || UtilityRoutines::SameString(GAlphas(2), "Clockwise")) {
                    CCW = false;
                    OutMsg += "Clockwise,";
                    OK = true;
                }
                if (!OK) {
                    ShowSevereError(state, cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + '=' + GAlphas(2));
                    ErrorsFound = true;
                }

                OK = false;
                if (UtilityRoutines::SameString(GAlphas(3), "World") || UtilityRoutines::SameString(GAlphas(3), "Absolute")) {
                    WorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                    OK = true;
                }
                if (UtilityRoutines::SameString(GAlphas(3), "Relative")) {
                    WorldCoordSystem = false;
                    OutMsg += "RelativeCoordinateSystem,";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(3) + '=' + GAlphas(3));
                    ShowContinueError(state, cAlphaFieldNames(3) + " defaults to \"WorldCoordinateSystem\"");
                    WorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                }

                OK = false;
                if (UtilityRoutines::SameString(GAlphas(4), "World") || UtilityRoutines::SameString(GAlphas(4), "Absolute")) {
                    DaylRefWorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem,";
                    OK = true;
                }
                if (UtilityRoutines::SameString(GAlphas(4), "Relative") || GAlphas(4).empty()) {
                    DaylRefWorldCoordSystem = false;
                    OutMsg += "RelativeCoordinateSystem,";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(4) + '=' + GAlphas(4));
                    ShowContinueError(state, cAlphaFieldNames(4) + " defaults to \"RelativeToZoneOrigin\"");
                    DaylRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin,";
                }

                OK = false;
                if (UtilityRoutines::SameString(GAlphas(5), "World") || UtilityRoutines::SameString(GAlphas(5), "Absolute")) {
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem";
                    OK = true;
                }
                if (UtilityRoutines::SameString(GAlphas(5), "Relative") || GAlphas(5).empty()) {
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(state, cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(5) + '=' + GAlphas(5));
                    ShowContinueError(state, cAlphaFieldNames(5) + " defaults to \"RelativeToZoneOrigin\"");
                    state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                }

            } else if (SELECT_CASE_var == 0) {

                ShowSevereError(state, cCurrentModuleObject + ": Required object not found.");
                OutMsg += "None found in input";
                ErrorsFound = true;

            } else {

                ShowSevereError(state, cCurrentModuleObject + ": Too many objects entered.  Only one allowed.");
                ErrorsFound = true;
            }
        }

        if (!WorldCoordSystem) {
            if (DaylRefWorldCoordSystem) {
                ShowWarningError(state, cCurrentModuleObject + ": Potential mismatch of coordinate specifications.");
                ShowContinueError(state, cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                ShowContinueError(state, cAlphaFieldNames(4) + "=\"" + GAlphas(4) + "\".");
            }
            if (state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem) {
                ShowWarningError(state, cCurrentModuleObject + ": Potential mismatch of coordinate specifications.");
                ShowContinueError(state, cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                ShowContinueError(state, cAlphaFieldNames(5) + "=\"" + GAlphas(5) + "\".");
            }
        } else {
            RelWarning = false;
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem) {
                ShowWarningError(state, cCurrentModuleObject + ": Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on the default SurfaceGeometry for 'Relative to zone' coordinate.");
                ShowContinueError(state, cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                if (GAlphas(5) == "RELATIVE") {
                    ShowContinueError(state, cAlphaFieldNames(5) + "=\"" + GAlphas(5) + "\".");
                } else if (GAlphas(5) != "ABSOLUTE") {
                    ShowContinueError(state, cAlphaFieldNames(5) + "=\"defaults to RELATIVE\".");
                }
            }
        }


        print(state.files.eio, "! <Surface Geometry>,Starting Corner,Vertex Input Direction,Coordinate System,Daylight Reference "
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the Detached Shading Surface Data,
        // checks it for errors, etc.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // Detached Shading Surface Definition(s)
        // Surface:Shading:Detached:Fixed,
        //       \memo used for shading elements such as trees
        //       \memo these items are fixed in space and would not move with relative geometry
        //  A1 , \field User Supplied Surface Name
        //       \required-field
        //       \type alpha
        //  A2,  \field TransSchedShadowSurf
        //       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
        //       \type object-list
        //       \object-list ScheduleNames
        //  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
        //       \required-field
        //       \note shown with 12 vertex coordinates -- extensible object
        //       \autocalculatable
        //       \default autocalculate
        //       \minimum 3
        //       \note Rules for vertices are given in SurfaceGeometry coordinates --
        //       \note For this object all surface coordinates are relative to the building origin (0,0,0)
        //       \note and will rotate with the BUILDING north axis.
        //  N2,  \field Vertex 1 X-coordinate
        //       \units m
        //       \type real
        //  N3-37; as indicated by the N1 value
        // Surface:Shading:Detached:Building,
        //       \memo used for shading elements such as trees, other buildings, parts of this building not being modeled
        //       \memo these items are relative to the current building and would move with relative geometry
        //  A1 , \field User Supplied Surface Name
        //       \required-field
        //       \type alpha
        //  A2,  \field TransSchedShadowSurf
        //       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
        //       \type object-list
        //       \object-list ScheduleNames
        //  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
        //       \required-field
        //       \note shown with 12 vertex coordinates -- extensible object
        //       \autocalculatable
        //       \default autocalculate
        //       \minimum 3
        //       \note Rules for vertices are given in SurfaceGeometry coordinates --
        //       \note For this object all surface coordinates are relative to the building origin (0,0,0)
        //       \note and will rotate with the BUILDING north axis.
        //  N2,  \field Vertex 1 X-coordinate
        //       \units m
        //       \type real
        //  N3-37; as indicated by the N1 value

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace DataReportingFlags;

        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;

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
        Real64 SchedMinValue;
        Real64 SchedMaxValue;

        if ((TotDetachedFixed + TotDetachedBldg) > 0 && SolarDistribution == MinimalShadowing) {
            ShowWarningError(state, "Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
        }

        if ((TotDetachedFixed + TotDetachedBldg) == 0) return;

        for (Item = 1; Item <= 2; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotDetachedFixed;
                ClassItem = SurfaceClass::Detached_F;
            } else { // IF (Item == 2) THEN
                ItemsToGet = TotDetachedBldg;
                ClassItem = SurfaceClass::Detached_B;
            }

            inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 2) {
                ShowSevereError(
                    state, format("{}: Object Definition indicates not = 2 Alpha Objects, Number Indicated={}", cCurrentModuleObject, NumAlphas));
                ErrorsFound = true;
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state, cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = ClassItem;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = false;
                // Base transmittance of a shadowing (sub)surface
                if (!lAlphaFieldBlanks(2)) {
                    // Schedule for a shadowing (sub)surface
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) +
                                        " not found=" + cAlphaArgs(2));
                        ErrorsFound = true;
                    }
                } else {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex != 0) {
                    if (!CheckScheduleValueMinMax(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0)) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", values not in range [0,1].");
                        ErrorsFound = true;
                    }
                    SchedMinValue = GetScheduleMinValue(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMinValue = SchedMinValue;
                    SchedMaxValue = GetScheduleMaxValue(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                    if (SchedMinValue == 1.0) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                         cAlphaArgs(2) + "\", is always transparent.");
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).IsTransparent = true;
                    }
                    if (SchedMinValue < 0.0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", has schedule values < 0.");
                        ShowContinueError(state, "...Schedule values < 0 have no meaning for shading elements.");
                    }
                    if (SchedMaxValue > 1.0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", has schedule values > 1.");
                        ShowContinueError(state, "...Schedule values > 1 have no meaning for shading elements.");
                    }
                    if (std::abs(SchedMinValue - SchedMaxValue) > 1.0e-6) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfSchedVaries = true;
                        ShadingTransmittanceVaries = true;
                    }
                }
                if (lNumericFieldBlanks(1) || rNumericArgs(1) == DataGlobalConstants::AutoCalculate) {
                    numSides = (NumNumbers - 1) / 3;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = numSides;
                    if (mod(NumNumbers - 1, 3) != 0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " +
                                             format("{} not even multiple of 3. Will read in {}",
                                                    cNumericFieldNames(1),
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                               cCurrentModuleObject,
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                               cNumericFieldNames(1),
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (NumNumbers - 1) / 3;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
                    if (numSides > state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", field " +
                                             cNumericFieldNames(1) + '=' + fmt::to_string(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                        ShowContinueError(state,
                                          format("...but {} were entered. Only the indicated {} will be used.", numSides, cNumericFieldNames(1)));
                    }
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                GetVertices(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, rNumericArgs({2, _}));
                CheckConvexity(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                if (MakeMirroredDetachedShading) {
                    MakeMirrorSurface(state, SurfNum);
                }
            }

        } // Item Loop
    }

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
        // Gets the simple, rectantular detached surfaces.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace DataReportingFlags;

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

        if ((TotRectDetachedFixed + TotRectDetachedBldg) > 0 && SolarDistribution == MinimalShadowing) {
            ShowWarningError(state, "Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
        }

        if (TotRectDetachedFixed + TotRectDetachedBldg == 0) return;

        for (Item = 1; Item <= 2; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotRectDetachedFixed;
                ClassItem = SurfaceClass::Detached_F;
            } else { // IF (Item == 2) THEN
                ItemsToGet = TotRectDetachedBldg;
                ClassItem = SurfaceClass::Detached_B;
            }

            inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 1) {
                ShowSevereError(
                    state, format("{}: Object Definition indicates not = 1 Alpha Objects, Number Indicated={}", cCurrentModuleObject, NumAlphas));
                ErrorsFound = true;
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = ClassItem;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = false;

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = rNumericArgs(1);
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B && !WorldCoordSystem) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth += BuildingAzimuth;
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth += BuildingRotationAppendixG;
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = rNumericArgs(2);

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

                MakeRectangularVertices(
                    state, SurfNum, rNumericArgs(3), rNumericArgs(4), rNumericArgs(5), rNumericArgs(6), rNumericArgs(7), state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                    ErrorsFound = true;
                }

                if (MakeMirroredDetachedShading) {
                    MakeMirrorSurface(state, SurfNum);
                }
            }

        } // Item Loop
    }

    void GetHTSurfaceData(EnergyPlusData &state,
                          bool &ErrorsFound,                // Error flag indicator (true if errors found)
                          int &SurfNum,                     // Count of Current SurfaceNumber
                          int const TotHTSurfs,             // Number of Heat Transfer Base Surfaces to obtain
                          int const TotDetailedWalls,       // Number of Wall:Detailed items to obtain
                          int const TotDetailedRoofs,       // Number of RoofCeiling:Detailed items to obtain
                          int const TotDetailedFloors,      // Number of Floor:Detailed items to obtain
                          const Array1D_string &BaseSurfCls, // Valid Classes for Base Surfaces
                          const Array1D<SurfaceClass> &BaseSurfIDs,
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
        using namespace DataIPShortCuts;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D_string const cModuleObjects(4, {"BuildingSurface:Detailed", "Wall:Detailed", "Floor:Detailed", "RoofCeiling:Detailed"});

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

        GetOSCData(state, ErrorsFound);
        GetOSCMData(state, ErrorsFound);
        GetFoundationData(state, ErrorsFound);

        NeedToAddSurfaces = 0;

        for (Item = 1; Item <= 4; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
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

            inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);
            if (Item == 1) {
                if (SurfaceNumAlpha != 8) {
                    ShowSevereError(
                        state,
                        format("{}: Object Definition indicates not = 8 Alpha Objects, Number Indicated={}", cCurrentModuleObject, SurfaceNumAlpha));
                    ErrorsFound = true;
                }
            } else {
                if (SurfaceNumAlpha != 7) {
                    ShowSevereError(
                        state,
                        format("{}: Object Definition indicates not = 7 Alpha Objects, Number Indicated={}", cCurrentModuleObject, SurfaceNumAlpha));
                    ErrorsFound = true;
                }
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              SurfaceNumAlpha,
                                              rNumericArgs,
                                              SurfaceNumProp,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                ArgPointer = 2;
                if (Item == 1) {
                    if (cAlphaArgs(2) == "CEILING") cAlphaArgs(2) = "ROOF";
                    ClassItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), BaseSurfCls, 3);
                    if (ClassItem == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2));
                        ErrorsFound = true;
                    } else {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem);
                    }
                    ++ArgPointer;
                } else {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem);
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(ArgPointer), state.dataConstruction->Construct, TotConstructs);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                } else if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\" - has Window materials.");
                    if (Item == 1) {
                        ShowContinueError(state, "...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                    } else {
                        ShowContinueError(state, "...because Surface Type=" + BaseSurfCls(ClassItem));
                    }
                } else {
                    state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;

                ++ArgPointer;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = cAlphaArgs(ArgPointer);
                ZoneNum = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName, Zone, state.dataGlobal->NumOfZones);

                if (ZoneNum != 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = ZoneNum;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::INVALID;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }
                // Get the ExteriorBoundaryCondition flag from input There are 4 conditions that
                // can take place. The conditions are set with a 0, -1, or -2, or all of the
                // zone names have to be looked at and generate the interzone array number
                ++ArgPointer;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(ArgPointer + 1);

                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Outdoors")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Adiabatic")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Ground")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = Ground;

                    if (state.dataSurfaceGeometry->NoGroundTempObjWarning) {
                        if (!state.dataEnvrn->GroundTempObjInput) {
                            ShowWarningError(state, "GetHTSurfaceData: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError(state, "Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp));
                        }
                        state.dataSurfaceGeometry->NoGroundTempObjWarning = false;
                    }

                    // Added for FCfactor method
                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundFCfactorMethod")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    if (state.dataSurfaceGeometry->NoFCGroundTempObjWarning) {
                        if (!state.dataEnvrn->FCGroundTemps) {
                            ShowSevereError(state, "GetHTSurfaceData: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError(state, "Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(state,
                                "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures.");
                            ErrorsFound = true;
                            state.dataSurfaceGeometry->NoFCGroundTempObjWarning = false;
                        }
                    }
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction > 0) {
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && !state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer));
                            ShowContinueError(state, "Construction=\"" + state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Name +
                                              "\" is not type Construction:CfactorUndergroundWall.");
                            ErrorsFound = true;
                        }
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && !state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer));
                            ShowContinueError(state, "Construction=\"" + state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Name +
                                              "\" is not type Construction:FfactorGroundFloor.");
                            ErrorsFound = true;
                        }
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "OtherSideCoefficients")) {
                    Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, OSC, TotOSC);
                    if (Found == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                        ShowContinueError(state, " no OtherSideCoefficients of that name.");
                        ErrorsFound = true;
                    } else {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = Found;
                        if (OSC(Found).SurfFilmCoef > 0.0) {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Surface")) {
                    // it has to be another surface which needs to be found
                    // this will be found on the second pass through the surface input
                    // for flagging, set the value to UnreconciledZoneSurface
                    // name (ExtBoundCondName) will be validated later.
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                    if (lAlphaFieldBlanks(ArgPointer + 1)) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=<blank>.");
                        ShowContinueError(state, ".." + cAlphaFieldNames(ArgPointer) + "=\"Surface\" must be non-blank.");
                        ShowContinueError(state, "..This surface will become an adiabatic surface - no doors/windows allowed.");
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Zone")) {
                    // This is the code for an unmatched "other surface"
                    // will be set up later.
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface;
                    // check OutsideFaceEnvironment for legal zone
                    Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, Zone, state.dataGlobal->NumOfZones);
                    ++NeedToAddSurfaces;

                    if (Found == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) +
                                        "=\"" + cAlphaArgs(ArgPointer) + "\".");
                        ShowContinueError(state, "..Referenced as Zone for this surface.");
                        ErrorsFound = true;
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Foundation")) {

                    if (!state.dataWeatherManager->WeatherFileExists) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", using \"Foundation\" type Outside Boundary Condition requires specification of a weather file");
                        ShowContinueError(state, "Either place in.epw in the working directory or specify a weather file on the command line using -w "
                                          "/path/to/weather.epw");
                        ErrorsFound = true;
                    }

                    // Find foundation object, if blank use default
                    if (lAlphaFieldBlanks(ArgPointer + 1)) {

                        if (!state.dataSurfaceGeometry->kivaManager.defaultSet) {
                            // Apply default foundation if no other foundation object specified
                            if (state.dataSurfaceGeometry->kivaManager.foundationInputs.size() == 0) {
                                state.dataSurfaceGeometry->kivaManager.defineDefaultFoundation(state);
                            }
                            state.dataSurfaceGeometry->kivaManager.addDefaultFoundation();
                        }
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = state.dataSurfaceGeometry->kivaManager.defaultIndex; // Reuse OSC pointer...shouldn't be used for non OSC surfaces anyway.
                    } else {
                        Found = state.dataSurfaceGeometry->kivaManager.findFoundation(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName);
                        if (Found != (int)state.dataSurfaceGeometry->kivaManager.foundationInputs.size()) {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = Found;
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " +
                                            cAlphaFieldNames(ArgPointer + 1) + "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                            ErrorsFound = true;
                        }
                    }

                    if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", construction may not have an internal source/sink");
                        ErrorsFound = true;
                    }
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = KivaFoundation;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "OtherSideConditionsModel")) {
                    Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, OSCM, TotOSCM);
                    if (Found == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                        ErrorsFound = true;
                    }
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCMPtr = Found;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCondModeledExt;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorAverage") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorCore") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorPerimeter") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageFloor") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageWall") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorUpperWall") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorLowerWall")) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ShowContinueError(state, "The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder.");
                    ErrorsFound = true;

                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ShowContinueError(state, "Should be one of \"Outdoors\", \"Adiabatic\", Ground\", \"Surface\", \"OtherSideCoefficients\", "
                                      "\"OtherSideConditionsModel\" or \"Zone\"");
                    ErrorsFound = true;
                } // ... End of the ExtBoundCond logical IF Block

                ArgPointer += 2;
                // Set the logical flag for the exterior solar
                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "SunExposed")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = true;

                    if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment) && (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond != OtherSideCondModeledExt)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                         cAlphaArgs(ArgPointer) + "\".");
                        ShowContinueError(state, "..This surface is not exposed to External Environment.  Sun exposure has no effect.");
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "NoSun")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = false;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ErrorsFound = true;
                }

                ++ArgPointer;
                // Set the logical flag for the exterior wind
                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "WindExposed")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = true;
                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "NoWind")) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = false;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ErrorsFound = true;
                }

                // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction > 0) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtEcoRoof = state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsEcoRoof;

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = rNumericArgs(1);
                if (lNumericFieldBlanks(1)) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate;
                if (lNumericFieldBlanks(2) || rNumericArgs(2) == DataGlobalConstants::AutoCalculate) {
                    numSides = (SurfaceNumProp - 2) / 3;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = numSides;
                    if (mod(SurfaceNumProp - 2, 3) != 0) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " +
                                             format("{} not even multiple of 3. Will read in {}",
                                                    cNumericFieldNames(2),
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                               cCurrentModuleObject,
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                               cNumericFieldNames(2),
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (SurfaceNumProp - 2) / 3;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(2);
                    if (numSides > state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) {
                        ShowWarningError(state,
                                         cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", field " +
                                             cNumericFieldNames(2) + '=' + fmt::to_string(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                        ShowContinueError(state,
                                          format("...but {} were entered. Only the indicated {} will be used.", numSides, cNumericFieldNames(2)));
                    }
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewVertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                GetVertices(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, rNumericArgs({3, _}));
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                    ErrorsFound = true;
                }

                CheckConvexity(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                if (UtilityRoutines::SameString(cAlphaArgs(5), "Surface")) {
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides != static_cast<int>(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.size())) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", After CheckConvexity, mismatch between Sides ({}) and size of Vertex ({}).",
                                               cCurrentModuleObject,
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                               state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.size()));
                        ShowContinueError(state, "CheckConvexity is used to verify the convexity of a surface and detect collinear points.");
                        ErrorsFound = true;
                    }
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction > 0) {
                    // Check wall height for the CFactor walls

                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                        if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height - state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Height) > 0.05) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Wall Height = {:.2T}",
                                                    cCurrentModuleObject,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height));
                            ShowContinueError(state, "..which does not match its construction height.");
                        }
                    }

                    // Check area and perimeter for the FFactor floors
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                        if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area - state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Area) > 0.1) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Floor Area = {:.2T}",
                                                    cCurrentModuleObject,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                            ShowContinueError(state, "..which does not match its construction area.");
                        }
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter < state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).PerimeterExposed - 0.1) {
                            ShowWarningError(state,
                                             format("{}=\"{}\", underground Floor Perimeter = {:.2T}",
                                                    cCurrentModuleObject,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter));
                            ShowContinueError(state, "..which is less than its construction exposed perimeter.");
                        }
                    }
                }
            }
        } // Item Looop
        // Check number of Vertex between base surface and Outside Boundary surface
        int ExtSurfNum;
        for (int i = 1; i <= SurfNum; i++) {
            if (state.dataSurfaceGeometry->SurfaceTmp(i).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface && state.dataSurfaceGeometry->SurfaceTmp(i).ExtBoundCondName != "") {
                ExtSurfNum = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(i).ExtBoundCondName, state.dataSurfaceGeometry->SurfaceTmp);
                // If we cannot find the referenced surface
                if (ExtSurfNum == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(i).Name +
                                    "\" references an outside boundary surface that cannot be found:" + state.dataSurfaceGeometry->SurfaceTmp(i).ExtBoundCondName);
                    ErrorsFound = true;
                    // If vertex size mistmatch
                } else if (state.dataSurfaceGeometry->SurfaceTmp(i).Vertex.size() !=
                           state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Vertex.size()) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(i).Name +
                                        "\", Vertex size mismatch between base surface :" + state.dataSurfaceGeometry->SurfaceTmp(i).Name +
                                        " and outside boundary surface: " + state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Name);
                    ShowContinueError(state,
                                      format("The vertex sizes are {} for base surface and {} for outside boundary surface. Please check inputs.",
                                             state.dataSurfaceGeometry->SurfaceTmp(i).Vertex.size(),
                                             state.dataSurfaceGeometry->SurfaceTmp(ExtSurfNum).Vertex.size()));
                    ErrorsFound = true;
                }
            }
        }
    }

    void GetRectSurfaces(EnergyPlusData &state,
                         bool &ErrorsFound,             // Error flag indicator (true if errors found)
                         int &SurfNum,                  // Count of Current SurfaceNumber
                         int const TotRectExtWalls,     // Number of Exterior Walls to obtain
                         int const TotRectIntWalls,     // Number of Adiabatic Walls to obtain
                         int const TotRectIZWalls,      // Number of Interzone Walls to obtain
                         int const TotRectUGWalls,      // Number of Underground to obtain
                         int const TotRectRoofs,        // Number of Roofs to obtain
                         int const TotRectCeilings,     // Number of Adiabatic Ceilings to obtain
                         int const TotRectIZCeilings,   // Number of Interzone Ceilings to obtain
                         int const TotRectGCFloors,     // Number of Floors with Ground Contact to obtain
                         int const TotRectIntFloors,    // Number of Adiabatic Walls to obtain
                         int const TotRectIZFloors,     // Number of Interzone Floors to obtain
                         const Array1D<SurfaceClass> &BaseSurfIDs, // ID Assignments for valid surface classes
                         int &NeedToAddSurfaces         // Number of surfaces to add, based on unentered IZ surfaces
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
        using namespace DataIPShortCuts;

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

        for (Item = 1; Item <= 10; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
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
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                ClassItem = 1;
            } else if (Item == 3) {
                ItemsToGet = TotRectIZWalls;
                GettingIZSurfaces = true;
                OtherSurfaceField = 4;
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
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
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                ClassItem = 3;
            } else if (Item == 7) {
                ItemsToGet = TotRectIZCeilings;
                GettingIZSurfaces = false;
                OtherSurfaceField = 4;
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
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
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                ClassItem = 2;
            } else { // IF (Item == 10) THEN
                ItemsToGet = TotRectIZFloors;
                GettingIZSurfaces = true;
                OtherSurfaceField = 4;
                ExtBoundCondition = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                ClassItem = 2;
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                if (NumNumbers < 7) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Too few number of numeric args=[{}].", cCurrentModuleObject, cAlphaArgs(1), NumNumbers));
                    ErrorsFound = true;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1);           // Set the Surface Name in the Derived Type
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem); // Set class number

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                } else if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(2) + "\" - has Window materials.");
                    ShowContinueError(state, "...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                } else {
                    state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = cAlphaArgs(3);
                ZoneNum = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName, Zone, state.dataGlobal->NumOfZones);

                if (ZoneNum != 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = ZoneNum;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\".");
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::INVALID;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExtBoundCondition;
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction > 0) {
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall &&
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", Construction type is \"Construction:CfactorUndergroundWall\" but invalid for this object.");
                    }
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor &&
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", Construction type is \"Construction:FfactorGroundFloor\" but invalid for this object.");
                    }
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = false;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = false;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate;

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = true;

                    // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction > 0)
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtEcoRoof = state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsEcoRoof;

                } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                    if (GettingIZSurfaces) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(OtherSurfaceField);
                        Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, Zone, state.dataGlobal->NumOfZones);
                        // see if match to zone, then it's an unentered other surface, else reconciled later
                        if (Found > 0) {
                            ++NeedToAddSurfaces;
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface;
                        }
                    } else {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                    }

                } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == Ground) {

                    if (state.dataSurfaceGeometry->NoGroundTempObjWarning) {
                        if (!state.dataEnvrn->GroundTempObjInput) {
                            ShowWarningError(state, "GetRectSurfaces: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError(state, "Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(state, format("Defaults, constant throughout the year of ({:.1R}) will be used.", state.dataEnvrn->GroundTemp));
                        }
                        state.dataSurfaceGeometry->NoGroundTempObjWarning = false;
                    }

                } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (state.dataSurfaceGeometry->NoFCGroundTempObjWarning) {
                        if (!state.dataEnvrn->FCGroundTemps) {
                            ShowSevereError(state, "GetRectSurfaces: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError(state, "Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(state,
                                "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures.");
                            ErrorsFound = true;
                            state.dataSurfaceGeometry->NoFCGroundTempObjWarning = false;
                        }
                    }

                } // ... End of the ExtBoundCond logical IF Block

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = rNumericArgs(1);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = rNumericArgs(2);
                if (!WorldCoordSystem) {
                    if (ZoneNum != 0) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth += BuildingAzimuth + Zone(ZoneNum).RelNorth;
                    }
                }
                if (ZoneNum != 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth += BuildingRotationAppendixG;
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

                MakeRectangularVertices(
                    state, SurfNum, rNumericArgs(3), rNumericArgs(4), rNumericArgs(5), rNumericArgs(6), rNumericArgs(7), state.dataSurfaceGeometry->RectSurfRefWorldCoordSystem);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                    ErrorsFound = true;
                }

                // Check wall height for the CFactor walls
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Wall &&
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height - state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Height) > 0.05) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Wall Height = {:.2T}",
                                                cCurrentModuleObject,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height));
                        ShowContinueError(state, "..which deos not match its construction height.");
                    }
                }

                // Check area and perimeter for the FFactor floors
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor &&
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area - state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Area) > 0.1) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Floor Area = {:.2T}",
                                                cCurrentModuleObject,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                        ShowContinueError(state, "..which does not match its construction area.");
                    }
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter < state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).PerimeterExposed - 0.1) {
                        ShowWarningError(state,
                                         format("{}=\"{}\", underground Floor Perimeter = {:.2T}",
                                                cCurrentModuleObject,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter));
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
        Real64 SurfAzimuth; // Surface Azimuth/Facing (same as Base Surface)
        Real64 SurfTilt;    // Tilt (same as Base Surface)
        Real64 XLLC;
        Real64 YLLC;
        Real64 ZLLC;
        Real64 CosSurfAzimuth;
        Real64 SinSurfAzimuth;
        Real64 CosSurfTilt;
        Real64 SinSurfTilt;
        Array1D<Real64> XX(4);
        Array1D<Real64> YY(4);
        Real64 Xb;
        Real64 Yb;
        Real64 Perimeter;
        int n;
        int Vrt;


        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone == 0 &&
            (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_B))
            return;

        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height = Height;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width = Length;

        SurfAzimuth = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth;
        SurfTilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt;
        CosSurfAzimuth = std::cos(SurfAzimuth * DataGlobalConstants::DegToRadians);
        SinSurfAzimuth = std::sin(SurfAzimuth * DataGlobalConstants::DegToRadians);
        CosSurfTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
        SinSurfTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
        if (!SurfWorldCoordSystem) {
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone > 0) {
                Xb = XCoord * state.dataSurfaceGeometry->CosZoneRelNorth(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone) - YCoord * state.dataSurfaceGeometry->SinZoneRelNorth(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone) +
                     Zone(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone).OriginX;
                Yb = XCoord * state.dataSurfaceGeometry->SinZoneRelNorth(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone) + YCoord * state.dataSurfaceGeometry->CosZoneRelNorth(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone) +
                     Zone(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone).OriginY;
                XLLC = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                YLLC = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                ZLLC = ZCoord + Zone(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone).OriginZ;
            } else {
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                    Xb = XCoord;
                    Yb = YCoord;
                    XLLC = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                    YLLC = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                    ZLLC = ZCoord;
                } else {
                    XLLC = XCoord;
                    YLLC = YCoord;
                    ZLLC = ZCoord;
                }
            }
        } else {
            // for world coordinates, only rotate for appendix G
            Xb = XCoord;
            Yb = YCoord;
            ZLLC = ZCoord;
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F) {
                XLLC = Xb * state.dataSurfaceGeometry->CosBldgRotAppGonly - Yb * state.dataSurfaceGeometry->SinBldgRotAppGonly;
                YLLC = Xb * state.dataSurfaceGeometry->SinBldgRotAppGonly + Yb * state.dataSurfaceGeometry->CosBldgRotAppGonly;
            } else {
                XLLC = Xb;
                YLLC = Yb;
            }
        }

        XX(1) = 0.0;
        XX(2) = 0.0;
        XX(3) = Length;
        XX(4) = Length;
        YY(1) = Height;
        YY(4) = Height;
        YY(3) = 0.0;
        YY(2) = 0.0;

        for (n = 1; n <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++n) {
            Vrt = n;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).x = XLLC - XX(n) * CosSurfAzimuth - YY(n) * CosSurfTilt * SinSurfAzimuth;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).y = YLLC + XX(n) * SinSurfAzimuth - YY(n) * CosSurfTilt * CosSurfAzimuth;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).z = ZLLC + YY(n) * SinSurfTilt;
        }

        CreateNewellAreaVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = VecLength(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
        CreateNewellSurfaceNormalVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = SurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = SurfTilt;
        // Sine and cosine of azimuth and tilt
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = SinSurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = CosSurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = SinSurfTilt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = CosSurfTilt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
        // Outward normal unit vector (pointing away from room)
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
        for (n = 1; n <= 3; ++n) {
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
        }

        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Roof .and. SurfTilt > 80.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError(state, 'Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Floor .and. SurfTilt < 170.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError(state, 'Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF

        // Can perform tests on this surface here
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);

        Perimeter = distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1));
        for (Vrt = 2; Vrt <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++Vrt) {
            Perimeter += distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1));
        }
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter = Perimeter;

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
    }

    void GetHTSubSurfaceData(EnergyPlusData &state,
                             bool &ErrorsFound,               // Error flag indicator (true if errors found)
                             int &SurfNum,                    // Count of Current SurfaceNumber
                             int const TotHTSubs,             // Number of Heat Transfer SubSurfaces to obtain
                             const Array1D_string &SubSurfCls, // Valid Classes for Sub Surfaces
                             const Array1D<SurfaceClass> &SubSurfIDs,    // ID Assignments for valid sub surface classes
                             int &AddedSubSurfaces,           // Subsurfaces added when windows reference Window5
                             int &NeedToAddSurfaces           // Number of surfaces to add, based on unentered IZ surfaces
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
        using namespace DataIPShortCuts;

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

        GetWindowShadingControlData(state, ErrorsFound);

        cCurrentModuleObject = "FenestrationSurface:Detailed";
        inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);

        if (SurfaceNumAlpha != 6) {
            ShowSevereError(
                state, format("{}: Object Definition indicates not = 6 Alpha Objects, Number Indicated={}", cCurrentModuleObject, SurfaceNumAlpha));
            ErrorsFound = true;
        }

        if (SurfaceNumProp != 15) {
            ShowSevereError(
                state, format("{}: Object Definition indicates > 15 Numeric Objects, Number Indicated={}", cCurrentModuleObject, SurfaceNumAlpha));
            ErrorsFound = true;
        }
        NeedToAddSurfaces = 0;

        for (Loop = 1; Loop <= TotHTSubs; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          SurfaceNumAlpha,
                                          rNumericArgs,
                                          SurfaceNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (GlobalNames::VerifyUniqueInterObjectName(state, state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            if (SurfaceNumProp < 12) {
                ShowSevereError(state,
                                format("{}=\"{}\", Too few number of numeric args=[{}].",
                                       cCurrentModuleObject,
                                       state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                       SurfaceNumProp));
                ErrorsFound = true;
            }

            ++SurfNum;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
            ValidChk = UtilityRoutines::FindItemInList(cAlphaArgs(2), SubSurfCls, 6);
            if (ValidChk == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ErrorsFound = true;
            } else {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SubSurfIDs(ValidChk); // Set class number
            }

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct, TotConstructs);

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction == 0) {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
            } else {
                state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).IsUsed = true;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome) {

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {
                    if (!state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\" has an opaque surface construction; it should have a window construction.");
                    }
                    if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\": Windows are not allowed to have embedded sources/sinks");
                    }
                }

            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {
                if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" - has Window materials.");
                    ShowContinueError(state, "...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                }
            }

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(4);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            if (Found > 0) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = Found;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCondName;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = state.dataSurfaceGeometry->SurfaceTmp(Found).OSCPtr;
                if (state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface &&
                    state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCondName == state.dataSurfaceGeometry->SurfaceTmp(Found).Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(4) + "=\"" +
                                    cAlphaArgs(4) + "\".");
                    ShowContinueError(state, "... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError(state, "... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(4) + "=\"" +
                                cAlphaArgs(4));
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                ErrorsFound = true;
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                if (!lAlphaFieldBlanks(5)) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid field " + cAlphaFieldNames(5));
                    ShowContinueError(state, "...when Base surface uses \"Outdoors\" as " + cAlphaFieldNames(5) +
                                      ", subsurfaces need to be blank to inherit the outdoor characteristics.");
                    ShowContinueError(state, "...Surface external characteristics changed to reflect base surface.");
                }
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) { // "Surface" Base Surface
                if (!lAlphaFieldBlanks(5)) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5);
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid blank " + cAlphaFieldNames(5));
                    ShowContinueError(state, "...when Base surface uses \"Surface\" as " + cAlphaFieldNames(5) +
                                      ", subsurfaces must also specify specific surfaces in the adjacent zone.");
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5); // putting it as blank will not confuse things later.
                    ErrorsFound = true;
                }
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) { // "Zone" - unmatched interior surface
                ++NeedToAddSurfaces;
                // ignoring window5datafiles for now -- will need to add.
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                if (!lAlphaFieldBlanks(5)) { // Otherside Coef special Name
                    Found = UtilityRoutines::FindItemInList(cAlphaArgs(5), OSC, TotOSC);
                    if (Found == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(5) + "=\"" +
                                        cAlphaArgs(5) + "\".");
                        ShowContinueError(state, "...base surface requires that this subsurface have OtherSideCoefficients -- not found.");
                        ErrorsFound = true;
                    } else { // found
                        // The following allows for a subsurface that has different characteristics than
                        // the base surface with OtherSide Coeff -- do we want that or is it an error?
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = Found;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5);
                        if (OSC(Found).SurfFilmCoef > 0.0) {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }
                }
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName == BlankString) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
            }
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = rNumericArgs(1);
            if (lNumericFieldBlanks(1)) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate;

            if (lNumericFieldBlanks(3) || rNumericArgs(3) == DataGlobalConstants::AutoCalculate) {
                rNumericArgs(3) = (SurfaceNumProp - 3) / 3;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(3);
                if (mod(SurfaceNumProp - 3, 3) != 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " +
                                         format("{} not even multiple of 3. Will read in {}",
                                                cNumericFieldNames(3),
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                }
                if (rNumericArgs(3) < 3) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           cNumericFieldNames(3),
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    ErrorsFound = true;
                    continue;
                }
            } else {
                numSides = (SurfaceNumProp - 2) / 3;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(3);
                if (numSides > state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", field " +
                                         cNumericFieldNames(3) + '=' + fmt::to_string(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    ShowContinueError(state, format("...but {} were entered. Only the indicated {} will be used.", numSides, cNumericFieldNames(3)));
                }
            }
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier = int(rNumericArgs(2));
            // Only windows, glass doors and doors can have Multiplier > 1:
            if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Window && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
                 state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Door) &&
                rNumericArgs(2) > 1.0) {
                ShowWarningError(state,
                                 format("{}=\"{}\", invalid {}=[{:.1T}].",
                                        cCurrentModuleObject,
                                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                        cNumericFieldNames(2),
                                        rNumericArgs(2)));
                ShowContinueError(state, "...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2) + " multiplier will be set to 1.0.");
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier = 1.0;
            }

            GetVertices(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, rNumericArgs({4, _}));

            CheckConvexity(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList.clear();
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl = false;

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList.clear();
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = 0;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedStormWinConstructionList.clear();
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeStormWinShadedConstruction= 0;


            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome) {

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                    "\", Other side coefficients are not allowed with windows.");
                    ErrorsFound = true;
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                    "\", Exterior boundary condition = Ground is not allowed with windows.");
                    ErrorsFound = true;
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == KivaFoundation) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                    "\", Exterior boundary condition = Foundation is not allowed with windows.");
                    ErrorsFound = true;
                }

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                CheckWindowShadingControlFrameDivider(state, "GetHTSubSurfaceData", ErrorsFound, SurfNum, 6);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides == 3) { // Triangular window
                    if (!cAlphaArgs(6).empty()) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(6) + "=\"" +
                                         cAlphaArgs(6) + "\".");
                        ShowContinueError(state, ".. because it is a triangular window and cannot have a frame or divider or reveal reflection.");
                        ShowContinueError(state, "Frame, divider and reveal reflection will be ignored for this window.");
                    }
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider = 0;
                } // End of check if window is triangular or rectangular

            } // check on non-opaquedoor subsurfaces

            CheckSubSurfaceMiscellaneous(state, "GetHTSubSurfaceData", ErrorsFound, SurfNum, cAlphaArgs(1), cAlphaArgs(3), AddedSubSurfaces);

        } // End of main loop over subsurfaces
    }

    void GetRectSubSurfaces(EnergyPlusData &state,
                            bool &ErrorsFound,            // Error flag indicator (true if errors found)
                            int &SurfNum,                 // Count of Current SurfaceNumber
                            int const TotWindows,         // Number of Window SubSurfaces to obtain
                            int const TotDoors,           // Number of Door SubSurfaces to obtain
                            int const TotGlazedDoors,     // Number of Glass Door SubSurfaces to obtain
                            int const TotIZWindows,       // Number of Interzone Window SubSurfaces to obtain
                            int const TotIZDoors,         // Number of Interzone Door SubSurfaces to obtain
                            int const TotIZGlazedDoors,   // Number of Interzone Glass Door SubSurfaces to obtain
                            const Array1D<SurfaceClass> &SubSurfIDs, // ID Assignments for valid sub surface classes
                            int &AddedSubSurfaces,        // Subsurfaces added when windows reference Window5
                            int &NeedToAddSubSurfaces     // Number of surfaces to add, based on unentered IZ surfaces
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
        using namespace DataIPShortCuts;

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

        for (Item = 1; Item <= 6; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
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

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                if (NumNumbers < 5) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Too few number of numeric args=[{}].", cCurrentModuleObject, cAlphaArgs(1), NumNumbers));
                    ErrorsFound = true;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1);          // Set the Surface Name in the Derived Type
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SubSurfIDs(ClassItem); // Set class number

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                } else {
                    state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) {

                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {
                        if (!state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                            "\" has an opaque surface construction; it should have a window construction.");
                        }
                        if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                            "\": Windows are not allowed to have embedded sources/sinks");
                        }
                    }

                } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {
                    if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\" - has Window materials.");
                    }
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(3);
                //  The subsurface inherits properties from the base surface
                //  Exterior conditions, Zone, etc.
                //  We can figure out the base surface though, because they've all been entered
                Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
                if (Found > 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = Found;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCondName;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = state.dataSurfaceGeometry->SurfaceTmp(Found).Tilt;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(Found).Azimuth;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr = state.dataSurfaceGeometry->SurfaceTmp(Found).OSCPtr;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = state.dataSurfaceGeometry->SurfaceTmp(Found).ViewFactorGround;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = state.dataSurfaceGeometry->SurfaceTmp(Found).ViewFactorSky;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3));
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                    continue;
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface &&
                    state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCondName == state.dataSurfaceGeometry->SurfaceTmp(Found).Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\".");
                    ShowContinueError(state, "... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError(state, "... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (!GettingIZSurfaces) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid use of object");
                        ShowContinueError(state, "...when Base surface uses \"Surface\" as " + cAlphaFieldNames(5) +
                                          ", subsurfaces must also specify specific surfaces in the adjacent zone.");
                        ShowContinueError(state, "...Please use " + cCurrentModuleObject + ":Interzone to enter this surface.");
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = BlankString; // putting it as blank will not confuse things later.
                        ErrorsFound = true;
                    }
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (GettingIZSurfaces) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(OtherSurfaceField);
                        IZFound = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName, Zone, state.dataGlobal->NumOfZones);
                        if (IZFound > 0) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface;
                    } else { // Interior Window
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                    }
                }

                // This is the parent's property:
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) { // OtherZone - unmatched interior surface
                    if (GettingIZSurfaces) {
                        ++NeedToAddSubSurfaces;
                    } else { // Interior Window
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid Interzone Surface, specify " +
                                        cCurrentModuleObject + ":InterZone");
                        ShowContinueError(state, "...when base surface is an interzone surface, subsurface must also be an interzone surface.");
                        ++NeedToAddSubSurfaces;
                        ErrorsFound = true;
                    }
                }

                if (GettingIZSurfaces) {
                    if (lAlphaFieldBlanks(OtherSurfaceField)) {
                        // blank -- set it up for unentered adjacent zone
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) {        // already set but need Zone
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCondName; // base surface has it
                        } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName; // base surface has it
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface;
                        } else { // not correct boundary condition for interzone subsurface
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                            "\", invalid Base Surface type for Interzone Surface");
                            ShowContinueError(state, "...when base surface is not an interzone surface, subsurface must also not be an interzone surface.");
                            ErrorsFound = true;
                        }
                    }
                }

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
                }

                //      state.dataSurfaceGeometry->SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier = int(rNumericArgs(1));
                // Only windows, glass doors and doors can have Multiplier > 1:
                if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Window && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Door) &&
                    rNumericArgs(1) > 1.0) {
                    ShowWarningError(state,
                                     format("{}=\"{}\", invalid {}=[{:.1T}].",
                                            cCurrentModuleObject,
                                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                            cNumericFieldNames(1),
                                            rNumericArgs(1)));
                    ShowContinueError(state, "...because " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + " multiplier will be set to 1.0.");
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier = 1.0;
                }

                MakeRelativeRectangularVertices(
                    state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf, SurfNum, rNumericArgs(2), rNumericArgs(3), rNumericArgs(4), rNumericArgs(5));

                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", Surface Area <= 0.0; Entered Area={:.2T}",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area));
                    ErrorsFound = true;
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList.clear();
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl = false;

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList.clear();
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = 0;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedStormWinConstructionList.clear();
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeStormWinShadedConstruction= 0;

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                if (!GettingIZSurfaces && (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor)) {

                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", Other side coefficients are not allowed with windows.");
                        ErrorsFound = true;
                    }

                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                        "\", Exterior boundary condition = Ground is not allowed with windows.");
                        ErrorsFound = true;
                    }

                    CheckWindowShadingControlFrameDivider(state, "GetRectSubSurfaces", ErrorsFound, SurfNum, FrameField);

                } // check on non-opaquedoor subsurfaces

                CheckSubSurfaceMiscellaneous(state, "GetRectSubSurfaces", ErrorsFound, SurfNum, cAlphaArgs(1), cAlphaArgs(2), AddedSubSurfaces);

            } // Getting Items
        }
    }

    void CheckWindowShadingControlFrameDivider(EnergyPlusData &state,
                                               std::string const &cRoutineName, // routine name calling this one (for error messages)
                                               bool &ErrorsFound,               // true if errors have been found or are found here
                                               int const SurfNum,               // current surface number
                                               int const FrameField             // field number for frame/divider
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
        using namespace DataIPShortCuts;

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

        // If WindowShadingControl has been specified for this window --
        // Set shaded construction number if shaded construction was specified in WindowShadingControl.
        // Otherwise, create shaded construction if WindowShadingControl for this window has
        // interior or exterior shade/blind (but not between-glass shade/blind) specified.

        for (std::size_t shadeControlIndex = 0; shadeControlIndex < state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList.size(); ++shadeControlIndex){
            int WSCPtr = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList[shadeControlIndex];
            ConstrNumSh = 0;
            if (!ErrorsFound && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl) {
                ConstrNumSh = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex];
                if (ConstrNumSh > 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNumSh;
                } else {
                    if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorShade ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorScreen ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorBlind) {
                        ShDevNum = WindowShadingControl(WSCPtr).ShadingDevice;
                        if (ShDevNum > 0) {
                            CreateShadedWindowConstruction(state, SurfNum, WSCPtr, ShDevNum, shadeControlIndex);
                            ConstrNumSh = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction;
                        }
                    }
                }
            }

            // Error checks for shades and blinds

            ConstrNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
            if (!ErrorsFound && WSCPtr > 0 && ConstrNum > 0 && ConstrNumSh > 0) {

                if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind) {
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 1 != TotLayers) {
                        ShowWarningError(state, "WindowShadingControl: Interior shade or blind: Potential problem in match of unshaded/shaded constructions, "
                                         "shaded should have 1 more layers than unshaded.");
                        ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError(state, "If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay)) {
                            ErrorsFound = true;
                            ShowSevereError(state, " The glass and gas layers in the shaded and unshaded constructions do not match for window=" +
                                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                            ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                            ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                            break;
                        }
                    }
                }

                if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorShade ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorScreen ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorBlind) {
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 1 != TotLayers) {
                        ShowWarningError(state, "WindowShadingControl: Exterior shade, screen or blind: Potential problem in match of unshaded/shaded "
                                         "constructions, shaded should have 1 more layer than unshaded.");
                        ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError(state, "If preceding two constructions have the same name, you have likely specified a WindowShadingControl (Field "
                                          "#3) with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay + 1)) {
                            ErrorsFound = true;
                            ShowSevereError(state, " The glass and gas layers in the shaded and unshaded constructions do not match for window=" +
                                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                            ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                            ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                            break;
                        }
                    }
                }

                if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassShade ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                    // Divider not allowed with between-glass shade or blind
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider > 0) {
                        if (FrameDivider(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider).DividerWidth > 0.0) {
                            ShowWarningError(state, "A divider cannot be specified for window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                            ShowContinueError(state, ", which has a between-glass shade or blind.");
                            ShowContinueError(state, "Calculation will proceed without the divider for this window.");
                            FrameDivider(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider).DividerWidth = 0.0;
                        }
                    }
                    // Check consistency of gap widths between unshaded and shaded constructions
                    TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 2 != TotLayers) {
                        ShowWarningError(state, "WindowShadingControl: Between Glass Shade/Blind: Potential problem in match of unshaded/shaded constructions, "
                                         "shaded should have 2 more layers than unshaded.");
                        ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError(state, "If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers)) {
                        ShowSevereError(state, cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match.");
                        ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                          ", Material=" + state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Name);
                        ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name +
                                          ", Material=" + state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers)).Name);
                        ErrorsFound = true;
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(1) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)) {
                        ShowSevereError(state, cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match.");
                        ShowContinueError(state, "Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                          ", Material=" + state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Name);
                        ShowContinueError(state, "Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name +
                                          ", Material=" + state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Name);
                        ErrorsFound = true;
                    }
                    if (TotGlassLayers == 2 || TotGlassLayers == 3) {
                        MatGap = state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers);
                        MatSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 1);
                        if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                            MatGapCalc = std::abs(state.dataMaterial->Material(MatGap).Thickness - (state.dataMaterial->Material(MatGap1).Thickness + state.dataMaterial->Material(MatGap2).Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(state, cRoutineName + ": The gap width(s) for the unshaded window construction " + state.dataConstruction->Construct(ConstrNum).Name);
                                ShowContinueError(state, "are inconsistent with the gap widths for shaded window construction " + state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError(state, "for window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + ", which has a between-glass blind.");
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} -",
                                                         state.dataMaterial->Material(MatGap).Name,
                                                         state.dataMaterial->Material(MatGap).Thickness));
                                ShowContinueError(state,
                                                  format("..( Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap1).Name,
                                                         state.dataMaterial->Material(MatGap1).Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} )=[{:.3R}] >.001",
                                                         state.dataMaterial->Material(MatGap2).Name,
                                                         state.dataMaterial->Material(MatGap2).Thickness,
                                                         MatGapCalc));
                                ErrorsFound = true;
                            }
                        } else { // Between-glass shade
                            MatGapCalc = std::abs(state.dataMaterial->Material(MatGap).Thickness -
                                                  (state.dataMaterial->Material(MatGap1).Thickness + state.dataMaterial->Material(MatGap2).Thickness + state.dataMaterial->Material(MatSh).Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(state, cRoutineName + ": The gap width(s) for the unshaded window construction " + state.dataConstruction->Construct(ConstrNum).Name);
                                ShowContinueError(state, "are inconsistent with the gap widths for shaded window construction " + state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError(state, "for window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + ", which has a between-glass shade.");
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} -",
                                                         state.dataMaterial->Material(MatGap).Name,
                                                         state.dataMaterial->Material(MatGap).Thickness));
                                ShowContinueError(state,
                                                  format("...( Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap1).Name,
                                                         state.dataMaterial->Material(MatGap1).Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} +",
                                                         state.dataMaterial->Material(MatGap2).Name,
                                                         state.dataMaterial->Material(MatGap2).Thickness));
                                ShowContinueError(state,
                                                  format("..Material={} thickness={:.3R} )=[{:.3R}] >.001",
                                                         state.dataMaterial->Material(MatSh).Name,
                                                         state.dataMaterial->Material(MatSh).Thickness,
                                                         MatGapCalc));
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
        }

        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides != 3) { // Rectangular Window
            // Initialize the FrameDivider number for this window. W5FrameDivider will be positive if
            // this window's construction came from the Window5 data file and that construction had an
            // associated frame or divider. It will be zero if the window's construction is not from the
            // Window5 data file, or the construction is from the data file, but the construction has no
            // associated frame or divider. Note that if there is a FrameDivider candidate for this
            // window from the Window5 data file it is used instead of the window's input FrameDivider.

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider = state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).W5FrameDivider;

                // Warning if FrameAndDivider for this window is over-ridden by one from Window5 Data File
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider > 0 && !lAlphaFieldBlanks(FrameField)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(FrameField) + "=\"" +
                                    cAlphaArgs(FrameField) + "\"");
                    ShowContinueError(state, "will be replaced with FrameAndDivider from Window5 Data File entry " +
                                      state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).Name);
                }

                if (!lAlphaFieldBlanks(FrameField) && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider == 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider = UtilityRoutines::FindItemInList(cAlphaArgs(FrameField), FrameDivider);
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider == 0) {
                        if (!state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) +
                                            "=\"" + cAlphaArgs(FrameField) + "\"");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) +
                                            "=\"" + cAlphaArgs(FrameField) + "\"");
                            ShowContinueError(state, "...Frame/Divider is not supported in Equivalent Layer Window model.");
                        }
                    }
                    // Divider not allowed with between-glass shade or blind
                    for (int WSCPtr : state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList) {
                        if (!ErrorsFound && WSCPtr > 0 && ConstrNumSh > 0) {
                            if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassShade ||
                                WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider > 0) {
                                    if (FrameDivider(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider).DividerWidth > 0.0) {
                                        ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " +
                                                        cAlphaFieldNames(FrameField) + "=\"" + cAlphaArgs(FrameField) + "\"");
                                        ShowContinueError(state, "Divider cannot be specified because the construction has a between-glass shade or blind.");
                                        ShowContinueError(state, "Calculation will proceed without the divider for this window.");
                                        ShowContinueError(
                                            state,
                                            format("Divider width = [{:.2R}].",
                                                   FrameDivider(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider).DividerWidth));
                                        FrameDivider(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider).DividerWidth = 0.0;
                                    }
                                } // End of check if window has divider
                            }     // End of check if window has a between-glass shade or blind
                        }         // End of check if window has a shaded construction
                    }             // end of looping through window shading controls of window
                }             // End of check if window has an associated FrameAndDivider
            }                 // End of check if window has a construction
        }

        if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider > 0) {
                // Equivalent Layer window does not have frame/divider model
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) + "=\"" +
                                cAlphaArgs(FrameField) + "\"");
                ShowContinueError(state, "Frame/Divider is not supported in Equivalent Layer Window model.");
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider = 0;
            }
        }
    }

    void CheckSubSurfaceMiscellaneous(EnergyPlusData &state,
                                      std::string const &cRoutineName,           // routine name calling this one (for error messages)
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

        // Warning if window has multiplier > 1 and SolarDistribution = FullExterior or FullInteriorExterior

        if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) &&
            SolarDistribution > MinimalShadowing && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier > 1.0) {
            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state, cRoutineName + ": A Multiplier > 1.0 for window/glass door " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                ShowContinueError(state, "in conjunction with SolarDistribution = FullExterior or FullInteriorExterior");
                ShowContinueError(state, "can cause inaccurate shadowing on the window and/or");
                ShowContinueError(state, "inaccurate interior solar distribution from the window.");
            }
            ++state.dataErrTracking->TotalMultipliedWindows;
        }

        //  Require that a construction referenced by a surface that is a window
        //  NOT have a shading device layer; use WindowShadingControl to specify a shading device.
        ConstrNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
        if (ConstrNum > 0) {
            NumShades = 0;
            for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                if (LayerPtr == 0) continue; // Error is caught already, will terminate later
                if (state.dataMaterial->Material(LayerPtr).Group == Shade || state.dataMaterial->Material(LayerPtr).Group == WindowBlind || state.dataMaterial->Material(LayerPtr).Group == Screen) ++NumShades;
            }
            if (NumShades != 0) {
                ShowSevereError(state, cRoutineName + ": Window \"" + SubSurfaceName + "\" must not directly reference");
                ShowContinueError(state, "a Construction (i.e, \"" + SubSurfaceConstruction + "\") with a shading device.");
                ShowContinueError(state, "Use WindowShadingControl to specify a shading device for a window.");
                ErrorsFound = true;
            }
        }

        // Disallow glass transmittance dirt factor for interior windows and glass doors

        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment &&
            (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor)) {
            ConstrNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
            if (ConstrNum > 0) {
                for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                    LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                    if (state.dataMaterial->Material(LayerPtr).Group == WindowGlass && state.dataMaterial->Material(LayerPtr).GlassTransDirtFactor < 1.0) {
                        ShowSevereError(state, cRoutineName + ": Interior Window or GlassDoor " + SubSurfaceName + " has a glass layer with");
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

        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction != 0) {

            if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).FromWindow5DataFile) {

                ModifyWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else {
                // Calculate net area for base surface (note that ModifyWindow, above, adjusts net area of
                // base surface for case where window construction is from Window5 Data File
                // In case there is in error in this window's base surface (i.e. none)..
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf > 0) {
                    state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;

                    // Subtract TDD:DIFFUSER area from other side interzone surface
                    if ((state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser) &&
                        not_blank(state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).ExtBoundCondName)) { // Base surface is an interzone surface
                        // Lookup interzone surface of the base surface
                        // (Interzone surfaces have not been assigned yet, but all base surfaces should already be loaded.)
                        Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).ExtBoundCondName, state.dataSurfaceGeometry->SurfaceTmp, SurfNum);
                        if (Found != 0) state.dataSurfaceGeometry->SurfaceTmp(Found).Area -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
                    }
                    if (state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
                        ShowSevereError(state, cRoutineName +
                                        ": Surface Openings have too much area for base surface=" + state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Name);
                        ShowContinueError(state, "Opening Surface creating error=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                        ErrorsFound = true;
                    }
                    // Net area of base surface with unity window multipliers (used in shadowing checks)
                    // For Windows, Glass Doors and Doors, just one area is subtracted.  For the rest, should be
                    // full area.
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) {
                        state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area / state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
                    } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Door) { // Door, TDD:Diffuser, TDD:DOME
                        state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area / state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
                    } else {
                        state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
                    }
                }
            }
        }
    }

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
        Real64 SurfAzimuth; // Surface Azimuth/Facing (same as Base Surface)
        Real64 SurfTilt;    // Tilt (same as Base Surface)
        Real64 XLLC;
        Real64 YLLC;
        Real64 ZLLC;
        Real64 CosSurfAzimuth;
        Real64 SinSurfAzimuth;
        Real64 CosSurfTilt;
        Real64 SinSurfTilt;
        Real64 BaseCosSurfAzimuth;
        Real64 BaseSinSurfAzimuth;
        Real64 BaseCosSurfTilt;
        Real64 BaseSinSurfTilt;
        Array1D<Real64> XX(4);
        Array1D<Real64> YY(4);
        Real64 Perimeter;
        int n;
        int Vrt;

        if (BaseSurfNum == 0) return; // invalid base surface, don't bother

        // Tilt and Facing (Azimuth) will be same as the Base Surface

        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height = Height;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width = Length;

        SurfAzimuth = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth;
        SurfTilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt;
        CosSurfAzimuth = std::cos(SurfAzimuth * DataGlobalConstants::DegToRadians);
        SinSurfAzimuth = std::sin(SurfAzimuth * DataGlobalConstants::DegToRadians);
        CosSurfTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
        SinSurfTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
        BaseCosSurfAzimuth = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim;
        BaseSinSurfAzimuth = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim;
        BaseCosSurfTilt = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt;
        BaseSinSurfTilt = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinTilt;

        XLLC = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).x - XCoord * BaseCosSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseSinSurfAzimuth;
        YLLC = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).y + XCoord * BaseSinSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseCosSurfAzimuth;
        ZLLC = state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).z + ZCoord * BaseSinSurfTilt;

        XX(1) = 0.0;
        XX(2) = 0.0;
        XX(3) = Length;
        XX(4) = Length;
        YY(1) = Height;
        YY(4) = Height;
        YY(3) = 0.0;
        YY(2) = 0.0;

        for (n = 1; n <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++n) {
            Vrt = n;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).x = XLLC - XX(n) * CosSurfAzimuth - YY(n) * CosSurfTilt * SinSurfAzimuth;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).y = YLLC + XX(n) * SinSurfAzimuth - YY(n) * CosSurfTilt * CosSurfAzimuth;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).z = ZLLC + YY(n) * SinSurfTilt;
        }

        CreateNewellAreaVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = VecLength(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
        CreateNewellSurfaceNormalVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = SurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = SurfTilt;
        // Sine and cosine of azimuth and tilt
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = SinSurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = CosSurfAzimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = SinSurfTilt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = CosSurfTilt;
        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Window && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class != SurfaceClass::Door)
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
        // Outward normal unit vector (pointing away from room)
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
        for (n = 1; n <= 3; ++n) {
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
            if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
        }

        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Roof .and. SurfTilt > 80.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError(state, 'Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Floor .and. SurfTilt < 170.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError(state, 'Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area *= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
        // Can perform tests on this surface here
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);

        Perimeter = distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1));
        for (Vrt = 2; Vrt <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++Vrt) {
            Perimeter += distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1));
        }
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter = Perimeter;

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the HeatTransfer Surface Data,
        // checks it for errors, etc.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        //  Attached Shading Surface Definition
        // Surface:Shading:Attached,
        //       \memo used For fins, overhangs, elements that shade the building, are attached to the building
        //       \memo but are not part of the heat transfer calculations
        //  A1 , \field User Supplied Surface Name
        //       \required-field
        //       \type alpha
        //       \reference AttachedShadingSurfNames
        //  A2 , \field Base Surface Name
        //       \required-field
        //       \type object-list
        //       \object-list SurfaceNames
        //  A3,  \field TransSchedShadowSurf
        //       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
        //       \type object-list
        //       \object-list ScheduleNames
        //  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
        //       \required-field
        //       \note currently limited 3 or 4, later?
        //       \minimum 3
        //       \maximum 4
        //       \note vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
        //       \note are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
        //       \note for some internal calculations, but all coordinates are given in an "absolute" system.
        //  N2,  \field Vertex 1 X-coordinate
        //       \units m
        //       \type real
        //  N3-13; as indicated by the N2 value

        // Using/Aliasing
        using namespace DataIPShortCuts;

        using ScheduleManager::CheckScheduleValueMinMax;
        using ScheduleManager::GetScheduleIndex;
        using ScheduleManager::GetScheduleMaxValue;
        using ScheduleManager::GetScheduleMinValue;
        using namespace DataReportingFlags;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;     // IO Status when calling get input subroutine
        int NumAlphas;  // Number of alpha names being passed
        int NumNumbers; // Number of properties being passed
        int Found;      // For matching interzone surfaces
        int Loop;
        Real64 SchedMinValue;
        Real64 SchedMaxValue;

        if (TotShdSubs > 0 && SolarDistribution == MinimalShadowing) {
            ShowWarningError(state, "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
        }

        cCurrentModuleObject = "Shading:Zone:Detailed";
        inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
        if (NumAlphas != 3) {
            ShowSevereError(state,
                            format("{}: Object Definition indicates not = 3 Alpha Objects, Number Indicated={}", cCurrentModuleObject, NumAlphas));
            ErrorsFound = true;
        }

        for (Loop = 1; Loop <= TotShdSubs; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (GlobalNames::VerifyUniqueInterObjectName(state, state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            ++SurfNum;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = false;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(2);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            Found = UtilityRoutines::FindItemInList(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName, state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            if (Found > 0) {
                // SurfaceTmp(SurfNum)%BaseSurf=Found
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ErrorsFound = true;
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ShowContinueError(state, "...trying to attach a shading device to an interzone surface.");
                ErrorsFound = true;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ShowContinueError(state, "...trying to attach a shading device to an interior surface.");
                ErrorsFound = true;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }

            if (!lAlphaFieldBlanks(3)) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(3) + " not found=\"" +
                                    cAlphaArgs(3));
                    ErrorsFound = true;
                }
            } else {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex != 0) {
                if (!CheckScheduleValueMinMax(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                    "\", values not in range [0,1].");
                    ErrorsFound = true;
                }
                SchedMinValue = GetScheduleMinValue(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMinValue = SchedMinValue;
                SchedMaxValue = GetScheduleMaxValue(state, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                if (SchedMinValue == 1.0) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                     "\", is always transparent.");
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).IsTransparent = true;
                }
                if (SchedMinValue < 0.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                    "\", has schedule values < 0.");
                    ShowContinueError(state, "...Schedule values < 0 have no meaning for shading elements.");
                }
                if (SchedMaxValue > 1.0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                    "\", has schedule values > 1.");
                    ShowContinueError(state, "...Schedule values > 1 have no meaning for shading elements.");
                }
                if (std::abs(SchedMinValue - SchedMaxValue) > 1.0e-6) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfSchedVaries = true;
                    ShadingTransmittanceVaries = true;
                }
            }
            if (lNumericFieldBlanks(1) || rNumericArgs(1) == DataGlobalConstants::AutoCalculate) {
                rNumericArgs(1) = (NumNumbers - 1) / 3;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
                if (mod(NumNumbers - 1, 3) != 0) {
                    ShowWarningError(state,
                                     cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " +
                                         format("{} not even multiple of 3. Will read in {}",
                                                cNumericFieldNames(1),
                                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                }
                if (rNumericArgs(1) < 3) {
                    ShowSevereError(state,
                                    format("{}=\"{}\", {} (autocalculate) must be >= 3. Only {} provided.",
                                           cCurrentModuleObject,
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                           cNumericFieldNames(1),
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    ErrorsFound = true;
                    continue;
                }
            } else {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
            }
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
            GetVertices(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, rNumericArgs({2, _}));
            CheckConvexity(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
            //    IF (SurfaceTmp(SurfNum)%Sides == 3) THEN
            //      CALL ShowWarningError(state, TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            //                        ' should not be triangular.')
            //      CALL ShowContinueError(state, '...Check results carefully.')
            //      ErrorsFound=.TRUE.
            //    ENDIF
            // Reset surface to be "detached"
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = 0;
            //    SurfaceTmp(SurfNum)%BaseSurfName='  '
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = 0;
            // SurfaceTmp(SurfNum)%ZoneName='  '
            if (MakeMirroredAttachedShading) {
                MakeMirrorSurface(state, SurfNum);
            }
        }
    }

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
        using namespace DataIPShortCuts;

        using namespace DataReportingFlags;
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
        Real64 Xp;
        Real64 Yp;
        Real64 Zp;
        Real64 XLLC;
        Real64 YLLC;
        int BaseSurfNum;
        Real64 TiltAngle;
        bool MakeFin;

        if ((TotOverhangs + TotOverhangsProjection + TotFins + TotFinsProjection) > 0 && SolarDistribution == MinimalShadowing) {
            ShowWarningError(state, "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
        }

        for (Item = 1; Item <= 4; ++Item) {

            cCurrentModuleObject = cModuleObjects(Item);
            if (Item == 1) {
                ItemsToGet = TotOverhangs;
            } else if (Item == 2) {
                ItemsToGet = TotOverhangsProjection;
            } else if (Item == 3) {
                ItemsToGet = TotFins;
            } else { // ! (Item == 4) THEN
                ItemsToGet = TotFinsProjection;
            }

            for (Loop = 1; Loop <= ItemsToGet; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumNumbers,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                if (GlobalNames::VerifyUniqueInterObjectName(state,
                        state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = false;
                // this object references a window or door....
                Found = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
                if (Found > 0) {
                    BaseSurfNum = state.dataSurfaceGeometry->SurfaceTmp(Found).BaseSurf;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(Found).BaseSurfName;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ErrorsFound = true;
                    continue;
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnenteredAdjacentZoneSurface) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ShowContinueError(state, "...trying to attach a shading device to an interzone surface.");
                    ErrorsFound = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == state.dataSurfaceGeometry->UnreconciledZoneSurface) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ShowContinueError(state, "...trying to attach a shading device to an interior surface.");
                    ErrorsFound = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }

                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;

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
                    Length = rNumericArgs(3) + rNumericArgs(4) + state.dataSurfaceGeometry->SurfaceTmp(Found).Width;
                    if (Item == 1) {
                        Depth = rNumericArgs(5);
                    } else if (Item == 2) {
                        Depth = rNumericArgs(5) * state.dataSurfaceGeometry->SurfaceTmp(Found).Height;
                    }

                    if (Length * Depth <= 0.0) {
                        ShowSevereError(state,
                                        format("{}=\"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                               cCurrentModuleObject,
                                               cAlphaArgs(1),
                                               Length * Depth));
                        continue;
                    }

                    TiltAngle = state.dataSurfaceGeometry->SurfaceTmp(Found).Tilt + rNumericArgs(2);
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = TiltAngle;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(Found).Azimuth;

                    // Make it relative to surface origin.....
                    Xp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).x - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).x;
                    Yp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).y - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).y;
                    Zp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).z - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).z;

                    XLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim + Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim;

                    YLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt -
                           Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt + Zp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinTilt;

                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

                    MakeRelativeRectangularVertices(
                        state, BaseSurfNum, SurfNum, XLLC - rNumericArgs(3), YLLC + state.dataSurfaceGeometry->SurfaceTmp(Found).Height + rNumericArgs(1), Length, Depth);

                    // Reset surface to be "detached"
                    //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                    //    SurfaceTmp(SurfNum)%ZoneName='  '

                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = 0;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = 0;

                    // and mirror
                    if (MakeMirroredAttachedShading) {
                        MakeMirrorSurface(state, SurfNum);
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
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + " Left";
                    Length = rNumericArgs(2) + rNumericArgs(3) + state.dataSurfaceGeometry->SurfaceTmp(Found).Height;
                    if (Item == 3) {
                        Depth = rNumericArgs(5);
                    } else if (Item == 4) {
                        Depth = rNumericArgs(5) * state.dataSurfaceGeometry->SurfaceTmp(Found).Width;
                    }

                    MakeFin = true;
                    if (Length * Depth <= 0.0) {
                        ShowWarningError(state,
                                         format("{}=Left Fin of \"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                                cCurrentModuleObject,
                                                cAlphaArgs(1),
                                                Length * Depth));
                        MakeFin = false;
                    }

                    if (MakeFin) {
                        TiltAngle = state.dataSurfaceGeometry->SurfaceTmp(Found).Tilt;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = TiltAngle;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(Found).Azimuth - (180.0 - rNumericArgs(4));

                        // Make it relative to surface origin.....

                        Xp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).x - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).x;
                        Yp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).y - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).y;
                        Zp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).z - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).z;

                        XLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim + Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim;

                        YLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt -
                               Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt + Zp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinTilt;

                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = std::cos(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = std::sin(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = std::cos(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = std::sin(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians);

                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

                        MakeRelativeRectangularVertices(state, BaseSurfNum, SurfNum, XLLC - rNumericArgs(1), YLLC - rNumericArgs(3), -Depth, Length);

                        // Reset surface to be "detached"
                        //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                        //    SurfaceTmp(SurfNum)%ZoneName='  '

                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = 0;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = 0;

                        // and mirror
                        if (MakeMirroredAttachedShading) {
                            MakeMirrorSurface(state, SurfNum);
                        }
                    } else {
                        --SurfNum;
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
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = cAlphaArgs(1) + " Right"; // Set the Surface Name in the Derived Type
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = false;
                    BaseSurfNum = state.dataSurfaceGeometry->SurfaceTmp(Found).BaseSurf;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(Found).BaseSurfName;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtBoundCond;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtSolar;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(Found).ExtWind;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = state.dataSurfaceGeometry->SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports

                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
                    Length = rNumericArgs(7) + rNumericArgs(8) + state.dataSurfaceGeometry->SurfaceTmp(Found).Height;
                    if (Item == 3) {
                        Depth = rNumericArgs(10);
                    } else if (Item == 4) {
                        Depth = rNumericArgs(10) * state.dataSurfaceGeometry->SurfaceTmp(Found).Width;
                    }

                    MakeFin = true;
                    if (Length * Depth <= 0.0) {
                        ShowWarningError(state,
                                         format("{}=Right Fin of \"{}\", illegal surface area=[{:.2R}]. Surface will NOT be entered.",
                                                cCurrentModuleObject,
                                                cAlphaArgs(1),
                                                Length * Depth));
                        MakeFin = false;
                    }

                    if (MakeFin) {
                        // Make it relative to surface origin.....

                        Xp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).x - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).x;
                        Yp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).y - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).y;
                        Zp = state.dataSurfaceGeometry->SurfaceTmp(Found).Vertex(2).z - state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).Vertex(2).z;

                        XLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim + Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim;

                        YLLC = -Xp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt -
                               Yp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosAzim * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).CosTilt + Zp * state.dataSurfaceGeometry->SurfaceTmp(BaseSurfNum).SinTilt;

                        TiltAngle = state.dataSurfaceGeometry->SurfaceTmp(Found).Tilt;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = TiltAngle;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(Found).Azimuth - (180.0 - rNumericArgs(9));
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = std::cos(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = std::sin(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = std::cos(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = std::sin(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians);

                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = 4;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

                        MakeRelativeRectangularVertices(
                            state, BaseSurfNum, SurfNum, XLLC + state.dataSurfaceGeometry->SurfaceTmp(Found).Width + rNumericArgs(6), YLLC - rNumericArgs(8), -Depth, Length);

                        // Reset surface to be "detached"
                        //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                        //    SurfaceTmp(SurfNum)%ZoneName='  '

                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = 0;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = 0;

                        // and mirror
                        if (MakeMirroredAttachedShading) {
                            MakeMirrorSurface(state, SurfNum);
                        }
                    } else {
                        --SurfNum;
                    }
                }
            }
        }
    }

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
        using namespace DataIPShortCuts;
        using namespace Vectors;
        using General::CheckCreatedZoneItemName;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetIntMassSurfaceData: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int IOStat;                // IO Status when calling get input subroutine
        int SurfaceNumAlpha;       // Number of material alpha names being passed
        int SurfaceNumArg;         // Number of material properties being passed
        int ZoneNum;               // index to a zone
        int NumIntMassSurfaces(0); // total count of internal mass surfaces
        bool errFlag;              //  local error flag

        cCurrentModuleObject = "InternalMass";
        int TotIntMass = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotIntMass == 0) return;

        IntMassObjects.allocate(TotIntMass);

        // scan for use of Zone lists in InternalMass objects
        errFlag = false;
        NumIntMassSurfaces = 0;
        for (int Item = 1; Item <= TotIntMass; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          SurfaceNumAlpha,
                                          rNumericArgs,
                                          SurfaceNumArg,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (GlobalNames::VerifyUniqueInterObjectName(state, state.dataSurfaceGeometry->UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            IntMassObjects(Item).Name = cAlphaArgs(1);
            IntMassObjects(Item).GrossArea = rNumericArgs(1);
            IntMassObjects(Item).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
            IntMassObjects(Item).ZoneOrZoneListName = cAlphaArgs(3);
            int Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(3), Zone, state.dataGlobal->NumOfZones);
            int ZLItem = 0;
            if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(3), ZoneList);
            if (Item1 > 0) {
                ++NumIntMassSurfaces;
                IntMassObjects(Item).NumOfZones = 1;
                IntMassObjects(Item).ZoneListActive = false;
                IntMassObjects(Item).ZoneOrZoneListPtr = Item1;
            } else if (ZLItem > 0) {
                NumIntMassSurfaces += ZoneList(ZLItem).NumOfZones;
                IntMassObjects(Item).NumOfZones = ZoneList(ZLItem).NumOfZones;
                IntMassObjects(Item).ZoneListActive = true;
                IntMassObjects(Item).ZoneOrZoneListPtr = ZLItem;
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ++SurfNum;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::INVALID;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                ErrorsFound = true;
                errFlag = true;
            }

            if (errFlag) {
                ShowSevereError(state, RoutineName + "Errors with invalid names in " + cCurrentModuleObject + " objects.");
                ShowContinueError(state, "...These will not be read in.  Other errors may occur.");
                NumIntMassSurfaces = 0;
            }

            if (IntMassObjects(Item).Construction == 0) {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(2) + " not found=" + cAlphaArgs(2));
            } else if (state.dataConstruction->Construct(IntMassObjects(Item).Construction).TypeIsWindow) {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" - has Window materials.");
            } else {
                state.dataConstruction->Construct(IntMassObjects(Item).Construction).IsUsed = true;
            }
        }

        if (NumIntMassSurfaces > 0) {
            for (int Loop = 1; Loop <= TotIntMass; ++Loop) {

                for (int Item1 = 1; Item1 <= IntMassObjects(Loop).NumOfZones; ++Item1) {

                    ++SurfNum;

                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction = IntMassObjects(Loop).Construction;
                    if (!IntMassObjects(Loop).ZoneListActive) {
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = IntMassObjects(Loop).ZoneOrZoneListPtr;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = IntMassObjects(Loop).Name;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::IntMass;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = IntMassObjects(Loop).ZoneOrZoneListName;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;
                    } else {
                        CheckCreatedZoneItemName(state, RoutineName,
                                                 cCurrentModuleObject,
                                                 Zone(ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                                 ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).MaxZoneNameLength,
                                                 IntMassObjects(Loop).Name,
                                                 state.dataSurfaceGeometry->SurfaceTmp,
                                                 SurfNum - 1,
                                                 state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                                 errFlag);

                        ZoneNum = ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).Zone(Item1);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone = ZoneNum;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class = SurfaceClass::IntMass;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName = Zone(ZoneNum).Name;
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf = true;
                        if (errFlag) ErrorsFound = true;
                    }

                    if (IntMassObjects(Loop).Construction > 0) {
                        if (state.dataConstruction->Construct(IntMassObjects(Loop).Construction).IsUsed) {
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue = IntMassObjects(Loop).Construction;
                        }
                    }
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = IntMassObjects(Loop).GrossArea;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height = 1.0;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = 90.0;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = 0.0; // Tuned Was std::cos( 90.0 * DegToRadians )
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = 1.0; // Tuned Was std::sin( 90.0 * DegToRadians )
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = 0.0;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = 1.0; // Tuned Was std::cos( 0.0 )
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = 0.0; // Tuned Was std::sin( 0.0 )
                    // Outward normal unit vector (pointing away from room)
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = 0.5;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar = false;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind = false;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond = state.dataSurfaceGeometry->UnreconciledZoneSurface;
                }
            }
        }
    }

    int GetNumIntMassSurfaces(EnergyPlusData &state) // Number of Internal Mass Surfaces to obtain

    {
        // Counts internal mass surfaces applied to zones and zone lists

        // Using/Aliasing
        using namespace DataIPShortCuts;

        int IOStat;          // IO Status when calling get input subroutine
        int SurfaceNumAlpha; // Number of material alpha names being passed
        int SurfaceNumArg;   // Number of material properties being passed
        int NumIntMassSurf;  // total count of internal mass surfaces

        NumIntMassSurf = 0;
        int TotIntMass = inputProcessor->getNumObjectsFound(state, "InternalMass");

        if (TotIntMass == 0) return NumIntMassSurf;

        cCurrentModuleObject = "InternalMass";
        // scan for zones and zone lists in InternalMass objects
        for (int Item = 1; Item <= TotIntMass; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          SurfaceNumAlpha,
                                          rNumericArgs,
                                          SurfaceNumArg,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            int Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(3), Zone, state.dataGlobal->NumOfZones);
            int ZLItem = 0;
            if (Item1 == 0 && NumOfZoneLists > 0) ZLItem = UtilityRoutines::FindItemInList(cAlphaArgs(3), ZoneList);
            if (Item1 > 0) {
                ++NumIntMassSurf;
            } else if (ZLItem > 0) {
                NumIntMassSurf += ZoneList(ZLItem).NumOfZones;
            }
        }
        NumIntMassSurf = max(NumIntMassSurf, TotIntMass);
        return NumIntMassSurf;
    }

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
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;                       // IO Status when calling get input subroutine
        int NumAlpha;                     // Number of alpha names being passed
        int NumProp;                      // Number of properties being passed
        int TotShadingSurfaceReflectance; // Total Shading Surface Refleftance statements
        int Loop;                         // DO loop index
        int SurfNum;                      // Surface number
        int GlConstrNum;                  // Glazing construction number
        bool WrongSurfaceType;

        // For shading surfaces, initialize value of reflectance values to default values. These values
        // may be overridden below for shading surfaces with an associated Shading Surface Reflectance object.
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                continue;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = 0.2;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = 0.2;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = 0.0;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = 0;
        }

        // Get the total number of Shading Surface Reflectance objects
        cCurrentModuleObject = "ShadingProperty:Reflectance";
        TotShadingSurfaceReflectance = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        //  IF(TotShadingSurfaceReflectance.EQ.0) RETURN

        for (Loop = 1; Loop <= TotShadingSurfaceReflectance; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlpha,
                                          rNumericArgs,
                                          NumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            if (SurfNum == 0) {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid specification");
                ShowContinueError(state, ".. not found " + cAlphaFieldNames(1) + "=\"" + cAlphaArgs(1) + "\".");
                //      ErrorsFound =.TRUE.
                continue;
            }

            // Check that associated surface is a shading surface
            WrongSurfaceType = false;
            if (SurfNum != 0) {
                if (!(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                    WrongSurfaceType = true;
                if (WrongSurfaceType) {
                    ShowSevereError(state, "GetShadingSurfReflectanceData: " + cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                    "\", surface is not a shading surface.");
                    ErrorsFound = true;
                    continue;
                }
            }

            // If associated surface is a shading surface, set reflectance values
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = rNumericArgs(3);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(1);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(2);
            if (rNumericArgs(3) > 0.0) {
                GlConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
                if (GlConstrNum == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) +
                                    " not found=" + cAlphaArgs(2));
                    ErrorsFound = true;
                } else {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = GlConstrNum;
            }
            SurfNum = UtilityRoutines::FindItemInList("Mir-" + cAlphaArgs(1), state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            if (SurfNum == 0) continue;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = rNumericArgs(3);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(1);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(2);
            if (rNumericArgs(3) > 0.0) {
                GlConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
                if (GlConstrNum != 0) {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = GlConstrNum;
            }

        } // End of loop over Shading Surface Reflectance objects

        // Write reflectance values to .eio file.
        print(state.files.eio, "! <ShadingProperty Reflectance>,Shading Surface Name,Shading Type,Diffuse Solar Reflectance, Diffuse "
               "Visible Reflectance,Surface Glazing Fraction,Surface Glazing Contruction\n");

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                continue;

            constexpr auto fmt{"ShadingProperty Reflectance,{},{},{:.2R},{:.2R},{:.2R}, {}\n"};
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct != 0) {
                print(state.files.eio,
                      fmt,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                      cSurfaceClass(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class),
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingFrac,
                      state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct).Name);
            } else {
                print(state.files.eio,
                      fmt,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                      cSurfaceClass(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class),
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl,
                      state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfGlazingFrac,
                      "N/A");
            }
        }
    }

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
        using namespace DataIPShortCuts;

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
        int ThisSurf;      // do loop counter
        Real64 AvgAzimuth; // temp for error checking
        Real64 AvgTilt;    // temp for error checking
        int SurfID;        // local surface "pointer"
        bool IsBlank;
        bool ErrorInName;

        cCurrentModuleObject = "SurfaceProperty:ExteriorNaturalVentedCavity";
        inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers);

        if (MaxNumNumbers != 8) {
            ShowSevereError(
                state, format("{}: Object Definition indicates not = 8 Number Objects, Number Indicated={}", cCurrentModuleObject, MaxNumNumbers));
            ErrorsFound = true;
        }

        TotExtVentCav = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        ExtVentedCavity.allocate(TotExtVentCav);

        for (Item = 1; Item <= TotExtVentCav; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            // first handle cAlphaArgs
            ErrorInName = false;
            IsBlank = false;

            UtilityRoutines::VerifyName(state, cAlphaArgs(1), ExtVentedCavity, Item - 1, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ShowContinueError(state, "...cannot not duplicate other names");
                ErrorsFound = true;
                continue;
            }
            ExtVentedCavity(Item).Name = cAlphaArgs(1);

            ExtVentedCavity(Item).OSCMName = cAlphaArgs(2);
            if (!lAlphaFieldBlanks(2)) {
                Found = UtilityRoutines::FindItemInList(ExtVentedCavity(Item).OSCMName, OSCM, TotOSCM);
                if (Found == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
            } else {
                Found = 0;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(2) +
                                " cannot be blank.");
                ErrorsFound = true;
            }
            ExtVentedCavity(Item).OSCMPtr = Found;

            Roughness = cAlphaArgs(3);
            // Select the correct Number for the associated ascii name for the roughness type
            if (UtilityRoutines::SameString(Roughness, "VeryRough")) ExtVentedCavity(Item).BaffleRoughness = VeryRough;
            if (UtilityRoutines::SameString(Roughness, "Rough")) ExtVentedCavity(Item).BaffleRoughness = Rough;
            if (UtilityRoutines::SameString(Roughness, "MediumRough")) ExtVentedCavity(Item).BaffleRoughness = MediumRough;
            if (UtilityRoutines::SameString(Roughness, "MediumSmooth")) ExtVentedCavity(Item).BaffleRoughness = MediumSmooth;
            if (UtilityRoutines::SameString(Roughness, "Smooth")) ExtVentedCavity(Item).BaffleRoughness = Smooth;
            if (UtilityRoutines::SameString(Roughness, "VerySmooth")) ExtVentedCavity(Item).BaffleRoughness = VerySmooth;

            // Was it set?
            if (ExtVentedCavity(Item).BaffleRoughness == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3));
                ErrorsFound = true;
            }

            AlphaOffset = 3;
            ExtVentedCavity(Item).NumSurfs = NumAlphas - AlphaOffset;
            if (ExtVentedCavity(Item).NumSurfs == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name +
                                "\", no underlying surfaces specified. Must have at least one.");
                ErrorsFound = true;
                continue;
            }
            ExtVentedCavity(Item).SurfPtrs.allocate(ExtVentedCavity(Item).NumSurfs);
            ExtVentedCavity(Item).SurfPtrs = 0;
            for (ThisSurf = 1; ThisSurf <= ExtVentedCavity(Item).NumSurfs; ++ThisSurf) {
                Found = UtilityRoutines::FindItemInList(cAlphaArgs(ThisSurf + AlphaOffset), Surface, TotSurfaces);
                if (Found == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ErrorsFound = true;
                    continue;
                }
                // check that surface is appropriate, Heat transfer, Sun, Wind,
                if (!Surface(Found).HeatTransSurf) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError(state, "...because it is not a Heat Transfer Surface.");
                    ErrorsFound = true;
                    continue;
                }
                if (!Surface(Found).ExtSolar) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError(state, "...because it is not exposed to Sun.");
                    ErrorsFound = true;
                    continue;
                }
                if (!Surface(Found).ExtWind) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError(state, "...because it is not exposed to Wind.");
                    ErrorsFound = true;
                    continue;
                }
                if (Surface(Found).ExtBoundCond != OtherSideCondModeledExt) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", is invalid");
                    ShowContinueError(state, "...because " + cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset) + "\".");
                    ShowContinueError(state, "...is not an OtherSideConditionedModel surface.");
                    ErrorsFound = true;
                    continue;
                }
                ExtVentedCavity(Item).SurfPtrs(ThisSurf) = Found;

                // now set info in Surface structure
                Surface(Found).ExtCavNum = Item;
                Surface(Found).ExtCavityPresent = true;
            }

            if (ErrorsFound) continue; // previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

            // now that we should have all the surfaces, do some preperations and checks.

            // are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
            Real64 const surfaceArea(sum_sub(Surface, &SurfaceData::Area, ExtVentedCavity(Item).SurfPtrs));
            //			AvgAzimuth = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Azimuth * Surface( ExtVentedCavity( Item ).SurfPtrs
            //).Area
            //)
            ///  sum(  Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            AvgAzimuth = sum_product_sub(Surface, &SurfaceData::Azimuth, &SurfaceData::Area, ExtVentedCavity(Item).SurfPtrs) /
                         surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage
            //			AvgTilt = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Tilt * Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ) /
            // sum(  Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            AvgTilt = sum_product_sub(Surface, &SurfaceData::Tilt, &SurfaceData::Area, ExtVentedCavity(Item).SurfPtrs) /
                      surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage
            for (ThisSurf = 1; ThisSurf <= ExtVentedCavity(Item).NumSurfs; ++ThisSurf) {
                SurfID = ExtVentedCavity(Item).SurfPtrs(ThisSurf);
                if (std::abs(Surface(SurfID).Azimuth - AvgAzimuth) > 15.0) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + ", Surface " + Surface(SurfID).Name +
                                     " has Azimuth different from others in the associated group.");
                }
                if (std::abs(Surface(SurfID).Tilt - AvgTilt) > 10.0) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + ", Surface " + Surface(SurfID).Name +
                                     " has Tilt different from others in the associated group.");
                }

                // test that there are no windows.  Now allow windows
                // If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
                //      Call ShowWarningError(state, 'Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
                //         //'subtracted in the group of surfaces associated with '//TRIM(ExtVentedCavity(Item)%Name))
                // endif
            }
            ExtVentedCavity(Item).Tilt = AvgTilt;
            ExtVentedCavity(Item).Azimuth = AvgAzimuth;

            // find area weighted centroid.
            //			ExtVentedCavity( Item ).Centroid.z = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Centroid.z * Surface(
            // ExtVentedCavity(  Item
            //).SurfPtrs ).Area ) / sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
            ExtVentedCavity(Item).Centroid.z =
                sum_product_sub(Surface, &SurfaceData::Centroid, &Vector::z, Surface, &SurfaceData::Area, ExtVentedCavity(Item).SurfPtrs) /
                surfaceArea; // Autodesk:F2C++ Functions handle array subscript usage

            // now handle rNumericArgs from input object
            ExtVentedCavity(Item).Porosity = rNumericArgs(1);
            ExtVentedCavity(Item).LWEmitt = rNumericArgs(2);
            ExtVentedCavity(Item).SolAbsorp = rNumericArgs(3);
            ExtVentedCavity(Item).HdeltaNPL = rNumericArgs(4);
            ExtVentedCavity(Item).PlenGapThick = rNumericArgs(5);
            if (ExtVentedCavity(Item).PlenGapThick <= 0.0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid .");
                ErrorsFound = true;
                ShowContinueError(state,
                                  format("...because field \"{}\" must be greater than Zero=[{:.2T}].", cNumericFieldNames(5), rNumericArgs(5)));
                continue;
            }
            ExtVentedCavity(Item).AreaRatio = rNumericArgs(6);
            ExtVentedCavity(Item).Cv = rNumericArgs(7);
            ExtVentedCavity(Item).Cd = rNumericArgs(8);

            // Fill out data we now know
            // sum areas of HT surface areas
            //			ExtVentedCavity( Item ).ProjArea = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array
            // subscript  usage: Replaced by below
            ExtVentedCavity(Item).ProjArea = surfaceArea;
            if (ExtVentedCavity(Item).ProjArea <= 0.0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid .");
                ErrorsFound = true;
                ShowContinueError(
                    state,
                    format("...because gross area of underlying surfaces must be greater than Zero=[{:.2T}].", ExtVentedCavity(Item).ProjArea));
                continue;
            }
            ExtVentedCavity(Item).ActualArea = ExtVentedCavity(Item).ProjArea * ExtVentedCavity(Item).AreaRatio;

            SetupOutputVariable(state, "Surface Exterior Cavity Baffle Surface Temperature",
                                OutputProcessor::Unit::C,
                                ExtVentedCavity(Item).Tbaffle,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
            SetupOutputVariable(state, "Surface Exterior Cavity Air Drybulb Temperature",
                                OutputProcessor::Unit::C,
                                ExtVentedCavity(Item).TAirCav,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
            SetupOutputVariable(state, "Surface Exterior Cavity Total Natural Ventilation Air Change Rate",
                                OutputProcessor::Unit::ach,
                                ExtVentedCavity(Item).PassiveACH,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
            SetupOutputVariable(state, "Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                ExtVentedCavity(Item).PassiveMdotVent,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
            SetupOutputVariable(state, "Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                ExtVentedCavity(Item).PassiveMdotWind,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
            SetupOutputVariable(state, "Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                ExtVentedCavity(Item).PassiveMdotTherm,
                                "System",
                                "Average",
                                ExtVentedCavity(Item).Name);
        }
    }

    void ExposedFoundationPerimeter::getData(EnergyPlusData &state, bool &ErrorsFound)
    {
        using namespace DataIPShortCuts;
        using DataSurfaces::Surface;

        int IOStatus; // Used in GetObjectItem
        int NumAlphas;
        int NumNumbers;

        auto const tolerance = 1e-6;

        std::string cCurrentModuleObject = "SurfaceProperty:ExposedFoundationPerimeter";
        int numObjects = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (int obj = 1; obj <= numObjects; ++obj) {
            int alpF = 1;
            int numF = 1;
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          obj,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            int Found = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), Surface, TotSurfaces);
            if (Found == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface");
                ErrorsFound = true;
            }
            alpF++;
            if (Surface(Found).Class != SurfaceClass::Floor) {
                ShowWarningError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", surface is not a floor surface");
                ShowContinueError(state, cCurrentModuleObject + " will not be used");
                continue;
            }

            // Choose calculation method
            std::string calculationMethod = cAlphaArgs(alpF);
            if (calculationMethod != "TOTALEXPOSEDPERIMETER" && calculationMethod != "EXPOSEDPERIMETERFRACTION" && calculationMethod != "BYSEGMENT") {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + calculationMethod + " is not a valid choice for " +
                                cAlphaFieldNames(alpF));
                ErrorsFound = true;
            }
            alpF++;

            Data data;
            data.useDetailedExposedPerimeter = true;

            if (!lNumericFieldBlanks(numF)) {
                if (calculationMethod == "TOTALEXPOSEDPERIMETER") {
                    data.exposedFraction = rNumericArgs(numF) / Surface(Found).Perimeter;
                    if (data.exposedFraction > 1 + tolerance) {
                        ShowWarningError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + cNumericFieldNames(numF) +
                                         " is greater than the perimeter of " + Surface(Found).Name);
                        ShowContinueError(state,
                                          format("{} perimeter = {}, {} exposed perimeter = {}",
                                                 Surface(Found).Name,
                                                 Surface(Found).Perimeter,
                                                 cCurrentModuleObject,
                                                 rNumericArgs(numF)));
                        ShowContinueError(state, cNumericFieldNames(numF) + " will be set equal to " + Surface(Found).Name + " perimeter");
                        data.exposedFraction = 1.0;
                    }

                    data.useDetailedExposedPerimeter = false;
                } else {
                    ShowWarningError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                     " set as calculation method, but a value has been set for " + cNumericFieldNames(numF) +
                                     ". This value will be ignored.");
                }
            } else {
                if (calculationMethod == "TOTALEXPOSEDPERIMETER") {
                    ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                    " set as calculation method, but no value has been set for " + cNumericFieldNames(numF));
                    ErrorsFound = true;
                }
            }
            numF++;

            if (!lNumericFieldBlanks(numF)) {
                if (calculationMethod == "EXPOSEDPERIMETERFRACTION") {
                    data.exposedFraction = rNumericArgs(numF);
                    data.useDetailedExposedPerimeter = false;
                } else {
                    ShowWarningError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                     " set as calculation method, but a value has been set for " + cNumericFieldNames(numF) +
                                     ". This value will be ignored.");
                }
            } else {
                if (calculationMethod == "EXPOSEDPERIMETERFRACTION") {
                    ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                    " set as calculation method, but no value has been set for " + cNumericFieldNames(numF));
                    ErrorsFound = true;
                }
            }
            numF++;

            int numRemainingFields = NumAlphas - (alpF - 1) + NumNumbers - (numF - 1);
            if (numRemainingFields > 0) {
                if (calculationMethod == "BYSEGMENT") {
                    if (numRemainingFields != (int)Surface(Found).Vertex.size()) {
                        ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name +
                                        ", must have equal number of segments as the floor has vertices." + cAlphaFieldNames(alpF) + "\" and \"" +
                                        cNumericFieldNames(numF - 1) + "\"");
                        ShowContinueError(state,
                                          format("{} number of vertices = {}, {} number of segments = {}",
                                                 Surface(Found).Name,
                                                 Surface(Found).Vertex.size(),
                                                 cCurrentModuleObject,
                                                 numRemainingFields));
                        ErrorsFound = true;
                    }
                    for (int segNum = 0; segNum < numRemainingFields; segNum++) {
                        if (UtilityRoutines::SameString(cAlphaArgs(alpF), "YES")) {
                            data.isExposedPerimeter.push_back(true);
                        } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "NO")) {
                            data.isExposedPerimeter.push_back(false);
                        } else if (lAlphaFieldBlanks(alpF)) {
                            ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                            " set as calculation method, but no value has been set for " + cAlphaFieldNames(alpF) +
                                            ". Must be \"Yes\" or \"No\".");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + cAlphaFieldNames(alpF) + " invalid [" +
                                            cAlphaArgs(alpF) + "]. Must be \"Yes\" or \"No\".");
                            ErrorsFound = true;
                        }
                        alpF++;
                    }
                }
            } else {
                if (calculationMethod == "BYSEGMENT") {
                    ShowSevereError(state, cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                    " set as calculation method, but no values have been set for Surface Segments Exposed");
                    ErrorsFound = true;
                }
            }
            surfaceMap[Found] = data;
        }
    }

    void GetSurfaceLocalEnvData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         X LUO
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for Outdoor Air Node for exterior surfaces

        // METHODOLOGY EMPLOYED:
        // usual E+ input processes

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace DataErrorTracking;

        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;

        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeType_Air;
        using DataLoopNode::ObjectIsParent;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfLocalEnvironment;
        using DataSurfaces::TotSurfaces;
        using DataSurfaces::TotSurfLocalEnv;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSurfaceLocalEnvData: ");

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumeric;
        int Loop;
        int SurfLoop;
        int IOStat;
        int SurfNum;
        int NodeNum;
        int ExtShadingSchedNum;
        int SurroundingSurfsNum;

        //-----------------------------------------------------------------------
        //                SurfaceProperty:LocalEnvironment
        //-----------------------------------------------------------------------

        cCurrentModuleObject = "SurfaceProperty:LocalEnvironment";
        TotSurfLocalEnv = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotSurfLocalEnv > 0) {

            state.dataGlobal->AnyLocalEnvironmentsInModel = true;

            if (!allocated(SurfLocalEnvironment)) {
                SurfLocalEnvironment.allocate(TotSurfLocalEnv);
            }

            for (Loop = 1; Loop <= TotSurfLocalEnv; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlpha,
                                              rNumericArgs,
                                              NumNumeric,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                SurfLocalEnvironment(Loop).Name = cAlphaArgs(1);

                // Assign surface number
                SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Surface);
                if (SurfNum == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                    cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(state, cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                      "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    SurfLocalEnvironment(Loop).SurfPtr = SurfNum;
                }

                // Assign External Shading Schedule number
                if (!lAlphaFieldBlanks(3)) {
                    ExtShadingSchedNum = GetScheduleIndex(state, cAlphaArgs(3));
                    if (ExtShadingSchedNum == 0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                        ShowContinueError(state, cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
                                          "\" no corresponding schedule has been found in the input file.");
                        ErrorsFound = true;
                    } else {
                        SurfLocalEnvironment(Loop).ExtShadingSchedPtr = ExtShadingSchedNum;
                    }
                }

                // Assign surrounding surfaces object number;
                if (!lAlphaFieldBlanks(4)) {
                    SurroundingSurfsNum = UtilityRoutines::FindItemInList(cAlphaArgs(4), SurroundingSurfsProperty);
                    if (SurroundingSurfsNum == 0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(4) + " has been found.");
                        ShowContinueError(state, cAlphaFieldNames(4) + " entered value = \"" + cAlphaArgs(4) +
                                          "\" no corresponding surrounding surfaces properties has been found in the input file.");
                        ErrorsFound = true;
                    } else {
                        SurfLocalEnvironment(Loop).SurroundingSurfsPtr = SurroundingSurfsNum;
                    }
                }

                // Assign outdoor air node number;
                if (!lAlphaFieldBlanks(5)) {
                    NodeNum = GetOnlySingleNode(state,
                        cAlphaArgs(5), ErrorsFound, cCurrentModuleObject, cAlphaArgs(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);
                    if (NodeNum == 0 && CheckOutAirNodeNumber(state, NodeNum)) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(5) + " has been found.");
                        ShowContinueError(state, cAlphaFieldNames(5) + " entered value = \"" + cAlphaArgs(5) +
                                          "\" no corresponding outdoor air node has been found in the input file.");
                        ErrorsFound = true;
                    } else {
                        SurfLocalEnvironment(Loop).OutdoorAirNodePtr = NodeNum;
                    }
                }
            }
        }
        // Link surface properties to surface object
        for (SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop) {
            for (Loop = 1; Loop <= TotSurfLocalEnv; ++Loop) {
                if (SurfLocalEnvironment(Loop).SurfPtr == SurfLoop) {
                    if (SurfLocalEnvironment(Loop).OutdoorAirNodePtr != 0) {
                        Surface(SurfLoop).HasLinkedOutAirNode = true;
                        Surface(SurfLoop).LinkedOutAirNode = SurfLocalEnvironment(Loop).OutdoorAirNodePtr;
                    }
                    if (SurfLocalEnvironment(Loop).ExtShadingSchedPtr != 0) {
                        Surface(SurfLoop).SchedExternalShadingFrac = true;
                        Surface(SurfLoop).ExternalShadingSchInd = SurfLocalEnvironment(Loop).ExtShadingSchedPtr;
                    }
                    if (SurfLocalEnvironment(Loop).SurroundingSurfsPtr != 0) {
                        Surface(SurfLoop).HasSurroundingSurfProperties = true;
                        Surface(SurfLoop).SurroundingSurfacesNum = SurfLocalEnvironment(Loop).SurroundingSurfsPtr;
                    }
                }
            }
        }
    }

    void GetSurfaceSrdSurfsData(EnergyPlusData &state, bool &ErrorsFound) // Error flag indicator (true if errors found)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         X LUO
        //       DATE WRITTEN   July 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // load input data for surrounding surfaces properties for exterior surfaces

        // METHODOLOGY EMPLOYED:
        // usual E+ input processes

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using namespace DataErrorTracking;

        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using ScheduleManager::GetScheduleIndex;

        using DataLoopNode::NodeConnectionType_Inlet;
        using DataLoopNode::NodeType_Air;
        using DataLoopNode::ObjectIsParent;
        using DataSurfaces::Surface;
        using DataSurfaces::SurfLocalEnvironment;
        using DataSurfaces::TotSurfaces;
        using DataSurfaces::TotSurfLocalEnv;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSurfaceSrdSurfsData: ");

        // INTERFACE BLOCK SPECIFICATIONS:na
        // DERIVED TYPE DEFINITIONS:na
        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlpha;
        int NumNumeric;
        int Loop;
        int IOStat;

        int TotSrdSurfProperties;
        int TotSrdSurf;
        int SurfLoop;
        int SurfNameArg;
        int SurfVFArg;
        int SurfTempArg;

        //-----------------------------------------------------------------------
        //                SurfaceProperty:SurroundingSurfaces
        //-----------------------------------------------------------------------

        cCurrentModuleObject = "SurfaceProperty:SurroundingSurfaces";
        TotSrdSurfProperties = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotSrdSurfProperties > 0) {

            if (!allocated(SurroundingSurfsProperty)) {
                SurroundingSurfsProperty.allocate(TotSrdSurfProperties);
            }

            for (Loop = 1; Loop <= TotSrdSurfProperties; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlpha,
                                              rNumericArgs,
                                              NumNumeric,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                // A1: Name
                SurroundingSurfsProperty(Loop).Name = cAlphaArgs(1);

                // N1: sky view factor
                if (!lNumericFieldBlanks(1)) {
                    SurroundingSurfsProperty(Loop).SkyViewFactor = rNumericArgs(1);
                }

                // A2: sky temp sch name
                if (!lAlphaFieldBlanks(2)) {
                    SurroundingSurfsProperty(Loop).SkyTempSchNum = GetScheduleIndex(state, cAlphaArgs(2));
                }

                // N2: ground view factor
                if (!lNumericFieldBlanks(2)) {
                    SurroundingSurfsProperty(Loop).GroundViewFactor = rNumericArgs(2);
                }

                // A3: ground temp sch name
                if (!lAlphaFieldBlanks(3)) {
                    SurroundingSurfsProperty(Loop).GroundTempSchNum = GetScheduleIndex(state, cAlphaArgs(3));
                }

                // The object requires at least one srd surface input, each surface requires a set of 3 fields (2 Alpha fields Name and Temp Sch Name
                // and 1 Num fields View Factor)
                if (NumAlpha < 5) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                    ShowContinueError(state, "At lease one set of surrounding surface properties should be defined.");
                    ErrorsFound = true;
                    continue;
                }
                if ((NumAlpha - 3) / 2 != (NumNumeric - 2)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                    ShowContinueError(state, "Check number of input fields for each surrounding surface.");
                    ErrorsFound = true;
                    continue;
                }
                // Read surrounding surfaces properties
                TotSrdSurf = NumNumeric - 2;
                SurroundingSurfsProperty(Loop).TotSurroundingSurface = TotSrdSurf;
                SurroundingSurfsProperty(Loop).SurroundingSurfs.allocate(TotSrdSurf);
                for (SurfLoop = 1; SurfLoop <= TotSrdSurf; ++SurfLoop) {
                    SurfNameArg = SurfLoop * 2 + 2; // A4, A6, A8, ...
                    SurfVFArg = SurfLoop + 2;       // N3, N4, N5, ...
                    SurfTempArg = SurfLoop * 2 + 3; // A5, A7, A9, ...
                    SurroundingSurfsProperty(Loop).SurroundingSurfs(SurfLoop).Name = cAlphaArgs(SurfNameArg);
                    SurroundingSurfsProperty(Loop).SurroundingSurfs(SurfLoop).ViewFactor = rNumericArgs(SurfVFArg);
                    SurroundingSurfsProperty(Loop).SurroundingSurfs(SurfLoop).TempSchNum = GetScheduleIndex(state, cAlphaArgs(SurfTempArg));
                }
            }
        }
    }

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
        using namespace DataIPShortCuts;
        using DataHeatBalance::HighHConvLimit;
        using DataHeatBalance::LowHConvLimit;
        using DataHeatBalSurface::MaxSurfaceTempLimit;
        using DataSurfaces::Surface;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int CountHTAlgoObjectsSingleSurf;
        int CountHTAlgoObjectsMultiSurf;
        int CountHTAlgoObjectsSurfList;
        int IOStatus; // Used in GetObjectItem
        static bool ErrorsFoundSingleSurf(false);
        static bool ErrorsFoundMultiSurf(false);
        static bool ErrorsFoundSurfList(false);
        static bool ErrorsFoundByConstruct(false);
        int tmpAlgoInput;
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

        cCurrentModuleObject = "SurfaceProperty:HeatBalanceSourceTerm";
        int CountAddHeatSourceSurf = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (Item = 1; Item <= CountAddHeatSourceSurf; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            Found = UtilityRoutines::FindItemInList(cAlphaArgs(1), Surface, TotSurfaces);

            if (Found == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
                ErrorsFound = true;
            } else if (Surface(Found).InsideHeatSourceTermSchedule || Surface(Found).OutsideHeatSourceTermSchedule) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                "\", multiple SurfaceProperty:HeatBalanceSourceTerm objects applied to the same surface.");
                ErrorsFound = true;
            }

            if (!lAlphaFieldBlanks(2)) {
                Surface(Found).InsideHeatSourceTermSchedule = EnergyPlus::ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                DataSurfaces::AnyHeatBalanceInsideSourceTerm = true;
                if (Surface(Found).InsideHeatSourceTermSchedule == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", cannot find the matching Schedule: " + cAlphaFieldNames(2) +
                                    "=\"" + cAlphaArgs(2));
                    ErrorsFound = true;
                }
            }

            if (!lAlphaFieldBlanks(3)) {
                Surface(Found).OutsideHeatSourceTermSchedule = EnergyPlus::ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                DataSurfaces::AnyHeatBalanceOutsideSourceTerm = true;
                if (Surface(Found).OutsideHeatSourceTermSchedule == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", cannot find the matching Schedule: " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3));
                    ErrorsFound = true;
                } else if (Surface(Found).OSCPtr > 0) {
                    ShowSevereError(state,
                        cCurrentModuleObject +
                        "=\"SurfaceProperty:HeatBalanceSourceTerm\", cannot be specified for OtherSideCoefficient Surface=" + cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            if (Surface(Found).OutsideHeatSourceTermSchedule == 0 && Surface(Found).InsideHeatSourceTermSchedule == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", no schedule defined for additional heat source.");
                ErrorsFound = true;
            }
        }

        // first initialize each heat transfer surface with the overall model type, array assignment
        for (auto &e : Surface)
            e.HeatTransferAlgorithm = OverallHeatTransferSolutionAlgo;

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
        CountHTAlgoObjectsSingleSurf = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
        for (Item = 1; Item <= CountHTAlgoObjectsSingleSurf; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ErrorsFoundSingleSurf = false;
            Found = UtilityRoutines::FindItemInList(cAlphaArgs(1), Surface, TotSurfaces);

            if (Found == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
                ErrorsFoundSingleSurf = true;
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;
                } else if (SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_EMPD;
                    DataHeatBalance::AnyEMPD = true;
                } else if (SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT") {
                    tmpAlgoInput = HeatTransferModel_HAMT;
                    DataHeatBalance::AnyHAMT = true;
                } else if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                    tmpAlgoInput = HeatTransferModel_CondFD;
                    DataHeatBalance::AnyCondFD = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundSingleSurf = true;
                }
            }

            if (!ErrorsFoundSingleSurf) {
                Surface(Found).HeatTransferAlgorithm = tmpAlgoInput;
            } else {
                ErrorsFound = true;
            }
        } // single surface heat transfer algorithm override

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
        CountHTAlgoObjectsMultiSurf = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        for (Item = 1; Item <= CountHTAlgoObjectsMultiSurf; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ErrorsFoundMultiSurf = false;
            {
                auto const SELECT_CASE_var(cAlphaArgs(3));

                if (SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;
                } else if (SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_EMPD;
                    DataHeatBalance::AnyEMPD = true;
                } else if (SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT") {
                    tmpAlgoInput = HeatTransferModel_HAMT;
                    DataHeatBalance::AnyHAMT = true;
                } else if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                    tmpAlgoInput = HeatTransferModel_CondFD;
                    DataHeatBalance::AnyCondFD = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
                    ErrorsFoundMultiSurf = true;
                }
            }

            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "ALLEXTERIORSURFACES") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLEXTERIORWALLS") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces

                        if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLEXTERIORROOFS") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                        if (Surface(SurfNum).Class != SurfaceClass::Roof) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLEXTERIORFLOORS") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                        if (Surface(SurfNum).Class != SurfaceClass::Floor) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLGROUNDCONTACTSURFACES") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond != Ground) continue; // ground BC
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }
                } else if (SELECT_CASE_var == "ALLINTERIORSURFACES") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLINTERIORWALLS") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                        if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if ((SELECT_CASE_var == "ALLINTERIORROOFS") || (SELECT_CASE_var == "ALLINTERIORCEILINGS")) {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                        if (Surface(SurfNum).Class != SurfaceClass::Roof) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }

                } else if (SELECT_CASE_var == "ALLINTERIORFLOORS") {
                    SurfacesOfType = false;
                    for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).HeatTransSurf) continue;
                        if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                        if (Surface(SurfNum).Class != SurfaceClass::Floor) continue;
                        if (state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                        SurfacesOfType = true;
                        Surface(SurfNum).HeatTransferAlgorithm = tmpAlgoInput;
                    }
                } else {
                    SurfacesOfType = false;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundMultiSurf = true;
                }
            }

            if (!SurfacesOfType) {
                ShowWarningError(state, "In " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", for Multiple Surface Assignment=\"" + cAlphaArgs(2) +
                                 "\", there were no surfaces of that type found for assignment.");
            }
            if (ErrorsFoundMultiSurf) ErrorsFound = true;

        } // multi surface heat transfer algo override

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
        CountHTAlgoObjectsSurfList = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ErrorsFoundSurfList = false;
            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;
                } else if (SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_EMPD;
                    DataHeatBalance::AnyEMPD = true;
                } else if (SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT") {
                    tmpAlgoInput = HeatTransferModel_HAMT;
                    DataHeatBalance::AnyHAMT = true;
                } else if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                    tmpAlgoInput = HeatTransferModel_CondFD;
                    DataHeatBalance::AnyCondFD = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundSurfList = true;
                }
            }

            for (Item1 = 3; Item1 <= NumAlphas; ++Item1) {

                Found = UtilityRoutines::FindItemInList(cAlphaArgs(Item1), Surface, TotSurfaces);

                if (Found == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
                    ShowContinueError(state, "Name of surface not found = \"" + cAlphaArgs(Item1) + "\"");
                    ErrorsFoundSurfList = true;
                }

                if (!ErrorsFoundSurfList) {
                    Surface(Found).HeatTransferAlgorithm = tmpAlgoInput;
                } else {
                    ErrorsFound = true;
                }
            }
        }

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
        CountHTAlgoObjectsSurfList = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Item,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ErrorsFoundByConstruct = false;
            {
                auto const SELECT_CASE_var(cAlphaArgs(2));

                if (SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_CTF;
                    DataHeatBalance::AnyCTF = true;
                } else if (SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION") {
                    tmpAlgoInput = HeatTransferModel_EMPD;
                    DataHeatBalance::AnyEMPD = true;
                } else if (SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT") {
                    tmpAlgoInput = HeatTransferModel_HAMT;
                    DataHeatBalance::AnyHAMT = true;
                } else if (SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE") {
                    tmpAlgoInput = HeatTransferModel_CondFD;
                    DataHeatBalance::AnyCondFD = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundByConstruct = true;
                }
            }

            Found = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct, TotConstructs);
            if (Found == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
                ErrorsFoundByConstruct = true;
            }

            if (!ErrorsFoundByConstruct) {
                for (Item1 = 1; Item1 <= TotSurfaces; ++Item1) {
                    if (Surface(Item1).Construction == Found) {
                        Surface(Item1).HeatTransferAlgorithm = tmpAlgoInput;
                    }
                }
            }
        }

        // Change algorithm for Kiva and air boundary foundation surfaces
        for (auto &surf : Surface) {
            if (surf.ExtBoundCond == KivaFoundation) {
                surf.HeatTransferAlgorithm = HeatTransferModel_Kiva;
                DataHeatBalance::AnyKiva = true;
            }
        }

        // Setup Kiva instances
        if (DataHeatBalance::AnyKiva) {
            if (!ErrorsFound) ErrorsFound = state.dataSurfaceGeometry->kivaManager.setupKivaInstances(state);
        }

        // test for missing materials for algorithms selected
        NumEMPDMat = inputProcessor->getNumObjectsFound(state, "MaterialProperty:MoisturePenetrationDepth:Settings");
        NumPCMat = inputProcessor->getNumObjectsFound(state, "MaterialProperty:PhaseChange") +
                   inputProcessor->getNumObjectsFound(state, "MaterialProperty:PhaseChangeHysteresis");
        NumVTCMat = inputProcessor->getNumObjectsFound(state, "MaterialProperty:VariableThermalConductivity");
        NumHAMTMat1 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Settings");
        NumHAMTMat2 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm");
        NumHAMTMat3 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Suction");
        NumHAMTMat4 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Redistribution");
        NumHAMTMat5 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:Diffusion");
        NumHAMTMat6 = inputProcessor->getNumObjectsFound(state, "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity");
        SumHAMTMat = NumHAMTMat1 + NumHAMTMat2 + NumHAMTMat3 + NumHAMTMat4 + NumHAMTMat5 + NumHAMTMat6;
        msgneeded = false;

        if (NumEMPDMat > 0 && !DataHeatBalance::AnyEMPD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:MoisturePenetrationDepth:Settings objects but the moisture "
                                    "penetration depth algorithm is not used anywhere.",
                                    NumEMPDMat));
            msgneeded = true;
        }
        if (NumPCMat > 0 && !DataHeatBalance::AnyCondFD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:PhaseChange objects but the conduction finite difference algorithm "
                                    "is not used anywhere.",
                                    NumPCMat));
            msgneeded = true;
        }
        if (NumVTCMat > 0 && !DataHeatBalance::AnyCondFD) {
            ShowWarningError(state,
                             format("The input file includes {} MaterialProperty:VariableThermalConductivity objects but the conduction finite "
                                    "difference algorithm is not used anywhere.",
                                    NumVTCMat));
            msgneeded = true;
        }
        if (SumHAMTMat > 0 && !DataHeatBalance::AnyHAMT) {
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
        if (NumEMPDMat == 0 && DataHeatBalance::AnyEMPD) {
            ShowWarningError(state, "The moisture penetration depth conduction transfer function algorithm is used but the input file includes no "
                             "MaterialProperty:MoisturePenetrationDepth:Settings objects.");
            msgneeded = true;
        }
        if (SumHAMTMat == 0 && DataHeatBalance::AnyHAMT) {
            ShowWarningError(state, "The combined heat and moisture finite element algorithm is used but the input file includes no "
                             "MaterialProperty:HeatAndMoistureTransfer:* objects.");
            msgneeded = true;
        }
        if (msgneeded) {
            ShowContinueError(state, "Certain materials objects are necessary to achieve proper results with the heat transfer algorithm(s) selected.");
        }

        // Write Solution Algorithm to the initialization output file for User Verification
        print(state.files.eio, "{}\n",
             "! <Surface Heat Transfer Algorithm>, Value {CTF - ConductionTransferFunction | EMPD - "
               "MoisturePenetrationDepthConductionTransferFunction | CondFD - ConductionFiniteDifference | HAMT - "
               "CombinedHeatAndMoistureFiniteElement} - Description,Inside Surface Max Temperature Limit{C}, Surface "
               "Convection Coefficient Lower Limit {W/m2-K}, Surface Convection Coefficient Upper Limit {W/m2-K}");

        int numberOfHeatTransferAlgosUsed = 0;
        // Formats
        static constexpr auto Format_725("Surface Heat Transfer Algorithm, {},{:.0R},{:.2R},{:.1R}\n");

        if (DataHeatBalance::AnyCTF) {
            const auto AlgoName = "CTF - ConductionTransferFunction";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio, Format_725, AlgoName, MaxSurfaceTempLimit, LowHConvLimit, HighHConvLimit);
        }
        if (DataHeatBalance::AnyEMPD) {
            DataHeatBalance::AllCTF = false;
            const auto AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio, Format_725, AlgoName, MaxSurfaceTempLimit, LowHConvLimit, HighHConvLimit);
        }
        if (DataHeatBalance::AnyCondFD) {
            DataHeatBalance::AllCTF = false;
            const auto AlgoName = "CondFD - ConductionFiniteDifference";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio, Format_725, AlgoName, MaxSurfaceTempLimit, LowHConvLimit, HighHConvLimit);
        }
        if (DataHeatBalance::AnyHAMT) {
            DataHeatBalance::AllCTF = false;
            const auto AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio, Format_725, AlgoName, MaxSurfaceTempLimit, LowHConvLimit, HighHConvLimit);
        }
        if (DataHeatBalance::AnyKiva) {
            DataHeatBalance::AllCTF = false;
            const auto AlgoName = "KivaFoundation - TwoDimensionalFiniteDifference";
            ++numberOfHeatTransferAlgosUsed;
            print(state.files.eio, Format_725, AlgoName, MaxSurfaceTempLimit, LowHConvLimit, HighHConvLimit);
        }

        // Check HeatTransferAlgorithm for interior surfaces
        if (numberOfHeatTransferAlgosUsed > 1) {
            int ExtSurfNum;
            for (Item = 1; Item <= TotSurfaces; ++Item) {
                if (Surface(Item).ExtBoundCond > 0) {
                    if (Surface(Item).HeatTransferAlgorithm <= 0) continue;
                    ExtSurfNum = Surface(Item).ExtBoundCond;
                    if (Surface(Item).HeatTransferAlgorithm != Surface(ExtSurfNum).HeatTransferAlgorithm) {
                        ShowWarningError(state, "An interior surface is defined as two surfaces with reverse constructions. The HeatTransferAlgorithm in "
                                         "both constructions should be same.");
                        ShowContinueError(state, "The HeatTransferAlgorithm of Surface: " + Surface(Item).Name + ", is " +
                                          HeatTransferModelNames(Surface(Item).HeatTransferAlgorithm));
                        ShowContinueError(state, "The HeatTransferAlgorithm of Surface: " + Surface(ExtSurfNum).Name + ", is " +
                                          HeatTransferModelNames(Surface(ExtSurfNum).HeatTransferAlgorithm));
                        if (Surface(Item).HeatTransferAlgorithm > Surface(ExtSurfNum).HeatTransferAlgorithm) {
                            ShowContinueError(state, "The HeatTransferAlgorithm of Surface: " + Surface(ExtSurfNum).Name + ", is assigned to " +
                                              HeatTransferModelNames(Surface(Item).HeatTransferAlgorithm) + ". Simulation continues.");
                            Surface(ExtSurfNum).HeatTransferAlgorithm = Surface(Item).HeatTransferAlgorithm;
                        } else {
                            ShowContinueError(state, "The HeatTransferAlgorithm of Surface: " + Surface(Item).Name + ", is assigned to " +
                                              HeatTransferModelNames(Surface(ExtSurfNum).HeatTransferAlgorithm) + ". Simulation continues.");
                            Surface(Item).HeatTransferAlgorithm = Surface(ExtSurfNum).HeatTransferAlgorithm;
                        }
                    }
                }
            }
        }

        // Assign model type to windows, shading surfaces, and TDDs
        for (Item = 1; Item <= TotSurfaces; ++Item) {
            if (Surface(Item).Class == SurfaceClass::Window || Surface(Item).Class == SurfaceClass::GlassDoor) {
                // todo, add complex fenestration switch  HeatTransferModel_ComplexFenestration
                if (SurfWinWindowModelType(Item) == WindowBSDFModel) {
                    Surface(Item).HeatTransferAlgorithm = HeatTransferModel_ComplexFenestration;
                } else {
                    Surface(Item).HeatTransferAlgorithm = HeatTransferModel_Window5;
                }
            }
            if (Surface(Item).Class == SurfaceClass::Detached_B || Surface(Item).Class == SurfaceClass::Detached_F ||
                Surface(Item).Class == SurfaceClass::Shading || Surface(Item).Class == SurfaceClass::Overhang ||
                Surface(Item).Class == SurfaceClass::Fin) {
                Surface(Item).HeatTransferAlgorithm = HeatTransferModel_None;
            }
            if (Surface(Item).Class == SurfaceClass::TDD_Diffuser || Surface(Item).Class == SurfaceClass::TDD_Dome) {
                Surface(Item).HeatTransferAlgorithm = HeatTransferModel_TDD;
            }

            if (Surface(Item).HeatTransferAlgorithm == HeatTransferModel_CTF || Surface(Item).HeatTransferAlgorithm == HeatTransferModel_EMPD) {
                state.dataConstruction->Construct(Surface(Item).Construction).IsUsedCTF = true;
            }
        }
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
        static std::string const RoutineName("GetVertices: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Ptr;  // Pointer into Vertice array
        int n;    // Loop counter
        int NSrc; // Used for CW -> CCW transformation
        int NTar; // Used for CW -> CCW transformation
        Real64 SurfWorldAz;
        Real64 SurfTilt;
        Real64 Perimeter; // Perimeter length of the surface
        int Vrt;          // Used for calculating perimeter
        Real64 Xb;        // Intermediate calculation
        Real64 Yb;        // Intermediate calculation
        int ZoneNum;
        int ThisCorner;
        std::string TiltString;
        Real64 ThisWidth;
        Real64 ThisHeight;
        Real64 DistanceCheck;
        // unused    REAL(r64) :: ccwtest
        // unused    LOGICAL   :: SurfaceCCW
        Real64 dotp;

        // Object Data
        Vector const TestVector(0.0, 0.0, 1.0);
        Vector temp;

        if (NSides > MaxVerticesPerSurface) MaxVerticesPerSurface = NSides;
        Ptr = 1;
        for (n = 1; n <= NSides; ++n) {
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Vertices(Ptr);
            ++Ptr;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Vertices(Ptr);
            ++Ptr;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).z = Vertices(Ptr);
            ++Ptr;
        }

        // Address changing vertices if they were put in in CW order rather than CCW
        if (!CCW) {
            // If even number of sides, this will transfer appropriately
            // If odd number, will leave the "odd" one, which is what you want.
            NSrc = NSides;
            NTar = 2;
            for (n = 1; n <= (NSides - 1) / 2; ++n) {
                temp = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NSrc);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NSrc) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NTar);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NTar) = temp;
                --NSrc;
                ++NTar;
            }
        }
        // Now address which "Corner" has been put in first.  Note: the azimuth and tilt and area
        // calculations do not care which corner is put in first.
        // 2/2011 - don't think the shading calculations have a corner preference.  Will keep this for
        // consistency (for now)
        ThisCorner = Corner;
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
                temp = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NTar);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NTar) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NSrc);
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NSrc) = temp;
                ++NTar;
                ++NSrc;
                if (NTar > NSides) NTar = 1;
                if (NSrc > NSides) NSrc = 1;
            }
            ++ThisCorner;
            if (ThisCorner > NSides) ThisCorner = 1;
        } // Corners
        if (!WorldCoordSystem) {
            // Input in "relative" coordinates, use Building and Zone North Axes and Origins
            //                                  to translate each point (including rotation for Appendix G)
            ZoneNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x * state.dataSurfaceGeometry->CosZoneRelNorth(ZoneNum) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y * state.dataSurfaceGeometry->SinZoneRelNorth(ZoneNum) +
                         Zone(ZoneNum).OriginX;
                    Yb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x * state.dataSurfaceGeometry->SinZoneRelNorth(ZoneNum) + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y * state.dataSurfaceGeometry->CosZoneRelNorth(ZoneNum) +
                         Zone(ZoneNum).OriginY;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).z += Zone(ZoneNum).OriginZ;
                }
            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Xb * state.dataSurfaceGeometry->CosBldgRelNorth - Yb * state.dataSurfaceGeometry->SinBldgRelNorth;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Xb * state.dataSurfaceGeometry->SinBldgRelNorth + Yb * state.dataSurfaceGeometry->CosBldgRelNorth;
                }
            }
        } else {
            // if world coordinate only need to rotate for Appendix G
            ZoneNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Xb * state.dataSurfaceGeometry->CosBldgRotAppGonly - Yb * state.dataSurfaceGeometry->SinBldgRotAppGonly;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Xb * state.dataSurfaceGeometry->SinBldgRotAppGonly + Yb * state.dataSurfaceGeometry->CosBldgRotAppGonly;
                }
            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Xb * state.dataSurfaceGeometry->CosBldgRotAppGonly - Yb * state.dataSurfaceGeometry->SinBldgRotAppGonly;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Xb * state.dataSurfaceGeometry->SinBldgRotAppGonly + Yb * state.dataSurfaceGeometry->CosBldgRotAppGonly;
                }
            }
        }

        if (NSides > 2) {
            DistanceCheck = distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1));
            if (DistanceCheck < 0.01) {
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state,
                                     RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" +
                                         state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                         ", in Zone=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName);
                    ShowContinueError(
                        state,
                        format("Vertex [{}", state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) +
                            format("]=({:.2R},{:.2R},{:.2R})",
                                   state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides).x,
                                   state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides).y,
                                   state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides).z));
                    ShowContinueError(state,
                                      format("Vertex [{}", 1) +
                                          format("]=({:.2R},{:.2R},{:.2R}",
                                                 state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).x,
                                                 state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).y,
                                                 state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).z) +
                                          ')');
                }
                ++state.dataErrTracking->TotalCoincidentVertices;
                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides > 3) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(state, format("Dropping Vertex [{}].", state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    }
                    --state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.redimension(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                } else {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowContinueError(
                            state,
                            format("Cannot Drop Vertex [{}]; Number of Surface Sides at minimum. This surface is now a degenerate surface.",
                                   state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                    }
                    ++state.dataErrTracking->TotalDegenerateSurfaces;
                    // mark degenerate surface?
                }
                DistanceCheck = 0.0;
            }
            Perimeter = DistanceCheck;
            //      DO Vrt=2,SurfaceTmp(SurfNum)%Sides
            Vrt = 2;
            while (true) {
                DistanceCheck = distance(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1));
                if (DistanceCheck < 0.01) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state,
                                         RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" +
                                             state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                             ", in Zone=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName);
                        ShowContinueError(state,
                                          format("Vertex [{}", Vrt) + format("]=({:.2R},{:.2R},{:.2R})",
                                                                             state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).x,
                                                                             state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).y,
                                                                             state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt).z));
                        ShowContinueError(state,
                                          format("Vertex [{}]=({:.2R},", Vrt - 1, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1).x) +
                                              format("{:.2R},{:.2R})",
                                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1).y,
                                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(Vrt - 1).z));
                    }
                    ++state.dataErrTracking->TotalCoincidentVertices;
                    if (Vrt == state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) {
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides > 3) {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowContinueError(state, format("Dropping Vertex [{}].", state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                            }
                            --state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.redimension(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                        } else {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowContinueError(
                                    state,
                                    format("Cannot Drop Vertex [{}]; Number of Surface Sides at minimum. This surface is now a degenerate surface.",
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                            }
                            ++state.dataErrTracking->TotalDegenerateSurfaces;
                            // mark degenerate surface?
                        }
                        DistanceCheck = 0.0;
                    } else {
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides > 3) {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowContinueError(state, format("Dropping Vertex [{}].", Vrt));
                            }
                            for (n = Vrt; n <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides - 1; ++n) {
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n + 1).x;
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n + 1).y;
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).z = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n + 1).z;
                            }
                            --state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
                            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.redimension(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);
                        } else {
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                ShowContinueError(
                                    state,
                                    format("Cannot Drop Vertex [{}]; Number of Surface Sides at minimum. This surface is now a degenerate surface.",
                                           state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
                            }
                            ++state.dataErrTracking->TotalDegenerateSurfaces;
                            // mark degenerate surface?
                        }
                        DistanceCheck = 0.0;
                    }
                }
                Perimeter += DistanceCheck;
                ++Vrt;
                if (Vrt > state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides) break;
            }

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter = Perimeter;

            CreateNewellSurfaceNormalVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            CreateNewellAreaVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
            // For surfaces with subsurfaces, the following two areas are turned into net areas later by
            // subtracting subsurface areas
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = VecLength(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
            DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                    SurfWorldAz,
                                    SurfTilt,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            dotp = dot(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector, TestVector);
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && dotp < -0.000001) {
                TiltString = format("{:.1R}", SurfTilt);
                ShowWarningError(state,
                                 RoutineName + "Roof/Ceiling is upside down! Tilt angle=[" + TiltString + "], should be near 0, Surface=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", in Zone=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName + "\".");
                ShowContinueError(state, "Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, SurfWorldAz, SurfTilt);
            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && SurfTilt > 80.0) {
                TiltString = format("{:.1R}", SurfTilt);
                ShowWarningError(state,
                                 RoutineName + "Roof/Ceiling is not oriented correctly! Tilt angle=[" + TiltString +
                                     "], should be near 0, Surface=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", in Zone=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName + "\".");
            }
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && dotp > 0.000001) {
                TiltString = format("{:.1R}", SurfTilt);
                ShowWarningError(state,
                                 RoutineName + "Floor is upside down! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", in Zone=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName + "\".");
                ShowContinueError(state, "Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, SurfWorldAz, SurfTilt);
            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && SurfTilt < 158.2) { // slope/grade = 40%!
                TiltString = format("{:.1R}", SurfTilt);
                ShowWarningError(state,
                                 RoutineName + "Floor is not oriented correctly! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", in Zone=\"" +
                                     state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName + "\".");
            }
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = SurfWorldAz;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = SurfTilt;

            // Sine and cosine of azimuth and tilt
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround == DataGlobalConstants::AutoCalculate) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
            }
            // Outward normal unit vector (pointing away from room)
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
            for (n = 1; n <= 3; ++n) {
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
            }

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window || state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area *= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
            // Can perform tests on this surface here
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);

            // Call to transform vertices

            TransformVertsByAspect(state, SurfNum, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides);

        } else {
            ShowFatalError(state, RoutineName + "Called with less than 2 sides, Surface=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
        }

        // Preliminary Height/Width
        temp = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(3) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2);
        ThisWidth = VecLength(temp);
        temp = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1);
        ThisHeight = VecLength(temp);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height = ThisHeight;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width = ThisWidth;
    }

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
        static std::string const RoutineName("ReverseAndRecalculate: ");

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int n;      // Loop Control
        int RevPtr; // pointer for reversing vertices
        std::string TiltString;

        // Object Data
        Array1D<Vector> Vertices(NSides); // Vertices, in specified order

        for (n = 1; n <= NSides; ++n) {
            Vertices(n) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n);
        }
        RevPtr = NSides;
        for (n = 1; n <= NSides; ++n) {
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n) = Vertices(RevPtr);
            --RevPtr;
        }

        print(state.files.debug, "Reversing Surface Name={}\n", state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
        for (n = 1; n <= NSides; ++n) {
            print(state.files.debug,
                  "side={:5} abs coord vertex= {:18.13F} {:18.13F} {:18.13F}\n",
                  n,
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x,
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y,
                  state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).z);
        }

        CreateNewellSurfaceNormalVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea,
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && SurfTilt > 80.0) {
            TiltString = format("{:.1R}", SurfTilt);
            ShowWarningError(state, RoutineName + "Roof/Ceiling is still upside down! Tilt angle=[" + TiltString +
                             "], should be near 0, please fix manually.");
        }
        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && SurfTilt < 158.2) { // 40% grade!
            ShowWarningError(state, RoutineName + "Floor is still upside down! Tilt angle=[" + TiltString + "], should be near 180, please fix manually.");
        }
    }

    void MakeMirrorSurface(EnergyPlusData &state, int &SurfNum) // In=>Surface to Mirror, Out=>new Surface index
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

        using namespace Vectors;

        int Vert;
        int NVert;
        Real64 SurfWorldAz;
        Real64 SurfTilt;
        int n;
        //  TYPE(Vector) :: temp1

        NVert = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Vertex.allocate(NVert);
        // doesn't work when Vertex are pointers  state.dataSurfaceGeometry->SurfaceTmp(SurfNum+1)=state.dataSurfaceGeometry->SurfaceTmp(SurfNum)
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Name = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Construction = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ConstructionStoredInputValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ConstructionStoredInputValue;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Class = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).GrossArea = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Height = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Reveal = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Reveal;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Shape = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Shape;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Sides = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Tilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Width = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).HeatTransSurf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).BaseSurf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Zone = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ViewFactorGround = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ViewFactorSky = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ViewFactorGroundIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).SchedShadowSurfIndex = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ShadowSurfSchedVaries = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfSchedVaries;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).SchedMinValue = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMinValue;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).IsTransparent = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).IsTransparent;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).ShadowingSurf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowingSurf;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).MaterialMovInsulExt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulExt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).MaterialMovInsulInt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).SchedMovInsulExt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMovInsulExt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).SchedMovInsulInt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMovInsulInt;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).activeWindowShadingControl = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).windowShadingControlList = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).HasShadeControl = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).activeShadedConstruction = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).FrameDivider = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Multiplier = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Perimeter = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter;

        for (Vert = 1; Vert <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++Vert) {
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum + 1).Vertex(Vert) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(NVert);
            --NVert;
        }
        ++SurfNum;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name = "Mir-" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum - 1).Name;

        // TH 3/26/2010
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MirroredSurf = true;

        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides > 2) {
            CreateNewellAreaVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = VecLength(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
            CreateNewellSurfaceNormalVector(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            DetermineAzimuthAndTilt(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides,
                                    SurfWorldAz,
                                    SurfTilt,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea,
                                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth = SurfWorldAz;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt = SurfTilt;

            // Sine and cosine of azimuth and tilt
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
            // Outward normal unit vector (pointing away from room)
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
            for (n = 1; n <= 3; ++n) {
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
                if (std::abs(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
            }

            // Can perform tests on this surface here
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt);
        }
    }

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
        using namespace DataIPShortCuts;
        using ScheduleManager::GetScheduleIndex;

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const NumValidShadingTypes(8);
        static Array1D_string const cValidShadingTypes(NumValidShadingTypes,
                                                       {"INTERIORSHADE",
                                                        "EXTERIORSHADE",
                                                        "EXTERIORSCREEN",
                                                        "INTERIORBLIND",
                                                        "EXTERIORBLIND",
                                                        "BETWEENGLASSSHADE",
                                                        "BETWEENGLASSBLIND",
                                                        "SWITCHABLEGLAZING"});
        static Array1D_int const ValidShadingTypes(NumValidShadingTypes,
                                                   {WSC_ST_InteriorShade,
                                                    WSC_ST_ExteriorShade,
                                                    WSC_ST_ExteriorScreen,
                                                    WSC_ST_InteriorBlind,
                                                    WSC_ST_ExteriorBlind,
                                                    WSC_ST_BetweenGlassShade,
                                                    WSC_ST_BetweenGlassBlind,
                                                    WSC_ST_SwitchableGlazing});

        int const NumValidWindowShadingControlTypes(21);
        static Array1D_string const cValidWindowShadingControlTypes(NumValidWindowShadingControlTypes,
                                                                    {"ALWAYSON",
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
                                                                     "ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR"});

        static Array1D_int const ValidWindowShadingControlTypes(
            NumValidWindowShadingControlTypes,
            {WSCT_AlwaysOn,
             WSCT_AlwaysOff,
             WSCT_OnIfScheduled,
             WSCT_HiSolar,
             WSCT_HiHorzSolar,
             WSCT_HiOutAirTemp,
             WSCT_HiZoneAirTemp,
             WSCT_HiZoneCooling,
             WSCT_HiGlare,
             WSCT_MeetDaylIlumSetp,
             WSCT_OnNightLoOutTemp_OffDay,
             WSCT_OnNightLoInTemp_OffDay,
             WSCT_OnNightIfHeating_OffDay,
             WSCT_OnNightLoOutTemp_OnDayCooling,
             WSCT_OnNightIfHeating_OnDayCooling,
             WSCT_OffNight_OnDay_HiSolarWindow,
             WSCT_OnNight_OnDay_HiSolarWindow,
             WSCT_OnHiOutTemp_HiSolarWindow,
             WSCT_OnHiOutTemp_HiHorzSolar,
             WSCT_OnHiZoneTemp_HiSolarWindow,
             WSCT_OnHiZoneTemp_HiHorzSolar}); // 'ALWAYSON                                    ', & | 'ALWAYSOFF                                   ', &
                                              // | 'ONIFSCHEDULEALLOWS                          ', & | 'ONIFHIGHSOLARONWINDOW                       ',
                                              // & | 'ONIFHIGHHORIZONTALSOLAR                     ', & | 'ONIFHIGHOUTDOORAIRTEMPERATURE
                                              // ', & | 'ONIFHIGHZONEAIRTEMPERATURE                         ', & | 'ONIFHIGHZONECOOLING
                                              // ', & | 'ONIFHIGHGLARE                               ', & | 'MEETDAYLIGHTILLUMINANCESETPOINT
                                              // ', & | 'ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY              ', & | 'ONNIGHTIFLOWINSIDETEMPANDOFFDAY
                                              // ', & | 'ONNIGHTIFHEATINGANDOFFDAY                     ', & |
                                              // 'ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING      ', & | 'ONNIGHTIFHEATINGANDONDAYIFCOOLING
                                              // ', & | 'OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW ', & |
                                              // 'ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW  ', & | 'ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW  ',
                                              // & | 'ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR', & | 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW
                                              // ', & | 'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR   '/)

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
        int ShTyp;               // Shading type
        std::string ControlType; // Shading control type
        bool BGShadeBlindError;  // True if problem with construction that is supposed to have between-glass
        // shade or blind
        int Found;


        // Get the total number of window shading control blocks
        cCurrentModuleObject = "WindowShadingControl";
        TotWinShadingControl = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotWinShadingControl == 0) return;

        WindowShadingControl.allocate(TotWinShadingControl);

        ControlNum = 0;
        for (Loop = 1; Loop <= TotWinShadingControl; ++Loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          ControlNumAlpha,
                                          rNumericArgs,
                                          ControlNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            ErrorInName = false;
            IsBlank = false;
            UtilityRoutines::VerifyName(state, cAlphaArgs(1), WindowShadingControl, ControlNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ErrorsFound = true;
                continue;
            }

            ++ControlNum;
            WindowShadingControl(ControlNum).Name = cAlphaArgs(1); // Set the Control Name in the Derived Type

            WindowShadingControl(ControlNum).ZoneIndex = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            if (WindowShadingControl(ControlNum).ZoneIndex == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

            WindowShadingControl(ControlNum).SequenceNumber = int(rNumericArgs(1));
            // WindowShadingControl().getInputShadedConstruction is only used during GetInput process and is ultimately stored in Surface().shadedConstructionList
            WindowShadingControl(ControlNum).getInputShadedConstruction = UtilityRoutines::FindItemInList(cAlphaArgs(4), state.dataConstruction->Construct, TotConstructs);
            WindowShadingControl(ControlNum).ShadingDevice = UtilityRoutines::FindItemInList(cAlphaArgs(9), state.dataMaterial->Material, TotMaterials);
            WindowShadingControl(ControlNum).Schedule = GetScheduleIndex(state, cAlphaArgs(6));
            WindowShadingControl(ControlNum).SetPoint = rNumericArgs(2);
            WindowShadingControl(ControlNum).SetPoint2 = rNumericArgs(3);
            WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
            if (cAlphaArgs(7) == "YES") WindowShadingControl(ControlNum).ShadingControlIsScheduled = true;
            WindowShadingControl(ControlNum).GlareControlIsActive = false;
            if (cAlphaArgs(8) == "YES") WindowShadingControl(ControlNum).GlareControlIsActive = true;
            WindowShadingControl(ControlNum).SlatAngleSchedule = GetScheduleIndex(state, cAlphaArgs(11));

            // store the string for now and associate it after daylighting control objects are read
            WindowShadingControl(ControlNum).DaylightingControlName = cAlphaArgs(12);

            if (cAlphaArgs(13) == "SEQUENTIAL") {
                WindowShadingControl(ControlNum).MultiSurfaceCtrlIsGroup = false;
            } else if (cAlphaArgs(13) == "GROUP") {
                WindowShadingControl(ControlNum).MultiSurfaceCtrlIsGroup = true;
            } else {
                WindowShadingControl(ControlNum).MultiSurfaceCtrlIsGroup = false;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" should be either SEQUENTIAL or GROUP " +
                                 cAlphaFieldNames(13) + "=\"" + cAlphaArgs(13) + "\", defaulting to \"SEQUENTIAL\"");
            }
            ControlType = cAlphaArgs(5);

            if (ControlNumAlpha >= 14) {
                WindowShadingControl(ControlNum).FenestrationCount = ControlNumAlpha - 13;
                WindowShadingControl(ControlNum).FenestrationName.allocate(WindowShadingControl(ControlNum).FenestrationCount);
                WindowShadingControl(ControlNum).FenestrationIndex.allocate(WindowShadingControl(ControlNum).FenestrationCount);
                for (int i = 1; i <= WindowShadingControl(ControlNum).FenestrationCount; i++) {
                    WindowShadingControl(ControlNum).FenestrationName(i) = cAlphaArgs(i + 13);
                }
            } else {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                "\" invalid. Must reference at least one Fenestration Surface object name.");
            }

            if (ControlType == "SCHEDULE") {
                ControlType = "ONIFSCHEDULEALLOWS";
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = true;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
                // Error if schedule has not been specified
                if (WindowShadingControl(ControlNum).Schedule <= 0) {
                    ErrorsFound = true;
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + " has " + cAlphaFieldNames(5) + " \"" +
                                     ControlType + "\" but a schedule has not been specified.");
                }
            }

            if (has_prefix(ControlType, "SCHEDULEAND")) {
                ControlType = "ONIFHIGH" + ControlType.substr(11);
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = true;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
                // Error if schedule has not been specified
                if (WindowShadingControl(ControlNum).Schedule <= 0) {
                    ErrorsFound = true;
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + " has " + cAlphaFieldNames(5) + " \"" +
                                     ControlType + "\" but a schedule has not been specified.");
                }
            }

            if (has_prefix(ControlType, "GLAREOR")) {
                ControlType = "ONIFHIGH" + ControlType.substr(7);
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = true;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            if (ControlType == "GLARE") {
                ControlType = "ONIFHIGHGLARE";
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = true;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            if (WindowShadingControl(ControlNum).ShadingDevice > 0) {
                if (state.dataMaterial->Material(WindowShadingControl(ControlNum).ShadingDevice).Group == Screen &&
                    !(ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS")) {
                    ErrorsFound = true;
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) +
                                    "=\"" + cAlphaArgs(5) + "\" for exterior screens.");
                    ShowContinueError(state, "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                }
            } else {
                if (WindowShadingControl(ControlNum).getInputShadedConstruction > 0) {
                    state.dataConstruction->Construct(WindowShadingControl(ControlNum).getInputShadedConstruction).IsUsed = true;
                    if (state.dataMaterial->Material(state.dataConstruction->Construct(WindowShadingControl(ControlNum).getInputShadedConstruction).LayerPoint(1)).Group == Screen &&
                        !(ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS")) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) +
                                        "=\"" + cAlphaArgs(5) + "\" for exterior screens.");
                        ShowContinueError(state, "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                    }
                } else if (lAlphaFieldBlanks(4)) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", " + cAlphaFieldNames(4) +
                                    " is blank.");
                    ShowContinueError(state, "A valid construction is required.");
                    ErrorsFound = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", " + cAlphaFieldNames(4) +
                                    " is invalid.");
                    ShowContinueError(state, "Construction=\"" + cAlphaArgs(4) + "\" was used. A valid construction is required.");
                    ErrorsFound = true;
                }
            }

            // Warning if setpoint is unintentionally zero
            if (WindowShadingControl(ControlNum).SetPoint == 0 && ControlType != "ALWAYSON" && ControlType != "ALWAYSOFF" &&
                ControlType != "ONIFSCHEDULEALLOWS" && ControlType != "SCHEDULE" && ControlType != "ONIFHIGHGLARE" && ControlType != "GLARE" &&
                ControlType != "DAYLIGHTILLUMINANCE") {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", The first SetPoint is zero.");
                ShowContinueError(state, "..You may have forgotten to specify that setpoint.");
            }

            // Upward compatibility for old Shading Control Type names
            if (ControlType == "SOLARONWINDOW" || ControlType == "HORIZONTALSOLAR" || ControlType == "OUTSIDEAIRTEMP" ||
                ControlType == "ZONEAIRTEMP" || ControlType == "ZONECOOLING") {
                ControlType = "ONIFHIGH" + ControlType;
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            // Error if illegal control type
            Found = UtilityRoutines::FindItemInList(ControlType, cValidWindowShadingControlTypes, NumValidWindowShadingControlTypes);
            if (Found == 0) {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) + "=\"" +
                                cAlphaArgs(5) + "\".");
            } else {
                WindowShadingControl(ControlNum).ShadingControlType = ValidWindowShadingControlTypes(Found);
            }

            // Error checks
            if (cAlphaArgs(7) != "YES" && cAlphaArgs(7) != "NO") { // Shading Control is Schedule field
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(7) + "=\"" +
                                cAlphaArgs(7) + "\".");
            }
            if (cAlphaArgs(8) != "YES" && cAlphaArgs(8) != "NO") { // Glare Control is Active field
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(8) + "=\"" +
                                cAlphaArgs(8) + "\".");
            }

            if ((WindowShadingControl(ControlNum).ShadingControlType == WSCT_OnIfScheduled) &&
                (!WindowShadingControl(ControlNum).ShadingControlIsScheduled)) { // CR 7709 BG
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + " = \"" + WindowShadingControl(ControlNum).Name + "\" invalid, " + cAlphaFieldNames(7) +
                                " must be set to \"Yes\" for " + cAlphaFieldNames(5) + " = OnIfScheduleAllows");
            }

            if (cAlphaArgs(10) != "FIXEDSLATANGLE" && cAlphaArgs(10) != "SCHEDULEDSLATANGLE" && cAlphaArgs(10) != "BLOCKBEAMSOLAR") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(10) + "=\"" +
                                cAlphaArgs(10) + "\".");
            } else if (cAlphaArgs(10) == "FIXEDSLATANGLE") {
                WindowShadingControl(ControlNum).SlatAngleControlForBlinds = WSC_SAC_FixedSlatAngle;
            } else if (cAlphaArgs(10) == "SCHEDULEDSLATANGLE") {
                WindowShadingControl(ControlNum).SlatAngleControlForBlinds = WSC_SAC_ScheduledSlatAngle;
            } else if (cAlphaArgs(10) == "BLOCKBEAMSOLAR") {
                WindowShadingControl(ControlNum).SlatAngleControlForBlinds = WSC_SAC_BlockBeamSolar;
            }

            // For upward compatibility change old "noninsulating" and "insulating" shade types to
            // INTERIORSHADE or EXTERIORSHADE
            if (cAlphaArgs(3) == "INTERIORNONINSULATINGSHADE" || cAlphaArgs(3) == "INTERIORINSULATINGSHADE") {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\", changing to \"InteriorShade\"");
                WindowShadingControl(ControlNum).ShadingType = WSC_ST_InteriorShade;
                cAlphaArgs(3) = "INTERIORSHADE";
            }
            if (cAlphaArgs(3) == "EXTERIORNONINSULATINGSHADE" || cAlphaArgs(3) == "EXTERIORINSULATINGSHADE") {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\", changing to \"ExteriorShade\"");
                WindowShadingControl(ControlNum).ShadingType = WSC_ST_ExteriorShade;
                cAlphaArgs(3) = "EXTERIORSHADE";
            }

            if (ControlType == "MEETDAYLIGHTILLUMINANCESETPOINT" && cAlphaArgs(3) != "SWITCHABLEGLAZING") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
                ShowContinueError(state, "..." + cAlphaFieldNames(3) + " must be SwitchableGlazing for this control, but entered type=\"" + cAlphaArgs(3) +
                                  "\".");
            }

            // Check for illegal shading type name
            Found = UtilityRoutines::FindItemInList(cAlphaArgs(3), cValidShadingTypes, NumValidShadingTypes);
            if (Found == 0) {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
            } else {
                WindowShadingControl(ControlNum).ShadingType = ValidShadingTypes(Found);
            }

            ShTyp = WindowShadingControl(ControlNum).ShadingType;
            IShadedConst = WindowShadingControl(ControlNum).getInputShadedConstruction;
            IShadingDevice = WindowShadingControl(ControlNum).ShadingDevice;

            if (IShadedConst == 0 && IShadingDevice == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name +
                                "\" has no matching shaded construction or shading device.");
                ErrorsFound = true;
            } else if (IShadedConst == 0 && IShadingDevice > 0) {
                if (ShTyp == WSC_ST_SwitchableGlazing) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= SwitchableGlazing but no matching shaded construction");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade) && state.dataMaterial->Material(IShadingDevice).Group != Shade) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= InteriorShade or ExteriorShade but matching shading device is not a window shade");
                    ShowContinueError(state, cAlphaFieldNames(8) + " in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_ExteriorScreen) && state.dataMaterial->Material(IShadingDevice).Group != Screen) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= ExteriorScreen but matching shading device is not a window screen");
                    ShowContinueError(state, cAlphaFieldNames(8) + " in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind) && state.dataMaterial->Material(IShadingDevice).Group != WindowBlind) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind");
                    ShowContinueError(state, cAlphaFieldNames(8) + " in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if (ShTyp == WSC_ST_BetweenGlassShade || ShTyp == WSC_ST_BetweenGlassBlind) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= BetweenGlassShade or BetweenGlassBlind and");
                    ShowContinueError(state, cAlphaFieldNames(8) + " is specified. This is illegal. Specify shaded construction instead.");
                    ErrorsFound = true;
                }
            } else if (IShadedConst > 0 && IShadingDevice > 0) {
                IShadingDevice = 0;
                ShowWarningError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" Both " + cAlphaFieldNames(4) + " and " +
                                 cAlphaFieldNames(9) + " are specified.");
                ShowContinueError(state, "The " + cAlphaFieldNames(4) + "=\"" + state.dataConstruction->Construct(IShadedConst).Name + "\" will be used.");
            }

            // If type = interior or exterior shade or blind require that the shaded construction
            // have a shade layer in the correct position
            if (IShadedConst != 0) {

                NLayers = state.dataConstruction->Construct(IShadedConst).TotLayers;
                BGShadeBlindError = false;
                IShadingDevice = 0;
                if (state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers) != 0) {
                    if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_InteriorShade) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers)).Group != Shade) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a shade layer on the inside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorShade) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != Shade) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(43) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a shade layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorScreen) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != Screen) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a screen layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_InteriorBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers)).Group != WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a blind layer on the inside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a blind layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_BetweenGlassShade) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3)).Group != Shade) BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5)).Group != Shade) BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(32) +
                                              "\" should have two or three glass layers and a");
                            ShowContinueError(state, "between-glass shade layer with a gas layer on each side.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_BetweenGlassBlind) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3)).Group != WindowBlind) BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (state.dataMaterial->Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5)).Group != WindowBlind) BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError(state, "of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" should have two or three glass layers and a");
                            ShowContinueError(state, "between-glass blind layer with a gas layer on each side.");
                        }
                    }
                }
                if (IShadingDevice > 0) {
                    if ((ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade) && state.dataMaterial->Material(IShadingDevice).Group != Shade) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= InteriorShade or ExteriorShade but matching shading device is not a window shade");
                        ShowContinueError(state, "Shading Device in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WSC_ST_ExteriorScreen) && state.dataMaterial->Material(IShadingDevice).Group != Screen) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= ExteriorScreen but matching shading device is not an exterior window screen.");
                        ShowContinueError(state, "Shading Device in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind) && state.dataMaterial->Material(IShadingDevice).Group != WindowBlind) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind.");
                        ShowContinueError(state, "Shading Device in error=\"" + state.dataMaterial->Material(IShadingDevice).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            }
        } // End of loop over window shading controls
    }

    void InitialAssociateWindowShadingControlFenestration(EnergyPlusData &state, bool &ErrorsFound, int &SurfNum)
    {
        // J.Glazer 2018 - operates on SurfaceTmp array before final indices are known for windows and sets the activeWindowShadingControl
        for (int iShadeCtrl = 1; iShadeCtrl <= TotWinShadingControl; ++iShadeCtrl) {
            int curShadedConstruction = WindowShadingControl(iShadeCtrl).getInputShadedConstruction;
            for (int jFeneRef = 1; jFeneRef <= WindowShadingControl(iShadeCtrl).FenestrationCount; ++jFeneRef) {
                if (UtilityRoutines::SameString(WindowShadingControl(iShadeCtrl).FenestrationName(jFeneRef), state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name)) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl = true;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList.push_back(iShadeCtrl);
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl = iShadeCtrl;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList.push_back(curShadedConstruction);
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = curShadedConstruction;

                    // check to make the window refenced is an exterior window
                    if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment) {
                        ErrorsFound = true;
                        ShowSevereError(state, "InitialAssociateWindowShadingControlFenestration: \"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " +
                                        " because it is not an exterior window.");
                        ShowContinueError(state, ".. It appears on WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name);
                    }
                    // check to make sure the window is not using equivalent layer window construction
                    if (state.dataConstruction->Construct(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
                        ErrorsFound = true;
                        ShowSevereError(state, "InitialAssociateWindowShadingControlFenestration: =\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", invalid " +
                                        "\".");
                        ShowContinueError(state, ".. equivalent layer window model does not use shading control object.");
                        ShowContinueError(state, ".. Shading control is set to none or zero, and simulation continues.");
                        ShowContinueError(state, ".. It appears on WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name);
                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
                    }
                }
            }
        }
    }

    void FinalAssociateWindowShadingControlFenestration(EnergyPlusData &state, bool &ErrorsFound)
    {
        // J.Glazer 2018 - operates on Surface array after final indices are known for windows and checks to make sure it is correct
        for (int iShadeCtrl = 1; iShadeCtrl <= TotWinShadingControl; ++iShadeCtrl) {
            for (int jFeneRef = 1; jFeneRef <= WindowShadingControl(iShadeCtrl).FenestrationCount; ++jFeneRef) {
                int fenestrationIndex =
                    UtilityRoutines::FindItemInList(WindowShadingControl(iShadeCtrl).FenestrationName(jFeneRef), Surface, TotSurfaces);
                if (std::find(Surface(fenestrationIndex).windowShadingControlList.begin(), Surface(fenestrationIndex).windowShadingControlList.end(),
                      iShadeCtrl) != Surface(fenestrationIndex).windowShadingControlList.end()) {
                    WindowShadingControl(iShadeCtrl).FenestrationIndex(jFeneRef) = fenestrationIndex;
                } else {
                    // this error condition should not occur since the rearrangement of Surface() from SurfureTmp() is reliable.
                    ErrorsFound = true;
                    ShowSevereError(state, "FinalAssociateWindowShadingControlFenestration: Fenestration surface named \"" +
                                    Surface(fenestrationIndex).Name +
                                    "\" has WindowShadingContol index that does not match the initial index assigned.");
                    ShowContinueError(state, "This occurs while WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name +
                                      "\" is being evaluated. ");
                }
            }
        }
    }

    void CheckWindowShadingControlSimilarForWindow(EnergyPlusData &state, bool& ErrorsFound)
    {
        // For each window check if all window shading controls on list are the same except for name, schedule name, construction, and material
        for (auto theSurf : Surface) {
            if (theSurf.HasShadeControl) {
                if (theSurf.windowShadingControlList.size() > 1) {
                    int firstWindowShadingControl = theSurf.windowShadingControlList.front();
                    for (auto wsc = std::next(theSurf.windowShadingControlList.begin()); wsc != theSurf.windowShadingControlList.end(); ++wsc) {
                        if (!isWindowShadingControlSimilar(firstWindowShadingControl, *wsc)) {
                            ErrorsFound = true;
                            ShowSevereError(state, "CheckWindowShadingControlSimilarForWindow: Fenestration surface named \"" + theSurf.Name +
                                        "\" has multiple WindowShadingContols that are not similar.");
                            ShowContinueError(state, "for: \"" + WindowShadingControl(firstWindowShadingControl).Name + " and: " + WindowShadingControl(*wsc).Name);
                        }
                    }
                }
            }
        }
    }

    bool isWindowShadingControlSimilar(int a, int b)
    {
        // Compares two window shading controls are the same except for the name, schedule name, construction, and material
        return (WindowShadingControl(a).ZoneIndex == WindowShadingControl(b).ZoneIndex &&
            WindowShadingControl(a).ShadingType == WindowShadingControl(b).ShadingType &&
            WindowShadingControl(a).ShadingControlType == WindowShadingControl(b).ShadingControlType &&
            WindowShadingControl(a).SetPoint == WindowShadingControl(b).SetPoint &&
            WindowShadingControl(a).ShadingControlIsScheduled == WindowShadingControl(b).ShadingControlIsScheduled &&
            WindowShadingControl(a).GlareControlIsActive == WindowShadingControl(b).GlareControlIsActive &&
            WindowShadingControl(a).SlatAngleControlForBlinds == WindowShadingControl(b).SlatAngleControlForBlinds &&
            WindowShadingControl(a).SetPoint2 == WindowShadingControl(b).SetPoint2 &&
            WindowShadingControl(a).DaylightingControlName == WindowShadingControl(b).DaylightingControlName &&
            WindowShadingControl(a).DaylightControlIndex == WindowShadingControl(b).DaylightControlIndex &&
            WindowShadingControl(a).MultiSurfaceCtrlIsGroup == WindowShadingControl(b).MultiSurfaceCtrlIsGroup);
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
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;           // IO Status when calling get input subroutine
        int StormWinNumAlpha; // Number of alpha names being passed
        int StormWinNumProp;  // Number of properties being passed
        int StormWinNum;      // Index for storm window number
        int loop;             // Do loop counter
        int SurfNum;          // Surface number
        int MatNum;           // Material number



        // Get the total number of storm window input objects
        cCurrentModuleObject = "WindowProperty:StormWindow";
        TotStormWin = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotStormWin == 0) return;

        StormWindow.allocate(TotStormWin);

        StormWinNum = 0;
        for (loop = 1; loop <= TotStormWin; ++loop) {

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          loop,
                                          cAlphaArgs,
                                          StormWinNumAlpha,
                                          rNumericArgs,
                                          StormWinNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ++StormWinNum;
            StormWindow(StormWinNum).BaseWindowNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), Surface, TotSurfaces);
            StormWindow(StormWinNum).StormWinMaterialNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataMaterial->Material, TotMaterials);
            StormWindow(StormWinNum).StormWinDistance = rNumericArgs(1);
            StormWindow(StormWinNum).MonthOn = rNumericArgs(2);
            StormWindow(StormWinNum).DayOfMonthOn = rNumericArgs(3);
            StormWindow(StormWinNum).DateOn = General::OrdinalDay(StormWindow(StormWinNum).MonthOn, StormWindow(StormWinNum).DayOfMonthOn, 1);
            StormWindow(StormWinNum).MonthOff = rNumericArgs(4);
            StormWindow(StormWinNum).DayOfMonthOff = rNumericArgs(5);
            StormWindow(StormWinNum).DateOff = General::OrdinalDay(StormWindow(StormWinNum).MonthOff, StormWindow(StormWinNum).DayOfMonthOff, 1);

            if (StormWindow(StormWinNum).DateOn == StormWindow(StormWinNum).DateOff) {
                ShowSevereError(state,
                                format("{}: Date On = Date Off -- not allowed, occurred in WindowProperty:StormWindow Input #{}",
                                       cCurrentModuleObject,
                                       StormWinNum));
                ErrorsFound = true;
            }

            {
                auto const SELECT_CASE_var(StormWindow(StormWinNum).MonthOn);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 31) {
                        ShowSevereError(state,
                                        format("{}: Date On (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOn,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 30) {
                        ShowSevereError(state,
                                        format("{}: Date On (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOn,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 29) {
                        ShowSevereError(state,
                                        format("{}: Date On (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOn,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    format("{}: Date On Month [{}], invalid for WindowProperty:StormWindow Input #{}",
                                           cCurrentModuleObject,
                                           StormWindow(StormWinNum).MonthOn,
                                           StormWinNum));
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(StormWindow(StormWinNum).MonthOff);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 31) {
                        ShowSevereError(state,
                                        format("{}: Date Off (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOff,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 30) {
                        ShowSevereError(state,
                                        format("{}: Date Off (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOff,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 29) {
                        ShowSevereError(state,
                                        format("{}: Date Off (Day of Month) [{}], invalid for WindowProperty:StormWindow Input #{}",
                                               cCurrentModuleObject,
                                               StormWindow(StormWinNum).DayOfMonthOff,
                                               StormWinNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    format("{}: Date Off Month [{}], invalid for WindowProperty:StormWindow Input #{}",
                                           cCurrentModuleObject,
                                           StormWindow(StormWinNum).MonthOff,
                                           StormWinNum));
                    ErrorsFound = true;
                }
            }
        }

        // Error checks

        for (StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
            // Require BaseWindowNum be that of an exterior window
            SurfNum = StormWindow(StormWinNum).BaseWindowNum;
            if (SurfNum == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid.");
                ErrorsFound = true;
            } else {
                if (Surface(SurfNum).Class != SurfaceClass::Window || Surface(SurfNum).ExtBoundCond != 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowSevereError(state, "cannot be used with surface=" + Surface(SurfNum).Name);
                    ShowContinueError(state, "because that surface is not an exterior window.");
                    ErrorsFound = true;
                }
            }

            // Require that storm window material be glass
            MatNum = StormWindow(StormWinNum).StormWinMaterialNum;
            if (SurfNum > 0) {
                if (MatNum == 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowContinueError(state, cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found as storm window layer.");
                    ErrorsFound = true;
                } else {
                    if (state.dataMaterial->Material(MatNum).Group != WindowGlass) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                        ShowContinueError(state, cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                          "must be a WindowMaterial:Glazing or WindowMaterial:Glazing:RefractionExtinctionMethod");
                        ErrorsFound = true;
                    }
                }
            }

            // Error if base window has airflow control
            if (SurfNum > 0) {
                if (SurfWinAirflowControlType(SurfNum) != 0) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowContinueError(state, " cannot be used because it is an airflow window (i.e., has WindowProperty:AirflowControl specified)");
                    ErrorsFound = true;
                }
            }

            // Check for reversal of on and off times
            if (SurfNum > 0) {
                if ((state.dataEnvrn->Latitude > 0.0 && (StormWindow(StormWinNum).MonthOn < StormWindow(StormWinNum).MonthOff)) ||
                    (state.dataEnvrn->Latitude <= 0.0 && (StormWindow(StormWinNum).MonthOn > StormWindow(StormWinNum).MonthOff))) {
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" check times that storm window");
                    ShowContinueError(state,
                                      format("is put on (month={}, day={}) and taken off (month={}, day={});",
                                             StormWindow(StormWinNum).MonthOn,
                                             StormWindow(StormWinNum).DayOfMonthOn,
                                             StormWindow(StormWinNum).MonthOff,
                                             StormWindow(StormWinNum).DayOfMonthOff));
                    ShowContinueError(state, format("these times may be reversed for your building latitude={:.2R} deg.", state.dataEnvrn->Latitude));
                }
            }
        }
    }

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
        using namespace DataIPShortCuts;
        using ScheduleManager::GetScheduleIndex;

        std::string const RoutineName("GetWindowGapAirflowControlData");
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
        // of the shaded construction of airflow window

        // Get the total number of window airflow control statements
        cCurrentModuleObject = "WindowProperty:AirflowControl";
        TotWinAirflowControl = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        if (TotWinAirflowControl == 0) return;

        for (Loop = 1; Loop <= TotWinAirflowControl; ++Loop) { // Loop through all surfaces in the input...

            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          ControlNumAlpha,
                                          rNumericArgs,
                                          ControlNumProp,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), Surface, TotSurfaces);
            if (SurfNum == 0) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" not found.");
                ErrorsFound = true;
            }
            // Check that associated surface is a 2- or 3-pane exterior window
            WrongSurfaceType = false;
            if (SurfNum != 0) {
                if (Surface(SurfNum).Class != SurfaceClass::Window) WrongSurfaceType = true;
                if (Surface(SurfNum).Class == SurfaceClass::Window) {
                    ConstrNum = Surface(SurfNum).Construction;
                    if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers != 2 && state.dataConstruction->Construct(ConstrNum).TotGlassLayers != 3) WrongSurfaceType = true;
                    if (Surface(SurfNum).ExtBoundCond != ExternalEnvironment) WrongSurfaceType = true;
                }
                if (WrongSurfaceType) {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not an exterior window with 2 or 3 glass layers.");
                    ErrorsFound = true;
                }
            }

            // Error if illegal airflow source
            if (cAlphaArgs(2) != "INDOORAIR" && cAlphaArgs(2) != "OUTDOORAIR") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
            }

            // Error if illegal airflow destination
            if (cAlphaArgs(3) != "INDOORAIR" && cAlphaArgs(3) != "OUTDOORAIR" && cAlphaArgs(3) != "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
            }

            // Error if source = OutsideAir and destination = ReturnAir
            if (cAlphaArgs(2) == "OUTDOORAIR" && cAlphaArgs(3) == "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
                ShowContinueError(state, "..when " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
            }

            // Error if illegal airflow control type
            if (cAlphaArgs(4) != "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs(4) != "ALWAYSOFF" && cAlphaArgs(4) != "SCHEDULEDONLY") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
            }

            // Error if illegal value for Airflow Has Multiplier Schedule
            if (cAlphaArgs(5) != "YES" && cAlphaArgs(5) != "NO") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\"");
            }

            // Error if Airflow Control Type = ScheduledOnly and Airflow Has Multiplier Schedule = No
            if (cAlphaArgs(4) == "SCHEDULEDONLY" && cAlphaArgs(5) == "NO") {
                ErrorsFound = true;
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError(state, "..when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\"");
            }

            // Warning if Airflow Control Type = AlwaysOnAtMaxFlow and Airflow Has Multiplier Schedule = Yes
            if (cAlphaArgs(4) == "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs(5) == "YES") {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError(state, "..but " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "If specified, the " + cAlphaFieldNames(5) +
                                  " will be ignored.");
            }

            // Warning if Airflow Control Type = AlwaysOff and Airflow Has Multiplier Schedule = Yes
            if (cAlphaArgs(4) == "ALWAYSOFF" && cAlphaArgs(5) == "YES") {
                ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError(state, "..but " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\". If specified, the " + cAlphaFieldNames(5) +
                                  " will be ignored.");
            }

            if (SurfNum > 0) {
                AirflowWindows = true;
                if (UtilityRoutines::SameString(cAlphaArgs(2), "IndoorAir")) {
                    SurfWinAirflowSource(SurfNum) = AirFlowWindow_Source_IndoorAir;
                } else if (UtilityRoutines::SameString(cAlphaArgs(2), "OutdoorAir")) {
                    SurfWinAirflowSource(SurfNum) = AirFlowWindow_Source_OutdoorAir;
                }
                if (UtilityRoutines::SameString(cAlphaArgs(3), "IndoorAir")) {
                    SurfWinAirflowDestination(SurfNum) = AirFlowWindow_Destination_IndoorAir;
                } else if (UtilityRoutines::SameString(cAlphaArgs(3), "OutdoorAir")) {
                    SurfWinAirflowDestination(SurfNum) = AirFlowWindow_Destination_OutdoorAir;
                } else if (UtilityRoutines::SameString(cAlphaArgs(3), "ReturnAir")) {
                    SurfWinAirflowDestination(SurfNum) = AirFlowWindow_Destination_ReturnAir;
                    int controlledZoneNum = DataZoneEquipment::GetControlledZoneIndex(state, Surface(SurfNum).ZoneName);
                    if (controlledZoneNum > 0) {
                        state.dataZoneEquip->ZoneEquipConfig(controlledZoneNum).ZoneHasAirFlowWindowReturn = true;
                        DataHeatBalance::Zone(Surface(SurfNum).Zone).HasAirFlowWindowReturn = true;
                    }

                    // Set return air node number
                    SurfWinAirflowReturnNodePtr(SurfNum) = 0;
                    std::string retNodeName = "";
                    if (!lAlphaFieldBlanks(7)) {
                        retNodeName = cAlphaArgs(7);
                    }
                    std::string callDescription = cCurrentModuleObject + "=" + Surface(SurfNum).Name;
                    SurfWinAirflowReturnNodePtr(SurfNum) =
                        DataZoneEquipment::GetReturnAirNodeForZone(state, Surface(SurfNum).ZoneName, retNodeName, callDescription);
                    if (SurfWinAirflowReturnNodePtr(SurfNum) == 0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + Surface(SurfNum).Name +
                                        "\", airflow window return air node not found for " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                        if (!lAlphaFieldBlanks(7))
                            ShowContinueError(state, cAlphaFieldNames(7) + "=\"" + cAlphaArgs(7) + "\" did not find a matching return air node.");
                        ShowContinueError(state, "..Airflow windows with Airflow Destination = ReturnAir must reference a controlled Zone (appear in a "
                                          "ZoneHVAC:EquipmentConnections object) with at least one return air node.");
                        ErrorsFound = true;
                    }
                }
                if (UtilityRoutines::SameString(cAlphaArgs(4), "AlwaysOnAtMaximumFlow")) {
                    SurfWinAirflowControlType(SurfNum) = AirFlowWindow_ControlType_MaxFlow;
                } else if (UtilityRoutines::SameString(cAlphaArgs(4), "AlwaysOff")) {
                    SurfWinAirflowControlType(SurfNum) = AirFlowWindow_ControlType_AlwaysOff;
                } else if (UtilityRoutines::SameString(cAlphaArgs(4), "ScheduledOnly")) {
                    SurfWinAirflowControlType(SurfNum) = AirFlowWindow_ControlType_Schedule;
                }
                SurfWinMaxAirflow(SurfNum) = rNumericArgs(1);
                if (cAlphaArgs(4) == "SCHEDULEDONLY" && cAlphaArgs(5) == "YES") {
                    if (lAlphaFieldBlanks(6)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                        "\"");
                        ShowContinueError(state, "..and " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", but no " + cAlphaFieldNames(6) +
                                          " specified.");
                    } else {
                        SurfWinAirflowHasSchedule(SurfNum) = true;
                        SurfWinAirflowSchedulePtr(SurfNum) = GetScheduleIndex(state, cAlphaArgs(6));
                        if (SurfWinAirflowSchedulePtr(SurfNum) == 0) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(6) + "=\"" +
                                            cAlphaArgs(6) + "\"");
                        }
                    }
                }
                // Warning if associated window is an interior window
                if (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && !ErrorsFound)
                    ShowWarningError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", is an Interior window; cannot be an airflow window.");
                if (!ErrorsFound) {
                    // Require that gas in airflow gap has type = air
                    MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(2);
                    if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers == 3) MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(4);
                    if (state.dataMaterial->Material(MatGapFlow).GasType(1) != 1) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Gas type not air in airflow gap of construction " +
                                        state.dataConstruction->Construct(ConstrNum).Name);
                    }
                    // Require that gas be air in airflow gaps on either side of a between glass shade/blind
                    if (Surface(SurfNum).HasShadeControl) {
                        for (std::size_t listIndex = 0; listIndex < Surface(SurfNum).windowShadingControlList.size(); ++listIndex) {
                            int WSCPtr = Surface(SurfNum).windowShadingControlList[listIndex];
                            if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassShade ||
                                WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                                ConstrNumSh = Surface(SurfNum).shadedConstructionList[listIndex];
                                if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers == 2) {
                                    MatGapFlow1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2);
                                    MatGapFlow2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(4);
                                }
                                else {
                                    MatGapFlow1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(4);
                                    MatGapFlow2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(6);
                                }
                                if (state.dataMaterial->Material(MatGapFlow1).GasType(1) != 1 || state.dataMaterial->Material(MatGapFlow2).GasType(1) != 1) {
                                    ErrorsFound = true;
                                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                        "\", gas type must be air on either side of the shade/blind");
                                }
                                break; // only need the first window shading control since they should be the same
                            }
                        }
                    }
                }
            }

        } // End of loop over window airflow controls
    }

    void GetFoundationData(EnergyPlusData &state, bool &ErrorsFound)
    {
        using namespace DataIPShortCuts;

        int NumAlphas;
        int NumProps;
        int IOStat;

        // Read Kiva Settings
        cCurrentModuleObject = "Foundation:Kiva:Settings";
        int TotKivaStgs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotKivaStgs > 1) {
            ErrorsFound = true;
            ShowSevereError(state, "Multiple " + cCurrentModuleObject + " objects found. Only one is allowed.");
        }

        if (TotKivaStgs == 1) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          1,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumProps,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            int numF = 1;
            int alpF = 1;

            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.soilK = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.soilRho = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.soilCp = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.groundSolarAbs = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.groundThermalAbs = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.groundRoughness = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.farFieldWidth = rNumericArgs(numF);
            }
            numF++;

            if (!lAlphaFieldBlanks(alpF)) {
                if (UtilityRoutines::SameString(cAlphaArgs(alpF), "ZeroFlux")) {
                    state.dataSurfaceGeometry->kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::ZERO_FLUX;
                } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "GroundWater")) {
                    state.dataSurfaceGeometry->kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::GROUNDWATER;
                } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "Autoselect")) {
                    state.dataSurfaceGeometry->kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::AUTO;
                } else {
                    ErrorsFound = true;
                    ShowSevereError(state, "Foundation:Kiva:Settings, " + cAlphaArgs(alpF) + " is not a valid choice for " + cAlphaFieldNames(alpF));
                }
            }
            alpF++;

            if (lNumericFieldBlanks(numF) || rNumericArgs(numF) == DataGlobalConstants::AutoCalculate) {
                state.dataSurfaceGeometry->kivaManager.settings.deepGroundDepth = 40.0;
            } else {
                state.dataSurfaceGeometry->kivaManager.settings.deepGroundDepth = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.minCellDim = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                state.dataSurfaceGeometry->kivaManager.settings.maxGrowthCoeff = rNumericArgs(numF);
            }
            numF++;

            if (!lAlphaFieldBlanks(alpF)) {
                if (UtilityRoutines::SameString(cAlphaArgs(alpF), "Hourly")) {
                    state.dataSurfaceGeometry->kivaManager.settings.timestepType = HeatBalanceKivaManager::KivaManager::Settings::HOURLY;
                    state.dataSurfaceGeometry->kivaManager.timestep = 3600.; // seconds
                } else /* if (UtilityRoutines::SameString(cAlphaArgs( alpF ), "Timestep")) */ {
                    state.dataSurfaceGeometry->kivaManager.settings.timestepType = HeatBalanceKivaManager::KivaManager::Settings::TIMESTEP;
                    state.dataSurfaceGeometry->kivaManager.timestep = state.dataGlobal->MinutesPerTimeStep * 60.;
                }
            }
            alpF++;
        }

        /* ====================================================================== */

        // Read Foundation objects
        cCurrentModuleObject = "Foundation:Kiva";
        int TotKivaFnds = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (TotKivaFnds > 0) {
            state.dataSurfaceGeometry->kivaManager.defineDefaultFoundation(state);

            Array1D_string fndNames;
            fndNames.allocate(TotKivaFnds + 1);
            fndNames(1) = "<Default Foundation>";

            for (int Loop = 1; Loop <= TotKivaFnds; ++Loop) {
                inputProcessor->getObjectItem(state,
                                              cCurrentModuleObject,
                                              Loop,
                                              cAlphaArgs,
                                              NumAlphas,
                                              rNumericArgs,
                                              NumProps,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);

                int numF = 1;
                int alpF = 1;

                bool ErrorInName = false;

                HeatBalanceKivaManager::FoundationKiva fndInput;

                fndInput.name = cAlphaArgs(alpF);
                alpF++;
                UtilityRoutines::IsNameEmpty(state, fndInput.name, cCurrentModuleObject, ErrorInName);
                if (ErrorInName) {
                    ErrorsFound = true;
                    continue;
                } else {
                    fndNames(Loop) = fndInput.name;
                }

                // Start with copy of default
                auto &fnd = fndInput.foundation;
                fnd = state.dataSurfaceGeometry->kivaManager.defaultFoundation.foundation;

                // Indoor temperature
                if (!lNumericFieldBlanks(numF)) {
                    fndInput.assumedIndoorTemperature = rNumericArgs(numF);
                } else {
                    fndInput.assumedIndoorTemperature = -9999;
                }
                numF++;

                // Interior horizontal insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = state.dataMaterial->Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Must be of type \"Material\"");
                        continue;
                    }
                    fndInput.intHIns.x = 0.0;
                    fndInput.intHIns.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                    fndInput.intHIns.depth = m.Thickness;
                }
                alpF++;

                if (!lAlphaFieldBlanks(alpF - 1)) {
                    if (lNumericFieldBlanks(numF)) {
                        fndInput.intHIns.z = 0.0;
                    } else {
                        fndInput.intHIns.z = rNumericArgs(numF);
                    }
                    numF++;
                    if (lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.intHIns.width = -rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Interior vertical insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = state.dataMaterial->Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Must be of type \"Material\"");
                        continue;
                    }
                    fndInput.intVIns.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                    fndInput.intVIns.width = -m.Thickness;
                    fndInput.intVIns.x = 0.0;
                    fndInput.intVIns.z = 0.0;
                }
                alpF++;

                if (!lAlphaFieldBlanks(alpF - 1)) {
                    if (lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.intVIns.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Exterior horizontal insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = state.dataMaterial->Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Must be of type \"Material\"");
                        continue;
                    }
                    fndInput.extHIns.x = 0.0;
                    fndInput.extHIns.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                    fndInput.extHIns.depth = m.Thickness;
                }
                alpF++;

                if (!lAlphaFieldBlanks(alpF - 1)) {
                    if (lNumericFieldBlanks(numF)) {
                        fndInput.extHIns.z = 0.0;
                    } else {
                        fndInput.extHIns.z = rNumericArgs(numF);
                    }
                    numF++;
                    if (lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.extHIns.width = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Exterior vertical insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = state.dataMaterial->Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Must be of type \"Material\"");
                        continue;
                    }
                    fndInput.extVIns.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                    fndInput.extVIns.width = m.Thickness;
                    fndInput.extVIns.x = 0.0;
                    fndInput.extVIns.z = 0.0;
                }
                alpF++;

                if (!lAlphaFieldBlanks(alpF - 1)) {
                    if (lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.extVIns.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Foundation wall
                if (!lNumericFieldBlanks(numF)) {
                    fnd.wall.heightAboveGrade = rNumericArgs(numF);
                }
                numF++;

                if (!lNumericFieldBlanks(numF)) {
                    fnd.wall.depthBelowSlab = rNumericArgs(numF);
                }
                numF++;

                if (!lAlphaFieldBlanks(alpF)) {
                    fndInput.wallConstructionIndex = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataConstruction->Construct);
                    if (fndInput.wallConstructionIndex == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching construction for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing construction = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &c = state.dataConstruction->Construct(fndInput.wallConstructionIndex);
                    c.IsUsed = true;
                    if (c.TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Cannot be a window construction");
                        continue;
                    }
                } else {
                    fndInput.wallConstructionIndex = 0; // Use default wall construction
                }
                alpF++;

                // Footing
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = state.dataMaterial->Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError(state, "Must be of type \"Material\"");
                        continue;
                    }
                    fndInput.footing.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                    fndInput.footing.width = m.Thickness;
                    fndInput.footing.x = 0.0;
                    fndInput.footing.z = 0.0;
                }
                alpF++;

                if (!lAlphaFieldBlanks(alpF - 1)) {
                    if (lNumericFieldBlanks(numF)) {
                        ErrorsFound = true;
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.footing.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(state, cNumericFieldNames(numF) + " will not be used.");
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
                                                cCurrentModuleObject,
                                                fndInput.name,
                                                numBlocks));
                    }
                    for (int blockNum = 0; blockNum < numBlocks; blockNum++) {
                        Kiva::InputBlock block;
                        if (!lAlphaFieldBlanks(alpF)) {
                            int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), state.dataMaterial->Material);
                            if (index == 0) {
                                ErrorsFound = true;
                                ShowSevereError(state, "Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                                cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                                continue;
                            }
                            auto &m = state.dataMaterial->Material(index);
                            if (m.Group != RegularMaterial || m.ROnly) {
                                ErrorsFound = true;
                                ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                                cAlphaArgs(alpF));
                                ShowContinueError(state, "Must be of type \"Material\"");
                                continue;
                            }
                            block.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                            block.width = m.Thickness;
                        } else {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF) +
                                            " is required and not given.");
                            continue;
                        }
                        alpF++;

                        if (lNumericFieldBlanks(numF)) {
                            block.depth = 0.0; // Temporary indicator to default to foundation depth
                        } else {
                            block.depth = rNumericArgs(numF);
                        }
                        numF++;

                        if (lNumericFieldBlanks(numF)) {
                            ErrorsFound = true;
                            ShowSevereError(state, cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                            cNumericFieldNames(numF) + "provided");
                            continue;
                        } else {
                            block.x = rNumericArgs(numF);
                        }
                        numF++;

                        if (lNumericFieldBlanks(numF)) {
                            block.z = 0.0;
                        } else {
                            block.z = rNumericArgs(numF);
                        }
                        numF++;

                        fnd.inputBlocks.push_back(block);
                    }
                }

                state.dataSurfaceGeometry->kivaManager.foundationInputs.push_back(fndInput);
            }
        }
    }

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
        using namespace DataIPShortCuts;

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

        cCurrentModuleObject = "SurfaceProperty:OtherSideCoefficients";
        TotOSC = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        OSC.allocate(TotOSC);

        OSCNum = 0;
        for (Loop = 1; Loop <= TotOSC; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumProps,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            ErrorInName = false;
            IsBlank = false;
            UtilityRoutines::VerifyName(state, cAlphaArgs(1), OSC, OSCNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ErrorsFound = true;
                continue;
            }

            ++OSCNum;
            OSC(OSCNum).Name = cAlphaArgs(1);
            OSC(OSCNum).SurfFilmCoef = rNumericArgs(1);
            OSC(OSCNum).ConstTemp = rNumericArgs(2);     //  This will be replaced if  schedule is used
            OSC(OSCNum).ConstTempCoef = rNumericArgs(3); //  This multiplier is used (even with schedule).  It should normally be 1.0
            OSC(OSCNum).ExtDryBulbCoef = rNumericArgs(4);
            OSC(OSCNum).GroundTempCoef = rNumericArgs(5);
            OSC(OSCNum).WindSpeedCoef = rNumericArgs(6);
            OSC(OSCNum).ZoneAirTempCoef = rNumericArgs(7);
            OSC(OSCNum).SinusoidPeriod = rNumericArgs(8);

            if ((!lAlphaFieldBlanks(2)) && (NumAlphas != 1)) { //  Const temp will come from schedule specified below.
                OSC(OSCNum).ConstTempScheduleName = cAlphaArgs(2);
                if (!OSC(OSCNum).ConstTempScheduleName.empty()) {
                    OSC(OSCNum).ConstTempScheduleIndex = GetScheduleIndex(state, OSC(OSCNum).ConstTempScheduleName);
                    if (OSC(OSCNum).ConstTempScheduleIndex == 0) {
                        ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                        ErrorsFound = true;
                    }
                }
            }

            if (!lAlphaFieldBlanks(3)) {

                if (UtilityRoutines::SameString(cAlphaArgs(3), "No")) {
                    OSC(OSCNum).SinusoidalConstTempCoef = false;
                } else if (UtilityRoutines::SameString(cAlphaArgs(3), "Yes")) {
                    OSC(OSCNum).SinusoidalConstTempCoef = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }

            if (rNumericArgs(1) > 0.0 && !any_ne(rNumericArgs({3, 7}), 0.0) && (!OSC(OSCNum).SinusoidalConstTempCoef)) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has zeros for all coefficients.");
                ShowContinueError(state, "...The outdoor air temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            if (rNumericArgs(1) <= 0.0 && !any_ne(rNumericArgs({3, 7}), 0.0) && (!OSC(OSCNum).SinusoidalConstTempCoef)) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has zeros for all coefficients.");
                ShowContinueError(state, "...The outside surface temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            OSC(OSCNum).TPreviousCoef = rNumericArgs(9);

            if (!lNumericFieldBlanks(10)) {
                OSC(OSCNum).MinLimitPresent = true;
                OSC(OSCNum).MinTempLimit = rNumericArgs(10);
                cOSCLimitsString = format("{:.3R}", rNumericArgs(10));
            } else {
                cOSCLimitsString = "N/A";
            }
            if (!lNumericFieldBlanks(11)) {
                OSC(OSCNum).MaxLimitPresent = true;
                OSC(OSCNum).MaxTempLimit = rNumericArgs(11);
                cOSCLimitsString += format(",{:.3R}", rNumericArgs(10));
            } else {
                cOSCLimitsString += ",N/A";
            }
        }

        for (Loop = 1; Loop <= TotOSC; ++Loop) {
            if (Loop == 1) {
                static constexpr auto OSCFormat1(
                    "! <Other Side Coefficients>,Name,Combined convective/radiative film coefficient {W/m2-K},User selected "
                    "Constant Temperature {C},Coefficient modifying the constant temperature term,Coefficient modifying the external "
                    "dry bulb temperature term,Coefficient modifying the ground temperature term,Coefficient modifying the wind speed "
                    "term {s/m},Coefficient modifying the zone air temperature term,Constant Temperature Schedule Name,Sinusoidal "
                    "Variation,Period of Sinusoidal Variation,Previous Other Side Temperature Coefficient,Minimum Other Side "
                    "Temperature {C},Maximum Other Side Temperature {C}");
                print(state.files.eio, "{}\n", OSCFormat1);
            }
            if (OSC(Loop).SurfFilmCoef > 0.0) {
                cAlphaArgs(1) = format("{:.3R}", OSC(Loop).SurfFilmCoef);
                SetupOutputVariable(state, "Surface Other Side Coefficients Exterior Air Drybulb Temperature",
                                    OutputProcessor::Unit::C,
                                    OSC(Loop).OSCTempCalc,
                                    "System",
                                    "Average",
                                    OSC(Loop).Name);
            } else {
                cAlphaArgs(1) = "N/A";
            }
            if (OSC(Loop).ConstTempScheduleIndex != 0) {
                cAlphaArgs(2) = OSC(Loop).ConstTempScheduleName;
                constexpr auto format{"Other Side Coefficients,{},{},{},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{},{},{:.3R},{:.3R},{}\n"};
                print(state.files.eio,
                      format,
                      OSC(Loop).Name,
                      cAlphaArgs(1),
                      "N/A",
                      OSC(Loop).ConstTempCoef,
                      OSC(Loop).ExtDryBulbCoef,
                      OSC(Loop).GroundTempCoef,
                      OSC(Loop).WindSpeedCoef,
                      OSC(Loop).ZoneAirTempCoef,
                      cAlphaArgs(2),
                      cAlphaArgs(3),
                      OSC(Loop).SinusoidPeriod,
                      OSC(Loop).TPreviousCoef,
                      cOSCLimitsString);
            } else {
                cAlphaArgs(2) = "N/A";
                constexpr auto format{"Other Side Coefficients,{},{},{:.2R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{},{},{:.3R},{:.3R},{}\n"};
                print(state.files.eio,
                      format,
                      OSC(Loop).Name,
                      cAlphaArgs(1),
                      OSC(Loop).ConstTemp,
                      OSC(Loop).ConstTempCoef,
                      OSC(Loop).ExtDryBulbCoef,
                      OSC(Loop).GroundTempCoef,
                      OSC(Loop).WindSpeedCoef,
                      OSC(Loop).ZoneAirTempCoef,
                      cAlphaArgs(2),
                      cAlphaArgs(3),
                      OSC(Loop).SinusoidPeriod,
                      OSC(Loop).TPreviousCoef,
                      cOSCLimitsString);
            }
        }
    }

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
        using namespace DataIPShortCuts;


        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumProps;
        int Loop;
        int IOStat;
        int OSCMNum;
        bool ErrorInName;
        bool IsBlank;

        cCurrentModuleObject = "SurfaceProperty:OtherSideConditionsModel";
        TotOSCM = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        OSCM.allocate(TotOSCM);
        // OSCM is already initialized in derived type defn.

        OSCMNum = 0;
        for (Loop = 1; Loop <= TotOSCM; ++Loop) {
            inputProcessor->getObjectItem(state, cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumProps, IOStat);
            ErrorInName = false;
            IsBlank = false;
            UtilityRoutines::VerifyName(state, cAlphaArgs(1), OSCM, OSCMNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ErrorsFound = true;
                continue;
            }

            ++OSCMNum;
            OSCM(OSCMNum).Name = cAlphaArgs(1);
            // Note no validation of the below at this time:
            OSCM(OSCMNum).Class = cAlphaArgs(2);
            // setup output vars for modeled coefficients
            SetupOutputVariable(state, "Surface Other Side Conditions Modeled Convection Air Temperature",
                                OutputProcessor::Unit::C,
                                OSCM(OSCMNum).TConv,
                                "System",
                                "Average",
                                OSCM(OSCMNum).Name);
            SetupOutputVariable(state, "Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                OSCM(OSCMNum).HConv,
                                "System",
                                "Average",
                                OSCM(OSCMNum).Name);
            SetupOutputVariable(state, "Surface Other Side Conditions Modeled Radiation Temperature",
                                OutputProcessor::Unit::C,
                                OSCM(OSCMNum).TRad,
                                "System",
                                "Average",
                                OSCM(OSCMNum).Name);
            SetupOutputVariable(state, "Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient",
                                OutputProcessor::Unit::W_m2K,
                                OSCM(OSCMNum).HRad,
                                "System",
                                "Average",
                                OSCM(OSCMNum).Name);

            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state, "Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Convection Bulk Air Temperature",
                                 "[C]",
                                 OSCM(OSCMNum).EMSOverrideOnTConv,
                                 OSCM(OSCMNum).EMSOverrideTConvValue);
                SetupEMSActuator(state, "Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Convection Heat Transfer Coefficient",
                                 "[W/m2-K]",
                                 OSCM(OSCMNum).EMSOverrideOnHConv,
                                 OSCM(OSCMNum).EMSOverrideHConvValue);
                SetupEMSActuator(state, "Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Radiation Effective Temperature",
                                 "[C]",
                                 OSCM(OSCMNum).EMSOverrideOnTRad,
                                 OSCM(OSCMNum).EMSOverrideTRadValue);
                SetupEMSActuator(state, "Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Radiation Linear Heat Transfer Coefficient",
                                 "[W/m2-K]",
                                 OSCM(OSCMNum).EMSOverrideOnHrad,
                                 OSCM(OSCMNum).EMSOverrideHradValue);
            }
        }

        for (Loop = 1; Loop <= TotOSCM; ++Loop) {
            if (Loop == 1) {
                static constexpr auto OSCMFormat1("! <Other Side Conditions Model>,Name,Class\n");
                print(state.files.eio, OSCMFormat1);
            }
            print(state.files.eio, "Other Side Conditions Model,{},{}\n", OSCM(Loop).Name, OSCM(Loop).Class);
        }
    }

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
        using namespace DataIPShortCuts;

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
        int InslType;

        cCurrentModuleObject = "SurfaceControl:MovableInsulation";
        NMatInsul = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        for (Loop = 1; Loop <= NMatInsul; ++Loop) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          Loop,
                                          cAlphaArgs,
                                          NAlphas,
                                          rNumericArgs,
                                          NNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataSurfaceGeometry->SurfaceTmp, TotSurfaces);
            MaterNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataMaterial->Material, TotMaterials);
            SchNum = GetScheduleIndex(state, cAlphaArgs(4));
            if (UtilityRoutines::SameString(cAlphaArgs(1), "Outside")) {
                InslType = 1;
            } else if (UtilityRoutines::SameString(cAlphaArgs(1), "Inside")) {
                InslType = 2;
            } else {
                InslType = 0;
                ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                ShowContinueError(state, " invalid " + cAlphaFieldNames(1) + "=\"" + cAlphaArgs(1) + "\", [should be Inside or Outside]");
                ErrorsFound = true;
            }
            if (SurfNum == 0) {
                ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                ShowContinueError(state, " invalid (not found) " + cAlphaFieldNames(2));
                ErrorsFound = true;
            } else {
                if (MaterNum == 0) {
                    ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                    ShowContinueError(state, " invalid (not found) " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
                    ErrorsFound = true;
                } else {
                    int const MaterialLayerGroup = state.dataMaterial->Material(MaterNum).Group;
                    if ((MaterialLayerGroup == WindowSimpleGlazing) || (MaterialLayerGroup == ShadeEquivalentLayer) ||
                        (MaterialLayerGroup == DrapeEquivalentLayer) || (MaterialLayerGroup == BlindEquivalentLayer) ||
                        (MaterialLayerGroup == ScreenEquivalentLayer) || (MaterialLayerGroup == GapEquivalentLayer)) {
                        ShowSevereError(state, "Invalid movable insulation material for " + cCurrentModuleObject + ":");
                        ShowSevereError(state, "...Movable insulation material type specified = " + DataHeatBalance::cMaterialGroupType(MaterialLayerGroup));
                        ShowSevereError(state, "...Movable insulation material name specified = " + cAlphaArgs(3));
                        ErrorsFound = true;
                    }
                    if (SchNum == 0) {
                        ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                        ShowContinueError(state, " invalid (not found) " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                        ErrorsFound = true;
                    } else {
                        {
                            auto const SELECT_CASE_var(InslType);
                            if (SELECT_CASE_var == 1) {
                                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulExt > 0) {
                                    ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                    "\", already assigned.");
                                    ShowContinueError(state, "\"Outside\", was already assigned Material=\"" +
                                                      state.dataMaterial->Material(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt).Name + "\".");
                                    ShowContinueError(state, "attempting to assign Material=\"" + state.dataMaterial->Material(MaterNum).Name + "\".");
                                    ErrorsFound = true;
                                }
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulExt = MaterNum;
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMovInsulExt = SchNum;
                                if (state.dataMaterial->Material(MaterNum).Resistance <= 0.0) {
                                    if (state.dataMaterial->Material(MaterNum).Conductivity <= 0.0 || state.dataMaterial->Material(MaterNum).Thickness <= 0.0) {
                                        ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError(state, "\"Outside\", invalid material for movable insulation.");
                                        ShowContinueError(state,
                                                          format("Material=\"{}\",Resistance=[{:.3R}], must be > 0 for use in Movable Insulation.",
                                                                 state.dataMaterial->Material(MaterNum).Name,
                                                                 state.dataMaterial->Material(MaterNum).Resistance));
                                        ErrorsFound = true;
                                    } else if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                                        state.dataMaterial->Material(MaterNum).Resistance = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                                    }
                                }
                                if (state.dataMaterial->Material(MaterNum).Conductivity <= 0.0) {
                                    if (state.dataMaterial->Material(MaterNum).Resistance <= 0.0) {
                                        ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError(state, "\"Outside\", invalid material for movable insulation.");
                                        ShowContinueError(state,
                                                          format("Material=\"{}\",Conductivity=[{:.3R}], must be > 0 for use in Movable Insulation.",
                                                                 state.dataMaterial->Material(MaterNum).Name,
                                                                 state.dataMaterial->Material(MaterNum).Conductivity));
                                        ErrorsFound = true;
                                    }
                                }
                            } else if (SELECT_CASE_var == 2) {
                                if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt > 0) {
                                    ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                    "\", already assigned.");
                                    ShowContinueError(state, "\"Inside\", was already assigned Material=\"" +
                                                      state.dataMaterial->Material(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt).Name + "\".");
                                    ShowContinueError(state, "attempting to assign Material=\"" + state.dataMaterial->Material(MaterNum).Name + "\".");
                                    ErrorsFound = true;
                                }
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt = MaterNum;
                                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMovInsulInt = SchNum;
                                if (state.dataMaterial->Material(MaterNum).Resistance <= 0.0) {
                                    if (state.dataMaterial->Material(MaterNum).Conductivity <= 0.0 || state.dataMaterial->Material(MaterNum).Thickness <= 0.0) {
                                        ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError(state, "\"Inside\", invalid material for movable insulation.");
                                        ShowContinueError(state,
                                                          format("Material=\"{}\",Resistance=[{:.3R}], must be > 0 for use in Movable Insulation.",
                                                                 state.dataMaterial->Material(MaterNum).Name,
                                                                 state.dataMaterial->Material(MaterNum).Resistance));
                                        ErrorsFound = true;
                                    } else if (state.dataMaterial->Material(MaterNum).Conductivity > 0.0) {
                                        state.dataMaterial->Material(MaterNum).Resistance = state.dataMaterial->Material(MaterNum).Thickness / state.dataMaterial->Material(MaterNum).Conductivity;
                                    }
                                }
                            } else {
                            }
                        }
                        if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Window) {
                            ShowSevereError(state, cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
                            ShowContinueError(state, "invalid use on a Window. Use WindowShadingControl instead.");
                            ErrorsFound = true;
                        }
                    }
                }
            }
        }
    }

    // Calculates the volume (m3) of a zone using the surfaces as possible.
    void CalculateZoneVolume(EnergyPlusData &state, const Array1D_bool &CeilingHeightEntered)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN   1992-1994
        //       MODIFIED       Sep 2007, Mar 2017
        //       RE-ENGINEERED  na

        // METHODOLOGY EMPLOYED:
        // Uses surface area information for calculations.  Modified to use the
        // user-entered ceiling height (x floor area, if applicable) instead of using
        // the calculated volume when the user enters the ceiling height.

        // REFERENCES:
        // Legacy Code (IBLAST)

        // Using/Aliasing
        using namespace Vectors;

        // SUBROUTINE PARAMETER DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 SumAreas;  // Sum of the Zone surface areas that are not "internal mass"
        Real64 SurfCount; // Surface Count
        int SurfNum;      // Loop counter for surfaces
        int ZoneNum;      // Loop counter for Zones
        Array1D_int surfacenotused;
        int notused;
        int NFaces;
        int NActFaces;
        Real64 CalcVolume;
        bool initmsg;
        int iside;
        static bool ShowZoneSurfaces(false);
        static bool ShowZoneSurfaceHeaders(true);
        static int ErrCount(0);

        // Object Data
        Polyhedron ZoneStruct;

        initmsg = true;
        ShowZoneSurfaces = (inputProcessor->getNumSectionsFound("SHOWZONESURFACES_DEBUG") > 0);

        enum class zoneVolumeCalculationMethod
        {
            enclosed,
            floorAreaTimesHeight1,
            floorAreaTimesHeight2,
            ceilingAreaTimesHeight,
            opWallAreaTimesDistance,
            userProvided,
            error
        };

        int countNotFullyEnclosedZones = 0;
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

            if (!Zone(ZoneNum).HasFloor) {
                ShowWarningError(state, "No floor exists in Zone=\"" + Zone(ZoneNum).Name +
                                 "\", zone floor area is zero. All values for this zone that are entered per floor area will be zero.");
            }

            SumAreas = 0.0;
            SurfCount = 0.0;
            // Use AllSurfaceFirst which includes air boundaries
            NFaces = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).AllSurfaceFirst + 1;
            notused = 0;
            ZoneStruct.NumSurfaceFaces = NFaces;
            ZoneStruct.SurfaceFace.allocate(NFaces);
            NActFaces = 0;
            surfacenotused.dimension(NFaces, 0);

            for (SurfNum = Zone(ZoneNum).AllSurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {

                // Only include Base Surfaces in Calc.

                if (Surface(SurfNum).Class != SurfaceClass::Wall && Surface(SurfNum).Class != SurfaceClass::Floor &&
                    Surface(SurfNum).Class != SurfaceClass::Roof) {
                    ++notused;
                    surfacenotused(notused) = SurfNum;
                    continue;
                }

                ++NActFaces;
                ZoneStruct.SurfaceFace(NActFaces).FacePoints.allocate(Surface(SurfNum).Sides);
                ZoneStruct.SurfaceFace(NActFaces).NSides = Surface(SurfNum).Sides;
                ZoneStruct.SurfaceFace(NActFaces).SurfNum = SurfNum;
                ZoneStruct.SurfaceFace(NActFaces).FacePoints({1, Surface(SurfNum).Sides}) = Surface(SurfNum).Vertex({1, Surface(SurfNum).Sides});
                CreateNewellAreaVector(ZoneStruct.SurfaceFace(NActFaces).FacePoints,
                                       ZoneStruct.SurfaceFace(NActFaces).NSides,
                                       ZoneStruct.SurfaceFace(NActFaces).NewellAreaVector);
                SumAreas += VecLength(ZoneStruct.SurfaceFace(NActFaces).NewellAreaVector);
            }
            ZoneStruct.NumSurfaceFaces = NActFaces;
            SurfCount = double(NActFaces);

            bool isFloorHorizontal;
            bool isCeilingHorizontal;
            bool areWallsVertical;
            std::tie(isFloorHorizontal, isCeilingHorizontal, areWallsVertical) = areSurfaceHorizAndVert(ZoneStruct);
            Real64 oppositeWallArea;
            Real64 distanceBetweenOppositeWalls;

            bool areWallsSameHeight = areWallHeightSame(ZoneStruct);

            std::vector<EdgeOfSurf> listOfedgeNotUsedTwice;
            bool isZoneEnclosed = isEnclosedVolume(ZoneStruct, listOfedgeNotUsedTwice);
            zoneVolumeCalculationMethod volCalcMethod;

            if (isZoneEnclosed) {
                CalcVolume = CalcPolyhedronVolume(ZoneStruct);
                volCalcMethod = zoneVolumeCalculationMethod::enclosed;
            } else if (Zone(ZoneNum).FloorArea > 0.0 && Zone(ZoneNum).CeilingHeight > 0.0 && areFloorAndCeilingSame(ZoneStruct)) {
                CalcVolume = Zone(ZoneNum).FloorArea * Zone(ZoneNum).CeilingHeight;
                volCalcMethod = zoneVolumeCalculationMethod::floorAreaTimesHeight1;
            } else if (isFloorHorizontal && areWallsVertical && areWallsSameHeight && Zone(ZoneNum).FloorArea > 0.0 &&
                       Zone(ZoneNum).CeilingHeight > 0.0) {
                CalcVolume = Zone(ZoneNum).FloorArea * Zone(ZoneNum).CeilingHeight;
                volCalcMethod = zoneVolumeCalculationMethod::floorAreaTimesHeight2;
            } else if (isCeilingHorizontal && areWallsVertical && areWallsSameHeight && Zone(ZoneNum).CeilingArea > 0.0 &&
                       Zone(ZoneNum).CeilingHeight > 0.0) {
                CalcVolume = Zone(ZoneNum).CeilingArea * Zone(ZoneNum).CeilingHeight;
                volCalcMethod = zoneVolumeCalculationMethod::ceilingAreaTimesHeight;
            } else if (areOppositeWallsSame(ZoneStruct, oppositeWallArea, distanceBetweenOppositeWalls)) {
                CalcVolume = oppositeWallArea * distanceBetweenOppositeWalls;
                volCalcMethod = zoneVolumeCalculationMethod::opWallAreaTimesDistance;
            } else if (Zone(ZoneNum).Volume == DataGlobalConstants::AutoCalculate) { // no user entered zone volume
                ShowSevereError(state, "For zone: " + Zone(ZoneNum).Name +
                                " it is not possible to calculate the volume from the surrounding surfaces so either provide the volume value or "
                                "define all the surfaces to fully enclose the zone.");
                CalcVolume = 0.;
                volCalcMethod = zoneVolumeCalculationMethod::error;
            } else {
                CalcVolume = 0.;
                volCalcMethod = zoneVolumeCalculationMethod::userProvided;
            }
            if (!isZoneEnclosed) {
                ++countNotFullyEnclosedZones;
                if (state.dataGlobal->DisplayExtraWarnings) { // report missing
                    ShowWarningError(state,
                        "CalculateZoneVolume: The Zone=\"" + Zone(ZoneNum).Name +
                        "\" is not fully enclosed. To be fully enclosed, each edge of a surface must also be an edge on one other surface.");
                    switch (volCalcMethod) {
                    case zoneVolumeCalculationMethod::floorAreaTimesHeight1:
                        ShowContinueError(state, "  The zone volume was calculated using the floor area times ceiling height method where the floor and "
                                          "ceiling are the same except for the z-coordinates.");
                        break;
                    case zoneVolumeCalculationMethod::floorAreaTimesHeight2:
                        ShowContinueError(state, "  The zone volume was calculated using the floor area times ceiling height method where the floor is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case zoneVolumeCalculationMethod::ceilingAreaTimesHeight:
                        ShowContinueError(state, "  The zone volume was calculated using the ceiling area times ceiling height method where the ceiling is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case zoneVolumeCalculationMethod::opWallAreaTimesDistance:
                        ShowContinueError(state, "  The zone volume was calculated using the opposite wall area times the distance between them method ");
                        break;
                    case zoneVolumeCalculationMethod::userProvided:
                        ShowContinueError(state, "  The zone volume was provided as an input to the ZONE object ");
                        break;
                    case zoneVolumeCalculationMethod::error:
                        ShowContinueError(state, "  The zone volume was not calculated and an error exists. ");
                        break;
                    case zoneVolumeCalculationMethod::enclosed: // should not be called but completes enumeration
                        ShowContinueError(state, "  The zone volume was calculated using multiple pyramids and was fully enclosed. ");
                        break;
                    }
                    for (auto edge : listOfedgeNotUsedTwice) {
                        ShowContinueError(state, "  The surface    \"" + Surface(edge.surfNum).Name +
                                          "\" has an edge that is either not an edge on another surface or is an edge on three or more surfaces: ");
                        ShowContinueError(state, format("    Vertex start {{ {:.4R}, {:.4R}, {:.4R}}}", edge.start.x, edge.start.y, edge.start.z));
                        ShowContinueError(state, format("    Vertex end   {{ {:.4R}, {:.4R}, {:.4R}}}", edge.end.x, edge.end.y, edge.end.z));
                    }
                }
            }
            if (Zone(ZoneNum).Volume > 0.0) { // User entered zone volume, produce message if not near calculated
                if (CalcVolume > 0.0) {
                    if (std::abs(CalcVolume - Zone(ZoneNum).Volume) / Zone(ZoneNum).Volume > 0.05) {
                        ++ErrCount;
                        if (ErrCount == 1 && !state.dataGlobal->DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage(state, "Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            ShowWarningError(state, "Entered Zone Volumes differ from calculated zone volume(s).");
                            ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                        }
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage(state, "Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            // Warn user of using specified Zone Volume
                            ShowWarningError(state, "Entered Volume entered for Zone=\"" + Zone(ZoneNum).Name +
                                             "\" significantly different from calculated Volume");
                            ShowContinueError(state,
                                              format("Entered Zone Volume value={:.2R}, Calculated Zone Volume value={:.2R}, entered volume will be "
                                                     "used in calculations.",
                                                     Zone(ZoneNum).Volume,
                                                     CalcVolume));
                        }
                    }
                }
            } else if (CeilingHeightEntered(ZoneNum)) { // User did not enter zone volume, but entered ceiling height
                if (Zone(ZoneNum).FloorArea > 0.0) {
                    Zone(ZoneNum).Volume = Zone(ZoneNum).FloorArea * Zone(ZoneNum).CeilingHeight;
                } else { // ceiling height entered but floor area zero
                    Zone(ZoneNum).Volume = CalcVolume;
                }
            } else { // Neither ceiling height nor volume entered
                Zone(ZoneNum).Volume = CalcVolume;
            }

            if (Zone(ZoneNum).Volume <= 0.0) {
                ShowWarningError(state, "Indicated Zone Volume <= 0.0 for Zone=" + Zone(ZoneNum).Name);
                ShowContinueError(state, format("The calculated Zone Volume was={:.2R}", Zone(ZoneNum).Volume));
                ShowContinueError(state, "The simulation will continue with the Zone Volume set to 10.0 m3. ");
                ShowContinueError(state, "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                Zone(ZoneNum).Volume = 10.;
            }

            if (ShowZoneSurfaces) {
                if (ShowZoneSurfaceHeaders) {
                    print(state.files.debug, "{}\n", "===================================");
                    print(state.files.debug, "{}\n", "showing zone surfaces used and not used in volume calculation");
                    print(state.files.debug, "{}\n", "for volume calculation, only floors, walls and roofs/ceilings are used");
                    print(state.files.debug, "{}\n", "surface class, 1=wall, 2=floor, 3=roof/ceiling");
                    print(state.files.debug, "{}\n", "unused surface class(es), 5=internal mass, 11=window, 12=glass door");
                    print(state.files.debug, "{}\n", "                          13=door, 14=shading, 15=overhang, 16=fin");
                    print(state.files.debug, "{}\n", "                          17=TDD Dome, 18=TDD Diffuser");
                    ShowZoneSurfaceHeaders = false;
                }
                print(state.files.debug, "{}\n", "===================================");
                print(state.files.debug, "zone={} calc volume={}\n", Zone(ZoneNum).Name, CalcVolume);
                print(state.files.debug, " nsurfaces={} nactual={}\n", NFaces, NActFaces);
            }
            for (SurfNum = 1; SurfNum <= ZoneStruct.NumSurfaceFaces; ++SurfNum) {
                if (ShowZoneSurfaces) {
                    if (SurfNum <= NActFaces) {
                        print(state.files.debug,
                             "surface={} nsides={}\n", ZoneStruct.SurfaceFace(SurfNum).SurfNum, ZoneStruct.SurfaceFace(SurfNum).NSides);
                        print(state.files.debug, "surface name={} class={}\n", Surface(ZoneStruct.SurfaceFace(SurfNum).SurfNum).Name
                                                                      , Surface(ZoneStruct.SurfaceFace(SurfNum).SurfNum).Class);
                        print(state.files.debug, "area={}\n", Surface(ZoneStruct.SurfaceFace(SurfNum).SurfNum).GrossArea);
                        for (iside = 1; iside <= ZoneStruct.SurfaceFace(SurfNum).NSides; ++iside) {
                            auto const &FacePoint(ZoneStruct.SurfaceFace(SurfNum).FacePoints(iside));
                            print(state.files.debug, "{} {} {}\n", FacePoint.x, FacePoint.y, FacePoint.z);
                        }
                    }
                }
                ZoneStruct.SurfaceFace(SurfNum).FacePoints.deallocate();
            }
            if (ShowZoneSurfaces) {
                for (SurfNum = 1; SurfNum <= notused; ++SurfNum) {
                    print(state.files.debug,
                          "notused:surface={} name={} class={}\n",
                          surfacenotused(SurfNum),
                          Surface(surfacenotused(SurfNum)).Name,
                          Surface(surfacenotused(SurfNum)).Class);
                }
            }

            ZoneStruct.SurfaceFace.deallocate();
            surfacenotused.deallocate();

        } // zone loop
        if (!state.dataGlobal->DisplayExtraWarnings) {
            if (countNotFullyEnclosedZones == 1) {
                ShowWarningError(state,
                    "CalculateZoneVolume: 1 zone is not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; ");
            } else if (countNotFullyEnclosedZones > 1) {
                ShowWarningError(
                    state,
                    format("CalculateZoneVolume: {} zones are not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; ",
                           countNotFullyEnclosedZones));
            }
        }
    }

    // test if the volume described by the polyhedron if full enclosed (would not leak)
    bool isEnclosedVolume(DataVectorTypes::Polyhedron const &zonePoly, std::vector<EdgeOfSurf> &edgeNot2)
    {
        // J. Glazer - March 2017

        std::vector<Vector> uniqueVertices;
        makeListOfUniqueVertices(zonePoly, uniqueVertices);

        std::vector<EdgeOfSurf> edgeNot2orig = edgesNotTwoForEnclosedVolumeTest(zonePoly, uniqueVertices);

        // if all edges had two counts then it is fully enclosed
        if (edgeNot2orig.size() == size_t(0)) {
            edgeNot2 = edgeNot2orig;
            return true;
        } else { // if the count is three or greater it is likely that a vertex that is colinear was counted on the faces on one edge and not
                 // on the "other side" of the edge Go through all the points looking for the number that are colinear and see if that is
                 // consistent with the number of edges found that didn't have a count of two
            DataVectorTypes::Polyhedron updatedZonePoly = updateZonePolygonsForMissingColinearPoints(
                zonePoly, uniqueVertices); // this is done after initial test since it is computationally intensive.
            std::vector<EdgeOfSurf> edgeNot2again = edgesNotTwoForEnclosedVolumeTest(updatedZonePoly, uniqueVertices);
            if (edgeNot2again.size() == size_t(0)) {
                return true;
            } else {
                edgeNot2 = edgesInBoth(edgeNot2orig,
                                       edgeNot2again); // only return a list of those edges that appear in both the original edge and the
                                                       // revised edges this eliminates added edges that will confuse users and edges that
                                                       // were caught by the updateZonePoly routine
                return false;
            }
        }
    }

    // returns a vector of edges that are in both vectors
    std::vector<EdgeOfSurf> edgesInBoth(std::vector<EdgeOfSurf> edges1, std::vector<EdgeOfSurf> edges2)
    {
        // J. Glazer - June 2017
        // this is not optimized but the number of edges for a typical polyhedron is 12 and is probably rarely bigger than 20.

        std::vector<EdgeOfSurf> inBoth;
        for (auto e1 : edges1) {
            for (auto e2 : edges2) {
                if (edgesEqualOnSameSurface(e1, e2)) {
                    inBoth.push_back(e1);
                    break;
                }
            }
        }
        return inBoth;
    }

    // returns true if the edges match - including the surface number
    bool edgesEqualOnSameSurface(EdgeOfSurf a, EdgeOfSurf b)
    {
        if (a.surfNum == b.surfNum) {
            if (a.start == b.start && a.end == b.end) { // vertex comparison
                return true;
            } else if (a.start == b.end && a.end == b.start) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    // returns the number of times the edges of the polyhedron of the zone are not used twice by the sides
    std::vector<EdgeOfSurf> edgesNotTwoForEnclosedVolumeTest(DataVectorTypes::Polyhedron const &zonePoly, std::vector<Vector> const &uniqueVertices)
    {
        // J. Glazer - March 2017

        using DataVectorTypes::Vector;

        struct EdgeByPts
        {
            int start;
            int end;
            int count;
            int firstSurfNum;
            EdgeByPts() : start(0), end(0), count(0), firstSurfNum(0)
            {
            }
        };
        std::vector<EdgeByPts> uniqueEdges;
        uniqueEdges.reserve(zonePoly.NumSurfaceFaces * 6);

        // construct list of unique edges
        Vector curVertex;
        int curVertexIndex;
        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            Vector prevVertex;
            int prevVertexIndex;
            for (int jVertex = 1; jVertex <= zonePoly.SurfaceFace(iFace).NSides; ++jVertex) {
                if (jVertex == 1) {
                    prevVertex = zonePoly.SurfaceFace(iFace).FacePoints(zonePoly.SurfaceFace(iFace).NSides); // the last point
                    prevVertexIndex = findIndexOfVertex(prevVertex, uniqueVertices);
                } else {
                    prevVertex = curVertex;
                    prevVertexIndex = curVertexIndex;
                }
                curVertex = zonePoly.SurfaceFace(iFace).FacePoints(jVertex);
                curVertexIndex = findIndexOfVertex(curVertex, uniqueVertices);
                int found = -1;
                for (std::size_t i = 0; i < uniqueEdges.size(); i++) {
                    if ((uniqueEdges[i].start == curVertexIndex && uniqueEdges[i].end == prevVertexIndex) ||
                        (uniqueEdges[i].start == prevVertexIndex && uniqueEdges[i].end == curVertexIndex)) {
                        found = i;
                        break;
                    }
                }
                if (found == -1) {
                    EdgeByPts curEdge;
                    curEdge.start = prevVertexIndex;
                    curEdge.end = curVertexIndex;
                    curEdge.count = 1;
                    curEdge.firstSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
                    uniqueEdges.emplace_back(curEdge);
                } else {
                    ++uniqueEdges[found].count;
                }
            }
        }
        // All edges for an enclosed polyhedron should be shared by two (and only two) sides.
        // So if the count is not two for all edges, the polyhedron is not enclosed
        std::vector<EdgeOfSurf> edgesNotTwoCount;
        for (auto anEdge : uniqueEdges) {
            if (anEdge.count != 2) {
                EdgeOfSurf curEdgeOne;
                curEdgeOne.surfNum = anEdge.firstSurfNum;
                curEdgeOne.start = uniqueVertices[anEdge.start];
                curEdgeOne.end = uniqueVertices[anEdge.end];
                edgesNotTwoCount.push_back(curEdgeOne);
            }
        }
        return edgesNotTwoCount;
    }

    // create a list of unique vertices given the polyhedron describing the zone
    void makeListOfUniqueVertices(DataVectorTypes::Polyhedron const &zonePoly, std::vector<Vector> &uniqVertices)
    {
        // J. Glazer - March 2017

        using DataVectorTypes::Vector;
        uniqVertices.clear();
        uniqVertices.reserve(zonePoly.NumSurfaceFaces * 6);

        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            for (int jVertex = 1; jVertex <= zonePoly.SurfaceFace(iFace).NSides; ++jVertex) {
                Vector curVertex = zonePoly.SurfaceFace(iFace).FacePoints(jVertex);
                if (uniqVertices.size() == 0) {
                    uniqVertices.emplace_back(curVertex);
                } else {
                    bool found = false;
                    for (auto unqV : uniqVertices) {
                        if (isAlmostEqual3dPt(curVertex, unqV)) {
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
    }

    // updates the polyhedron used to describe a zone to include points on an edge that are between and collinear to points already describing
    // the edge
    DataVectorTypes::Polyhedron updateZonePolygonsForMissingColinearPoints(DataVectorTypes::Polyhedron const &zonePoly,
                                                                           std::vector<Vector> const &uniqVertices)
    {
        // J. Glazer - March 2017

        using DataVectorTypes::Vector;

        DataVectorTypes::Polyhedron updZonePoly = zonePoly; // set the return value to the original polyhedron describing the zone

        for (int iFace = 1; iFace <= updZonePoly.NumSurfaceFaces; ++iFace) {
            bool faceUpdated = false;
            DataVectorTypes::Face updFace = updZonePoly.SurfaceFace(iFace);
            for (int iterationLimiter = 0; iterationLimiter < 20;
                 ++iterationLimiter) { // could probably be while loop but want to make sure it does not get stuck
                bool insertedVertext = false;
                for (int curVertexIndex = updFace.NSides; curVertexIndex >= 1; --curVertexIndex) { // go through array from end
                    Vector curVertex = updFace.FacePoints(curVertexIndex);
                    Vector nextVertex;
                    int nextVertexIndex;
                    if (curVertexIndex == updFace.NSides) {
                        nextVertexIndex = 1;
                    } else {
                        nextVertexIndex = curVertexIndex + 1;
                    }
                    nextVertex = updFace.FacePoints(nextVertexIndex);
                    // now go through all the vertices and see if they are colinear with start and end vertices
                    bool found = false;
                    Vector foundIntermediateVertex;
                    for (auto testVertex : uniqVertices) {
                        if (!isAlmostEqual3dPt(curVertex, testVertex) && !isAlmostEqual3dPt(nextVertex, testVertex)) {
                            if (isPointOnLineBetweenPoints(curVertex, nextVertex, testVertex)) {
                                foundIntermediateVertex = testVertex;
                                found = true;
                            }
                        }
                    }
                    if (found) {
                        insertVertexOnFace(updFace, nextVertexIndex, foundIntermediateVertex);
                        faceUpdated = true;
                        insertedVertext = true;
                        break;
                    }
                }
                if (!insertedVertext) break;
            }
            if (faceUpdated) {
                updZonePoly.SurfaceFace(iFace) = updFace;
            }
        }
        return updZonePoly;
    }

    // inserts a vertex in the polygon describing the face (wall) of polyhedron (zone)
    void insertVertexOnFace(DataVectorTypes::Face &face,
                            int const &indexAt, // index of where to insert new vertex - remaining vertices are moved later
                            DataVectorTypes::Vector const &vertexToInsert)
    {
        // J. Glazer - March 2017

        if (indexAt >= 1 && indexAt <= face.NSides) {
            int origNumSides = face.NSides;
            DataVectorTypes::Vector emptyVector(0., 0., 0.);
            face.FacePoints.append(emptyVector); // just to add new item to the end of array
            for (int i = origNumSides + 1; i > indexAt; --i) {
                face.FacePoints(i) = face.FacePoints(i - 1); // move existing items one location further
            }
            face.FacePoints(indexAt) = vertexToInsert;
            ++face.NSides;
        }
    }

    // test if the ceiling and floor are the same except for their height difference by looking at the corners
    bool areFloorAndCeilingSame(DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // check if the floor and ceiling are the same
        // this is almost equivent to saying, if you ignore the z-coordinate, are the vertices the same
        // so if you could all the unique vertices of the floor and ceiling, ignoring the z-coordinate, they
        // should always be even (they would be two but you might define multiple surfaces that meet in a corner)

        using DataVectorTypes::Vector;
        using DataVectorTypes::Vector2dCount;
        using DataVectorTypes::Vector_2d;

        std::vector<Vector2dCount> floorCeilingXY;
        floorCeilingXY.reserve(zonePoly.NumSurfaceFaces * 6);

        // make list of x and y coordinates for all faces that are on the floor or ceiling
        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if (Surface(curSurfNum).Class == SurfaceClass::Floor || Surface(curSurfNum).Class == SurfaceClass::Roof) {
                for (int jVertex = 1; jVertex <= zonePoly.SurfaceFace(iFace).NSides; ++jVertex) {
                    Vector curVertex = zonePoly.SurfaceFace(iFace).FacePoints(jVertex);
                    Vector2dCount curXYc;
                    curXYc.x = curVertex.x;
                    curXYc.y = curVertex.y;
                    curXYc.count = 1;
                    bool found = false;
                    for (Vector2dCount &curFloorCeiling : floorCeilingXY) { // can't use just "auto" because updating floorCeilingXY
                        if (isAlmostEqual2dPt(curXYc, curFloorCeiling)) {   // count ignored in comparison
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
            for (auto curFloorCeiling : floorCeilingXY) {
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
    bool areWallHeightSame(DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // test if all the wall heights are the same (all walls have the same maximum z-coordinate

        bool areWlHgtSame = true;
        Real64 wallHeightZ = -1.0E50;
        bool foundWallHeight = false;
        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if (Surface(curSurfNum).Class == SurfaceClass::Wall) {
                Real64 maxZ = -1.0E50;
                for (int jVertex = 1; jVertex <= zonePoly.SurfaceFace(iFace).NSides; ++jVertex) {
                    Vector curVertex = zonePoly.SurfaceFace(iFace).FacePoints(jVertex);
                    if (maxZ < curVertex.z) {
                        maxZ = curVertex.z;
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
    }

    // tests if the floor is horizontal, ceiling is horizontal, and walls are vertical and returns all three as a tuple of booleans
    std::tuple<bool, bool, bool> areSurfaceHorizAndVert(DataVectorTypes::Polyhedron const &zonePoly)
    {
        // J. Glazer - March 2017

        // check if floors and ceilings are horizonatal and walls are vertical
        bool isFlrHoriz = true;
        bool isClgHoriz = true;
        bool areWlVert = true;
        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if (Surface(curSurfNum).Class == SurfaceClass::Floor) {
                if (std::abs(Surface(curSurfNum).Tilt - 180.) > 1.) { // with 1 degree angle
                    isFlrHoriz = false;
                }
            } else if (Surface(curSurfNum).Class == SurfaceClass::Roof) { // includes ceilings
                if (std::abs(Surface(curSurfNum).Tilt) > 1.) {           // with 1 degree angle of
                    isClgHoriz = false;
                }
            } else if (Surface(curSurfNum).Class == SurfaceClass::Wall) {
                if (std::abs(Surface(curSurfNum).Tilt - 90) > 1.) { // with 1 degree angle
                    areWlVert = false;
                }
            }
        }
        return std::make_tuple(isFlrHoriz, isClgHoriz, areWlVert);
    }

    // tests whether a pair of walls in the zone are the same except offset from one another and facing the opposite direction and also
    // returns the wall area and distance between
    bool areOppositeWallsSame(DataVectorTypes::Polyhedron const &zonePoly,
                              Real64 &oppositeWallArea,            // return the area of the wall that has an opposite wall
                              Real64 &distanceBetweenOppositeWalls // returns distance
    )
    {
        // J. Glazer - March 2017

        // approach: if opposite surfaces have opposite azimuth and same area, then check the distance between the
        // vertices( one counting backwards ) and if it is the same distance than assume that it is the same.
        using DataVectorTypes::Vector;
        bool foundOppEqual = false;
        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if (Surface(curSurfNum).Class == SurfaceClass::Wall) {
                std::vector<int> facesAtAz = listOfFacesFacingAzimuth(zonePoly, Surface(curSurfNum).Azimuth);
                bool allFacesEquidistant = true;
                oppositeWallArea = 0.;
                for (auto curFace : facesAtAz) {
                    int possOppFace = findPossibleOppositeFace(zonePoly, curFace);
                    if (possOppFace > 0) { // an opposite fact was found
                        oppositeWallArea += Surface(zonePoly.SurfaceFace(curFace).SurfNum).Area;
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
    std::vector<int> listOfFacesFacingAzimuth(DataVectorTypes::Polyhedron const &zonePoly, Real64 const &azimuth)
    {
        // J. Glazer - March 2017

        std::vector<int> facingAzimuth;
        facingAzimuth.reserve(zonePoly.NumSurfaceFaces);

        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if (std::abs(Surface(curSurfNum).Azimuth - azimuth) < 1.) {
                facingAzimuth.emplace_back(iFace);
            }
        }
        return facingAzimuth;
    }

    // returns the index of the face of a polyhedron that is probably opposite of the face index provided
    int findPossibleOppositeFace(DataVectorTypes::Polyhedron const &zonePoly, int const &faceIndex)
    {
        // J. Glazer - March 2017

        int selectedSurNum = zonePoly.SurfaceFace(faceIndex).SurfNum;
        Real64 selectedAzimuth = Surface(selectedSurNum).Azimuth;
        Real64 oppositeAzimuth = fmod(selectedAzimuth + 180., 360.);
        Real64 selectedArea = Surface(selectedSurNum).Area;
        int selectedNumCorners = zonePoly.SurfaceFace(faceIndex).NSides;
        int found = -1;

        for (int iFace = 1; iFace <= zonePoly.NumSurfaceFaces; ++iFace) {
            int curSurfNum = zonePoly.SurfaceFace(iFace).SurfNum;
            if ((zonePoly.SurfaceFace(iFace).NSides == selectedNumCorners) && (std::abs(Surface(curSurfNum).Area - selectedArea) < 0.01) &&
                (std::abs(Surface(curSurfNum).Azimuth - oppositeAzimuth) < 1.)) {
                found = iFace;
                break;
            }
        }
        return found;
    }

    // tests if the corners of one face of the polyhedron are the same distance from corners of another face
    bool areCornersEquidistant(DataVectorTypes::Polyhedron const &zonePoly, int const &faceIndex, int const &opFaceIndex, Real64 &distanceBetween)
    {
        // J. Glazer - March 2017

        Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        bool allAreEquidistant = true;
        Real64 firstDistance = -99.;
        if (zonePoly.SurfaceFace(faceIndex).NSides == zonePoly.SurfaceFace(opFaceIndex).NSides) { // double check that the number of sides match
            for (int iVertex = 1; iVertex <= zonePoly.SurfaceFace(faceIndex).NSides; ++iVertex) {
                int iVertexOpp = 1 + zonePoly.SurfaceFace(faceIndex).NSides - iVertex; // count backwards for opposite face
                Real64 curDistBetwCorners =
                    distance(zonePoly.SurfaceFace(faceIndex).FacePoints(iVertex), zonePoly.SurfaceFace(opFaceIndex).FacePoints(iVertexOpp));
                if (iVertex == 1) {
                    firstDistance = curDistBetwCorners;
                } else {
                    if (std::abs(curDistBetwCorners - firstDistance) > tol) {
                        allAreEquidistant = false;
                        break;
                    }
                }
            }
        } else {
            allAreEquidistant = false;
        }
        if (allAreEquidistant) distanceBetween = firstDistance;
        return allAreEquidistant;
    }

    // test if two points in space are in the same position based on a small tolerance
    bool isAlmostEqual3dPt(DataVectorTypes::Vector v1, DataVectorTypes::Vector v2)
    {
        // J. Glazer - March 2017

        Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        return ((std::abs(v1.x - v2.x) < tol) && (std::abs(v1.y - v2.y) < tol) && (std::abs(v1.z - v2.z) < tol));
    }

    // test if two points on a plane are in the same position based on a small tolerance
    bool isAlmostEqual2dPt(DataVectorTypes::Vector_2d v1, DataVectorTypes::Vector_2d v2)
    {
        // J. Glazer - March 2017

        Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        return ((std::abs(v1.x - v2.x) < tol) && (std::abs(v1.y - v2.y) < tol));
    }

    // test if two points on a plane are in the same position based on a small tolerance (based on Vector2dCount comparison)
    bool isAlmostEqual2dPt(DataVectorTypes::Vector2dCount v1, DataVectorTypes::Vector2dCount v2)
    {
        // J. Glazer - March 2017

        Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        return ((std::abs(v1.x - v2.x) < tol) && (std::abs(v1.y - v2.y) < tol));
    }

    // returns the index of vertex in a list that is in the same position in space as the given vertex
    int findIndexOfVertex(DataVectorTypes::Vector vertexToFind, std::vector<DataVectorTypes::Vector> listOfVertices)
    {
        // J. Glazer - March 2017

        for (std::size_t i = 0; i < listOfVertices.size(); i++) {
            if (isAlmostEqual3dPt(listOfVertices[i], vertexToFind)) {
                return i;
            }
        }
        return -1;
    }

    // returns the distance between two points in space
    Real64 distance(DataVectorTypes::Vector v1, DataVectorTypes::Vector v2)
    {
        // J. Glazer - March 2017

        return sqrt(pow(v1.x - v2.x, 2) + pow(v1.y - v2.y, 2) + pow(v1.z - v2.z, 2));
    }

    // tests if a point in space lies on the line segment defined by two other points
    bool isPointOnLineBetweenPoints(DataVectorTypes::Vector start, DataVectorTypes::Vector end, DataVectorTypes::Vector test)
    {
        // J. Glazer - March 2017

        Real64 tol = 0.0127; //  1.27 cm = 1/2 inch
        return (std::abs((distance(start, end) - (distance(start, test) + distance(test, end)))) < tol);
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

        using namespace Vectors;

        static std::string const RoutineName("ProcessSurfaceVertices: ");

        Real64 X1;           // Intermediate Result
        Real64 Y1;           // Intermediate Result
        Real64 Z1;           // Intermediate Result
        Real64 XLLC;         // X-coordinate of lower left corner
        Real64 YLLC;         // Y-coordinate of lower left corner
        Real64 ZLLC;         // Z-coordinate of lower left corner
        int n;               // Vertex Number in Loop
        int ThisBaseSurface; // Current base surface
        Real64 Xp;
        Real64 Yp;
        Real64 Zp;
        Real64 SurfWorldAz; // Surface Azimuth (facing)
        Real64 SurfTilt;    // Surface Tilt
        SurfaceShape ThisShape(SurfaceShape::None);
        bool BaseSurface; // True if a base surface or a detached shading surface
        Real64 ThisSurfAz;
        Real64 ThisSurfTilt;
        Real64 ThisReveal;
        Real64 ThisWidth;
        Real64 ThisHeight;
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
        PlaneEq BasePlane;
        Vector TVect;
        Vector CoordinateTransVector;

        ErrorInSurface = false;

        if (state.dataSurfaceGeometry->ProcessSurfaceVerticesOneTimeFlag) {
            state.dataSurfaceGeometry->Xpsv.allocate(MaxVerticesPerSurface);
            state.dataSurfaceGeometry->Ypsv.allocate(MaxVerticesPerSurface);
            state.dataSurfaceGeometry->Zpsv.allocate(MaxVerticesPerSurface);
            state.dataSurfaceGeometry->Xpsv = 0.0;
            state.dataSurfaceGeometry->Ypsv = 0.0;
            state.dataSurfaceGeometry->Zpsv = 0.0;
            state.dataSurfaceGeometry->ProcessSurfaceVerticesOneTimeFlag = false;
        }

        // Categorize this surface

        if (Surface(ThisSurf).BaseSurf == 0 || Surface(ThisSurf).BaseSurf == ThisSurf) {
            BaseSurface = true;
        } else {
            BaseSurface = false;
        }

        ThisBaseSurface = Surface(ThisSurf).BaseSurf; // Dont know if this is still needed or not
        HeatTransSurf = Surface(ThisSurf).HeatTransSurf;

        // Kludge for daylighting shelves
        if (Surface(ThisSurf).ShadowingSurf) {
            ThisBaseSurface = ThisSurf;
            HeatTransSurf = true;
        }

        // IF (Surface(ThisSurf)%Name(1:3) /= 'Mir') THEN
        if (!Surface(ThisSurf).MirroredSurf) {
            CalcCoPlanarNess(Surface(ThisSurf).Vertex, Surface(ThisSurf).Sides, IsCoPlanar, OutOfLine, LastVertexInError);
            if (!IsCoPlanar) {
                if (OutOfLine > 0.01) {
                    ShowSevereError(state,
                                    format("{}Suspected non-planar surface:\"{}\", Max \"out of line\"={:.5T} at Vertex # {}",
                                           RoutineName,
                                           Surface(ThisSurf).Name,
                                           OutOfLine,
                                           LastVertexInError));
                } else {
                    ShowWarningError(state,
                                     format("{}Possible non-planar surface:\"{}\", Max \"out of line\"={:.5T} at Vertex # {}",
                                            RoutineName,
                                            Surface(ThisSurf).Name,
                                            OutOfLine,
                                            LastVertexInError));
                }
                //       ErrorInSurface=.TRUE.
            }
        }

        if (BaseSurface) {
            SurfWorldAz = Surface(ThisSurf).Azimuth;
            SurfTilt = Surface(ThisSurf).Tilt;
            for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
                state.dataSurfaceGeometry->Xpsv(n) = Surface(ThisSurf).Vertex(n).x;
                state.dataSurfaceGeometry->Ypsv(n) = Surface(ThisSurf).Vertex(n).y;
                state.dataSurfaceGeometry->Zpsv(n) = Surface(ThisSurf).Vertex(n).z;
            }
            TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
            ThisWidth = VecLength(TVect);
            TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
            ThisHeight = VecLength(TVect);
            Surface(ThisSurf).Width = ThisWidth;
            Surface(ThisSurf).Height = ThisHeight; // For a horizontal surface this is actually length!
            if (Surface(ThisSurf).Sides == 3) {
                Surface(ThisSurf).Shape = SurfaceShape::Triangle;
            } else if (Surface(ThisSurf).Sides == 4) {
                // Test for rectangularity
                if (isRectangle(ThisSurf)) {
                    Surface(ThisSurf).Shape = SurfaceShape::Rectangle;
                } else {
                    Surface(ThisSurf).Shape = SurfaceShape::Quadrilateral;
                }
            } else { // Surface( ThisSurf ).Sides > 4
                Surface(ThisSurf).Shape = SurfaceShape::Polygonal;
                if (std::abs(ThisHeight * ThisWidth - Surface(ThisSurf).GrossArea) > 0.001) {
                    Surface(ThisSurf).Width = std::sqrt(Surface(ThisSurf).GrossArea);
                    Surface(ThisSurf).Height = Surface(ThisSurf).Width;
                    ThisWidth = Surface(ThisSurf).Width;
                    ThisHeight = Surface(ThisSurf).Height;
                }
            }

        } else { // It's a subsurface to previous basesurface in this set of calls

            ThisSurfAz = Surface(ThisSurf).Azimuth;
            ThisSurfTilt = Surface(ThisSurf).Tilt;

            // Retrieve base surface info
            Real64 const baseSurfWorldAz = Surface(ThisBaseSurface).Azimuth;
            Real64 const baseSurfTilt = Surface(ThisBaseSurface).Tilt;
            Real64 const BaseCosAzimuth = std::cos(baseSurfWorldAz * DataGlobalConstants::DegToRadians);
            Real64 const BaseSinAzimuth = std::sin(baseSurfWorldAz * DataGlobalConstants::DegToRadians);
            Real64 const BaseCosTilt = std::cos(baseSurfTilt * DataGlobalConstants::DegToRadians);
            Real64 const BaseSinTilt = std::sin(baseSurfTilt * DataGlobalConstants::DegToRadians);
            Real64 const BaseXLLC = Surface(ThisBaseSurface).Vertex(2).x;
            Real64 const BaseYLLC = Surface(ThisBaseSurface).Vertex(2).y;
            Real64 const BaseZLLC = Surface(ThisBaseSurface).Vertex(2).z;

            if (HeatTransSurf) {

                if (Surface(ThisSurf).Sides == 4) {
                    ThisShape = SurfaceShape::RectangularDoorWindow;
                } else if (Surface(ThisSurf).Sides == 3 && Surface(ThisSurf).Class == SurfaceClass::Window) {
                    ThisShape = SurfaceShape::TriangularWindow;
                } else if (Surface(ThisSurf).Sides == 3 && Surface(ThisSurf).Class == SurfaceClass::Door) {
                    ThisShape = SurfaceShape::TriangularDoor;
                } else {
                    assert(false);
                }

            } else { //  this is a shadowing subsurface

                if (std::abs(Surface(Surface(ThisSurf).BaseSurf).Tilt - ThisSurfTilt) <= 0.01) {
                    // left or right fin
                    if (ThisSurfAz < 0.0) ThisSurfAz += 360.0;
                    if (ThisSurfAz > Surface(Surface(ThisSurf).BaseSurf).Azimuth) {
                        ThisShape = SurfaceShape::RectangularLeftFin;
                    } else {
                        ThisShape = SurfaceShape::RectangularRightFin;
                    }
                } else {
                    ThisShape = SurfaceShape::RectangularOverhang;
                }
            }

            // Setting relative coordinates for shadowing calculations for subsurfaces
            {
                auto const SELECT_CASE_var(ThisShape);

                if (SELECT_CASE_var == SurfaceShape::RectangularDoorWindow) { // Rectangular heat transfer subsurface

                    PlaneEquation(Surface(Surface(ThisSurf).BaseSurf).Vertex, Surface(Surface(ThisSurf).BaseSurf).Sides, BasePlane, SError);
                    if (SError) {
                        ShowSevereError(state, RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface(ThisSurf).Name + "\".");
                        ErrorInSurface = true;
                    }
                    ThisReveal = -Pt2Plane(Surface(ThisSurf).Vertex(2), BasePlane);
                    if (std::abs(ThisReveal) < 0.0002) ThisReveal = 0.0;
                    Surface(ThisSurf).Reveal = ThisReveal;
                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
                    TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
                    ThisWidth = VecLength(TVect);
                    TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
                    ThisHeight = VecLength(TVect);
                    Surface(ThisSurf).Width = ThisWidth;
                    Surface(ThisSurf).Height = ThisHeight;

                    // Processing of 4-sided but non-rectangular Window, Door or GlassDoor, for use in calc of convective air flow.
                    if (!isRectangle(ThisSurf)) {

                        // Transform the surface into an equivalent rectangular surface with the same area and aspect ratio.
                        MakeEquivalentRectangle(ThisSurf, ErrorsFound);

                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, RoutineName + "Suspected 4-sided but non-rectangular Window, Door or GlassDoor:");
                            ShowContinueError(state, "Surface=" + Surface(ThisSurf).Name +
                                              " is transformed into an equivalent rectangular surface with the same area and aspect ratio. ");
                        }
                    }

                    state.dataSurfaceGeometry->Xpsv(1) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(2) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(3) = XLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Xpsv(4) = XLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Ypsv(1) = YLLC + Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Ypsv(4) = YLLC + Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Ypsv(2) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(3) = YLLC;
                    state.dataSurfaceGeometry->Zpsv(1) = ZLLC;
                    state.dataSurfaceGeometry->Zpsv(2) = ZLLC;
                    state.dataSurfaceGeometry->Zpsv(3) = ZLLC;
                    state.dataSurfaceGeometry->Zpsv(4) = ZLLC;

                    if (Surface(ThisSurf).Class == SurfaceClass::Window && Surface(ThisSurf).ExtBoundCond == ExternalEnvironment &&
                        Surface(ThisSurf).FrameDivider > 0) {
                        FrDivNum = Surface(ThisSurf).FrameDivider;
                        // Set flag for calculating beam solar reflection from outside and/or inside window reveal
                        if ((Surface(ThisSurf).Reveal > 0.0 && FrameDivider(FrDivNum).OutsideRevealSolAbs > 0.0) ||
                            (FrameDivider(FrDivNum).InsideSillDepth > 0.0 && FrameDivider(FrDivNum).InsideSillSolAbs > 0.0) ||
                            (FrameDivider(FrDivNum).InsideReveal > 0.0 && FrameDivider(FrDivNum).InsideRevealSolAbs > 0.0))
                            CalcWindowRevealReflection = true;

                        // For exterior window with frame, subtract frame area from base surface
                        // (only rectangular windows are allowed to have a frame and/or divider;
                        // Surface(ThisSurf)%FrameDivider will be 0 for triangular windows)
                        FrWidth = FrameDivider(FrDivNum).FrameWidth;
                        if (FrWidth > 0.0) {
                            FrArea = (Surface(ThisSurf).Height + 2.0 * FrWidth) * (Surface(ThisSurf).Width + 2.0 * FrWidth) -
                                     Surface(ThisSurf).Area / Surface(ThisSurf).Multiplier;
                            SurfWinFrameArea(ThisSurf) = FrArea * Surface(ThisSurf).Multiplier;
                            if ((Surface(Surface(ThisSurf).BaseSurf).Area - SurfWinFrameArea(ThisSurf)) <= 0.0) {
                                ShowSevereError(state, RoutineName + "Base Surface=\"" + Surface(Surface(ThisSurf).BaseSurf).Name + "\", ");
                                ShowContinueError(state, "Window Surface=\"" + Surface(ThisSurf).Name +
                                                  "\" area (with frame) is too large to fit on the surface.");
                                ShowContinueError(state,
                                                  format("Base surface area (-windows and doors)=[{:.2T}] m2, frame area=[{:.2T}] m2.",
                                                         Surface(Surface(ThisSurf).BaseSurf).Area,
                                                         SurfWinFrameArea(ThisSurf)));
                                ErrorInSurface = true;
                            }
                            Surface(Surface(ThisSurf).BaseSurf).Area -= SurfWinFrameArea(ThisSurf);
                        }
                        // If exterior window has divider, subtract divider area to get glazed area
                        DivWidth = FrameDivider(Surface(ThisSurf).FrameDivider).DividerWidth;
                        if (DivWidth > 0.0 && !ErrorInSurface) {
                            DivArea = DivWidth * (FrameDivider(FrDivNum).HorDividers * Surface(ThisSurf).Width +
                                                  FrameDivider(FrDivNum).VertDividers * Surface(ThisSurf).Height -
                                                  FrameDivider(FrDivNum).HorDividers * FrameDivider(FrDivNum).VertDividers * DivWidth);
                            SurfWinDividerArea(ThisSurf) = DivArea * Surface(ThisSurf).Multiplier;
                            if ((Surface(ThisSurf).Area - SurfWinDividerArea(ThisSurf)) <= 0.0) {
                                ShowSevereError(state, RoutineName + "Divider area exceeds glazed opening for window " + Surface(ThisSurf).Name);
                                ShowContinueError(state,
                                                  format("Window surface area=[{:.2T}] m2, divider area=[{:.2T}] m2.",
                                                         Surface(ThisSurf).Area,
                                                         SurfWinDividerArea(ThisSurf)));
                                ErrorInSurface = true;
                            }
                            Surface(ThisSurf).Area -= SurfWinDividerArea(ThisSurf); // Glazed area
                            if (DivArea <= 0.0) {
                                ShowWarningError(state, RoutineName + "Calculated Divider Area <= 0.0 for Window=" + Surface(ThisSurf).Name);
                                if (FrameDivider(FrDivNum).HorDividers == 0) {
                                    ShowContinueError(state, "..Number of Horizontal Dividers = 0.");
                                }
                                if (FrameDivider(FrDivNum).VertDividers == 0) {
                                    ShowContinueError(state, "..Number of Vertical Dividers = 0.");
                                }
                            } else {
                                SurfWinGlazedFrac(ThisSurf) =
                                    Surface(ThisSurf).Area / (Surface(ThisSurf).Area + SurfWinDividerArea(ThisSurf));
                                // Correction factor for portion of divider subject to divider projection correction
                                DivFrac =
                                    (1.0 - FrameDivider(FrDivNum).HorDividers * FrameDivider(FrDivNum).VertDividers * pow_2(DivWidth) / DivArea);
                                SurfWinProjCorrDivOut(ThisSurf) = DivFrac * FrameDivider(FrDivNum).DividerProjectionOut / DivWidth;
                                SurfWinProjCorrDivIn(ThisSurf) = DivFrac * FrameDivider(FrDivNum).DividerProjectionIn / DivWidth;
                                // Correction factor for portion of frame subject to frame projection correction
                                if (FrWidth > 0.0) {
                                    SurfWinProjCorrFrOut(ThisSurf) =
                                        (FrameDivider(FrDivNum).FrameProjectionOut / FrWidth) *
                                        (ThisHeight + ThisWidth -
                                         (FrameDivider(FrDivNum).HorDividers + FrameDivider(FrDivNum).VertDividers) * DivWidth) /
                                        (ThisHeight + ThisWidth + 2 * FrWidth);
                                    SurfWinProjCorrFrIn(ThisSurf) =
                                        (FrameDivider(FrDivNum).FrameProjectionIn / FrWidth) *
                                        (ThisHeight + ThisWidth -
                                         (FrameDivider(FrDivNum).HorDividers + FrameDivider(FrDivNum).VertDividers) * DivWidth) /
                                        (ThisHeight + ThisWidth + 2 * FrWidth);
                                }
                            }
                        }
                    }

                } else if ((SELECT_CASE_var == SurfaceShape::TriangularWindow) || (SELECT_CASE_var == SurfaceShape::TriangularDoor)) {

                    PlaneEquation(Surface(Surface(ThisSurf).BaseSurf).Vertex, Surface(Surface(ThisSurf).BaseSurf).Sides, BasePlane, SError);
                    if (SError) {
                        ShowSevereError(state, RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface(ThisSurf).Name + "\".");
                        ErrorInSurface = true;
                    }
                    ThisReveal = -Pt2Plane(Surface(ThisSurf).Vertex(2), BasePlane);
                    if (std::abs(ThisReveal) < 0.0002) ThisReveal = 0.0;
                    Surface(ThisSurf).Reveal = ThisReveal;
                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    state.dataSurfaceGeometry->Xpsv(2) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    state.dataSurfaceGeometry->Ypsv(2) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    state.dataSurfaceGeometry->Zpsv(2) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
                    TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
                    ThisWidth = VecLength(TVect);
                    TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
                    ThisHeight = VecLength(TVect);
                    Surface(ThisSurf).Width = ThisWidth;
                    Surface(ThisSurf).Height = ThisHeight;
                    // Effective height and width of a triangular window for use in calc of convective air flow
                    // in gap between glass and shading device when shading device is present
                    Surface(ThisSurf).Height = 4.0 * Surface(ThisSurf).Area / (3.0 * Surface(ThisSurf).Width);
                    Surface(ThisSurf).Width *= 0.75;

                    Xp = Surface(ThisSurf).Vertex(1).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(1).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(1).z - BaseZLLC;
                    state.dataSurfaceGeometry->Xpsv(1) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    state.dataSurfaceGeometry->Ypsv(1) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    state.dataSurfaceGeometry->Zpsv(1) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

                    Xp = Surface(ThisSurf).Vertex(3).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(3).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(3).z - BaseZLLC;
                    state.dataSurfaceGeometry->Xpsv(3) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    state.dataSurfaceGeometry->Ypsv(3) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    state.dataSurfaceGeometry->Zpsv(3) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

                } else if (SELECT_CASE_var == SurfaceShape::RectangularOverhang) {

                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
                    TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
                    ThisWidth = VecLength(TVect);
                    TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
                    ThisHeight = VecLength(TVect);
                    Surface(ThisSurf).Width = ThisWidth;
                    Surface(ThisSurf).Height = ThisHeight;
                    state.dataSurfaceGeometry->Xpsv(1) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(2) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(3) = XLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Xpsv(4) = XLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Ypsv(1) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(2) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(3) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(4) = YLLC;
                    state.dataSurfaceGeometry->Zpsv(1) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(4) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(2) = 0.0;
                    state.dataSurfaceGeometry->Zpsv(3) = 0.0;

                } else if (SELECT_CASE_var == SurfaceShape::RectangularLeftFin) {

                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
                    TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
                    ThisWidth = VecLength(TVect);
                    TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
                    ThisHeight = VecLength(TVect);
                    Surface(ThisSurf).Width = ThisWidth;
                    Surface(ThisSurf).Height = ThisHeight;
                    state.dataSurfaceGeometry->Xpsv(1) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(2) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(3) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(4) = XLLC;
                    state.dataSurfaceGeometry->Ypsv(1) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(2) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(3) = YLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Ypsv(4) = YLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Zpsv(1) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(4) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(2) = 0.0;
                    state.dataSurfaceGeometry->Zpsv(3) = 0.0;

                } else if (SELECT_CASE_var == SurfaceShape::RectangularRightFin) {

                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
                    TVect = Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2);
                    ThisWidth = VecLength(TVect);
                    TVect = Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1);
                    ThisHeight = VecLength(TVect);
                    Surface(ThisSurf).Width = ThisWidth;
                    Surface(ThisSurf).Height = ThisHeight;
                    state.dataSurfaceGeometry->Xpsv(1) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(2) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(3) = XLLC;
                    state.dataSurfaceGeometry->Xpsv(4) = XLLC;
                    state.dataSurfaceGeometry->Ypsv(1) = YLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Ypsv(2) = YLLC + Surface(ThisSurf).Width;
                    state.dataSurfaceGeometry->Ypsv(3) = YLLC;
                    state.dataSurfaceGeometry->Ypsv(4) = YLLC;
                    state.dataSurfaceGeometry->Zpsv(1) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(4) = Surface(ThisSurf).Height;
                    state.dataSurfaceGeometry->Zpsv(2) = 0.0;
                    state.dataSurfaceGeometry->Zpsv(3) = 0.0;

                } else {
                    // Error Condition
                    ShowSevereError(state, RoutineName + "Incorrect surface shape number.", OptionalOutputFileRef{state.files.eso});
                    ShowContinueError(state, "Please notify EnergyPlus support of this error and send input file.");
                    ErrorInSurface = true;
                }
            }

            for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
                // if less than 1/10 inch
                state.dataSurfaceGeometry->Xpsv(n) = nint64(10000.0 * state.dataSurfaceGeometry->Xpsv(n)) / 10000.0;
                if (std::abs(state.dataSurfaceGeometry->Xpsv(n)) < 0.0025) state.dataSurfaceGeometry->Xpsv(n) = 0.0;
                state.dataSurfaceGeometry->Ypsv(n) = nint64(10000.0 * state.dataSurfaceGeometry->Ypsv(n)) / 10000.0;
                if (std::abs(state.dataSurfaceGeometry->Ypsv(n)) < 0.0025) state.dataSurfaceGeometry->Ypsv(n) = 0.0;
                state.dataSurfaceGeometry->Zpsv(n) = nint64(10000.0 * state.dataSurfaceGeometry->Zpsv(n)) / 10000.0;
                if (std::abs(state.dataSurfaceGeometry->Zpsv(n)) < 0.0025) state.dataSurfaceGeometry->Zpsv(n) = 0.0;
            }

            Surface(ThisSurf).Shape = ThisShape;

        } // End of check if ThisSurf is a base surface

        if (ErrorInSurface) {
            ErrorsFound = true;
            return;
        }

        // Transfer to XV,YV,ZV arrays

        ShadeV(ThisSurf).NVert = Surface(ThisSurf).Sides;
        ShadeV(ThisSurf).XV.allocate(Surface(ThisSurf).Sides);
        ShadeV(ThisSurf).YV.allocate(Surface(ThisSurf).Sides);
        ShadeV(ThisSurf).ZV.allocate(Surface(ThisSurf).Sides);

        for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
            // if less than 1/10 inch
            ShadeV(ThisSurf).XV(n) = state.dataSurfaceGeometry->Xpsv(n);
            ShadeV(ThisSurf).YV(n) = state.dataSurfaceGeometry->Ypsv(n);
            ShadeV(ThisSurf).ZV(n) = state.dataSurfaceGeometry->Zpsv(n);
        }

        // Process Surfaces According to Type of Coordinate Origin.
        if (BaseSurface) {

            // General Surfaces:
            CalcCoordinateTransformation(state, ThisSurf, CoordinateTransVector); // X00,Y00,Z00,X,Y,Z,A)    ! Compute Coordinate Transformation

            // RECORD DIRECTION COSINES.
            if (HeatTransSurf) { // This is a general surface but not detached shading surface

                // RECORD COORDINATE TRANSFORMATION FOR BASE SURFACES.
                X0(ThisBaseSurface) = CoordinateTransVector.x;
                Y0(ThisBaseSurface) = CoordinateTransVector.y;
                Z0(ThisBaseSurface) = CoordinateTransVector.z;

                // COMPUTE INVERSE TRANSFORMATION.
                X1 = state.dataSurfaceGeometry->Xpsv(2) - CoordinateTransVector.x;
                Y1 = state.dataSurfaceGeometry->Ypsv(2) - CoordinateTransVector.y;
                Z1 = state.dataSurfaceGeometry->Zpsv(2) - CoordinateTransVector.z;
                // Store the relative coordinate shift values for later use by any subsurfaces
                Surface(ThisBaseSurface).XShift = Surface(ThisBaseSurface).lcsx.x * X1 + Surface(ThisBaseSurface).lcsx.y * Y1 + Surface(ThisBaseSurface).lcsx.z * Z1;
                Surface(ThisBaseSurface).YShift = Surface(ThisBaseSurface).lcsy.x * X1 + Surface(ThisBaseSurface).lcsy.y * Y1 + Surface(ThisBaseSurface).lcsy.z * Z1;
                Surface(ThisBaseSurface).VerticesProcessed = true;
            }

            // SUBSURFACES: (Surface(ThisSurf)%BaseSurf /= ThisSurf)
        } else {
            // WINDOWS OR DOORS:

            // SHIFT RELATIVE COORDINATES FROM LOWER LEFT CORNER TO ORIGIN DEFINED
            // BY CTRAN AND SET DIRECTION COSINES SAME AS BASE SURFACE.
            if (!Surface(ThisBaseSurface).VerticesProcessed) {
                ShowSevereError(state, RoutineName + "Developer error for Subsurface=" + Surface(ThisSurf).Name);
                ShowContinueError(state, "Base surface=" + Surface(ThisBaseSurface).Name + " vertices must be processed before any subsurfaces.");
                ShowFatalError(state, RoutineName);
            }

            for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
                ShadeV(ThisSurf).XV(n) += Surface(ThisBaseSurface).XShift;
                ShadeV(ThisSurf).YV(n) += Surface(ThisBaseSurface).YShift;
            }
        }

        if (ErrorInSurface) {
            ErrorsFound = true;
        }
    }

    void CalcCoordinateTransformation(EnergyPlusData &state,
                                      int const SurfNum,            // Surface Number
                                      Vector &CompCoordTranslVector // Coordinate Translation Vector
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

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int I;        // Loop Control
        Real64 Gamma; // Intermediate Result
        Real64 DotSelfX23;
        static std::string ErrLineOut; // Character string for producing error messages

        // Object Data
        Vector x21;
        Vector x23;

        // Determine Components of the Coordinate Translation Vector.

        x21 = Surface(SurfNum).Vertex(2) - Surface(SurfNum).Vertex(1);
        x23 = Surface(SurfNum).Vertex(2) - Surface(SurfNum).Vertex(3);

        DotSelfX23 = magnitude_squared(x23);

        if (DotSelfX23 <= .1e-6) {
            ShowSevereError(state, "CalcCoordinateTransformation: Invalid dot product, surface=\"" + Surface(SurfNum).Name + "\":");
            for (I = 1; I <= Surface(SurfNum).Sides; ++I) {
                auto const &point{Surface(SurfNum).Vertex(I)};
                ShowContinueError(state, format(" ({:8.3F},{:8.3F},{:8.3F})", point.x, point.y, point.z));
            }
            ShowFatalError(state, "CalcCoordinateTransformation: Program terminates due to preceding condition.", OptionalOutputFileRef{state.files.eso});
            return;
        }

        Gamma = dot(x21, x23) / magnitude_squared(x23);

        CompCoordTranslVector = Surface(SurfNum).Vertex(2) + Gamma * (Surface(SurfNum).Vertex(3) - Surface(SurfNum).Vertex(2));
    }

    void CreateShadedWindowConstruction(EnergyPlusData &state,
                                        int const SurfNum, // Surface number
                                        int const WSCPtr,  // Pointer to WindowShadingControl for SurfNum
                                        int const ShDevNum, // Shading device material number for WSCptr
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
        int ConstrNum;            // Number of unshaded construction
        int ConstrNewSh;          // Number of shaded construction that is created
        std::string ShDevName;    // Shading device material name
        std::string ConstrName;   // Unshaded construction name
        std::string ConstrNameSh; // Shaded construction name
        int TotLayersOld;         // Total layers in old (unshaded) construction
        int TotLayersNew;         // Total layers in new (shaded) construction
        //  INTEGER :: loop                            ! DO loop index

        ShDevName = state.dataMaterial->Material(ShDevNum).Name;
        ConstrNum = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;
        ConstrName = state.dataConstruction->Construct(ConstrNum).Name;
        if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade || WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind) {
            ConstrNameSh = ConstrName + ':' + ShDevName + ":INT";
        } else {
            ConstrNameSh = ConstrName + ':' + ShDevName + ":EXT";
        }

        // If this construction name already exists, set the surface's shaded construction number to it

        ConstrNewSh = UtilityRoutines::FindItemInList(ConstrNameSh, state.dataConstruction->Construct);

        if (ConstrNewSh > 0) {
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNewSh; //set the active to the current for now
        } else {

            // Create new construction

            ConstrNewSh = TotConstructs + 1;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNewSh; //set the active to the current for now
            TotConstructs = ConstrNewSh;
            state.dataConstruction->Construct.redimension(TotConstructs);
            NominalRforNominalUCalculation.redimension(TotConstructs);
            NominalRforNominalUCalculation(TotConstructs) = 0.0;
            NominalU.redimension(TotConstructs);
            NominalU(TotConstructs) = 0.0;

            TotLayersOld = state.dataConstruction->Construct(ConstrNum).TotLayers;
            TotLayersNew = TotLayersOld + 1;

            state.dataConstruction->Construct(ConstrNewSh).LayerPoint = 0;

            if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade ||
                WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind) {
                // Interior shading device
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint({1, TotLayersOld}) = state.dataConstruction->Construct(ConstrNum).LayerPoint({1, TotLayersOld});
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint(TotLayersNew) = ShDevNum;
                state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpSolar = state.dataMaterial->Material(ShDevNum).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpSolar = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1)).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpThermal = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1)).AbsorpThermalFront;
            } else {
                // Exterior shading device
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1) = ShDevNum;
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint({2, TotLayersNew}) = state.dataConstruction->Construct(ConstrNum).LayerPoint({1, TotLayersOld});
                state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpSolar = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(TotLayersNew)).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpSolar = state.dataMaterial->Material(ShDevNum).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpThermal = state.dataMaterial->Material(ShDevNum).AbsorpThermalFront;
            }
            // The following InsideAbsorpThermal applies only to inside glass; it is corrected
            //  later in InitGlassOpticalCalculations if construction has inside shade or blind.
            state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpThermal = state.dataMaterial->Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayersOld)).AbsorpThermalBack;
            state.dataConstruction->Construct(ConstrNewSh).OutsideRoughness = VerySmooth;
            state.dataConstruction->Construct(ConstrNewSh).DayltPropPtr = 0;
            state.dataConstruction->Construct(ConstrNewSh).CTFCross = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFFlux = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFInside = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFOutside = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFSourceIn = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFSourceOut = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTimeStep = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTSourceOut = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTSourceIn = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTSourceQ = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTUserOut = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTUserIn = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).CTFTUserSource = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).NumHistories = 0;
            state.dataConstruction->Construct(ConstrNewSh).NumCTFTerms = 0;
            state.dataConstruction->Construct(ConstrNewSh).UValue = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).SourceSinkPresent = false;
            state.dataConstruction->Construct(ConstrNewSh).SolutionDimensions = 0;
            state.dataConstruction->Construct(ConstrNewSh).SourceAfterLayer = 0;
            state.dataConstruction->Construct(ConstrNewSh).TempAfterLayer = 0;
            state.dataConstruction->Construct(ConstrNewSh).ThicknessPerpend = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsDiff = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsDiffBack = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsDiffShade = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsDiffBackShade = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ShadeAbsorpThermal = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsBeamCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsBeamBackCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).AbsBeamShadeCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).TransDiff = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).TransDiffVis = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflectSolDiffBack = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflectSolDiffFront = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflectVisDiffBack = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflectVisDiffFront = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).TransSolBeamCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).TransVisBeamCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflSolBeamFrontCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).ReflSolBeamBackCoef = 0.0;
            state.dataConstruction->Construct(ConstrNewSh).W5FrameDivider = 0;
            state.dataConstruction->Construct(ConstrNewSh).FromWindow5DataFile = false;

            state.dataConstruction->Construct(ConstrNewSh).Name = ConstrNameSh;
            state.dataConstruction->Construct(ConstrNewSh).TotLayers = TotLayersNew;
            state.dataConstruction->Construct(ConstrNewSh).TotSolidLayers = state.dataConstruction->Construct(ConstrNum).TotSolidLayers + 1;
            state.dataConstruction->Construct(ConstrNewSh).TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
            state.dataConstruction->Construct(ConstrNewSh).TypeIsWindow = true;
            state.dataConstruction->Construct(ConstrNewSh).IsUsed = true;
        }
    }

    void CreateStormWindowConstructions(EnergyPlusData &state)
    {
        // For windows with an associated StormWindow object, creates a construction
        // consisting of the base construction plus a storm window and air gap on the outside.
        // If the window has an interior or between-glass shade/blind, also creates a
        // construction consisting of the storm window added to the shaded construction.
        DisplayString(state, "Creating Storm Window Constructions");

        for (int StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
            int SurfNum = StormWindow(StormWinNum).BaseWindowNum; // Surface number
            int ConstrNum = Surface(SurfNum).Construction; // Number of unshaded construction
            // Fatal error if base construction has more than three glass layers
            if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers > 3) {
                ShowFatalError(state, "Window=" + Surface(SurfNum).Name + " has more than 3 glass layers; a storm window cannot be applied.");
            }

            // create unshaded construction with storm window
            const auto ChrNum = fmt::to_string(StormWinNum);
            std::string ConstrNameSt = "BARECONSTRUCTIONWITHSTORMWIN:" + ChrNum; // Name of unshaded construction with storm window
            // If this construction name already exists, set the surface's storm window construction number to it
            int ConstrNewSt = UtilityRoutines::FindItemInList(ConstrNameSt, state.dataConstruction->Construct, TotConstructs); // Number of unshaded storm window construction that is created
            // If necessary, create new material corresponding to the air layer between the storm winddow and the rest of the window
            int MatNewStAir = createAirMaterialFromDistance(state, StormWindow(StormWinNum).StormWinDistance, "AIR:STORMWIN:");
            if (ConstrNewSt == 0) {
                ConstrNewSt = createConstructionWithStorm(state, ConstrNum, ConstrNameSt, StormWindow(StormWinNum).StormWinMaterialNum, MatNewStAir);
            }
            Surface(SurfNum).StormWinConstruction = ConstrNewSt;

            // create shaded constructions with storm window
            Surface(SurfNum).shadedStormWinConstructionList.resize(Surface(SurfNum).shadedConstructionList.size(), 0);  // make the shaded storm window size the same size as the number of shaded constructions
            for (std::size_t iConstruction = 0; iConstruction < Surface(SurfNum).shadedConstructionList.size(); ++iConstruction) {
                int curConstruction = Surface(SurfNum).shadedConstructionList[iConstruction];
                // Set ShAndSt, which is true if the window has a shaded construction to which a storm window
                // can be added. (A storm window can be added if there is an interior shade or blind and up to three
                // glass layers, or there is a between-glass shade or blind and two glass layers.)
                bool ShAndSt = false; // True if unshaded and shaded window can have a storm window
                std::string ConstrNameSh = state.dataConstruction->Construct(curConstruction).Name; // Name of original shaded window construction
                int TotLayers = state.dataConstruction->Construct(curConstruction).TotLayers; // Total layers in a construction
                int MatIntSh = state.dataConstruction->Construct(curConstruction).LayerPoint(TotLayers); // Material number of interior shade or blind
                int MatBetweenGlassSh = 0; // Material number of between-glass shade or blind
                if (TotLayers == 5) MatBetweenGlassSh = state.dataConstruction->Construct(curConstruction).LayerPoint(3);
                if (state.dataConstruction->Construct(curConstruction).TotGlassLayers <= 3 && (state.dataMaterial->Material(MatIntSh).Group == Shade || state.dataMaterial->Material(MatIntSh).Group == WindowBlind)) ShAndSt = true;
                if (MatBetweenGlassSh > 0) {
                    if (state.dataMaterial->Material(MatBetweenGlassSh).Group == Shade || state.dataMaterial->Material(MatBetweenGlassSh).Group == WindowBlind) {
                        ShAndSt = true;
                    } else {
                        ShowContinueError(state, "Window=" + Surface(SurfNum).Name + " has a shaded construction to which a storm window cannot be applied.");
                        ShowContinueError(state, "Storm windows can only be applied to shaded constructions that:");
                        ShowContinueError(state, "have an interior shade or blind and up to three glass layers, or");
                        ShowContinueError(state, "have a between-glass shade or blind and two glass layers.");
                        ShowFatalError(state, "EnergyPlus is exiting due to reason stated above.");
                    }
                }
                if (ShAndSt) {
                    std::string ConstrNameStSh = "SHADEDCONSTRUCTIONWITHSTORMWIN:" + state.dataConstruction->Construct(iConstruction).Name + ":" + ChrNum  ; // Name of shaded construction with storm window
                    int ConstrNewStSh = createConstructionWithStorm(state, ConstrNum, ConstrNameStSh, StormWindow(StormWinNum).StormWinMaterialNum, MatNewStAir);
                    Surface(SurfNum).shadedStormWinConstructionList[iConstruction] = ConstrNewStSh; // put in same index as the shaded constuction
                }
            } // end of loop for shaded constructions
        } // end of loop over storm window objects
    }

    int createAirMaterialFromDistance(EnergyPlusData &state, Real64 distance, std::string namePrefix)
    {
        int mmDistance = int(1000 * distance); // Thickness of air gap in mm (usually between storm window and rest of window)
        std::string MatNameStAir = namePrefix + format("{}MM", mmDistance); // Name of created air layer material
        int newAirMaterial = UtilityRoutines::FindItemInList(MatNameStAir, state.dataMaterial->Material, TotMaterials);
        if (newAirMaterial == 0) {
            // Create new material
            TotMaterials = TotMaterials + 1;
            newAirMaterial = TotMaterials;
            state.dataMaterial->Material.redimension(TotMaterials);
            NominalR.redimension(TotMaterials);
            state.dataMaterial->Material(TotMaterials).Name = MatNameStAir;
            state.dataMaterial->Material(TotMaterials).Group = WindowGas;
            state.dataMaterial->Material(TotMaterials).Roughness = 3;
            state.dataMaterial->Material(TotMaterials).Conductivity = 0.0;
            state.dataMaterial->Material(TotMaterials).Density = 0.0;
            state.dataMaterial->Material(TotMaterials).IsoMoistCap = 0.0;
            state.dataMaterial->Material(TotMaterials).Porosity = 0.0;
            state.dataMaterial->Material(TotMaterials).Resistance = 0.0;
            state.dataMaterial->Material(TotMaterials).SpecHeat = 0.0;
            state.dataMaterial->Material(TotMaterials).ThermGradCoef = 0.0;
            state.dataMaterial->Material(TotMaterials).Thickness = distance;
            state.dataMaterial->Material(TotMaterials).VaporDiffus = 0.0;
            state.dataMaterial->Material(TotMaterials).GasType = 0;
            state.dataMaterial->Material(TotMaterials).GasCon = 0.0;
            state.dataMaterial->Material(TotMaterials).GasVis = 0.0;
            state.dataMaterial->Material(TotMaterials).GasCp = 0.0;
            state.dataMaterial->Material(TotMaterials).GasWght = 0.0;
            state.dataMaterial->Material(TotMaterials).GasFract = 0.0;
            state.dataMaterial->Material(TotMaterials).GasType(1) = 1;
            state.dataMaterial->Material(TotMaterials).GlassSpectralDataPtr = 0;
            state.dataMaterial->Material(TotMaterials).NumberOfGasesInMixture = 1;
            state.dataMaterial->Material(TotMaterials).GasCon(1, 1) = 2.873e-3;
            state.dataMaterial->Material(TotMaterials).GasCon(2, 1) = 7.760e-5;
            state.dataMaterial->Material(TotMaterials).GasVis(1, 1) = 3.723e-6;
            state.dataMaterial->Material(TotMaterials).GasVis(2, 1) = 4.940e-8;
            state.dataMaterial->Material(TotMaterials).GasCp(1, 1) = 1002.737;
            state.dataMaterial->Material(TotMaterials).GasCp(2, 1) = 1.2324e-2;
            state.dataMaterial->Material(TotMaterials).GasWght(1) = 28.97;
            state.dataMaterial->Material(TotMaterials).GasFract(1) = 1.0;
            state.dataMaterial->Material(TotMaterials).AbsorpSolar = 0.0;
            state.dataMaterial->Material(TotMaterials).AbsorpThermal = 0.0;
            state.dataMaterial->Material(TotMaterials).AbsorpVisible = 0.0;
            state.dataMaterial->Material(TotMaterials).Trans = 0.0;
            state.dataMaterial->Material(TotMaterials).TransVis = 0.0;
            state.dataMaterial->Material(TotMaterials).GlassTransDirtFactor = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectShade = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectShadeVis = 0.0;
            state.dataMaterial->Material(TotMaterials).AbsorpThermalBack = 0.0;
            state.dataMaterial->Material(TotMaterials).AbsorpThermalFront = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectSolBeamBack = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectSolBeamFront = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectSolDiffBack = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectSolDiffFront = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectVisBeamBack = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectVisBeamFront = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectVisDiffBack = 0.0;
            state.dataMaterial->Material(TotMaterials).ReflectVisDiffFront = 0.0;
            state.dataMaterial->Material(TotMaterials).TransSolBeam = 0.0;
            state.dataMaterial->Material(TotMaterials).TransThermal = 0.0;
            state.dataMaterial->Material(TotMaterials).TransVisBeam = 0.0;
            state.dataMaterial->Material(TotMaterials).BlindDataPtr = 0;
            state.dataMaterial->Material(TotMaterials).WinShadeToGlassDist = 0.0;
            state.dataMaterial->Material(TotMaterials).WinShadeTopOpeningMult = 0.0;
            state.dataMaterial->Material(TotMaterials).WinShadeBottomOpeningMult = 0.0;
            state.dataMaterial->Material(TotMaterials).WinShadeLeftOpeningMult = 0.0;
            state.dataMaterial->Material(TotMaterials).WinShadeRightOpeningMult = 0.0;
            state.dataMaterial->Material(TotMaterials).WinShadeAirFlowPermeability = 0.0;
        }
        return(newAirMaterial);
    }

    // create a new construction with storm based on an old construction and storm and gap materials
    int createConstructionWithStorm(EnergyPlusData &state, int oldConstruction, std::string name, int stormMaterial, int gapMaterial)
    {
        int newConstruct = UtilityRoutines::FindItemInList(name, state.dataConstruction->Construct, TotConstructs); // Number of shaded storm window construction that is created
        if (newConstruct == 0) {
            TotConstructs = TotConstructs + 1;
            newConstruct = TotConstructs;
            state.dataConstruction->Construct.redimension(TotConstructs);
            NominalRforNominalUCalculation.redimension(TotConstructs);
            NominalU.redimension(TotConstructs);

            int TotLayersOld = state.dataConstruction->Construct(oldConstruction).TotLayers;
            state.dataConstruction->Construct(TotConstructs).LayerPoint({ 1, Construction::MaxLayersInConstruct }) = 0;
            state.dataConstruction->Construct(TotConstructs).LayerPoint(1) = stormMaterial;
            state.dataConstruction->Construct(TotConstructs).LayerPoint(2) = gapMaterial;
            state.dataConstruction->Construct(TotConstructs).LayerPoint({ 3, TotLayersOld + 2 }) = state.dataConstruction->Construct(oldConstruction).LayerPoint({ 1, TotLayersOld });
            state.dataConstruction->Construct(TotConstructs).Name = name;
            state.dataConstruction->Construct(TotConstructs).TotLayers = TotLayersOld + 2;
            state.dataConstruction->Construct(TotConstructs).TotSolidLayers = state.dataConstruction->Construct(oldConstruction).TotSolidLayers + 1;
            state.dataConstruction->Construct(TotConstructs).TotGlassLayers = state.dataConstruction->Construct(oldConstruction).TotGlassLayers + 1;
            state.dataConstruction->Construct(TotConstructs).TypeIsWindow = true;
            state.dataConstruction->Construct(TotConstructs).InsideAbsorpVis = 0.0;
            state.dataConstruction->Construct(TotConstructs).OutsideAbsorpVis = 0.0;
            state.dataConstruction->Construct(TotConstructs).InsideAbsorpSolar = 0.0;
            state.dataConstruction->Construct(TotConstructs).OutsideAbsorpSolar = 0.0;
            state.dataConstruction->Construct(TotConstructs).InsideAbsorpThermal = state.dataConstruction->Construct(oldConstruction).InsideAbsorpThermal;
            state.dataConstruction->Construct(TotConstructs).OutsideAbsorpThermal = state.dataMaterial->Material(stormMaterial).AbsorpThermalFront;
            state.dataConstruction->Construct(TotConstructs).OutsideRoughness = VerySmooth;
            state.dataConstruction->Construct(TotConstructs).DayltPropPtr = 0;
            state.dataConstruction->Construct(TotConstructs).CTFCross = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFFlux = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFInside = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFOutside = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFSourceIn = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFSourceOut = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTimeStep = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTSourceOut = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTSourceIn = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTSourceQ = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTUserOut = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTUserIn = 0.0;
            state.dataConstruction->Construct(TotConstructs).CTFTUserSource = 0.0;
            state.dataConstruction->Construct(TotConstructs).NumHistories = 0;
            state.dataConstruction->Construct(TotConstructs).NumCTFTerms = 0;
            state.dataConstruction->Construct(TotConstructs).UValue = 0.0;
            state.dataConstruction->Construct(TotConstructs).SourceSinkPresent = false;
            state.dataConstruction->Construct(TotConstructs).SolutionDimensions = 0;
            state.dataConstruction->Construct(TotConstructs).SourceAfterLayer = 0;
            state.dataConstruction->Construct(TotConstructs).TempAfterLayer = 0;
            state.dataConstruction->Construct(TotConstructs).ThicknessPerpend = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiffIn = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiffOut = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiff = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiffBack = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiffShade = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsDiffBackShade = 0.0;
            state.dataConstruction->Construct(TotConstructs).ShadeAbsorpThermal = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsBeamCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsBeamBackCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).AbsBeamShadeCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).TransDiff = 0.0;
            state.dataConstruction->Construct(TotConstructs).TransDiffVis = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflectSolDiffBack = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflectSolDiffFront = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflectVisDiffBack = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflectVisDiffFront = 0.0;
            state.dataConstruction->Construct(TotConstructs).TransSolBeamCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).TransVisBeamCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflSolBeamFrontCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).ReflSolBeamBackCoef = 0.0;
            state.dataConstruction->Construct(TotConstructs).W5FrameDivider = 0;
            state.dataConstruction->Construct(TotConstructs).FromWindow5DataFile = false;
            state.dataConstruction->Construct(TotConstructs).W5FileMullionWidth = 0.0;
            state.dataConstruction->Construct(TotConstructs).W5FileMullionOrientation = 0;
            state.dataConstruction->Construct(TotConstructs).W5FileGlazingSysWidth = 0.0;
            state.dataConstruction->Construct(TotConstructs).W5FileGlazingSysHeight = 0.0;
        }
        return(newConstruct);
    }

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

        // Using/Aliasing

        using namespace Vectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // If there is a second glazing systme on the Data File, SurfNum+1
        // has the construction of the second glazing system.

        // 2-glazing system Window5 data file entry

        // DERIVED TYPE DEFINITIONS:

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // unused1208  INTEGER :: TotSurfacesPrev                 ! Total number of surfaces before splitting window
        // unused1208  INTEGER :: loop                            ! DO loop index
        Real64 H; // Height and width of original window (m)
        Real64 W;
        // unused1208  REAL(r64)    :: MulWidth                        ! Mullion width (m)
        Real64 h1; // height and width of first glazing system (m)
        Real64 w1;
        // unused1208  REAL(r64)    :: h2,w2                           ! height and width of second glazing system (m)
        // unused1208  type (rectangularwindow) :: NewCoord
        int IConst;             // Construction number of first glazing system
        int IConst2;            // Construction number of second glazing system
        std::string Const2Name; // Name of construction of second glazing system
        // unused1208  REAL(r64)    :: AreaNew                         ! Sum of areas of the two glazing systems (m2)

        struct rectangularwindow
        {
            // Members
            Array1D<Vector> Vertex;

            // Default Constructor
            rectangularwindow() : Vertex(4)
            {
            }
        };

        // Object Data
        Vector TVect;
        rectangularwindow OriginalCoord;

        IConst = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;

        // Height and width of original window
        TVect = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(3) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2);
        W = VecLength(TVect); // SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
        TVect = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1);
        H = VecLength(TVect); // SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord.Vertex({1, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides}) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex({1, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides});

        // Height and width of first glazing system
        h1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysHeight;
        w1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysWidth;

        Const2Name = state.dataConstruction->Construct(IConst).Name + ":2";
        IConst2 = UtilityRoutines::FindItemInList(Const2Name, state.dataConstruction->Construct);

        if (IConst2 == 0) { // Only one glazing system on Window5 Data File for this window.

            // So... original dimensions and area of window are used (entered in IDF)
            // Warning if dimensions of original window differ from those on Data File by more than 10%

            if (std::abs((H - h1) / H) > 0.10 || std::abs((W - w1) / W) > 0.10) {

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, "SurfaceGeometry: ModifyWindow: Window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                     " uses the Window5 Data File Construction " + state.dataConstruction->Construct(IConst).Name);
                    ShowContinueError(state, format("The height {:.3R}(m) or width  (m) of this window differs by more than 10%{:.3R}", H, W));
                    ShowContinueError(state,
                                      format("from the corresponding height {:.3R} (m) or width  (m) on the Window5 Data file.{:.3R}", h1, w1));
                    ShowContinueError(state, "This will affect the frame heat transfer calculation if the frame in the Data File entry");
                    ShowContinueError(state, "is not uniform, i.e., has sections with different geometry and/or thermal properties.");
                } else {
                    ++state.dataSurfaceGeometry->Warning1Count;
                }
            }

            // Calculate net area for base surface
            state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
            if (state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
                ShowSevereError(state, "Subsurfaces have too much area for base surface=" + state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Name);
                ShowContinueError(state, "Subsurface creating error=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
                ErrorsFound = true;
            }

            // Net area of base surface with unity window multipliers (used in shadowing checks)
            state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area / state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;

        } else { // Two glazing systems on Window5 data file for this window

            // if exterior window, okay.

            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                // There are two glazing systems (separated by a vertical or horizontal mullion) on the Window5 Data File.
                // Fill in geometry data for the second window (corresponding to the second glazing system on the data file.
                // The first glazing system is assumed to be at left for vertical mullion, at bottom for horizontal mullion.
                // The second glazing system is assumed to be at right for vertical mullion, at top for horizontal mullion.
                // The lower left-hand corner of the original window (its vertex #2) is assumed to coincide with
                // vertex #2 of the first glazing system.

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowMessage(state, "SurfaceGeometry: ModifyWindow: Window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                " has been replaced with the Window 5/6 two glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                    ShowContinueError(state, "Note that originally entered dimensions are overridden.");
                } else {
                    ++state.dataSurfaceGeometry->Warning2Count;
                }

                // Allocate another window
                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond > 0) { // Interior window, specified  ! not external environment

                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, "SurfaceGeometry: ModifyWindow: Interior Window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name +
                                     " has been replaced with the Window 5/6 two glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                    ShowContinueError(state,
                        "Please check to make sure interior window is correct. Note that originally entered dimensions are overridden.");
                } else {
                    ++state.dataSurfaceGeometry->Warning3Count;
                }

                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else { // Interior window, specified not entered

                ShowSevereError(state, "SurfaceGeometry: ModifyWindow: Interior Window " + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + " is a window in an adjacent zone.");
                ShowContinueError(state, "Attempted to add/reverse Window 5/6 multiple glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                ShowContinueError(state, "Cannot use these Window 5/6 constructs for these Interior Windows. Program will terminate.");
                ErrorsFound = true;
            }

        } // End of check if one or two glazing systems are on the Window5 Data File
    }

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

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int loop; // DO loop index
        Real64 H; // Height and width of original window (m)
        Real64 W;
        Real64 MulWidth; // Mullion width (m)
        Real64 h1;       // height and width of first glazing system (m)
        Real64 w1;
        Real64 h2; // height and width of second glazing system (m)
        Real64 w2;
        Real64 xa; // Vertex intermediate variables (m)
        Real64 ya;
        Real64 za;
        Real64 xb;
        Real64 yb;
        Real64 zb;
        Real64 dx; // Vertex displacements from original window (m)
        Real64 dy;
        int IConst;             // Construction number of first glazing system
        int IConst2;            // Construction number of second glazing system
        std::string Const2Name; // Name of construction of second glazing system
        Real64 AreaNew;         // Sum of areas of the two glazing systems (m2)

        struct rectangularwindow
        {
            // Members
            Array1D<Vector> Vertex;

            // Default Constructor
            rectangularwindow() : Vertex(4)
            {
            }
        };

        // Object Data
        Vector TVect;
        rectangularwindow NewCoord;
        rectangularwindow OriginalCoord;

        IConst = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Construction;

        // Height and width of original window
        TVect = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(3) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2);
        W = VecLength(TVect); // SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
        TVect = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2) - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1);
        H = VecLength(TVect); // SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord.Vertex({1, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides}) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex({1, state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides});

        // Height and width of first glazing system
        h1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysHeight;
        w1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysWidth;

        Const2Name = state.dataConstruction->Construct(IConst).Name + ":2";
        IConst2 = UtilityRoutines::FindItemInList(Const2Name, state.dataConstruction->Construct);

        ++AddedSubSurfaces;
        state.dataSurfaceGeometry->SurfaceTmp.redimension(++TotSurfaces);

        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Vertex.allocate(4);

        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Name = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + ":2";
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Construction = IConst2;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ConstructionStoredInputValue = IConst2;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Class = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Azimuth = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Azimuth;
        // Sine and cosine of azimuth and tilt
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).SinAzim = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinAzim;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).CosAzim = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosAzim;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).SinTilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SinTilt;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).CosTilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).CosTilt;
        // Outward normal unit vector (pointing away from room)
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Centroid = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Centroid;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).lcsx = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsx;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).lcsy = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsy;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).lcsz = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).lcsz;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).NewellAreaVector = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NewellAreaVector;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).OutNormVec = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OutNormVec;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Reveal = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Reveal;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Shape = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Shape;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Sides = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Tilt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Tilt;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).HeatTransSurf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).BaseSurfName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurfName;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).BaseSurf = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ZoneName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ZoneName;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Zone = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ExtBoundCondName = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCondName;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ExtBoundCond = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtBoundCond;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ExtSolar = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ExtWind = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtWind;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ViewFactorGround = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGround;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ViewFactorSky = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSky;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ViewFactorGroundIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorGroundIR;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ViewFactorSkyIR = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ViewFactorSkyIR;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).OSCPtr = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).OSCPtr;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).SchedShadowSurfIndex = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedShadowSurfIndex;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).ShadowSurfSchedVaries = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ShadowSurfSchedVaries;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).MaterialMovInsulExt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulExt;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).MaterialMovInsulInt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).MaterialMovInsulInt;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).SchedMovInsulExt = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).SchedMovInsulExt;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).activeWindowShadingControl = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeWindowShadingControl;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).windowShadingControlList = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).HasShadeControl = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HasShadeControl;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).activeShadedConstruction = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeShadedConstruction;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).windowShadingControlList = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).windowShadingControlList;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).StormWinConstruction = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).StormWinConstruction;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).activeStormWinShadedConstruction = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).activeStormWinShadedConstruction;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).shadedStormWinConstructionList = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).shadedStormWinConstructionList;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).FrameDivider = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).FrameDivider;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Multiplier = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).NetAreaShadowCalc = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc;

        MulWidth = state.dataConstruction->Construct(IConst).W5FileMullionWidth;
        w2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysWidth;
        h2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysHeight;

        // Correction to net area of base surface. Add back in the original glazing area and subtract the
        // area of the two glazing systems. Note that for Surface(SurfNum)%Class = 'Window' the effect
        // of a window multiplier is included in the glazing area. Note that frame areas are subtracted later.

        AreaNew = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier * (h1 * w1 + h2 * w2); // both glazing systems
        // Adjust net area for base surface
        state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area -= AreaNew;

        // Net area of base surface with unity window multipliers (used in shadowing checks)
        state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= AreaNew / state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier;

        // Reset area, etc. of original window
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Multiplier * (h1 * w1);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).GrossArea = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Area;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).NetAreaShadowCalc = h1 * w1;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Perimeter = 2 * (h1 + w1);
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Height = h1;
        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Width = w1;
        // Set area, etc. of new window
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Area = state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Multiplier * (h2 * w2);
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).GrossArea = state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Area;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).NetAreaShadowCalc = h2 * w2;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Perimeter = 2 * (h2 + w2);
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Height = h2;
        state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Width = w2;

        if (state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
            ShowSevereError(state,
                            "SurfaceGeometry: ModifyWindow: Subsurfaces have too much area for base surface=" +
                                state.dataSurfaceGeometry->SurfaceTmp(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).BaseSurf).Name);
            ShowContinueError(state, "Subsurface (window) creating error=" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name);
            ShowContinueError(state,
                              format("This window has been replaced by two windows from the Window5 Data File of total area {:.2R} m2", AreaNew));
            ErrorsFound = true;
        }

        // Assign vertices to the new window; modify vertices of original window.
        // In the following, vertices are numbered counter-clockwise with vertex #1 at the upper left.

        if (state.dataConstruction->Construct(IConst).W5FileMullionOrientation == Vertical) {

            // VERTICAL MULLION: original window is modified to become left-hand glazing (system #1);
            // new window is created to become right-hand glazing (system #2)

            // Left-hand glazing

            // Vertex 1
            dx = 0.0;
            dy = H - h1;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(1).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(1).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = 0.0;
            dy = H;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(2).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(2).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1;
            dy = H;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(3).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(3).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1;
            dy = H - h1;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(4).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(4).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(4).z = za + (dx / W) * (zb - za);

            for (loop = 1; loop <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++loop) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(loop) = NewCoord.Vertex(loop);
            }

            // Right-hand glazing

            // Vertex 1
            dx = w1 + MulWidth;
            dy = H - (h1 + h2) / 2.0;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(1).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(1).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = w1 + MulWidth;
            dy = H + (h2 - h1) / 2.0;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(2).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(2).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1 + MulWidth + w2;
            dy = H + (h2 - h1) / 2.0;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(3).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(3).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1 + MulWidth + w2;
            dy = H - (h1 + h2) / 2.0;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(4).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(4).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(4).z = za + (dx / W) * (zb - za);

            for (loop = 1; loop <= state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Sides; ++loop) {
                state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Vertex(loop) = NewCoord.Vertex(loop);
            }

        } else { // Horizontal mullion

            // HORIZONTAL MULLION: original window is modified to become bottom glazing (system #1);
            // new window is created to become top glazing (system #2)

            // Bottom glazing

            // Vertex 1
            dx = 0.0;
            dy = H - h1;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(1).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(1).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = 0.0;
            dy = H;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(2).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(2).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = w1;
            dy = H;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(3).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(3).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = w1;
            dy = H - h1;
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(4).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(4).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(4).z = za + (dx / W) * (zb - za);

            for (loop = 1; loop <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++loop) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(loop) = NewCoord.Vertex(loop);
            }

            // Top glazing

            // Vertex 1
            dx = (w1 - w2) / 2.0;
            dy = H - (h1 + h2 + MulWidth);
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(1).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(1).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(1).z = za + (dx / W) * (zb - za);

            // Vertex 2
            dx = (w1 - w2) / 2.0;
            dy = H - (h1 + MulWidth);
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(2).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(2).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(2).z = za + (dx / W) * (zb - za);

            // Vertex 3
            dx = (w1 + w2) / 2.0;
            dy = H - (h1 + MulWidth);
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(3).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(3).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(3).z = za + (dx / W) * (zb - za);

            // Vertex 4
            dx = (w1 + w2) / 2.0;
            dy = H - (h1 + h2 + MulWidth);
            xa = OriginalCoord.Vertex(1).x + (dy / H) * (OriginalCoord.Vertex(2).x - OriginalCoord.Vertex(1).x);
            ya = OriginalCoord.Vertex(1).y + (dy / H) * (OriginalCoord.Vertex(2).y - OriginalCoord.Vertex(1).y);
            za = OriginalCoord.Vertex(1).z + (dy / H) * (OriginalCoord.Vertex(2).z - OriginalCoord.Vertex(1).z);
            xb = OriginalCoord.Vertex(4).x + (dy / H) * (OriginalCoord.Vertex(3).x - OriginalCoord.Vertex(4).x);
            yb = OriginalCoord.Vertex(4).y + (dy / H) * (OriginalCoord.Vertex(3).y - OriginalCoord.Vertex(4).y);
            zb = OriginalCoord.Vertex(4).z + (dy / H) * (OriginalCoord.Vertex(3).z - OriginalCoord.Vertex(4).z);
            NewCoord.Vertex(4).x = xa + (dx / W) * (xb - xa);
            NewCoord.Vertex(4).y = ya + (dx / W) * (yb - ya);
            NewCoord.Vertex(4).z = za + (dx / W) * (zb - za);

            for (loop = 1; loop <= state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Sides; ++loop) {
                state.dataSurfaceGeometry->SurfaceTmp(TotSurfaces).Vertex(loop) = NewCoord.Vertex(loop);
            }

        } // End of check if vertical or horizontal mullion
    }

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

        using namespace DataIPShortCuts;

        static std::string const CurrentModuleObject("GeometryTransform");

        Array1D_string cAlphas(1);
        Array1D<Real64> rNumerics(2);
        int NAlphas;
        int NNum;
        int IOStat;
        static Real64 OldAspectRatio;
        static Real64 NewAspectRatio;
        static std::string transformPlane;
        int n;
        Real64 Xo;
        Real64 XnoRot;
        Real64 Xtrans;
        Real64 Yo;
        Real64 YnoRot;
        Real64 Ytrans;
        // begin execution
        // get user input...

        if (state.dataSurfaceGeometry->firstTime) {
            if (inputProcessor->getNumObjectsFound(state, CurrentModuleObject) == 1) {
                inputProcessor->getObjectItem(state, CurrentModuleObject,
                                              1,
                                              cAlphas,
                                              NAlphas,
                                              rNumerics,
                                              NNum,
                                              IOStat,
                                              lNumericFieldBlanks,
                                              lAlphaFieldBlanks,
                                              cAlphaFieldNames,
                                              cNumericFieldNames);
                OldAspectRatio = rNumerics(1);
                NewAspectRatio = rNumerics(2);
                transformPlane = cAlphas(1);
                if (transformPlane != "XY") {
                    ShowWarningError(state, CurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + cAlphas(1) + "...ignored.");
                }
                state.dataSurfaceGeometry->firstTime = false;
                state.dataSurfaceGeometry->noTransform = false;
                AspectTransform = true;
                if (WorldCoordSystem) {
                    ShowWarningError(state, CurrentModuleObject + ": must use Relative Coordinate System.  Transform request ignored.");
                    state.dataSurfaceGeometry->noTransform = true;
                    AspectTransform = false;
                }
            } else {
                state.dataSurfaceGeometry->firstTime = false;
            }
        }
        if (state.dataSurfaceGeometry->noTransform) return;

        // check surface type.
        if (!state.dataSurfaceGeometry->SurfaceTmp(SurfNum).HeatTransSurf) {
            // Site Shading do not get transformed.
            if (state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F) return;
        }

        // testing method of transforming  x and y coordinates as follows

        // this works if not rotated wrt north axis ... but if it is, then trouble
        // try to first derotate it , transform by aspect and then rotate back.

        for (n = 1; n <= NSides; ++n) {
            Xo = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x; // world coordinates.... shifted by relative north angle...
            Yo = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y;
            // next derotate the building
            XnoRot = Xo * state.dataSurfaceGeometry->CosBldgRelNorth + Yo * state.dataSurfaceGeometry->SinBldgRelNorth;
            YnoRot = Yo * state.dataSurfaceGeometry->CosBldgRelNorth - Xo * state.dataSurfaceGeometry->SinBldgRelNorth;
            // translate
            Xtrans = XnoRot * std::sqrt(NewAspectRatio / OldAspectRatio);
            Ytrans = YnoRot * std::sqrt(OldAspectRatio / NewAspectRatio);
            // rerotate
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x = Xtrans * state.dataSurfaceGeometry->CosBldgRelNorth - Ytrans * state.dataSurfaceGeometry->SinBldgRelNorth;

            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y = Xtrans * state.dataSurfaceGeometry->SinBldgRelNorth + Ytrans * state.dataSurfaceGeometry->CosBldgRelNorth;
        }
    }

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

        using namespace Vectors;

        static Array1D<Vector> Triangle1(3); // working struct for a 3-sided surface
        static Array1D<Vector> Triangle2(3); // working struct for a 3-sided surface
        static Vector const zero_vector(0.0);
        Vector centroid;

        int negZcount(0); // for warning error in surface centroids

        // loop through all the surfaces
        for (int ThisSurf = 1; ThisSurf <= TotSurfaces; ++ThisSurf) {
            auto &surface(Surface(ThisSurf));

            if (surface.Class == SurfaceClass::IntMass) continue;

            auto const &vertex(surface.Vertex);

            {
                auto const SELECT_CASE_var(surface.Sides); // is this a 3- or 4-sided surface

                if (SELECT_CASE_var == 3) { // 3-sided polygon

                    centroid = cen(vertex(1), vertex(2), vertex(3));

                } else if (SELECT_CASE_var == 4) { // 4-sided polygon

                    // split into 2 3-sided polygons (Triangle 1 and Triangle 2)
                    Triangle1(1) = vertex(1);
                    Triangle1(2) = vertex(2);
                    Triangle1(3) = vertex(3);
                    Triangle2(1) = vertex(1);
                    Triangle2(2) = vertex(3);
                    Triangle2(3) = vertex(4);

                    // get total Area of quad.
                    Real64 TotalArea(surface.GrossArea);
                    if (TotalArea <= 0.0) {
                        // catch a problem....
                        ShowWarningError(state, "CalcSurfaceCentroid: zero area surface, for surface=" + surface.Name);
                        continue;
                    }

                    // get area fraction of triangles.
                    Real64 Tri1Area(AreaPolygon(3, Triangle1) / TotalArea);
                    Real64 Tri2Area(AreaPolygon(3, Triangle2) / TotalArea);

                    // check if sum of fractions are slightly greater than 1.0 which is a symptom of the triangles for a non-convex
                    // quadralateral using the wrong two triangles
                    if ((Tri1Area + Tri2Area) > 1.05) {

                        // if so repeat the process with the other two possible triangles (notice the vertices are in a different order this
                        // time) split into 2 3-sided polygons (Triangle 1 and Triangle 2)
                        Triangle1(1) = vertex(1);
                        Triangle1(2) = vertex(2);
                        Triangle1(3) = vertex(4);
                        Triangle2(1) = vertex(2);
                        Triangle2(2) = vertex(3);
                        Triangle2(3) = vertex(4);

                        // get area fraction of triangles.
                        Real64 AreaTriangle1 = AreaPolygon(3, Triangle1);
                        Real64 AreaTriangle2 = AreaPolygon(3, Triangle2);
                        TotalArea = AreaTriangle1 + AreaTriangle2;
                        Tri1Area = AreaTriangle1 / TotalArea;
                        Tri2Area = AreaTriangle2 / TotalArea;
                    }

                    // get centroid of Triangle 1
                    Vector cen1(cen(Triangle1(1), Triangle1(2), Triangle1(3)));

                    // get centroid of Triangle 2
                    Vector cen2(cen(Triangle2(1), Triangle2(2), Triangle2(3)));

                    // find area weighted combination of the two centroids (coded to avoid temporary Vectors)
                    cen1 *= Tri1Area;
                    cen2 *= Tri2Area;
                    centroid = cen1;
                    centroid += cen2;

                } else if ((SELECT_CASE_var >= 5)) { // multi-sided polygon
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
                        ShowWarningError(state, "CalcSurfaceCentroid: caught problem with # of sides, for surface=" + surface.Name);
                        ShowContinueError(state, format("... number of sides must be >= 3, this surface # sides={}", surface.Sides));
                    } else {
                        ShowWarningError(state, format("CalcSurfaceCentroid: caught problem with # of sides, for surface=#{}", ThisSurf));
                        ShowContinueError(state, "...surface name is blank. Examine surfaces -- this may be a problem with ill-formed interzone surfaces.");
                        ShowContinueError(state, format("... number of sides must be >= 3, this surface # sides={}", surface.Sides));
                    }
                    centroid = 0.0;
                }
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
    }

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
        using namespace DataIPShortCuts;

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

        // First collect names of surfaces referenced by active solar components
        cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
        NumOfFlatPlateUnits = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        NumPVTs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        cCurrentModuleObject = "Generator:Photovoltaic";
        NumPVs = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
        NumOfICSUnits = inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        NumCandidateNames = NumOfFlatPlateUnits + NumPVTs + NumPVs + NumOfICSUnits;
        NumOfCollectors = NumOfFlatPlateUnits + NumOfICSUnits;

        TmpCandidateSurfaceNames.allocate(NumCandidateNames);
        TmpCandidateICSSurfaceNames.allocate(NumOfCollectors);
        TmpCandidateICSBCTypeNames.allocate(NumOfCollectors);

        if (NumOfCollectors > 0) {
            cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
            for (CollectorNum = 1; CollectorNum <= NumOfFlatPlateUnits; ++CollectorNum) {

                inputProcessor->getObjectItem(state, cCurrentModuleObject, CollectorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);

                TmpCandidateSurfaceNames(CollectorNum) = cAlphaArgs(3);
                TmpCandidateICSBCTypeNames(CollectorNum) = "";
            }
        }

        if (NumPVTs > 0) {
            cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
            for (PVTnum = 1; PVTnum <= NumPVTs; ++PVTnum) {

                inputProcessor->getObjectItem(state, cCurrentModuleObject, PVTnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);

                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + PVTnum) = cAlphaArgs(2);
            }
        }

        if (NumPVs > 0) {
            cCurrentModuleObject = "Generator:Photovoltaic";
            for (PVnum = 1; PVnum <= NumPVs; ++PVnum) {
                inputProcessor->getObjectItem(state, cCurrentModuleObject, PVnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + PVnum) = cAlphaArgs(2);
            }
        }

        if (NumOfICSUnits > 0) {
            cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
            for (CollectorNum = 1; CollectorNum <= NumOfICSUnits; ++CollectorNum) {
                inputProcessor->getObjectItem(state, cCurrentModuleObject, CollectorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
                TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + NumPVs + CollectorNum) = cAlphaArgs(3);
                TmpCandidateICSSurfaceNames(NumOfFlatPlateUnits + CollectorNum) = cAlphaArgs(3);
                TmpCandidateICSBCTypeNames(NumOfFlatPlateUnits + CollectorNum) = cAlphaArgs(4);
            }
        }

        // loop through all the surfaces
        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

            Found = UtilityRoutines::FindItemInList(Surface(SurfNum).Name, TmpCandidateSurfaceNames, NumCandidateNames);
            if (Found > 0) {
                if (!Surface(SurfNum).HeatTransSurf) { // not BIPV, must be a shading surf with solar device
                    // Setup missing values to allow shading surfaces to model incident solar and wind
                    Surface(SurfNum).ExtSolar = true;
                    Surface(SurfNum).ExtWind = true;
                    Surface(SurfNum).ViewFactorGround = 0.5 * (1.0 - Surface(SurfNum).CosTilt);
                }
                // check if this surface is used for ICS collector mounting and has OthersideCondictionsModel as its
                // boundary condition
                if (NumOfICSUnits > 0) {
                    for (CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum) {
                        if (UtilityRoutines::SameString(Surface(SurfNum).Name, TmpCandidateICSSurfaceNames(CollectorNum)) &&
                            UtilityRoutines::SameString(TmpCandidateICSBCTypeNames(CollectorNum), "OTHERSIDECONDITIONSMODEL")) {
                            Surface(SurfNum).IsICS = true;
                            Surface(SurfNum).ICSPtr = CollectorNum;
                        }
                    }
                }

            } // end of IF (Found > 0) Then
        }
    }

    void SetupEnclosuresAndAirBoundaries(EnergyPlusData &state,
                                         Array1D<DataViewFactorInformation::ZoneViewFactorInformation> &Enclosures, // Radiant or Solar Enclosures
                                         SurfaceGeometry::enclosureType const &EnclosureType,                       // Radiant or Solar
                                         bool &ErrorsFound)                                                         // Set to true if errors found
    {
        std::string RoutineName = "SetupEnclosuresAndAirBoundaries";
        bool anyGroupedZones = false;
        bool radiantSetup = false;
        bool solarSetup = false;
        std::string RadiantOrSolar = "";
        int enclosureNum = 0;
        if (EnclosureType == RadiantEnclosures) {
            radiantSetup = true;
            RadiantOrSolar = "Radiant";
        } else if (EnclosureType == SolarEnclosures) {
            solarSetup = true;
            RadiantOrSolar = "Solar";
        } else {
            ShowFatalError(state, RoutineName + ": Illegal call to this function. Second argument must be 'RadiantEnclosures' or 'SolarEnclosures'");
        }
        if (std::any_of(state.dataConstruction->Construct.begin(), state.dataConstruction->Construct.end(), [](Construction::ConstructionProps const &e) { return e.TypeIsAirBoundary; })) {
            int errorCount = 0;
            for (int surfNum = 1; surfNum <= DataSurfaces::TotSurfaces; ++surfNum) {
                auto &surf(Surface(surfNum));
                if (surf.Construction == 0) continue;
                auto &constr(state.dataConstruction->Construct(surf.Construction));
                if (!constr.TypeIsAirBoundary) continue;
                surf.IsAirBoundarySurf = true;

                // Check for invalid air boundary surfaces - valid only on non-adiabatic interzone surfaces
                // Only check this once during radiant setup, skip for solar setup
                if (radiantSetup && (surf.ExtBoundCond <= 0 || surf.ExtBoundCond == surfNum)) {
                    ErrorsFound = true;
                    if (!state.dataGlobal->DisplayExtraWarnings) {
                        ++errorCount;
                    } else {
                        ShowSevereError(state, RoutineName + ": Surface=\"" + surf.Name + "\" uses Construction:AirBoundary in a non-interzone surface.");
                    }
                } else {
                    // Process air boundary - set surface properties and set up enclosures
                    // Radiant exchange
                    if (surf.IsAirBoundarySurf) {
                        // Boundary is grouped - assign enclosure
                        DataHeatBalance::AnyAirBoundary = true;
                        int thisSideEnclosureNum = 0;
                        int otherSideEnclosureNum = 0;
                        if (radiantSetup) {
                            // Radiant enclosure setup
                            constr.IsUsedCTF = false;
                            surf.HeatTransSurf = false;
                            surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel_AirBoundaryNoHT;
                            thisSideEnclosureNum = Zone(surf.Zone).RadiantEnclosureNum;
                            otherSideEnclosureNum = Zone(Surface(surf.ExtBoundCond).Zone).RadiantEnclosureNum;
                        } else {
                            // Solar enclosure setup
                            thisSideEnclosureNum = Zone(surf.Zone).SolarEnclosureNum;
                            otherSideEnclosureNum = Zone(Surface(surf.ExtBoundCond).Zone).SolarEnclosureNum;
                        }
                        anyGroupedZones = true;
                        if ((thisSideEnclosureNum == 0) && (otherSideEnclosureNum == 0)) {
                            // Neither zone is assigned to an enclosure, so increment the counter and assign to both
                            ++enclosureNum;
                            auto &thisEnclosure(Enclosures(enclosureNum));
                            thisSideEnclosureNum = enclosureNum;
                            thisEnclosure.Name = format("{} Enclosure {}", RadiantOrSolar, enclosureNum);
                            thisEnclosure.ZoneNames.push_back(surf.ZoneName);
                            thisEnclosure.ZoneNums.push_back(surf.Zone);
                            thisEnclosure.FloorArea += Zone(surf.Zone).FloorArea;
                            otherSideEnclosureNum = enclosureNum;
                            thisEnclosure.ZoneNames.push_back(Surface(surf.ExtBoundCond).ZoneName);
                            thisEnclosure.ZoneNums.push_back(Surface(surf.ExtBoundCond).Zone);
                            thisEnclosure.FloorArea += Zone(Surface(surf.ExtBoundCond).Zone).FloorArea;
                            if (radiantSetup) {
                                Zone(surf.Zone).RadiantEnclosureNum = thisSideEnclosureNum;
                                Zone(Surface(surf.ExtBoundCond).Zone).RadiantEnclosureNum = otherSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea += Zone(surf.Zone).ExtWindowArea;
                                thisEnclosure.TotalSurfArea += Zone(surf.Zone).TotalSurfArea;
                                thisEnclosure.ExtWindowArea += Zone(Surface(surf.ExtBoundCond).Zone).ExtWindowArea;
                                thisEnclosure.TotalSurfArea += Zone(Surface(surf.ExtBoundCond).Zone).TotalSurfArea;
                                Zone(surf.Zone).SolarEnclosureNum = thisSideEnclosureNum;
                                Zone(Surface(surf.ExtBoundCond).Zone).SolarEnclosureNum = otherSideEnclosureNum;
                            }
                        } else if (thisSideEnclosureNum == 0) {
                            // Other side is assigned, so use that one for both
                            thisSideEnclosureNum = otherSideEnclosureNum;
                            auto &thisEnclosure(Enclosures(thisSideEnclosureNum));
                            thisEnclosure.ZoneNames.push_back(surf.ZoneName);
                            thisEnclosure.ZoneNums.push_back(surf.Zone);
                            thisEnclosure.FloorArea += Zone(surf.Zone).FloorArea;
                            if (radiantSetup) {
                                Zone(surf.Zone).RadiantEnclosureNum = thisSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea += Zone(surf.Zone).ExtWindowArea;
                                thisEnclosure.TotalSurfArea += Zone(surf.Zone).TotalSurfArea;
                                Zone(surf.Zone).SolarEnclosureNum = thisSideEnclosureNum;
                            }
                        } else if (otherSideEnclosureNum == 0) {
                            // This side is assigned, so use that one for both
                            otherSideEnclosureNum = thisSideEnclosureNum;
                            auto &thisEnclosure(Enclosures(thisSideEnclosureNum));
                            thisEnclosure.ZoneNames.push_back(Surface(surf.ExtBoundCond).ZoneName);
                            thisEnclosure.ZoneNums.push_back(Surface(surf.ExtBoundCond).Zone);
                            thisEnclosure.FloorArea += Zone(Surface(surf.ExtBoundCond).Zone).FloorArea;
                            if (radiantSetup) {
                                Zone(Surface(surf.ExtBoundCond).Zone).RadiantEnclosureNum = otherSideEnclosureNum;
                            } else {
                                thisEnclosure.ExtWindowArea += Zone(Surface(surf.ExtBoundCond).Zone).ExtWindowArea;
                                thisEnclosure.TotalSurfArea += Zone(Surface(surf.ExtBoundCond).Zone).TotalSurfArea;
                                Zone(Surface(surf.ExtBoundCond).Zone).SolarEnclosureNum = otherSideEnclosureNum;
                            }
                        } else if (thisSideEnclosureNum != otherSideEnclosureNum) {
                            // If both sides are already assigned to an enclosure, then merge the two enclosures
                            auto &thisEnclosure(Enclosures(thisSideEnclosureNum));
                            auto &otherEnclosure(Enclosures(otherSideEnclosureNum));
                            for (const auto &zName : thisEnclosure.ZoneNames) {
                                otherEnclosure.ZoneNames.push_back(zName);
                            }
                            for (const auto &zNum : thisEnclosure.ZoneNums) {
                                otherEnclosure.ZoneNums.push_back(zNum);
                                if (radiantSetup) {
                                    Zone(zNum).RadiantEnclosureNum = otherSideEnclosureNum;
                                } else {
                                    Zone(zNum).SolarEnclosureNum = otherSideEnclosureNum;
                                }
                            }
                            otherEnclosure.FloorArea += thisEnclosure.FloorArea;
                            otherEnclosure.ExtWindowArea += thisEnclosure.ExtWindowArea;
                            otherEnclosure.TotalSurfArea += thisEnclosure.TotalSurfArea;
                            // Move any enclosures beyond thisEnclosure down one slot - at this point all enclosures are named "Radiant Enclosure N"
                            for (int enclNum = thisSideEnclosureNum; enclNum < enclosureNum; ++enclNum) {
                                std::string saveName = Enclosures(enclNum).Name;
                                Enclosures(enclNum) = Enclosures(enclNum + 1);
                                Enclosures(enclNum).Name = saveName;
                                for (auto zNum : thisEnclosure.ZoneNums) {
                                    if (radiantSetup) {
                                        Zone(zNum).RadiantEnclosureNum = enclNum;
                                    } else {
                                        Zone(zNum).SolarEnclosureNum = enclNum;
                                    }
                                }
                            }
                            // Clear the last rad enclosure and reduce the total number of enclosures by 1
                            Enclosures(enclosureNum).Name.clear();
                            Enclosures(enclosureNum).ZoneNames.clear();
                            Enclosures(enclosureNum).ZoneNums.clear();
                            Enclosures(enclosureNum).FloorArea = 0;
                            Enclosures(enclosureNum).ExtWindowArea = 0;
                            Enclosures(enclosureNum).TotalSurfArea = 0;
                            enclosureNum -= 1;
                        }
                    } else {
                        ErrorsFound = true;
                        ShowSevereError(state, RoutineName + ": Surface=" + surf.Name + " uses Construction:AirBoundary with illegal option:");
                        if (radiantSetup) {
                            ShowContinueError(state, "Radiant Exchange Method must be either GroupedZones or IRTSurface.");
                        } else {
                            ShowContinueError(state, "Solar and Daylighting Method must be either GroupedZones or InteriorWindow");
                        }
                    }
                    if (solarSetup && constr.TypeIsAirBoundaryMixing) {
                        // Set up mixing air boundaries only once, during solar setup
                        int zoneNum1 = min(surf.Zone, Surface(surf.ExtBoundCond).Zone);
                        int zoneNum2 = min(surf.Zone, Surface(surf.ExtBoundCond).Zone);
                        // This pair already saved?
                        bool found = false;
                        for (int n = 0; n < DataHeatBalance::NumAirBoundaryMixing - 1; ++n) {
                            if ((zoneNum1 == DataHeatBalance::AirBoundaryMixingZone1[n]) &&
                                (zoneNum2 == DataHeatBalance::AirBoundaryMixingZone2[n])) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            // Store the zone pairs to use later to create cross mixing objects
                            ++DataHeatBalance::NumAirBoundaryMixing;
                            DataHeatBalance::AirBoundaryMixingZone1.push_back(zoneNum1);
                            DataHeatBalance::AirBoundaryMixingZone2.push_back(zoneNum2);
                            DataHeatBalance::AirBoundaryMixingSched.push_back(state.dataConstruction->Construct(surf.Construction).AirBoundaryMixingSched);
                            Real64 mixingVol = state.dataConstruction->Construct(surf.Construction).AirBoundaryACH * min(Zone(zoneNum1).Volume, Zone(zoneNum2).Volume) /
                                               DataGlobalConstants::SecInHour;
                            DataHeatBalance::AirBoundaryMixingVol.push_back(mixingVol);
                        }
                    }
                }
            }
            if (errorCount > 0) {
                ShowSevereError(state, format("{}: {} surfaces use Construction:AirBoundary in non-interzone surfaces.", RoutineName, errorCount));
                ShowContinueError(state, "For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
            }
        }
        if (anyGroupedZones) {
            // All grouped zones have been assigned to an enclosure, now assign remaining zones
            for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
                int zoneEnclosureNum = 0;
                if (radiantSetup) {
                    zoneEnclosureNum = Zone(zoneNum).RadiantEnclosureNum;
                } else {
                    zoneEnclosureNum = Zone(zoneNum).SolarEnclosureNum;
                }
                if (zoneEnclosureNum == 0) {
                    ++enclosureNum;
                    if (radiantSetup) {
                        Zone(zoneNum).RadiantEnclosureNum = enclosureNum;
                    } else {
                        Zone(zoneNum).SolarEnclosureNum = enclosureNum;
                    }
                    auto &thisEnclosure(Enclosures(enclosureNum));
                    thisEnclosure.Name = Zone(zoneNum).Name;
                    thisEnclosure.ZoneNames.push_back(Zone(zoneNum).Name);
                    thisEnclosure.ZoneNums.push_back(zoneNum);
                    thisEnclosure.FloorArea = Zone(zoneNum).FloorArea;
                    thisEnclosure.ExtWindowArea = Zone(zoneNum).ExtWindowArea;
                    thisEnclosure.TotalSurfArea = Zone(zoneNum).TotalSurfArea;
                }
            }
            if (radiantSetup) {
                DataViewFactorInformation::NumOfRadiantEnclosures = enclosureNum;
            } else {
                DataViewFactorInformation::NumOfSolarEnclosures = enclosureNum;
            }
        } else {
            // There are no grouped radiant air boundaries, assign each zone to it's own radiant enclosure
            for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
                auto &thisEnclosure(Enclosures(zoneNum));
                thisEnclosure.Name = Zone(zoneNum).Name;
                thisEnclosure.ZoneNames.push_back(Zone(zoneNum).Name);
                thisEnclosure.ZoneNums.push_back(zoneNum);
                thisEnclosure.FloorArea = Zone(zoneNum).FloorArea;
                if (radiantSetup) {
                    Zone(zoneNum).RadiantEnclosureNum = zoneNum;
                } else {
                    Zone(zoneNum).SolarEnclosureNum = zoneNum;
                    thisEnclosure.ExtWindowArea = Zone(zoneNum).ExtWindowArea;
                    thisEnclosure.TotalSurfArea = Zone(zoneNum).TotalSurfArea;
                }
            }
            if (radiantSetup) {
                DataViewFactorInformation::NumOfRadiantEnclosures = state.dataGlobal->NumOfZones;
            } else {
                DataViewFactorInformation::NumOfSolarEnclosures = state.dataGlobal->NumOfZones;
            }
        }
    }

    void CheckConvexity(EnergyPlusData &state, int const SurfNum, // Current surface number
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

        int n;                    // Loop index
        int Np1;                  // Loop index
        int Np2;                  // Loop index
        Real64 Det;               // Determinant for picking projection plane
        Real64 DotProd;           // Dot product for determining angle
        Real64 Theta;             // Angle between edge vectors
        Real64 LastTheta;         // Angle between edge vectors
        Real64 V1len;             // Edge vector length
        Real64 V2len;             // Edge vector length
        bool SignFlag;            // Direction of edge turn : true is right, false is left
        bool PrevSignFlag(false); // Container for the sign of the previous iteration's edge turn
        static Array1D<Real64> X; // containers for x,y,z vertices of the surface
        static Array1D<Real64> Y;
        static Array1D<Real64> Z;
        static Array1D<Real64> A; // containers for convexity test
        static Array1D<Real64> B;
        static Array1D_int SurfCollinearVerts; // Array containing indices of collinear vertices
        static int VertSize;                   // size of X,Y,Z,A,B arrays
        Real64 cosarg;
        int M;   // Array index for SurfCollinearVerts container
        int J;   // Loop index
        int K;   // Loop index
        int Ind; // Location of surface vertex to be removed
        static Real64 ACosZero; // set on firstTime
        bool SurfCollinearWarning;
        static std::string ErrLineOut; // Character string for producing error messages

        if (state.dataSurfaceGeometry->CheckConvexityFirstTime) {
            ACosZero = std::acos(0.0);
            X.allocate(MaxVerticesPerSurface + 2);
            Y.allocate(MaxVerticesPerSurface + 2);
            Z.allocate(MaxVerticesPerSurface + 2);
            A.allocate(MaxVerticesPerSurface + 2);
            B.allocate(MaxVerticesPerSurface + 2);
            SurfCollinearVerts.allocate(MaxVerticesPerSurface);
            VertSize = MaxVerticesPerSurface;
            state.dataSurfaceGeometry->CheckConvexityFirstTime = false;
        }

        if (NSides > VertSize) {
            X.deallocate();
            Y.deallocate();
            Z.deallocate();
            A.deallocate();
            B.deallocate();
            SurfCollinearVerts.deallocate();
            X.allocate(MaxVerticesPerSurface + 2);
            Y.allocate(MaxVerticesPerSurface + 2);
            Z.allocate(MaxVerticesPerSurface + 2);
            A.allocate(MaxVerticesPerSurface + 2);
            B.allocate(MaxVerticesPerSurface + 2);
            SurfCollinearVerts.allocate(MaxVerticesPerSurface);
            VertSize = MaxVerticesPerSurface;
        }

        for (n = 1; n <= NSides; ++n) {
            X(n) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).x;
            Y(n) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).y;
            Z(n) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n).z;
        }
        X(NSides + 1) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).x;
        Y(NSides + 1) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).y;
        Z(NSides + 1) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(1).z;
        X(NSides + 2) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2).x;
        Y(NSides + 2) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2).y;
        Z(NSides + 2) = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(2).z;

        // Determine a suitable plane in which to do the tests
        Det = 0.0;
        for (n = 1; n <= NSides; ++n) {
            Det += X(n) * Y(n + 1) - X(n + 1) * Y(n);
        }
        if (std::abs(Det) > 1.e-4) {
            A = X;
            B = Y;
        } else {
            Det = 0.0;
            for (n = 1; n <= NSides; ++n) {
                Det += X(n) * Z(n + 1) - X(n + 1) * Z(n);
            }
            if (std::abs(Det) > 1.e-4) {
                A = X;
                B = Z;
            } else {
                Det = 0.0;
                for (n = 1; n <= NSides; ++n) {
                    Det += Y(n) * Z(n + 1) - Y(n + 1) * Z(n);
                }
                if (std::abs(Det) > 1.e-4) {
                    A = Y;
                    B = Z;
                } else {
                    // This condition should not be reached if the surfaces are guaranteed to be planar already
                    ShowSevereError(state, "CheckConvexity: Surface=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\" is non-planar.");
                    ShowContinueError(state, "Coincident Vertices will be removed as possible.");
                    for (n = 1; n <= state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides; ++n) {
                        auto const &point(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(n));
                        static constexpr auto ErrFmt(" ({:8.3F},{:8.3F},{:8.3F})");
                        ShowContinueError(state, format(ErrFmt, point.x, point.y, point.z));
                    }
                }
            }
        }

        M = 0;
        SurfCollinearWarning = false;
        for (n = 1; n <= NSides; ++n) { // perform convexity test in the plane determined above.
            V1len = std::sqrt(pow_2(A(n + 1) - A(n)) + pow_2(B(n + 1) - B(n)));
            V2len = std::sqrt(pow_2(A(n + 2) - A(n + 1)) + pow_2(B(n + 2) - B(n + 1)));
            if (V1len <= 1.e-8 || V2len <= 1.e-8) continue;
            DotProd = (A(n + 1) - A(n)) * (B(n + 2) - B(n + 1)) - (B(n + 1) - B(n)) * (A(n + 2) - A(n + 1));
            cosarg = DotProd / (V1len * V2len);
            if (cosarg < -1.0) {
                cosarg = -1.0;
            } else if (cosarg > 1.0) {
                cosarg = 1.0;
            }
            Theta = std::acos(cosarg);
            if (Theta < (ACosZero - TurnThreshold)) {
                SignFlag = true;
            } else {
                if (Theta > (ACosZero + TurnThreshold)) {
                    SignFlag = false;
                } else { // Store the index of the collinear vertex for removal
                    if (!SurfCollinearWarning) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state, "CheckConvexity: Surface=\"" + state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\", Collinear points have been removed.");
                        }
                        SurfCollinearWarning = true;
                    }
                    ++state.dataErrTracking->TotalCoincidentVertices;
                    ++M;
                    SurfCollinearVerts(M) = n + 1;
                    continue;
                }
            }

            if (n == 1) {
                PrevSignFlag = SignFlag;
                LastTheta = Theta;
                continue;
            }

            if (SignFlag != PrevSignFlag) {
                if (SolarDistribution != MinimalShadowing && state.dataSurfaceGeometry->SurfaceTmp(SurfNum).ExtSolar) {
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state, "CheckConvexity: Zone=\"" + Zone(state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Zone).Name + "\", Surface=\"" +
                                         state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name + "\" is non-convex.");
                        Np1 = n + 1;
                        if (Np1 > NSides) Np1 -= NSides;
                        Np2 = n + 2;
                        if (Np2 > NSides) Np2 -= NSides;
                        ShowContinueError(state, format("...vertex {} to vertex {} to vertex {}", n, Np1, Np2));
                        ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", n, X(n), Y(n), Z(n)));
                        ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", Np1, X(n + 1), Y(n + 1), Z(n + 1)));
                        ShowContinueError(state, format("...vertex {}=[{:.2R},{:.2R},{:.2R}]", Np2, X(n + 2), Y(n + 2), Z(n + 2)));
                        //          CALL ShowContinueError(state, '...theta angle=['//TRIM(format("{:.6R}", Theta))//']')
                        //          CALL ShowContinueError(state, '...last theta angle=['//TRIM(format("{:.6R}", LastTheta))//']')
                    }
                }
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).IsConvex = false;
                break;
            }
            PrevSignFlag = SignFlag;
            LastTheta = Theta;
        }

        // must check to make sure don't remove NSides below 3
        if (M > 0) { // Remove the collinear points determined above
            if (NSides - M > 2) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = NSides - M;
            } else { // too many
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(
                        state,
                        format("CheckConvexity: Surface=\"{}\" has [{}] collinear points.", state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name, M));
                    ShowContinueError(state, "...too many to remove all.  Will leave the surface with 3 sides. But this is now a degenerate surface");
                }
                ++state.dataErrTracking->TotalDegenerateSurfaces;
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides = max(NSides - M, 3);
                M = NSides - state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides;
            }
            for (J = 1; J <= M; ++J) {
                Ind = SurfCollinearVerts(J);
                if (Ind > NSides) {
                    Ind = Ind - NSides + M - 1;
                }
                for (K = Ind; K <= NSides - 1; ++K) {
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 1).x = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 2).x;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 1).y = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 2).y;
                    state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 1).z = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(K - J + 2).z;
                }
            }
            // remove duplicated points and resize Vertex
            Array1D<Vector> OldVertex;
            OldVertex.allocate(NSides);
            OldVertex = state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex;
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.deallocate();
            state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex.allocate(NSides - M);
            for (J = 1; J <= NSides - M; ++J) {
                state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Vertex(J) = OldVertex(J);
            }
            OldVertex.deallocate();
            if (state.dataGlobal->DisplayExtraWarnings) {
                ShowWarningError(state,
                                 format("CheckConvexity: Surface=\"{}\": The vertex points has been reprocessed as Sides = {}",
                                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Name,
                                        state.dataSurfaceGeometry->SurfaceTmp(SurfNum).Sides));
            }
        }
    }

    bool isRectangle(int const ThisSurf // Surface number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         M.J. Witte
        //       DATE WRITTEN   October 2015

        // PURPOSE: Check if a 4-sided surface is a rectangle

        using namespace Vectors;

        Real64 Diagonal1;                                      // Length of diagonal of 4-sided figure from vertex 1 to vertex 3 (m)
        Real64 Diagonal2;                                      // Length of diagonal of 4-sided figure from vertex 2 to vertex 4 (m)
        Real64 DotProd;                                        // Dot product of two adjacent sides - to test for right angle
        Real64 const cos89deg = std::cos(89.0 * DataGlobalConstants::DegToRadians); // tolerance for right angle
        Vector Vect32;                                         // normalized vector from vertex 3 to vertex 2
        Vector Vect21;                                         // normalized vector from vertex 2 to vertex 1

        Diagonal1 = VecLength(Surface(ThisSurf).Vertex(1) - Surface(ThisSurf).Vertex(3));
        Diagonal2 = VecLength(Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(4));
        // Test for rectangularity
        if (std::abs(Diagonal1 - Diagonal2) < 0.020) { // This tolerance based on coincident vertex tolerance of 0.01
            Vect32 = VecNormalize(Surface(ThisSurf).Vertex(3) - Surface(ThisSurf).Vertex(2));
            Vect21 = VecNormalize(Surface(ThisSurf).Vertex(2) - Surface(ThisSurf).Vertex(1));
            DotProd = dot(Vect32, Vect21);
            if (std::abs(DotProd) <= cos89deg) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    void MakeEquivalentRectangle(int const SurfNum, // Surface number
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

        Real64 BaseCosAzimuth;
        Real64 BaseCosTilt;
        Real64 BaseSinAzimuth;
        Real64 BaseSinTilt;
        Real64 SurfWorldAz;
        Real64 SurfTilt;
        Real64 AspectRatio;  // Aspect ratio
        Real64 NumSurfSides; // Number of surface sides
        Real64 WidthEff;     // Effective width of the surface
        Real64 WidthMax;     // X difference between the vertex on the most left and the one on the most right
        Real64 HeightEff;    // Effective height of the surface
        Real64 HeightMax;    // Y difference between the lowest and toppest vertices
        Real64 Xp;
        Real64 Yp;
        Real64 Zp;
        Real64 XLLC;
        Real64 YLLC;
        Real64 ZLLC;

        if (SurfNum == 0) {
            // invalid surface
            ErrorsFound = true;
            return;
        } else if (Surface(SurfNum).Sides != 4) {
            // the method is designed for 4-sided surface
            return;
        } else if (isRectangle(SurfNum)) {
            // no need to transform
            return;
        }

        SurfWorldAz = Surface(SurfNum).Azimuth;
        SurfTilt = Surface(SurfNum).Tilt;
        BaseCosAzimuth = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians);
        BaseSinAzimuth = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians);
        BaseCosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians);
        BaseSinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians);
        NumSurfSides = Surface(SurfNum).Sides;

        // Calculate WidthMax and HeightMax
        WidthMax = 0.0;
        HeightMax = 0.0;
        for (int i = 1; i < NumSurfSides; ++i) {
            for (int j = i + 1; j <= NumSurfSides; ++j) {

                Xp = Surface(SurfNum).Vertex(j).x - Surface(SurfNum).Vertex(i).x;
                Yp = Surface(SurfNum).Vertex(j).y - Surface(SurfNum).Vertex(i).y;
                Zp = Surface(SurfNum).Vertex(j).z - Surface(SurfNum).Vertex(i).z;

                XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

                if (std::abs(XLLC) > WidthMax) WidthMax = std::abs(XLLC);
                if (std::abs(YLLC) > WidthMax) HeightMax = std::abs(YLLC);
            }
        }

        // Perform transformation by calculating WidthEff and HeightEff
        if ((WidthMax > 0) && (HeightMax > 0)) {
            AspectRatio = WidthMax / HeightMax;
        } else {
            AspectRatio = 1;
        }
        WidthEff = std::sqrt(Surface(SurfNum).Area * AspectRatio);
        HeightEff = std::sqrt(Surface(SurfNum).Area / AspectRatio);

        // Assign the effective width and length to the surface
        Surface(SurfNum).Width = WidthEff;
        Surface(SurfNum).Height = HeightEff;
    }


    void CheckForReversedLayers(EnergyPlusData &state,
                                bool &RevLayerDiffs,    // true when differences are discovered in interzone constructions
                                int const ConstrNum,    // construction index
                                int const ConstrNumRev, // construction index for reversed construction
                                int const TotalLayers   // total layers for construction definition
    )
    {

        RevLayerDiffs = false;

        for (int LayerNo = 1; LayerNo <= TotalLayers; ++LayerNo) {
            auto &thisConstLayer(state.dataConstruction->Construct(ConstrNum).LayerPoint(LayerNo));
            auto &revConstLayer(state.dataConstruction->Construct(ConstrNumRev).LayerPoint(TotalLayers - LayerNo + 1));
            auto &thisMatLay(state.dataMaterial->Material(thisConstLayer));
            auto &revMatLay(state.dataMaterial->Material(revConstLayer));
            if ((thisConstLayer != revConstLayer) ||    // Not pointing to the same layer
                (thisMatLay.Group == WindowGlass) ||    // Not window glass or glass equivalent layer which have
                (revMatLay.Group == WindowGlass) ||     // to have certain properties flipped from front to back
                (thisMatLay.Group == GlassEquivalentLayer) ||
                (revMatLay.Group == GlassEquivalentLayer)) {
                // If not point to the same layer, check to see if this is window glass which might need to have
                // front and back material properties reversed.
                Real64 const SmallDiff = 0.0001;
                if ((thisMatLay.Group == WindowGlass) && (revMatLay.Group == WindowGlass)) {
                    // Both layers are window glass, so need to check to see if the properties are reversed
                    if ((abs(thisMatLay.Thickness - revMatLay.Thickness) > SmallDiff) ||
                        (abs(thisMatLay.ReflectSolBeamBack - revMatLay.ReflectSolBeamFront) > SmallDiff) ||
                        (abs(thisMatLay.ReflectSolBeamFront - revMatLay.ReflectSolBeamBack) > SmallDiff) ||
                        (abs(thisMatLay.TransVis - revMatLay.TransVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflectVisBeamBack - revMatLay.ReflectVisBeamFront) > SmallDiff) ||
                        (abs(thisMatLay.ReflectVisBeamFront - revMatLay.ReflectVisBeamBack) > SmallDiff) ||
                        (abs(thisMatLay.TransThermal - revMatLay.TransThermal) > SmallDiff) ||
                        (abs(thisMatLay.AbsorpThermalBack - revMatLay.AbsorpThermalFront) > SmallDiff) ||
                        (abs(thisMatLay.AbsorpThermalFront - revMatLay.AbsorpThermalBack) > SmallDiff) ||
                        (abs(thisMatLay.Conductivity - revMatLay.Conductivity) > SmallDiff) ||
                        (abs(thisMatLay.GlassTransDirtFactor - revMatLay.GlassTransDirtFactor) > SmallDiff) ||
                        (thisMatLay.SolarDiffusing != revMatLay.SolarDiffusing) ||
                        (abs(thisMatLay.YoungModulus - revMatLay.YoungModulus) > SmallDiff) ||
                        (abs(thisMatLay.PoissonsRatio - revMatLay.PoissonsRatio) > SmallDiff)) {
                        RevLayerDiffs = true;
                        break; // exit when diff
                    }   // If none of the above conditions is met, then these should be the same layers in reverse (RevLayersDiffs = false)
                } else if ((thisMatLay.Group == GlassEquivalentLayer) && (revMatLay.Group == GlassEquivalentLayer)) {
                    if ((abs(thisMatLay.TausBackBeamBeam - revMatLay.TausFrontBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay.TausFrontBeamBeam - revMatLay.TausBackBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackBeamBeam - revMatLay.ReflFrontBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontBeamBeam - revMatLay.ReflBackBeamBeam) > SmallDiff) ||
                        (abs(thisMatLay.TausBackBeamBeamVis - revMatLay.TausFrontBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay.TausFrontBeamBeamVis - revMatLay.TausBackBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackBeamBeamVis - revMatLay.ReflFrontBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontBeamBeamVis - revMatLay.ReflBackBeamBeamVis) > SmallDiff) ||
                        (abs(thisMatLay.TausBackBeamDiff - revMatLay.TausFrontBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay.TausFrontBeamDiff - revMatLay.TausBackBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackBeamDiff - revMatLay.ReflFrontBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontBeamDiff - revMatLay.ReflBackBeamDiff) > SmallDiff) ||
                        (abs(thisMatLay.TausBackBeamDiffVis - revMatLay.TausFrontBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.TausFrontBeamDiffVis - revMatLay.TausBackBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackBeamDiffVis - revMatLay.ReflFrontBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontBeamDiffVis - revMatLay.ReflBackBeamDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.TausDiffDiff - revMatLay.TausDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackDiffDiff - revMatLay.ReflFrontDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontDiffDiff - revMatLay.ReflBackDiffDiff) > SmallDiff) ||
                        (abs(thisMatLay.TausDiffDiffVis - revMatLay.TausDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflBackDiffDiffVis - revMatLay.ReflFrontDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.ReflFrontDiffDiffVis - revMatLay.ReflBackDiffDiffVis) > SmallDiff) ||
                        (abs(thisMatLay.TausThermal - revMatLay.TausThermal) > SmallDiff) ||
                        (abs(thisMatLay.EmissThermalBack - revMatLay.EmissThermalFront) > SmallDiff) ||
                        (abs(thisMatLay.EmissThermalFront - revMatLay.EmissThermalBack) > SmallDiff) ||
                        (abs(thisMatLay.Resistance - revMatLay.Resistance) > SmallDiff)) {
                        RevLayerDiffs = true;
                        break; // exit when diff
                    }   // If none of the above conditions is met, then these should be the same layers in reverse (RevLayersDiffs = false)
                } else {
                    // Other material types do not have reversed constructions so if they are not the same layer there is a problem (RevLayersDiffs = true)
                    RevLayerDiffs = true;
                    break; // exit when diff
                }       // End check of whether or not these are WindowGlass
            }           // else: thisConstLayer is the same as revConstLayer--so there is no problem (RevLayersDiffs = false)
        }
    }

} // namespace SurfaceGeometry

} // namespace EnergyPlus
