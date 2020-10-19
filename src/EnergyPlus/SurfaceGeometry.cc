// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataGlobals;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using DataWindowEquivalentLayer::CFSMAXNL;

    // Use statements for access to subroutines in other modules
    // na

    // Data
    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;
    Array1D_string const BaseSurfCls(3, {"WALL", "FLOOR", "ROOF"});
    Array1D_string const SubSurfCls(6, {"WINDOW", "DOOR", "GLASSDOOR", "SHADING", "TUBULARDAYLIGHTDOME", "TUBULARDAYLIGHTDIFFUSER"});
    Array1D<SurfaceClass> const BaseSurfIDs(3, {SurfaceClass::Wall, SurfaceClass::Floor, SurfaceClass::Roof});

    Array1D<SurfaceClass> const SubSurfIDs(
        6, {SurfaceClass::Window, SurfaceClass::Door, SurfaceClass::GlassDoor, SurfaceClass::Shading, SurfaceClass::TDD_Dome, SurfaceClass::TDD_Diffuser});

    int const UnenteredAdjacentZoneSurface(-998); // allows users to enter one zone surface ("Zone")
    // referencing another in adjacent zone
    int const UnreconciledZoneSurface(-999); // interim value between entering surfaces ("Surface") and reconciling
    // surface names in other zones

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool ProcessSurfaceVerticesOneTimeFlag(true);
        int checkSubSurfAzTiltNormErrCount(0);
        Array1D<Real64> Xpsv;
        Array1D<Real64> Ypsv;
        Array1D<Real64> Zpsv;

        bool GetSurfaceDataOneTimeFlag(false);
        std::unordered_map<std::string, std::string> UniqueSurfaceNames;
        bool firstTime(true);
        bool noTransform(true);
        bool CheckConvexityFirstTime(true);
    } // namespace

    // Following are used only during getting vertices, so are module variables here.
    Real64 CosBldgRelNorth(0.0);     // Cosine of the building rotation (relative north) (includes appendix G rotation)
    Real64 SinBldgRelNorth(0.0);     // Sine of the building rotation (relative north)   (includes appendix G rotation)
    Real64 CosBldgRotAppGonly(0.0);  // Cosine of the building rotation for appendix G only(relative north)
    Real64 SinBldgRotAppGonly(0.0);  // Sine of the building rotation for appendix G only (relative north)
    Array1D<Real64> CosZoneRelNorth; // Cosine of the zone rotation (relative north)
    Array1D<Real64> SinZoneRelNorth; // Sine of the zone rotation (relative north)

    bool NoGroundTempObjWarning(true); // This will cause a warning to be issued if surfaces with "Ground"
    // outside environment are used but no ground temperature object was input.
    bool NoFCGroundTempObjWarning(true); // This will cause a warning to be issued if surfaces with "GroundFCfactorMethod"
    // outside environment are used but no FC ground temperatures was input.
    bool
        RectSurfRefWorldCoordSystem(false); // GlobalGeometryRules:Field Rectangular Surface Coordinate System (A5) = World (true) or Relative (false)
    int Warning1Count(0);                   // counts of Modify Window 5/6 windows
    int Warning2Count(0);                   // counts of overriding exterior windows with Window 5/6 glazing systems
    int Warning3Count(0);                   // counts of overriding interior windows with Window 5/6 glazing systems

    // SUBROUTINE SPECIFICATIONS FOR MODULE SurfaceGeometry

    // Object Data
    Array1D<SurfaceData> SurfaceTmp; // Allocated/Deallocated during input processing
    Array1D<bool> SurfaceTmpClassInvalid; // Tmp class is invalid
    Array1D<bool> SurfaceTmpClassMoved; // Tmp class is invalid
    HeatBalanceKivaManager::KivaManager kivaManager;
    ExposedFoundationPerimeter exposedFoundationPerimeter;

    // Functions

    // Clears the global data in HeatBalanceManager.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        ProcessSurfaceVerticesOneTimeFlag = true;
        checkSubSurfAzTiltNormErrCount = 0;
        Xpsv.deallocate();
        Ypsv.deallocate();
        Zpsv.deallocate();
        // Following are used only during getting vertices, so are module variables here.
        CosBldgRelNorth = 0.0;
        SinBldgRelNorth = 0.0;
        CosBldgRotAppGonly = 0.0;
        SinBldgRotAppGonly = 0.0;
        CosZoneRelNorth.deallocate();
        SinZoneRelNorth.deallocate();
        NoGroundTempObjWarning = true;
        NoFCGroundTempObjWarning = true;
        RectSurfRefWorldCoordSystem = false;
        Warning1Count = 0;
        Warning2Count = 0;
        Warning3Count = 0;
        SurfaceTmp.deallocate();
        SurfaceTmpClassInvalid.deallocate();
        SurfaceTmpClassMoved.deallocate();
        GetSurfaceDataOneTimeFlag = false;
        UniqueSurfaceNames.clear();
        kivaManager = HeatBalanceKivaManager::KivaManager();
        firstTime = true;
        noTransform = true;
        CheckConvexityFirstTime = true;
    }

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
        using General::RoundSigDigits;
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
        CosBldgRelNorth = std::cos(-(BuildingAzimuth + BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians());
        SinBldgRelNorth = std::sin(-(BuildingAzimuth + BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians());

        // these are only for Building Rotation for Appendix G when using world coordinate system
        CosBldgRotAppGonly = std::cos(-BuildingRotationAppendixG * DataGlobalConstants::DegToRadians());
        SinBldgRotAppGonly = std::sin(-BuildingRotationAppendixG * DataGlobalConstants::DegToRadians());

        CosZoneRelNorth.allocate(NumOfZones);
        SinZoneRelNorth.allocate(NumOfZones);

        ZoneCeilingHeightEntered.dimension(NumOfZones, false);
        ZoneCeilingArea.dimension(NumOfZones, 0.0);

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            CosZoneRelNorth(ZoneNum) = std::cos(-Zone(ZoneNum).RelNorth * DataGlobalConstants::DegToRadians());
            SinZoneRelNorth(ZoneNum) = std::sin(-Zone(ZoneNum).RelNorth * DataGlobalConstants::DegToRadians());
        }
        GetSurfaceData(state, ErrorsFound);

        if (ErrorsFound) {
            CosZoneRelNorth.deallocate();
            SinZoneRelNorth.deallocate();
            return;
        }

        GetWindowGapAirflowControlData(state, ErrorsFound);

        GetStormWindowData(state, ErrorsFound);

        if (!ErrorsFound && TotStormWin > 0) CreateStormWindowConstructions(state);

        SetFlagForWindowConstructionWithShadeOrBlindLayer(state);

        CosZoneRelNorth.deallocate();
        SinZoneRelNorth.deallocate();

        AllocateModuleArrays(); // This needs to be moved to the main manager routine of SSG at a later date

        AirSkyRadSplit.dimension(TotSurfaces, 0.0);

        CalcWindowRevealReflection = false; // Set to True in ProcessSurfaceVertices if beam solar reflection from window reveals
        // is requested for one or more exterior windows.
        BuildingShadingCount = 0;
        FixedShadingCount = 0;
        AttachedShadingCount = 0;

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) { // Loop through all surfaces...

            AirSkyRadSplit(SurfNum) = std::sqrt(0.5 * (1.0 + Surface(SurfNum).CosTilt));

            // Set flag that determines whether a surface is a shadowing surface
            Surface(SurfNum).ShadowingSurf = false;
            if (Surface(SurfNum).Class == SurfaceClass::Shading || Surface(SurfNum).Class == SurfaceClass::Detached_F ||
                Surface(SurfNum).Class == SurfaceClass::Detached_B)
                Surface(SurfNum).ShadowingSurf = true;
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

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
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
            for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
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
                        if (ErrCount == 1 && !DisplayExtraWarnings) {
                            ShowWarningError(RoutineName +
                                             "Entered Ceiling Height for some zone(s) significantly different from calculated Ceiling Height");
                            ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded.");
                        }
                        if (DisplayExtraWarnings) {
                            ShowWarningError(RoutineName + "Entered Ceiling Height for Zone=\"" + Zone(ZoneNum).Name +
                                             "\" significantly different from calculated Ceiling Height");
                            static constexpr auto ValFmt("{:.2F}");
                            String1 = format(ValFmt, Zone(ZoneNum).CeilingHeight);
                            String2 = format(ValFmt, AverageHeight);
                            ShowContinueError(RoutineName + "Entered Ceiling Height=" + String1 + ", Calculated Ceiling Height=" + String2 +
                                              ", entered height will be used in calculations.");
                        }
                    }
                }
            }
            if ((Zone(ZoneNum).CeilingHeight <= 0.0) && (AverageHeight > 0.0)) Zone(ZoneNum).CeilingHeight = AverageHeight;
        }

        CalculateZoneVolume(state, ZoneCeilingHeightEntered); // Calculate Zone Volumes

        // Calculate zone centroid (and min/max x,y,z for zone)
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            nonInternalMassSurfacesPresent = false;
            TotSurfArea = 0.0;
            Zone(ZoneNum).Centroid = Vector(0.0, 0.0, 0.0);
            if (Surface(Zone(ZoneNum).SurfaceFirst).Sides > 0) {
                Zone(ZoneNum).MinimumX = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).x;
                Zone(ZoneNum).MaximumX = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).x;
                Zone(ZoneNum).MinimumY = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).y;
                Zone(ZoneNum).MaximumY = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).y;
                Zone(ZoneNum).MinimumZ = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).z;
                Zone(ZoneNum).MaximumZ = Surface(Zone(ZoneNum).SurfaceFirst).Vertex(1).z;
            }
            for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
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
                ShowSevereError(RoutineName + "Zone=\"" + Zone(ZoneNum).Name +
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

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
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
                    cNominalUwithConvCoeffs = RoundSigDigits(NominalUwithConvCoeffs, 3);
                } else {
                    cNominalUwithConvCoeffs = "[invalid]";
                }
                if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::TDD_Dome)) {
                    // SurfaceClass::Window also covers glass doors and TDD:Diffusers
                    cNominalU = "N/A";
                } else {
                    cNominalU = RoundSigDigits(NominalU(Surface(SurfNum).Construction), 3);
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
                    PreDefTableEntry(pdchOpUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    PreDefTableEntry(pdchDrUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                }
            }else{
                if ((SurfaceClass == SurfaceClass::Wall) || (SurfaceClass == SurfaceClass::Floor) || (SurfaceClass == SurfaceClass::Roof)) {
                    PreDefTableEntry(pdchIntOpUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
                } else if (SurfaceClass == SurfaceClass::Door) {
                    PreDefTableEntry(pdchIntDrUfactFilm, Surface(SurfNum).Name, NominalUwithConvCoeffs, 3);
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
              NumOfZones,
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

        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
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
        SetZoneOutBulbTempAt();
        CheckZoneOutBulbTempAt();
    }

    void AllocateModuleArrays()
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

        // FLOW:

        ShadeV.allocate(TotSurfaces);
        for (auto &e : ShadeV)
            e.NVert = 0;
        // Individual components (XV,YV,ZV) allocated in routine ProcessSurfaceVertices
        X0.dimension(TotSurfaces, 0.0);
        Y0.dimension(TotSurfaces, 0.0);
        Z0.dimension(TotSurfaces, 0.0);

//        CBZone.dimension(NumOfZones, 0.0);
        EnclSolDS.dimension(NumOfZones, 0.0);
        DGZone.dimension(NumOfZones, 0.0);
        DBZone.dimension(NumOfZones, 0.0);
        EnclSolDBSSG.dimension(NumOfZones, 0.0);
        QSDifSol.dimension(NumOfZones, 0.0);
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;
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

        if (GetSurfaceDataOneTimeFlag) {
            return;
        } else {
            GetSurfaceDataOneTimeFlag = true;
        }

        GetGeometryParameters(state, ErrorsFound);

        if (WorldCoordSystem) {
            if (BuildingAzimuth != 0.0) RelWarning = true;
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).RelNorth != 0.0) RelWarning = true;
            }
            if (RelWarning && !WarningDisplayed) {
                ShowWarningError(RoutineName +
                                 "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.");
                ShowContinueError("These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                WarningDisplayed = true;
            }
            RelWarning = false;
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !WarningDisplayed) {
                ShowWarningError(RoutineName +
                                 "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.");
                ShowContinueError("These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs.");
                WarningDisplayed = true;
            }
        }

        TotDetachedFixed = inputProcessor->getNumObjectsFound("Shading:Site:Detailed");
        TotDetachedBldg = inputProcessor->getNumObjectsFound("Shading:Building:Detailed");
        TotRectDetachedFixed = inputProcessor->getNumObjectsFound("Shading:Site");
        TotRectDetachedBldg = inputProcessor->getNumObjectsFound("Shading:Building");
        TotHTSurfs = inputProcessor->getNumObjectsFound("BuildingSurface:Detailed");
        TotDetailedWalls = inputProcessor->getNumObjectsFound("Wall:Detailed");
        TotDetailedRoofs = inputProcessor->getNumObjectsFound("RoofCeiling:Detailed");
        TotDetailedFloors = inputProcessor->getNumObjectsFound("Floor:Detailed");
        TotHTSubs = inputProcessor->getNumObjectsFound("FenestrationSurface:Detailed");
        TotShdSubs = inputProcessor->getNumObjectsFound("Shading:Zone:Detailed");
        TotOverhangs = inputProcessor->getNumObjectsFound("Shading:Overhang");
        TotOverhangsProjection = inputProcessor->getNumObjectsFound("Shading:Overhang:Projection");
        TotFins = inputProcessor->getNumObjectsFound("Shading:Fin");
        TotFinsProjection = inputProcessor->getNumObjectsFound("Shading:Fin:Projection");
        TotRectWindows = inputProcessor->getNumObjectsFound("Window");
        TotRectDoors = inputProcessor->getNumObjectsFound("Door");
        TotRectGlazedDoors = inputProcessor->getNumObjectsFound("GlazedDoor");
        TotRectIZWindows = inputProcessor->getNumObjectsFound("Window:Interzone");
        TotRectIZDoors = inputProcessor->getNumObjectsFound("Door:Interzone");
        TotRectIZGlazedDoors = inputProcessor->getNumObjectsFound("GlazedDoor:Interzone");
        TotRectExtWalls = inputProcessor->getNumObjectsFound("Wall:Exterior");
        TotRectIntWalls = inputProcessor->getNumObjectsFound("Wall:Adiabatic");
        TotRectIZWalls = inputProcessor->getNumObjectsFound("Wall:Interzone");
        TotRectUGWalls = inputProcessor->getNumObjectsFound("Wall:Underground");
        TotRectRoofs = inputProcessor->getNumObjectsFound("Roof");
        TotRectCeilings = inputProcessor->getNumObjectsFound("Ceiling:Adiabatic");
        TotRectIZCeilings = inputProcessor->getNumObjectsFound("Ceiling:Interzone");
        TotRectGCFloors = inputProcessor->getNumObjectsFound("Floor:GroundContact");
        TotRectIntFloors = inputProcessor->getNumObjectsFound("Floor:Adiabatic");
        TotRectIZFloors = inputProcessor->getNumObjectsFound("Floor:Interzone");

        TotOSC = 0;

        TotIntMassSurfaces = GetNumIntMassSurfaces(state);

        TotSurfaces = (TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg) * 2 + TotHTSurfs + TotHTSubs +
                      TotShdSubs * 2 + TotIntMassSurfaces + TotOverhangs * 2 + TotOverhangsProjection * 2 + TotFins * 4 + TotFinsProjection * 4 +
                      TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors + TotRectWindows + TotRectDoors + TotRectGlazedDoors +
                      TotRectIZWindows + TotRectIZDoors + TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls +
                      TotRectRoofs + TotRectCeilings + TotRectIZCeilings + TotRectGCFloors + TotRectIntFloors + TotRectIZFloors;

        SurfaceTmp.allocate(TotSurfaces); // Allocate the Surface derived type appropriately

        UniqueSurfaceNames.reserve(TotSurfaces);
        // SurfaceTmp structure is allocated via derived type initialization.

        int NumSurfs = 0;
        AddedSubSurfaces = 0;
        AskForSurfacesReport = true;

        GetDetShdSurfaceData(state, ErrorsFound, NumSurfs, TotDetachedFixed, TotDetachedBldg);

        GetRectDetShdSurfaceData(state, ErrorsFound, NumSurfs, TotRectDetachedFixed, TotRectDetachedBldg);

        GetHTSurfaceData(state,
                         ErrorsFound,
                         NumSurfs,
                         TotHTSurfs,
                         TotDetailedWalls,
                         TotDetailedRoofs,
                         TotDetailedFloors,
                         BaseSurfCls,
                         BaseSurfIDs,
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
                        BaseSurfIDs,
                        NeedToAddSurfaces);

        GetHTSubSurfaceData(state, ErrorsFound, NumSurfs, TotHTSubs, SubSurfCls, SubSurfIDs, AddedSubSurfaces, NeedToAddSubSurfaces);

        GetRectSubSurfaces(state,
                           ErrorsFound,
                           NumSurfs,
                           TotRectWindows,
                           TotRectDoors,
                           TotRectGlazedDoors,
                           TotRectIZWindows,
                           TotRectIZDoors,
                           TotRectIZGlazedDoors,
                           SubSurfIDs,
                           AddedSubSurfaces,
                           NeedToAddSubSurfaces);

        GetAttShdSurfaceData(state, ErrorsFound, NumSurfs, TotShdSubs);

        GetSimpleShdSurfaceData(state, ErrorsFound, NumSurfs, TotOverhangs, TotOverhangsProjection, TotFins, TotFinsProjection);

        GetIntMassSurfaceData(state, ErrorsFound, NumSurfs);

        GetMovableInsulationData(state, ErrorsFound);

        if (CalcSolRefl) GetShadingSurfReflectanceData(state, ErrorsFound);

        TotSurfaces = NumSurfs + AddedSubSurfaces + NeedToAddSurfaces + NeedToAddSubSurfaces;

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors discovered, program terminates.");
        }

        // Have to make room for added surfaces, if needed
        FirstTotalSurfaces = NumSurfs + AddedSubSurfaces;
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) {
            SurfaceTmp.redimension(TotSurfaces);
        }

        SurfaceWindow.allocate(TotSurfaces);

        AllocateSurfaceWindows(TotSurfaces);

        // add the "need to add" surfaces
        // Debug    write(outputfiledebug,*) ' need to add ',NeedtoAddSurfaces+NeedToAddSubSurfaces
        if (NeedToAddSurfaces + NeedToAddSubSurfaces > 0) CurNewSurf = FirstTotalSurfaces;
        for (int SurfNum = 1; SurfNum <= FirstTotalSurfaces; ++SurfNum) {
            if (SurfaceTmp(SurfNum).ExtBoundCond != UnenteredAdjacentZoneSurface) continue;
            // Need to add surface
            ++CurNewSurf;
            // Debug    write(outputfiledebug,*) ' adding surface=',curnewsurf
            SurfaceTmp(CurNewSurf) = SurfaceTmp(SurfNum);
            //  Basic parameters are the same for both surfaces.
            Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, Zone, NumOfZones);
            if (Found == 0) continue;
            SurfaceTmp(CurNewSurf).Zone = Found;
            SurfaceTmp(CurNewSurf).ZoneName = Zone(Found).Name;
            // Reverse Construction
            SurfaceTmp(CurNewSurf).Construction = AssignReverseConstructionNumber(state, SurfaceTmp(SurfNum).Construction, SurfError);
            SurfaceTmp(CurNewSurf).ConstructionStoredInputValue = SurfaceTmp(CurNewSurf).Construction;
            // Reverse Vertices
            NVert = SurfaceTmp(SurfNum).Sides;
            for (Vert = 1; Vert <= SurfaceTmp(SurfNum).Sides; ++Vert) {
                SurfaceTmp(CurNewSurf).Vertex(Vert) = SurfaceTmp(SurfNum).Vertex(NVert);
                --NVert;
            }
            if (SurfaceTmp(CurNewSurf).Sides > 2) {
                CreateNewellAreaVector(SurfaceTmp(CurNewSurf).Vertex, SurfaceTmp(CurNewSurf).Sides, SurfaceTmp(CurNewSurf).NewellAreaVector);
                SurfaceTmp(CurNewSurf).GrossArea = VecLength(SurfaceTmp(CurNewSurf).NewellAreaVector);
                SurfaceTmp(CurNewSurf).Area = SurfaceTmp(CurNewSurf).GrossArea;
                SurfaceTmp(CurNewSurf).NetAreaShadowCalc = SurfaceTmp(CurNewSurf).Area;
                CreateNewellSurfaceNormalVector(
                    SurfaceTmp(CurNewSurf).Vertex, SurfaceTmp(CurNewSurf).Sides, SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector);
                DetermineAzimuthAndTilt(SurfaceTmp(CurNewSurf).Vertex,
                                        SurfaceTmp(CurNewSurf).Sides,
                                        SurfWorldAz,
                                        SurfTilt,
                                        SurfaceTmp(CurNewSurf).lcsx,
                                        SurfaceTmp(CurNewSurf).lcsy,
                                        SurfaceTmp(CurNewSurf).lcsz,
                                        SurfaceTmp(CurNewSurf).GrossArea,
                                        SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector);
                SurfaceTmp(CurNewSurf).Azimuth = SurfWorldAz;
                SurfaceTmp(CurNewSurf).Tilt = SurfTilt;

                // Sine and cosine of azimuth and tilt
                SurfaceTmp(CurNewSurf).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians());
                SurfaceTmp(CurNewSurf).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians());
                SurfaceTmp(CurNewSurf).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
                SurfaceTmp(CurNewSurf).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
                // Outward normal unit vector (pointing away from room)
                SurfaceTmp(CurNewSurf).OutNormVec = SurfaceTmp(CurNewSurf).NewellSurfaceNormalVector;
                for (n = 1; n <= 3; ++n) {
                    if (std::abs(SurfaceTmp(CurNewSurf).OutNormVec(n) - 1.0) < 1.e-06) SurfaceTmp(CurNewSurf).OutNormVec(n) = +1.0;
                    if (std::abs(SurfaceTmp(CurNewSurf).OutNormVec(n) + 1.0) < 1.e-06) SurfaceTmp(CurNewSurf).OutNormVec(n) = -1.0;
                    if (std::abs(SurfaceTmp(CurNewSurf).OutNormVec(n)) < 1.e-06) SurfaceTmp(CurNewSurf).OutNormVec(n) = 0.0;
                }

                // Can perform tests on this surface here
                SurfaceTmp(CurNewSurf).ViewFactorSky = 0.5 * (1.0 + SurfaceTmp(CurNewSurf).CosTilt);
                SurfaceTmp(CurNewSurf).ViewFactorGround = 0.5 * (1.0 - SurfaceTmp(CurNewSurf).CosTilt);

                // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
                // surfaces
                SurfaceTmp(CurNewSurf).ViewFactorSkyIR = SurfaceTmp(CurNewSurf).ViewFactorSky;
                SurfaceTmp(CurNewSurf).ViewFactorGroundIR = 0.5 * (1.0 - SurfaceTmp(CurNewSurf).CosTilt);
            }

            // Change Name
            SurfaceTmp(CurNewSurf).Name = "iz-" + SurfaceTmp(SurfNum).Name;
            // Debug   write(outputfiledebug,*) ' new surf name=',TRIM(SurfaceTmp(CurNewSurf)%Name)
            // Debug   write(outputfiledebug,*) ' new surf in zone=',TRIM(surfacetmp(curnewsurf)%zoneName)
            SurfaceTmp(CurNewSurf).ExtBoundCond = UnreconciledZoneSurface;
            SurfaceTmp(SurfNum).ExtBoundCond = UnreconciledZoneSurface;
            SurfaceTmp(CurNewSurf).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
            SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(CurNewSurf).Name;
            if (SurfaceTmp(CurNewSurf).Class == SurfaceClass::Roof || SurfaceTmp(CurNewSurf).Class == SurfaceClass::Wall ||
                SurfaceTmp(CurNewSurf).Class == SurfaceClass::Floor) {
                // base surface
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Roof) {
                    SurfaceTmp(CurNewSurf).Class = SurfaceClass::Floor;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a floor'
                } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor) {
                    SurfaceTmp(CurNewSurf).Class = SurfaceClass::Roof;
                    // Debug          write(outputfiledebug,*) ' new surfaces is a roof'
                }
                SurfaceTmp(CurNewSurf).BaseSurf = CurNewSurf;
                SurfaceTmp(CurNewSurf).BaseSurfName = SurfaceTmp(CurNewSurf).Name;
                // Debug        write(outputfiledebug,*) ' basesurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
            } else {
                // subsurface
                Found = UtilityRoutines::FindItemInList("iz-" + SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp, FirstTotalSurfaces + CurNewSurf - 1);
                if (Found > 0) {
                    SurfaceTmp(CurNewSurf).BaseSurfName = "iz-" + SurfaceTmp(SurfNum).BaseSurfName;
                    SurfaceTmp(CurNewSurf).BaseSurf = Found;
                    SurfaceTmp(Found).Area -= SurfaceTmp(CurNewSurf).Area;
                    if (SurfaceTmp(CurNewSurf).Class == SurfaceClass::Window || SurfaceTmp(CurNewSurf).Class == SurfaceClass::GlassDoor) {
                        SurfaceTmp(Found).NetAreaShadowCalc -= SurfaceTmp(CurNewSurf).Area / SurfaceTmp(CurNewSurf).Multiplier;
                    } else { // Door, TDD:Diffuser, TDD:DOME
                        SurfaceTmp(Found).NetAreaShadowCalc -= SurfaceTmp(CurNewSurf).Area;
                    }
                    SurfaceTmp(CurNewSurf).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                    SurfaceTmp(CurNewSurf).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
                    SurfaceTmp(CurNewSurf).ExtSolar = SurfaceTmp(Found).ExtSolar;
                    SurfaceTmp(CurNewSurf).ExtWind = SurfaceTmp(Found).ExtWind;
                    SurfaceTmp(CurNewSurf).Zone = SurfaceTmp(Found).Zone;
                    SurfaceTmp(CurNewSurf).ZoneName = SurfaceTmp(Found).ZoneName;
                    SurfaceTmp(CurNewSurf).OSCPtr = SurfaceTmp(Found).OSCPtr;
                    // Debug        write(outputfiledebug,*) ' subsurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
                    // Debug        write(outputfiledebug,*) ' subsurf, basesurf=',TRIM('iz-'//SurfaceTmp(SurfNum)%BaseSurfName)
                } else {
                    ShowSevereError(RoutineName + "Adding unentered subsurface, could not find base surface=" + "iz-" +
                                    SurfaceTmp(SurfNum).BaseSurfName);
                    SurfError = true;
                }
            }
        }
        //**********************************************************************************
        // After all of the surfaces have been defined then the base surfaces for the
        // sub-surfaces can be defined.  Loop through surfaces and match with the sub-surface
        // names.
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!SurfaceTmp(SurfNum).HeatTransSurf) continue;

            // why are we doing this again?  this should have already been done.
            if (UtilityRoutines::SameString(SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp(SurfNum).Name)) {
                Found = SurfNum;
            } else {
                Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp, TotSurfaces);
            }
            if (Found > 0) {
                SurfaceTmp(SurfNum).BaseSurf = Found;
                if (SurfNum != Found) { // for subsurfaces
                    if (SurfaceTmp(SurfNum).HeatTransSurf) ++SurfaceTmp(Found).NumSubSurfaces;
                    if (SurfaceTmp(SurfNum).Class < SurfaceClass::Window || SurfaceTmp(SurfNum).Class > SurfaceClass::TDD_Diffuser) {
                        if (SurfaceTmp(SurfNum).Class == SurfaceClass::None) {
                            ShowSevereError(RoutineName + "Invalid SubSurface detected, Surface=" + SurfaceTmp(SurfNum).Name);
                        } else {
                            ShowSevereError(RoutineName + "Invalid SubSurface detected, Surface=" + SurfaceTmp(SurfNum).Name +
                                            ", class=" + BaseSurfCls(int(SurfaceTmp(SurfNum).Class)) + " invalid class for subsurface");
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

        // Move all shading Surfaces to Front

        SurfaceTmpClassInvalid.dimension(TotSurfaces, false);
        SurfaceTmpClassMoved.dimension(TotSurfaces, false);
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F && SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_B &&
                SurfaceTmp(SurfNum).Class != SurfaceClass::Shading)
                continue;

            //  A shading surface

            ++MovedSurfs;
            Surface(MovedSurfs) = SurfaceTmp(SurfNum);
            SurfaceTmpClassMoved(SurfNum) = true; //'Moved'
            // Store list of moved surface numbers in reporting order 
            DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);
        }

        //  For each zone

        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            //  For each Base Surface Type (Wall, Floor, Roof/Ceiling) - put these first

            for (int Loop = 1; Loop <= 3; ++Loop) {

                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                    if (SurfaceTmp(SurfNum).Zone == 0) continue;

                    if (!UtilityRoutines::SameString(SurfaceTmp(SurfNum).ZoneName, Zone(ZoneNum).Name)) continue;
                    if (SurfaceTmp(SurfNum).Class != BaseSurfIDs(Loop)) continue;

                    ++MovedSurfs;
                    Surface(MovedSurfs) = SurfaceTmp(SurfNum);
                    SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                    SurfaceTmp(SurfNum).BaseSurf = -1;              // Default has base surface = base surface
                    BaseSurfNum = MovedSurfs;
                    Surface(MovedSurfs).BaseSurf = BaseSurfNum;
                    // Store list of moved surface numbers in order reporting order (subsurfaces follow their base surface)
                    DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);

                    //  Find all subsurfaces to this surface - just to update the base surface number - don't move these yet
                    for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                        if (SurfaceTmp(SubSurfNum).Zone == 0) continue;
                        if (SurfaceTmp(SubSurfNum).BaseSurf != SurfNum) continue;
                        // Set BaseSurf to negative of new BaseSurfNum (to avoid confusion with other base surfaces)
                        SurfaceTmp(SubSurfNum).BaseSurf = -BaseSurfNum;
                        // Add original sub-surface numbers as placeholders in surface list for reporting
                        DataSurfaces::AllSurfaceListReportOrder.push_back(-SubSurfNum);
                    }
                }
            }

            // Internal mass goes next
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                if (!UtilityRoutines::SameString(SurfaceTmp(SurfNum).ZoneName, Zone(ZoneNum).Name)) continue;
                if (SurfaceTmp(SurfNum).Class != SurfaceClass::IntMass) continue;

                ++MovedSurfs;
                Surface(MovedSurfs) = SurfaceTmp(SurfNum);
                Surface(MovedSurfs).BaseSurf = MovedSurfs;
                SurfaceTmpClassMoved(SurfNum) = true; // 'Moved'
                // Store list of moved surface numbers in reporting order
                DataSurfaces::AllSurfaceListReportOrder.push_back(MovedSurfs);
            }

            // Non-window) subsurfaces are next (anything left in this zone that's not a window or a glass door)
            // includes SurfaceClass::TDD_Dome which transmits light but is not a window for heat balance purposes
            for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                if (SurfaceTmp(SubSurfNum).Zone != ZoneNum) continue;
                if (SurfaceTmp(SubSurfNum).Class == SurfaceClass::Window) continue;
                if (SurfaceTmp(SubSurfNum).Class == SurfaceClass::GlassDoor) continue;
                if (SurfaceTmp(SubSurfNum).Class == SurfaceClass::TDD_Diffuser) continue;


                ++MovedSurfs;
                Surface(MovedSurfs) = SurfaceTmp(SubSurfNum);
                SurfaceTmpClassMoved(SubSurfNum) = true; // 'Moved'
                // Reset BaseSurf to it's positive value (set to negative earlier)
                Surface(MovedSurfs).BaseSurf = -Surface(MovedSurfs).BaseSurf;
                SurfaceTmp(SubSurfNum).BaseSurf = -1;
                // Find and replace negative SubSurfNum with new MovedSurfs num in surface list for reporting
                std::replace(DataSurfaces::AllSurfaceListReportOrder.begin(), DataSurfaces::AllSurfaceListReportOrder.end(), -SubSurfNum, MovedSurfs);
            }

            // Last but not least, the window subsurfaces (includes SurfaceClass::TDD_Diffuser)
            for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                if (SurfaceTmpClassMoved(SubSurfNum)) continue;
                if (SurfaceTmp(SubSurfNum).Zone != ZoneNum) continue;
                if ((SurfaceTmp(SubSurfNum).Class != SurfaceClass::Window) && (SurfaceTmp(SubSurfNum).Class != SurfaceClass::GlassDoor) &&
                    (SurfaceTmp(SubSurfNum).Class != SurfaceClass::TDD_Diffuser))
                    continue;

                ++MovedSurfs;
                Surface(MovedSurfs) = SurfaceTmp(SubSurfNum);
                SurfaceTmpClassMoved(SubSurfNum) = true;// 'Moved'
                // Reset BaseSurf to it's positive value (set to negative earlier)
                Surface(MovedSurfs).BaseSurf = -Surface(MovedSurfs).BaseSurf;
                SurfaceTmp(SubSurfNum).BaseSurf = -1;
                // Find and replace negative SubSurfNum with new MovedSurfs num in surface list for reporting
                std::replace(DataSurfaces::AllSurfaceListReportOrder.begin(), DataSurfaces::AllSurfaceListReportOrder.end(), -SubSurfNum, MovedSurfs);
            }
        }

        if (MovedSurfs != TotSurfaces) {
            ShowSevereError(format("{}Reordered # of Surfaces ({}) not = Total # of Surfaces ({})", RoutineName, MovedSurfs, TotSurfaces));
            SurfError = true;
            for (int Loop = 1; Loop <= TotSurfaces; ++Loop) {
                if (SurfaceTmpClassMoved(Loop) && SurfaceTmpClassInvalid(Loop)) {
                    ShowSevereError(RoutineName + "Error in Surface= \"" + SurfaceTmp(Loop).Name + "\" Class=" +
                    cSurfaceClass(SurfaceTmp(Loop).Class) + " indicated Zone=\"" + SurfaceTmp(Loop).ZoneName + "\"");
                }
            }
            ShowWarningError(RoutineName + "Remaining surface checks will use \"reordered number of surfaces\", not number of original surfaces");
        }

        SurfaceTmp.deallocate(); // DeAllocate the Temp Surface derived type

        //  For each Base Surface Type (Wall, Floor, Roof)

        for (int Loop = 1; Loop <= 3; ++Loop) {

            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                if (Surface(SurfNum).Zone == 0) continue;

                if (Surface(SurfNum).Class != BaseSurfIDs(Loop)) continue;

                //  Find all subsurfaces to this surface
                for (int SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum) {

                    if (SurfNum == SubSurfNum) continue;
                    if (Surface(SubSurfNum).Zone == 0) continue;
                    if (Surface(SubSurfNum).BaseSurf != SurfNum) continue;

                    // Check facing angle of Sub compared to base
                    checkSubSurfAzTiltNorm(Surface(SurfNum), Surface(SubSurfNum), subSurfaceError);
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
            if (Surface(SurfNum).ExtBoundCond == UnreconciledZoneSurface) {
                if (not_blank(Surface(SurfNum).ExtBoundCondName)) {
                    if (Surface(SurfNum).ExtBoundCondName == Surface(SurfNum).Name) {
                        Found = SurfNum;
                    } else {
                        Found = UtilityRoutines::FindItemInList(Surface(SurfNum).ExtBoundCondName, Surface, MovedSurfs);
                    }
                    if (Found != 0) {
                        Surface(SurfNum).ExtBoundCond = Found;
                        // Check that matching surface is also "OtherZoneSurface"
                        if (Surface(Found).ExtBoundCond <= 0 && Surface(Found).ExtBoundCond != UnreconciledZoneSurface) {
                            ShowSevereError(RoutineName + "Potential \"OtherZoneSurface\" is not matched correctly:");

                            ShowContinueError("Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                            ShowContinueError("Nonmatched Other/InterZone Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                            SurfError = true;
                        }
                        // Check that matching interzone surface has construction with reversed layers
                        if (Found != SurfNum) { // Interzone surface
                            // Make sure different zones too (CR 4110)
                            if (Surface(SurfNum).Zone == Surface(Found).Zone) {
                                ++ErrCount2;
                                if (ErrCount2 == 1 && !DisplayExtraWarnings) {
                                    ShowWarningError(RoutineName + "CAUTION -- Interzone surfaces are occuring in the same zone(s).");
                                    ShowContinueError(
                                        "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual occurrences.");
                                }
                                if (DisplayExtraWarnings) {
                                    ShowWarningError(RoutineName + "CAUTION -- Interzone surfaces are usually in different zones");
                                    ShowContinueError("Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                    ShowContinueError("Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
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
                                    ShowSevereError(RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                    Surface(SurfNum).Name + " does not have the same number of layers as the construction " +
                                                    state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionNumLayersWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionNumLayersWarning) {
                                        ShowContinueError("...this problem for this pair will not be reported again.");
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
                                    ShowSevereError(RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                    Surface(SurfNum).Name +
                                                    " does not have the same materials in the reverse order as the construction " +
                                                    state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    ShowContinueError("or the properties of the reversed layers are not correct due to differing layer front and back side values");
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError("...this problem for this pair will not be reported again.");
                                        state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning = true;
                                        state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning = true;
                                    }
                                    SurfError = true;
                                } else if (izConstDiff) {
                                    ShowWarningError(RoutineName + "Construction " + state.dataConstruction->Construct(ConstrNum).Name + " of interzone surface " +
                                                     Surface(SurfNum).Name +
                                                     " does not have the same materials in the reverse order as the construction " +
                                                     state.dataConstruction->Construct(ConstrNumFound).Name + " of adjacent surface " + Surface(Found).Name);
                                    ShowContinueError("or the properties of the reversed layers are not correct due to differing layer front and back side values");
                                    ShowContinueError("...but Nominal U values are similar, diff=[" +
                                                      RoundSigDigits(std::abs(NominalU(ConstrNum) - NominalU(ConstrNumFound)), 4) +
                                                      "] ... simulation proceeds.");
                                    if (!izConstDiffMsg) {
                                        ShowContinueError("...if the two zones are expected to have significantly different temperatures, the proper "
                                                          "\"reverse\" construction should be created.");
                                        izConstDiffMsg = true;
                                    }
                                    if (!state.dataConstruction->Construct(ConstrNum).ReverseConstructionLayersOrderWarning ||
                                        !state.dataConstruction->Construct(ConstrNumFound).ReverseConstructionLayersOrderWarning) {
                                        ShowContinueError("...this problem for this pair will not be reported again.");
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
                                    if (ErrCount4 == 1 && !DisplayExtraWarnings) {
                                        ShowWarningError(
                                            RoutineName +
                                            "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:");
                                        ShowContinueError(
                                            "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual mismatches.");
                                    }
                                    if (DisplayExtraWarnings) {
                                        ShowWarningError(
                                            RoutineName +
                                            "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:");

                                        if (MultFound == 1 && MultSurfNum == 1) {
                                            ShowContinueError("  Area=" + TrimSigDigits(Surface(SurfNum).Area, 1) +
                                                              " in Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                            ShowContinueError("  Area=" + TrimSigDigits(Surface(Found).Area, 1) +
                                                              " in Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                                        } else { // Show multiplier info
                                            ShowContinueError(format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
                                                                     Surface(SurfNum).Area,
                                                                     MultSurfNum,
                                                                    Surface(SurfNum).Area * MultSurfNum,
                                                                     Surface(SurfNum).Name,
                                                                     Surface(SurfNum).ZoneName));

                                            ShowContinueError(format("  Area={:.1T}, Multipliers={}, Total Area={:.1T} in Surface={} Zone={}",
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
                                ShowWarningError(RoutineName + "InterZone Surface Tilts do not match as expected.");
                                ShowContinueError("  Tilt=" + TrimSigDigits(Surface(SurfNum).Tilt, 1) + " in Surface=" + Surface(SurfNum).Name +
                                                  ", Zone=" + Surface(SurfNum).ZoneName);
                                ShowContinueError("  Tilt=" + TrimSigDigits(Surface(Found).Tilt, 1) + " in Surface=" + Surface(Found).Name +
                                                  ", Zone=" + Surface(Found).ZoneName);
                            }
                            // check surface class match.  interzone surface.
                            if ((Surface(SurfNum).Class == SurfaceClass::Wall && Surface(Found).Class != SurfaceClass::Wall) ||
                                (Surface(SurfNum).Class != SurfaceClass::Wall && Surface(Found).Class == SurfaceClass::Wall)) {
                                ShowWarningError(RoutineName + "InterZone Surface Classes do not match as expected.");
                                ShowContinueError("Surface=\"" + Surface(SurfNum).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(SurfNum).Class));
                                ShowContinueError("Adjacent Surface=\"" + Surface(Found).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(Found).Class));
                                ShowContinueError("Other errors/warnings may follow about these surfaces.");
                            }
                            if ((Surface(SurfNum).Class == SurfaceClass::Roof && Surface(Found).Class != SurfaceClass::Floor) ||
                                (Surface(SurfNum).Class != SurfaceClass::Roof && Surface(Found).Class == SurfaceClass::Floor)) {
                                ShowWarningError(RoutineName + "InterZone Surface Classes do not match as expected.");
                                ShowContinueError("Surface=\"" + Surface(SurfNum).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(SurfNum).Class));
                                ShowContinueError("Adjacent Surface=\"" + Surface(Found).Name +
                                                  "\", surface class=" + cSurfaceClass(Surface(Found).Class));
                                ShowContinueError("Other errors/warnings may follow about these surfaces.");
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
                                    if (std::abs(Surface(SurfNum).SinTilt) > 0.5 || DisplayExtraWarnings) {
                                        // if horizontal surfaces, then these are windows/doors/etc in those items.
                                        ShowWarningError(RoutineName + "InterZone Surface Azimuths do not match as expected.");
                                        ShowContinueError("  Azimuth=" + TrimSigDigits(Surface(SurfNum).Azimuth, 1) +
                                                          ", Tilt=" + TrimSigDigits(Surface(SurfNum).Tilt, 1) +
                                                          ", in Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                        ShowContinueError("  Azimuth=" + TrimSigDigits(Surface(Found).Azimuth, 1) +
                                                          ", Tilt=" + TrimSigDigits(Surface(Found).Tilt, 1) + ", in Surface=" + Surface(Found).Name +
                                                          ", Zone=" + Surface(Found).ZoneName);
                                        ShowContinueError("..surface class of first surface=" + cSurfaceClass(Surface(SurfNum).Class));
                                        ShowContinueError("..surface class of second surface=" + cSurfaceClass(Surface(Found).Class));
                                    }
                                }
                            }

                            // Make sure exposures (Sun, Wind) are the same.....and are "not"
                            if (Surface(SurfNum).ExtSolar || Surface(Found).ExtSolar) {
                                ShowWarningError(RoutineName + "Interzone surfaces cannot be \"SunExposed\" -- removing SunExposed");
                                ShowContinueError("  Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                ShowContinueError("  Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
                                Surface(SurfNum).ExtSolar = false;
                                Surface(Found).ExtSolar = false;
                            }
                            if (Surface(SurfNum).ExtWind || Surface(Found).ExtWind) {
                                ShowWarningError(RoutineName + "Interzone surfaces cannot be \"WindExposed\" -- removing WindExposed");
                                ShowContinueError("  Surface=" + Surface(SurfNum).Name + ", Zone=" + Surface(SurfNum).ZoneName);
                                ShowContinueError("  Surface=" + Surface(Found).Name + ", Zone=" + Surface(Found).ZoneName);
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
                                    ShowSevereError(RoutineName + "SubSurface=\"" + Surface(SurfNum).Name + "\" is an interzone subsurface.");
                                    ShowContinueError("..but the Base Surface is not an interzone surface, Surface=\"" +
                                                      Surface(Surface(SurfNum).BaseSurf).Name + "\".");
                                    SurfError = true;
                                }
                            }
                        }
                    } else {
                        //  Seems unlikely that an internal surface would be missing itself, so this message
                        //  only indicates for adjacent (interzone) surfaces.
                        ShowSevereError(RoutineName + "Adjacent Surface not found: " + Surface(SurfNum).ExtBoundCondName + " adjacent to surface " +
                                        Surface(SurfNum).Name);
                        NonMatch = true;
                        SurfError = true;
                    }
                } else if (Surface(SurfNum).BaseSurf != SurfNum) { // Subsurface
                    if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond > 0 &&
                        Surface(Surface(SurfNum).BaseSurf).ExtBoundCond !=
                            Surface(SurfNum).BaseSurf) { // If Interzone surface, subsurface must be also.
                        ShowSevereError(RoutineName + "SubSurface on Interzone Surface must be an Interzone SubSurface.");
                        ShowContinueError("...OutsideFaceEnvironment is blank, in Surface=" + Surface(SurfNum).Name);
                        SurfError = true;
                    } else {
                        ++ErrCount3;
                        if (ErrCount3 == 1 && !DisplayExtraWarnings) {
                            ShowWarningError(RoutineName + "Blank name for Outside Boundary Condition Objects.");
                            ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                        }
                        if (DisplayExtraWarnings) {
                            ShowWarningError(RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface(SurfNum).Name);
                            ShowContinueError("Resetting this surface to be an internal zone surface, zone=" + Surface(SurfNum).ZoneName);
                        }
                        Surface(SurfNum).ExtBoundCondName = Surface(SurfNum).Name;
                        Surface(SurfNum).ExtBoundCond = SurfNum;
                    }
                } else {
                    ++ErrCount3;
                    if (ErrCount3 == 1 && !DisplayExtraWarnings) {
                        ShowSevereError(RoutineName + "Blank name for Outside Boundary Condition Objects.");
                        ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                    }
                    if (DisplayExtraWarnings) {
                        ShowWarningError(RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface(SurfNum).Name);
                        ShowContinueError("Resetting this surface to be an internal zone (adiabatic) surface, zone=" + Surface(SurfNum).ZoneName);
                    }
                    Surface(SurfNum).ExtBoundCondName = Surface(SurfNum).Name;
                    Surface(SurfNum).ExtBoundCond = SurfNum;
                    SurfError = true;
                }
            }

        } // ...end of the Surface DO loop for finding BaseSurf
        if (NonMatch) {
            ShowSevereError(RoutineName + "Non matching interzone surfaces found");
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
                        ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [adiabatic surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [" + cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    } else if (Surface(SurfNum).ExtBoundCond > 0) {
                        ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [interzone surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [" + cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    } else if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond == OtherSideCondModeledExt) {
                        ShowWarningError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                         cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                         Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [" +
                                         cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        ShowContinueError("...SubSurface will not use the exterior condition model of the base surface.");
                    } else {
                        ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                        cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                        Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [" +
                                        cExtBoundCondition(Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) + ']');
                        SurfError = true;
                    }
                    if (!SubSurfaceSevereDisplayed && SurfError) {
                        ShowContinueError("...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                }
            } else if (Surface(Surface(SurfNum).BaseSurf).BaseSurf == Surface(Surface(SurfNum).BaseSurf).ExtBoundCond) {
                // adiabatic surface. make sure subsurfaces match
                if (Surface(SurfNum).ExtBoundCond != SurfNum) { // not adiabatic surface
                    if (Surface(SurfNum).ExtBoundCond > 0) {
                        ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                        "\" exterior condition [interzone surface] in a base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name +
                                        "\" with exterior condition [adiabatic surface]");
                    } else {
                        ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name + "\" exterior condition [" +
                                        cExtBoundCondition(Surface(SurfNum).ExtBoundCond) + "] in a base surface=\"" +
                                        Surface(Surface(SurfNum).BaseSurf).Name + "\" with exterior condition [adiabatic surface]");
                    }
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError("...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    SurfError = true;
                }
            } else if (Surface(Surface(SurfNum).BaseSurf).ExtBoundCond > 0) { // interzone surface
                if (Surface(SurfNum).ExtBoundCond == SurfNum) {
                    ShowSevereError(RoutineName + "Subsurface=\"" + Surface(SurfNum).Name +
                                    "\" is an adiabatic surface in an Interzone base surface=\"" + Surface(Surface(SurfNum).BaseSurf).Name + "\"");
                    if (!SubSurfaceSevereDisplayed) {
                        ShowContinueError("...calculations for heat balance would be compromised.");
                        SubSurfaceSevereDisplayed = true;
                    }
                    //        SurfError=.TRUE.
                }
            }
        }

        //**********************************************************************************
        //   Set up Zone Surface Pointers
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            for (int SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum) { // TotSurfaces
                if (Surface(SurfNum).Zone == ZoneNum) {
                    if (Zone(ZoneNum).SurfaceFirst == 0) {
                        Zone(ZoneNum).SurfaceFirst = SurfNum;
                        // Non window surfaces are grouped first within each zone
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
        if (NumOfZones > 0) {
            Zone(NumOfZones).SurfaceLast = TotSurfaces;
            if ((Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::Window) ||
                (Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                (Surface(TotSurfaces).Class == DataSurfaces::SurfaceClass::TDD_Diffuser)) {
                Zone(NumOfZones).WindowSurfaceLast = TotSurfaces;
            } else {
                // If there are no windows in the zone, then set this to -1 so any for loops on WindowSurfaceFirst to WindowSurfaceLast will not
                // execute
                Zone(NumOfZones).WindowSurfaceLast = -1;
                Zone(NumOfZones).NonWindowSurfaceLast = TotSurfaces;
            }
        }
        for (int ZoneNum = 1; ZoneNum <= NumOfZones - 1; ++ZoneNum) {
            Zone(ZoneNum).SurfaceLast = Zone(ZoneNum + 1).SurfaceFirst - 1;
            if ((Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::Window) ||
                (Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::GlassDoor) ||
                (Surface(Zone(ZoneNum).SurfaceLast).Class == DataSurfaces::SurfaceClass::TDD_Diffuser)) {
                Zone(ZoneNum).WindowSurfaceLast = Zone(ZoneNum + 1).SurfaceFirst - 1;
            } else {
                // If there are no windows in the zone, then set this to -1 so any for loops on WindowSurfaceFirst to WindowSurfaceLast will not
                // execute
                Zone(ZoneNum).WindowSurfaceLast = -1;
                Zone(ZoneNum).NonWindowSurfaceLast = Zone(ZoneNum).SurfaceLast;
            }
        }

        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (Zone(ZoneNum).SurfaceFirst == 0) {
                ShowSevereError(RoutineName + "Zone has no surfaces, Zone=" + Zone(ZoneNum).Name);
                SurfError = true;
            }
        }

        // Set up Floor Areas for Zones
        if (!SurfError) {
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
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
            for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                Zone(ZoneNum).CalcFloorArea = Zone(ZoneNum).FloorArea;
                if (Zone(ZoneNum).UserEnteredFloorArea != DataGlobalConstants::AutoCalculate()) {
                    // Check entered vs calculated
                    if (Zone(ZoneNum).UserEnteredFloorArea > 0.0) { // User entered zone floor area,
                        // produce message if not near calculated
                        if (Zone(ZoneNum).CalcFloorArea > 0.0) {
                            diffp = std::abs(Zone(ZoneNum).CalcFloorArea - Zone(ZoneNum).UserEnteredFloorArea) / Zone(ZoneNum).UserEnteredFloorArea;
                            if (diffp > 0.05) {
                                ++ErrCount;
                                if (ErrCount == 1 && !DisplayExtraWarnings) {
                                    ShowWarningError(RoutineName + "Entered Zone Floor Areas differ from calculated Zone Floor Area(s).");
                                    ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                                }
                                if (DisplayExtraWarnings) {
                                    // Warn user of using specified Zone Floor Area
                                    ShowWarningError(RoutineName + "Entered Floor Area entered for Zone=\"" + Zone(ZoneNum).Name +
                                                     "\" significantly different from calculated Floor Area");
                                    ShowContinueError("Entered Zone Floor Area value=" + RoundSigDigits(Zone(ZoneNum).UserEnteredFloorArea, 2) +
                                                      ", Calculated Zone Floor Area value=" + RoundSigDigits(Zone(ZoneNum).CalcFloorArea, 2) +
                                                      ", entered Floor Area will be used in calculations.");
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
                ShowSevereError(RoutineName + "Zero or negative surface area[" + RoundSigDigits(Surface(SurfNum).Area, 5) +
                                "], Surface=" + Surface(SurfNum).Name);
                SurfError = true;
            }
            if (Surface(SurfNum).Area >= 1.e-06 && Surface(SurfNum).Area < 0.001) {
                ShowWarningError(RoutineName + "Very small surface area[" + RoundSigDigits(Surface(SurfNum).Area, 5) +
                                 "], Surface=" + Surface(SurfNum).Name);
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
                        if (dataMaterial.Material(iTmp2).Group == WindowBlind) {
                            BlNum = dataMaterial.Material(iTmp2).BlindDataPtr;
                            SurfWinBlindNumber(SurfNum) = BlNum;
                            // TH 2/18/2010. CR 8010
                            // if it is a blind with movable slats, create one new blind and set it to VariableSlat if not done so yet.
                            //  the new blind is created only once, it can be shared by multiple windows though.
                            if (SurfWinMovableSlats(SurfNum) && Blind(BlNum).SlatAngleType != VariableSlats) {
                                errFlag = false;
                                AddVariableSlatBlind(BlNum, BlNumNew, errFlag);
                                // point to the new blind
                                dataMaterial.Material(iTmp2).BlindDataPtr = BlNumNew;
                                // window surface points to new blind
                                SurfWinBlindNumber(SurfNum) = BlNumNew;
                            }
                            break;
                        }
                    }

                    if (errFlag) {
                        ErrorsFound = true;
                        ShowContinueError("WindowShadingControl " + WindowShadingControl(WinShadingControlPtr).Name +
                                          " has errors, program will terminate.");
                    }
                }
            } // End of surface loop

            // final associate fenestration surfaces referenced in WindowShadingControl
            FinalAssociateWindowShadingControlFenestration(ErrorsFound);
            CheckWindowShadingControlSimilarForWindow(ErrorsFound);
        }

        // Check for zones with not enough surfaces
        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
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
                ShowSevereError(RoutineName + "Zone " + Zone(ZoneNum).Name + " has only one floor, wall or roof, and this surface has a window.");
                ShowContinueError("Add more floors, walls or roofs, or an internal mass surface.");
            }
        }

        // set up vertex of centroid for each surface.
        CalcSurfaceCentroid();

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
                ShowSevereError(RoutineName + "Only one EcoRoof Material is currently allowed for all constructions.");
                ShowContinueError("... first material=" + dataMaterial.Material(LayNumOutside).Name);
                ShowContinueError("... conflicting Construction=" + state.dataConstruction->Construct(Surface(SurfNum).Construction).Name +
                                  " uses material=" + dataMaterial.Material(state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1)).Name);
                ErrorsFound = true;
            }
        }

        // Set flag that determines whether a surface can be an exterior obstruction
        // Also set associated surfaces for Kiva foundations and build heat transfer surface lists
        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            Surface(SurfNum).ShadowSurfPossibleObstruction = false;

            if (Surface(SurfNum).ShadowingSurf) {
                DataSurfaces::AllShadingSurfList.push_back(SurfNum);
            }
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
                if (!ErrorsFound) kivaManager.foundationInputs[Surface(SurfNum).OSCPtr].surfaces.push_back(SurfNum);
                continue;
            }
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt) continue;
            if (Surface(SurfNum).HeatTransSurf && Surface(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) continue;
            // Exclude windows and doors, i.e., consider only their base surfaces as possible obstructions
            if (Surface(SurfNum).Class == SurfaceClass::Window || Surface(SurfNum).Class == SurfaceClass::Door) continue;
            // Exclude duplicate shading surfaces
            if (Surface(SurfNum).MirroredSurf) continue;
            // Exclude air boundary surfaces
            if (Surface(SurfNum).HeatTransSurf && state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsAirBoundary) continue;

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
                if (!DisplayExtraWarnings) {
                    ++iTmp1;
                } else {
                    ShowWarningError(RoutineName + "Surface=\"" + Surface(SurfNum).Name +
                                     "\" uses InfraredTransparent construction in a non-interzone surface. (illegal use)");
                }
            }
            if (iTmp1 > 0) {
                ShowWarningError(RoutineName + "Surfaces use InfraredTransparent constructions " + TrimSigDigits(iTmp1) +
                                 " in non-interzone surfaces. (illegal use)");
                ShowContinueError("For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
            }
        }

        // Note, could do same for Window Area and detecting if Interzone Surface in Zone

        if (Warning1Count > 0) {
            ShowWarningMessage(RoutineName + "Window dimensions differ from Window 5/6 data file dimensions, " + TrimSigDigits(Warning1Count) +
                               " times.");
            ShowContinueError("This will affect the frame heat transfer calculation if the frame in the Data File entry");
            ShowContinueError("is not uniform, i.e., has sections with different geometry and/or thermal properties.");
            ShowContinueError("For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
        }
        if (Warning2Count > 0) {
            ShowWarningMessage(RoutineName + "Exterior Windows have been replaced with Window 5/6 two glazing systems, " +
                               TrimSigDigits(Warning2Count) + " times.");
            ShowContinueError("Note that originally entered dimensions are overridden.");
            ShowContinueError("For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
        }
        if (Warning3Count > 0) {
            ShowWarningMessage(RoutineName + "Interior Windows have been replaced with Window 5/6 two glazing systems, " +
                               TrimSigDigits(Warning3Count) + " times.");
            ShowContinueError("Note that originally entered dimensions are overridden.");
            ShowContinueError("For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
        }

        if (TotalMultipliedWindows > 0) {
            ShowWarningMessage(RoutineName + "There are " + TrimSigDigits(TotalMultipliedWindows) +
                               " window/glass door(s) that may cause inaccurate shadowing due to Solar Distribution.");
            ShowContinueError("For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;");
            TotalWarningErrors += TotalMultipliedWindows;
        }
        if (TotalCoincidentVertices > 0) {
            ShowWarningMessage(
                RoutineName + "There are " + TrimSigDigits(TotalCoincidentVertices) +
                " coincident/collinear vertices; These have been deleted unless the deletion would bring the number of surface sides < 3.");
            ShowContinueError("For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;");
            TotalWarningErrors += TotalCoincidentVertices;
        }
        if (TotalDegenerateSurfaces > 0) {
            ShowSevereMessage(RoutineName + "There are " + TrimSigDigits(TotalDegenerateSurfaces) +
                              " degenerate surfaces; Degenerate surfaces are those with number of sides < 3.");
            ShowContinueError("These surfaces should be deleted.");
            ShowContinueError("For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;");
            TotalSevereErrors += TotalDegenerateSurfaces;
        }

        GetHTSurfExtVentedCavityData(state, ErrorsFound);

        exposedFoundationPerimeter.getData(state, ErrorsFound);

        GetSurfaceHeatTransferAlgorithmOverrides(state, ErrorsFound);

        // Set up enclosures, process Air Boundaries if any
        SetupEnclosuresAndAirBoundaries(state, DataViewFactorInformation::ZoneRadiantInfo, SurfaceGeometry::enclosureType::RadiantEnclosures, ErrorsFound);

        GetSurfaceSrdSurfsData(state, ErrorsFound);

        GetSurfaceLocalEnvData(state, ErrorsFound);

        if (SurfError || ErrorsFound) {
            ErrorsFound = true;
            ShowFatalError(RoutineName + "Errors discovered, program terminates.");
        }

        int TotShadSurf = TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg + TotShdSubs + TotOverhangs +
                          TotOverhangsProjection + TotFins + TotFinsProjection;
        int NumDElightCmplxFen = inputProcessor->getNumObjectsFound("Daylighting:DElight:ComplexFenestration");
        if (TotShadSurf > 0 && (NumDElightCmplxFen > 0 || DaylightingManager::doesDayLightingUseDElight())) {
            ShowWarningError(RoutineName + "When using DElight daylighting the presence of exterior shading surfaces is ignored.");
        }
    }

    void checkSubSurfAzTiltNorm(SurfaceData &baseSurface, // Base surface data (in)
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
                ShowSevereError("checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than " +
                                General::RoundSigDigits(errorTolerance, 1) + " degrees from base surface.");
                ShowContinueError("Subsurface=\"" + subSurface.Name + "\" Tilt = " + General::RoundSigDigits(subSurface.Tilt, 1) +
                                  "  Azimuth = " + General::RoundSigDigits(subSurface.Azimuth, 1));
                ShowContinueError("Base surface=\"" + baseSurface.Name + "\" Tilt = " + General::RoundSigDigits(baseSurface.Tilt, 1) +
                                  "  Azimuth = " + General::RoundSigDigits(baseSurface.Azimuth, 1));
            } else if (((std::abs(baseSurface.Azimuth - subSurface.Azimuth) > warningTolerance) && !baseSurfHoriz) ||
                       (std::abs(baseSurface.Tilt - subSurface.Tilt) > warningTolerance)) {
                ++checkSubSurfAzTiltNormErrCount;
                if (checkSubSurfAzTiltNormErrCount == 1 && !DisplayExtraWarnings) {
                    ShowWarningError("checkSubSurfAzTiltNorm: Some Outward Facing angles of subsurfaces differ more than " +
                                     General::RoundSigDigits(warningTolerance, 1) + " degrees from base surface.");
                    ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.");
                }
                if (DisplayExtraWarnings) {
                    ShowWarningError("checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than " +
                                     General::RoundSigDigits(warningTolerance, 1) + " degrees from base surface.");
                    ShowContinueError("Subsurface=\"" + subSurface.Name + "\" Tilt = " + General::RoundSigDigits(subSurface.Tilt, 1) +
                                      "  Azimuth = " + General::RoundSigDigits(subSurface.Azimuth, 1));
                    ShowContinueError("Base surface=\"" + baseSurface.Name + "\" Tilt = " + General::RoundSigDigits(baseSurface.Tilt, 1) +
                                      "  Azimuth = " + General::RoundSigDigits(baseSurface.Azimuth, 1));
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
        NumStmt = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
                    ShowSevereError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(1) + '=' + GAlphas(1));
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
                    ShowSevereError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(2) + '=' + GAlphas(2));
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
                    ShowWarningError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(3) + '=' + GAlphas(3));
                    ShowContinueError(cAlphaFieldNames(3) + " defaults to \"WorldCoordinateSystem\"");
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
                    ShowWarningError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(4) + '=' + GAlphas(4));
                    ShowContinueError(cAlphaFieldNames(4) + " defaults to \"RelativeToZoneOrigin\"");
                    DaylRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin,";
                }

                OK = false;
                if (UtilityRoutines::SameString(GAlphas(5), "World") || UtilityRoutines::SameString(GAlphas(5), "Absolute")) {
                    RectSurfRefWorldCoordSystem = true;
                    OutMsg += "WorldCoordinateSystem";
                    OK = true;
                }
                if (UtilityRoutines::SameString(GAlphas(5), "Relative") || GAlphas(5).empty()) {
                    RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                    OK = true;
                }
                if (!OK) {
                    ShowWarningError(cCurrentModuleObject + ": Invalid " + cAlphaFieldNames(5) + '=' + GAlphas(5));
                    ShowContinueError(cAlphaFieldNames(5) + " defaults to \"RelativeToZoneOrigin\"");
                    RectSurfRefWorldCoordSystem = false;
                    OutMsg += "RelativeToZoneOrigin";
                }

            } else if (SELECT_CASE_var == 0) {

                ShowSevereError(cCurrentModuleObject + ": Required object not found.");
                OutMsg += "None found in input";
                ErrorsFound = true;

            } else {

                ShowSevereError(cCurrentModuleObject + ": Too many objects entered.  Only one allowed.");
                ErrorsFound = true;
            }
        }

        if (!WorldCoordSystem) {
            if (DaylRefWorldCoordSystem) {
                ShowWarningError(cCurrentModuleObject + ": Potential mismatch of coordinate specifications.");
                ShowContinueError(cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                ShowContinueError(cAlphaFieldNames(4) + "=\"" + GAlphas(4) + "\".");
            }
            if (RectSurfRefWorldCoordSystem) {
                ShowWarningError(cCurrentModuleObject + ": Potential mismatch of coordinate specifications.");
                ShowContinueError(cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                ShowContinueError(cAlphaFieldNames(5) + "=\"" + GAlphas(5) + "\".");
            }
        } else {
            RelWarning = false;
            for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).OriginX != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginY != 0.0) RelWarning = true;
                if (Zone(ZoneNum).OriginZ != 0.0) RelWarning = true;
            }
            if (RelWarning && !RectSurfRefWorldCoordSystem) {
                ShowWarningError(cCurrentModuleObject + ": Potential mismatch of coordinate specifications. Note that the rectangular surfaces are relying on the default SurfaceGeometry for 'Relative to zone' coordinate.");
                ShowContinueError(cAlphaFieldNames(3) + "=\"" + GAlphas(3) + "\"; while ");
                if (GAlphas(5) == "RELATIVE") {
                    ShowContinueError(cAlphaFieldNames(5) + "=\"" + GAlphas(5) + "\".");
                } else if (GAlphas(5) != "ABSOLUTE") {
                    ShowContinueError(cAlphaFieldNames(5) + "=\"defaults to RELATIVE\".");
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
        using General::TrimSigDigits;
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
            ShowWarningError("Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
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

            inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 2) {
                ShowSevereError(cCurrentModuleObject +
                                ": Object Definition indicates not = 2 Alpha Objects, Number Indicated=" + TrimSigDigits(NumAlphas));
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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                SurfaceTmp(SurfNum).Class = ClassItem;
                SurfaceTmp(SurfNum).HeatTransSurf = false;
                // Base transmittance of a shadowing (sub)surface
                if (!lAlphaFieldBlanks(2)) {
                    // Schedule for a shadowing (sub)surface
                    SurfaceTmp(SurfNum).SchedShadowSurfIndex = GetScheduleIndex(state, cAlphaArgs(2));
                    if (SurfaceTmp(SurfNum).SchedShadowSurfIndex == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) +
                                        " not found=" + cAlphaArgs(2));
                        ErrorsFound = true;
                    }
                } else {
                    SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
                }
                if (SurfaceTmp(SurfNum).SchedShadowSurfIndex != 0) {
                    if (!CheckScheduleValueMinMax(SurfaceTmp(SurfNum).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", values not in range [0,1].");
                        ErrorsFound = true;
                    }
                    SchedMinValue = GetScheduleMinValue(SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                    SurfaceTmp(SurfNum).SchedMinValue = SchedMinValue;
                    SchedMaxValue = GetScheduleMaxValue(SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                    if (SchedMinValue == 1.0) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                         cAlphaArgs(2) + "\", is always transparent.");
                        SurfaceTmp(SurfNum).IsTransparent = true;
                    }
                    if (SchedMinValue < 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", has schedule values < 0.");
                        ShowContinueError("...Schedule values < 0 have no meaning for shading elements.");
                    }
                    if (SchedMaxValue > 1.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\", has schedule values > 1.");
                        ShowContinueError("...Schedule values > 1 have no meaning for shading elements.");
                    }
                    if (std::abs(SchedMinValue - SchedMaxValue) > 1.0e-6) {
                        SurfaceTmp(SurfNum).ShadowSurfSchedVaries = true;
                        ShadingTransmittanceVaries = true;
                    }
                }
                if (lNumericFieldBlanks(1) || rNumericArgs(1) == DataGlobalConstants::AutoCalculate()) {
                    numSides = (NumNumbers - 1) / 3;
                    SurfaceTmp(SurfNum).Sides = numSides;
                    if (mod(NumNumbers - 1, 3) != 0) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(1) +
                                         " not even multiple of 3. Will read in " + TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(1) +
                                        " (autocalculate) must be >= 3. Only " + TrimSigDigits(SurfaceTmp(SurfNum).Sides) + " provided.");
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (NumNumbers - 1) / 3;
                    SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
                    if (numSides > SurfaceTmp(SurfNum).Sides) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", field " + cNumericFieldNames(1) + '=' +
                                         TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                        ShowContinueError("...but " + TrimSigDigits(numSides) + " were entered. Only the indicated " + cNumericFieldNames(1) +
                                          " will be used.");
                    }
                }
                SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);
                GetVertices(state, SurfNum, SurfaceTmp(SurfNum).Sides, rNumericArgs({2, _}));
                CheckConvexity(SurfNum, SurfaceTmp(SurfNum).Sides);
                if (MakeMirroredDetachedShading) {
                    MakeMirrorSurface(SurfNum);
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
        using General::TrimSigDigits;

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
            ShowWarningError("Detached shading effects are ignored when Solar Distribution = MinimalShadowing");
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

            inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
            if (NumAlphas != 1) {
                ShowSevereError(cCurrentModuleObject +
                                ": Object Definition indicates not = 1 Alpha Objects, Number Indicated=" + TrimSigDigits(NumAlphas));
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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                SurfaceTmp(SurfNum).Class = ClassItem;
                SurfaceTmp(SurfNum).HeatTransSurf = false;

                SurfaceTmp(SurfNum).Azimuth = rNumericArgs(1);
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B && !WorldCoordSystem) {
                    SurfaceTmp(SurfNum).Azimuth += BuildingAzimuth;
                }
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                    SurfaceTmp(SurfNum).Azimuth += BuildingRotationAppendixG;
                }
                SurfaceTmp(SurfNum).Tilt = rNumericArgs(2);

                SurfaceTmp(SurfNum).Sides = 4;
                SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);

                MakeRectangularVertices(
                    state, SurfNum, rNumericArgs(3), rNumericArgs(4), rNumericArgs(5), rNumericArgs(6), rNumericArgs(7), RectSurfRefWorldCoordSystem);

                if (SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                    ErrorsFound = true;
                }

                if (MakeMirroredDetachedShading) {
                    MakeMirrorSurface(SurfNum);
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

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

            inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);
            if (Item == 1) {
                if (SurfaceNumAlpha != 8) {
                    ShowSevereError(cCurrentModuleObject +
                                    ": Object Definition indicates not = 8 Alpha Objects, Number Indicated=" + TrimSigDigits(SurfaceNumAlpha));
                    ErrorsFound = true;
                }
            } else {
                if (SurfaceNumAlpha != 7) {
                    ShowSevereError(cCurrentModuleObject +
                                    ": Object Definition indicates not = 7 Alpha Objects, Number Indicated=" + TrimSigDigits(SurfaceNumAlpha));
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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                ArgPointer = 2;
                if (Item == 1) {
                    if (cAlphaArgs(2) == "CEILING") cAlphaArgs(2) = "ROOF";
                    ClassItem = UtilityRoutines::FindItemInList(cAlphaArgs(2), BaseSurfCls, 3);
                    if (ClassItem == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2));
                        ErrorsFound = true;
                    } else {
                        SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem);
                    }
                    ++ArgPointer;
                } else {
                    SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem);
                }

                SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(ArgPointer), state.dataConstruction->Construct, TotConstructs);

                if (SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                } else if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\" - has Window materials.");
                    if (Item == 1) {
                        ShowContinueError("...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                    } else {
                        ShowContinueError("...because Surface Type=" + BaseSurfCls(ClassItem));
                    }
                } else {
                    state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    SurfaceTmp(SurfNum).ConstructionStoredInputValue = SurfaceTmp(SurfNum).Construction;
                }
                SurfaceTmp(SurfNum).HeatTransSurf = true;
                SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                SurfaceTmp(SurfNum).BaseSurfName = SurfaceTmp(SurfNum).Name;

                ++ArgPointer;
                SurfaceTmp(SurfNum).ZoneName = cAlphaArgs(ArgPointer);
                ZoneNum = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ZoneName, Zone, NumOfZones);

                if (ZoneNum != 0) {
                    SurfaceTmp(SurfNum).Zone = ZoneNum;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    SurfaceTmpClassInvalid(SurfNum) = true;
                    SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }
                // Get the ExteriorBoundaryCondition flag from input There are 4 conditions that
                // can take place. The conditions are set with a 0, -1, or -2, or all of the
                // zone names have to be looked at and generate the interzone array number
                ++ArgPointer;
                SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(ArgPointer + 1);

                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Outdoors")) {
                    SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Adiabatic")) {
                    SurfaceTmp(SurfNum).ExtBoundCond = UnreconciledZoneSurface;
                    SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Ground")) {
                    SurfaceTmp(SurfNum).ExtBoundCond = Ground;

                    if (NoGroundTempObjWarning) {
                        if (!GroundTempObjInput) {
                            ShowWarningError("GetHTSurfaceData: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError("Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError("Defaults, constant throughout the year of (" + RoundSigDigits(GroundTemp, 1) + ") will be used.");
                        }
                        NoGroundTempObjWarning = false;
                    }

                    // Added for FCfactor method
                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundFCfactorMethod")) {
                    SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    if (NoFCGroundTempObjWarning) {
                        if (!FCGroundTemps) {
                            ShowSevereError("GetHTSurfaceData: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError("Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(
                                "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures.");
                            ErrorsFound = true;
                            NoFCGroundTempObjWarning = false;
                        }
                    }
                    if (SurfaceTmp(SurfNum).Construction > 0) {
                        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && !state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer));
                            ShowContinueError("Construction=\"" + state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Name +
                                              "\" is not type Construction:CfactorUndergroundWall.");
                            ErrorsFound = true;
                        }
                        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && !state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer));
                            ShowContinueError("Construction=\"" + state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Name +
                                              "\" is not type Construction:FfactorGroundFloor.");
                            ErrorsFound = true;
                        }
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "OtherSideCoefficients")) {
                    Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, OSC, TotOSC);
                    if (Found == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                        ShowContinueError(" no OtherSideCoefficients of that name.");
                        ErrorsFound = true;
                    } else {
                        SurfaceTmp(SurfNum).OSCPtr = Found;
                        if (OSC(Found).SurfFilmCoef > 0.0) {
                            SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Surface")) {
                    // it has to be another surface which needs to be found
                    // this will be found on the second pass through the surface input
                    // for flagging, set the value to UnreconciledZoneSurface
                    // name (ExtBoundCondName) will be validated later.
                    SurfaceTmp(SurfNum).ExtBoundCond = UnreconciledZoneSurface;
                    if (lAlphaFieldBlanks(ArgPointer + 1)) {
                        SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=<blank>.");
                        ShowContinueError(".." + cAlphaFieldNames(ArgPointer) + "=\"Surface\" must be non-blank.");
                        ShowContinueError("..This surface will become an adiabatic surface - no doors/windows allowed.");
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Zone")) {
                    // This is the code for an unmatched "other surface"
                    // will be set up later.
                    SurfaceTmp(SurfNum).ExtBoundCond = UnenteredAdjacentZoneSurface;
                    // check OutsideFaceEnvironment for legal zone
                    Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, Zone, NumOfZones);
                    ++NeedToAddSurfaces;

                    if (Found == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) +
                                        "=\"" + cAlphaArgs(ArgPointer) + "\".");
                        ShowContinueError("..Referenced as Zone for this surface.");
                        ErrorsFound = true;
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "Foundation")) {

                    if (!state.dataWeatherManager->WeatherFileExists) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", using \"Foundation\" type Outside Boundary Condition requires specification of a weather file");
                        ShowContinueError("Either place in.epw in the working directory or specify a weather file on the command line using -w "
                                          "/path/to/weather.epw");
                        ErrorsFound = true;
                    }

                    // Find foundation object, if blank use default
                    if (lAlphaFieldBlanks(ArgPointer + 1)) {

                        if (!kivaManager.defaultSet) {
                            // Apply default foundation if no other foundation object specified
                            if (kivaManager.foundationInputs.size() == 0) {
                                kivaManager.defineDefaultFoundation();
                            }
                            kivaManager.addDefaultFoundation();
                        }
                        SurfaceTmp(SurfNum).OSCPtr = kivaManager.defaultIndex; // Reuse OSC pointer...shouldn't be used for non OSC surfaces anyway.
                    } else {
                        Found = kivaManager.findFoundation(SurfaceTmp(SurfNum).ExtBoundCondName);
                        if (Found != (int)kivaManager.foundationInputs.size()) {
                            SurfaceTmp(SurfNum).OSCPtr = Found;
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " +
                                            cAlphaFieldNames(ArgPointer + 1) + "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                            ErrorsFound = true;
                        }
                    }

                    if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", construction may not have an internal source/sink");
                        ErrorsFound = true;
                    }
                    SurfaceTmp(SurfNum).ExtBoundCond = KivaFoundation;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "OtherSideConditionsModel")) {
                    Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, OSCM, TotOSCM);
                    if (Found == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer + 1) +
                                        "=\"" + cAlphaArgs(ArgPointer + 1) + "\".");
                        ErrorsFound = true;
                    }
                    SurfaceTmp(SurfNum).OSCMPtr = Found;
                    SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCondModeledExt;

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorAverage") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorCore") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundSlabPreprocessorPerimeter") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageFloor") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorAverageWall") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorUpperWall") ||
                           UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "GroundBasementPreprocessorLowerWall")) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ShowContinueError("The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder.");
                    ErrorsFound = true;

                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ShowContinueError("Should be one of \"Outdoors\", \"Adiabatic\", Ground\", \"Surface\", \"OtherSideCoefficients\", "
                                      "\"OtherSideConditionsModel\" or \"Zone\"");
                    ErrorsFound = true;
                } // ... End of the ExtBoundCond logical IF Block

                ArgPointer += 2;
                // Set the logical flag for the exterior solar
                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "SunExposed")) {
                    SurfaceTmp(SurfNum).ExtSolar = true;

                    if ((SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment) && (SurfaceTmp(SurfNum).ExtBoundCond != OtherSideCondModeledExt)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                         cAlphaArgs(ArgPointer) + "\".");
                        ShowContinueError("..This surface is not exposed to External Environment.  Sun exposure has no effect.");
                    }

                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "NoSun")) {
                    SurfaceTmp(SurfNum).ExtSolar = false;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ErrorsFound = true;
                }

                ++ArgPointer;
                // Set the logical flag for the exterior wind
                if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "WindExposed")) {
                    SurfaceTmp(SurfNum).ExtWind = true;
                } else if (UtilityRoutines::SameString(cAlphaArgs(ArgPointer), "NoWind")) {
                    SurfaceTmp(SurfNum).ExtWind = false;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(ArgPointer) + "=\"" +
                                    cAlphaArgs(ArgPointer) + "\".");
                    ErrorsFound = true;
                }

                // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                if (SurfaceTmp(SurfNum).Construction > 0) SurfaceTmp(SurfNum).ExtEcoRoof = state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsEcoRoof;

                SurfaceTmp(SurfNum).ViewFactorGround = rNumericArgs(1);
                if (lNumericFieldBlanks(1)) SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate();
                if (lNumericFieldBlanks(2) || rNumericArgs(2) == DataGlobalConstants::AutoCalculate()) {
                    numSides = (SurfaceNumProp - 2) / 3;
                    SurfaceTmp(SurfNum).Sides = numSides;
                    if (mod(SurfaceNumProp - 2, 3) != 0) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(2) +
                                         " not even multiple of 3. Will read in " + TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                    }
                    if (numSides < 3) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(2) +
                                        " (autocalculate) must be >= 3. Only " + TrimSigDigits(SurfaceTmp(SurfNum).Sides) + " provided.");
                        ErrorsFound = true;
                        continue;
                    }
                } else {
                    numSides = (SurfaceNumProp - 2) / 3;
                    SurfaceTmp(SurfNum).Sides = rNumericArgs(2);
                    if (numSides > SurfaceTmp(SurfNum).Sides) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", field " + cNumericFieldNames(2) + '=' +
                                         TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                        ShowContinueError("...but " + TrimSigDigits(numSides) + " were entered. Only the indicated " + cNumericFieldNames(2) +
                                          " will be used.");
                    }
                }
                SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);
                SurfaceTmp(SurfNum).NewVertex.allocate(SurfaceTmp(SurfNum).Sides);
                GetVertices(state, SurfNum, SurfaceTmp(SurfNum).Sides, rNumericArgs({3, _}));
                if (SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                    ErrorsFound = true;
                }

                CheckConvexity(SurfNum, SurfaceTmp(SurfNum).Sides);
                if (UtilityRoutines::SameString(cAlphaArgs(5), "Surface")) {
                    if (SurfaceTmp(SurfNum).Sides != static_cast<int>(SurfaceTmp(SurfNum).Vertex.size())) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", After CheckConvexity, mismatch between Sides (" + TrimSigDigits(SurfaceTmp(SurfNum).Sides) +
                                        ") and size of Vertex (" + TrimSigDigits(SurfaceTmp(SurfNum).Vertex.size()) + ").");
                        ShowContinueError("CheckConvexity is used to verify the convexity of a surface and detect collinear points.");
                        ErrorsFound = true;
                    }
                }
                if (SurfaceTmp(SurfNum).Construction > 0) {
                    // Check wall height for the CFactor walls
                    if (SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                        if (std::abs(SurfaceTmp(SurfNum).Height - state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Height) > 0.05) {
                            ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                             "\", underground Wall Height = " + TrimSigDigits(SurfaceTmp(SurfNum).Height, 2));
                            ShowContinueError("..which does not match its construction height.");
                        }
                    }

                    // Check area and perimeter for the FFactor floors
                    if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                        if (std::abs(SurfaceTmp(SurfNum).Area - state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Area) > 0.1) {
                            ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                             "\", underground Floor Area = " + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                            ShowContinueError("..which does not match its construction area.");
                        }
                        if (SurfaceTmp(SurfNum).Perimeter < state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).PerimeterExposed - 0.1) {
                            ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                             "\", underground Floor Perimeter = " + TrimSigDigits(SurfaceTmp(SurfNum).Perimeter, 2));
                            ShowContinueError("..which is less than its construction exposed perimeter.");
                        }
                    }
                }
            }
        } // Item Looop
        // Check number of Vertex between base surface and Outside Boundary surface
        int ExtSurfNum;
        for (int i = 1; i <= SurfNum; i++) {
            if (SurfaceTmp(i).ExtBoundCond == UnreconciledZoneSurface && SurfaceTmp(i).ExtBoundCondName != "") {
                ExtSurfNum = UtilityRoutines::FindItemInList(SurfaceTmp(i).ExtBoundCondName, SurfaceTmp);
                // If we cannot find the referenced surface
                if (ExtSurfNum == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(i).Name +
                                    "\" references an outside boundary surface that cannot be found:" + SurfaceTmp(i).ExtBoundCondName);
                    ErrorsFound = true;
                    // If vertex size mistmatch
                } else if (SurfaceTmp(i).Vertex.size() != SurfaceTmp(ExtSurfNum).Vertex.size()) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(i).Name + "\", Vertex size mismatch between base surface :" +
                                    SurfaceTmp(i).Name + " and outside boundary surface: " + SurfaceTmp(ExtSurfNum).Name);
                    ShowContinueError("The vertex sizes are " + TrimSigDigits(SurfaceTmp(i).Vertex.size()) + " for base surface and " +
                                      TrimSigDigits(SurfaceTmp(ExtSurfNum).Vertex.size()) + " for outside boundary surface. Please check inputs.");
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

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
                ExtBoundCondition = UnreconciledZoneSurface;
                ClassItem = 1;
            } else if (Item == 3) {
                ItemsToGet = TotRectIZWalls;
                GettingIZSurfaces = true;
                OtherSurfaceField = 4;
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
                OtherSurfaceField = 4;
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
                OtherSurfaceField = 4;
                ExtBoundCondition = UnreconciledZoneSurface;
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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                if (NumNumbers < 7) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Too few number of numeric args=[" +
                                    TrimSigDigits(NumNumbers) + "].");
                    ErrorsFound = true;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1);           // Set the Surface Name in the Derived Type
                SurfaceTmp(SurfNum).Class = BaseSurfIDs(ClassItem); // Set class number

                SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);

                if (SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                } else if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(2) + "\" - has Window materials.");
                    ShowContinueError("...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                } else {
                    state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    SurfaceTmp(SurfNum).ConstructionStoredInputValue = SurfaceTmp(SurfNum).Construction;
                }
                SurfaceTmp(SurfNum).HeatTransSurf = true;
                SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                SurfaceTmp(SurfNum).BaseSurfName = SurfaceTmp(SurfNum).Name;

                SurfaceTmp(SurfNum).ZoneName = cAlphaArgs(3);
                ZoneNum = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ZoneName, Zone, NumOfZones);

                if (ZoneNum != 0) {
                    SurfaceTmp(SurfNum).Zone = ZoneNum;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\".");
                    SurfaceTmpClassInvalid(SurfNum) = true;
                    SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                }

                SurfaceTmp(SurfNum).ExtBoundCond = ExtBoundCondition;
                if (SurfaceTmp(SurfNum).Construction > 0) {

                    if (SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall &&
                        SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsCfactorWall) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", Construction type is \"Construction:CfactorUndergroundWall\" but invalid for this object.");
                    }
                    if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor &&
                        SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        SurfaceTmp(SurfNum).ExtBoundCond = GroundFCfactorMethod;
                    } else if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsFfactorFloor) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", Construction type is \"Construction:FfactorGroundFloor\" but invalid for this object.");
                    }
                }
                SurfaceTmp(SurfNum).ExtSolar = false;
                SurfaceTmp(SurfNum).ExtWind = false;
                SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate();

                if (SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                    SurfaceTmp(SurfNum).ExtSolar = true;
                    SurfaceTmp(SurfNum).ExtWind = true;

                    // Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
                    if (SurfaceTmp(SurfNum).Construction > 0)
                        SurfaceTmp(SurfNum).ExtEcoRoof = state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsEcoRoof;

                } else if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) {
                    if (GettingIZSurfaces) {
                        SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(OtherSurfaceField);
                        Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, Zone, NumOfZones);
                        // see if match to zone, then it's an unentered other surface, else reconciled later
                        if (Found > 0) {
                            ++NeedToAddSurfaces;
                            SurfaceTmp(SurfNum).ExtBoundCond = UnenteredAdjacentZoneSurface;
                        }
                    } else {
                        SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
                    }

                } else if (SurfaceTmp(SurfNum).ExtBoundCond == Ground) {

                    if (NoGroundTempObjWarning) {
                        if (!GroundTempObjInput) {
                            ShowWarningError("GetRectSurfaces: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input.");
                            ShowContinueError("Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError("Defaults, constant throughout the year of (" + RoundSigDigits(GroundTemp, 1) + ") will be used.");
                        }
                        NoGroundTempObjWarning = false;
                    }

                } else if (SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (NoFCGroundTempObjWarning) {
                        if (!FCGroundTemps) {
                            ShowSevereError("GetRectSurfaces: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground "
                                            "Temperatures\" were input.");
                            ShowContinueError("Found first in surface=" + cAlphaArgs(1));
                            ShowContinueError(
                                "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures.");
                            ErrorsFound = true;
                            NoFCGroundTempObjWarning = false;
                        }
                    }

                } // ... End of the ExtBoundCond logical IF Block

                SurfaceTmp(SurfNum).Azimuth = rNumericArgs(1);
                SurfaceTmp(SurfNum).Tilt = rNumericArgs(2);
                if (!WorldCoordSystem) {
                    if (ZoneNum != 0) {
                        SurfaceTmp(SurfNum).Azimuth += BuildingAzimuth + Zone(ZoneNum).RelNorth;
                    }
                }
                if (ZoneNum != 0) {
                    SurfaceTmp(SurfNum).Azimuth += BuildingRotationAppendixG;
                }

                SurfaceTmp(SurfNum).Sides = 4;
                SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);

                MakeRectangularVertices(
                    state, SurfNum, rNumericArgs(3), rNumericArgs(4), rNumericArgs(5), rNumericArgs(6), rNumericArgs(7), RectSurfRefWorldCoordSystem);

                if (SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                    ErrorsFound = true;
                }

                // Check wall height for the CFactor walls
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Wall && SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(SurfaceTmp(SurfNum).Height - state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Height) > 0.05) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                         "\", underground Wall Height = " + TrimSigDigits(SurfaceTmp(SurfNum).Height, 2));
                        ShowContinueError("..which deos not match its construction height.");
                    }
                }

                // Check area and perimeter for the FFactor floors
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && SurfaceTmp(SurfNum).ExtBoundCond == GroundFCfactorMethod) {
                    if (std::abs(SurfaceTmp(SurfNum).Area - state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Area) > 0.1) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                         "\", underground Floor Area = " + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                        ShowContinueError("..which does not match its construction area.");
                    }
                    if (SurfaceTmp(SurfNum).Perimeter < state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).PerimeterExposed - 0.1) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                         "\", underground Floor Perimeter = " + TrimSigDigits(SurfaceTmp(SurfNum).Perimeter, 2));
                        ShowContinueError("..which is less than its construction exposed perimeter.");
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

        if (SurfaceTmp(SurfNum).Zone == 0 &&
            (SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F && SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_B))
            return;

        SurfaceTmp(SurfNum).Height = Height;
        SurfaceTmp(SurfNum).Width = Length;

        SurfAzimuth = SurfaceTmp(SurfNum).Azimuth;
        SurfTilt = SurfaceTmp(SurfNum).Tilt;
        CosSurfAzimuth = std::cos(SurfAzimuth * DataGlobalConstants::DegToRadians());
        SinSurfAzimuth = std::sin(SurfAzimuth * DataGlobalConstants::DegToRadians());
        CosSurfTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
        SinSurfTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
        if (!SurfWorldCoordSystem) {
            if (SurfaceTmp(SurfNum).Zone > 0) {
                Xb = XCoord * CosZoneRelNorth(SurfaceTmp(SurfNum).Zone) - YCoord * SinZoneRelNorth(SurfaceTmp(SurfNum).Zone) +
                     Zone(SurfaceTmp(SurfNum).Zone).OriginX;
                Yb = XCoord * SinZoneRelNorth(SurfaceTmp(SurfNum).Zone) + YCoord * CosZoneRelNorth(SurfaceTmp(SurfNum).Zone) +
                     Zone(SurfaceTmp(SurfNum).Zone).OriginY;
                XLLC = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                YLLC = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
                ZLLC = ZCoord + Zone(SurfaceTmp(SurfNum).Zone).OriginZ;
            } else {
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                    Xb = XCoord;
                    Yb = YCoord;
                    XLLC = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                    YLLC = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
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
            if (SurfaceTmp(SurfNum).Class != SurfaceClass::Detached_F) {
                XLLC = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
                YLLC = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
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

        for (n = 1; n <= SurfaceTmp(SurfNum).Sides; ++n) {
            Vrt = n;
            SurfaceTmp(SurfNum).Vertex(Vrt).x = XLLC - XX(n) * CosSurfAzimuth - YY(n) * CosSurfTilt * SinSurfAzimuth;
            SurfaceTmp(SurfNum).Vertex(Vrt).y = YLLC + XX(n) * SinSurfAzimuth - YY(n) * CosSurfTilt * CosSurfAzimuth;
            SurfaceTmp(SurfNum).Vertex(Vrt).z = ZLLC + YY(n) * SinSurfTilt;
        }

        CreateNewellAreaVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellAreaVector);
        SurfaceTmp(SurfNum).GrossArea = VecLength(SurfaceTmp(SurfNum).NewellAreaVector);
        SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).GrossArea;
        SurfaceTmp(SurfNum).NetAreaShadowCalc = SurfaceTmp(SurfNum).Area;
        CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(SurfaceTmp(SurfNum).Vertex,
                                SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                SurfaceTmp(SurfNum).lcsx,
                                SurfaceTmp(SurfNum).lcsy,
                                SurfaceTmp(SurfNum).lcsz,
                                SurfaceTmp(SurfNum).GrossArea,
                                SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        SurfaceTmp(SurfNum).Azimuth = SurfAzimuth;
        SurfaceTmp(SurfNum).Tilt = SurfTilt;
        // Sine and cosine of azimuth and tilt
        SurfaceTmp(SurfNum).SinAzim = SinSurfAzimuth;
        SurfaceTmp(SurfNum).CosAzim = CosSurfAzimuth;
        SurfaceTmp(SurfNum).SinTilt = SinSurfTilt;
        SurfaceTmp(SurfNum).CosTilt = CosSurfTilt;
        SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);
        // Outward normal unit vector (pointing away from room)
        SurfaceTmp(SurfNum).OutNormVec = SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
        for (n = 1; n <= 3; ++n) {
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
        }

        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Roof .and. SurfTilt > 80.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Floor .and. SurfTilt < 170.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF

        // Can perform tests on this surface here
        SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + SurfaceTmp(SurfNum).CosTilt);
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        SurfaceTmp(SurfNum).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSky;
        SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);

        Perimeter = distance(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides), SurfaceTmp(SurfNum).Vertex(1));
        for (Vrt = 2; Vrt <= SurfaceTmp(SurfNum).Sides; ++Vrt) {
            Perimeter += distance(SurfaceTmp(SurfNum).Vertex(Vrt), SurfaceTmp(SurfNum).Vertex(Vrt - 1));
        }
        SurfaceTmp(SurfNum).Perimeter = Perimeter;

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, SurfaceTmp(SurfNum).Sides);
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

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
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp);

        if (SurfaceNumAlpha != 6) {
            ShowSevereError(cCurrentModuleObject +
                            ": Object Definition indicates not = 6 Alpha Objects, Number Indicated=" + TrimSigDigits(SurfaceNumAlpha));
            ErrorsFound = true;
        }

        if (SurfaceNumProp != 15) {
            ShowSevereError(cCurrentModuleObject +
                            ": Object Definition indicates > 15 Numeric Objects, Number Indicated=" + TrimSigDigits(SurfaceNumAlpha));
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

            if (GlobalNames::VerifyUniqueInterObjectName(UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            if (SurfaceNumProp < 12) {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", Too few number of numeric args=[" +
                                TrimSigDigits(SurfaceNumProp) + "].");
                ErrorsFound = true;
            }

            ++SurfNum;
            SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
            ValidChk = UtilityRoutines::FindItemInList(cAlphaArgs(2), SubSurfCls, 6);
            if (ValidChk == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ErrorsFound = true;
            } else {
                SurfaceTmp(SurfNum).Class = SubSurfIDs(ValidChk); // Set class number
            }

            SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct, TotConstructs);

            if (SurfaceTmp(SurfNum).Construction == 0) {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
            } else {
                state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).IsUsed = true;
                SurfaceTmp(SurfNum).ConstructionStoredInputValue = SurfaceTmp(SurfNum).Construction;
            }

            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser || SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome) {

                if (SurfaceTmp(SurfNum).Construction != 0) {
                    if (!state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\" has an opaque surface construction; it should have a window construction.");
                    }
                    if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\": Windows are not allowed to have embedded sources/sinks");
                    }
                }

            } else if (SurfaceTmp(SurfNum).Construction != 0) {
                if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\" - has Window materials.");
                    ShowContinueError("...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2));
                }
            }

            SurfaceTmp(SurfNum).HeatTransSurf = true;

            SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(4);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp, TotSurfaces);
            if (Found > 0) {
                SurfaceTmp(SurfNum).BaseSurf = Found;
                SurfaceTmp(SurfNum).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(Found).ExtBoundCondName;
                SurfaceTmp(SurfNum).ExtSolar = SurfaceTmp(Found).ExtSolar;
                SurfaceTmp(SurfNum).ExtWind = SurfaceTmp(Found).ExtWind;
                SurfaceTmp(SurfNum).Zone = SurfaceTmp(Found).Zone;
                SurfaceTmp(SurfNum).ZoneName = SurfaceTmp(Found).ZoneName;
                SurfaceTmp(SurfNum).OSCPtr = SurfaceTmp(Found).OSCPtr;
                if (SurfaceTmp(Found).ExtBoundCond == UnreconciledZoneSurface &&
                    SurfaceTmp(Found).ExtBoundCondName == SurfaceTmp(Found).Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(4) + "=\"" +
                                    cAlphaArgs(4) + "\".");
                    ShowContinueError("... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError("... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }
            } else {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(4) + "=\"" +
                                cAlphaArgs(4));
                SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                ErrorsFound = true;
            }

            if (SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome || SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser) {
                SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
            }

            if (SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                if (!lAlphaFieldBlanks(5)) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid field " + cAlphaFieldNames(5));
                    ShowContinueError("...when Base surface uses \"Outdoors\" as " + cAlphaFieldNames(5) +
                                      ", subsurfaces need to be blank to inherit the outdoor characteristics.");
                    ShowContinueError("...Surface external characteristics changed to reflect base surface.");
                }
            }

            if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                if (!lAlphaFieldBlanks(5)) {
                    SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5);
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid blank " + cAlphaFieldNames(5));
                    ShowContinueError("...when Base surface uses \"Surface\" as " + cAlphaFieldNames(5) +
                                      ", subsurfaces must also specify specific surfaces in the adjacent zone.");
                    SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5); // putting it as blank will not confuse things later.
                    ErrorsFound = true;
                }
            }

            if (SurfaceTmp(SurfNum).ExtBoundCond == UnenteredAdjacentZoneSurface) { // "Zone" - unmatched interior surface
                ++NeedToAddSurfaces;
                // ignoring window5datafiles for now -- will need to add.
            }

            if (SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                if (!lAlphaFieldBlanks(5)) { // Otherside Coef special Name
                    Found = UtilityRoutines::FindItemInList(cAlphaArgs(5), OSC, TotOSC);
                    if (Found == 0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(5) + "=\"" +
                                        cAlphaArgs(5) + "\".");
                        ShowContinueError("...base surface requires that this subsurface have OtherSideCoefficients -- not found.");
                        ErrorsFound = true;
                    } else { // found
                        // The following allows for a subsurface that has different characteristics than
                        // the base surface with OtherSide Coeff -- do we want that or is it an error?
                        SurfaceTmp(SurfNum).OSCPtr = Found;
                        SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(5);
                        if (OSC(Found).SurfFilmCoef > 0.0) {
                            SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefCalcExt;
                        } else {
                            SurfaceTmp(SurfNum).ExtBoundCond = OtherSideCoefNoCalcExt;
                        }
                    }
                }
            }

            if (SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
            }

            if (SurfaceTmp(SurfNum).ExtBoundCondName == BlankString) {
                SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
            }
            SurfaceTmp(SurfNum).ViewFactorGround = rNumericArgs(1);
            if (lNumericFieldBlanks(1)) SurfaceTmp(SurfNum).ViewFactorGround = DataGlobalConstants::AutoCalculate();

            if (lNumericFieldBlanks(3) || rNumericArgs(3) == DataGlobalConstants::AutoCalculate()) {
                rNumericArgs(3) = (SurfaceNumProp - 3) / 3;
                SurfaceTmp(SurfNum).Sides = rNumericArgs(3);
                if (mod(SurfaceNumProp - 3, 3) != 0) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(3) +
                                     " not even multiple of 3. Will read in " + TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                }
                if (rNumericArgs(3) < 3) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(3) +
                                    " (autocalculate) must be >= 3. Only " + TrimSigDigits(SurfaceTmp(SurfNum).Sides) + " provided.");
                    ErrorsFound = true;
                    continue;
                }
            } else {
                numSides = (SurfaceNumProp - 2) / 3;
                SurfaceTmp(SurfNum).Sides = rNumericArgs(3);
                if (numSides > SurfaceTmp(SurfNum).Sides) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", field " + cNumericFieldNames(3) + '=' +
                                     TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                    ShowContinueError("...but " + TrimSigDigits(numSides) + " were entered. Only the indicated " + cNumericFieldNames(3) +
                                      " will be used.");
                }
            }
            SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);
            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                SurfaceTmp(SurfNum).Multiplier = int(rNumericArgs(2));
            // Only windows, glass doors and doors can have Multiplier > 1:
            if ((SurfaceTmp(SurfNum).Class != SurfaceClass::Window && SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
                 SurfaceTmp(SurfNum).Class != SurfaceClass::Door) &&
                rNumericArgs(2) > 1.0) {
                ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cNumericFieldNames(2) + "=[" +
                                 TrimSigDigits(rNumericArgs(2), 1) + "].");
                ShowContinueError("...because " + cAlphaFieldNames(2) + '=' + cAlphaArgs(2) + " multiplier will be set to 1.0.");
                SurfaceTmp(SurfNum).Multiplier = 1.0;
            }

            GetVertices(state, SurfNum, SurfaceTmp(SurfNum).Sides, rNumericArgs({4, _}));

            CheckConvexity(SurfNum, SurfaceTmp(SurfNum).Sides);
            SurfaceTmp(SurfNum).windowShadingControlList.clear();
            SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
            SurfaceTmp(SurfNum).HasShadeControl = false;


            SurfaceTmp(SurfNum).shadedConstructionList.clear();
            SurfaceTmp(SurfNum).activeShadedConstruction = 0;
            SurfaceTmp(SurfNum).shadedStormWinConstructionList.clear();
            SurfaceTmp(SurfNum).activeStormWinShadedConstruction= 0;

            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser || SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Dome) {

                if (SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Other side coefficients are not allowed with windows.");
                    ErrorsFound = true;
                }

                if (SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Exterior boundary condition = Ground is not allowed with windows.");
                    ErrorsFound = true;
                }

                if (SurfaceTmp(SurfNum).ExtBoundCond == KivaFoundation) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Exterior boundary condition = Foundation is not allowed with windows.");
                    ErrorsFound = true;
                }

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                CheckWindowShadingControlFrameDivider(state, "GetHTSubSurfaceData", ErrorsFound, SurfNum, 6);

                if (SurfaceTmp(SurfNum).Sides == 3) { // Triangular window
                    if (!cAlphaArgs(6).empty()) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(6) + "=\"" +
                                         cAlphaArgs(6) + "\".");
                        ShowContinueError(".. because it is a triangular window and cannot have a frame or divider or reveal reflection.");
                        ShowContinueError("Frame, divider and reveal reflection will be ignored for this window.");
                    }
                    SurfaceTmp(SurfNum).FrameDivider = 0;
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                if (NumNumbers < 5) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Too few number of numeric args=[" +
                                    TrimSigDigits(NumNumbers) + "].");
                    ErrorsFound = true;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1);          // Set the Surface Name in the Derived Type
                SurfaceTmp(SurfNum).Class = SubSurfIDs(ClassItem); // Set class number

                SurfaceTmp(SurfNum).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);

                if (SurfaceTmp(SurfNum).Construction == 0) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                } else {
                    state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).IsUsed = true;
                    SurfaceTmp(SurfNum).ConstructionStoredInputValue = SurfaceTmp(SurfNum).Construction;
                }

                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) {

                    if (SurfaceTmp(SurfNum).Construction != 0) {
                        if (!state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                            "\" has an opaque surface construction; it should have a window construction.");
                        }
                        if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).SourceSinkPresent) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                            "\": Windows are not allowed to have embedded sources/sinks");
                        }
                    }

                } else if (SurfaceTmp(SurfNum).Construction != 0) {
                    if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                        cAlphaArgs(2) + "\" - has Window materials.");
                    }
                }

                SurfaceTmp(SurfNum).HeatTransSurf = true;

                SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(3);
                //  The subsurface inherits properties from the base surface
                //  Exterior conditions, Zone, etc.
                //  We can figure out the base surface though, because they've all been entered
                Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp, TotSurfaces);
                if (Found > 0) {
                    SurfaceTmp(SurfNum).BaseSurf = Found;
                    SurfaceTmp(SurfNum).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                    SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(Found).ExtBoundCondName;
                    SurfaceTmp(SurfNum).ExtSolar = SurfaceTmp(Found).ExtSolar;
                    SurfaceTmp(SurfNum).ExtWind = SurfaceTmp(Found).ExtWind;
                    SurfaceTmp(SurfNum).Tilt = SurfaceTmp(Found).Tilt;
                    SurfaceTmp(SurfNum).Azimuth = SurfaceTmp(Found).Azimuth;
                    SurfaceTmp(SurfNum).Zone = SurfaceTmp(Found).Zone;
                    SurfaceTmp(SurfNum).ZoneName = SurfaceTmp(Found).ZoneName;
                    SurfaceTmp(SurfNum).OSCPtr = SurfaceTmp(Found).OSCPtr;
                    SurfaceTmp(SurfNum).ViewFactorGround = SurfaceTmp(Found).ViewFactorGround;
                    SurfaceTmp(SurfNum).ViewFactorSky = SurfaceTmp(Found).ViewFactorSky;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3));
                    SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                    ErrorsFound = true;
                    continue;
                }
                if (SurfaceTmp(Found).ExtBoundCond == UnreconciledZoneSurface &&
                    SurfaceTmp(Found).ExtBoundCondName == SurfaceTmp(Found).Name) { // Adiabatic surface, no windows or doors allowed
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                    cAlphaArgs(3) + "\".");
                    ShowContinueError("... adiabatic surfaces cannot have windows or doors.");
                    ShowContinueError("... no solar transmission will result for these windows or doors. You must have interior windows or doors on "
                                      "Interzone surfaces for transmission to result.");
                }

                if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (!GettingIZSurfaces) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid use of object");
                        ShowContinueError("...when Base surface uses \"Surface\" as " + cAlphaFieldNames(5) +
                                          ", subsurfaces must also specify specific surfaces in the adjacent zone.");
                        ShowContinueError("...Please use " + cCurrentModuleObject + ":Interzone to enter this surface.");
                        SurfaceTmp(SurfNum).ExtBoundCondName = BlankString; // putting it as blank will not confuse things later.
                        ErrorsFound = true;
                    }
                }

                if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) { // "Surface" Base Surface
                    if (GettingIZSurfaces) {
                        SurfaceTmp(SurfNum).ExtBoundCondName = cAlphaArgs(OtherSurfaceField);
                        IZFound = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).ExtBoundCondName, Zone, NumOfZones);
                        if (IZFound > 0) SurfaceTmp(SurfNum).ExtBoundCond = UnenteredAdjacentZoneSurface;
                    } else { // Interior Window
                        SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
                    }
                }

                // This is the parent's property:
                if (SurfaceTmp(SurfNum).ExtBoundCond == UnenteredAdjacentZoneSurface) { // OtherZone - unmatched interior surface
                    if (GettingIZSurfaces) {
                        ++NeedToAddSubSurfaces;
                    } else { // Interior Window
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid Interzone Surface, specify " +
                                        cCurrentModuleObject + ":InterZone");
                        ShowContinueError("...when base surface is an interzone surface, subsurface must also be an interzone surface.");
                        ++NeedToAddSubSurfaces;
                        ErrorsFound = true;
                    }
                }

                if (GettingIZSurfaces) {
                    if (lAlphaFieldBlanks(OtherSurfaceField)) {
                        // blank -- set it up for unentered adjacent zone
                        if (SurfaceTmp(SurfNum).ExtBoundCond == UnenteredAdjacentZoneSurface) {        // already set but need Zone
                            SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(Found).ExtBoundCondName; // base surface has it
                        } else if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) {
                            SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(Found).ZoneName; // base surface has it
                            SurfaceTmp(SurfNum).ExtBoundCond = UnenteredAdjacentZoneSurface;
                        } else { // not correct boundary condition for interzone subsurface
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                            "\", invalid Base Surface type for Interzone Surface");
                            ShowContinueError("...when base surface is not an interzone surface, subsurface must also not be an interzone surface.");
                            ErrorsFound = true;
                        }
                    }
                }

                if (SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCondModeledExt) {
                    SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment;
                }

                //      SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

                SurfaceTmp(SurfNum).Sides = 4;
                SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);
                if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                    SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                    SurfaceTmp(SurfNum).Multiplier = int(rNumericArgs(1));
                // Only windows, glass doors and doors can have Multiplier > 1:
                if ((SurfaceTmp(SurfNum).Class != SurfaceClass::Window && SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
                     SurfaceTmp(SurfNum).Class != SurfaceClass::Door) &&
                    rNumericArgs(1) > 1.0) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cNumericFieldNames(1) + "=[" +
                                     TrimSigDigits(rNumericArgs(1), 1) + "].");
                    ShowContinueError("...because " + cAlphaFieldNames(1) + '=' + cAlphaArgs(1) + " multiplier will be set to 1.0.");
                    SurfaceTmp(SurfNum).Multiplier = 1.0;
                }

                MakeRelativeRectangularVertices(
                    state, SurfaceTmp(SurfNum).BaseSurf, SurfNum, rNumericArgs(2), rNumericArgs(3), rNumericArgs(4), rNumericArgs(5));

                if (SurfaceTmp(SurfNum).Area <= 0.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits(SurfaceTmp(SurfNum).Area, 2));
                    ErrorsFound = true;
                }

                SurfaceTmp(SurfNum).windowShadingControlList.clear();
                SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
                SurfaceTmp(SurfNum).HasShadeControl = false;

                SurfaceTmp(SurfNum).shadedConstructionList.clear();
                SurfaceTmp(SurfNum).activeShadedConstruction = 0;
                SurfaceTmp(SurfNum).shadedStormWinConstructionList.clear();
                SurfaceTmp(SurfNum).activeStormWinShadedConstruction= 0;

                InitialAssociateWindowShadingControlFenestration(state, ErrorsFound, SurfNum);

                if (!GettingIZSurfaces && (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor)) {

                    if (SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp(SurfNum).ExtBoundCond == OtherSideCoefCalcExt) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                        "\", Other side coefficients are not allowed with windows.");
                        ErrorsFound = true;
                    }

                    if (SurfaceTmp(SurfNum).ExtBoundCond == Ground) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

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

        for (std::size_t shadeControlIndex = 0; shadeControlIndex < SurfaceTmp(SurfNum).windowShadingControlList.size(); ++shadeControlIndex){
            int WSCPtr = SurfaceTmp(SurfNum).windowShadingControlList[shadeControlIndex];
            ConstrNumSh = 0;
            if (!ErrorsFound && SurfaceTmp(SurfNum).HasShadeControl) {
                ConstrNumSh = SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex];
                if (ConstrNumSh > 0) {
                    SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNumSh;
                } else {
                    if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorShade ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorScreen ||
                        WindowShadingControl(WSCPtr).ShadingType == WSC_ST_ExteriorBlind) {
                        ShDevNum = WindowShadingControl(WSCPtr).ShadingDevice;
                        if (ShDevNum > 0) {
                            CreateShadedWindowConstruction(state, SurfNum, WSCPtr, ShDevNum, shadeControlIndex);
                            ConstrNumSh = SurfaceTmp(SurfNum).activeShadedConstruction;
                        }
                    }
                }
            }

            // Error checks for shades and blinds

            ConstrNum = SurfaceTmp(SurfNum).Construction;
            if (!ErrorsFound && WSCPtr > 0 && ConstrNum > 0 && ConstrNumSh > 0) {

                if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind) {
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 1 != TotLayers) {
                        ShowWarningError("WindowShadingControl: Interior shade or blind: Potential problem in match of unshaded/shaded constructions, "
                                         "shaded should have 1 more layers than unshaded.");
                        ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError("If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay)) {
                            ErrorsFound = true;
                            ShowSevereError(" The glass and gas layers in the shaded and unshaded constructions do not match for window=" +
                                            SurfaceTmp(SurfNum).Name);
                            ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                            ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
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
                        ShowWarningError("WindowShadingControl: Exterior shade, screen or blind: Potential problem in match of unshaded/shaded "
                                         "constructions, shaded should have 1 more layer than unshaded.");
                        ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError("If preceding two constructions have the same name, you have likely specified a WindowShadingControl (Field "
                                          "#3) with the Window Construction rather than a shaded construction.");
                    }
                    for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                        if (state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(Lay + 1)) {
                            ErrorsFound = true;
                            ShowSevereError(" The glass and gas layers in the shaded and unshaded constructions do not match for window=" +
                                            SurfaceTmp(SurfNum).Name);
                            ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                            ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                            break;
                        }
                    }
                }

                if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassShade ||
                    WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                    // Divider not allowed with between-glass shade or blind
                    if (SurfaceTmp(SurfNum).FrameDivider > 0) {
                        if (FrameDivider(SurfaceTmp(SurfNum).FrameDivider).DividerWidth > 0.0) {
                            ShowWarningError("A divider cannot be specified for window " + SurfaceTmp(SurfNum).Name);
                            ShowContinueError(", which has a between-glass shade or blind.");
                            ShowContinueError("Calculation will proceed without the divider for this window.");
                            FrameDivider(SurfaceTmp(SurfNum).FrameDivider).DividerWidth = 0.0;
                        }
                    }
                    // Check consistency of gap widths between unshaded and shaded constructions
                    TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                    TotLayers = state.dataConstruction->Construct(ConstrNum).TotLayers;
                    TotShLayers = state.dataConstruction->Construct(ConstrNumSh).TotLayers;
                    if (TotShLayers - 2 != TotLayers) {
                        ShowWarningError("WindowShadingControl: Between Glass Shade/Blind: Potential problem in match of unshaded/shaded constructions, "
                                         "shaded should have 2 more layers than unshaded.");
                        ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name);
                        ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name);
                        ShowContinueError("If preceding two constructions are same name, you have likely specified a WindowShadingControl (Field #3) "
                                          "with the Window Construction rather than a shaded construction.");
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers)) {
                        ShowSevereError(cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match.");
                        ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                          ", Material=" + dataMaterial.Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayers)).Name);
                        ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name +
                                          ", Material=" + dataMaterial.Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotShLayers)).Name);
                        ErrorsFound = true;
                    }
                    if (state.dataConstruction->Construct(ConstrNum).LayerPoint(1) != state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)) {
                        ShowSevereError(cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match.");
                        ShowContinueError("Unshaded construction=" + state.dataConstruction->Construct(ConstrNum).Name +
                                          ", Material=" + dataMaterial.Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(1)).Name);
                        ShowContinueError("Shaded construction=" + state.dataConstruction->Construct(ConstrNumSh).Name +
                                          ", Material=" + dataMaterial.Material(state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1)).Name);
                        ErrorsFound = true;
                    }
                    if (TotGlassLayers == 2 || TotGlassLayers == 3) {
                        MatGap = state.dataConstruction->Construct(ConstrNum).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap1 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 2);
                        MatGap2 = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers);
                        MatSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(2 * TotGlassLayers - 1);
                        if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                            MatGapCalc = std::abs(dataMaterial.Material(MatGap).Thickness - (dataMaterial.Material(MatGap1).Thickness + dataMaterial.Material(MatGap2).Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(cRoutineName + ": The gap width(s) for the unshaded window construction " + state.dataConstruction->Construct(ConstrNum).Name);
                                ShowContinueError("are inconsistent with the gap widths for shaded window construction " + state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError("for window " + SurfaceTmp(SurfNum).Name + ", which has a between-glass blind.");
                                ShowContinueError("..Material=" + dataMaterial.Material(MatGap).Name + " thickness=" + RoundSigDigits(dataMaterial.Material(MatGap).Thickness, 3) +
                                                  " -");
                                ShowContinueError("..( Material=" + dataMaterial.Material(MatGap1).Name +
                                                  " thickness=" + RoundSigDigits(dataMaterial.Material(MatGap1).Thickness, 3) + " +");
                                ShowContinueError("..Material=" + dataMaterial.Material(MatGap2).Name + " thickness=" +
                                                  RoundSigDigits(dataMaterial.Material(MatGap2).Thickness, 3) + " )=[" + RoundSigDigits(MatGapCalc, 3) + "] >.001");
                                ErrorsFound = true;
                            }
                        } else { // Between-glass shade
                            MatGapCalc = std::abs(dataMaterial.Material(MatGap).Thickness -
                                                  (dataMaterial.Material(MatGap1).Thickness + dataMaterial.Material(MatGap2).Thickness + dataMaterial.Material(MatSh).Thickness));
                            if (MatGapCalc > 0.001) {
                                ShowSevereError(cRoutineName + ": The gap width(s) for the unshaded window construction " + state.dataConstruction->Construct(ConstrNum).Name);
                                ShowContinueError("are inconsistent with the gap widths for shaded window construction " + state.dataConstruction->Construct(ConstrNumSh).Name);
                                ShowContinueError("for window " + SurfaceTmp(SurfNum).Name + ", which has a between-glass shade.");
                                ShowContinueError("..Material=" + dataMaterial.Material(MatGap).Name + " thickness=" + RoundSigDigits(dataMaterial.Material(MatGap).Thickness, 3) +
                                    " -");
                                ShowContinueError("...( Material=" + dataMaterial.Material(MatGap1).Name +
                                    " thickness=" + RoundSigDigits(dataMaterial.Material(MatGap1).Thickness, 3) + " +");
                                ShowContinueError("..Material=" + dataMaterial.Material(MatGap2).Name +
                                    " thickness=" + RoundSigDigits(dataMaterial.Material(MatGap2).Thickness, 3) + " +");
                                ShowContinueError("..Material=" + dataMaterial.Material(MatSh).Name + " thickness=" + RoundSigDigits(dataMaterial.Material(MatSh).Thickness, 3) +
                                    " )=[" + RoundSigDigits(MatGapCalc, 3) + "] >.001");
                                ErrorsFound = true;
                            }
                        }
                    }
                }
            }
        }

        if (SurfaceTmp(SurfNum).Sides != 3) { // Rectangular Window
            // Initialize the FrameDivider number for this window. W5FrameDivider will be positive if
            // this window's construction came from the Window5 data file and that construction had an
            // associated frame or divider. It will be zero if the window's construction is not from the
            // Window5 data file, or the construction is from the data file, but the construction has no
            // associated frame or divider. Note that if there is a FrameDivider candidate for this
            // window from the Window5 data file it is used instead of the window's input FrameDivider.

            if (SurfaceTmp(SurfNum).Construction != 0) {
                SurfaceTmp(SurfNum).FrameDivider = state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).W5FrameDivider;

                // Warning if FrameAndDivider for this window is over-ridden by one from Window5 Data File
                if (SurfaceTmp(SurfNum).FrameDivider > 0 && !lAlphaFieldBlanks(FrameField)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(FrameField) + "=\"" +
                                    cAlphaArgs(FrameField) + "\"");
                    ShowContinueError("will be replaced with FrameAndDivider from Window5 Data File entry " +
                                      state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).Name);
                }

                if (!lAlphaFieldBlanks(FrameField) && SurfaceTmp(SurfNum).FrameDivider == 0) {
                    SurfaceTmp(SurfNum).FrameDivider = UtilityRoutines::FindItemInList(cAlphaArgs(FrameField), FrameDivider);
                    if (SurfaceTmp(SurfNum).FrameDivider == 0) {
                        if (!state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) +
                                            "=\"" + cAlphaArgs(FrameField) + "\"");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) +
                                            "=\"" + cAlphaArgs(FrameField) + "\"");
                            ShowContinueError("...Frame/Divider is not supported in Equivalent Layer Window model.");
                        }
                    }
                    // Divider not allowed with between-glass shade or blind
                    for (int WSCPtr : SurfaceTmp(SurfNum).windowShadingControlList) {
                        if (!ErrorsFound && WSCPtr > 0 && ConstrNumSh > 0) {
                            if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassShade ||
                                WindowShadingControl(WSCPtr).ShadingType == WSC_ST_BetweenGlassBlind) {
                                if (SurfaceTmp(SurfNum).FrameDivider > 0) {
                                    if (FrameDivider(SurfaceTmp(SurfNum).FrameDivider).DividerWidth > 0.0) {
                                        ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " +
                                                        cAlphaFieldNames(FrameField) + "=\"" + cAlphaArgs(FrameField) + "\"");
                                        ShowContinueError("Divider cannot be specified because the construction has a between-glass shade or blind.");
                                        ShowContinueError("Calculation will proceed without the divider for this window.");
                                        ShowContinueError("Divider width = [" +
                                                          RoundSigDigits(FrameDivider(SurfaceTmp(SurfNum).FrameDivider).DividerWidth, 2) + "].");
                                        FrameDivider(SurfaceTmp(SurfNum).FrameDivider).DividerWidth = 0.0;
                                    }
                                } // End of check if window has divider
                            }     // End of check if window has a between-glass shade or blind
                        }         // End of check if window has a shaded construction
                    }             // end of looping through window shading controls of window
                }             // End of check if window has an associated FrameAndDivider
            }                 // End of check if window has a construction
        }

        if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
            if (SurfaceTmp(SurfNum).FrameDivider > 0) {
                // Equivalent Layer window does not have frame/divider model
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(FrameField) + "=\"" +
                                cAlphaArgs(FrameField) + "\"");
                ShowContinueError("Frame/Divider is not supported in Equivalent Layer Window model.");
                SurfaceTmp(SurfNum).FrameDivider = 0;
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using namespace DataErrorTracking;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumShades; // count on number of shading layers
        int Lay;       // Layer number
        int LayerPtr;  // Layer pointer
        int ConstrNum; // Construction number
        int Found;     // when item is found

        // Warning if window has multiplier > 1 and SolarDistribution = FullExterior or FullInteriorExterior

        if ((SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) &&
            SolarDistribution > MinimalShadowing && SurfaceTmp(SurfNum).Multiplier > 1.0) {
            if (DisplayExtraWarnings) {
                ShowWarningError(cRoutineName + ": A Multiplier > 1.0 for window/glass door " + SurfaceTmp(SurfNum).Name);
                ShowContinueError("in conjunction with SolarDistribution = FullExterior or FullInteriorExterior");
                ShowContinueError("can cause inaccurate shadowing on the window and/or");
                ShowContinueError("inaccurate interior solar distribution from the window.");
            }
            ++TotalMultipliedWindows;
        }

        //  Require that a construction referenced by a surface that is a window
        //  NOT have a shading device layer; use WindowShadingControl to specify a shading device.
        ConstrNum = SurfaceTmp(SurfNum).Construction;
        if (ConstrNum > 0) {
            NumShades = 0;
            for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                if (LayerPtr == 0) continue; // Error is caught already, will terminate later
                if (dataMaterial.Material(LayerPtr).Group == Shade || dataMaterial.Material(LayerPtr).Group == WindowBlind || dataMaterial.Material(LayerPtr).Group == Screen) ++NumShades;
            }
            if (NumShades != 0) {
                ShowSevereError(cRoutineName + ": Window \"" + SubSurfaceName + "\" must not directly reference");
                ShowContinueError("a Construction (i.e, \"" + SubSurfaceConstruction + "\") with a shading device.");
                ShowContinueError("Use WindowShadingControl to specify a shading device for a window.");
                ErrorsFound = true;
            }
        }

        // Disallow glass transmittance dirt factor for interior windows and glass doors

        if (SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment &&
            (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor)) {
            ConstrNum = SurfaceTmp(SurfNum).Construction;
            if (ConstrNum > 0) {
                for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotLayers; ++Lay) {
                    LayerPtr = state.dataConstruction->Construct(ConstrNum).LayerPoint(Lay);
                    if (dataMaterial.Material(LayerPtr).Group == WindowGlass && dataMaterial.Material(LayerPtr).GlassTransDirtFactor < 1.0) {
                        ShowSevereError(cRoutineName + ": Interior Window or GlassDoor " + SubSurfaceName + " has a glass layer with");
                        ShowContinueError("Dirt Correction Factor for Solar and Visible Transmittance < 1.0");
                        ShowContinueError("A value less than 1.0 for this factor is only allowed for exterior windows and glass doors.");
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

        if (SurfaceTmp(SurfNum).Construction != 0) {

            if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).FromWindow5DataFile) {

                ModifyWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else {
                // Calculate net area for base surface (note that ModifyWindow, above, adjusts net area of
                // base surface for case where window construction is from Window5 Data File
                // In case there is in error in this window's base surface (i.e. none)..
                if (SurfaceTmp(SurfNum).BaseSurf > 0) {
                    SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area -= SurfaceTmp(SurfNum).Area;

                    // Subtract TDD:DIFFUSER area from other side interzone surface
                    if ((SurfaceTmp(SurfNum).Class == SurfaceClass::TDD_Diffuser) &&
                        not_blank(SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).ExtBoundCondName)) { // Base surface is an interzone surface

                        // Lookup interzone surface of the base surface
                        // (Interzone surfaces have not been assigned yet, but all base surfaces should already be loaded.)
                        Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).ExtBoundCondName, SurfaceTmp, SurfNum);
                        if (Found != 0) SurfaceTmp(Found).Area -= SurfaceTmp(SurfNum).Area;
                    }

                    if (SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
                        ShowSevereError(cRoutineName +
                                        ": Surface Openings have too much area for base surface=" + SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Name);
                        ShowContinueError("Opening Surface creating error=" + SurfaceTmp(SurfNum).Name);
                        ErrorsFound = true;
                    }
                    // Net area of base surface with unity window multipliers (used in shadowing checks)
                    // For Windows, Glass Doors and Doors, just one area is subtracted.  For the rest, should be
                    // full area.
                    if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor) {
                        SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= SurfaceTmp(SurfNum).Area / SurfaceTmp(SurfNum).Multiplier;
                    } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Door) { // Door, TDD:Diffuser, TDD:DOME
                        SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= SurfaceTmp(SurfNum).Area / SurfaceTmp(SurfNum).Multiplier;
                    } else {
                        SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= SurfaceTmp(SurfNum).Area;
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

        SurfaceTmp(SurfNum).Height = Height;
        SurfaceTmp(SurfNum).Width = Length;

        SurfAzimuth = SurfaceTmp(SurfNum).Azimuth;
        SurfTilt = SurfaceTmp(SurfNum).Tilt;
        CosSurfAzimuth = std::cos(SurfAzimuth * DataGlobalConstants::DegToRadians());
        SinSurfAzimuth = std::sin(SurfAzimuth * DataGlobalConstants::DegToRadians());
        CosSurfTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
        SinSurfTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
        BaseCosSurfAzimuth = SurfaceTmp(BaseSurfNum).CosAzim;
        BaseSinSurfAzimuth = SurfaceTmp(BaseSurfNum).SinAzim;
        BaseCosSurfTilt = SurfaceTmp(BaseSurfNum).CosTilt;
        BaseSinSurfTilt = SurfaceTmp(BaseSurfNum).SinTilt;

        XLLC = SurfaceTmp(BaseSurfNum).Vertex(2).x - XCoord * BaseCosSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseSinSurfAzimuth;
        YLLC = SurfaceTmp(BaseSurfNum).Vertex(2).y + XCoord * BaseSinSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseCosSurfAzimuth;
        ZLLC = SurfaceTmp(BaseSurfNum).Vertex(2).z + ZCoord * BaseSinSurfTilt;

        XX(1) = 0.0;
        XX(2) = 0.0;
        XX(3) = Length;
        XX(4) = Length;
        YY(1) = Height;
        YY(4) = Height;
        YY(3) = 0.0;
        YY(2) = 0.0;

        for (n = 1; n <= SurfaceTmp(SurfNum).Sides; ++n) {
            Vrt = n;
            SurfaceTmp(SurfNum).Vertex(Vrt).x = XLLC - XX(n) * CosSurfAzimuth - YY(n) * CosSurfTilt * SinSurfAzimuth;
            SurfaceTmp(SurfNum).Vertex(Vrt).y = YLLC + XX(n) * SinSurfAzimuth - YY(n) * CosSurfTilt * CosSurfAzimuth;
            SurfaceTmp(SurfNum).Vertex(Vrt).z = ZLLC + YY(n) * SinSurfTilt;
        }

        CreateNewellAreaVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellAreaVector);
        SurfaceTmp(SurfNum).GrossArea = VecLength(SurfaceTmp(SurfNum).NewellAreaVector);
        SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).GrossArea;
        SurfaceTmp(SurfNum).NetAreaShadowCalc = SurfaceTmp(SurfNum).Area;
        CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(SurfaceTmp(SurfNum).Vertex,
                                SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                SurfaceTmp(SurfNum).lcsx,
                                SurfaceTmp(SurfNum).lcsy,
                                SurfaceTmp(SurfNum).lcsz,
                                SurfaceTmp(SurfNum).GrossArea,
                                SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        SurfaceTmp(SurfNum).Azimuth = SurfAzimuth;
        SurfaceTmp(SurfNum).Tilt = SurfTilt;
        // Sine and cosine of azimuth and tilt
        SurfaceTmp(SurfNum).SinAzim = SinSurfAzimuth;
        SurfaceTmp(SurfNum).CosAzim = CosSurfAzimuth;
        SurfaceTmp(SurfNum).SinTilt = SinSurfTilt;
        SurfaceTmp(SurfNum).CosTilt = CosSurfTilt;
        if (SurfaceTmp(SurfNum).Class != SurfaceClass::Window && SurfaceTmp(SurfNum).Class != SurfaceClass::GlassDoor &&
            SurfaceTmp(SurfNum).Class != SurfaceClass::Door)
            SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);
        // Outward normal unit vector (pointing away from room)
        SurfaceTmp(SurfNum).OutNormVec = SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
        for (n = 1; n <= 3; ++n) {
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
            if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
        }

        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Roof .and. SurfTilt > 80.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        //  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass::Floor .and. SurfTilt < 170.) THEN
        //    WRITE(TiltString,'(F5.1)') SurfTilt
        //    TiltString=ADJUSTL(TiltString)
        //    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
        //                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
        //                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
        //  ENDIF
        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
            SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
            SurfaceTmp(SurfNum).Area *= SurfaceTmp(SurfNum).Multiplier;
        // Can perform tests on this surface here
        SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + SurfaceTmp(SurfNum).CosTilt);
        // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
        // surfaces
        SurfaceTmp(SurfNum).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSky;
        SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);

        Perimeter = distance(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides), SurfaceTmp(SurfNum).Vertex(1));
        for (Vrt = 2; Vrt <= SurfaceTmp(SurfNum).Sides; ++Vrt) {
            Perimeter += distance(SurfaceTmp(SurfNum).Vertex(Vrt), SurfaceTmp(SurfNum).Vertex(Vrt - 1));
        }
        SurfaceTmp(SurfNum).Perimeter = Perimeter;

        // Call to transform vertices

        TransformVertsByAspect(state, SurfNum, SurfaceTmp(SurfNum).Sides);
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
        using General::TrimSigDigits;
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
            ShowWarningError("Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
        }

        cCurrentModuleObject = "Shading:Zone:Detailed";
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Loop, NumAlphas, NumNumbers);
        if (NumAlphas != 3) {
            ShowSevereError(cCurrentModuleObject +
                            ": Object Definition indicates not = 3 Alpha Objects, Number Indicated=" + TrimSigDigits(NumAlphas));
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

            if (GlobalNames::VerifyUniqueInterObjectName(UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            ++SurfNum;
            SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
            SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
            SurfaceTmp(SurfNum).HeatTransSurf = false;
            SurfaceTmp(SurfNum).BaseSurfName = cAlphaArgs(2);
            //  The subsurface inherits properties from the base surface
            //  Exterior conditions, Zone, etc.
            //  We can figure out the base surface though, because they've all been entered
            Found = UtilityRoutines::FindItemInList(SurfaceTmp(SurfNum).BaseSurfName, SurfaceTmp, TotSurfaces);
            if (Found > 0) {
                // SurfaceTmp(SurfNum)%BaseSurf=Found
                SurfaceTmp(SurfNum).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                SurfaceTmp(SurfNum).ExtSolar = SurfaceTmp(Found).ExtSolar;
                SurfaceTmp(SurfNum).ExtWind = SurfaceTmp(Found).ExtWind;
                SurfaceTmp(SurfNum).Zone = SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                SurfaceTmp(SurfNum).ZoneName = SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports
            } else {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ErrorsFound = true;
            }
            if (SurfaceTmp(SurfNum).ExtBoundCond == UnenteredAdjacentZoneSurface) {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ShowContinueError("...trying to attach a shading device to an interzone surface.");
                ErrorsFound = true;
                SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }
            if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) {
                ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                cAlphaArgs(2));
                ShowContinueError("...trying to attach a shading device to an interior surface.");
                ErrorsFound = true;
                SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
            }

            if (!lAlphaFieldBlanks(3)) {
                SurfaceTmp(SurfNum).SchedShadowSurfIndex = GetScheduleIndex(state, cAlphaArgs(3));
                if (SurfaceTmp(SurfNum).SchedShadowSurfIndex == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(3) + " not found=\"" +
                                    cAlphaArgs(3));
                    ErrorsFound = true;
                }
            } else {
                SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
            }
            if (SurfaceTmp(SurfNum).SchedShadowSurfIndex != 0) {
                if (!CheckScheduleValueMinMax(SurfaceTmp(SurfNum).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                    "\", values not in range [0,1].");
                    ErrorsFound = true;
                }
                SchedMinValue = GetScheduleMinValue(SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                SurfaceTmp(SurfNum).SchedMinValue = SchedMinValue;
                SchedMaxValue = GetScheduleMaxValue(SurfaceTmp(SurfNum).SchedShadowSurfIndex);
                if (SchedMinValue == 1.0) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                     "\", is always transparent.");
                    SurfaceTmp(SurfNum).IsTransparent = true;
                }
                if (SchedMinValue < 0.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                    "\", has schedule values < 0.");
                    ShowContinueError("...Schedule values < 0 have no meaning for shading elements.");
                }
                if (SchedMaxValue > 1.0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                    "\", has schedule values > 1.");
                    ShowContinueError("...Schedule values > 1 have no meaning for shading elements.");
                }
                if (std::abs(SchedMinValue - SchedMaxValue) > 1.0e-6) {
                    SurfaceTmp(SurfNum).ShadowSurfSchedVaries = true;
                    ShadingTransmittanceVaries = true;
                }
            }
            if (lNumericFieldBlanks(1) || rNumericArgs(1) == DataGlobalConstants::AutoCalculate()) {
                rNumericArgs(1) = (NumNumbers - 1) / 3;
                SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
                if (mod(NumNumbers - 1, 3) != 0) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(1) +
                                     " not even multiple of 3. Will read in " + TrimSigDigits(SurfaceTmp(SurfNum).Sides));
                }
                if (rNumericArgs(1) < 3) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cNumericFieldNames(1) +
                                    " (autocalculate) must be >= 3. Only " + TrimSigDigits(SurfaceTmp(SurfNum).Sides) + " provided.");
                    ErrorsFound = true;
                    continue;
                }
            } else {
                SurfaceTmp(SurfNum).Sides = rNumericArgs(1);
            }
            SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);
            GetVertices(state, SurfNum, SurfaceTmp(SurfNum).Sides, rNumericArgs({2, _}));
            CheckConvexity(SurfNum, SurfaceTmp(SurfNum).Sides);
            //    IF (SurfaceTmp(SurfNum)%Sides == 3) THEN
            //      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            //                        ' should not be triangular.')
            //      CALL ShowContinueError('...Check results carefully.')
            //      ErrorsFound=.TRUE.
            //    ENDIF
            // Reset surface to be "detached"
            SurfaceTmp(SurfNum).BaseSurf = 0;
            //    SurfaceTmp(SurfNum)%BaseSurfName='  '
            SurfaceTmp(SurfNum).Zone = 0;
            // SurfaceTmp(SurfNum)%ZoneName='  '
            if (MakeMirroredAttachedShading) {
                MakeMirrorSurface(SurfNum);
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;
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
            ShowWarningError("Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing");
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

                if (GlobalNames::VerifyUniqueInterObjectName(
                        UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                    continue;
                }

                ++SurfNum;
                SurfaceTmp(SurfNum).Name = cAlphaArgs(1); // Set the Surface Name in the Derived Type
                SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
                SurfaceTmp(SurfNum).HeatTransSurf = false;
                // this object references a window or door....
                Found = UtilityRoutines::FindItemInList(cAlphaArgs(2), SurfaceTmp, TotSurfaces);
                if (Found > 0) {
                    BaseSurfNum = SurfaceTmp(Found).BaseSurf;
                    SurfaceTmp(SurfNum).BaseSurfName = SurfaceTmp(Found).BaseSurfName;
                    SurfaceTmp(SurfNum).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                    SurfaceTmp(SurfNum).ExtSolar = SurfaceTmp(Found).ExtSolar;
                    SurfaceTmp(SurfNum).ExtWind = SurfaceTmp(Found).ExtWind;
                    SurfaceTmp(SurfNum).Zone = SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                    SurfaceTmp(SurfNum).ZoneName = SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ErrorsFound = true;
                    continue;
                }
                if (SurfaceTmp(SurfNum).ExtBoundCond == UnenteredAdjacentZoneSurface) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ShowContinueError("...trying to attach a shading device to an interzone surface.");
                    ErrorsFound = true;
                    SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }
                if (SurfaceTmp(SurfNum).ExtBoundCond == UnreconciledZoneSurface) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2));
                    ShowContinueError("...trying to attach a shading device to an interior surface.");
                    ErrorsFound = true;
                    SurfaceTmp(SurfNum).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
                }

                SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;

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
                    Length = rNumericArgs(3) + rNumericArgs(4) + SurfaceTmp(Found).Width;
                    if (Item == 1) {
                        Depth = rNumericArgs(5);
                    } else if (Item == 2) {
                        Depth = rNumericArgs(5) * SurfaceTmp(Found).Height;
                    }

                    if (Length * Depth <= 0.0) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", illegal surface area=[" +
                                        RoundSigDigits(Length * Depth, 2) + "]. Surface will NOT be entered.");
                        continue;
                    }

                    TiltAngle = SurfaceTmp(Found).Tilt + rNumericArgs(2);
                    SurfaceTmp(SurfNum).Tilt = TiltAngle;
                    SurfaceTmp(SurfNum).Azimuth = SurfaceTmp(Found).Azimuth;

                    // Make it relative to surface origin.....
                    Xp = SurfaceTmp(Found).Vertex(2).x - SurfaceTmp(BaseSurfNum).Vertex(2).x;
                    Yp = SurfaceTmp(Found).Vertex(2).y - SurfaceTmp(BaseSurfNum).Vertex(2).y;
                    Zp = SurfaceTmp(Found).Vertex(2).z - SurfaceTmp(BaseSurfNum).Vertex(2).z;

                    XLLC = -Xp * SurfaceTmp(BaseSurfNum).CosAzim + Yp * SurfaceTmp(BaseSurfNum).SinAzim;

                    YLLC = -Xp * SurfaceTmp(BaseSurfNum).SinAzim * SurfaceTmp(BaseSurfNum).CosTilt -
                           Yp * SurfaceTmp(BaseSurfNum).CosAzim * SurfaceTmp(BaseSurfNum).CosTilt + Zp * SurfaceTmp(BaseSurfNum).SinTilt;

                    SurfaceTmp(SurfNum).Sides = 4;
                    SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);

                    MakeRelativeRectangularVertices(
                        state, BaseSurfNum, SurfNum, XLLC - rNumericArgs(3), YLLC + SurfaceTmp(Found).Height + rNumericArgs(1), Length, Depth);

                    // Reset surface to be "detached"
                    //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                    //    SurfaceTmp(SurfNum)%ZoneName='  '

                    SurfaceTmp(SurfNum).BaseSurf = 0;
                    SurfaceTmp(SurfNum).Zone = 0;

                    // and mirror
                    if (MakeMirroredAttachedShading) {
                        MakeMirrorSurface(SurfNum);
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
                    SurfaceTmp(SurfNum).Name = SurfaceTmp(SurfNum).Name + " Left";
                    Length = rNumericArgs(2) + rNumericArgs(3) + SurfaceTmp(Found).Height;
                    if (Item == 3) {
                        Depth = rNumericArgs(5);
                    } else if (Item == 4) {
                        Depth = rNumericArgs(5) * SurfaceTmp(Found).Width;
                    }

                    MakeFin = true;
                    if (Length * Depth <= 0.0) {
                        ShowWarningError(cCurrentModuleObject + "=Left Fin of \"" + cAlphaArgs(1) + "\", illegal surface area=[" +
                                         RoundSigDigits(Length * Depth, 2) + "]. Surface will NOT be entered.");
                        MakeFin = false;
                    }

                    if (MakeFin) {
                        TiltAngle = SurfaceTmp(Found).Tilt;
                        SurfaceTmp(SurfNum).Tilt = TiltAngle;
                        SurfaceTmp(SurfNum).Azimuth = SurfaceTmp(Found).Azimuth - (180.0 - rNumericArgs(4));

                        // Make it relative to surface origin.....

                        Xp = SurfaceTmp(Found).Vertex(2).x - SurfaceTmp(BaseSurfNum).Vertex(2).x;
                        Yp = SurfaceTmp(Found).Vertex(2).y - SurfaceTmp(BaseSurfNum).Vertex(2).y;
                        Zp = SurfaceTmp(Found).Vertex(2).z - SurfaceTmp(BaseSurfNum).Vertex(2).z;

                        XLLC = -Xp * SurfaceTmp(BaseSurfNum).CosAzim + Yp * SurfaceTmp(BaseSurfNum).SinAzim;

                        YLLC = -Xp * SurfaceTmp(BaseSurfNum).SinAzim * SurfaceTmp(BaseSurfNum).CosTilt -
                               Yp * SurfaceTmp(BaseSurfNum).CosAzim * SurfaceTmp(BaseSurfNum).CosTilt + Zp * SurfaceTmp(BaseSurfNum).SinTilt;

                        SurfaceTmp(SurfNum).CosAzim = std::cos(SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).SinAzim = std::sin(SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).CosTilt = std::cos(SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).SinTilt = std::sin(SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians());

                        SurfaceTmp(SurfNum).Sides = 4;
                        SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);

                        MakeRelativeRectangularVertices(state, BaseSurfNum, SurfNum, XLLC - rNumericArgs(1), YLLC - rNumericArgs(3), -Depth, Length);

                        // Reset surface to be "detached"
                        //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                        //    SurfaceTmp(SurfNum)%ZoneName='  '

                        SurfaceTmp(SurfNum).BaseSurf = 0;
                        SurfaceTmp(SurfNum).Zone = 0;

                        // and mirror
                        if (MakeMirroredAttachedShading) {
                            MakeMirrorSurface(SurfNum);
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
                    SurfaceTmp(SurfNum).Name = cAlphaArgs(1) + " Right"; // Set the Surface Name in the Derived Type
                    SurfaceTmp(SurfNum).Class = SurfaceClass::Shading;
                    SurfaceTmp(SurfNum).HeatTransSurf = false;
                    BaseSurfNum = SurfaceTmp(Found).BaseSurf;
                    SurfaceTmp(SurfNum).BaseSurfName = SurfaceTmp(Found).BaseSurfName;
                    SurfaceTmp(SurfNum).ExtBoundCond = SurfaceTmp(Found).ExtBoundCond;
                    SurfaceTmp(SurfNum).ExtSolar = SurfaceTmp(Found).ExtSolar;
                    SurfaceTmp(SurfNum).ExtWind = SurfaceTmp(Found).ExtWind;
                    SurfaceTmp(SurfNum).Zone = SurfaceTmp(Found).Zone;         // Necessary to do relative coordinates in GetVertices below
                    SurfaceTmp(SurfNum).ZoneName = SurfaceTmp(Found).ZoneName; // Necessary to have surface drawn in OutputReports

                    SurfaceTmp(SurfNum).SchedShadowSurfIndex = 0;
                    Length = rNumericArgs(7) + rNumericArgs(8) + SurfaceTmp(Found).Height;
                    if (Item == 3) {
                        Depth = rNumericArgs(10);
                    } else if (Item == 4) {
                        Depth = rNumericArgs(10) * SurfaceTmp(Found).Width;
                    }

                    MakeFin = true;
                    if (Length * Depth <= 0.0) {
                        ShowWarningError(cCurrentModuleObject + "=Right Fin of \"" + cAlphaArgs(1) + "\", illegal surface area=[" +
                                         RoundSigDigits(Length * Depth, 2) + "]. Surface will NOT be entered.");
                        MakeFin = false;
                    }

                    if (MakeFin) {
                        // Make it relative to surface origin.....

                        Xp = SurfaceTmp(Found).Vertex(2).x - SurfaceTmp(BaseSurfNum).Vertex(2).x;
                        Yp = SurfaceTmp(Found).Vertex(2).y - SurfaceTmp(BaseSurfNum).Vertex(2).y;
                        Zp = SurfaceTmp(Found).Vertex(2).z - SurfaceTmp(BaseSurfNum).Vertex(2).z;

                        XLLC = -Xp * SurfaceTmp(BaseSurfNum).CosAzim + Yp * SurfaceTmp(BaseSurfNum).SinAzim;

                        YLLC = -Xp * SurfaceTmp(BaseSurfNum).SinAzim * SurfaceTmp(BaseSurfNum).CosTilt -
                               Yp * SurfaceTmp(BaseSurfNum).CosAzim * SurfaceTmp(BaseSurfNum).CosTilt + Zp * SurfaceTmp(BaseSurfNum).SinTilt;

                        TiltAngle = SurfaceTmp(Found).Tilt;
                        SurfaceTmp(SurfNum).Tilt = TiltAngle;
                        SurfaceTmp(SurfNum).Azimuth = SurfaceTmp(Found).Azimuth - (180.0 - rNumericArgs(9));
                        SurfaceTmp(SurfNum).CosAzim = std::cos(SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).SinAzim = std::sin(SurfaceTmp(SurfNum).Azimuth * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).CosTilt = std::cos(SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians());
                        SurfaceTmp(SurfNum).SinTilt = std::sin(SurfaceTmp(SurfNum).Tilt * DataGlobalConstants::DegToRadians());

                        SurfaceTmp(SurfNum).Sides = 4;
                        SurfaceTmp(SurfNum).Vertex.allocate(SurfaceTmp(SurfNum).Sides);

                        MakeRelativeRectangularVertices(
                            state, BaseSurfNum, SurfNum, XLLC + SurfaceTmp(Found).Width + rNumericArgs(6), YLLC - rNumericArgs(8), -Depth, Length);

                        // Reset surface to be "detached"
                        //    SurfaceTmp(SurfNum)%BaseSurfName='  '
                        //    SurfaceTmp(SurfNum)%ZoneName='  '

                        SurfaceTmp(SurfNum).BaseSurf = 0;
                        SurfaceTmp(SurfNum).Zone = 0;

                        // and mirror
                        if (MakeMirroredAttachedShading) {
                            MakeMirrorSurface(SurfNum);
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
        int TotIntMass = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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

            if (GlobalNames::VerifyUniqueInterObjectName(UniqueSurfaceNames, cAlphaArgs(1), cCurrentModuleObject, cAlphaFieldNames(1), ErrorsFound)) {
                continue;
            }

            IntMassObjects(Item).Name = cAlphaArgs(1);
            IntMassObjects(Item).GrossArea = rNumericArgs(1);
            IntMassObjects(Item).Construction = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
            IntMassObjects(Item).ZoneOrZoneListName = cAlphaArgs(3);
            int Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(3), Zone, NumOfZones);
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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                "\" not found.");
                ++SurfNum;
                SurfaceTmpClassInvalid(SurfNum) = true;
                SurfaceTmp(SurfNum).ZoneName = "Unknown Zone";
                ErrorsFound = true;
                errFlag = true;
            }

            if (errFlag) {
                ShowSevereError(RoutineName + "Errors with invalid names in " + cCurrentModuleObject + " objects.");
                ShowContinueError("...These will not be read in.  Other errors may occur.");
                NumIntMassSurfaces = 0;
            }

            if (IntMassObjects(Item).Construction == 0) {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(2) + " not found=" + cAlphaArgs(2));
            } else if (state.dataConstruction->Construct(IntMassObjects(Item).Construction).TypeIsWindow) {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" - has Window materials.");
            } else {
                state.dataConstruction->Construct(IntMassObjects(Item).Construction).IsUsed = true;
            }
        }

        if (NumIntMassSurfaces > 0) {
            for (int Loop = 1; Loop <= TotIntMass; ++Loop) {

                for (int Item1 = 1; Item1 <= IntMassObjects(Loop).NumOfZones; ++Item1) {

                    ++SurfNum;

                    SurfaceTmp(SurfNum).Construction = IntMassObjects(Loop).Construction;
                    if (!IntMassObjects(Loop).ZoneListActive) {
                        SurfaceTmp(SurfNum).Zone = IntMassObjects(Loop).ZoneOrZoneListPtr;
                        SurfaceTmp(SurfNum).Name = IntMassObjects(Loop).Name;
                        SurfaceTmp(SurfNum).Class = SurfaceClass::IntMass;
                        SurfaceTmp(SurfNum).ZoneName = IntMassObjects(Loop).ZoneOrZoneListName;
                        SurfaceTmp(SurfNum).HeatTransSurf = true;
                    } else {
                        CheckCreatedZoneItemName(RoutineName,
                                                 cCurrentModuleObject,
                                                 Zone(ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).Zone(Item1)).Name,
                                                 ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).MaxZoneNameLength,
                                                 IntMassObjects(Loop).Name,
                                                 SurfaceTmp,
                                                 SurfNum - 1,
                                                 SurfaceTmp(SurfNum).Name,
                                                 errFlag);

                        ZoneNum = ZoneList(IntMassObjects(Loop).ZoneOrZoneListPtr).Zone(Item1);
                        SurfaceTmp(SurfNum).Zone = ZoneNum;
                        SurfaceTmp(SurfNum).Class = SurfaceClass::IntMass;
                        SurfaceTmp(SurfNum).ZoneName = Zone(ZoneNum).Name;
                        SurfaceTmp(SurfNum).HeatTransSurf = true;
                        if (errFlag) ErrorsFound = true;
                    }

                    if (IntMassObjects(Loop).Construction > 0) {
                        if (state.dataConstruction->Construct(IntMassObjects(Loop).Construction).IsUsed) {
                            SurfaceTmp(SurfNum).ConstructionStoredInputValue = IntMassObjects(Loop).Construction;
                        }
                    }
                    SurfaceTmp(SurfNum).GrossArea = IntMassObjects(Loop).GrossArea;
                    SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).GrossArea;
                    SurfaceTmp(SurfNum).NetAreaShadowCalc = SurfaceTmp(SurfNum).Area;
                    SurfaceTmp(SurfNum).Width = SurfaceTmp(SurfNum).Area;
                    SurfaceTmp(SurfNum).Height = 1.0;
                    SurfaceTmp(SurfNum).Tilt = 90.0;
                    SurfaceTmp(SurfNum).CosTilt = 0.0; // Tuned Was std::cos( 90.0 * DegToRadians )
                    SurfaceTmp(SurfNum).SinTilt = 1.0; // Tuned Was std::sin( 90.0 * DegToRadians )
                    SurfaceTmp(SurfNum).Azimuth = 0.0;
                    SurfaceTmp(SurfNum).CosAzim = 1.0; // Tuned Was std::cos( 0.0 )
                    SurfaceTmp(SurfNum).SinAzim = 0.0; // Tuned Was std::sin( 0.0 )
                    // Outward normal unit vector (pointing away from room)
                    SurfaceTmp(SurfNum).OutNormVec = SurfaceTmp(SurfNum).lcsz;
                    SurfaceTmp(SurfNum).ViewFactorSky = 0.5;
                    SurfaceTmp(SurfNum).ExtSolar = false;
                    SurfaceTmp(SurfNum).ExtWind = false;
                    SurfaceTmp(SurfNum).BaseSurf = SurfNum;
                    SurfaceTmp(SurfNum).BaseSurfName = SurfaceTmp(SurfNum).Name;
                    SurfaceTmp(SurfNum).ExtBoundCondName = SurfaceTmp(SurfNum).Name;
                    SurfaceTmp(SurfNum).ExtBoundCond = UnreconciledZoneSurface;
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
        int TotIntMass = inputProcessor->getNumObjectsFound("InternalMass");

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

            int Item1 = UtilityRoutines::FindItemInList(cAlphaArgs(3), Zone, NumOfZones);
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
        using General::RoundSigDigits;

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
            if (!(SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                  SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                  SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                continue;
            SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = 0.2;
            SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = 0.2;
            SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = 0.0;
            SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = 0;
        }

        // Get the total number of Shading Surface Reflectance objects
        cCurrentModuleObject = "ShadingProperty:Reflectance";
        TotShadingSurfaceReflectance = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
            SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(1), SurfaceTmp, TotSurfaces);
            if (SurfNum == 0) {
                ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid specification");
                ShowContinueError(".. not found " + cAlphaFieldNames(1) + "=\"" + cAlphaArgs(1) + "\".");
                //      ErrorsFound =.TRUE.
                continue;
            }

            // Check that associated surface is a shading surface
            WrongSurfaceType = false;
            if (SurfNum != 0) {
                if (!(SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                      SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                      SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                    WrongSurfaceType = true;
                if (WrongSurfaceType) {
                    ShowSevereError("GetShadingSurfReflectanceData: " + cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name +
                                    "\", surface is not a shading surface.");
                    ErrorsFound = true;
                    continue;
                }
            }

            // If associated surface is a shading surface, set reflectance values
            SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = rNumericArgs(3);
            SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(1);
            SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(2);
            if (rNumericArgs(3) > 0.0) {
                GlConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
                if (GlConstrNum == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + SurfaceTmp(SurfNum).Name + "\", " + cAlphaFieldNames(2) +
                                    " not found=" + cAlphaArgs(2));
                    ErrorsFound = true;
                } else {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = GlConstrNum;
            }
            SurfNum = UtilityRoutines::FindItemInList("Mir-" + cAlphaArgs(1), SurfaceTmp, TotSurfaces);
            if (SurfNum == 0) continue;
            SurfaceTmp(SurfNum).ShadowSurfGlazingFrac = rNumericArgs(3);
            SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(1);
            SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl = (1.0 - rNumericArgs(3)) * rNumericArgs(2);
            if (rNumericArgs(3) > 0.0) {
                GlConstrNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), state.dataConstruction->Construct, TotConstructs);
                if (GlConstrNum != 0) {
                    state.dataConstruction->Construct(GlConstrNum).IsUsed = true;
                }
                SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct = GlConstrNum;
            }

        } // End of loop over Shading Surface Reflectance objects

        // Write reflectance values to .eio file.
        print(state.files.eio, "! <ShadingProperty Reflectance>,Shading Surface Name,Shading Type,Diffuse Solar Reflectance, Diffuse "
               "Visible Reflectance,Surface Glazing Fraction,Surface Glazing Contruction\n");

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!(SurfaceTmp(SurfNum).Class == SurfaceClass::Shading || SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F ||
                  SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B || SurfaceTmp(SurfNum).Class == SurfaceClass::Overhang ||
                  SurfaceTmp(SurfNum).Class == SurfaceClass::Fin))
                continue;

            constexpr auto fmt{"ShadingProperty Reflectance,{},{},{:.2R},{:.2R},{:.2R}, {}\n"};
            if (SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct != 0) {
                print(state.files.eio,
                      fmt,
                      SurfaceTmp(SurfNum).Name,
                      cSurfaceClass(SurfaceTmp(SurfNum).Class),
                      SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl,
                      SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl,
                      SurfaceTmp(SurfNum).ShadowSurfGlazingFrac,
                      state.dataConstruction->Construct(SurfaceTmp(SurfNum).ShadowSurfGlazingConstruct).Name);
            } else {
                print(state.files.eio,
                      fmt,
                      SurfaceTmp(SurfNum).Name,
                      cSurfaceClass(SurfaceTmp(SurfNum).Class),
                      SurfaceTmp(SurfNum).ShadowSurfDiffuseSolRefl,
                      SurfaceTmp(SurfNum).ShadowSurfDiffuseVisRefl,
                      SurfaceTmp(SurfNum).ShadowSurfGlazingFrac,
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
        using General::TrimSigDigits;

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
        inputProcessor->getObjectDefMaxArgs(cCurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers);

        if (MaxNumNumbers != 8) {
            ShowSevereError(cCurrentModuleObject +
                            ": Object Definition indicates not = 8 Number Objects, Number Indicated=" + TrimSigDigits(MaxNumNumbers));
            ErrorsFound = true;
        }

        TotExtVentCav = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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

            UtilityRoutines::VerifyName(cAlphaArgs(1), ExtVentedCavity, Item - 1, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ShowContinueError("...cannot not duplicate other names");
                ErrorsFound = true;
                continue;
            }
            ExtVentedCavity(Item).Name = cAlphaArgs(1);

            ExtVentedCavity(Item).OSCMName = cAlphaArgs(2);
            if (!lAlphaFieldBlanks(2)) {
                Found = UtilityRoutines::FindItemInList(ExtVentedCavity(Item).OSCMName, OSCM, TotOSCM);
                if (Found == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
            } else {
                Found = 0;
                ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(2) +
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
                ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3));
                ErrorsFound = true;
            }

            AlphaOffset = 3;
            ExtVentedCavity(Item).NumSurfs = NumAlphas - AlphaOffset;
            if (ExtVentedCavity(Item).NumSurfs == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name +
                                "\", no underlying surfaces specified. Must have at least one.");
                ErrorsFound = true;
                continue;
            }
            ExtVentedCavity(Item).SurfPtrs.allocate(ExtVentedCavity(Item).NumSurfs);
            ExtVentedCavity(Item).SurfPtrs = 0;
            for (ThisSurf = 1; ThisSurf <= ExtVentedCavity(Item).NumSurfs; ++ThisSurf) {
                Found = UtilityRoutines::FindItemInList(cAlphaArgs(ThisSurf + AlphaOffset), Surface, TotSurfaces);
                if (Found == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ErrorsFound = true;
                    continue;
                }
                // check that surface is appropriate, Heat transfer, Sun, Wind,
                if (!Surface(Found).HeatTransSurf) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError("...because it is not a Heat Transfer Surface.");
                    ErrorsFound = true;
                    continue;
                }
                if (!Surface(Found).ExtSolar) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError("...because it is not exposed to Sun.");
                    ErrorsFound = true;
                    continue;
                }
                if (!Surface(Found).ExtWind) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid " +
                                    cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset));
                    ShowContinueError("...because it is not exposed to Wind.");
                    ErrorsFound = true;
                    continue;
                }
                if (Surface(Found).ExtBoundCond != OtherSideCondModeledExt) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", is invalid");
                    ShowContinueError("...because " + cAlphaFieldNames(ThisSurf + AlphaOffset) + "=\"" + cAlphaArgs(ThisSurf + AlphaOffset) + "\".");
                    ShowContinueError("...is not an OtherSideConditionedModel surface.");
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
                    ShowWarningError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + ", Surface " + Surface(SurfID).Name +
                                     " has Azimuth different from others in the associated group.");
                }
                if (std::abs(Surface(SurfID).Tilt - AvgTilt) > 10.0) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + ", Surface " + Surface(SurfID).Name +
                                     " has Tilt different from others in the associated group.");
                }

                // test that there are no windows.  Now allow windows
                // If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
                //      Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
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
                ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid .");
                ErrorsFound = true;
                ShowContinueError("...because field \"" + cNumericFieldNames(5) + "\" must be greater than Zero=[" +
                                  TrimSigDigits(rNumericArgs(5), 2) + "].");
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
                ShowSevereError(cCurrentModuleObject + "=\"" + ExtVentedCavity(Item).Name + "\", invalid .");
                ErrorsFound = true;
                ShowContinueError("...because gross area of underlying surfaces must be greater than Zero=[" +
                                  TrimSigDigits(ExtVentedCavity(Item).ProjArea, 2) + "].");
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;

        int IOStatus; // Used in GetObjectItem
        int NumAlphas;
        int NumNumbers;

        auto const tolerance = 1e-6;

        std::string cCurrentModuleObject = "SurfaceProperty:ExposedFoundationPerimeter";
        int numObjects = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface");
                ErrorsFound = true;
            }
            alpF++;
            if (Surface(Found).Class != SurfaceClass::Floor) {
                ShowWarningError(cCurrentModuleObject + ": " + Surface(Found).Name + ", surface is not a floor surface");
                ShowContinueError(cCurrentModuleObject + " will not be used");
                continue;
            }

            // Choose calculation method
            std::string calculationMethod = cAlphaArgs(alpF);
            if (calculationMethod != "TOTALEXPOSEDPERIMETER" && calculationMethod != "EXPOSEDPERIMETERFRACTION" && calculationMethod != "BYSEGMENT") {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", " + calculationMethod + " is not a valid choice for " +
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
                        ShowWarningError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + cNumericFieldNames(numF) +
                                         " is greater than the perimeter of " + Surface(Found).Name);
                        ShowContinueError(Surface(Found).Name + " perimeter = " + RoundSigDigits(Surface(Found).Perimeter) + ", " +
                                          cCurrentModuleObject + " exposed perimeter = " + RoundSigDigits(rNumericArgs(numF)));
                        ShowContinueError(cNumericFieldNames(numF) + " will be set equal to " + Surface(Found).Name + " perimeter");
                        data.exposedFraction = 1.0;
                    }

                    data.useDetailedExposedPerimeter = false;
                } else {
                    ShowWarningError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                     " set as calculation method, but a value has been set for " + cNumericFieldNames(numF) +
                                     ". This value will be ignored.");
                }
            } else {
                if (calculationMethod == "TOTALEXPOSEDPERIMETER") {
                    ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
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
                    ShowWarningError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                     " set as calculation method, but a value has been set for " + cNumericFieldNames(numF) +
                                     ". This value will be ignored.");
                }
            } else {
                if (calculationMethod == "EXPOSEDPERIMETERFRACTION") {
                    ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                    " set as calculation method, but no value has been set for " + cNumericFieldNames(numF));
                    ErrorsFound = true;
                }
            }
            numF++;

            int numRemainingFields = NumAlphas - (alpF - 1) + NumNumbers - (numF - 1);
            if (numRemainingFields > 0) {
                if (calculationMethod == "BYSEGMENT") {
                    if (numRemainingFields != (int)Surface(Found).Vertex.size()) {
                        ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name +
                                        ", must have equal number of segments as the floor has vertices." + cAlphaFieldNames(alpF) + "\" and \"" +
                                        cNumericFieldNames(numF - 1) + "\"");
                        ShowContinueError(Surface(Found).Name + " number of vertices = " + TrimSigDigits(Surface(Found).Vertex.size()) + ", " +
                                          cCurrentModuleObject + " number of segments = " + TrimSigDigits(numRemainingFields));
                        ErrorsFound = true;
                    }
                    for (int segNum = 0; segNum < numRemainingFields; segNum++) {
                        if (UtilityRoutines::SameString(cAlphaArgs(alpF), "YES")) {
                            data.isExposedPerimeter.push_back(true);
                        } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "NO")) {
                            data.isExposedPerimeter.push_back(false);
                        } else if (lAlphaFieldBlanks(alpF)) {
                            ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
                                            " set as calculation method, but no value has been set for " + cAlphaFieldNames(alpF) +
                                            ". Must be \"Yes\" or \"No\".");
                            ErrorsFound = true;
                        } else {
                            ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + cAlphaFieldNames(alpF) + " invalid [" +
                                            cAlphaArgs(alpF) + "]. Must be \"Yes\" or \"No\".");
                            ErrorsFound = true;
                        }
                        alpF++;
                    }
                }
            } else {
                if (calculationMethod == "BYSEGMENT") {
                    ShowSevereError(cCurrentModuleObject + ": " + Surface(Found).Name + ", " + calculationMethod +
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
        TotSurfLocalEnv = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (TotSurfLocalEnv > 0) {

            AnyLocalEnvironmentsInModel = true;

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
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

                SurfLocalEnvironment(Loop).Name = cAlphaArgs(1);

                // Assign surface number
                SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), Surface);
                if (SurfNum == 0) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                    cAlphaFieldNames(2) + " has been found.");
                    ShowContinueError(cAlphaFieldNames(2) + " entered value = \"" + cAlphaArgs(2) +
                                      "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file.");
                    ErrorsFound = true;
                } else {
                    SurfLocalEnvironment(Loop).SurfPtr = SurfNum;
                }

                // Assign External Shading Schedule number
                if (!lAlphaFieldBlanks(3)) {
                    ExtShadingSchedNum = GetScheduleIndex(state, cAlphaArgs(3));
                    if (ExtShadingSchedNum == 0) {
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(3) + " has been found.");
                        ShowContinueError(cAlphaFieldNames(3) + " entered value = \"" + cAlphaArgs(3) +
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
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(4) + " has been found.");
                        ShowContinueError(cAlphaFieldNames(4) + " entered value = \"" + cAlphaArgs(4) +
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
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + ", object. Illegal value for " +
                                        cAlphaFieldNames(5) + " has been found.");
                        ShowContinueError(cAlphaFieldNames(5) + " entered value = \"" + cAlphaArgs(5) +
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
        TotSrdSurfProperties = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
                UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                    ShowContinueError("At lease one set of surrounding surface properties should be defined.");
                    ErrorsFound = true;
                    continue;
                }
                if ((NumAlpha - 3) / 2 != (NumNumeric - 2)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not defined correctly.");
                    ShowContinueError("Check number of input fields for each surrounding surface.");
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
        using General::RoundSigDigits;

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
        int CountAddHeatSourceSurf = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
                ErrorsFound = true;
            } else if (Surface(Found).InsideHeatSourceTermSchedule || Surface(Found).OutsideHeatSourceTermSchedule) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                "\", multiple SurfaceProperty:HeatBalanceSourceTerm objects applied to the same surface.");
                ErrorsFound = true;
            }

            if (!lAlphaFieldBlanks(2)) {
                Surface(Found).InsideHeatSourceTermSchedule = EnergyPlus::ScheduleManager::GetScheduleIndex(state, cAlphaArgs(2));
                DataSurfaces::AnyHeatBalanceInsideSourceTerm = true;
                if (Surface(Found).InsideHeatSourceTermSchedule == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", cannot find the matching Schedule: " + cAlphaFieldNames(2) +
                                    "=\"" + cAlphaArgs(2));
                    ErrorsFound = true;
                }
            }

            if (!lAlphaFieldBlanks(3)) {
                Surface(Found).OutsideHeatSourceTermSchedule = EnergyPlus::ScheduleManager::GetScheduleIndex(state, cAlphaArgs(3));
                DataSurfaces::AnyHeatBalanceOutsideSourceTerm = true;
                if (Surface(Found).OutsideHeatSourceTermSchedule == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", cannot find the matching Schedule: " + cAlphaFieldNames(3) +
                                    "=\"" + cAlphaArgs(3));
                    ErrorsFound = true;
                } else if (Surface(Found).OSCPtr > 0) {
                    ShowSevereError(
                        cCurrentModuleObject +
                        "=\"SurfaceProperty:HeatBalanceSourceTerm\", cannot be specified for OtherSideCoefficient Surface=" + cAlphaArgs(1));
                    ErrorsFound = true;
                }
            }

            if (Surface(Found).OutsideHeatSourceTermSchedule == 0 && Surface(Found).InsideHeatSourceTermSchedule == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", no schedule defined for additional heat source.");
                ErrorsFound = true;
            }
        }

        // first initialize each heat transfer surface with the overall model type, array assignment
        for (auto &e : Surface)
            e.HeatTransferAlgorithm = OverallHeatTransferSolutionAlgo;

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
        CountHTAlgoObjectsSingleSurf = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
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
        CountHTAlgoObjectsMultiSurf = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundMultiSurf = true;
                }
            }

            if (!SurfacesOfType) {
                ShowWarningError("In " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", for Multiple Surface Assignment=\"" + cAlphaArgs(2) +
                                 "\", there were no surfaces of that type found for assignment.");
            }
            if (ErrorsFoundMultiSurf) ErrorsFound = true;

        } // multi surface heat transfer algo override

        cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
        CountHTAlgoObjectsSurfList = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundSurfList = true;
                }
            }

            for (Item1 = 3; Item1 <= NumAlphas; ++Item1) {

                Found = UtilityRoutines::FindItemInList(cAlphaArgs(Item1), Surface, TotSurfaces);

                if (Found == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", did not find matching surface.");
                    ShowContinueError("Name of surface not found = \"" + cAlphaArgs(Item1) + "\"");
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
        CountHTAlgoObjectsSurfList = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
                    ErrorsFoundByConstruct = true;
                }
            }

            Found = UtilityRoutines::FindItemInList(cAlphaArgs(3), state.dataConstruction->Construct, TotConstructs);
            if (Found == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
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
            if (!ErrorsFound) ErrorsFound = kivaManager.setupKivaInstances(state);
        }

        // test for missing materials for algorithms selected
        NumEMPDMat = inputProcessor->getNumObjectsFound("MaterialProperty:MoisturePenetrationDepth:Settings");
        NumPCMat = inputProcessor->getNumObjectsFound("MaterialProperty:PhaseChange") +
                   inputProcessor->getNumObjectsFound("MaterialProperty:PhaseChangeHysteresis");
        NumVTCMat = inputProcessor->getNumObjectsFound("MaterialProperty:VariableThermalConductivity");
        NumHAMTMat1 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:Settings");
        NumHAMTMat2 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm");
        NumHAMTMat3 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:Suction");
        NumHAMTMat4 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:Redistribution");
        NumHAMTMat5 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:Diffusion");
        NumHAMTMat6 = inputProcessor->getNumObjectsFound("MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity");
        SumHAMTMat = NumHAMTMat1 + NumHAMTMat2 + NumHAMTMat3 + NumHAMTMat4 + NumHAMTMat5 + NumHAMTMat6;
        msgneeded = false;

        if (NumEMPDMat > 0 && !DataHeatBalance::AnyEMPD) {
            ShowWarningError(
                "The input file includes " + RoundSigDigits(NumEMPDMat) +
                " MaterialProperty:MoisturePenetrationDepth:Settings objects but the moisture penetration depth algorithm is not used anywhere.");
            msgneeded = true;
        }
        if (NumPCMat > 0 && !DataHeatBalance::AnyCondFD) {
            ShowWarningError("The input file includes " + RoundSigDigits(NumPCMat) +
                             " MaterialProperty:PhaseChange objects but the conduction finite difference algorithm is not used anywhere.");
            msgneeded = true;
        }
        if (NumVTCMat > 0 && !DataHeatBalance::AnyCondFD) {
            ShowWarningError(
                "The input file includes " + RoundSigDigits(NumVTCMat) +
                " MaterialProperty:VariableThermalConductivity objects but the conduction finite difference algorithm is not used anywhere.");
            msgneeded = true;
        }
        if (SumHAMTMat > 0 && !DataHeatBalance::AnyHAMT) {
            ShowWarningError("The input file includes " + RoundSigDigits(SumHAMTMat) +
                             " MaterialProperty:HeatAndMoistureTransfer:* objects but the combined heat and moisture finite difference algorithm is "
                             "not used anywhere.");
            msgneeded = true;
        }
        if (msgneeded) {
            ShowContinueError("Previous materials will be ignored due to HeatBalanceAlgorithm choice.");
        }
        msgneeded = false;
        if (NumEMPDMat == 0 && DataHeatBalance::AnyEMPD) {
            ShowWarningError("The moisture penetration depth conduction transfer function algorithm is used but the input file includes no "
                             "MaterialProperty:MoisturePenetrationDepth:Settings objects.");
            msgneeded = true;
        }
        if (SumHAMTMat == 0 && DataHeatBalance::AnyHAMT) {
            ShowWarningError("The combined heat and moisture finite element algorithm is used but the input file includes no "
                             "MaterialProperty:HeatAndMoistureTransfer:* objects.");
            msgneeded = true;
        }
        if (msgneeded) {
            ShowContinueError("Certain materials objects are necessary to achieve proper results with the heat transfer algorithm(s) selected.");
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
                        ShowWarningError("An interior surface is defined as two surfaces with reverse constructions. The HeatTransferAlgorithm in "
                                         "both constructions should be same.");
                        ShowContinueError("The HeatTransferAlgorithm of Surface: " + Surface(Item).Name + ", is " +
                                          HeatTransferModelNames(Surface(Item).HeatTransferAlgorithm));
                        ShowContinueError("The HeatTransferAlgorithm of Surface: " + Surface(ExtSurfNum).Name + ", is " +
                                          HeatTransferModelNames(Surface(ExtSurfNum).HeatTransferAlgorithm));
                        if (Surface(Item).HeatTransferAlgorithm > Surface(ExtSurfNum).HeatTransferAlgorithm) {
                            ShowContinueError("The HeatTransferAlgorithm of Surface: " + Surface(ExtSurfNum).Name + ", is assigned to " +
                                              HeatTransferModelNames(Surface(Item).HeatTransferAlgorithm) + ". Simulation continues.");
                            Surface(ExtSurfNum).HeatTransferAlgorithm = Surface(Item).HeatTransferAlgorithm;
                        } else {
                            ShowContinueError("The HeatTransferAlgorithm of Surface: " + Surface(Item).Name + ", is assigned to " +
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
        using General::RoundSigDigits;
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
            SurfaceTmp(SurfNum).Vertex(n).x = Vertices(Ptr);
            ++Ptr;
            SurfaceTmp(SurfNum).Vertex(n).y = Vertices(Ptr);
            ++Ptr;
            SurfaceTmp(SurfNum).Vertex(n).z = Vertices(Ptr);
            ++Ptr;
        }

        // Address changing vertices if they were put in in CW order rather than CCW
        if (!CCW) {
            // If even number of sides, this will transfer appropriately
            // If odd number, will leave the "odd" one, which is what you want.
            NSrc = NSides;
            NTar = 2;
            for (n = 1; n <= (NSides - 1) / 2; ++n) {
                temp = SurfaceTmp(SurfNum).Vertex(NSrc);
                SurfaceTmp(SurfNum).Vertex(NSrc) = SurfaceTmp(SurfNum).Vertex(NTar);
                SurfaceTmp(SurfNum).Vertex(NTar) = temp;
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
                temp = SurfaceTmp(SurfNum).Vertex(NTar);
                SurfaceTmp(SurfNum).Vertex(NTar) = SurfaceTmp(SurfNum).Vertex(NSrc);
                SurfaceTmp(SurfNum).Vertex(NSrc) = temp;
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
            ZoneNum = SurfaceTmp(SurfNum).Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = SurfaceTmp(SurfNum).Vertex(n).x * CosZoneRelNorth(ZoneNum) - SurfaceTmp(SurfNum).Vertex(n).y * SinZoneRelNorth(ZoneNum) +
                         Zone(ZoneNum).OriginX;
                    Yb = SurfaceTmp(SurfNum).Vertex(n).x * SinZoneRelNorth(ZoneNum) + SurfaceTmp(SurfNum).Vertex(n).y * CosZoneRelNorth(ZoneNum) +
                         Zone(ZoneNum).OriginY;
                    SurfaceTmp(SurfNum).Vertex(n).x = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                    SurfaceTmp(SurfNum).Vertex(n).y = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
                    SurfaceTmp(SurfNum).Vertex(n).z += Zone(ZoneNum).OriginZ;
                }
            } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = SurfaceTmp(SurfNum).Vertex(n).y;
                    SurfaceTmp(SurfNum).Vertex(n).x = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                    SurfaceTmp(SurfNum).Vertex(n).y = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
                }
            }
        } else {
            // if world coordinate only need to rotate for Appendix G
            ZoneNum = SurfaceTmp(SurfNum).Zone;
            if (ZoneNum > 0) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = SurfaceTmp(SurfNum).Vertex(n).y;
                    SurfaceTmp(SurfNum).Vertex(n).x = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
                    SurfaceTmp(SurfNum).Vertex(n).y = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
                }
            } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_B) {
                for (n = 1; n <= NSides; ++n) {
                    Xb = SurfaceTmp(SurfNum).Vertex(n).x;
                    Yb = SurfaceTmp(SurfNum).Vertex(n).y;
                    SurfaceTmp(SurfNum).Vertex(n).x = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
                    SurfaceTmp(SurfNum).Vertex(n).y = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
                }
            }
        }

        if (NSides > 2) {
            DistanceCheck = distance(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides), SurfaceTmp(SurfNum).Vertex(1));
            if (DistanceCheck < 0.01) {
                if (DisplayExtraWarnings) {
                    ShowWarningError(RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" +
                                     SurfaceTmp(SurfNum).Name + ", in Zone=" + SurfaceTmp(SurfNum).ZoneName);
                    ShowContinueError("Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) + "]=(" +
                                      RoundSigDigits(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides).x, 2) + ',' +
                                      RoundSigDigits(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides).y, 2) + ',' +
                                      RoundSigDigits(SurfaceTmp(SurfNum).Vertex(SurfaceTmp(SurfNum).Sides).z, 2) + ')');
                    ShowContinueError("Vertex [" + RoundSigDigits(1) + "]=(" + RoundSigDigits(SurfaceTmp(SurfNum).Vertex(1).x, 2) + ',' +
                                      RoundSigDigits(SurfaceTmp(SurfNum).Vertex(1).y, 2) + ',' + RoundSigDigits(SurfaceTmp(SurfNum).Vertex(1).z, 2) +
                                      ')');
                }
                ++TotalCoincidentVertices;
                if (SurfaceTmp(SurfNum).Sides > 3) {
                    if (DisplayExtraWarnings) {
                        ShowContinueError("Dropping Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) + "].");
                    }
                    --SurfaceTmp(SurfNum).Sides;
                    SurfaceTmp(SurfNum).Vertex.redimension(SurfaceTmp(SurfNum).Sides);
                } else {
                    if (DisplayExtraWarnings) {
                        ShowContinueError("Cannot Drop Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) +
                                          "]; Number of Surface Sides at minimum. This surface is now a degenerate surface.");
                    }
                    ++TotalDegenerateSurfaces;
                    // mark degenerate surface?
                }
                DistanceCheck = 0.0;
            }
            Perimeter = DistanceCheck;
            //      DO Vrt=2,SurfaceTmp(SurfNum)%Sides
            Vrt = 2;
            while (true) {
                DistanceCheck = distance(SurfaceTmp(SurfNum).Vertex(Vrt), SurfaceTmp(SurfNum).Vertex(Vrt - 1));
                if (DistanceCheck < 0.01) {
                    if (DisplayExtraWarnings) {
                        ShowWarningError(RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" +
                                         SurfaceTmp(SurfNum).Name + ", in Zone=" + SurfaceTmp(SurfNum).ZoneName);
                        ShowContinueError("Vertex [" + RoundSigDigits(Vrt) + "]=(" + RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt).x, 2) + ',' +
                                          RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt).y, 2) + ',' +
                                          RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt).z, 2) + ')');
                        ShowContinueError("Vertex [" + RoundSigDigits(Vrt - 1) + "]=(" + RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt - 1).x, 2) +
                                          ',' + RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt - 1).y, 2) + ',' +
                                          RoundSigDigits(SurfaceTmp(SurfNum).Vertex(Vrt - 1).z, 2) + ')');
                    }
                    ++TotalCoincidentVertices;
                    if (Vrt == SurfaceTmp(SurfNum).Sides) {
                        if (SurfaceTmp(SurfNum).Sides > 3) {
                            if (DisplayExtraWarnings) {
                                ShowContinueError("Dropping Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) + "].");
                            }
                            --SurfaceTmp(SurfNum).Sides;
                            SurfaceTmp(SurfNum).Vertex.redimension(SurfaceTmp(SurfNum).Sides);
                        } else {
                            if (DisplayExtraWarnings) {
                                ShowContinueError("Cannot Drop Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) +
                                                  "]; Number of Surface Sides at minimum. This surface is now a degenerate surface.");
                            }
                            ++TotalDegenerateSurfaces;
                            // mark degenerate surface?
                        }
                        DistanceCheck = 0.0;
                    } else {
                        if (SurfaceTmp(SurfNum).Sides > 3) {
                            if (DisplayExtraWarnings) {
                                ShowContinueError("Dropping Vertex [" + RoundSigDigits(Vrt) + "].");
                            }
                            for (n = Vrt; n <= SurfaceTmp(SurfNum).Sides - 1; ++n) {
                                SurfaceTmp(SurfNum).Vertex(n).x = SurfaceTmp(SurfNum).Vertex(n + 1).x;
                                SurfaceTmp(SurfNum).Vertex(n).y = SurfaceTmp(SurfNum).Vertex(n + 1).y;
                                SurfaceTmp(SurfNum).Vertex(n).z = SurfaceTmp(SurfNum).Vertex(n + 1).z;
                            }
                            --SurfaceTmp(SurfNum).Sides;
                            SurfaceTmp(SurfNum).Vertex.redimension(SurfaceTmp(SurfNum).Sides);
                        } else {
                            if (DisplayExtraWarnings) {
                                ShowContinueError("Cannot Drop Vertex [" + RoundSigDigits(SurfaceTmp(SurfNum).Sides) +
                                                  "]; Number of Surface Sides at minimum. This surface is now a degenerate surface.");
                            }
                            ++TotalDegenerateSurfaces;
                            // mark degenerate surface?
                        }
                        DistanceCheck = 0.0;
                    }
                }
                Perimeter += DistanceCheck;
                ++Vrt;
                if (Vrt > SurfaceTmp(SurfNum).Sides) break;
            }

            SurfaceTmp(SurfNum).Perimeter = Perimeter;

            CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            CreateNewellAreaVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellAreaVector);
            // For surfaces with subsurfaces, the following two areas are turned into net areas later by
            // subtracting subsurface areas
            SurfaceTmp(SurfNum).GrossArea = VecLength(SurfaceTmp(SurfNum).NewellAreaVector);
            SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).GrossArea;
            SurfaceTmp(SurfNum).NetAreaShadowCalc = SurfaceTmp(SurfNum).Area;
            DetermineAzimuthAndTilt(SurfaceTmp(SurfNum).Vertex,
                                    SurfaceTmp(SurfNum).Sides,
                                    SurfWorldAz,
                                    SurfTilt,
                                    SurfaceTmp(SurfNum).lcsx,
                                    SurfaceTmp(SurfNum).lcsy,
                                    SurfaceTmp(SurfNum).lcsz,
                                    SurfaceTmp(SurfNum).GrossArea,
                                    SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            dotp = dot(SurfaceTmp(SurfNum).NewellSurfaceNormalVector, TestVector);
            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && dotp < -0.000001) {
                TiltString = RoundSigDigits(SurfTilt, 1);
                ShowWarningError(RoutineName + "Roof/Ceiling is upside down! Tilt angle=[" + TiltString + "], should be near 0, Surface=\"" +
                                 SurfaceTmp(SurfNum).Name + "\", in Zone=\"" + SurfaceTmp(SurfNum).ZoneName + "\".");
                ShowContinueError("Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, SurfaceTmp(SurfNum).Sides, SurfWorldAz, SurfTilt);
            } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && SurfTilt > 80.0) {
                TiltString = RoundSigDigits(SurfTilt, 1);
                ShowWarningError(RoutineName + "Roof/Ceiling is not oriented correctly! Tilt angle=[" + TiltString +
                                 "], should be near 0, Surface=\"" + SurfaceTmp(SurfNum).Name + "\", in Zone=\"" + SurfaceTmp(SurfNum).ZoneName +
                                 "\".");
            }
            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && dotp > 0.000001) {
                TiltString = RoundSigDigits(SurfTilt, 1);
                ShowWarningError(RoutineName + "Floor is upside down! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" +
                                 SurfaceTmp(SurfNum).Name + "\", in Zone=\"" + SurfaceTmp(SurfNum).ZoneName + "\".");
                ShowContinueError("Automatic fix is attempted.");
                ReverseAndRecalculate(state, SurfNum, SurfaceTmp(SurfNum).Sides, SurfWorldAz, SurfTilt);
            } else if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && SurfTilt < 158.2) { // slope/grade = 40%!
                TiltString = RoundSigDigits(SurfTilt, 1);
                ShowWarningError(RoutineName + "Floor is not oriented correctly! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" +
                                 SurfaceTmp(SurfNum).Name + "\", in Zone=\"" + SurfaceTmp(SurfNum).ZoneName + "\".");
            }
            SurfaceTmp(SurfNum).Azimuth = SurfWorldAz;
            SurfaceTmp(SurfNum).Tilt = SurfTilt;

            // Sine and cosine of azimuth and tilt
            SurfaceTmp(SurfNum).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
            if (SurfaceTmp(SurfNum).ViewFactorGround == DataGlobalConstants::AutoCalculate()) {
                SurfaceTmp(SurfNum).ViewFactorGround = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);
            }
            // Outward normal unit vector (pointing away from room)
            SurfaceTmp(SurfNum).OutNormVec = SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
            for (n = 1; n <= 3; ++n) {
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
            }

            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window || SurfaceTmp(SurfNum).Class == SurfaceClass::GlassDoor ||
                SurfaceTmp(SurfNum).Class == SurfaceClass::Door)
                SurfaceTmp(SurfNum).Area *= SurfaceTmp(SurfNum).Multiplier;
            // Can perform tests on this surface here
            SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + SurfaceTmp(SurfNum).CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            SurfaceTmp(SurfNum).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSky;
            SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);

            // Call to transform vertices

            TransformVertsByAspect(state, SurfNum, SurfaceTmp(SurfNum).Sides);

        } else {
            ShowFatalError(RoutineName + "Called with less than 2 sides, Surface=" + SurfaceTmp(SurfNum).Name);
        }

        // Preliminary Height/Width
        temp = SurfaceTmp(SurfNum).Vertex(3) - SurfaceTmp(SurfNum).Vertex(2);
        ThisWidth = VecLength(temp);
        temp = SurfaceTmp(SurfNum).Vertex(2) - SurfaceTmp(SurfNum).Vertex(1);
        ThisHeight = VecLength(temp);
        SurfaceTmp(SurfNum).Height = ThisHeight;
        SurfaceTmp(SurfNum).Width = ThisWidth;
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
        using General::RoundSigDigits;


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
            Vertices(n) = SurfaceTmp(SurfNum).Vertex(n);
        }
        RevPtr = NSides;
        for (n = 1; n <= NSides; ++n) {
            SurfaceTmp(SurfNum).Vertex(n) = Vertices(RevPtr);
            --RevPtr;
        }

        print(state.files.debug, "Reversing Surface Name={}\n", SurfaceTmp(SurfNum).Name);
        for (n = 1; n <= NSides; ++n) {
            print(state.files.debug,
                  "side={:5} abs coord vertex= {:18.13F} {:18.13F} {:18.13F}\n",
                  n,
                  SurfaceTmp(SurfNum).Vertex(n).x,
                  SurfaceTmp(SurfNum).Vertex(n).y,
                  SurfaceTmp(SurfNum).Vertex(n).z);
        }

        CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        DetermineAzimuthAndTilt(SurfaceTmp(SurfNum).Vertex,
                                SurfaceTmp(SurfNum).Sides,
                                SurfAzimuth,
                                SurfTilt,
                                SurfaceTmp(SurfNum).lcsx,
                                SurfaceTmp(SurfNum).lcsy,
                                SurfaceTmp(SurfNum).lcsz,
                                SurfaceTmp(SurfNum).GrossArea,
                                SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Roof && SurfTilt > 80.0) {
            TiltString = RoundSigDigits(SurfTilt, 1);
            ShowWarningError(RoutineName + "Roof/Ceiling is still upside down! Tilt angle=[" + TiltString +
                             "], should be near 0, please fix manually.");
        }
        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Floor && SurfTilt < 158.2) { // 40% grade!
            ShowWarningError(RoutineName + "Floor is still upside down! Tilt angle=[" + TiltString + "], should be near 180, please fix manually.");
        }
    }

    void MakeMirrorSurface(int &SurfNum) // In=>Surface to Mirror, Out=>new Surface index
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace Vectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Vert;
        int NVert;
        Real64 SurfWorldAz;
        Real64 SurfTilt;
        int n;
        //  TYPE(Vector) :: temp1

        NVert = SurfaceTmp(SurfNum).Sides;
        SurfaceTmp(SurfNum + 1).Vertex.allocate(NVert);
        // doesn't work when Vertex are pointers  SurfaceTmp(SurfNum+1)=SurfaceTmp(SurfNum)
        SurfaceTmp(SurfNum + 1).Name = SurfaceTmp(SurfNum).Name;
        SurfaceTmp(SurfNum + 1).Construction = SurfaceTmp(SurfNum).Construction;
        SurfaceTmp(SurfNum + 1).ConstructionStoredInputValue = SurfaceTmp(SurfNum).ConstructionStoredInputValue;
        SurfaceTmp(SurfNum + 1).Class = SurfaceTmp(SurfNum).Class;
        SurfaceTmp(SurfNum + 1).GrossArea = SurfaceTmp(SurfNum).GrossArea;
        SurfaceTmp(SurfNum + 1).Area = SurfaceTmp(SurfNum).Area;
        SurfaceTmp(SurfNum + 1).Azimuth = SurfaceTmp(SurfNum).Azimuth;
        SurfaceTmp(SurfNum + 1).Height = SurfaceTmp(SurfNum).Height;
        SurfaceTmp(SurfNum + 1).Reveal = SurfaceTmp(SurfNum).Reveal;
        SurfaceTmp(SurfNum + 1).Shape = SurfaceTmp(SurfNum).Shape;
        SurfaceTmp(SurfNum + 1).Sides = SurfaceTmp(SurfNum).Sides;
        SurfaceTmp(SurfNum + 1).Tilt = SurfaceTmp(SurfNum).Tilt;
        SurfaceTmp(SurfNum + 1).Width = SurfaceTmp(SurfNum).Width;
        SurfaceTmp(SurfNum + 1).HeatTransSurf = SurfaceTmp(SurfNum).HeatTransSurf;
        SurfaceTmp(SurfNum + 1).BaseSurfName = SurfaceTmp(SurfNum).BaseSurfName;
        SurfaceTmp(SurfNum + 1).BaseSurf = SurfaceTmp(SurfNum).BaseSurf;
        SurfaceTmp(SurfNum + 1).ZoneName = SurfaceTmp(SurfNum).ZoneName;
        SurfaceTmp(SurfNum + 1).Zone = SurfaceTmp(SurfNum).Zone;
        SurfaceTmp(SurfNum + 1).ExtBoundCondName = SurfaceTmp(SurfNum).ExtBoundCondName;
        SurfaceTmp(SurfNum + 1).ExtBoundCond = SurfaceTmp(SurfNum).ExtBoundCond;
        SurfaceTmp(SurfNum + 1).ExtSolar = SurfaceTmp(SurfNum).ExtSolar;
        SurfaceTmp(SurfNum + 1).ExtWind = SurfaceTmp(SurfNum).ExtWind;
        SurfaceTmp(SurfNum + 1).ViewFactorGround = SurfaceTmp(SurfNum).ViewFactorGround;
        SurfaceTmp(SurfNum + 1).ViewFactorSky = SurfaceTmp(SurfNum).ViewFactorSky;
        SurfaceTmp(SurfNum + 1).ViewFactorGroundIR = SurfaceTmp(SurfNum).ViewFactorGroundIR;
        SurfaceTmp(SurfNum + 1).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSkyIR;
        SurfaceTmp(SurfNum + 1).SchedShadowSurfIndex = SurfaceTmp(SurfNum).SchedShadowSurfIndex;
        SurfaceTmp(SurfNum + 1).ShadowSurfSchedVaries = SurfaceTmp(SurfNum).ShadowSurfSchedVaries;
        SurfaceTmp(SurfNum + 1).SchedMinValue = SurfaceTmp(SurfNum).SchedMinValue;
        SurfaceTmp(SurfNum + 1).IsTransparent = SurfaceTmp(SurfNum).IsTransparent;
        SurfaceTmp(SurfNum + 1).ShadowingSurf = SurfaceTmp(SurfNum).ShadowingSurf;
        SurfaceTmp(SurfNum + 1).MaterialMovInsulExt = SurfaceTmp(SurfNum).MaterialMovInsulExt;
        SurfaceTmp(SurfNum + 1).MaterialMovInsulInt = SurfaceTmp(SurfNum).MaterialMovInsulInt;
        SurfaceTmp(SurfNum + 1).SchedMovInsulExt = SurfaceTmp(SurfNum).SchedMovInsulExt;
        SurfaceTmp(SurfNum + 1).SchedMovInsulInt = SurfaceTmp(SurfNum).SchedMovInsulInt;
        SurfaceTmp(SurfNum + 1).activeWindowShadingControl = SurfaceTmp(SurfNum).activeWindowShadingControl;
        SurfaceTmp(SurfNum + 1).windowShadingControlList = SurfaceTmp(SurfNum).windowShadingControlList;
        SurfaceTmp(SurfNum + 1).HasShadeControl = SurfaceTmp(SurfNum).HasShadeControl;
        SurfaceTmp(SurfNum + 1).activeShadedConstruction = SurfaceTmp(SurfNum).activeShadedConstruction;
        SurfaceTmp(SurfNum + 1).FrameDivider = SurfaceTmp(SurfNum).FrameDivider;
        SurfaceTmp(SurfNum + 1).Multiplier = SurfaceTmp(SurfNum).Multiplier;
        SurfaceTmp(SurfNum + 1).NetAreaShadowCalc = SurfaceTmp(SurfNum).NetAreaShadowCalc;
        SurfaceTmp(SurfNum + 1).Perimeter = SurfaceTmp(SurfNum).Perimeter;

        for (Vert = 1; Vert <= SurfaceTmp(SurfNum).Sides; ++Vert) {
            SurfaceTmp(SurfNum + 1).Vertex(Vert) = SurfaceTmp(SurfNum).Vertex(NVert);
            --NVert;
        }
        ++SurfNum;
        SurfaceTmp(SurfNum).Name = "Mir-" + SurfaceTmp(SurfNum - 1).Name;

        // TH 3/26/2010
        SurfaceTmp(SurfNum).MirroredSurf = true;

        if (SurfaceTmp(SurfNum).Sides > 2) {
            CreateNewellAreaVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellAreaVector);
            SurfaceTmp(SurfNum).GrossArea = VecLength(SurfaceTmp(SurfNum).NewellAreaVector);
            SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).GrossArea;
            SurfaceTmp(SurfNum).NetAreaShadowCalc = SurfaceTmp(SurfNum).Area;
            CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum).Vertex, SurfaceTmp(SurfNum).Sides, SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            DetermineAzimuthAndTilt(SurfaceTmp(SurfNum).Vertex,
                                    SurfaceTmp(SurfNum).Sides,
                                    SurfWorldAz,
                                    SurfTilt,
                                    SurfaceTmp(SurfNum).lcsx,
                                    SurfaceTmp(SurfNum).lcsy,
                                    SurfaceTmp(SurfNum).lcsz,
                                    SurfaceTmp(SurfNum).GrossArea,
                                    SurfaceTmp(SurfNum).NewellSurfaceNormalVector);
            SurfaceTmp(SurfNum).Azimuth = SurfWorldAz;
            SurfaceTmp(SurfNum).Tilt = SurfTilt;

            // Sine and cosine of azimuth and tilt
            SurfaceTmp(SurfNum).SinAzim = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).CosAzim = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).SinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
            SurfaceTmp(SurfNum).CosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
            // Outward normal unit vector (pointing away from room)
            SurfaceTmp(SurfNum).OutNormVec = SurfaceTmp(SurfNum).NewellSurfaceNormalVector;
            for (n = 1; n <= 3; ++n) {
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) - 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = +1.0;
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n) + 1.0) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = -1.0;
                if (std::abs(SurfaceTmp(SurfNum).OutNormVec(n)) < 1.e-06) SurfaceTmp(SurfNum).OutNormVec(n) = 0.0;
            }

            // Can perform tests on this surface here
            SurfaceTmp(SurfNum).ViewFactorSky = 0.5 * (1.0 + SurfaceTmp(SurfNum).CosTilt);
            // The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
            // surfaces
            SurfaceTmp(SurfNum).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSky;
            SurfaceTmp(SurfNum).ViewFactorGroundIR = 0.5 * (1.0 - SurfaceTmp(SurfNum).CosTilt);
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

        // FLOW:
        // Get the total number of window shading control blocks
        cCurrentModuleObject = "WindowShadingControl";
        TotWinShadingControl = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
            UtilityRoutines::VerifyName(cAlphaArgs(1), WindowShadingControl, ControlNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
            if (ErrorInName) {
                ErrorsFound = true;
                continue;
            }

            ++ControlNum;
            WindowShadingControl(ControlNum).Name = cAlphaArgs(1); // Set the Control Name in the Derived Type

            WindowShadingControl(ControlNum).ZoneIndex = UtilityRoutines::FindItemInList(cAlphaArgs(2), Zone);
            if (WindowShadingControl(ControlNum).ZoneIndex == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                "\" not found.");
                ErrorsFound = true;
            }

            WindowShadingControl(ControlNum).SequenceNumber = int(rNumericArgs(1));
            // WindowShadingControl().getInputShadedConstruction is only used during GetInput process and is ultimately stored in Surface().shadedConstructionList
            WindowShadingControl(ControlNum).getInputShadedConstruction = UtilityRoutines::FindItemInList(cAlphaArgs(4), state.dataConstruction->Construct, TotConstructs);
            WindowShadingControl(ControlNum).ShadingDevice = UtilityRoutines::FindItemInList(cAlphaArgs(9), dataMaterial.Material, TotMaterials);
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
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" should be either SEQUENTIAL or GROUP " +
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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
                                "\" invalid. Must reference at least one Fenestration Surface object name.");
            }

            if (ControlType == "SCHEDULE") {
                ControlType = "ONIFSCHEDULEALLOWS";
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = true;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
                // Error if schedule has not been specified
                if (WindowShadingControl(ControlNum).Schedule <= 0) {
                    ErrorsFound = true;
                    ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + " has " + cAlphaFieldNames(5) + " \"" +
                                     ControlType + "\" but a schedule has not been specified.");
                }
            }

            if (has_prefix(ControlType, "SCHEDULEAND")) {
                ControlType = "ONIFHIGH" + ControlType.substr(11);
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = true;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
                // Error if schedule has not been specified
                if (WindowShadingControl(ControlNum).Schedule <= 0) {
                    ErrorsFound = true;
                    ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + " has " + cAlphaFieldNames(5) + " \"" +
                                     ControlType + "\" but a schedule has not been specified.");
                }
            }

            if (has_prefix(ControlType, "GLAREOR")) {
                ControlType = "ONIFHIGH" + ControlType.substr(7);
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = true;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            if (ControlType == "GLARE") {
                ControlType = "ONIFHIGHGLARE";
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = true;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            if (WindowShadingControl(ControlNum).ShadingDevice > 0) {
                if (dataMaterial.Material(WindowShadingControl(ControlNum).ShadingDevice).Group == Screen &&
                    !(ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS")) {
                    ErrorsFound = true;
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) +
                                    "=\"" + cAlphaArgs(5) + "\" for exterior screens.");
                    ShowContinueError("Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                }
            } else {
                if (WindowShadingControl(ControlNum).getInputShadedConstruction > 0) {
                    state.dataConstruction->Construct(WindowShadingControl(ControlNum).getInputShadedConstruction).IsUsed = true;
                    if (dataMaterial.Material(state.dataConstruction->Construct(WindowShadingControl(ControlNum).getInputShadedConstruction).LayerPoint(1)).Group == Screen &&
                        !(ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS")) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) +
                                        "=\"" + cAlphaArgs(5) + "\" for exterior screens.");
                        ShowContinueError("Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.");
                    }
                } else if (lAlphaFieldBlanks(4)) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", " + cAlphaFieldNames(4) +
                                    " is blank.");
                    ShowContinueError("A valid construction is required.");
                    ErrorsFound = true;
                } else {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", " + cAlphaFieldNames(4) +
                                    " is invalid.");
                    ShowContinueError("Construction=\"" + cAlphaArgs(4) + "\" was used. A valid construction is required.");
                    ErrorsFound = true;
                }
            }

            // Warning if setpoint is unintentionally zero
            if (WindowShadingControl(ControlNum).SetPoint == 0 && ControlType != "ALWAYSON" && ControlType != "ALWAYSOFF" &&
                ControlType != "ONIFSCHEDULEALLOWS" && ControlType != "SCHEDULE" && ControlType != "ONIFHIGHGLARE" && ControlType != "GLARE" &&
                ControlType != "DAYLIGHTILLUMINANCE") {
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\", The first SetPoint is zero.");
                ShowContinueError("..You may have forgotten to specify that setpoint.");
            }

            // Upward compatibility for old Shading Control Type names
            if (ControlType == "SOLARONWINDOW" || ControlType == "HORIZONTALSOLAR" || ControlType == "OUTSIDEAIRTEMP" ||
                ControlType == "ZONEAIRTEMP" || ControlType == "ZONECOOLING") {
                ControlType = "ONIFHIGH" + ControlType;
                WindowShadingControl(ControlNum).ShadingControlIsScheduled = false;
                WindowShadingControl(ControlNum).GlareControlIsActive = false;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", changing to \"" + ControlType + "\"");
            }

            // Error if illegal control type
            Found = UtilityRoutines::FindItemInList(ControlType, cValidWindowShadingControlTypes, NumValidWindowShadingControlTypes);
            if (Found == 0) {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(5) + "=\"" +
                                cAlphaArgs(5) + "\".");
            } else {
                WindowShadingControl(ControlNum).ShadingControlType = ValidWindowShadingControlTypes(Found);
            }

            // Error checks
            if (cAlphaArgs(7) != "YES" && cAlphaArgs(7) != "NO") { // Shading Control is Schedule field
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(7) + "=\"" +
                                cAlphaArgs(7) + "\".");
            }
            if (cAlphaArgs(8) != "YES" && cAlphaArgs(8) != "NO") { // Glare Control is Active field
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(8) + "=\"" +
                                cAlphaArgs(8) + "\".");
            }

            if ((WindowShadingControl(ControlNum).ShadingControlType == WSCT_OnIfScheduled) &&
                (!WindowShadingControl(ControlNum).ShadingControlIsScheduled)) { // CR 7709 BG
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + " = \"" + WindowShadingControl(ControlNum).Name + "\" invalid, " + cAlphaFieldNames(7) +
                                " must be set to \"Yes\" for " + cAlphaFieldNames(5) + " = OnIfScheduleAllows");
            }

            if (cAlphaArgs(10) != "FIXEDSLATANGLE" && cAlphaArgs(10) != "SCHEDULEDSLATANGLE" && cAlphaArgs(10) != "BLOCKBEAMSOLAR") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(10) + "=\"" +
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
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\", changing to \"InteriorShade\"");
                WindowShadingControl(ControlNum).ShadingType = WSC_ST_InteriorShade;
                cAlphaArgs(3) = "INTERIORSHADE";
            }
            if (cAlphaArgs(3) == "EXTERIORNONINSULATINGSHADE" || cAlphaArgs(3) == "EXTERIORINSULATINGSHADE") {
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" is using obsolete " +
                                 cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\", changing to \"ExteriorShade\"");
                WindowShadingControl(ControlNum).ShadingType = WSC_ST_ExteriorShade;
                cAlphaArgs(3) = "EXTERIORSHADE";
            }

            if (ControlType == "MEETDAYLIGHTILLUMINANCESETPOINT" && cAlphaArgs(3) != "SWITCHABLEGLAZING") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
                ShowContinueError("..." + cAlphaFieldNames(3) + " must be SwitchableGlazing for this control, but entered type=\"" + cAlphaArgs(3) +
                                  "\".");
            }

            // Check for illegal shading type name
            Found = UtilityRoutines::FindItemInList(cAlphaArgs(3), cValidShadingTypes, NumValidShadingTypes);
            if (Found == 0) {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" invalid " + cAlphaFieldNames(3) + "=\"" +
                                cAlphaArgs(3) + "\".");
            } else {
                WindowShadingControl(ControlNum).ShadingType = ValidShadingTypes(Found);
            }

            ShTyp = WindowShadingControl(ControlNum).ShadingType;
            IShadedConst = WindowShadingControl(ControlNum).getInputShadedConstruction;
            IShadingDevice = WindowShadingControl(ControlNum).ShadingDevice;

            if (IShadedConst == 0 && IShadingDevice == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name +
                                "\" has no matching shaded construction or shading device.");
                ErrorsFound = true;
            } else if (IShadedConst == 0 && IShadingDevice > 0) {
                if (ShTyp == WSC_ST_SwitchableGlazing) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= SwitchableGlazing but no matching shaded construction");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade) && dataMaterial.Material(IShadingDevice).Group != Shade) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= InteriorShade or ExteriorShade but matching shading device is not a window shade");
                    ShowContinueError(cAlphaFieldNames(8) + " in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_ExteriorScreen) && dataMaterial.Material(IShadingDevice).Group != Screen) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= ExteriorScreen but matching shading device is not a window screen");
                    ShowContinueError(cAlphaFieldNames(8) + " in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if ((ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind) && dataMaterial.Material(IShadingDevice).Group != WindowBlind) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind");
                    ShowContinueError(cAlphaFieldNames(8) + " in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
                    ErrorsFound = true;
                }
                if (ShTyp == WSC_ST_BetweenGlassShade || ShTyp == WSC_ST_BetweenGlassBlind) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaArgs(3) +
                                    "= BetweenGlassShade or BetweenGlassBlind and");
                    ShowContinueError(cAlphaFieldNames(8) + " is specified. This is illegal. Specify shaded construction instead.");
                    ErrorsFound = true;
                }
            } else if (IShadedConst > 0 && IShadingDevice > 0) {
                IShadingDevice = 0;
                ShowWarningError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" Both " + cAlphaFieldNames(4) + " and " +
                                 cAlphaFieldNames(9) + " are specified.");
                ShowContinueError("The " + cAlphaFieldNames(4) + "=\"" + state.dataConstruction->Construct(IShadedConst).Name + "\" will be used.");
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
                        if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers)).Group != Shade) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a shade layer on the inside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorShade) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != Shade) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(43) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a shade layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorScreen) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != Screen) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a screen layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_InteriorBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers);
                        if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(NLayers)).Group != WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a blind layer on the inside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_ExteriorBlind) {
                        IShadingDevice = state.dataConstruction->Construct(IShadedConst).LayerPoint(1);
                        if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(1)).Group != WindowBlind) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) +
                                              "\" should have a blind layer on the outside of the window.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_BetweenGlassShade) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3)).Group != Shade) BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5)).Group != Shade) BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(32) +
                                              "\" should have two or three glass layers and a");
                            ShowContinueError("between-glass shade layer with a gas layer on each side.");
                        }
                    } else if (WindowShadingControl(ControlNum).ShadingType == WSC_ST_BetweenGlassBlind) {
                        if (NLayers != 5 && NLayers != 7) BGShadeBlindError = true;
                        if (NLayers == 5) {
                            if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(3)).Group != WindowBlind) BGShadeBlindError = true;
                        }
                        if (NLayers == 7) {
                            if (dataMaterial.Material(state.dataConstruction->Construct(IShadedConst).LayerPoint(5)).Group != WindowBlind) BGShadeBlindError = true;
                        }
                        if (BGShadeBlindError) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" the " + cAlphaFieldNames(4) +
                                            "=\"" + cAlphaArgs(4) + "\"");
                            ShowContinueError("of " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" should have two or three glass layers and a");
                            ShowContinueError("between-glass blind layer with a gas layer on each side.");
                        }
                    }
                }
                if (IShadingDevice > 0) {
                    if ((ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade) && dataMaterial.Material(IShadingDevice).Group != Shade) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= InteriorShade or ExteriorShade but matching shading device is not a window shade");
                        ShowContinueError("Shading Device in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WSC_ST_ExteriorScreen) && dataMaterial.Material(IShadingDevice).Group != Screen) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= ExteriorScreen but matching shading device is not an exterior window screen.");
                        ShowContinueError("Shading Device in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
                        ErrorsFound = true;
                    }
                    if ((ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind) && dataMaterial.Material(IShadingDevice).Group != WindowBlind) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + WindowShadingControl(ControlNum).Name + "\" has " + cAlphaFieldNames(3) +
                                        "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind.");
                        ShowContinueError("Shading Device in error=\"" + dataMaterial.Material(IShadingDevice).Name + "\".");
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
                if (UtilityRoutines::SameString(WindowShadingControl(iShadeCtrl).FenestrationName(jFeneRef), SurfaceTmp(SurfNum).Name)) {
                    SurfaceTmp(SurfNum).HasShadeControl = true;
                    SurfaceTmp(SurfNum).windowShadingControlList.push_back(iShadeCtrl);
                    SurfaceTmp(SurfNum).activeWindowShadingControl = iShadeCtrl;
                    SurfaceTmp(SurfNum).shadedConstructionList.push_back(curShadedConstruction);
                    SurfaceTmp(SurfNum).activeShadedConstruction = curShadedConstruction;

                    // check to make the window refenced is an exterior window
                    if (SurfaceTmp(SurfNum).ExtBoundCond != ExternalEnvironment) {
                        ErrorsFound = true;
                        ShowSevereError("InitialAssociateWindowShadingControlFenestration: \"" + SurfaceTmp(SurfNum).Name + "\", invalid " +
                                        " because it is not an exterior window.");
                        ShowContinueError(".. It appears on WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name);
                    }
                    // check to make sure the window is not using equivalent layer window construction
                    if (state.dataConstruction->Construct(SurfaceTmp(SurfNum).Construction).WindowTypeEQL) {
                        ErrorsFound = true;
                        ShowSevereError("InitialAssociateWindowShadingControlFenestration: =\"" + SurfaceTmp(SurfNum).Name + "\", invalid " +
                                        "\".");
                        ShowContinueError(".. equivalent layer window model does not use shading control object.");
                        ShowContinueError(".. Shading control is set to none or zero, and simulation continues.");
                        ShowContinueError(".. It appears on WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name);
                        SurfaceTmp(SurfNum).activeWindowShadingControl = 0;
                    }
                }
            }
        }
    }

    void FinalAssociateWindowShadingControlFenestration(bool &ErrorsFound)
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
                    ShowSevereError("FinalAssociateWindowShadingControlFenestration: Fenestration surface named \"" +
                                    Surface(fenestrationIndex).Name +
                                    "\" has WindowShadingContol index that does not match the initial index assigned.");
                    ShowContinueError("This occurs while WindowShadingControl object: \"" + WindowShadingControl(iShadeCtrl).Name +
                                      "\" is being evaluated. ");
                }
            }
        }
    }

    void CheckWindowShadingControlSimilarForWindow(bool& ErrorsFound)
    {
        // For each window check if all window shading controls on list are the same except for name, schedule name, construction, and material
        for (auto theSurf : Surface) {
            if (theSurf.HasShadeControl) {
                if (theSurf.windowShadingControlList.size() > 1) {
                    int firstWindowShadingControl = theSurf.windowShadingControlList.front();
                    for (auto wsc = std::next(theSurf.windowShadingControlList.begin()); wsc != theSurf.windowShadingControlList.end(); ++wsc) {
                        if (!isWindowShadingControlSimilar(firstWindowShadingControl, *wsc)) {
                            ErrorsFound = true;
                            ShowSevereError("CheckWindowShadingControlSimilarForWindow: Fenestration surface named \"" + theSurf.Name +
                                        "\" has multiple WindowShadingContols that are not similar.");
                            ShowContinueError("for: \"" + WindowShadingControl(firstWindowShadingControl).Name + " and: " + WindowShadingControl(*wsc).Name);
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
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int IOStat;           // IO Status when calling get input subroutine
        int StormWinNumAlpha; // Number of alpha names being passed
        int StormWinNumProp;  // Number of properties being passed
        int StormWinNum;      // Index for storm window number
        int loop;             // Do loop counter
        int SurfNum;          // Surface number
        int MatNum;           // Material number

        // FLOW:

        // Get the total number of storm window input objects
        cCurrentModuleObject = "WindowProperty:StormWindow";
        TotStormWin = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
            StormWindow(StormWinNum).StormWinMaterialNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), dataMaterial.Material, TotMaterials);
            StormWindow(StormWinNum).StormWinDistance = rNumericArgs(1);
            StormWindow(StormWinNum).MonthOn = rNumericArgs(2);
            StormWindow(StormWinNum).DayOfMonthOn = rNumericArgs(3);
            StormWindow(StormWinNum).DateOn = General::OrdinalDay(StormWindow(StormWinNum).MonthOn, StormWindow(StormWinNum).DayOfMonthOn, 1);
            StormWindow(StormWinNum).MonthOff = rNumericArgs(4);
            StormWindow(StormWinNum).DayOfMonthOff = rNumericArgs(5);
            StormWindow(StormWinNum).DateOff = General::OrdinalDay(StormWindow(StormWinNum).MonthOff, StormWindow(StormWinNum).DayOfMonthOff, 1);

            if (StormWindow(StormWinNum).DateOn == StormWindow(StormWinNum).DateOff) {
                ShowSevereError(cCurrentModuleObject + ": Date On = Date Off -- not allowed, occurred in WindowProperty:StormWindow Input #" +
                                TrimSigDigits(StormWinNum));
                ErrorsFound = true;
            }

            {
                auto const SELECT_CASE_var(StormWindow(StormWinNum).MonthOn);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 31) {
                        ShowSevereError(cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOn) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 30) {
                        ShowSevereError(cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOn) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (StormWindow(StormWinNum).DayOfMonthOn > 29) {
                        ShowSevereError(cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOn) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + ": Date On Month [" + TrimSigDigits(StormWindow(StormWinNum).MonthOn) +
                                    "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                    ErrorsFound = true;
                }
            }
            {
                auto const SELECT_CASE_var(StormWindow(StormWinNum).MonthOff);

                if ((SELECT_CASE_var == 1) || (SELECT_CASE_var == 3) || (SELECT_CASE_var == 5) || (SELECT_CASE_var == 7) || (SELECT_CASE_var == 8) ||
                    (SELECT_CASE_var == 10) || (SELECT_CASE_var == 12)) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 31) {
                        ShowSevereError(cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOff) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else if ((SELECT_CASE_var == 4) || (SELECT_CASE_var == 6) || (SELECT_CASE_var == 9) || (SELECT_CASE_var == 11)) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 30) {
                        ShowSevereError(cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOff) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == 2) {
                    if (StormWindow(StormWinNum).DayOfMonthOff > 29) {
                        ShowSevereError(cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOff) +
                                        "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + ": Date Off Month [" + TrimSigDigits(StormWindow(StormWinNum).MonthOff) +
                                    "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits(StormWinNum));
                    ErrorsFound = true;
                }
            }
        }

        // Error checks

        for (StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
            // Require BaseWindowNum be that of an exterior window
            SurfNum = StormWindow(StormWinNum).BaseWindowNum;
            if (SurfNum == 0) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid.");
                ErrorsFound = true;
            } else {
                if (Surface(SurfNum).Class != SurfaceClass::Window || Surface(SurfNum).ExtBoundCond != 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowSevereError("cannot be used with surface=" + Surface(SurfNum).Name);
                    ShowContinueError("because that surface is not an exterior window.");
                    ErrorsFound = true;
                }
            }

            // Require that storm window material be glass
            MatNum = StormWindow(StormWinNum).StormWinMaterialNum;
            if (SurfNum > 0) {
                if (MatNum == 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowContinueError(cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\" not found as storm window layer.");
                    ErrorsFound = true;
                } else {
                    if (dataMaterial.Material(MatNum).Group != WindowGlass) {
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                        ShowContinueError(cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                          "must be a WindowMaterial:Glazing or WindowMaterial:Glazing:RefractionExtinctionMethod");
                        ErrorsFound = true;
                    }
                }
            }

            // Error if base window has airflow control
            if (SurfNum > 0) {
                if (SurfWinAirflowControlType(SurfNum) != 0) {
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\"");
                    ShowContinueError(" cannot be used because it is an airflow window (i.e., has WindowProperty:AirflowControl specified)");
                    ErrorsFound = true;
                }
            }

            // Check for reversal of on and off times
            if (SurfNum > 0) {
                if ((Latitude > 0.0 && (StormWindow(StormWinNum).MonthOn < StormWindow(StormWinNum).MonthOff)) ||
                    (Latitude <= 0.0 && (StormWindow(StormWinNum).MonthOn > StormWindow(StormWinNum).MonthOff))) {
                    ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" check times that storm window");
                    ShowContinueError("is put on (month=" + TrimSigDigits(StormWindow(StormWinNum).MonthOn) +
                                      ", day=" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOn) +
                                      ") and taken off (month=" + TrimSigDigits(StormWindow(StormWinNum).MonthOff) +
                                      ", day=" + TrimSigDigits(StormWindow(StormWinNum).DayOfMonthOff) + ");");
                    ShowContinueError("these times may be reversed for your building latitude=" + TrimSigDigits(Latitude, 2) + " deg.");
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
        TotWinAirflowControl = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" not found.");
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" is not an exterior window with 2 or 3 glass layers.");
                    ErrorsFound = true;
                }
            }

            // Error if illegal airflow source
            if (cAlphaArgs(2) != "INDOORAIR" && cAlphaArgs(2) != "OUTDOORAIR") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
            }

            // Error if illegal airflow destination
            if (cAlphaArgs(3) != "INDOORAIR" && cAlphaArgs(3) != "OUTDOORAIR" && cAlphaArgs(3) != "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
            }

            // Error if source = OutsideAir and destination = ReturnAir
            if (cAlphaArgs(2) == "OUTDOORAIR" && cAlphaArgs(3) == "RETURNAIR") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
                ShowContinueError("..when " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
            }

            // Error if illegal airflow control type
            if (cAlphaArgs(4) != "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs(4) != "ALWAYSOFF" && cAlphaArgs(4) != "SCHEDULEDONLY") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
            }

            // Error if illegal value for Airflow Has Multiplier Schedule
            if (cAlphaArgs(5) != "YES" && cAlphaArgs(5) != "NO") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\"");
            }

            // Error if Airflow Control Type = ScheduledOnly and Airflow Has Multiplier Schedule = No
            if (cAlphaArgs(4) == "SCHEDULEDONLY" && cAlphaArgs(5) == "NO") {
                ErrorsFound = true;
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError("..when " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\"");
            }

            // Warning if Airflow Control Type = AlwaysOnAtMaxFlow and Airflow Has Multiplier Schedule = Yes
            if (cAlphaArgs(4) == "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs(5) == "YES") {
                ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError("..but " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "If specified, the " + cAlphaFieldNames(5) +
                                  " will be ignored.");
            }

            // Warning if Airflow Control Type = AlwaysOff and Airflow Has Multiplier Schedule = Yes
            if (cAlphaArgs(4) == "ALWAYSOFF" && cAlphaArgs(5) == "YES") {
                ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                ShowContinueError("..but " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\". If specified, the " + cAlphaFieldNames(5) +
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
                        DataZoneEquipment::ZoneEquipConfig(controlledZoneNum).ZoneHasAirFlowWindowReturn = true;
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
                        ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + Surface(SurfNum).Name +
                                        "\", airflow window return air node not found for " + cAlphaFieldNames(3) + " = " + cAlphaArgs(3));
                        if (!lAlphaFieldBlanks(7))
                            ShowContinueError(cAlphaFieldNames(7) + "=\"" + cAlphaArgs(7) + "\" did not find a matching return air node.");
                        ShowContinueError("..Airflow windows with Airflow Destination = ReturnAir must reference a controlled Zone (appear in a "
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", has " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) +
                                        "\"");
                        ShowContinueError("..and " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\", but no " + cAlphaFieldNames(6) +
                                          " specified.");
                    } else {
                        SurfWinAirflowHasSchedule(SurfNum) = true;
                        SurfWinAirflowSchedulePtr(SurfNum) = GetScheduleIndex(state, cAlphaArgs(6));
                        if (SurfWinAirflowSchedulePtr(SurfNum) == 0) {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(6) + "=\"" +
                                            cAlphaArgs(6) + "\"");
                        }
                    }
                }
                // Warning if associated window is an interior window
                if (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && !ErrorsFound)
                    ShowWarningError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", is an Interior window; cannot be an airflow window.");
                if (!ErrorsFound) {
                    // Require that gas in airflow gap has type = air
                    MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(2);
                    if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers == 3) MatGapFlow = state.dataConstruction->Construct(ConstrNum).LayerPoint(4);
                    if (dataMaterial.Material(MatGapFlow).GasType(1) != 1) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", Gas type not air in airflow gap of construction " +
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
                                if (dataMaterial.Material(MatGapFlow1).GasType(1) != 1 || dataMaterial.Material(MatGapFlow2).GasType(1) != 1) {
                                    ErrorsFound = true;
                                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) +
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
        int TotKivaStgs = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (TotKivaStgs > 1) {
            ErrorsFound = true;
            ShowSevereError("Multiple " + cCurrentModuleObject + " objects found. Only one is allowed.");
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
                kivaManager.settings.soilK = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.soilRho = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.soilCp = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.groundSolarAbs = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.groundThermalAbs = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.groundRoughness = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.farFieldWidth = rNumericArgs(numF);
            }
            numF++;

            if (!lAlphaFieldBlanks(alpF)) {
                if (UtilityRoutines::SameString(cAlphaArgs(alpF), "ZeroFlux")) {
                    kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::ZERO_FLUX;
                } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "GroundWater")) {
                    kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::GROUNDWATER;
                } else if (UtilityRoutines::SameString(cAlphaArgs(alpF), "Autoselect")) {
                    kivaManager.settings.deepGroundBoundary = HeatBalanceKivaManager::KivaManager::Settings::AUTO;
                } else {
                    ErrorsFound = true;
                    ShowSevereError("Foundation:Kiva:Settings, " + cAlphaArgs(alpF) + " is not a valid choice for " + cAlphaFieldNames(alpF));
                }
            }
            alpF++;

            if (lNumericFieldBlanks(numF) || rNumericArgs(numF) == DataGlobalConstants::AutoCalculate()) {
                kivaManager.settings.deepGroundDepth = 40.0;
            } else {
                kivaManager.settings.deepGroundDepth = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.minCellDim = rNumericArgs(numF);
            }
            numF++;
            if (!lNumericFieldBlanks(numF)) {
                kivaManager.settings.maxGrowthCoeff = rNumericArgs(numF);
            }
            numF++;

            if (!lAlphaFieldBlanks(alpF)) {
                if (UtilityRoutines::SameString(cAlphaArgs(alpF), "Hourly")) {
                    kivaManager.settings.timestepType = HeatBalanceKivaManager::KivaManager::Settings::HOURLY;
                    kivaManager.timestep = 3600.; // seconds
                } else /* if (UtilityRoutines::SameString(cAlphaArgs( alpF ), "Timestep")) */ {
                    kivaManager.settings.timestepType = HeatBalanceKivaManager::KivaManager::Settings::TIMESTEP;
                    kivaManager.timestep = DataGlobals::MinutesPerTimeStep * 60.;
                }
            }
            alpF++;
        }

        /* ====================================================================== */

        // Read Foundation objects
        cCurrentModuleObject = "Foundation:Kiva";
        int TotKivaFnds = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (TotKivaFnds > 0) {
            kivaManager.defineDefaultFoundation();

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
                UtilityRoutines::IsNameEmpty(fndInput.name, cCurrentModuleObject, ErrorInName);
                if (ErrorInName) {
                    ErrorsFound = true;
                    continue;
                } else {
                    fndNames(Loop) = fndInput.name;
                }

                // Start with copy of default
                auto &fnd = fndInput.foundation;
                fnd = kivaManager.defaultFoundation.foundation;

                // Indoor temperature
                if (!lNumericFieldBlanks(numF)) {
                    fndInput.assumedIndoorTemperature = rNumericArgs(numF);
                } else {
                    fndInput.assumedIndoorTemperature = -9999;
                }
                numF++;

                // Interior horizontal insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = dataMaterial.Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Must be of type \"Material\"");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.intHIns.width = -rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Interior vertical insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = dataMaterial.Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Must be of type \"Material\"");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.intVIns.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Exterior horizontal insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = dataMaterial.Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Must be of type \"Material\"");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.extHIns.width = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // Exterior vertical insulation
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = dataMaterial.Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Must be of type \"Material\"");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.extVIns.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
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
                        ShowSevereError("Did not find matching construction for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing construction = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &c = state.dataConstruction->Construct(fndInput.wallConstructionIndex);
                    c.IsUsed = true;
                    if (c.TypeIsWindow) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Cannot be a window construction");
                        continue;
                    }
                } else {
                    fndInput.wallConstructionIndex = 0; // Use default wall construction
                }
                alpF++;

                // Footing
                if (!lAlphaFieldBlanks(alpF)) {
                    int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                    if (index == 0) {
                        ErrorsFound = true;
                        ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                        cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                        continue;
                    }
                    auto &m = dataMaterial.Material(index);
                    if (m.Group != RegularMaterial || m.ROnly) {
                        ErrorsFound = true;
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                        cAlphaArgs(alpF));
                        ShowContinueError("Must be of type \"Material\"");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
                                        cNumericFieldNames(numF) + "provided");
                        continue;
                    } else {
                        fndInput.footing.depth = rNumericArgs(numF);
                    }
                    numF++;
                } else {
                    if (!lNumericFieldBlanks(numF)) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name + "\", no " + cAlphaFieldNames(alpF - 1) + " defined");
                        ShowContinueError(cNumericFieldNames(numF) + " will not be used.");
                    }
                    numF++;
                }

                // General Blocks
                int numRemainingFields = NumAlphas - (alpF - 1) + NumProps - (numF - 1);
                if (numRemainingFields > 0) {
                    int numBlocks = numRemainingFields / 4;
                    if (mod(numRemainingFields, 4) != 0) {
                        ShowWarningError(cCurrentModuleObject + "=\"" + fndInput.name +
                                         "\", number of Block fields not even multiple of 4. Will read in " + General::TrimSigDigits(numBlocks));
                    }
                    for (int blockNum = 0; blockNum < numBlocks; blockNum++) {
                        Kiva::InputBlock block;
                        if (!lAlphaFieldBlanks(alpF)) {
                            int index = UtilityRoutines::FindItemInList(cAlphaArgs(alpF), dataMaterial.Material);
                            if (index == 0) {
                                ErrorsFound = true;
                                ShowSevereError("Did not find matching material for " + cCurrentModuleObject + "=\"" + fndInput.name + "\", " +
                                                cAlphaFieldNames(alpF) + ", missing material = " + cAlphaArgs(alpF));
                                continue;
                            }
                            auto &m = dataMaterial.Material(index);
                            if (m.Group != RegularMaterial || m.ROnly) {
                                ErrorsFound = true;
                                ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", invalid " + cAlphaFieldNames(alpF) + "=\"" +
                                                cAlphaArgs(alpF));
                                ShowContinueError("Must be of type \"Material\"");
                                continue;
                            }
                            block.material = Kiva::Material(m.Conductivity, m.Density, m.SpecHeat);
                            block.width = m.Thickness;
                        } else {
                            ErrorsFound = true;
                            ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF) +
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
                            ShowSevereError(cCurrentModuleObject + "=\"" + fndInput.name + "\", " + cAlphaFieldNames(alpF - 1) + " defined, but no " +
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

                kivaManager.foundationInputs.push_back(fndInput);
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
        using General::RoundSigDigits;
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
        TotOSC = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
            UtilityRoutines::VerifyName(cAlphaArgs(1), OSC, OSCNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
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
                        ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2));
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
                    ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3));
                    ErrorsFound = true;
                }
            }

            if (rNumericArgs(1) > 0.0 && !any_ne(rNumericArgs({3, 7}), 0.0) && (!OSC(OSCNum).SinusoidalConstTempCoef)) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has zeros for all coefficients.");
                ShowContinueError("...The outdoor air temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            if (rNumericArgs(1) <= 0.0 && !any_ne(rNumericArgs({3, 7}), 0.0) && (!OSC(OSCNum).SinusoidalConstTempCoef)) {
                ShowSevereError(cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" has zeros for all coefficients.");
                ShowContinueError("...The outside surface temperature for surfaces using this OtherSideCoefficients object will always be 0C.");
            }

            OSC(OSCNum).TPreviousCoef = rNumericArgs(9);

            if (!lNumericFieldBlanks(10)) {
                OSC(OSCNum).MinLimitPresent = true;
                OSC(OSCNum).MinTempLimit = rNumericArgs(10);
                cOSCLimitsString = RoundSigDigits(rNumericArgs(10), 3);
            } else {
                cOSCLimitsString = "N/A";
            }
            if (!lNumericFieldBlanks(11)) {
                OSC(OSCNum).MaxLimitPresent = true;
                OSC(OSCNum).MaxTempLimit = rNumericArgs(11);
                cOSCLimitsString += "," + RoundSigDigits(rNumericArgs(10), 3);
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
                cAlphaArgs(1) = RoundSigDigits(OSC(Loop).SurfFilmCoef, 3);
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
        TotOSCM = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        OSCM.allocate(TotOSCM);
        // OSCM is already initialized in derived type defn.

        OSCMNum = 0;
        for (Loop = 1; Loop <= TotOSCM; ++Loop) {
            inputProcessor->getObjectItem(state, cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumProps, IOStat);
            ErrorInName = false;
            IsBlank = false;
            UtilityRoutines::VerifyName(cAlphaArgs(1), OSCM, OSCMNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name");
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

            if (AnyEnergyManagementSystemInModel) {
                SetupEMSActuator("Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Convection Bulk Air Temperature",
                                 "[C]",
                                 OSCM(OSCMNum).EMSOverrideOnTConv,
                                 OSCM(OSCMNum).EMSOverrideTConvValue);
                SetupEMSActuator("Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Convection Heat Transfer Coefficient",
                                 "[W/m2-K]",
                                 OSCM(OSCMNum).EMSOverrideOnHConv,
                                 OSCM(OSCMNum).EMSOverrideHConvValue);
                SetupEMSActuator("Other Side Boundary Conditions",
                                 OSCM(OSCMNum).Name,
                                 "Radiation Effective Temperature",
                                 "[C]",
                                 OSCM(OSCMNum).EMSOverrideOnTRad,
                                 OSCM(OSCMNum).EMSOverrideTRadValue);
                SetupEMSActuator("Other Side Boundary Conditions",
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
        using General::RoundSigDigits;
        using General::TrimSigDigits;
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
        NMatInsul = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
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
            SurfNum = UtilityRoutines::FindItemInList(cAlphaArgs(2), SurfaceTmp, TotSurfaces);
            MaterNum = UtilityRoutines::FindItemInList(cAlphaArgs(3), dataMaterial.Material, TotMaterials);
            SchNum = GetScheduleIndex(state, cAlphaArgs(4));
            if (UtilityRoutines::SameString(cAlphaArgs(1), "Outside")) {
                InslType = 1;
            } else if (UtilityRoutines::SameString(cAlphaArgs(1), "Inside")) {
                InslType = 2;
            } else {
                InslType = 0;
                ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                ShowContinueError(" invalid " + cAlphaFieldNames(1) + "=\"" + cAlphaArgs(1) + "\", [should be Inside or Outside]");
                ErrorsFound = true;
            }
            if (SurfNum == 0) {
                ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                ShowContinueError(" invalid (not found) " + cAlphaFieldNames(2));
                ErrorsFound = true;
            } else {
                if (MaterNum == 0) {
                    ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                    ShowContinueError(" invalid (not found) " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\"");
                    ErrorsFound = true;
                } else {
                    int const MaterialLayerGroup = dataMaterial.Material(MaterNum).Group;
                    if ((MaterialLayerGroup == WindowSimpleGlazing) || (MaterialLayerGroup == ShadeEquivalentLayer) ||
                        (MaterialLayerGroup == DrapeEquivalentLayer) || (MaterialLayerGroup == BlindEquivalentLayer) ||
                        (MaterialLayerGroup == ScreenEquivalentLayer) || (MaterialLayerGroup == GapEquivalentLayer)) {
                        ShowSevereError("Invalid movable insulation material for " + cCurrentModuleObject + ":");
                        ShowSevereError("...Movable insulation material type specified = " + DataHeatBalance::cMaterialGroupType(MaterialLayerGroup));
                        ShowSevereError("...Movable insulation material name specified = " + cAlphaArgs(3));
                        ErrorsFound = true;
                    }
                    if (SchNum == 0) {
                        ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\", invalid data.");
                        ShowContinueError(" invalid (not found) " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\"");
                        ErrorsFound = true;
                    } else {
                        {
                            auto const SELECT_CASE_var(InslType);
                            if (SELECT_CASE_var == 1) {
                                if (SurfaceTmp(SurfNum).MaterialMovInsulExt > 0) {
                                    ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                    "\", already assigned.");
                                    ShowContinueError("\"Outside\", was already assigned Material=\"" +
                                                      dataMaterial.Material(SurfaceTmp(SurfNum).MaterialMovInsulInt).Name + "\".");
                                    ShowContinueError("attempting to assign Material=\"" + dataMaterial.Material(MaterNum).Name + "\".");
                                    ErrorsFound = true;
                                }
                                SurfaceTmp(SurfNum).MaterialMovInsulExt = MaterNum;
                                SurfaceTmp(SurfNum).SchedMovInsulExt = SchNum;
                                if (dataMaterial.Material(MaterNum).Resistance <= 0.0) {
                                    if (dataMaterial.Material(MaterNum).Conductivity <= 0.0 || dataMaterial.Material(MaterNum).Thickness <= 0.0) {
                                        ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError("\"Outside\", invalid material for movable insulation.");
                                        ShowContinueError("Material=\"" + dataMaterial.Material(MaterNum).Name + "\",Resistance=[" +
                                                          RoundSigDigits(dataMaterial.Material(MaterNum).Resistance, 3) +
                                                          "], must be > 0 for use in Movable Insulation.");
                                        ErrorsFound = true;
                                    } else if (dataMaterial.Material(MaterNum).Conductivity > 0.0) {
                                        dataMaterial.Material(MaterNum).Resistance = dataMaterial.Material(MaterNum).Thickness / dataMaterial.Material(MaterNum).Conductivity;
                                    }
                                }
                                if (dataMaterial.Material(MaterNum).Conductivity <= 0.0) {
                                    if (dataMaterial.Material(MaterNum).Resistance <= 0.0) {
                                        ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError("\"Outside\", invalid material for movable insulation.");
                                        ShowContinueError("Material=\"" + dataMaterial.Material(MaterNum).Name + "\",Conductivity=[" +
                                                          RoundSigDigits(dataMaterial.Material(MaterNum).Conductivity, 3) +
                                                          "], must be > 0 for use in Movable Insulation.");
                                        ErrorsFound = true;
                                    }
                                }
                            } else if (SELECT_CASE_var == 2) {
                                if (SurfaceTmp(SurfNum).MaterialMovInsulInt > 0) {
                                    ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                    "\", already assigned.");
                                    ShowContinueError("\"Inside\", was already assigned Material=\"" +
                                                      dataMaterial.Material(SurfaceTmp(SurfNum).MaterialMovInsulInt).Name + "\".");
                                    ShowContinueError("attempting to assign Material=\"" + dataMaterial.Material(MaterNum).Name + "\".");
                                    ErrorsFound = true;
                                }
                                SurfaceTmp(SurfNum).MaterialMovInsulInt = MaterNum;
                                SurfaceTmp(SurfNum).SchedMovInsulInt = SchNum;
                                if (dataMaterial.Material(MaterNum).Resistance <= 0.0) {
                                    if (dataMaterial.Material(MaterNum).Conductivity <= 0.0 || dataMaterial.Material(MaterNum).Thickness <= 0.0) {
                                        ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) +
                                                        "\", invalid material.");
                                        ShowContinueError("\"Inside\", invalid material for movable insulation.");
                                        ShowContinueError("Material=\"" + dataMaterial.Material(MaterNum).Name + "\",Resistance=[" +
                                                          RoundSigDigits(dataMaterial.Material(MaterNum).Resistance, 3) +
                                                          "], must be > 0 for use in Movable Insulation.");
                                        ErrorsFound = true;
                                    } else if (dataMaterial.Material(MaterNum).Conductivity > 0.0) {
                                        dataMaterial.Material(MaterNum).Resistance = dataMaterial.Material(MaterNum).Thickness / dataMaterial.Material(MaterNum).Conductivity;
                                    }
                                }
                            } else {
                            }
                        }
                        if (SurfaceTmp(SurfNum).Class == SurfaceClass::Window) {
                            ShowSevereError(cCurrentModuleObject + ", " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\"");
                            ShowContinueError("invalid use on a Window. Use WindowShadingControl instead.");
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
        using General::RoundSigDigits;

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
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {

            if (!Zone(ZoneNum).HasFloor) {
                ShowWarningError("No floor exists in Zone=\"" + Zone(ZoneNum).Name +
                                 "\", zone floor area is zero. All values for this zone that are entered per floor area will be zero.");
            }

            SumAreas = 0.0;
            SurfCount = 0.0;
            NFaces = Zone(ZoneNum).SurfaceLast - Zone(ZoneNum).SurfaceFirst + 1;
            notused = 0;
            ZoneStruct.NumSurfaceFaces = NFaces;
            ZoneStruct.SurfaceFace.allocate(NFaces);
            NActFaces = 0;
            surfacenotused.dimension(NFaces, 0);

            for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {

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
            } else if (Zone(ZoneNum).Volume == DataGlobalConstants::AutoCalculate()) { // no user entered zone volume
                ShowSevereError("For zone: " + Zone(ZoneNum).Name +
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
                if (DisplayExtraWarnings) { // report missing
                    ShowWarningError(
                        "CalculateZoneVolume: The Zone=\"" + Zone(ZoneNum).Name +
                        "\" is not fully enclosed. To be fully enclosed, each edge of a surface must also be an edge on one other surface.");
                    switch (volCalcMethod) {
                    case zoneVolumeCalculationMethod::floorAreaTimesHeight1:
                        ShowContinueError("  The zone volume was calculated using the floor area times ceiling height method where the floor and "
                                          "ceiling are the same except for the z-coordinates.");
                        break;
                    case zoneVolumeCalculationMethod::floorAreaTimesHeight2:
                        ShowContinueError("  The zone volume was calculated using the floor area times ceiling height method where the floor is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case zoneVolumeCalculationMethod::ceilingAreaTimesHeight:
                        ShowContinueError("  The zone volume was calculated using the ceiling area times ceiling height method where the ceiling is "
                                          "horizontal, the walls are vertical, and the wall heights are all the same.");
                        break;
                    case zoneVolumeCalculationMethod::opWallAreaTimesDistance:
                        ShowContinueError("  The zone volume was calculated using the opposite wall area times the distance between them method ");
                        break;
                    case zoneVolumeCalculationMethod::userProvided:
                        ShowContinueError("  The zone volume was provided as an input to the ZONE object ");
                        break;
                    case zoneVolumeCalculationMethod::error:
                        ShowContinueError("  The zone volume was not calculated and an error exists. ");
                        break;
                    case zoneVolumeCalculationMethod::enclosed: // should not be called but completes enumeration
                        ShowContinueError("  The zone volume was calculated using multiple pyramids and was fully enclosed. ");
                        break;
                    }
                    for (auto edge : listOfedgeNotUsedTwice) {
                        ShowContinueError("  The surface    \"" + Surface(edge.surfNum).Name +
                                          "\" has an edge that is either not an edge on another surface or is an edge on three or more surfaces: ");
                        ShowContinueError("    Vertex start { " + RoundSigDigits(edge.start.x, 4) + ", " + RoundSigDigits(edge.start.y, 4) + ", " +
                                          RoundSigDigits(edge.start.z, 4) + "}");
                        ShowContinueError("    Vertex end   { " + RoundSigDigits(edge.end.x, 4) + ", " + RoundSigDigits(edge.end.y, 4) + ", " +
                                          RoundSigDigits(edge.end.z, 4) + "}");
                    }
                }
            }
            if (Zone(ZoneNum).Volume > 0.0) { // User entered zone volume, produce message if not near calculated
                if (CalcVolume > 0.0) {
                    if (std::abs(CalcVolume - Zone(ZoneNum).Volume) / Zone(ZoneNum).Volume > 0.05) {
                        ++ErrCount;
                        if (ErrCount == 1 && !DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage("Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            ShowWarningError("Entered Zone Volumes differ from calculated zone volume(s).");
                            ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
                        }
                        if (DisplayExtraWarnings) {
                            if (initmsg) {
                                ShowMessage("Note that the following warning(s) may/will occur if you have not enclosed your zone completely.");
                                initmsg = false;
                            }
                            // Warn user of using specified Zone Volume
                            ShowWarningError("Entered Volume entered for Zone=\"" + Zone(ZoneNum).Name +
                                             "\" significantly different from calculated Volume");
                            ShowContinueError("Entered Zone Volume value=" + RoundSigDigits(Zone(ZoneNum).Volume, 2) +
                                              ", Calculated Zone Volume value=" + RoundSigDigits(CalcVolume, 2) +
                                              ", entered volume will be used in calculations.");
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
                ShowWarningError("Indicated Zone Volume <= 0.0 for Zone=" + Zone(ZoneNum).Name);
                ShowContinueError("The calculated Zone Volume was=" + RoundSigDigits(Zone(ZoneNum).Volume, 2));
                ShowContinueError("The simulation will continue with the Zone Volume set to 10.0 m3. ");
                ShowContinueError("...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.");
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
        if (!DisplayExtraWarnings) {
            if (countNotFullyEnclosedZones == 1) {
                ShowWarningError(
                    "CalculateZoneVolume: 1 zone is not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; ");
            } else if (countNotFullyEnclosedZones > 1) {
                ShowWarningError("CalculateZoneVolume: " + RoundSigDigits(countNotFullyEnclosedZones) +
                                 " zones are not fully enclosed. For more details use:  Output:Diagnostics,DisplayExtrawarnings; ");
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

        using General::TrimSigDigits;
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

        if (ProcessSurfaceVerticesOneTimeFlag) {
            Xpsv.allocate(MaxVerticesPerSurface);
            Ypsv.allocate(MaxVerticesPerSurface);
            Zpsv.allocate(MaxVerticesPerSurface);
            Xpsv = 0.0;
            Ypsv = 0.0;
            Zpsv = 0.0;
            ProcessSurfaceVerticesOneTimeFlag = false;
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
                    ShowSevereError(RoutineName + "Suspected non-planar surface:\"" + Surface(ThisSurf).Name +
                                    "\", Max \"out of line\"=" + TrimSigDigits(OutOfLine, 5) + " at Vertex # " + TrimSigDigits(LastVertexInError));
                } else {
                    ShowWarningError(RoutineName + "Possible non-planar surface:\"" + Surface(ThisSurf).Name +
                                     "\", Max \"out of line\"=" + TrimSigDigits(OutOfLine, 5) + " at Vertex # " + TrimSigDigits(LastVertexInError));
                }
                //       ErrorInSurface=.TRUE.
            }
        }

        if (BaseSurface) {
            SurfWorldAz = Surface(ThisSurf).Azimuth;
            SurfTilt = Surface(ThisSurf).Tilt;
            for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
                Xpsv(n) = Surface(ThisSurf).Vertex(n).x;
                Ypsv(n) = Surface(ThisSurf).Vertex(n).y;
                Zpsv(n) = Surface(ThisSurf).Vertex(n).z;
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
            Real64 const BaseCosAzimuth = std::cos(baseSurfWorldAz * DataGlobalConstants::DegToRadians());
            Real64 const BaseSinAzimuth = std::sin(baseSurfWorldAz * DataGlobalConstants::DegToRadians());
            Real64 const BaseCosTilt = std::cos(baseSurfTilt * DataGlobalConstants::DegToRadians());
            Real64 const BaseSinTilt = std::sin(baseSurfTilt * DataGlobalConstants::DegToRadians());
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
                        ShowSevereError(RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface(ThisSurf).Name + "\".");
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

                        if (DisplayExtraWarnings) {
                            ShowWarningError(RoutineName + "Suspected 4-sided but non-rectangular Window, Door or GlassDoor:");
                            ShowContinueError("Surface=" + Surface(ThisSurf).Name +
                                              " is transformed into an equivalent rectangular surface with the same area and aspect ratio. ");
                        }
                    }

                    Xpsv(1) = XLLC;
                    Xpsv(2) = XLLC;
                    Xpsv(3) = XLLC + Surface(ThisSurf).Width;
                    Xpsv(4) = XLLC + Surface(ThisSurf).Width;
                    Ypsv(1) = YLLC + Surface(ThisSurf).Height;
                    Ypsv(4) = YLLC + Surface(ThisSurf).Height;
                    Ypsv(2) = YLLC;
                    Ypsv(3) = YLLC;
                    Zpsv(1) = ZLLC;
                    Zpsv(2) = ZLLC;
                    Zpsv(3) = ZLLC;
                    Zpsv(4) = ZLLC;

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
                                ShowSevereError(RoutineName + "Base Surface=\"" + Surface(Surface(ThisSurf).BaseSurf).Name + "\", ");
                                ShowContinueError("Window Surface=\"" + Surface(ThisSurf).Name +
                                                  "\" area (with frame) is too large to fit on the surface.");
                                ShowContinueError("Base surface area (-windows and doors)=[" +
                                                  TrimSigDigits(Surface(Surface(ThisSurf).BaseSurf).Area, 2) + "] m2, frame area=[" +
                                                  TrimSigDigits(SurfWinFrameArea(ThisSurf), 2) + "] m2.");
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
                                ShowSevereError(RoutineName + "Divider area exceeds glazed opening for window " + Surface(ThisSurf).Name);
                                ShowContinueError("Window surface area=[" + TrimSigDigits(Surface(ThisSurf).Area, 2) + "] m2, divider area=[" +
                                                  TrimSigDigits(SurfWinDividerArea(ThisSurf), 2) + "] m2.");
                                ErrorInSurface = true;
                            }
                            Surface(ThisSurf).Area -= SurfWinDividerArea(ThisSurf); // Glazed area
                            if (DivArea <= 0.0) {
                                ShowWarningError(RoutineName + "Calculated Divider Area <= 0.0 for Window=" + Surface(ThisSurf).Name);
                                if (FrameDivider(FrDivNum).HorDividers == 0) {
                                    ShowContinueError("..Number of Horizontal Dividers = 0.");
                                }
                                if (FrameDivider(FrDivNum).VertDividers == 0) {
                                    ShowContinueError("..Number of Vertical Dividers = 0.");
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
                        ShowSevereError(RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface(ThisSurf).Name + "\".");
                        ErrorInSurface = true;
                    }
                    ThisReveal = -Pt2Plane(Surface(ThisSurf).Vertex(2), BasePlane);
                    if (std::abs(ThisReveal) < 0.0002) ThisReveal = 0.0;
                    Surface(ThisSurf).Reveal = ThisReveal;
                    Xp = Surface(ThisSurf).Vertex(2).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(2).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(2).z - BaseZLLC;
                    Xpsv(2) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    Ypsv(2) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    Zpsv(2) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
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
                    Xpsv(1) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    Ypsv(1) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    Zpsv(1) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

                    Xp = Surface(ThisSurf).Vertex(3).x - BaseXLLC;
                    Yp = Surface(ThisSurf).Vertex(3).y - BaseYLLC;
                    Zp = Surface(ThisSurf).Vertex(3).z - BaseZLLC;
                    Xpsv(3) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
                    Ypsv(3) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
                    Zpsv(3) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

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
                    Xpsv(1) = XLLC;
                    Xpsv(2) = XLLC;
                    Xpsv(3) = XLLC + Surface(ThisSurf).Width;
                    Xpsv(4) = XLLC + Surface(ThisSurf).Width;
                    Ypsv(1) = YLLC;
                    Ypsv(2) = YLLC;
                    Ypsv(3) = YLLC;
                    Ypsv(4) = YLLC;
                    Zpsv(1) = Surface(ThisSurf).Height;
                    Zpsv(4) = Surface(ThisSurf).Height;
                    Zpsv(2) = 0.0;
                    Zpsv(3) = 0.0;

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
                    Xpsv(1) = XLLC;
                    Xpsv(2) = XLLC;
                    Xpsv(3) = XLLC;
                    Xpsv(4) = XLLC;
                    Ypsv(1) = YLLC;
                    Ypsv(2) = YLLC;
                    Ypsv(3) = YLLC + Surface(ThisSurf).Width;
                    Ypsv(4) = YLLC + Surface(ThisSurf).Width;
                    Zpsv(1) = Surface(ThisSurf).Height;
                    Zpsv(4) = Surface(ThisSurf).Height;
                    Zpsv(2) = 0.0;
                    Zpsv(3) = 0.0;

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
                    Xpsv(1) = XLLC;
                    Xpsv(2) = XLLC;
                    Xpsv(3) = XLLC;
                    Xpsv(4) = XLLC;
                    Ypsv(1) = YLLC + Surface(ThisSurf).Width;
                    Ypsv(2) = YLLC + Surface(ThisSurf).Width;
                    Ypsv(3) = YLLC;
                    Ypsv(4) = YLLC;
                    Zpsv(1) = Surface(ThisSurf).Height;
                    Zpsv(4) = Surface(ThisSurf).Height;
                    Zpsv(2) = 0.0;
                    Zpsv(3) = 0.0;

                } else {
                    // Error Condition
                    ShowSevereError(RoutineName + "Incorrect surface shape number.", OptionalOutputFileRef{state.files.eso});
                    ShowContinueError("Please notify EnergyPlus support of this error and send input file.");
                    ErrorInSurface = true;
                }
            }

            for (n = 1; n <= Surface(ThisSurf).Sides; ++n) {
                // if less than 1/10 inch
                Xpsv(n) = nint64(10000.0 * Xpsv(n)) / 10000.0;
                if (std::abs(Xpsv(n)) < 0.0025) Xpsv(n) = 0.0;
                Ypsv(n) = nint64(10000.0 * Ypsv(n)) / 10000.0;
                if (std::abs(Ypsv(n)) < 0.0025) Ypsv(n) = 0.0;
                Zpsv(n) = nint64(10000.0 * Zpsv(n)) / 10000.0;
                if (std::abs(Zpsv(n)) < 0.0025) Zpsv(n) = 0.0;
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
            ShadeV(ThisSurf).XV(n) = Xpsv(n);
            ShadeV(ThisSurf).YV(n) = Ypsv(n);
            ShadeV(ThisSurf).ZV(n) = Zpsv(n);
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
                X1 = Xpsv(2) - CoordinateTransVector.x;
                Y1 = Ypsv(2) - CoordinateTransVector.y;
                Z1 = Zpsv(2) - CoordinateTransVector.z;
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
                ShowFatalError(RoutineName + "Developer error for Subsurface=" + Surface(ThisSurf).Name);
                ShowContinueError("Base surface=" + Surface(ThisBaseSurface).Name + " vertices must be processed before any subsurfaces.");
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
            ShowSevereError("CalcCoordinateTransformation: Invalid dot product, surface=\"" + Surface(SurfNum).Name + "\":");
            for (I = 1; I <= Surface(SurfNum).Sides; ++I) {
                auto const &point{Surface(SurfNum).Vertex(I)};
                ShowContinueError(format(" ({:8.3F},{:8.3F},{:8.3F})", point.x, point.y, point.z));
            }
            ShowFatalError("CalcCoordinateTransformation: Program terminates due to preceding condition.", OptionalOutputFileRef{state.files.eso});
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

        ShDevName = dataMaterial.Material(ShDevNum).Name;
        ConstrNum = SurfaceTmp(SurfNum).Construction;
        ConstrName = state.dataConstruction->Construct(ConstrNum).Name;
        if (WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorShade || WindowShadingControl(WSCPtr).ShadingType == WSC_ST_InteriorBlind) {
            ConstrNameSh = ConstrName + ':' + ShDevName + ":INT";
        } else {
            ConstrNameSh = ConstrName + ':' + ShDevName + ":EXT";
        }

        // If this construction name already exists, set the surface's shaded construction number to it

        ConstrNewSh = UtilityRoutines::FindItemInList(ConstrNameSh, state.dataConstruction->Construct);

        if (ConstrNewSh > 0) {
            SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNewSh; //set the active to the current for now
        } else {

            // Create new construction

            ConstrNewSh = TotConstructs + 1;
            SurfaceTmp(SurfNum).shadedConstructionList[shadeControlIndex] = ConstrNewSh;
            SurfaceTmp(SurfNum).activeShadedConstruction = ConstrNewSh; //set the active to the current for now
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
                state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpSolar = dataMaterial.Material(ShDevNum).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpSolar = dataMaterial.Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1)).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpThermal = dataMaterial.Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1)).AbsorpThermalFront;
            } else {
                // Exterior shading device
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint(1) = ShDevNum;
                state.dataConstruction->Construct(ConstrNewSh).LayerPoint({2, TotLayersNew}) = state.dataConstruction->Construct(ConstrNum).LayerPoint({1, TotLayersOld});
                state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpSolar = dataMaterial.Material(state.dataConstruction->Construct(ConstrNewSh).LayerPoint(TotLayersNew)).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpSolar = dataMaterial.Material(ShDevNum).AbsorpSolar;
                state.dataConstruction->Construct(ConstrNewSh).OutsideAbsorpThermal = dataMaterial.Material(ShDevNum).AbsorpThermalFront;
            }
            // The following InsideAbsorpThermal applies only to inside glass; it is corrected
            //  later in InitGlassOpticalCalculations if construction has inside shade or blind.
            state.dataConstruction->Construct(ConstrNewSh).InsideAbsorpThermal = dataMaterial.Material(state.dataConstruction->Construct(ConstrNum).LayerPoint(TotLayersOld)).AbsorpThermalBack;
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
        DisplayString("Creating Storm Window Constructions");

        for (int StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum) {
            int SurfNum = StormWindow(StormWinNum).BaseWindowNum; // Surface number
            int ConstrNum = Surface(SurfNum).Construction; // Number of unshaded construction
            // Fatal error if base construction has more than three glass layers
            if (state.dataConstruction->Construct(ConstrNum).TotGlassLayers > 3) {
                ShowFatalError("Window=" + Surface(SurfNum).Name + " has more than 3 glass layers; a storm window cannot be applied.");
            }

            // create unshaded construction with storm window
            const auto ChrNum = fmt::to_string(StormWinNum);
            std::string ConstrNameSt = "BARECONSTRUCTIONWITHSTORMWIN:" + ChrNum; // Name of unshaded construction with storm window
            // If this construction name already exists, set the surface's storm window construction number to it
            int ConstrNewSt = UtilityRoutines::FindItemInList(ConstrNameSt, state.dataConstruction->Construct, TotConstructs); // Number of unshaded storm window construction that is created
            // If necessary, create new material corresponding to the air layer between the storm winddow and the rest of the window
            int MatNewStAir = createAirMaterialFromDistance(StormWindow(StormWinNum).StormWinDistance, "AIR:STORMWIN:");
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
                if (state.dataConstruction->Construct(curConstruction).TotGlassLayers <= 3 && (dataMaterial.Material(MatIntSh).Group == Shade || dataMaterial.Material(MatIntSh).Group == WindowBlind)) ShAndSt = true;
                if (MatBetweenGlassSh > 0) {
                    if (dataMaterial.Material(MatBetweenGlassSh).Group == Shade || dataMaterial.Material(MatBetweenGlassSh).Group == WindowBlind) {
                        ShAndSt = true;
                    } else {
                        ShowContinueError("Window=" + Surface(SurfNum).Name + " has a shaded construction to which a storm window cannot be applied.");
                        ShowContinueError("Storm windows can only be applied to shaded constructions that:");
                        ShowContinueError("have an interior shade or blind and up to three glass layers, or");
                        ShowContinueError("have a between-glass shade or blind and two glass layers.");
                        ShowFatalError("EnergyPlus is exiting due to reason stated above.");
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

    int createAirMaterialFromDistance(Real64 distance, std::string namePrefix)
    {
        int mmDistance = int(1000 * distance); // Thickness of air gap in mm (usually between storm window and rest of window)
        std::string MatNameStAir = namePrefix + fmt::to_string(mmDistance) + "MM";   // Name of created air layer material
        int newAirMaterial = UtilityRoutines::FindItemInList(MatNameStAir, dataMaterial.Material, TotMaterials);
        if (newAirMaterial == 0) {
            // Create new material
            TotMaterials = TotMaterials + 1;
            newAirMaterial = TotMaterials;
            dataMaterial.Material.redimension(TotMaterials);
            NominalR.redimension(TotMaterials);
            dataMaterial.Material(TotMaterials).Name = MatNameStAir;
            dataMaterial.Material(TotMaterials).Group = WindowGas;
            dataMaterial.Material(TotMaterials).Roughness = 3;
            dataMaterial.Material(TotMaterials).Conductivity = 0.0;
            dataMaterial.Material(TotMaterials).Density = 0.0;
            dataMaterial.Material(TotMaterials).IsoMoistCap = 0.0;
            dataMaterial.Material(TotMaterials).Porosity = 0.0;
            dataMaterial.Material(TotMaterials).Resistance = 0.0;
            dataMaterial.Material(TotMaterials).SpecHeat = 0.0;
            dataMaterial.Material(TotMaterials).ThermGradCoef = 0.0;
            dataMaterial.Material(TotMaterials).Thickness = distance;
            dataMaterial.Material(TotMaterials).VaporDiffus = 0.0;
            dataMaterial.Material(TotMaterials).GasType = 0;
            dataMaterial.Material(TotMaterials).GasCon = 0.0;
            dataMaterial.Material(TotMaterials).GasVis = 0.0;
            dataMaterial.Material(TotMaterials).GasCp = 0.0;
            dataMaterial.Material(TotMaterials).GasWght = 0.0;
            dataMaterial.Material(TotMaterials).GasFract = 0.0;
            dataMaterial.Material(TotMaterials).GasType(1) = 1;
            dataMaterial.Material(TotMaterials).GlassSpectralDataPtr = 0;
            dataMaterial.Material(TotMaterials).NumberOfGasesInMixture = 1;
            dataMaterial.Material(TotMaterials).GasCon(1, 1) = 2.873e-3;
            dataMaterial.Material(TotMaterials).GasCon(2, 1) = 7.760e-5;
            dataMaterial.Material(TotMaterials).GasVis(1, 1) = 3.723e-6;
            dataMaterial.Material(TotMaterials).GasVis(2, 1) = 4.940e-8;
            dataMaterial.Material(TotMaterials).GasCp(1, 1) = 1002.737;
            dataMaterial.Material(TotMaterials).GasCp(2, 1) = 1.2324e-2;
            dataMaterial.Material(TotMaterials).GasWght(1) = 28.97;
            dataMaterial.Material(TotMaterials).GasFract(1) = 1.0;
            dataMaterial.Material(TotMaterials).AbsorpSolar = 0.0;
            dataMaterial.Material(TotMaterials).AbsorpThermal = 0.0;
            dataMaterial.Material(TotMaterials).AbsorpVisible = 0.0;
            dataMaterial.Material(TotMaterials).Trans = 0.0;
            dataMaterial.Material(TotMaterials).TransVis = 0.0;
            dataMaterial.Material(TotMaterials).GlassTransDirtFactor = 0.0;
            dataMaterial.Material(TotMaterials).ReflectShade = 0.0;
            dataMaterial.Material(TotMaterials).ReflectShadeVis = 0.0;
            dataMaterial.Material(TotMaterials).AbsorpThermalBack = 0.0;
            dataMaterial.Material(TotMaterials).AbsorpThermalFront = 0.0;
            dataMaterial.Material(TotMaterials).ReflectSolBeamBack = 0.0;
            dataMaterial.Material(TotMaterials).ReflectSolBeamFront = 0.0;
            dataMaterial.Material(TotMaterials).ReflectSolDiffBack = 0.0;
            dataMaterial.Material(TotMaterials).ReflectSolDiffFront = 0.0;
            dataMaterial.Material(TotMaterials).ReflectVisBeamBack = 0.0;
            dataMaterial.Material(TotMaterials).ReflectVisBeamFront = 0.0;
            dataMaterial.Material(TotMaterials).ReflectVisDiffBack = 0.0;
            dataMaterial.Material(TotMaterials).ReflectVisDiffFront = 0.0;
            dataMaterial.Material(TotMaterials).TransSolBeam = 0.0;
            dataMaterial.Material(TotMaterials).TransThermal = 0.0;
            dataMaterial.Material(TotMaterials).TransVisBeam = 0.0;
            dataMaterial.Material(TotMaterials).BlindDataPtr = 0;
            dataMaterial.Material(TotMaterials).WinShadeToGlassDist = 0.0;
            dataMaterial.Material(TotMaterials).WinShadeTopOpeningMult = 0.0;
            dataMaterial.Material(TotMaterials).WinShadeBottomOpeningMult = 0.0;
            dataMaterial.Material(TotMaterials).WinShadeLeftOpeningMult = 0.0;
            dataMaterial.Material(TotMaterials).WinShadeRightOpeningMult = 0.0;
            dataMaterial.Material(TotMaterials).WinShadeAirFlowPermeability = 0.0;
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
            state.dataConstruction->Construct(TotConstructs).OutsideAbsorpThermal = dataMaterial.Material(stormMaterial).AbsorpThermalFront;
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
        using General::RoundSigDigits;
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

        IConst = SurfaceTmp(SurfNum).Construction;

        // Height and width of original window
        TVect = SurfaceTmp(SurfNum).Vertex(3) - SurfaceTmp(SurfNum).Vertex(2);
        W = VecLength(TVect); // SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
        TVect = SurfaceTmp(SurfNum).Vertex(2) - SurfaceTmp(SurfNum).Vertex(1);
        H = VecLength(TVect); // SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord.Vertex({1, SurfaceTmp(SurfNum).Sides}) = SurfaceTmp(SurfNum).Vertex({1, SurfaceTmp(SurfNum).Sides});

        // Height and width of first glazing system
        h1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysHeight;
        w1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysWidth;

        Const2Name = state.dataConstruction->Construct(IConst).Name + ":2";
        IConst2 = UtilityRoutines::FindItemInList(Const2Name, state.dataConstruction->Construct);

        if (IConst2 == 0) { // Only one glazing system on Window5 Data File for this window.

            // So... original dimensions and area of window are used (entered in IDF)
            // Warning if dimensions of original window differ from those on Data File by more than 10%

            if (std::abs((H - h1) / H) > 0.10 || std::abs((W - w1) / W) > 0.10) {

                if (DisplayExtraWarnings) {
                    ShowWarningError("SurfaceGeometry: ModifyWindow: Window " + SurfaceTmp(SurfNum).Name +
                                     " uses the Window5 Data File Construction " + state.dataConstruction->Construct(IConst).Name);
                    ShowContinueError("The height " + RoundSigDigits(H, 3) + "(m) or width " + RoundSigDigits(W, 3) +
                                      " (m) of this window differs by more than 10%");
                    ShowContinueError("from the corresponding height " + RoundSigDigits(h1, 3) + " (m) or width " + RoundSigDigits(w1, 3) +
                                      " (m) on the Window5 Data file.");
                    ShowContinueError("This will affect the frame heat transfer calculation if the frame in the Data File entry");
                    ShowContinueError("is not uniform, i.e., has sections with different geometry and/or thermal properties.");
                } else {
                    ++Warning1Count;
                }
            }

            // Calculate net area for base surface
            SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area -= SurfaceTmp(SurfNum).Area;
            if (SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
                ShowSevereError("Subsurfaces have too much area for base surface=" + SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Name);
                ShowContinueError("Subsurface creating error=" + SurfaceTmp(SurfNum).Name);
                ErrorsFound = true;
            }

            // Net area of base surface with unity window multipliers (used in shadowing checks)
            SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= SurfaceTmp(SurfNum).Area / SurfaceTmp(SurfNum).Multiplier;

        } else { // Two glazing systems on Window5 data file for this window

            // if exterior window, okay.

            if (SurfaceTmp(SurfNum).ExtBoundCond == ExternalEnvironment) {
                // There are two glazing systems (separated by a vertical or horizontal mullion) on the Window5 Data File.
                // Fill in geometry data for the second window (corresponding to the second glazing system on the data file.
                // The first glazing system is assumed to be at left for vertical mullion, at bottom for horizontal mullion.
                // The second glazing system is assumed to be at right for vertical mullion, at top for horizontal mullion.
                // The lower left-hand corner of the original window (its vertex #2) is assumed to coincide with
                // vertex #2 of the first glazing system.

                if (DisplayExtraWarnings) {
                    ShowMessage("SurfaceGeometry: ModifyWindow: Window " + SurfaceTmp(SurfNum).Name +
                                " has been replaced with the Window 5/6 two glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                    ShowContinueError("Note that originally entered dimensions are overridden.");
                } else {
                    ++Warning2Count;
                }

                // Allocate another window
                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else if (SurfaceTmp(SurfNum).ExtBoundCond > 0) { // Interior window, specified  ! not external environment

                if (DisplayExtraWarnings) {
                    ShowWarningError("SurfaceGeometry: ModifyWindow: Interior Window " + SurfaceTmp(SurfNum).Name +
                                     " has been replaced with the Window 5/6 two glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                    ShowContinueError(
                        "Please check to make sure interior window is correct. Note that originally entered dimensions are overridden.");
                } else {
                    ++Warning3Count;
                }

                AddWindow(state, SurfNum, ErrorsFound, AddedSubSurfaces);

            } else { // Interior window, specified not entered

                ShowSevereError("SurfaceGeometry: ModifyWindow: Interior Window " + SurfaceTmp(SurfNum).Name + " is a window in an adjacent zone.");
                ShowContinueError("Attempted to add/reverse Window 5/6 multiple glazing system=\"" + state.dataConstruction->Construct(IConst).Name + "\".");
                ShowContinueError("Cannot use these Window 5/6 constructs for these Interior Windows. Program will terminate.");
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
        using General::RoundSigDigits;
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

        IConst = SurfaceTmp(SurfNum).Construction;

        // Height and width of original window
        TVect = SurfaceTmp(SurfNum).Vertex(3) - SurfaceTmp(SurfNum).Vertex(2);
        W = VecLength(TVect); // SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
        TVect = SurfaceTmp(SurfNum).Vertex(2) - SurfaceTmp(SurfNum).Vertex(1);
        H = VecLength(TVect); // SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

        // Save coordinates of original window in case Window 5 data overwrites.
        OriginalCoord.Vertex({1, SurfaceTmp(SurfNum).Sides}) = SurfaceTmp(SurfNum).Vertex({1, SurfaceTmp(SurfNum).Sides});

        // Height and width of first glazing system
        h1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysHeight;
        w1 = state.dataConstruction->Construct(IConst).W5FileGlazingSysWidth;

        Const2Name = state.dataConstruction->Construct(IConst).Name + ":2";
        IConst2 = UtilityRoutines::FindItemInList(Const2Name, state.dataConstruction->Construct);

        ++AddedSubSurfaces;
        SurfaceTmp.redimension(++TotSurfaces);

        SurfaceTmp(TotSurfaces).Vertex.allocate(4);

        SurfaceTmp(TotSurfaces).Name = SurfaceTmp(SurfNum).Name + ":2";
        SurfaceTmp(TotSurfaces).Construction = IConst2;
        SurfaceTmp(TotSurfaces).ConstructionStoredInputValue = IConst2;
        SurfaceTmp(TotSurfaces).Class = SurfaceTmp(SurfNum).Class;
        SurfaceTmp(TotSurfaces).Azimuth = SurfaceTmp(SurfNum).Azimuth;
        // Sine and cosine of azimuth and tilt
        SurfaceTmp(TotSurfaces).SinAzim = SurfaceTmp(SurfNum).SinAzim;
        SurfaceTmp(TotSurfaces).CosAzim = SurfaceTmp(SurfNum).CosAzim;
        SurfaceTmp(TotSurfaces).SinTilt = SurfaceTmp(SurfNum).SinTilt;
        SurfaceTmp(TotSurfaces).CosTilt = SurfaceTmp(SurfNum).CosTilt;
        // Outward normal unit vector (pointing away from room)
        SurfaceTmp(TotSurfaces).Centroid = SurfaceTmp(SurfNum).Centroid;
        SurfaceTmp(TotSurfaces).lcsx = SurfaceTmp(SurfNum).lcsx;
        SurfaceTmp(TotSurfaces).lcsy = SurfaceTmp(SurfNum).lcsy;
        SurfaceTmp(TotSurfaces).lcsz = SurfaceTmp(SurfNum).lcsz;
        SurfaceTmp(TotSurfaces).NewellAreaVector = SurfaceTmp(SurfNum).NewellAreaVector;
        SurfaceTmp(TotSurfaces).OutNormVec = SurfaceTmp(SurfNum).OutNormVec;
        SurfaceTmp(TotSurfaces).Reveal = SurfaceTmp(SurfNum).Reveal;
        SurfaceTmp(TotSurfaces).Shape = SurfaceTmp(SurfNum).Shape;
        SurfaceTmp(TotSurfaces).Sides = SurfaceTmp(SurfNum).Sides;
        SurfaceTmp(TotSurfaces).Tilt = SurfaceTmp(SurfNum).Tilt;
        SurfaceTmp(TotSurfaces).HeatTransSurf = SurfaceTmp(SurfNum).HeatTransSurf;
        SurfaceTmp(TotSurfaces).BaseSurfName = SurfaceTmp(SurfNum).BaseSurfName;
        SurfaceTmp(TotSurfaces).BaseSurf = SurfaceTmp(SurfNum).BaseSurf;
        SurfaceTmp(TotSurfaces).ZoneName = SurfaceTmp(SurfNum).ZoneName;
        SurfaceTmp(TotSurfaces).Zone = SurfaceTmp(SurfNum).Zone;
        SurfaceTmp(TotSurfaces).ExtBoundCondName = SurfaceTmp(SurfNum).ExtBoundCondName;
        SurfaceTmp(TotSurfaces).ExtBoundCond = SurfaceTmp(SurfNum).ExtBoundCond;
        SurfaceTmp(TotSurfaces).ExtSolar = SurfaceTmp(SurfNum).ExtSolar;
        SurfaceTmp(TotSurfaces).ExtWind = SurfaceTmp(SurfNum).ExtWind;
        SurfaceTmp(TotSurfaces).ViewFactorGround = SurfaceTmp(SurfNum).ViewFactorGround;
        SurfaceTmp(TotSurfaces).ViewFactorSky = SurfaceTmp(SurfNum).ViewFactorSky;
        SurfaceTmp(TotSurfaces).ViewFactorGroundIR = SurfaceTmp(SurfNum).ViewFactorGroundIR;
        SurfaceTmp(TotSurfaces).ViewFactorSkyIR = SurfaceTmp(SurfNum).ViewFactorSkyIR;
        SurfaceTmp(TotSurfaces).OSCPtr = SurfaceTmp(SurfNum).OSCPtr;
        SurfaceTmp(TotSurfaces).SchedShadowSurfIndex = SurfaceTmp(SurfNum).SchedShadowSurfIndex;
        SurfaceTmp(TotSurfaces).ShadowSurfSchedVaries = SurfaceTmp(SurfNum).ShadowSurfSchedVaries;
        SurfaceTmp(TotSurfaces).MaterialMovInsulExt = SurfaceTmp(SurfNum).MaterialMovInsulExt;
        SurfaceTmp(TotSurfaces).MaterialMovInsulInt = SurfaceTmp(SurfNum).MaterialMovInsulInt;
        SurfaceTmp(TotSurfaces).SchedMovInsulExt = SurfaceTmp(SurfNum).SchedMovInsulExt;
        SurfaceTmp(TotSurfaces).activeWindowShadingControl = SurfaceTmp(SurfNum).activeWindowShadingControl;
        SurfaceTmp(TotSurfaces).windowShadingControlList = SurfaceTmp(SurfNum).windowShadingControlList;
        SurfaceTmp(TotSurfaces).HasShadeControl = SurfaceTmp(SurfNum).HasShadeControl;
        SurfaceTmp(TotSurfaces).activeShadedConstruction = SurfaceTmp(SurfNum).activeShadedConstruction;
        SurfaceTmp(TotSurfaces).windowShadingControlList = SurfaceTmp(SurfNum).windowShadingControlList;
        SurfaceTmp(TotSurfaces).StormWinConstruction = SurfaceTmp(SurfNum).StormWinConstruction;
        SurfaceTmp(TotSurfaces).activeStormWinShadedConstruction = SurfaceTmp(SurfNum).activeStormWinShadedConstruction;
        SurfaceTmp(TotSurfaces).shadedStormWinConstructionList = SurfaceTmp(SurfNum).shadedStormWinConstructionList;
        SurfaceTmp(TotSurfaces).FrameDivider = SurfaceTmp(SurfNum).FrameDivider;
        SurfaceTmp(TotSurfaces).Multiplier = SurfaceTmp(SurfNum).Multiplier;
        SurfaceTmp(TotSurfaces).NetAreaShadowCalc = SurfaceTmp(SurfNum).NetAreaShadowCalc;

        MulWidth = state.dataConstruction->Construct(IConst).W5FileMullionWidth;
        w2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysWidth;
        h2 = state.dataConstruction->Construct(IConst2).W5FileGlazingSysHeight;

        // Correction to net area of base surface. Add back in the original glazing area and subtract the
        // area of the two glazing systems. Note that for Surface(SurfNum)%Class = 'Window' the effect
        // of a window multiplier is included in the glazing area. Note that frame areas are subtracted later.

        AreaNew = SurfaceTmp(SurfNum).Multiplier * (h1 * w1 + h2 * w2); // both glazing systems
        // Adjust net area for base surface
        SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area -= AreaNew;

        // Net area of base surface with unity window multipliers (used in shadowing checks)
        SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).NetAreaShadowCalc -= AreaNew / SurfaceTmp(SurfNum).Multiplier;

        // Reset area, etc. of original window
        SurfaceTmp(SurfNum).Area = SurfaceTmp(SurfNum).Multiplier * (h1 * w1);
        SurfaceTmp(SurfNum).GrossArea = SurfaceTmp(SurfNum).Area;
        SurfaceTmp(SurfNum).NetAreaShadowCalc = h1 * w1;
        SurfaceTmp(SurfNum).Perimeter = 2 * (h1 + w1);
        SurfaceTmp(SurfNum).Height = h1;
        SurfaceTmp(SurfNum).Width = w1;
        // Set area, etc. of new window
        SurfaceTmp(TotSurfaces).Area = SurfaceTmp(TotSurfaces).Multiplier * (h2 * w2);
        SurfaceTmp(TotSurfaces).GrossArea = SurfaceTmp(TotSurfaces).Area;
        SurfaceTmp(TotSurfaces).NetAreaShadowCalc = h2 * w2;
        SurfaceTmp(TotSurfaces).Perimeter = 2 * (h2 + w2);
        SurfaceTmp(TotSurfaces).Height = h2;
        SurfaceTmp(TotSurfaces).Width = w2;

        if (SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Area <= 0.0) {
            ShowSevereError("SurfaceGeometry: ModifyWindow: Subsurfaces have too much area for base surface=" +
                            SurfaceTmp(SurfaceTmp(SurfNum).BaseSurf).Name);
            ShowContinueError("Subsurface (window) creating error=" + SurfaceTmp(SurfNum).Name);
            ShowContinueError("This window has been replaced by two windows from the Window5 Data File of total area " + RoundSigDigits(AreaNew, 2) +
                              " m2");
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

            for (loop = 1; loop <= SurfaceTmp(SurfNum).Sides; ++loop) {
                SurfaceTmp(SurfNum).Vertex(loop) = NewCoord.Vertex(loop);
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

            for (loop = 1; loop <= SurfaceTmp(TotSurfaces).Sides; ++loop) {
                SurfaceTmp(TotSurfaces).Vertex(loop) = NewCoord.Vertex(loop);
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

            for (loop = 1; loop <= SurfaceTmp(SurfNum).Sides; ++loop) {
                SurfaceTmp(SurfNum).Vertex(loop) = NewCoord.Vertex(loop);
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

            for (loop = 1; loop <= SurfaceTmp(TotSurfaces).Sides; ++loop) {
                SurfaceTmp(TotSurfaces).Vertex(loop) = NewCoord.Vertex(loop);
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataIPShortCuts;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const CurrentModuleObject("GeometryTransform");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

        if (firstTime) {
            if (inputProcessor->getNumObjectsFound(CurrentModuleObject) == 1) {
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
                    ShowWarningError(CurrentModuleObject + ": invalid " + cAlphaFieldNames(1) + "=\"" + cAlphas(1) + "...ignored.");
                }
                firstTime = false;
                noTransform = false;
                AspectTransform = true;
                if (WorldCoordSystem) {
                    ShowWarningError(CurrentModuleObject + ": must use Relative Coordinate System.  Transform request ignored.");
                    noTransform = true;
                    AspectTransform = false;
                }
            } else {
                firstTime = false;
            }
        }
        if (noTransform) return;

        // check surface type.
        if (!SurfaceTmp(SurfNum).HeatTransSurf) {
            // Site Shading do not get transformed.
            if (SurfaceTmp(SurfNum).Class == SurfaceClass::Detached_F) return;
        }

        // testing method of transforming  x and y coordinates as follows

        // this works if not rotated wrt north axis ... but if it is, then trouble
        // try to first derotate it , transform by aspect and then rotate back.

        for (n = 1; n <= NSides; ++n) {
            Xo = SurfaceTmp(SurfNum).Vertex(n).x; // world coordinates.... shifted by relative north angle...
            Yo = SurfaceTmp(SurfNum).Vertex(n).y;
            // next derotate the building
            XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
            YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
            // translate
            Xtrans = XnoRot * std::sqrt(NewAspectRatio / OldAspectRatio);
            Ytrans = YnoRot * std::sqrt(OldAspectRatio / NewAspectRatio);
            // rerotate
            SurfaceTmp(SurfNum).Vertex(n).x = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

            SurfaceTmp(SurfNum).Vertex(n).y = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
        }
    }

    void CalcSurfaceCentroid()
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

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace Vectors;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Object Data
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
                        ShowWarningError("CalcSurfaceCentroid: zero area surface, for surface=" + surface.Name);
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
                        ShowWarningError("CalcSurfaceCentroid: caught problem with # of sides, for surface=" + surface.Name);
                        ShowContinueError("... number of sides must be >= 3, this surface # sides=" + RoundSigDigits(surface.Sides));
                    } else {
                        ShowWarningError("CalcSurfaceCentroid: caught problem with # of sides, for surface=#" + RoundSigDigits(ThisSurf));
                        ShowContinueError("...surface name is blank. Examine surfaces -- this may be a problem with ill-formed interzone surfaces.");
                        ShowContinueError("... number of sides must be >= 3, this surface # sides=" + RoundSigDigits(surface.Sides));
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
            ShowWarningError("CalcSurfaceCentroid: " + RoundSigDigits(negZcount) + " Surfaces have the Z coordinate < 0.");
            ShowContinueError("...in any calculations, Wind Speed will be 0.0 for these surfaces.");
            ShowContinueError("...in any calculations, Outside temperatures will be the outside temperature + " +
                              RoundSigDigits(WeatherFileTempModCoeff, 3) + " for these surfaces.");
            ShowContinueError("...that is, these surfaces will have conditions as though at ground level.");
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
        NumOfFlatPlateUnits = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
        NumPVTs = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        cCurrentModuleObject = "Generator:Photovoltaic";
        NumPVs = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
        NumOfICSUnits = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

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
            ShowFatalError(RoutineName + ": Illegal call to this function. Second argument must be 'RadiantEnclosures' or 'SolarEnclosures'");
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
                    if (!DisplayExtraWarnings) {
                        ++errorCount;
                    } else {
                        ShowSevereError(RoutineName + ": Surface=\"" + surf.Name + "\" uses Construction:AirBoundary in a non-interzone surface.");
                    }
                } else {
                    // Process air boundary - set surface properties and set up enclosures
                    // Radiant exchange
                    if (radiantSetup && constr.TypeIsAirBoundaryIRTSurface) {
                        // IRT air boundaries use CTF algorithm
                        surf.HeatTransferAlgorithm = DataSurfaces::HeatTransferModel_CTF;
                        surf.HeatTransSurf = true;
                        // Interior convection coefficient set to low H limit in ConvectionCoefficients::GetUserConvectionCoefficients
                    } else if (solarSetup && constr.TypeIsAirBoundaryInteriorWindow) {
                        // Override surface class for interior window
                        surf.Class = SurfaceClass::Window;
                        Zone(surf.Zone).HasInterZoneWindow = true;
                    } else if ((radiantSetup && constr.TypeIsAirBoundaryGroupedRadiant) || (solarSetup && constr.TypeIsAirBoundarySolar)) {
                        // Boundary is grouped - assign enclosure
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
                            DataHeatBalance::AnyAirBoundaryGroupedSolar = true;
                            thisSideEnclosureNum = Zone(surf.Zone).SolarEnclosureNum;
                            otherSideEnclosureNum = Zone(Surface(surf.ExtBoundCond).Zone).SolarEnclosureNum;
                        }
                        anyGroupedZones = true;
                        if ((thisSideEnclosureNum == 0) && (otherSideEnclosureNum == 0)) {
                            // Neither zone is assigned to an enclosure, so increment the counter and assign to both
                            ++enclosureNum;
                            auto &thisEnclosure(Enclosures(enclosureNum));
                            thisSideEnclosureNum = enclosureNum;
                            thisEnclosure.Name = RadiantOrSolar + " Enclosure " + General::RoundSigDigits(enclosureNum);
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
                        ShowSevereError(RoutineName + ": Surface=" + surf.Name + " uses Construction:AirBoundary with illegal option:");
                        if (radiantSetup) {
                            ShowContinueError("Radiant Exchange Method must be either GroupedZones or IRTSurface.");
                        } else {
                            ShowContinueError("Solar and Daylighting Method must be either GroupedZones or InteriorWindow");
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
                                               DataGlobalConstants::SecInHour();
                            DataHeatBalance::AirBoundaryMixingVol.push_back(mixingVol);
                        }
                    }
                }
            }
            if (errorCount > 0) {
                ShowSevereError(RoutineName + ": " + General::TrimSigDigits(errorCount) +
                                " surfaces use Construction:AirBoundary in non-interzone surfaces.");
                ShowContinueError("For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;");
            }
        }
        if (anyGroupedZones) {
            // All grouped zones have been assigned to an enclosure, now assign remaining zones
            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
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
            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
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
                DataViewFactorInformation::NumOfRadiantEnclosures = DataGlobals::NumOfZones;
            } else {
                DataViewFactorInformation::NumOfSolarEnclosures = DataGlobals::NumOfZones;
            }
        }
    }

    void CheckConvexity(int const SurfNum, // Current surface number
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
        using General::RoundSigDigits;
        using namespace DataErrorTracking;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        constexpr Real64 TurnThreshold(0.000001); // Sensitivity of convexity test, in radians

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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

        if (CheckConvexityFirstTime) {
            ACosZero = std::acos(0.0);
            X.allocate(MaxVerticesPerSurface + 2);
            Y.allocate(MaxVerticesPerSurface + 2);
            Z.allocate(MaxVerticesPerSurface + 2);
            A.allocate(MaxVerticesPerSurface + 2);
            B.allocate(MaxVerticesPerSurface + 2);
            SurfCollinearVerts.allocate(MaxVerticesPerSurface);
            VertSize = MaxVerticesPerSurface;
            CheckConvexityFirstTime = false;
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
            X(n) = SurfaceTmp(SurfNum).Vertex(n).x;
            Y(n) = SurfaceTmp(SurfNum).Vertex(n).y;
            Z(n) = SurfaceTmp(SurfNum).Vertex(n).z;
        }
        X(NSides + 1) = SurfaceTmp(SurfNum).Vertex(1).x;
        Y(NSides + 1) = SurfaceTmp(SurfNum).Vertex(1).y;
        Z(NSides + 1) = SurfaceTmp(SurfNum).Vertex(1).z;
        X(NSides + 2) = SurfaceTmp(SurfNum).Vertex(2).x;
        Y(NSides + 2) = SurfaceTmp(SurfNum).Vertex(2).y;
        Z(NSides + 2) = SurfaceTmp(SurfNum).Vertex(2).z;

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
                    ShowSevereError("CheckConvexity: Surface=\"" + SurfaceTmp(SurfNum).Name + "\" is non-planar.");
                    ShowContinueError("Coincident Vertices will be removed as possible.");
                    for (n = 1; n <= SurfaceTmp(SurfNum).Sides; ++n) {
                        auto const &point(SurfaceTmp(SurfNum).Vertex(n));
                        static constexpr auto ErrFmt(" ({:8.3F},{:8.3F},{:8.3F})");
                        ShowContinueError(format(ErrFmt, point.x, point.y, point.z));
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
                        if (DisplayExtraWarnings) {
                            ShowWarningError("CheckConvexity: Surface=\"" + SurfaceTmp(SurfNum).Name + "\", Collinear points have been removed.");
                        }
                        SurfCollinearWarning = true;
                    }
                    ++TotalCoincidentVertices;
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
                if (SolarDistribution != MinimalShadowing && SurfaceTmp(SurfNum).ExtSolar) {
                    if (DisplayExtraWarnings) {
                        ShowWarningError("CheckConvexity: Zone=\"" + Zone(SurfaceTmp(SurfNum).Zone).Name + "\", Surface=\"" +
                                         SurfaceTmp(SurfNum).Name + "\" is non-convex.");
                        Np1 = n + 1;
                        if (Np1 > NSides) Np1 -= NSides;
                        Np2 = n + 2;
                        if (Np2 > NSides) Np2 -= NSides;
                        ShowContinueError("...vertex " + RoundSigDigits(n) + " to vertex " + RoundSigDigits(Np1) + " to vertex " +
                                          RoundSigDigits(Np2));
                        ShowContinueError("...vertex " + RoundSigDigits(n) + "=[" + RoundSigDigits(X(n), 2) + ',' + RoundSigDigits(Y(n), 2) + ',' +
                                          RoundSigDigits(Z(n), 2) + ']');
                        ShowContinueError("...vertex " + RoundSigDigits(Np1) + "=[" + RoundSigDigits(X(n + 1), 2) + ',' +
                                          RoundSigDigits(Y(n + 1), 2) + ',' + RoundSigDigits(Z(n + 1), 2) + ']');
                        ShowContinueError("...vertex " + RoundSigDigits(Np2) + "=[" + RoundSigDigits(X(n + 2), 2) + ',' +
                                          RoundSigDigits(Y(n + 2), 2) + ',' + RoundSigDigits(Z(n + 2), 2) + ']');
                        //          CALL ShowContinueError('...theta angle=['//TRIM(RoundSigDigits(Theta,6))//']')
                        //          CALL ShowContinueError('...last theta angle=['//TRIM(RoundSigDigits(LastTheta,6))//']')
                    }
                }
                SurfaceTmp(SurfNum).IsConvex = false;
                break;
            }
            PrevSignFlag = SignFlag;
            LastTheta = Theta;
        }

        // must check to make sure don't remove NSides below 3
        if (M > 0) { // Remove the collinear points determined above
            if (NSides - M > 2) {
                SurfaceTmp(SurfNum).Sides = NSides - M;
            } else { // too many
                if (DisplayExtraWarnings) {
                    ShowWarningError("CheckConvexity: Surface=\"" + SurfaceTmp(SurfNum).Name + "\" has [" + RoundSigDigits(M) +
                                     "] collinear points.");
                    ShowContinueError("...too many to remove all.  Will leave the surface with 3 sides. But this is now a degenerate surface");
                }
                ++TotalDegenerateSurfaces;
                SurfaceTmp(SurfNum).Sides = max(NSides - M, 3);
                M = NSides - SurfaceTmp(SurfNum).Sides;
            }
            for (J = 1; J <= M; ++J) {
                Ind = SurfCollinearVerts(J);
                if (Ind > NSides) {
                    Ind = Ind - NSides + M - 1;
                }
                for (K = Ind; K <= NSides - 1; ++K) {
                    SurfaceTmp(SurfNum).Vertex(K - J + 1).x = SurfaceTmp(SurfNum).Vertex(K - J + 2).x;
                    SurfaceTmp(SurfNum).Vertex(K - J + 1).y = SurfaceTmp(SurfNum).Vertex(K - J + 2).y;
                    SurfaceTmp(SurfNum).Vertex(K - J + 1).z = SurfaceTmp(SurfNum).Vertex(K - J + 2).z;
                }
            }
            // remove duplicated points and resize Vertex
            Array1D<Vector> OldVertex;
            OldVertex.allocate(NSides);
            OldVertex = SurfaceTmp(SurfNum).Vertex;
            SurfaceTmp(SurfNum).Vertex.deallocate();
            SurfaceTmp(SurfNum).Vertex.allocate(NSides - M);
            for (J = 1; J <= NSides - M; ++J) {
                SurfaceTmp(SurfNum).Vertex(J) = OldVertex(J);
            }
            OldVertex.deallocate();
            if (DisplayExtraWarnings) {
                ShowWarningError("CheckConvexity: Surface=\"" + SurfaceTmp(SurfNum).Name +
                                 "\": The vertex points has been reprocessed as Sides = " + RoundSigDigits(SurfaceTmp(SurfNum).Sides));
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
        Real64 const cos89deg = std::cos(89.0 * DataGlobalConstants::DegToRadians()); // tolerance for right angle
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
        BaseCosAzimuth = std::cos(SurfWorldAz * DataGlobalConstants::DegToRadians());
        BaseSinAzimuth = std::sin(SurfWorldAz * DataGlobalConstants::DegToRadians());
        BaseCosTilt = std::cos(SurfTilt * DataGlobalConstants::DegToRadians());
        BaseSinTilt = std::sin(SurfTilt * DataGlobalConstants::DegToRadians());
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
            auto &thisMatLay(dataMaterial.Material(thisConstLayer));
            auto &revMatLay(dataMaterial.Material(revConstLayer));
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
