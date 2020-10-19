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
#include <cassert>
#include <cmath>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/CommandLineInterface.hh>
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataDaylightingDevices.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataReportingFlags.hh>
#include <EnergyPlus/DataShadowingCombinations.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarReflectionManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>
#include <EnergyPlus/WindowComplexManager.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>
#include <EnergyPlus/WindowManager.hh>
#include <EnergyPlus/WindowManagerExteriorData.hh>
#include <EnergyPlus/WindowModel.hh>
#include <WCEMultiLayerOptics.hpp>

namespace EnergyPlus {

namespace SolarShading {

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   March 1997
    //       MODIFIED       December 1998, FCW
    //       MODIFIED       July 1999, Linda Lawrie, eliminate shadefl.scr,
    //                      do shadowing calculations during simulation
    //       MODIFIED       June 2001, FCW, handle window blinds
    //       MODIFIED       May 2004, LKL, Polygons > 4 sides (not subsurfaces)
    //       MODIFIED       January 2007, LKL, Taking parameters back to original integer (HC)
    //       MODIFIED       August 2011, JHK, Including Complex Fenestration optical calculations
    //       MODIFIED       November 2012, BG, Timestep solar and daylighting calculations
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to encompass the routines and data
    // which are need to perform the solar calculations in EnergyPlus.
    // This also requires that shading and geometry routines and data
    // which are used by the solar calculations be included in this module.

    // METHODOLOGY EMPLOYED:
    // Many of the methods used in this module have been carried over from the
    // (I)BLAST program.  As such, there is not much documentation on the
    // methodology used.  The original code was written mainly by George
    // Walton and requires coordinate transformations.  It calculates
    // shading using an overlapping polygon approach.

    // REFERENCES:
    // TARP Manual, NIST Publication.
    // Passive Solar Extension of the BLAST Program, CERL/UIUC Publication.

    using namespace DataGlobals;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using namespace DataShadowingCombinations;
    using DaylightingManager::ProfileAngle;
    using namespace SolarReflectionManager;
    using namespace DataReportingFlags;
    using DataBSDFWindow::ComplexWind;
    using DataBSDFWindow::MaxBkSurf;
    using DataBSDFWindow::SUNCOSTS;
    using namespace DataVectorTypes;
    using namespace WindowManager;
    using namespace FenestrationCommon;
    using namespace SingleLayerOptics;

    std::unique_ptr<std::iostream> shd_stream; // Shading file stream

    int const NPhi = 6;                      // Number of altitude angle steps for sky integration
    int const NTheta = 24;                   // Number of azimuth angle steps for sky integration
    Real64 const Eps = 1.e-10;               // Small number
    Real64 const DPhi = DataGlobalConstants::PiOvr2() / NPhi;       // Altitude step size
    Real64 const DTheta = 2.0 * DataGlobalConstants::Pi() / NTheta; // Azimuth step size
    Real64 const DThetaDPhi = DTheta * DPhi; // Product of DTheta and DPhi
    Real64 const PhiMin = 0.5 * DPhi;        // Minimum altitude

    std::vector<Real64> sin_Phi;
    std::vector<Real64> cos_Phi;
    std::vector<Real64> sin_Theta;
    std::vector<Real64> cos_Theta;

    void InitSolarCalculations(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         George Walton
        //       DATE WRITTEN   September 1977
        //       MODIFIED       na
        //       RE-ENGINEERED  Mar97, RKS, Initial EnergyPlus Version

        // PURPOSE OF THIS SUBROUTINE:
        // This routine controls the computation of the solar flux multipliers.

        // METHODOLOGY EMPLOYED:
        // All shadowing calculations have been grouped under this routine to
        // allow segmentation separating it from the hourly loads calculation.

#ifdef EP_Count_Calls
        ++NumInitSolar_Calls;
#endif
        if (BeginSimFlag) {
            if (state.files.outputControl.shd) {
                shd_stream = std::unique_ptr<std::iostream>(new std::fstream(DataStringGlobals::outputShdFileName.c_str(), std::ios_base::out | std::ios_base::trunc));
                if (!shd_stream) {
                    ShowFatalError("InitSolarCalculations: Could not open file \"" + DataStringGlobals::outputShdFileName + "\" for output (write).");
                }
            } else {
                shd_stream = std::unique_ptr<std::iostream>(new std::iostream(nullptr));
            }

            if (state.dataSolarShading->GetInputFlag) {
                GetShadowingInput(state);
                state.dataSolarShading->GetInputFlag = false;
                state.dataSolarShading->MaxHCV = (((max(15, MaxVerticesPerSurface) + 16) / 16) * 16) - 1; // Assure MaxHCV+1 is multiple of 16 for 128 B alignment
                assert((state.dataSolarShading->MaxHCV + 1) % 16 == 0);
            }

            if (state.dataSolarShading->firstTime) DisplayString("Allocate Solar Module Arrays");
            AllocateModuleArrays(state);

            if (SolarDistribution != FullInteriorExterior) {
                if (state.dataSolarShading->firstTime) DisplayString("Computing Interior Solar Absorption Factors");
                ComputeIntSolarAbsorpFactors(state);
            }

            if (state.dataSolarShading->firstTime) DisplayString("Determining Shadowing Combinations");
            DetermineShadowingCombinations(state);
            shd_stream.reset(); // Done writing to shd file

            if (state.dataSolarShading->firstTime) DisplayString("Computing Window Shade Absorption Factors");
            ComputeWinShadeAbsorpFactors(state);

            if (CalcSolRefl) {
                DisplayString("Initializing Solar Reflection Factors");
                InitSolReflRecSurf(state);
            }

            if (state.dataSolarShading->firstTime) DisplayString("Proceeding with Initializing Solar Calculations");
        }

        if (BeginEnvrnFlag) {
            state.dataSolarShading->CTHETA = 0.0;
            state.dataSolarShading->SAREA = 0.0;
            SurfSunlitArea = 0.0;
            SurfSunlitFrac = 0.0;
            SunlitFracHR = 0.0;
            SunlitFrac = 0.0;
            SunlitFracWithoutReveal = 0.0;
            BackSurfaces = 0;
            OverlapAreas = 0.0;
            CosIncAngHR = 0.0;
            CosIncAng = 0.0;
            AnisoSkyMult = 1.0; // For isotropic sky; recalculated in AnisoSkyViewFactors if anisotropic radiance
            //    WithShdgIsoSky=0.0
            //    WoShdgIsoSky=0.0
            //    WithShdgHoriz=0.0
            //    WoShdgHoriz=0.0
            //    DifShdgRatioIsoSky=0.0
            //    DifShdgRatioHoriz=0.0
            MultIsoSky = 0.0;
            MultCircumSolar = 0.0;
            MultHorizonZenith = 0.0;

            InsideGlassCondensationFlag = 0;
            InsideFrameCondensationFlag = 0;
            InsideDividerCondensationFlag = 0;
            ZoneTransSolar = 0.0;
            ZoneBmSolFrExtWinsRep = 0.0;
            ZoneBmSolFrIntWinsRep = 0.0;
            InitialZoneDifSolReflW = 0.0;
            ZoneDifSolFrExtWinsRep = 0.0;
            ZoneDifSolFrIntWinsRep = 0.0;
            ZoneWinHeatGain = 0.0;
            ZoneWinHeatGainRep = 0.0;
            ZoneWinHeatLossRep = 0.0;
            ZoneOpaqSurfInsFaceCond = 0.0;
            ZoneOpaqSurfInsFaceCondGainRep = 0.0;
            ZoneOpaqSurfInsFaceCondLossRep = 0.0;

            SurfQRadSWOutIncident = 0.0;
            SurfQRadSWOutIncidentBeam = 0.0;
            SurfBmIncInsSurfIntensRep = 0.0;
            SurfBmIncInsSurfAmountRep = 0.0;
            SurfIntBmIncInsSurfIntensRep = 0.0;
            SurfIntBmIncInsSurfAmountRep = 0.0;
            SurfQRadSWOutIncidentSkyDiffuse = 0.0;
            SurfQRadSWOutIncidentGndDiffuse = 0.0;
            SurfQRadSWOutIncBmToDiffReflGnd = 0.0;
            SurfQRadSWOutIncSkyDiffReflGnd = 0.0;
            SurfQRadSWOutIncBmToBmReflObs = 0.0;
            SurfQRadSWOutIncBmToDiffReflObs = 0.0;
            SurfQRadSWOutIncSkyDiffReflObs = 0.0;
            SurfCosIncidenceAngle = 0.0;
            SurfSWInAbsTotalReport = 0.0;
            SurfBmIncInsSurfAmountRepEnergy = 0.0;
            SurfIntBmIncInsSurfAmountRepEnergy = 0.0;

            SurfWinQRadSWwinAbsTot = 0.0;
            SurfWinQRadSWwinAbsTotEnergy = 0.0;
            SurfWinSWwinAbsTotalReport = 0.0;
            SurfInitialDifSolInAbsReport = 0.0;
            SurfWinInitialDifSolInTransReport = 0.0;

            state.dataSolarShading->WindowRevealStatus = 0;
            ZoneTransSolarEnergy = 0.0;
            ZoneBmSolFrExtWinsRepEnergy = 0.0;
            ZoneBmSolFrIntWinsRepEnergy = 0.0;
            ZoneDifSolFrExtWinsRepEnergy = 0.0;
            ZoneDifSolFrIntWinsRepEnergy = 0.0;
            ZoneWinHeatGainRepEnergy = 0.0;
            ZoneWinHeatLossRepEnergy = 0.0;
            ZnOpqSurfInsFaceCondGnRepEnrg = 0.0;
            ZnOpqSurfInsFaceCondLsRepEnrg = 0.0;


            // Surface Win
            for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
                int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
                int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
                if (firstSurfWin == -1) continue;
                for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {

                    SurfWinTransSolar(SurfNum) = 0.0;
                    SurfWinBmSolar(SurfNum) = 0.0;
                    SurfWinBmBmSolar(SurfNum) = 0.0;
                    SurfWinBmDifSolar(SurfNum) = 0.0;
                    SurfWinDifSolar(SurfNum) = 0.0;
                    SurfWinDirSolTransAtIncAngle(SurfNum) = 0.0;

                    SurfWinTransSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmBmSolarEnergy(SurfNum) = 0.0;
                    SurfWinBmDifSolarEnergy(SurfNum) = 0.0;

                    SurfWinHeatGain(SurfNum) = 0.0;
                    SurfWinHeatTransfer(SurfNum) = 0.0;
                    SurfWinHeatGainRep(SurfNum) = 0.0;
                    SurfWinHeatLossRep(SurfNum) = 0.0;

                    SurfWinGainConvGlazToZoneRep(SurfNum) = 0.0;
                    SurfWinGainIRGlazToZoneRep(SurfNum) = 0.0;
                    SurfWinLossSWZoneToOutWinRep(SurfNum) = 0.0;
                    SurfWinGainFrameDividerToZoneRep(SurfNum) = 0.0;
                    SurfWinGainConvGlazShadGapToZoneRep(SurfNum) = 0.0;
                    SurfWinGainConvShadeToZoneRep(SurfNum) = 0.0;
                    SurfWinGainIRShadeToZoneRep(SurfNum) = 0.0;
                    SurfWinGapConvHtFlowRep(SurfNum) = 0.0;
                    SurfWinShadingAbsorbedSolar(SurfNum) = 0.0;

                    SurfWinSysSolTransmittance(SurfNum) = 0.0;
                    SurfWinSysSolReflectance(SurfNum) = 0.0;
                    SurfWinSysSolAbsorptance(SurfNum) = 0.0;

                    SurfWinDifSolarEnergy(SurfNum) = 0.0;
                    SurfWinHeatGainRepEnergy(SurfNum) = 0.0;
                    SurfWinHeatLossRepEnergy(SurfNum) = 0.0;
                    SurfWinGapConvHtFlowRepEnergy(SurfNum) = 0.0;
                    SurfWinHeatTransferRepEnergy(SurfNum) = 0.0;
                    SurfWinShadingAbsorbedSolarEnergy(SurfNum) = 0.0;
                }
            }
        }

        // Initialize these once
        for (int IPhi = 1; IPhi <= NPhi; ++IPhi) {   // Loop over patch altitude values
            Real64 Phi = PhiMin + (IPhi - 1) * DPhi; // 7.5,22.5,37.5,52.5,67.5,82.5 for NPhi = 6
            sin_Phi.push_back(std::sin(Phi));
            cos_Phi.push_back(std::cos(Phi));
        }

        for (int ITheta = 1; ITheta <= NTheta; ++ITheta) { // Loop over patch azimuth values
            Real64 Theta = (ITheta - 1) * DTheta;          // 0,15,30,....,330,345 for NTheta = 24
            sin_Theta.push_back(std::sin(Theta));
            cos_Theta.push_back(std::cos(Theta));
        }

        state.dataSolarShading->firstTime = false;
    }

    void GetShadowingInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 1999
        //       MODIFIED       B. Griffith, Nov 2012, add calculation method
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the Shadowing Calculation object.

        // Using/Aliasing
        using General::RoundSigDigits;
        using namespace DataIPShortCuts;
        using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
        using DataSystemVariables::DetailedSolarTimestepIntegration;
        using DataSystemVariables::DisableAllSelfShading;
        using DataSystemVariables::DisableGroupSelfShading;
        using DataSystemVariables::ReportExtShadingSunlitFrac;
        using DataSystemVariables::SutherlandHodgman;
        using DataSystemVariables::ShadingMethod;
        using DataSystemVariables::shadingMethod;
        using DataSystemVariables::SlaterBarsky;
        using ScheduleManager::ScheduleFileShadingProcessed;


        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumItems;
        int NumNumbers;
        int NumAlphas;
        int IOStat;
        int Found = 0;

        rNumericArgs({1, 4}) = 0.0; // so if nothing gotten, defaults will be maintained.
        cAlphaArgs(1) = "";
        cAlphaArgs(2) = "";
        cCurrentModuleObject = "ShadowCalculation";
        NumItems = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        NumAlphas = 0;
        NumNumbers = 0;
        if (NumItems > 1) {
            ShowWarningError(cCurrentModuleObject + ": More than 1 occurrence of this object found, only first will be used.");
        }

        if (NumItems != 0) {
            inputProcessor->getObjectItem(state,
                                          cCurrentModuleObject,
                                          1,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            state.dataSolarShading->ShadowingCalcFrequency = rNumericArgs(1);
        }

        if (state.dataSolarShading->ShadowingCalcFrequency <= 0) {
            //  Set to default value
            state.dataSolarShading->ShadowingCalcFrequency = 20;
        }
        if (state.dataSolarShading->ShadowingCalcFrequency > 31) {
            ShowWarningError(cCurrentModuleObject + ": suspect " + cNumericFieldNames(1));
            ShowContinueError("Value entered=[" + RoundSigDigits(rNumericArgs(1), 0) + "], Shadowing Calculations will be inaccurate.");
        }

        if (rNumericArgs(2) > 199.0) {
            state.dataSolarShading->MaxHCS = rNumericArgs(2);
        } else {
            state.dataSolarShading->MaxHCS = 15000;
        }

        int aNum = 1;
        unsigned pixelRes = 512u;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Scheduled")) {
                shadingMethod = ShadingMethod::Scheduled;
                cAlphaArgs(aNum) = "Scheduled";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Imported")) {
                if (ScheduleFileShadingProcessed) {
                    shadingMethod = ShadingMethod::Imported;
                    cAlphaArgs(aNum) = "Imported";
                } else {
                    ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                    ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) +
                                      "\" while no Schedule:File:Shading object is defined, InternalCalculation will be used.");
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "PolygonClipping")) {
                shadingMethod = ShadingMethod::PolygonClipping;
                cAlphaArgs(aNum) = "PolygonClipping";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "PixelCounting")) {
                shadingMethod = ShadingMethod::PixelCounting;
                cAlphaArgs(aNum) = "PixelCounting";
                if (NumNumbers >= 3) {
                    pixelRes = (unsigned)rNumericArgs(3);
                }
#ifdef EP_NO_OPENGL
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\"");
                ShowContinueError("This version of EnergyPlus was not compiled to use OpenGL (required for PixelCounting)");
                ShowContinueError("PolygonClipping will be used instead");
                shadingMethod = ShadingMethod::PolygonClipping;
                cAlphaArgs(aNum) = "PolygonClipping";
#else
                auto error_callback = [](const int messageType, const std::string & message, void * /*contextPtr*/){
                    if (messageType == Pumbra::MSG_ERR) {
                        ShowSevereError(message);
                    } else if (messageType == Pumbra::MSG_WARN) {
                        ShowWarningError(message);
                    } else /*if (messageType == MSG_INFO)*/ {
                        ShowMessage(message);
                    }
                };
                if (Pumbra::Penumbra::isValidContext()) {
                    state.dataSolarShading->penumbra = std::unique_ptr<Pumbra::Penumbra>(new Pumbra::Penumbra(error_callback, pixelRes));
                } else {
                    ShowWarningError("No GPU found (required for PixelCounting)");
                    ShowContinueError("PolygonClipping will be used instead");
                    shadingMethod = ShadingMethod::PolygonClipping;
                    cAlphaArgs(aNum) = "PolygonClipping";
                }
#endif
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", PolygonClipping will be used.");
            }
        } else {
            cAlphaArgs(aNum) = "PolygonClipping";
            shadingMethod = ShadingMethod::PolygonClipping;
        }

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Periodic")) {
                DetailedSolarTimestepIntegration = false;
                cAlphaArgs(aNum) = "Periodic";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Timestep")) {
                DetailedSolarTimestepIntegration = true;
                cAlphaArgs(aNum) = "Timestep";
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", Periodic will be used.");
                DetailedSolarTimestepIntegration = false;
                cAlphaArgs(aNum) = "Periodic";
            }
        } else {
            DetailedSolarTimestepIntegration = false;
            cAlphaArgs(aNum) = "Periodic";
        }

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "SutherlandHodgman")) {
                SutherlandHodgman = true;
                cAlphaArgs(aNum) = "SutherlandHodgman";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "ConvexWeilerAtherton")) {
                SutherlandHodgman = false;
                cAlphaArgs(aNum) = "ConvexWeilerAtherton";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "SlaterBarskyandSutherlandHodgman")) {
                SutherlandHodgman = true;
                SlaterBarsky = true;
                cAlphaArgs(aNum) = "SlaterBarskyandSutherlandHodgman";
            } else if (lAlphaFieldBlanks(aNum)) {
                if (!SutherlandHodgman) { // if already set.
                    cAlphaArgs(aNum) = "ConvexWeilerAtherton";
                } else {
                    if (!SlaterBarsky) {
                        cAlphaArgs(aNum) = "SutherlandHodgman";
                    } else {
                        cAlphaArgs(aNum) = "SlaterBarskyandSutherlandHodgman";
                    }
                }
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                if (!SutherlandHodgman) {
                    ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", ConvexWeilerAtherton will be used.");
                } else {
                    if (!SlaterBarsky) {
                        ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", SutherlandHodgman will be used.");
                    } else {
                        ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", SlaterBarskyandSutherlandHodgman will be used.");
                    }

                }
            }
        } else {
            if (!SutherlandHodgman) {
                cAlphaArgs(aNum) = "ConvexWeilerAtherton";
            } else {
                if (!SlaterBarsky) {
                    cAlphaArgs(aNum) = "SutherlandHodgman";
                } else {
                    cAlphaArgs(aNum) = "SlaterBarskyandSutherlandHodgman";
                }
            }
        }

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "SimpleSkyDiffuseModeling")) {
                DetailedSkyDiffuseAlgorithm = false;
                cAlphaArgs(aNum) = "SimpleSkyDiffuseModeling";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "DetailedSkyDiffuseModeling")) {
                DetailedSkyDiffuseAlgorithm = true;
                cAlphaArgs(aNum) = "DetailedSkyDiffuseModeling";
            } else if (lAlphaFieldBlanks(3)) {
                DetailedSkyDiffuseAlgorithm = false;
                cAlphaArgs(aNum) = "SimpleSkyDiffuseModeling";
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", SimpleSkyDiffuseModeling will be used.");
            }
        } else {
            cAlphaArgs(aNum) = "SimpleSkyDiffuseModeling";
            DetailedSkyDiffuseAlgorithm = false;
        }

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Yes")) {
                ReportExtShadingSunlitFrac = true;
                cAlphaArgs(aNum) = "Yes";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "No")) {
                ReportExtShadingSunlitFrac = false;
                cAlphaArgs(aNum) = "No";
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", InternalCalculation will be used.");
            }
        } else {
            cAlphaArgs(aNum) = "No";
            ReportExtShadingSunlitFrac = false;
        }
        int ExtShadingSchedNum;
        if (shadingMethod == ShadingMethod::Imported) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                ExtShadingSchedNum = ScheduleManager::GetScheduleIndex(state, Surface(SurfNum).Name + "_shading");
                if (ExtShadingSchedNum) {
                    Surface(SurfNum).SchedExternalShadingFrac = true;
                    Surface(SurfNum).ExternalShadingSchInd = ExtShadingSchedNum;
                } else {
                    ShowWarningError(cCurrentModuleObject + ": sunlit fraction schedule not found for " + Surface(SurfNum).Name + " when using ImportedShading.");
                    ShowContinueError("These values are set to 1.0.");
                }
            }
        }

        bool DisableSelfShadingWithinGroup = false;
        bool DisableSelfShadingBetweenGroup = false;

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Yes")) {
                DisableSelfShadingWithinGroup = true;
                cAlphaArgs(aNum) = "Yes";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "No")) {
                cAlphaArgs(aNum) = "No";
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", all shading effects would be considered.");
            }
        } else {
            cAlphaArgs(aNum) = "No";
        }

        aNum++;
        if (NumAlphas >= aNum) {
            if (UtilityRoutines::SameString(cAlphaArgs(aNum), "Yes")) {
                DisableSelfShadingBetweenGroup = true;
                cAlphaArgs(aNum) = "Yes";
            } else if (UtilityRoutines::SameString(cAlphaArgs(aNum), "No")) {
                cAlphaArgs(aNum) = "No";
            } else {
                ShowWarningError(cCurrentModuleObject + ": invalid " + cAlphaFieldNames(aNum));
                ShowContinueError("Value entered=\"" + cAlphaArgs(aNum) + "\", all shading effects would be considered.");
            }
        } else {
            cAlphaArgs(aNum) = "No";
        }

        if (DisableSelfShadingBetweenGroup && DisableSelfShadingWithinGroup) {
            DisableAllSelfShading = true;
        } else if (DisableSelfShadingBetweenGroup || DisableSelfShadingWithinGroup) {
            DisableGroupSelfShading = true;
        }

        aNum++;
        int SurfZoneGroup, CurZoneGroup;
        if (DisableGroupSelfShading) {
            Array1D_int DisableSelfShadingGroups;
            int NumOfShadingGroups;
            if (NumAlphas >= aNum) {
                // Read all shading groups
                NumOfShadingGroups = NumAlphas - (aNum - 1);
                DisableSelfShadingGroups.allocate(NumOfShadingGroups);
                for (int i = 1; i <= NumOfShadingGroups; i++) {
                    Found = UtilityRoutines::FindItemInList(cAlphaArgs(i + (aNum - 1)), ZoneList, NumOfZoneLists);
                    if (Found != 0) DisableSelfShadingGroups(i) = Found;
                }

                for (int SurfNum = 1; SurfNum <= TotSurfaces; SurfNum++) {
                    if (Surface(SurfNum).ExtBoundCond == 0) { // Loop through all exterior surfaces
                        SurfZoneGroup = 0;
                        // Check the shading zone group of each exterior surface
                        for (int ZoneGroupLoop = 1; ZoneGroupLoop <= NumOfShadingGroups; ZoneGroupLoop++) { // Loop through all defined shading groups
                            CurZoneGroup = DisableSelfShadingGroups(ZoneGroupLoop);
                            for (int ZoneNum = 1; ZoneNum <= ZoneList(CurZoneGroup).NumOfZones;
                                 ZoneNum++) { // Loop through all zones in the zone list
                                if (Surface(SurfNum).Zone == ZoneList(CurZoneGroup).Zone(ZoneNum)) {
                                    SurfZoneGroup = CurZoneGroup;
                                    break;
                                }
                            }
                        }
                        // if a surface is not in any zone group, no self shading is disabled for this surface
                        if (SurfZoneGroup != 0) {
                            // if DisableSelfShadingWithinGroup, add all zones in the same zone group to the surface's disabled zone list
                            // if DisableSelfShadingBetweenGroups, add all zones in all other zone groups to the surface's disabled zone list
                            for (int ZoneGroupLoop = 1; ZoneGroupLoop <= NumOfShadingGroups;
                                 ZoneGroupLoop++) { // Loop through all defined shading groups
                                CurZoneGroup = DisableSelfShadingGroups(ZoneGroupLoop);
                                if (SurfZoneGroup == CurZoneGroup && DisableSelfShadingWithinGroup) {
                                    for (int ZoneNum = 1; ZoneNum <= ZoneList(CurZoneGroup).NumOfZones;
                                         ZoneNum++) { // Loop through all zones in the zone list
                                        Surface(SurfNum).DisabledShadowingZoneList.push_back(ZoneList(CurZoneGroup).Zone(ZoneNum));
                                    }
                                } else if (SurfZoneGroup != CurZoneGroup && DisableSelfShadingBetweenGroup) {
                                    for (int ZoneNum = 1; ZoneNum <= ZoneList(CurZoneGroup).NumOfZones; ZoneNum++) {
                                        Surface(SurfNum).DisabledShadowingZoneList.push_back(ZoneList(CurZoneGroup).Zone(ZoneNum));
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                ShowFatalError("No Shading groups are defined when disabling grouped self shading.");
            }
        }

        if (!DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
            ShowWarningError("GetShadowingInput: The shading transmittance for shading devices changes throughout the year. Choose "
                             "DetailedSkyDiffuseModeling in the " +
                             cCurrentModuleObject + " object to remove this warning.");
            ShowContinueError("Simulation has been reset to use DetailedSkyDiffuseModeling. Simulation continues.");
            DetailedSkyDiffuseAlgorithm = true;
            cAlphaArgs(2) = "DetailedSkyDiffuseModeling";
            if (state.dataSolarShading->ShadowingCalcFrequency > 1) {
                ShowContinueError("Better accuracy may be gained by setting the " + cNumericFieldNames(1) + " to 1 in the " + cCurrentModuleObject +
                                  " object.");
            }
        } else if (DetailedSkyDiffuseAlgorithm) {
            if (!ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing) {
                ShowWarningError("GetShadowingInput: DetailedSkyDiffuseModeling is chosen but not needed as either the shading transmittance for "
                                 "shading devices does not change throughout the year");
                ShowContinueError(" or MinimalShadowing has been chosen.");
                ShowContinueError("Simulation should be set to use SimpleSkyDiffuseModeling, but is left at Detailed for simulation.");
                ShowContinueError("Choose SimpleSkyDiffuseModeling in the " + cCurrentModuleObject + " object to reduce computation time.");
            }
        }

        print(state.files.eio,
              "{}",
              "! <Shadowing/Sun Position Calculations Annual Simulations>, Shading Calculation Method, "
              "Shading Calculation Update Frequency Method, Shading Calculation Update Frequency {days}, "
              "Maximum Figures in Shadow Overlap Calculations {}, Polygon Clipping Algorithm, Pixel Counting Resolution, Sky Diffuse Modeling "
              "Algorithm, Output External Shading Calculation Results, Disable "
              "Self-Shading Within Shading Zone Groups, Disable Self-Shading From Shading Zone Groups to Other Zones\n");
        print(state.files.eio,
              "Shadowing/Sun Position Calculations Annual Simulations,{},{},{},{},{},{},{},{},{},{}\n",
              cAlphaArgs(1),
              cAlphaArgs(2),
              state.dataSolarShading->ShadowingCalcFrequency,
              state.dataSolarShading->MaxHCS,
              cAlphaArgs(3),
              pixelRes,
              cAlphaArgs(4),
              cAlphaArgs(5),
              cAlphaArgs(6),
              cAlphaArgs(7));
    }

    void AllocateModuleArrays(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 1998
        //       MODIFIED       August 2005 JG - Added output variables for energy in J

        // PURPOSE OF THIS SUBROUTINE:
        // This routine allocates all of the arrays at the module level which
        // require allocation.

        // METHODOLOGY EMPLOYED:
        // Allocation is dependent on the user input file.

        using General::RoundSigDigits;

        int SurfLoop;
        int I;
        int NumOfLayers;

        // FLOW:

        state.dataSolarShading->CTHETA.dimension(TotSurfaces, 0.0);
        state.dataSolarShading->SAREA.dimension(TotSurfaces, 0.0);
        SurfSunlitArea.dimension(TotSurfaces, 0.0);
        SurfSunlitFrac.dimension(TotSurfaces, 0.0);
        SunlitFracHR.dimension(24, TotSurfaces, 0.0);
        SunlitFrac.dimension(NumOfTimeStepInHour, 24, TotSurfaces, 0.0);
        SunlitFracWithoutReveal.dimension(NumOfTimeStepInHour, 24, TotSurfaces, 0.0);
        BackSurfaces.dimension(NumOfTimeStepInHour, 24, MaxBkSurf, TotSurfaces, 0);
        OverlapAreas.dimension(NumOfTimeStepInHour, 24, MaxBkSurf, TotSurfaces, 0.0);
        CosIncAngHR.dimension(24, TotSurfaces, 0.0);
        CosIncAng.dimension(NumOfTimeStepInHour, 24, TotSurfaces, 0.0);
        AnisoSkyMult.dimension(TotSurfaces, 1.0); // For isotropic sky: recalculated in AnisoSkyViewFactors if anisotropic radiance
        //  ALLOCATE(WithShdgIsoSky(TotSurfaces))
        //  WithShdgIsoSky=0.0
        //  ALLOCATE(WoShdgIsoSky(TotSurfaces))
        //  WoShdgIsoSky=0.0
        //  ALLOCATE(WithShdgHoriz(TotSurfaces))
        //  WithShdgHoriz=0.0
        //  ALLOCATE(WoShdgHoriz(TotSurfaces))
        //  WoShdgHoriz=0.0
        //  ALLOCATE(DifShdgRatioIsoSky(TotSurfaces))
        //  DifShdgRatioIsoSky=0.0
        //  ALLOCATE(DifShdgRatioHoriz(TotSurfaces))
        //  DifShdgRatioHoriz=0.0
        MultIsoSky.dimension(TotSurfaces, 0.0);
        MultCircumSolar.dimension(TotSurfaces, 0.0);
        MultHorizonZenith.dimension(TotSurfaces, 0.0);
        SurfWinTransSolar.dimension(TotSurfaces, 0.0);
        SurfWinBmSolar.dimension(TotSurfaces, 0.0);
        SurfWinBmBmSolar.dimension(TotSurfaces, 0.0);
        SurfWinBmDifSolar.dimension(TotSurfaces, 0.0);

        SurfWinDifSolar.dimension(TotSurfaces, 0.0);
        SurfWinDirSolTransAtIncAngle.dimension(TotSurfaces, 0.0);
        SurfWinHeatGain.dimension(TotSurfaces, 0.0);
        SurfWinHeatTransfer.dimension(TotSurfaces, 0.0);
        SurfWinHeatGainRep.dimension(TotSurfaces, 0.0);
        SurfWinHeatLossRep.dimension(TotSurfaces, 0.0);
        SurfWinGainConvGlazToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinGainIRGlazToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinLossSWZoneToOutWinRep.dimension(TotSurfaces, 0.0);
        SurfWinGainFrameDividerToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinGainConvGlazShadGapToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinGainConvShadeToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinOtherConvGainInsideFaceToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinGainIRShadeToZoneRep.dimension(TotSurfaces, 0.0);
        SurfWinGapConvHtFlowRep.dimension(TotSurfaces, 0.0);
        SurfWinShadingAbsorbedSolar.dimension(TotSurfaces, 0.0);
        SurfWinSysSolTransmittance.dimension(TotSurfaces, 0.0);
        SurfWinSysSolReflectance.dimension(TotSurfaces, 0.0);
        SurfWinSysSolAbsorptance.dimension(TotSurfaces, 0.0);
        InsideGlassCondensationFlag.dimension(TotSurfaces, 0);
        InsideFrameCondensationFlag.dimension(TotSurfaces, 0);
        InsideDividerCondensationFlag.dimension(TotSurfaces, 0);
        ZoneTransSolar.dimension(NumOfZones, 0.0);
        ZoneBmSolFrExtWinsRep.dimension(NumOfZones, 0.0);
        ZoneBmSolFrIntWinsRep.dimension(NumOfZones, 0.0);
        InitialZoneDifSolReflW.dimension(NumOfZones, 0.0);
        ZoneDifSolFrExtWinsRep.dimension(NumOfZones, 0.0);
        ZoneDifSolFrIntWinsRep.dimension(NumOfZones, 0.0);
        ZoneWinHeatGain.dimension(NumOfZones, 0.0);
        ZoneWinHeatGainRep.dimension(NumOfZones, 0.0);
        ZoneWinHeatLossRep.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfInsFaceCond.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfInsFaceCondGainRep.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfInsFaceCondLossRep.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfExtFaceCond.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfExtFaceCondGainRep.dimension(NumOfZones, 0.0);
        ZoneOpaqSurfExtFaceCondLossRep.dimension(NumOfZones, 0.0);

        SurfQRadSWOutIncident.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncidentBeam.dimension(TotSurfaces, 0.0);
        SurfBmIncInsSurfIntensRep.dimension(TotSurfaces, 0.0);
        SurfBmIncInsSurfAmountRep.dimension(TotSurfaces, 0.0);
        //  ALLOCATE(DifIncInsSurfIntensRep(TotSurfaces))
        //  DifIncInsSurfIntensRep=0.0
        //  ALLOCATE(DifIncInsSurfAmountRep(TotSurfaces))
        //  DifIncInsSurfAmountRep=0.0
        SurfIntBmIncInsSurfIntensRep.dimension(TotSurfaces, 0.0);
        SurfIntBmIncInsSurfAmountRep.dimension(TotSurfaces, 0.0);
        //  ALLOCATE(IntDifIncInsSurfIntensRep(TotSurfaces))
        //  IntDifIncInsSurfIntensRep=0.0
        //  ALLOCATE(IntDifIncInsSurfAmountRep(TotSurfaces))
        //  IntDifIncInsSurfAmountRep=0.0
        SurfQRadSWOutIncidentSkyDiffuse.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncidentGndDiffuse.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncBmToDiffReflGnd.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncSkyDiffReflGnd.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncBmToBmReflObs.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncBmToDiffReflObs.dimension(TotSurfaces, 0.0);
        SurfQRadSWOutIncSkyDiffReflObs.dimension(TotSurfaces, 0.0);
        SurfCosIncidenceAngle.dimension(TotSurfaces, 0.0);

        SurfWinBSDFBeamDirectionRep.dimension(TotSurfaces, 0);
        SurfWinBSDFBeamThetaRep.dimension(TotSurfaces, 0.0);
        SurfWinBSDFBeamPhiRep.dimension(TotSurfaces, 0.0);
        SurfWinQRadSWwinAbsTot.dimension(TotSurfaces, 0.0);

        SurfWinQRadSWwinAbsLayer.dimension(DataHeatBalance::MaxSolidWinLayers, TotSurfaces, 0.0);

        SurfWinFenLaySurfTempFront.dimension(DataHeatBalance::MaxSolidWinLayers, TotSurfaces, 0.0);
        SurfWinFenLaySurfTempBack.dimension(DataHeatBalance::MaxSolidWinLayers, TotSurfaces, 0.0);

        SurfWinSWwinAbsTotalReport.dimension(TotSurfaces, 0.0);
        SurfInitialDifSolInAbsReport.dimension(TotSurfaces, 0.0);
        SurfWinInitialDifSolInTransReport.dimension(TotSurfaces, 0.0);
        SurfSWInAbsTotalReport.dimension(TotSurfaces, 0.0);
        state.dataSolarShading->WindowRevealStatus.dimension(NumOfTimeStepInHour, 24, TotSurfaces, 0);

        // Weiler-Atherton
        state.dataSolarShading->MAXHCArrayBounds = 2 * (MaxVerticesPerSurface + 1);
        state.dataSolarShading->MAXHCArrayIncrement = MaxVerticesPerSurface + 1;
        state.dataSolarShading->XTEMP.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->YTEMP.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->XVC.dimension(MaxVerticesPerSurface + 1, 0.0);
        state.dataSolarShading->XVS.dimension(MaxVerticesPerSurface + 1, 0.0);
        state.dataSolarShading->YVC.dimension(MaxVerticesPerSurface + 1, 0.0);
        state.dataSolarShading->YVS.dimension(MaxVerticesPerSurface + 1, 0.0);
        state.dataSolarShading->ZVC.dimension(MaxVerticesPerSurface + 1, 0.0);

        // Sutherland-Hodgman
        state.dataSolarShading->ATEMP.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->BTEMP.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->CTEMP.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->XTEMP1.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);
        state.dataSolarShading->YTEMP1.dimension(2 * (MaxVerticesPerSurface + 1), 0.0);

        // energy
        SurfWinTransSolarEnergy.dimension(TotSurfaces, 0.0);
        SurfWinBmSolarEnergy.dimension(TotSurfaces, 0.0);

        SurfWinBmBmSolarEnergy.dimension(TotSurfaces, 0.0);
        SurfWinBmDifSolarEnergy.dimension(TotSurfaces, 0.0);

        SurfWinDifSolarEnergy.dimension(TotSurfaces, 0.0);
        SurfWinHeatGainRepEnergy.dimension(TotSurfaces, 0.0);
        SurfWinHeatLossRepEnergy.dimension(TotSurfaces, 0.0);
        SurfWinGapConvHtFlowRepEnergy.dimension(TotSurfaces, 0.0);
        SurfWinHeatTransferRepEnergy.dimension(TotSurfaces, 0.0);
        SurfWinShadingAbsorbedSolarEnergy.dimension(TotSurfaces, 0.0);

        ZoneTransSolarEnergy.dimension(NumOfZones, 0.0);
        ZoneBmSolFrExtWinsRepEnergy.dimension(NumOfZones, 0.0);
        ZoneBmSolFrIntWinsRepEnergy.dimension(NumOfZones, 0.0);
        ZoneDifSolFrExtWinsRepEnergy.dimension(NumOfZones, 0.0);
        ZoneDifSolFrIntWinsRepEnergy.dimension(NumOfZones, 0.0);
        ZoneWinHeatGainRepEnergy.dimension(NumOfZones, 0.0);
        ZoneWinHeatLossRepEnergy.dimension(NumOfZones, 0.0);

        ZnOpqSurfInsFaceCondGnRepEnrg.dimension(NumOfZones, 0.0);
        ZnOpqSurfInsFaceCondLsRepEnrg.dimension(NumOfZones, 0.0);
        ZnOpqSurfExtFaceCondGnRepEnrg.dimension(NumOfZones, 0.0);
        ZnOpqSurfExtFaceCondLsRepEnrg.dimension(NumOfZones, 0.0);
        //  ALLOCATE(DifIncInsSurfAmountRepEnergy(TotSurfaces))
        //  DifIncInsSurfAmountRepEnergy=0.0
        SurfBmIncInsSurfAmountRepEnergy.dimension(TotSurfaces, 0.0);
        SurfIntBmIncInsSurfAmountRepEnergy.dimension(TotSurfaces, 0.0);
        //  ALLOCATE(IntDifIncInsSurfAmountRepEnergy(TotSurfaces))
        //  IntDifIncInsSurfAmountRepEnergy=0.0
        SurfWinQRadSWwinAbsTotEnergy.dimension(TotSurfaces, 0.0);

        for (int SurfNum = 1; SurfNum <= TotSurfaces; SurfNum++) {
            SurfWinBmSolAbsdOutsReveal(SurfNum) = 0.0;
            SurfWinBmSolRefldOutsRevealReport(SurfNum) = 0.0;
            SurfWinBmSolAbsdInsReveal(SurfNum) = 0.0;
            SurfWinBmSolRefldInsReveal(SurfNum) = 0.0;
            SurfWinBmSolRefldInsRevealReport(SurfNum) = 0.0;
            SurfWinOutsRevealDiffOntoGlazing(SurfNum) = 0.0;
            SurfWinInsRevealDiffOntoGlazing(SurfNum) = 0.0;
            SurfWinInsRevealDiffIntoZone(SurfNum) = 0.0;
            SurfWinOutsRevealDiffOntoFrame(SurfNum) = 0.0;
            SurfWinInsRevealDiffOntoFrame(SurfNum) = 0.0;
        }

        // Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
        for (int SurfNum = 1; SurfNum <= TotSurfaces; SurfNum++) {
            SurfWinInsRevealDiffOntoGlazingReport(SurfNum) = 0.0;
            SurfWinInsRevealDiffIntoZoneReport(SurfNum) = 0.0;
            SurfWinInsRevealDiffOntoFrameReport(SurfNum) = 0.0;
            SurfWinBmSolAbsdInsRevealReport(SurfNum) = 0.0;
        }

        DisplayString("Initializing Zone and Enclosure Report Variables");
        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            auto &thisEnclosureName = DataViewFactorInformation::ZoneSolarInfo(enclosureNum).Name;
            SetupOutputVariable(state, "Zone Windows Total Transmitted Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                ZoneTransSolar(enclosureNum),
                                "Zone",
                                "Average",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                ZoneBmSolFrExtWinsRep(enclosureNum),
                                "Zone",
                                "Average",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Interior Windows Total Transmitted Beam Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                ZoneBmSolFrIntWinsRep(enclosureNum),
                                "Zone",
                                "Average",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                ZoneDifSolFrExtWinsRep(enclosureNum),
                                "Zone",
                                "Average",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                ZoneDifSolFrIntWinsRep(enclosureNum),
                                "Zone",
                                "Average",
                                thisEnclosureName);
            SetupOutputVariable(state,
                "Zone Windows Total Heat Gain Rate", OutputProcessor::Unit::W, ZoneWinHeatGainRep(enclosureNum), "Zone", "Average", thisEnclosureName);
            SetupOutputVariable(state,
                "Zone Windows Total Heat Loss Rate", OutputProcessor::Unit::W, ZoneWinHeatLossRep(enclosureNum), "Zone", "Average", thisEnclosureName);
            // Energy variables
            SetupOutputVariable(state, "Zone Windows Total Transmitted Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                ZoneTransSolarEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                ZoneBmSolFrExtWinsRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Interior Windows Total Transmitted Beam Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                ZoneBmSolFrIntWinsRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                ZoneDifSolFrExtWinsRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                ZoneDifSolFrIntWinsRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Windows Total Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                ZoneWinHeatGainRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            SetupOutputVariable(state, "Zone Windows Total Heat Loss Energy",
                                OutputProcessor::Unit::J,
                                ZoneWinHeatLossRepEnergy(enclosureNum),
                                "Zone",
                                "Sum",
                                thisEnclosureName);
            }
            for (int ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop) {
                if (DisplayAdvancedReportVariables) {
                // CurrentModuleObject='Zone(Advanced)'
                SetupOutputVariable(state, "Zone Opaque Surface Inside Faces Total Conduction Heat Gain Rate",
                                    OutputProcessor::Unit::W,
                                    ZoneOpaqSurfInsFaceCondGainRep(ZoneLoop),
                                    "Zone",
                                    "Average",
                                    Zone(ZoneLoop).Name);
                SetupOutputVariable(state, "Zone Opaque Surface Inside Faces Total Conduction Heat Loss Rate",
                                    OutputProcessor::Unit::W,
                                    ZoneOpaqSurfInsFaceCondLossRep(ZoneLoop),
                                    "Zone",
                                    "Average",
                                    Zone(ZoneLoop).Name);
                // Energy variables
                SetupOutputVariable(state, "Zone Opaque Surface Inside Faces Total Conduction Heat Gain Energy",
                                    OutputProcessor::Unit::J,
                                    ZnOpqSurfInsFaceCondGnRepEnrg(ZoneLoop),
                                    "Zone",
                                    "Sum",
                                    Zone(ZoneLoop).Name);
                SetupOutputVariable(state, "Zone Opaque Surface Inside Faces Total Conduction Heat Loss Energy",
                                    OutputProcessor::Unit::J,
                                    ZnOpqSurfInsFaceCondLsRepEnrg(ZoneLoop),
                                    "Zone",
                                    "Sum",
                                    Zone(ZoneLoop).Name);
            }
        }

        DisplayString("Initializing Surface (Shading) Report Variables");
        // CurrentModuleObject='Surfaces'
        for (SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop) {
            SetupOutputVariable(state, "Surface Outside Normal Azimuth Angle",
                                OutputProcessor::Unit::deg,
                                Surface(SurfLoop).Azimuth,
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            if (Surface(SurfLoop).ExtSolar) {
                SetupOutputVariable(state,
                    "Surface Outside Face Sunlit Area", OutputProcessor::Unit::m2, SurfSunlitArea(SurfLoop), "Zone", "State", Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Sunlit Fraction",
                                    OutputProcessor::Unit::None,
                                    SurfSunlitFrac(SurfLoop),
                                    "Zone",
                                    "State",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncident(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Beam Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncidentBeam(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncidentSkyDiffuse(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncidentGndDiffuse(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Beam Solar Incident Angle Cosine Value",
                                    OutputProcessor::Unit::None,
                                    SurfCosIncidenceAngle(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Sky Diffuse Ground Reflected Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncSkyDiffReflGnd(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Sky Diffuse Surface Reflected Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncSkyDiffReflObs(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Beam To Beam Surface Reflected Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncBmToBmReflObs(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Beam To Diffuse Surface Reflected Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncBmToDiffReflObs(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Outside Face Incident Beam To Diffuse Ground Reflected Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncBmToDiffReflGnd(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Anisotropic Sky Multiplier",
                                    OutputProcessor::Unit::None,
                                    AnisoSkyMult(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window BSDF Beam Direction Number",
                                    OutputProcessor::Unit::None,
                                    SurfWinBSDFBeamDirectionRep(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window BSDF Beam Theta Angle",
                                    OutputProcessor::Unit::rad,
                                    SurfWinBSDFBeamThetaRep(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window BSDF Beam Phi Angle",
                                    OutputProcessor::Unit::rad,
                                    SurfWinBSDFBeamPhiRep(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
            }
            if (!Surface(SurfLoop).HeatTransSurf) continue;

            if (Surface(SurfLoop).Class == SurfaceClass::Window) {
                // CurrentModuleObject='Windows/GlassDoors'
                if (Surface(SurfLoop).ExtSolar) {
                    SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinQRadSWwinAbsTot(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinSWwinAbsTotalReport(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);

                    if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).WindowTypeBSDF) {
                        NumOfLayers = state.dataConstruction->Construct(Surface(SurfLoop).Construction).TotSolidLayers;
                    } else {
                        NumOfLayers = state.dataConstruction->Construct(Surface(SurfLoop).Construction).TotLayers;
                    }
                    for (I = 1; I <= NumOfLayers; ++I) {
                        if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).WindowTypeBSDF) {
                            SetupOutputVariable(state, "Surface Window Total Absorbed Shortwave Radiation Rate Layer " + RoundSigDigits(I) + "",
                                                OutputProcessor::Unit::W,
                                                SurfWinQRadSWwinAbsLayer(I, SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).WindowTypeBSDF || (I == 1)) {
                            SetupOutputVariable(state, "Surface Window Front Face Temperature Layer " + RoundSigDigits(I) + "",
                                                OutputProcessor::Unit::C,
                                                SurfWinFenLaySurfTempFront(I, SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).WindowTypeBSDF || (I == NumOfLayers)) {
                            SetupOutputVariable(state, "Surface Window Back Face Temperature Layer " + RoundSigDigits(I) + "",
                                                OutputProcessor::Unit::C,
                                                SurfWinFenLaySurfTempBack(I, SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                    }

                    SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinTransSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinBmSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);

                    // added TH 12/9/2009
                    SetupOutputVariable(state, "Surface Window Transmitted Beam To Beam Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinBmBmSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Transmitted Beam To Diffuse Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinBmDifSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);

                    SetupOutputVariable(state, "Surface Window Transmitted Diffuse Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinDifSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Heat Gain Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinHeatGainRep(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Heat Loss Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinHeatLossRep(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Gap Convective Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinGapConvHtFlowRep(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Shading Device Absorbed Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinShadingAbsorbedSolar(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Net Heat Transfer Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinHeatTransfer(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);

                    if (DisplayAdvancedReportVariables) {
                        // CurrentModuleObject='Windows/GlassDoors(Advanced)'
                        SetupOutputVariable(state, "Surface Window Inside Face Glazing Zone Convection Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainConvGlazToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Glazing Net Infrared Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainIRGlazToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Shortwave from Zone Back Out Window Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinLossSWZoneToOutWinRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Frame and Divider Zone Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainFrameDividerToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Gap between Shade and Glazing Zone Convection Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainConvGlazShadGapToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Shade Zone Convection Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainConvShadeToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Shade Net Infrared Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGainIRShadeToZoneRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).WindowTypeEQL) {
                            SetupOutputVariable(state, "Surface Window Inside Face Other Convection Heat Gain Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinOtherConvGainInsideFaceToZoneRep(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                    }

                    // Added TH 12/23/2008 for thermochromic windows
                    // CurrentModuleObject='Thermochromic Windows'
                    if (state.dataConstruction->Construct(Surface(SurfLoop).Construction).TCFlag == 1) {
                        SetupOutputVariable(state, "Surface Window Thermochromic Layer Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinTCLayerTemp(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Thermochromic Layer Property Specification Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinSpecTemp(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                    }

                    // Added TH 5/26/2009 for switchable windows to report switching factor (tinted level)
                    // CurrentModuleObject='Switchable Windows'
                    if (Surface(SurfLoop).HasShadeControl) {
                        if (WindowShadingControl(Surface(SurfLoop).activeWindowShadingControl).ShadingType == WSC_ST_SwitchableGlazing) {
                            // IF (SurfaceWindow(SurfLoop)%ShadingFlag == SwitchableGlazing) THEN  !ShadingFlag is not set to SwitchableGlazing yet!
                            SetupOutputVariable(state, "Surface Window Switchable Glazing Switching Factor",
                                                OutputProcessor::Unit::None,
                                                SurfWinSwitchingFactor(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Switchable Glazing Visible Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinVisTransSelected(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                    }

                    if (SurfWinFrameArea(SurfLoop) > 0.0) {
                        // CurrentModuleObject='Window Frames'
                        SetupOutputVariable(state, "Surface Window Frame Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinFrameHeatGain(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Frame Heat Loss Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinFrameHeatLoss(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Frame Inside Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinFrameTempSurfIn(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Frame Outside Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinFrameTempSurfOut(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                    }
                    if (SurfWinDividerArea(SurfLoop) > 0.0) {
                        // CurrentModuleObject='Window Dividers'
                        SetupOutputVariable(state, "Surface Window Divider Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinDividerHeatGain(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Divider Heat Loss Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinDividerHeatLoss(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Divider Inside Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinDividerTempSurfIn(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Divider Outside Temperature",
                                            OutputProcessor::Unit::C,
                                            SurfWinDividerTempSurfOut(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                    }

                    // CurrentModuleObject='Windows'
                    // Energy
                    SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinQRadSWwinAbsTotEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinTransSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinBmSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);

                    // added TH 12/9/2009
                    SetupOutputVariable(state, "Surface Window Transmitted Beam To Beam Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinBmBmSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Transmitted Beam To Diffuse Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinBmDifSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);

                    SetupOutputVariable(state, "Surface Window Transmitted Diffuse Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinDifSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Heat Gain Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinHeatGainRepEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Heat Loss Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinHeatLossRepEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Gap Convective Heat Transfer Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinGapConvHtFlowRepEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Shading Device Absorbed Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinShadingAbsorbedSolarEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Net Heat Transfer Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinHeatTransferRepEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);

                    SetupOutputVariable(state, "Surface Window System Solar Transmittance",
                                        OutputProcessor::Unit::None,
                                        SurfWinSysSolTransmittance(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window System Solar Reflectance",
                                        OutputProcessor::Unit::None,
                                        SurfWinSysSolReflectance(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window System Solar Absorptance",
                                        OutputProcessor::Unit::None,
                                        SurfWinSysSolAbsorptance(SurfLoop),
                                        "Zone",
                                        "Average",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Inside Face Glazing Condensation Status",
                                        OutputProcessor::Unit::None,
                                        InsideGlassCondensationFlag(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Inside Face Frame Condensation Status",
                                        OutputProcessor::Unit::None,
                                        InsideFrameCondensationFlag(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Inside Face Divider Condensation Status",
                                        OutputProcessor::Unit::None,
                                        InsideDividerCondensationFlag(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);

                    // Outside reveal report variables
                    // IF (Surface(SurfLoop)%Reveal > 0.0) THEN
                    SetupOutputVariable(state, "Surface Window Outside Reveal Reflected Beam Solar Radiation Rate",
                                        OutputProcessor::Unit::W,
                                        SurfWinBmSolRefldOutsRevealReport(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    // Energy
                    SetupOutputVariable(state, "Surface Window Outside Reveal Reflected Beam Solar Radiation Energy",
                                        OutputProcessor::Unit::J,
                                        SurfWinBmSolRefldOutsRevealRepEnergy(SurfLoop),
                                        "Zone",
                                        "Sum",
                                        Surface(SurfLoop).Name);
                    // ENDIF

                    // Inside reveal report variables
                    if (SurfWinInsideReveal(SurfLoop) > 0.0 || SurfWinInsideSillDepth(SurfLoop) > 0.0) {
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmSolRefldInsRevealReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        // Energy
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Beam Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmSolRefldInsRevealRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);

                        // Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
                        // All reflected solar by the inside reveal is turned into diffuse
                        SetupOutputVariable(state, "Surface Window Inside Reveal Absorbed Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmSolAbsdInsRevealReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Diffuse Zone Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinInsRevealDiffIntoZoneReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Diffuse Frame Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinInsRevealDiffOntoFrameReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Diffuse Glazing Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinInsRevealDiffOntoGlazingReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                    }

                    //     Output blind report variables only when blinds are used
                    if (SurfWinBlindNumber(SurfLoop) > 0) {
                        // CurrentModuleObject='Window Blinds'
                        SetupOutputVariable(state, "Surface Window Blind Beam to Beam Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinBlTsolBmBm(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Blind Beam to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinBlTsolBmDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Blind Diffuse to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinBlTsolDifDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Blind and Glazing System Beam Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinBlGlSysTsolBmBm(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Blind and Glazing System Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinBlGlSysTsolDifDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                    }

                    //     Output screen report variables only when screens are used
                    if (SurfWinScreenNumber(SurfLoop) > 0) {
                        // CurrentModuleObject='Window Screens'
                        SetupOutputVariable(state, "Surface Window Screen Beam to Beam Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinScTsolBmBm(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Screen Beam to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinScTsolBmDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Screen Diffuse to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinScTsolDifDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Screen and Glazing System Beam Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinScGlSysTsolBmBm(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Screen and Glazing System Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinScGlSysTsolDifDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                    }

                    // CurrentModuleObject='Windows'
                    SetupOutputVariable(state, "Surface Window Solar Horizontal Profile Angle",
                                        OutputProcessor::Unit::deg,
                                        SurfWinProfileAngHor(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Solar Vertical Profile Angle",
                                        OutputProcessor::Unit::deg,
                                        SurfWinProfileAngVert(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Glazing Beam to Beam Solar Transmittance",
                                        OutputProcessor::Unit::None,
                                        SurfWinGlTsolBmBm(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Glazing Beam to Diffuse Solar Transmittance",
                                        OutputProcessor::Unit::None,
                                        SurfWinGlTsolBmDif(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Glazing Diffuse to Diffuse Solar Transmittance",
                                        OutputProcessor::Unit::None,
                                        SurfWinGlTsolDifDif(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                    SetupOutputVariable(state, "Surface Window Model Solver Iteration Count",
                                        OutputProcessor::Unit::None,
                                        SurfWinWindowCalcIterationsRep(SurfLoop),
                                        "Zone",
                                        "State",
                                        Surface(SurfLoop).Name);
                } else if (!Surface(SurfLoop).ExtSolar) { // Not ExtSolar
                    if (DisplayAdvancedReportVariables) {
                        // CurrentModuleObject='InteriorWindows(Advanced)'
                        if (SurfWinOriginalClass(SurfLoop) != SurfaceClass::TDD_Diffuser) {
                            SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinQRadSWwinAbsTot(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinSWwinAbsTotalReport(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);

                        if (SurfWinOriginalClass(SurfLoop) != SurfaceClass::TDD_Diffuser) {
                            SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinTransSolar(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmSolar(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);

                        // added TH 12/9/2009
                        SetupOutputVariable(state, "Surface Window Transmitted Beam To Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmBmSolar(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Transmitted Beam To Diffuse Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmDifSolar(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);

                        SetupOutputVariable(state, "Surface Window Transmitted Diffuse Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinDifSolar(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Heat Gain Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinHeatGainRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Heat Loss Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinHeatLossRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Gap Convective Heat Transfer Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinGapConvHtFlowRep(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Shading Device Absorbed Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinShadingAbsorbedSolar(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        if (SurfWinFrameArea(SurfLoop) > 0.0) {
                            SetupOutputVariable(state, "Surface Window Frame Heat Gain Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinFrameHeatGain(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Frame Heat Loss Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinFrameHeatLoss(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Frame Inside Temperature",
                                                OutputProcessor::Unit::C,
                                                SurfWinFrameTempSurfIn(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Frame Outside Temperature",
                                                OutputProcessor::Unit::C,
                                                SurfWinFrameTempSurfOut(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        if (SurfWinDividerArea(SurfLoop) > 0.0) {
                            SetupOutputVariable(state, "Surface Window Divider Heat Gain Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinDividerHeatGain(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Divider Heat Loss Rate",
                                                OutputProcessor::Unit::W,
                                                SurfWinDividerHeatLoss(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Divider Inside Temperature",
                                                OutputProcessor::Unit::C,
                                                SurfWinDividerTempSurfIn(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Divider Outside Temperature",
                                                OutputProcessor::Unit::C,
                                                SurfWinDividerTempSurfOut(SurfLoop),
                                                "Zone",
                                                "Average",
                                                Surface(SurfLoop).Name);
                        }
                        // Energy

                        if (SurfWinOriginalClass(SurfLoop) != SurfaceClass::TDD_Diffuser) {
                            SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy",
                                                OutputProcessor::Unit::J,
                                                SurfWinQRadSWwinAbsTotEnergy(SurfLoop),
                                                "Zone",
                                                "Sum",
                                                Surface(SurfLoop).Name);
                        }

                        if (SurfWinOriginalClass(SurfLoop) != SurfaceClass::TDD_Diffuser) {
                            SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Energy",
                                                OutputProcessor::Unit::J,
                                                SurfWinTransSolarEnergy(SurfLoop),
                                                "Zone",
                                                "Sum",
                                                Surface(SurfLoop).Name);
                        }
                        SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmSolarEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);

                        SetupOutputVariable(state, "Surface Window Transmitted Beam To Beam Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmBmSolarEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Transmitted Beam To Diffuse Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmDifSolarEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);

                        SetupOutputVariable(state, "Surface Window Transmitted Diffuse Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinDifSolarEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Heat Gain Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinHeatGainRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Heat Loss Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinHeatLossRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Gap Convective Heat Transfer Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinGapConvHtFlowRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Shading Device Absorbed Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinShadingAbsorbedSolarEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);

                        SetupOutputVariable(state, "Surface Window System Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinSysSolTransmittance(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window System Solar Reflectance",
                                            OutputProcessor::Unit::None,
                                            SurfWinSysSolReflectance(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window System Solar Absorptance",
                                            OutputProcessor::Unit::None,
                                            SurfWinSysSolAbsorptance(SurfLoop),
                                            "Zone",
                                            "Average",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Glazing Condensation Status",
                                            OutputProcessor::Unit::None,
                                            InsideGlassCondensationFlag(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Frame Condensation Status",
                                            OutputProcessor::Unit::None,
                                            InsideFrameCondensationFlag(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Face Divider Condensation Status",
                                            OutputProcessor::Unit::None,
                                            InsideDividerCondensationFlag(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Outside Reveal Reflected Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmSolRefldOutsRevealReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Beam Solar Radiation Rate",
                                            OutputProcessor::Unit::W,
                                            SurfWinBmSolRefldInsRevealReport(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        // Energy
                        SetupOutputVariable(state, "Surface Window Outside Reveal Reflected Beam Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmSolRefldOutsRevealRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Inside Reveal Reflected Beam Solar Radiation Energy",
                                            OutputProcessor::Unit::J,
                                            SurfWinBmSolRefldInsRevealRepEnergy(SurfLoop),
                                            "Zone",
                                            "Sum",
                                            Surface(SurfLoop).Name);

                        //     Output blind report variables only when blinds are used
                        if (SurfWinBlindNumber(SurfLoop) > 0) {
                            SetupOutputVariable(state, "Surface Window Blind Beam to Beam Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinBlTsolBmBm(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Blind Beam to Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinBlTsolBmDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Blind Diffuse to Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinBlTsolDifDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Blind and Glazing System Beam Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinBlGlSysTsolBmBm(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Blind and Glazing System Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinBlGlSysTsolDifDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                        }

                        //     Output screen report variables only when screens are used
                        if (SurfWinScreenNumber(SurfLoop) > 0) {
                            SetupOutputVariable(state, "Surface Window Screen Beam to Beam Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinScTsolBmBm(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Screen Beam to Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinScTsolBmDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Screen Diffuse to Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinScTsolDifDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Screen and Glazing System Beam Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinScGlSysTsolBmBm(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                            SetupOutputVariable(state, "Surface Window Screen and Glazing System Diffuse Solar Transmittance",
                                                OutputProcessor::Unit::None,
                                                SurfWinScGlSysTsolDifDif(SurfLoop),
                                                "Zone",
                                                "State",
                                                Surface(SurfLoop).Name);
                        }

                        SetupOutputVariable(state, "Surface Window Solar Horizontal Profile Angle",
                                            OutputProcessor::Unit::deg,
                                            SurfWinProfileAngHor(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Solar Vertical Profile Angle",
                                            OutputProcessor::Unit::deg,
                                            SurfWinProfileAngVert(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Glazing Beam to Beam Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinGlTsolBmBm(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Glazing Beam to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinGlTsolBmDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Glazing Diffuse to Diffuse Solar Transmittance",
                                            OutputProcessor::Unit::None,
                                            SurfWinGlTsolDifDif(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                        SetupOutputVariable(state, "Surface Window Model Solver Iteration Count",
                                            OutputProcessor::Unit::None,
                                            SurfWinWindowCalcIterationsRep(SurfLoop),
                                            "Zone",
                                            "State",
                                            Surface(SurfLoop).Name);
                    }
                } // end non extsolar reporting as advanced variables
            }     // Window Reporting
            if (Surface(SurfLoop).Class == SurfaceClass::Window && Surface(SurfLoop).ExtBoundCond > 0 &&
                Surface(SurfLoop).ExtBoundCond != SurfLoop) { // Interzone window
                                                              // CurrentModuleObject='InterzoneWindows'
                SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Rate",
                                    OutputProcessor::Unit::W,
                                    SurfWinBmSolTransThruIntWinRep(SurfLoop),
                                    "Zone",
                                    "State",
                                    Surface(SurfLoop).Name);
                // energy
                SetupOutputVariable(state, "Surface Window Transmitted Beam Solar Radiation Energy",
                                    OutputProcessor::Unit::J,
                                    SurfWinBmSolTransThruIntWinRepEnergy(SurfLoop),
                                    "Zone",
                                    "Sum",
                                    Surface(SurfLoop).Name);
            }
            if (Surface(SurfLoop).Class == SurfaceClass::TDD_Dome && Surface(SurfLoop).ExtSolar) {
                // CurrentModuleObject='TDD Domes'
                SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate",
                                    OutputProcessor::Unit::W,
                                    SurfWinQRadSWwinAbsTot(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Rate",
                                    OutputProcessor::Unit::W,
                                    SurfWinTransSolar(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                // energy
                SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy",
                                    OutputProcessor::Unit::J,
                                    SurfWinQRadSWwinAbsTotEnergy(SurfLoop),
                                    "Zone",
                                    "Sum",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Energy",
                                    OutputProcessor::Unit::J,
                                    SurfWinTransSolarEnergy(SurfLoop),
                                    "Zone",
                                    "Sum",
                                    Surface(SurfLoop).Name);
            }
            if (SurfWinOriginalClass(SurfLoop) == SurfaceClass::TDD_Diffuser) {
                // CurrentModuleObject='TDD Diffusers'
                SetupOutputVariable(state, "Surface Outside Face Incident Solar Radiation Rate per Area",
                                    OutputProcessor::Unit::W_m2,
                                    SurfQRadSWOutIncident(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Rate",
                                    OutputProcessor::Unit::W,
                                    SurfWinQRadSWwinAbsTot(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Rate",
                                    OutputProcessor::Unit::W,
                                    SurfWinTransSolar(SurfLoop),
                                    "Zone",
                                    "Average",
                                    Surface(SurfLoop).Name);
                // energy
                SetupOutputVariable(state, "Surface Window Total Glazing Layers Absorbed Solar Radiation Energy",
                                    OutputProcessor::Unit::J,
                                    SurfWinQRadSWwinAbsTotEnergy(SurfLoop),
                                    "Zone",
                                    "Sum",
                                    Surface(SurfLoop).Name);
                SetupOutputVariable(state, "Surface Window Transmitted Solar Radiation Energy",
                                    OutputProcessor::Unit::J,
                                    SurfWinTransSolarEnergy(SurfLoop),
                                    "Zone",
                                    "Sum",
                                    Surface(SurfLoop).Name);
            }
        }

        for (SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop) {
            if (!Surface(SurfLoop).HeatTransSurf) continue;
            // CurrentModuleObject='Surfaces'
            SetupOutputVariable(state, "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                SurfBmIncInsSurfIntensRep(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                SurfBmIncInsSurfAmountRep(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate per Area",
                                OutputProcessor::Unit::W_m2,
                                SurfIntBmIncInsSurfIntensRep(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                SurfIntBmIncInsSurfAmountRep(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Initial Transmitted Diffuse Absorbed Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                SurfInitialDifSolInAbsReport(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Initial Transmitted Diffuse Transmitted Out Window Solar Radiation Rate",
                                OutputProcessor::Unit::W,
                                SurfWinInitialDifSolInTransReport(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Absorbed Shortwave Radiation Rate",
                                OutputProcessor::Unit::W,
                                SurfSWInAbsTotalReport(SurfLoop),
                                "Zone",
                                "Average",
                                Surface(SurfLoop).Name);
            // energy
            SetupOutputVariable(state, "Surface Inside Face Exterior Windows Incident Beam Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                SurfBmIncInsSurfAmountRepEnergy(SurfLoop),
                                "Zone",
                                "Sum",
                                Surface(SurfLoop).Name);
            SetupOutputVariable(state, "Surface Inside Face Interior Windows Incident Beam Solar Radiation Energy",
                                OutputProcessor::Unit::J,
                                SurfIntBmIncInsSurfAmountRepEnergy(SurfLoop),
                                "Zone",
                                "Sum",
                                Surface(SurfLoop).Name);
        }
    }

    void AnisoSkyViewFactors()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   April 1999
        //       MODIFIED       LKL; Dec 2002 -- Anisotropic is only sky radiance option
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates view factor multiplier, AnisoSkyMult, for diffuse
        // sky irradiance on exterior surfaces taking into account
        // anisotropic radiance of the sky. Called by InitSurfaceHeatBalance
        // In this case the diffuse sky irradiance on a surface is given by
        //  AnisoSkyMult(SurfNum) * DifSolarRad
        // AnisoSkyMult accounts not only for the sky radiance distribution but
        // also for the effects of shading of sky diffuse radiation by
        // shadowing surfaces such as overhangs. It does not account for reflection
        // of sky diffuse radiation from shadowing surfaces.
        // Based on an empirical model described in
        // R. Perez, P. Ineichen, R. Seals, J. Michalsky and R. Stewart,
        // "Modeling Daylight Availability and Irradiance Components from Direct
        // and Global Irradiance," Solar Energy 44, 271-289, 1990.
        // In this model the radiance of the sky consists of three distributions
        // that are superimposed:

        // (1) An isotropic distribution that covers the entire sky dome;
        // (2) A circumsolar brightening centered around the position of the sun;
        // (3) A horizon brightening
        // The circumsolar brightening is assumed to be concentrated at a point
        // source at the center of the sun although this region actually begins at the
        // periphery of the solar disk and falls off in intensity with increasing
        // angular distance from the periphery.
        // The horizon brightening is assumed to be concentrated at the horizon and
        // to be independent of azimuth. In actuality, for clear skies, the horizon
        // brightening is highest at the horizon and decreases in intensity away from
        // the horizon. For overcast skies the horizon brightening has a negative value
        // since for such skies the sky radiance increases rather than decreases away
        // from the horizon.
        // The F11R, F12R, etc. values were provided by R. Perez, private communication,
        // 5/21/99. These values have higher precision than those listed in the above
        // paper.

        // Using/Aliasing
        using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D<Real64> const EpsilonLimit(
            7, {1.065, 1.23, 1.5, 1.95, 2.8, 4.5, 6.2}); // Upper limit of bins of the sky clearness parameter, Epsilon
        // Circumsolar brightening coefficients; index corresponds to range of Epsilon, the sky clearness parameter
        static Array1D<Real64> const F11R(8, {-0.0083117, 0.1299457, 0.3296958, 0.5682053, 0.8730280, 1.1326077, 1.0601591, 0.6777470});
        static Array1D<Real64> const F12R(8, {0.5877285, 0.6825954, 0.4868735, 0.1874525, -0.3920403, -1.2367284, -1.5999137, -0.3272588});
        static Array1D<Real64> const F13R(8, {-0.0620636, -0.1513752, -0.2210958, -0.2951290, -0.3616149, -0.4118494, -0.3589221, -0.2504286});
        // Horizon/zenith brightening coefficient array; index corresponds to range of Epsilon, the sky clearness parameter
        static Array1D<Real64> const F21R(8, {-0.0596012, -0.0189325, 0.0554140, 0.1088631, 0.2255647, 0.2877813, 0.2642124, 0.1561313});
        static Array1D<Real64> const F22R(8, {0.0721249, 0.0659650, -0.0639588, -0.1519229, -0.4620442, -0.8230357, -1.1272340, -1.3765031});
        static Array1D<Real64> const F23R(8, {-0.0220216, -0.0288748, -0.0260542, -0.0139754, 0.0012448, 0.0558651, 0.1310694, 0.2506212});

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        Real64 CosZenithAng;           // Cosine of solar zenith angle
        Real64 ZenithAng;              // Solar zenith angle (radians)
        Real64 ZenithAngDeg;           // Solar zenith angle (degrees)
        Real64 F1;                     // Circumsolar brightening coefficient
        Real64 F2;                     // Horizon/zenith brightening coefficient
        Real64 Epsilon;                // Sky clearness parameter
        Real64 Delta;                  // Sky brightness parameter
        Real64 CosIncAngBeamOnSurface; // Cosine of incidence angle of beam solar on surface
        Real64 IncAng;                 // Incidence angle of beam solar on surface (radians)
        int SurfNum;                   // Surface number
        int EpsilonBin;                // Sky clearness (Epsilon) bin index
        Real64 AirMass;                // Relative air mass
        Real64 AirMassH;               // Intermediate variable for relative air mass calculation
        Real64 CircumSolarFac;         // Ratio of cosine of incidence angle to cosine of zenith angle
        Real64 KappaZ3;                // Intermediate variable
        Real64 ViewFactorSkyGeom;      // Geometrical sky view factor
        Real64 const cosine_tolerance(0.0001);

        // FLOW:
#ifdef EP_Count_Calls
        ++NumAnisoSky_Calls;
#endif

        CosZenithAng = SOLCOS(3);
        ZenithAng = std::acos(CosZenithAng);
        ZenithAngDeg = ZenithAng / DataGlobalConstants::DegToRadians();

        AnisoSkyMult = 0.0;

        //           Relative air mass
        AirMassH = (1.0 - 0.1 * Elevation / 1000.0);
        if (ZenithAngDeg <= 75.0) {
            AirMass = AirMassH / CosZenithAng;
        } else {
            AirMass = AirMassH / (CosZenithAng + 0.15 * std::pow(93.9 - ZenithAngDeg, -1.253));
        }
        KappaZ3 = 1.041 * pow_3(ZenithAng);
        Epsilon = ((BeamSolarRad + DifSolarRad) / DifSolarRad + KappaZ3) / (1.0 + KappaZ3);
        Delta = DifSolarRad * AirMass / 1353.0; // 1353 is average extraterrestrial irradiance (W/m2)
        //           Circumsolar (F1) and horizon/zenith (F2) brightening coefficients
        for (EpsilonBin = 1; EpsilonBin <= 8; ++EpsilonBin) {
            if (EpsilonBin == 8) break;
            if (Epsilon < EpsilonLimit(EpsilonBin)) break;
        }
        F1 = max(0.0, F11R(EpsilonBin) + F12R(EpsilonBin) * Delta + F13R(EpsilonBin) * ZenithAng);
        F2 = F21R(EpsilonBin) + F22R(EpsilonBin) * Delta + F23R(EpsilonBin) * ZenithAng;

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!Surface(SurfNum).ExtSolar) continue;

            CosIncAngBeamOnSurface =
                SOLCOS(1) * Surface(SurfNum).OutNormVec(1) + SOLCOS(2) * Surface(SurfNum).OutNormVec(2) + SOLCOS(3) * Surface(SurfNum).OutNormVec(3);

            // So I believe this should only be a diagnostic error...the calcs should always be within -1,+1; it's just round-off that we need to trap
            // for
            if (CosIncAngBeamOnSurface > 1.0) {
                if (CosIncAngBeamOnSurface > (1.0 + cosine_tolerance)) {
                    ShowSevereError("Cosine of incident angle of beam solar on surface out of range...too high");
                    ShowContinueError("This is a diagnostic error that should not be encountered under normal circumstances");
                    ShowContinueError("Occurs on surface: " + Surface(SurfNum).Name);
                    ShowContinueError("Current value = " + TrimSigDigits(CosIncAngBeamOnSurface) + " ... should be within [-1, +1]");
                    ShowFatalError("Anisotropic solar calculation causes fatal error");
                }
                CosIncAngBeamOnSurface = 1.0;
            } else if (CosIncAngBeamOnSurface < -1.0) {
                if (CosIncAngBeamOnSurface < (-1.0 - cosine_tolerance)) {
                    ShowSevereError("Cosine of incident angle of beam solar on surface out of range...too low");
                    ShowContinueError("This is a diagnostic error that should not be encountered under normal circumstances");
                    ShowContinueError("Occurs on surface: " + Surface(SurfNum).Name);
                    ShowContinueError("Current value = " + TrimSigDigits(CosIncAngBeamOnSurface) + " ... should be within [-1, +1]");
                    ShowFatalError("Anisotropic solar calculation causes fatal error");
                }
                CosIncAngBeamOnSurface = -1.0;
            }

            IncAng = std::acos(CosIncAngBeamOnSurface);

            ViewFactorSkyGeom = Surface(SurfNum).ViewFactorSky;
            MultIsoSky(SurfNum) = ViewFactorSkyGeom * (1.0 - F1);
            //           0.0871557 below corresponds to a zenith angle of 85 deg
            CircumSolarFac = max(0.0, CosIncAngBeamOnSurface) / max(0.0871557, CosZenithAng);
            //           For near-horizontal roofs, model has an inconsistency that gives sky diffuse
            //           irradiance significantly different from DifSolarRad when zenith angle is
            //           above 85 deg. The following forces irradiance to be very close to DifSolarRad
            //           in this case.
            if (CircumSolarFac > 0.0 && CosZenithAng < 0.0871557 && Surface(SurfNum).Tilt < 2.0) CircumSolarFac = 1.0;
            MultCircumSolar(SurfNum) = F1 * CircumSolarFac;
            MultHorizonZenith(SurfNum) = F2 * Surface(SurfNum).SinTilt;

            if (!DetailedSkyDiffuseAlgorithm || !ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing) {
                AnisoSkyMult(SurfNum) = MultIsoSky(SurfNum) * DifShdgRatioIsoSky(SurfNum) +
                                        MultCircumSolar(SurfNum) * SunlitFrac(TimeStep, HourOfDay, SurfNum) +
                                        MultHorizonZenith(SurfNum) * DifShdgRatioHoriz(SurfNum);
            } else {
                AnisoSkyMult(SurfNum) = MultIsoSky(SurfNum) * DifShdgRatioIsoSkyHRTS(TimeStep, HourOfDay, SurfNum) +
                                        MultCircumSolar(SurfNum) * SunlitFrac(TimeStep, HourOfDay, SurfNum) +
                                        MultHorizonZenith(SurfNum) * DifShdgRatioHorizHRTS(TimeStep, HourOfDay, SurfNum);
                curDifShdgRatioIsoSky(SurfNum) = DifShdgRatioIsoSkyHRTS(TimeStep, HourOfDay, SurfNum);
            }
            AnisoSkyMult(SurfNum) = max(0.0, AnisoSkyMult(SurfNum)); // make sure not negative.
        }
    }

    void CHKBKS(int const NBS, // Surface Number of the potential back surface
                int const NRS  // Surface Number of the potential shadow receiving surface
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       Nov 2001, FW: Reverse subroutine arguments NRS and NBS to
        //                                    correspond to how CHKBKS is called
        //                      Jan 2002, FW: change error message
        //       RE-ENGINEERED  Lawrie, Oct 2000
        //       Sep 2020: Revised the vector computation method to reliabily produce CVec,
        //                 and simplified the warning messages.

        // PURPOSE OF THIS SUBROUTINE:
        // Determines whether a any vertices of the back surface are in front of the receiving surface;
        // if so, gives severe error.  Only base heat transfer surfaces are checked.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using namespace Vectors;

        int N;                // Loop Control (vertex counter)
        int NVRS;             // Number of vertices of the receiving surface
        int NVBS;             // Number of vertices of the back surface
        Real64 DOTP;          // Dot product of C and D

        // Object Data
        Vector CVec(0.0); // Vector perpendicular to surface at vertex 1
        Vector DVec(0.0); // Vector from vertex 1 of first surface to vertex 'n' of second surface

        NVRS = Surface(NRS).Sides;
        NVBS = Surface(NBS).Sides;

        // SEE IF ANY VERTICES OF THE back surface ARE IN FRONT OF THE receiving surface

        for (N = 2; N < NVRS; N++) {
            CVec += cross(Surface(NRS).Vertex(N) - Surface(NRS).Vertex(1),
                Surface(NRS).Vertex((N + 1)) - Surface(NRS).Vertex(1));
        }
        CVec /= (NVRS>=3 ? NVRS : 3);

        for (N = 1; N <= NVBS; ++N) {
            DVec = Surface(NBS).Vertex(N) - Surface(NRS).Vertex(1);
            DOTP = dot(CVec, DVec);
            if (DOTP > 0.0009) {
                ShowSevereError("Problem in interior solar distribution calculation (CHKBKS)");
                ShowContinueError("   Solar Distribution = FullInteriorExterior will not work in Zone=" + Surface(NRS).ZoneName);
                ShowContinueError("   because one or more of vertices, such as Vertex " + std::to_string(N) + " of back surface=" + Surface(NBS).Name +
                                  ", is in front of receiving surface=" + Surface(NRS).Name);
                ShowContinueError(format("   (Dot Product indicator={:20.4F})", DOTP));
                ShowContinueError("   Check surface geometry; if OK, use Solar Distribution = FullExterior instead. Use Output:Diagnostics, DisplayExtraWarnings; for more details.");
                if (!EnergyPlus::DataGlobals::DisplayExtraWarnings) break;
            }
        }
    }

    void CHKGSS(int const NRS,     // Surface number of the potential shadow receiving surface
                int const NSS,     // Surface number of the potential shadow casting surface
                Real64 const ZMIN, // Lowest point of the receiving surface
                bool &CannotShade  // TRUE if shadow casting surface cannot shade receiving surface.
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // Determines the possible shadowing combinations.  The
        // routine checks detached shadowing or base heat transfer surfaces
        // for the possibility that they cannot shade a given base heat transfer surface.

        // METHODOLOGY EMPLOYED:
        // Shadowing is not possible if:
        // 1.  The lowest point of the shadow receiving surface (receiving surface)
        //     Is higher than the highest point of the shadow casting surface (s.s.)
        // 2.  The shadow casting surface Faces up (e.g. A flat roof)
        // 3.  The shadow casting surface Is behind the receiving surface
        // 4.  The receiving surface is behind the shadow casting surface

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using namespace Vectors;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Real64 TolValue(0.0003);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Object Data

        CannotShade = true;

        // see if no point of shadow casting surface is above low point of receiving surface

        auto const &surface_C(Surface(NSS));
        if (surface_C.OutNormVec(3) > 0.9999) return; // Shadow Casting Surface is horizontal and facing upward
        auto const &vertex_C(surface_C.Vertex);
        Real64 ZMAX(vertex_C(1).z);
        for (int i = 2, e = surface_C.Sides; i <= e; ++i) {
            ZMAX = std::max(ZMAX, vertex_C(i).z);
        }
        if (ZMAX <= ZMIN) return;

        // SEE IF ANY VERTICES OF THE Shadow Casting Surface ARE ABOVE THE PLANE OF THE receiving surface

        auto const &surface_R(Surface(NRS));
        auto const &vertex_R(surface_R.Vertex);
        auto const vertex_R_2(vertex_R(2));
        Vector const AVec(vertex_R(1) - vertex_R_2); // Vector from vertex 2 to vertex 1 of receiving surface
        Vector const BVec(vertex_R(3) - vertex_R_2); // Vector from vertex 2 to vertex 3 of receiving surface

        Vector const CVec(cross(BVec, AVec)); // Vector perpendicular to surface at vertex 2

        int const NVSS = surface_C.Sides; // Number of vertices of the shadow casting surface
        Real64 DOTP(0.0);                 // Dot Product
        for (int I = 1; I <= NVSS; ++I) {
            DOTP = dot(CVec, vertex_C(I) - vertex_R_2);
            if (DOTP > TolValue) break; // DO loop
        }

        // SEE IF ANY VERTICES OF THE receiving surface ARE ABOVE THE PLANE OF THE S.S.

        if (DOTP > TolValue) {

            auto const vertex_C_2(vertex_C(2));
            Vector const AVec(vertex_C(1) - vertex_C_2);
            Vector const BVec(vertex_C(3) - vertex_C_2);

            Vector const CVec(cross(BVec, AVec));

            int const NVRS = surface_R.Sides; // Number of vertices of the receiving surface
            for (int I = 1; I <= NVRS; ++I) {
                DOTP = dot(CVec, vertex_R(I) - vertex_C_2);
                if (DOTP > TolValue) {
                    CannotShade = false;
                    break; // DO loop
                }
            }
        }
    }

    void CHKSBS(EnergyPlusData &state, int const HTS,   // Heat transfer surface number of the general receiving surf
                int const GRSNR, // Surface number of general receiving surface
                int const SBSNR  // Surface number of subsurface
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // Checks that a subsurface is completely
        // enclosed by its base surface.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // 3D Planar Polygons
        // In 3D applications, one sometimes wants to test a point and polygon that are in the same plane.
        // For example, one may have the intersection point of a ray with the plane of a polyhedron's face,
        // and want to test if it is inside the face.  Or one may want to know if the base of a 3D perpendicular
        // dropped from a point is inside a planar polygon.

        // 3D inclusion is easily determined by projecting the point and polygon into 2D.  To do this, one simply
        // ignores one of the 3D coordinates and uses the other two.  To optimally select the coordinate to ignore,
        // compute a normal vector to the plane, and select the coordinate with the largest absolute value [Snyder & Barr, 1987].
        // This gives the projection of the polygon with maximum area, and results in robust computations.
        // John M. Snyder & Alan H. Barr, "Ray Tracing Complex Models Containing Surface Tessellations",
        // Computer Graphics 21(4), 119-126 (1987) [also in the Proceedings of SIGGRAPH 1987]
        //--- using adapted routine from Triangulation code -- EnergyPlus.

        // MSG - for error message
        static Array1D_string const MSG(4, {"misses", "", "within", "overlaps"});

        int N;                      // Loop Control
        int NVT;                    // Number of vertices
        static Array1D<Real64> XVT; // X Vertices of
        static Array1D<Real64> YVT; // Y vertices of
        static Array1D<Real64> ZVT; // Z vertices of

        int NS1; // Number of the figure being overlapped
        int NS2; // Number of the figure doing overlapping
        int NS3; // Location to place results of overlap

        bool inside;

        bool Out;
        Real64 X1; // ,SX,SY,SZ
        Real64 Y1;
        Real64 Z1;
        Real64 X2;
        Real64 Y2;
        Real64 Z2;
        Real64 BX;
        Real64 BY;
        Real64 BZ;
        Real64 BMAX;
        //  INTEGER M

        if (state.dataSolarShading->CHKSBSOneTimeFlag) {
            XVT.allocate(MaxVerticesPerSurface + 1);
            YVT.allocate(MaxVerticesPerSurface + 1);
            ZVT.allocate(MaxVerticesPerSurface + 1);
            XVT = 0.0;
            YVT = 0.0;
            ZVT = 0.0;
            state.dataSolarShading->CHKSBSOneTimeFlag = false;
        }

        NS1 = 1;
        NS2 = 2;
        NS3 = 3;
        state.dataSolarShading->HCT(1) = 0.0;
        state.dataSolarShading->HCT(2) = 0.0;

        // Put coordinates of base surface into clockwise sequence on the x'-y' plane.

        XVT = 0.0;
        YVT = 0.0;
        ZVT = 0.0;
        state.dataSolarShading->XVS = 0.0;
        state.dataSolarShading->YVS = 0.0;
        CTRANS(GRSNR, HTS, NVT, XVT, YVT, ZVT);
        for (N = 1; N <= NVT; ++N) {
            state.dataSolarShading->XVS(N) = XVT(NVT + 1 - N);
            state.dataSolarShading->YVS(N) = YVT(NVT + 1 - N);
        }

        HTRANS1(state, NS2, NVT);

        // Put coordinates of the subsurface into clockwise sequence.

        state.dataSolarShading->NVS = Surface(SBSNR).Sides;
        for (N = 1; N <= state.dataSolarShading->NVS; ++N) {
            state.dataSolarShading->XVS(N) = ShadeV(SBSNR).XV(state.dataSolarShading->NVS + 1 - N);
            state.dataSolarShading->YVS(N) = ShadeV(SBSNR).YV(state.dataSolarShading->NVS + 1 - N);
        }
        HTRANS1(state, NS1, state.dataSolarShading->NVS);

        // Determine the overlap condition.

        DeterminePolygonOverlap(state, NS1, NS2, NS3);

        // Print error condition if necessary.

        if (state.dataSolarShading->OverlapStatus != state.dataSolarShading->FirstSurfWithinSecond) {
            Out = false;
            // C                            COMPUTE COMPONENTS OF VECTOR
            // C                            NORMAL TO BASE SURFACE.
            X1 = Surface(GRSNR).Vertex(1).x - Surface(GRSNR).Vertex(2).x; // XV(1,GRSNR)-XV(2,GRSNR)
            Y1 = Surface(GRSNR).Vertex(1).y - Surface(GRSNR).Vertex(2).y; // YV(1,GRSNR)-YV(2,GRSNR)
            Z1 = Surface(GRSNR).Vertex(1).z - Surface(GRSNR).Vertex(2).z; // ZV(1,GRSNR)-ZV(2,GRSNR)
            X2 = Surface(GRSNR).Vertex(3).x - Surface(GRSNR).Vertex(2).x; // XV(3,GRSNR)-XV(2,GRSNR)
            Y2 = Surface(GRSNR).Vertex(3).y - Surface(GRSNR).Vertex(2).y; // YV(3,GRSNR)-YV(2,GRSNR)
            Z2 = Surface(GRSNR).Vertex(3).z - Surface(GRSNR).Vertex(2).z; // ZV(3,GRSNR)-ZV(2,GRSNR)
            BX = Y1 * Z2 - Y2 * Z1;
            BY = Z1 * X2 - Z2 * X1;
            BZ = X1 * Y2 - X2 * Y1;
            // C                            FIND LARGEST COMPONENT.
            BMAX = max(std::abs(BX), std::abs(BY), std::abs(BZ));
            // C
            if (std::abs(BX) == BMAX) {
                //        write(outputfiledebug,*) ' looking bx-bmax',bmax
                for (N = 1; N <= Surface(SBSNR).Sides; ++N) { // NV(SBSNR)
                    inside = polygon_contains_point(Surface(GRSNR).Sides, Surface(GRSNR).Vertex, Surface(SBSNR).Vertex(N), true, false, false);
                    if (!inside) {
                        Out = true;
                        //            do m=1,surface(grsnr)%sides
                        //            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
                        //            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
                        //            enddo
                        //            EXIT
                    }
                    //          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
                    //          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
                    //          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
                    //            Y2 = Y1
                    //            Z2 = Z1
                    //            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
                    //            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
                    //            SX = Y1*Z2-Y2*Z1
                    //            IF(SX*BX.LT.-1.0d-6) THEN
                    //              OUT=.TRUE.
                    //              write(outputfiledebug,*) 'sx*bx=',sx*bx
                    //              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
                    //              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
                    //            endif
                    //          ENDDO
                    //          IF (OUT) EXIT
                }
            } else if (std::abs(BY) == BMAX) {
                //        write(outputfiledebug,*) ' looking by-bmax',bmax
                for (N = 1; N <= Surface(SBSNR).Sides; ++N) { // NV(SBSNR)
                    inside = polygon_contains_point(Surface(GRSNR).Sides, Surface(GRSNR).Vertex, Surface(SBSNR).Vertex(N), false, true, false);
                    if (!inside) {
                        Out = true;
                        //            do m=1,surface(grsnr)%sides
                        //            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
                        //            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
                        //            enddo
                        //            EXIT
                    }
                    //          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
                    //          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
                    //          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
                    //            Z2 = Z1
                    //            X2 = X1
                    //            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
                    //            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
                    //            SY = Z1*X2-Z2*X1
                    //            IF(SY*BY.LT.-1.0d-6) THEN
                    //              OUT=.TRUE.
                    //              write(outputfiledebug,*) 'sy*by=',sy*by
                    //              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
                    //              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
                    //            ENDIF
                    //          ENDDO
                    //          IF (OUT) EXIT
                }
            } else {
                //        write(outputfiledebug,*) ' looking bz-bmax',bmax
                for (N = 1; N <= Surface(SBSNR).Sides; ++N) { // NV(SBSNR)
                    inside = polygon_contains_point(Surface(GRSNR).Sides, Surface(GRSNR).Vertex, Surface(SBSNR).Vertex(N), false, false, true);
                    if (!inside) {
                        Out = true;
                        //            do m=1,surface(grsnr)%sides
                        //            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
                        //            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
                        //            enddo
                        //            EXIT
                    }
                    //          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
                    //          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
                    //          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
                    //            X2 = X1
                    //            Y2 = Y1
                    //            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
                    //            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
                    //            SZ = X1*Y2-X2*Y1
                    //            IF(SZ*BZ.LT.-1.0d-6) THEN
                    //              OUT=.TRUE.
                    //              write(outputfiledebug,*) 'sz*bz=',sz*bz
                    //              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
                    //              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
                    //            ENDIF
                    //          ENDDO
                    //          IF (OUT) EXIT
                }
            }
            //    CALL ShowWarningError('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
            //                           TRIM(cOverLapStatus(OverlapStatus)))
            //    CALL ShowContinueError('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
            //                     ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"')
            //    IF (FirstSurroundError) THEN
            //      CALL ShowWarningError('Base Surface does not surround subsurface errors occuring...'//  &
            //                     'Check that the SurfaceGeometry object is expressing the proper starting corner and '//  &
            //                     'direction [CounterClockwise/Clockwise]')
            //      FirstSurroundError=.FALSE.
            //    ENDIF
            if (Out) {
                state.dataSolarShading->TrackBaseSubSurround.redimension(++state.dataSolarShading->NumBaseSubSurround);
                state.dataSolarShading->TrackBaseSubSurround(state.dataSolarShading->NumBaseSubSurround).SurfIndex1 = GRSNR;
                state.dataSolarShading->TrackBaseSubSurround(state.dataSolarShading->NumBaseSubSurround).SurfIndex2 = SBSNR;
                state.dataSolarShading->TrackBaseSubSurround(state.dataSolarShading->NumBaseSubSurround).MiscIndex = state.dataSolarShading->OverlapStatus;
                //    CALL ShowRecurringWarningErrorAtEnd('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
                //                       TRIM(cOverLapStatus(OverlapStatus)), &
                //                       TrackBaseSubSurround(GRSNR)%ErrIndex1)
                //    CALL ShowRecurringContinueErrorAtEnd('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
                //                       ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"',  &
                //                      TrackBaseSubSurround(SBSNR)%ErrIndex2)
                if (shd_stream) {
                    *shd_stream << "==== Base does not Surround subsurface details ====\n";
                    *shd_stream << "Surface=" << Surface(GRSNR).Name << ' ' << state.dataSolarShading->cOverLapStatus(state.dataSolarShading->OverlapStatus) << '\n';
                    *shd_stream << "Surface#=" << std::setw(5) << GRSNR << " NSides=" << std::setw(5) << Surface(GRSNR).Sides << '\n';
                    *shd_stream << std::fixed << std::setprecision(2);
                    for (N = 1; N <= Surface(GRSNR).Sides; ++N) {
                        Vector const &v(Surface(GRSNR).Vertex(N));
                        *shd_stream << "Vertex " << std::setw(5) << N << "=(" << std::setw(15) << v.x << ',' << std::setw(15) << v.y << ','
                                    << std::setw(15) << v.z << ")\n";
                    }
                    *shd_stream << "SubSurface=" << Surface(SBSNR).Name << '\n';
                    *shd_stream << "Surface#=" << std::setw(5) << SBSNR << " NSides=" << std::setw(5) << Surface(SBSNR).Sides << '\n';
                    for (N = 1; N <= Surface(SBSNR).Sides; ++N) {
                        Vector const &v(Surface(SBSNR).Vertex(N));
                        *shd_stream << "Vertex " << std::setw(5) << N << "=(" << std::setw(15) << v.x << ',' << std::setw(15) << v.y << ','
                                    << std::setw(15) << v.z << ")\n";
                    }
                    *shd_stream << "================================\n";
                }
            }
        }
    }

    bool polygon_contains_point(int const nsides,            // number of sides (vertices)
                                Array1D<Vector> &polygon_3d, // points of polygon
                                Vector const &point_3d,      // point to be tested
                                bool const ignorex,
                                bool const ignorey,
                                bool const ignorez)
    {

        // Function information:
        //       Author         Linda Lawrie
        //       Date written   October 2005
        //       Modified       na
        //       Re-engineered  na

        // Purpose of this function:
        // Determine if a point is inside a simple 2d polygon.  For a simple polygon (one whose
        // boundary never crosses itself).  The polygon does not need to be convex.


        // References:
        // M Shimrat, Position of Point Relative to Polygon, ACM Algorithm 112,
        // Communications of the ACM, Volume 5, Number 8, page 434, August 1962.

        // Use statements:
        // Using/Aliasing
        using namespace DataVectorTypes;

        // Return value
        bool inside; // return value, true=inside, false = not inside

        EP_SIZE_CHECK(polygon_3d, nsides);

        int i;
        int ip1;

        // Object Data
        Array1D<Vector_2d> polygon(nsides);
        Vector_2d point;

        inside = false;
        if (ignorex) {
            for (int i = 1; i <= nsides; ++i) {
                polygon(i).x = polygon_3d(i).y;
                polygon(i).y = polygon_3d(i).z;
            }
            point.x = point_3d.y;
            point.y = point_3d.z;
        } else if (ignorey) {
            for (int i = 1; i <= nsides; ++i) {
                polygon(i).x = polygon_3d(i).x;
                polygon(i).y = polygon_3d(i).z;
            }
            point.x = point_3d.x;
            point.y = point_3d.z;
        } else if (ignorez) {
            for (int i = 1; i <= nsides; ++i) {
                polygon(i).x = polygon_3d(i).x;
                polygon(i).y = polygon_3d(i).y;
            }
            point.x = point_3d.x;
            point.y = point_3d.y;
        } else { // Illegal
            assert(false);
            point.x = point.y = 0.0; // Elim possibly used uninitialized warnings
        }

        for (i = 1; i <= nsides; ++i) {

            if (i < nsides) {
                ip1 = i + 1;
            } else {
                ip1 = 1;
            }

            if ((polygon(i).y < point.y && point.y <= polygon(ip1).y) || (point.y <= polygon(i).y && polygon(ip1).y < point.y)) {
                if ((point.x - polygon(i).x) - (point.y - polygon(i).y) * (polygon(ip1).x - polygon(i).x) / (polygon(ip1).y - polygon(i).y) < 0) {
                    inside = !inside;
                }
            }
        }

        return inside;
    }

    void ComputeIntSolarAbsorpFactors(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       MODIFIED       B. Griffith, Oct 2010, deal with no floor case
        //                      L. Lawrie, Mar 2012, relax >154 tilt even further (>120 considered non-wall by ASHRAE)
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine computes the fractions of diffusely transmitted
        // solar energy absorbed by each zone surface.

        // METHODOLOGY EMPLOYED:
        // It is assumed that all transmitted solar energy is incident
        // on the floors of the zone (or enclosure).  The fraction directly absorbed in
        // the floor is given by 'ISABSF'.  It is proportional to the
        // area * solar absorptance.  The remaining solar energy is then
        // distributed uniformly around the room according to
        // area*absorptance product

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        using General::RoundSigDigits;
        using namespace DataWindowEquivalentLayer;

        Real64 AreaSum;       // Intermediate calculation value
        int Lay;              // Window glass layer number
        Real64 AbsDiffTotWin; // Sum of a window's glass layer solar absorptances
        Real64 TestFractSum;
        Real64 HorizAreaSum;

        if (!allocated(state.dataSolarShading->ISABSF)) {
            state.dataSolarShading->ISABSF.allocate(TotSurfaces);
        }
        state.dataSolarShading->ISABSF = 0.0;

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));

            AreaSum = 0.0;
            TestFractSum = 0.0;
            for (int const SurfNum : thisEnclosure.SurfacePtr) {
                if (Zone(Surface(SurfNum).Zone).OfType == StandardZone && Surface(SurfNum).CosTilt < -0.5) {
                    AreaSum += Surface(SurfNum).Area;
                }
            }

            HorizAreaSum = AreaSum;

            if ((thisEnclosure.FloorArea <=0.0) && (HorizAreaSum > 0.0)) {
                // fill floor area even though surfs not called "Floor", they are roughly horizontal and face upwards.
                thisEnclosure.FloorArea = HorizAreaSum;
                ShowWarningError("ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor,");
                ShowContinueError("...Enclosure=\"" + thisEnclosure.Name + "\" has no floor, but has approximate horizontal surfaces.");
                ShowContinueError("...these Tilt > 120 degrees, (area=[" + RoundSigDigits(HorizAreaSum, 2) + "] m2) will be used.");
            }

            // Compute ISABSF

            for (int const SurfNum : thisEnclosure.SurfacePtr) {

                // only horizontal surfaces. !      !CR 8229, relaxed from -0.99 to -0.5  (Tilt > 154)
                // only horizontal surfaces. !      !CR8769 use ASHRAE std of >120, -0.9 to -0.5  (Tilt > 120)
                if ((Zone(Surface(SurfNum).Zone).OfType != StandardZone || Surface(SurfNum).CosTilt < -0.5) &&
                    (Zone(Surface(SurfNum).Zone).OfType == StandardZone || Surface(SurfNum).ExtBoundCond > 0)) {

                    int ConstrNum = Surface(SurfNum).Construction;
                    // last minute V3.1
                    if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                        if (AreaSum > 0.0) state.dataSolarShading->ISABSF(SurfNum) = Surface(SurfNum).Area * state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar / AreaSum;
                    } else { // Window (floor windows are assumed to have no shading device and no divider,
                        // and assumed to be non-switchable)
                        if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;
                        AbsDiffTotWin = 0.0;
                        if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).WindowTypeEQL) {
                            for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotGlassLayers; ++Lay) {
                                AbsDiffTotWin += state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay);
                            }
                        } else {
                            for (Lay = 1; Lay <= CFS(state.dataConstruction->Construct(ConstrNum).EQLConsPtr).NL; ++Lay) {
                                AbsDiffTotWin += state.dataConstruction->Construct(ConstrNum).AbsDiffBackEQL(Lay);
                            }
                        }
                        if (AreaSum > 0.0) state.dataSolarShading->ISABSF(SurfNum) = Surface(SurfNum).Area * AbsDiffTotWin / AreaSum;
                    }
                }
                // CR 8229  test ISABSF for problems
                TestFractSum += state.dataSolarShading->ISABSF(SurfNum);
            }

            if (TestFractSum <= 0.0) {
                if (thisEnclosure.ExtWindowArea > 0.0) { // we have a problem, the sun has no floor to go to
                    if (thisEnclosure.FloorArea <= 0.0) {
                        ShowSevereError("ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor,");
                        ShowContinueError("but Zone or Enclosure =\"" + thisEnclosure.Name + "\" does not appear to have any floor surfaces.");
                        ShowContinueError("Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues...");
                    } else { // Floor Area > 0 but still can't absorb
                        ShowSevereError("ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains on the zone floor,");
                        ShowContinueError("but Zone or Enclosure =\"" + thisEnclosure.Name + "\" floor cannot absorb any solar gains. ");
                        ShowContinueError("Check the solar absorptance of the inside layer of the floor surface construction/material.");
                        ShowContinueError("Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues...");
                    }

                    // try again but use an even spread across all the surfaces in the zone, regardless of horizontal
                    //  so as to not lose solar energy
                    AreaSum = 0.0;
                    for (int SurfNum : thisEnclosure.SurfacePtr) {
                        AreaSum += Surface(SurfNum).Area;
                    }

                    for (int const SurfNum : thisEnclosure.SurfacePtr) {
                        int ConstrNum = Surface(SurfNum).Construction;
                        if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) { // Opaque surface
                            if (AreaSum > 0.0) state.dataSolarShading->ISABSF(SurfNum) = Surface(SurfNum).Area * state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar / AreaSum;
                        } else { // Window (floor windows are assumed to have no shading device and no divider,
                            // and assumed to be non-switchable)
                            if (SurfWinStormWinFlag(SurfNum) == 1) ConstrNum = Surface(SurfNum).StormWinConstruction;
                            AbsDiffTotWin = 0.0;
                            if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).WindowTypeEQL) {
                                for (Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNum).TotGlassLayers; ++Lay) {
                                    AbsDiffTotWin += state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay);
                                }
                            } else {
                                for (Lay = 1; Lay <= CFS(state.dataConstruction->Construct(ConstrNum).EQLConsPtr).NL; ++Lay) {
                                    AbsDiffTotWin += state.dataConstruction->Construct(ConstrNum).AbsDiffBackEQL(Lay);
                                }
                            }

                            if (AreaSum > 0.0) state.dataSolarShading->ISABSF(SurfNum) = Surface(SurfNum).Area * AbsDiffTotWin / AreaSum;
                        }
                    }
                }
            }

        } // enclosure loop
    }

    void CLIP(EnergyPlusData &state, int const NVT, Array1D<Real64> &XVT, Array1D<Real64> &YVT, Array1D<Real64> &ZVT)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine 'clips' the shadow casting surface polygon so that
        // none of it lies below the plane of the receiving surface polygon.  This
        // prevents the casting of 'false' shadows.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        int NABOVE(0);    // Number of vertices of shadow casting surface. above the plane of receiving surface
        int NEXT(0);      // First vertex above plane of receiving surface
        int NON(0);       // Number of vertices of shadow casting surface. on plane of receiving surface
        Real64 XIN(0.0);  // X of entry point of shadow casting surface. into plane of receiving surface
        Real64 XOUT(0.0); // X of exit point of shadow casting surface. from plane of receiving surface
        Real64 YIN(0.0);  // Y of entry point of shadow casting surface. into plane of receiving surface
        Real64 YOUT(0.0); // Y of exit point of shadow casting surface. from plane of receiving surface
        //  INTEGER NVS      ! Number of vertices of the shadow/clipped surface

        // Determine if the shadow casting surface. is above, below, or intersects with the plane of the receiving surface

        state.dataSolarShading->NumVertInShadowOrClippedSurface = state.dataSolarShading->NVS;
        for (int N = 1; N <= NVT; ++N) {
            Real64 const ZVT_N(ZVT(N));
            if (ZVT_N > 0.0) {
                ++NABOVE;
            } else if (ZVT_N == 0.0) {
                ++NON;
            }
        }

        if (NABOVE + NON == NVT) { // Rename the unclipped shadow casting surface.

            state.dataSolarShading->NVS = NVT;
            state.dataSolarShading->NumVertInShadowOrClippedSurface = NVT;
            for (int N = 1; N <= NVT; ++N) {
                state.dataSolarShading->XVC(N) = XVT(N);
                state.dataSolarShading->YVC(N) = YVT(N);
                state.dataSolarShading->ZVC(N) = ZVT(N);
            }

        } else if (NABOVE == 0) { // Totally submerged shadow casting surface.

            state.dataSolarShading->NVS = 0;
            state.dataSolarShading->NumVertInShadowOrClippedSurface = 0;

        } else { // Remove (clip) that portion of the shadow casting surface. which is below the receiving surface

            state.dataSolarShading->NVS = NABOVE + 2;
            state.dataSolarShading->NumVertInShadowOrClippedSurface = NABOVE + 2;
            Real64 ZVT_N, ZVT_P(ZVT(1));
            XVT(NVT + 1) = XVT(1);
            YVT(NVT + 1) = YVT(1);
            ZVT(NVT + 1) = ZVT_P;
            for (int N = 1, P = 2; N <= NVT; ++N, ++P) {
                ZVT_N = ZVT_P;
                ZVT_P = ZVT(P);
                if (ZVT_N >= 0.0 && ZVT_P < 0.0) { // Line enters plane of receiving surface
                    Real64 const ZVT_fac(1.0 / (ZVT_P - ZVT_N));
                    XIN = (ZVT_P * XVT(N) - ZVT_N * XVT(P)) * ZVT_fac;
                    YIN = (ZVT_P * YVT(N) - ZVT_N * YVT(P)) * ZVT_fac;
                } else if (ZVT_N <= 0.0 && ZVT_P > 0.0) { // Line exits plane of receiving surface
                    NEXT = N + 1;
                    Real64 const ZVT_fac(1.0 / (ZVT_P - ZVT_N));
                    XOUT = (ZVT_P * XVT(N) - ZVT_N * XVT(P)) * ZVT_fac;
                    YOUT = (ZVT_P * YVT(N) - ZVT_N * YVT(P)) * ZVT_fac;
                }
            }

            // Renumber the vertices of the clipped shadow casting surface. so they are still counter-clockwise sequential.

            state.dataSolarShading->XVC(1) = XOUT; //? Verify that the IN and OUT values were ever set?
            state.dataSolarShading->YVC(1) = YOUT;
            state.dataSolarShading->ZVC(1) = 0.0;
            state.dataSolarShading->XVC(state.dataSolarShading->NVS) = XIN;
            state.dataSolarShading->YVC(state.dataSolarShading->NVS) = YIN;
            state.dataSolarShading->ZVC(state.dataSolarShading->NVS) = 0.0;
            for (int N = 1; N <= NABOVE; ++N) {
                if (NEXT > NVT) NEXT = 1;
                state.dataSolarShading->XVC(N + 1) = XVT(NEXT);
                state.dataSolarShading->YVC(N + 1) = YVT(NEXT);
                state.dataSolarShading->ZVC(N + 1) = ZVT(NEXT);
                ++NEXT;
            }
        }
    }

    void CTRANS(int const NS,         // Surface number whose vertex coordinates are being transformed
                int const NGRS,       // Base surface number for surface NS
                int &NVT,             // Number of vertices for surface NS
                Array1D<Real64> &XVT, // XYZ coordinates of vertices of NS in plane of NGRS
                Array1D<Real64> &YVT,
                Array1D<Real64> &ZVT)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // Transforms the general coordinates of the vertices
        // of surface NS to coordinates in the plane of the receiving surface NGRS.
        // See subroutine 'CalcCoordinateTransformation' SurfaceGeometry Module.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton
        // NECAP subroutine 'SHADOW'

        Real64 Xdif; // Intermediate Result
        Real64 Ydif; // Intermediate Result
        Real64 Zdif; // Intermediate Result

        // Tuned
        auto const &surface(Surface(NS));
        auto const &base_surface(Surface(NGRS));
        auto const &base_lcsx(base_surface.lcsx);
        auto const &base_lcsy(base_surface.lcsy);
        auto const &base_lcsz(base_surface.lcsz);
        Real64 const base_X0(X0(NGRS));
        Real64 const base_Y0(Y0(NGRS));
        Real64 const base_Z0(Z0(NGRS));

        NVT = surface.Sides;

        // Perform transformation
        for (int N = 1; N <= NVT; ++N) {
            auto const &vertex(surface.Vertex(N));

            Xdif = vertex.x - base_X0;
            Ydif = vertex.y - base_Y0;
            Zdif = vertex.z - base_Z0;

            if (std::abs(Xdif) <= 1.E-15) Xdif = 0.0;
            if (std::abs(Ydif) <= 1.E-15) Ydif = 0.0;
            if (std::abs(Zdif) <= 1.E-15) Zdif = 0.0;

            XVT(N) = base_lcsx.x * Xdif + base_lcsx.y * Ydif + base_lcsx.z * Zdif;
            YVT(N) = base_lcsy.x * Xdif + base_lcsy.y * Ydif + base_lcsy.z * Zdif;
            ZVT(N) = base_lcsz.x * Xdif + base_lcsz.y * Ydif + base_lcsz.z * Zdif;
        }
    }

    void HTRANS(EnergyPlusData &state, int const I,          // Mode selector: 0 - Compute H.C. of sides
                int const NS,         // Figure Number
                int const NumVertices // Number of vertices
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets up the homogeneous coordinates.
        // This routine converts the cartesian coordinates of a surface
        // or shadow polygon to homogeneous coordinates.  It also
        // computes the area of the polygon.

        // METHODOLOGY EMPLOYED:
        // Note: Original legacy code used integer arithmetic (tests in subroutines
        // INCLOS and INTCPT are sensitive to round-off error).  However, porting to Fortran 77
        // (BLAST/IBLAST) required some variables to become REAL(r64) instead.

        // Notes on homogeneous coordinates:
        // A point (X,Y) is represented by a 3-element vector
        // (W*X,W*Y,W), where W may be any REAL(r64) number except 0.  a line
        // is also represented by a 3-element vector (A,B,C).  The
        // directed line (A,B,C) from point (W*X1,W*Y1,W) to point
        // (V*X2,V*Y2,V) is given by (A,B,C) = (W*X1,W*Y1,W) cross
        // (V*X2,V*Y2,V).  The sequence of the cross product is a
        // convention to determine sign.  The condition that a point lie
        // on a line is that (A,B,C) dot (W*X,W*Y,W) = 0.  'Normalize'
        // the representation of a point by setting W to 1.  Then if
        // (A,B,C) dot (X,Y,1) > 0.0, The point is to the left of the
        // line, and if it is less than zero, the point is to the right
        // of the line.  The intercept of two lines is given by
        // (W*X,W*Y,W) = (A1,B1,C1) cross (A2,B2,C3).

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton
        // W. M. Newman & R. F. Sproull, 'Principles of Interactive Computer Graphics', Appendix II, McGraw-Hill, 1973.
        // 'CRC Math Tables', 22 ED, 'Analytic Geometry', P.369

        // Using/Aliasing
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //                1 - Compute H.C. of vertices & sides

        if (NS > 2 * state.dataSolarShading->MaxHCS) {
            ShowFatalError("Solar Shading: HTrans: Too many Figures (>" + TrimSigDigits(state.dataSolarShading->MaxHCS) + ')');
        }

        state.dataSolarShading->HCNV(NS) = NumVertices;

        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCB));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCC));
        auto const l1(state.dataSolarShading->HCX.index(NS, 1));
        if (I != 0) { // Transform vertices of figure ns.

            // See comment at top of module regarding HCMULT
            auto l(l1);
            for (int N = 1; N <= NumVertices; ++N, ++l) { // [ l ] == ( NS, N )
                state.dataSolarShading->HCX[l] = nint64(state.dataSolarShading->XVS(N) * state.dataSolarShading->HCMULT);
                state.dataSolarShading->HCY[l] = nint64(state.dataSolarShading->YVS(N) * state.dataSolarShading->HCMULT);
            }
        }

        // Establish extra point for finding lines between points.

        auto l(state.dataSolarShading->HCX.index(NS, NumVertices + 1));
        Int64 HCX_m(state.dataSolarShading->HCX[l] = state.dataSolarShading->HCX[l1]); // [ l ] == ( NS, NumVertices + 1 ), [ l1 ] == ( NS, 1 )
        Int64 HCY_m(state.dataSolarShading->HCY[l] = state.dataSolarShading->HCY[l1]); // [ l ] == ( NS, NumVertices + 1 ), [ l1 ] == ( NS, 1 )

        // Determine lines between points.
        l = l1;
        auto m(l1 + 1u);
        Int64 HCX_l;
        Int64 HCY_l;
        Real64 SUM(0.0);                                   // Sum variable
        for (int N = 1; N <= NumVertices; ++N, ++l, ++m) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
            HCX_l = HCX_m;
            HCY_l = HCY_m;
            HCX_m = state.dataSolarShading->HCX[m];
            HCY_m = state.dataSolarShading->HCY[m];
            state.dataSolarShading->HCA[l] = HCY_l - HCY_m;
            state.dataSolarShading->HCB[l] = HCX_m - HCX_l;
            SUM += state.dataSolarShading->HCC[l] = (HCY_m * HCX_l) - (HCX_m * HCY_l);
        }

        // Compute area of polygon.
        //  SUM=0.0D0
        //  DO N = 1, NumVertices
        //    SUM = SUM + HCX(N,NS)*HCY(N+1,NS) - HCY(N,NS)*HCX(N+1,NS) ! Since HCX and HCY integerized, value of SUM should be ok
        //  END DO
        state.dataSolarShading->HCAREA(NS) = SUM * state.dataSolarShading->sqHCMULT_fac;
        //  HCAREA(NS)=0.5d0*SUM*(kHCMULT)
    }

    void HTRANS0(EnergyPlusData &state, int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    )
    {
        // Using/Aliasing
        using General::TrimSigDigits;

        // Locals

        if (NS > 2 * state.dataSolarShading->MaxHCS) {
            ShowFatalError("Solar Shading: HTrans0: Too many Figures (>" + TrimSigDigits(state.dataSolarShading->MaxHCS) + ')');
        }

        state.dataSolarShading->HCNV(NS) = NumVertices;

        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCB));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCC));

        auto const l1(state.dataSolarShading->HCX.index(NS, 1));

        auto l(state.dataSolarShading->HCX.index(NS, NumVertices + 1));
        Int64 HCX_m(state.dataSolarShading->HCX[l] = state.dataSolarShading->HCX[l1]); // [ l1 ] == ( NS, 1 )
        Int64 HCY_m(state.dataSolarShading->HCY[l] = state.dataSolarShading->HCY[l1]); // [ l1 ] == ( NS, 1 )

        l = l1;
        auto m(l1 + 1u);
        Int64 HCX_l;
        Int64 HCY_l;
        Real64 SUM(0.0);
        for (int N = 1; N <= NumVertices; ++N, ++l, ++m) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
            HCX_l = HCX_m;
            HCY_l = HCY_m;
            HCX_m = state.dataSolarShading->HCX[m];
            HCY_m = state.dataSolarShading->HCY[m];
            state.dataSolarShading->HCA[l] = HCY_l - HCY_m;
            state.dataSolarShading->HCB[l] = HCX_m - HCX_l;
            SUM += state.dataSolarShading->HCC[l] = (HCY_m * HCX_l) - (HCX_m * HCY_l);
        }

        state.dataSolarShading->HCAREA(NS) = SUM * state.dataSolarShading->sqHCMULT_fac;
    }

    void HTRANS1(EnergyPlusData &state, int const NS,         // Figure Number
                 int const NumVertices // Number of vertices
    )
    {
        // Using/Aliasing
        using General::TrimSigDigits;

        if (NS > 2 * state.dataSolarShading->MaxHCS) {
            ShowFatalError("Solar Shading: HTrans1: Too many Figures (>" + TrimSigDigits(state.dataSolarShading->MaxHCS) + ')');
        }

        state.dataSolarShading->HCNV(NS) = NumVertices;

        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCB));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCC));

        auto const l1(state.dataSolarShading->HCX.index(NS, 1));

        // only in HTRANS1
        auto l(l1);
        for (int N = 1; N <= NumVertices; ++N, ++l) { // [ l ] == ( NS, N )
            state.dataSolarShading->HCX[l] = nint64(state.dataSolarShading->XVS(N) * state.dataSolarShading->HCMULT);
            state.dataSolarShading->HCY[l] = nint64(state.dataSolarShading->YVS(N) * state.dataSolarShading->HCMULT);
        }

        l = state.dataSolarShading->HCX.index(NS, NumVertices + 1);
        Int64 HCX_m(state.dataSolarShading->HCX[l] = state.dataSolarShading->HCX[l1]); // [ l1 ] == ( NS, 1 )
        Int64 HCY_m(state.dataSolarShading->HCY[l] = state.dataSolarShading->HCY[l1]);

        l = l1;
        auto m(l1 + 1u);
        Int64 HCX_l;
        Int64 HCY_l;
        Real64 SUM(0.0);
        for (int N = 1; N <= NumVertices; ++N, ++l, ++m) { // [ l ] == ( NS, N ), [ m ] == ( NS, N + 1 )
            HCX_l = HCX_m;
            HCY_l = HCY_m;
            HCX_m = state.dataSolarShading->HCX[m];
            HCY_m = state.dataSolarShading->HCY[m];
            state.dataSolarShading->HCA[l] = HCY_l - HCY_m;
            state.dataSolarShading->HCB[l] = HCX_m - HCX_l;
            SUM += state.dataSolarShading->HCC[l] = (HCY_m * HCX_l) - (HCX_m * HCY_l);
        }

        state.dataSolarShading->HCAREA(NS) = SUM * state.dataSolarShading->sqHCMULT_fac;
    }

    void INCLOS(EnergyPlusData &state, int const N1,            // Figure number of figure 1
                int const N1NumVert,     // Number of vertices of figure 1
                int const N2,            // Figure number of figure 2
                int const N2NumVert,     // Number of vertices of figure 2
                int &NumVerticesOverlap, // Number of vertices which overlap
                int &NIN                 // Number of vertices of figure 1 within figure 2
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines which vertices of figure N1 lie within figure N2.

        // METHODOLOGY EMPLOYED:
        // For vertex N of figure N1 to lie within figure N2, it must be
        // on or to the right of all sides of figure N2, assuming
        // figure N2 is convex.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        int K;              // Vertex number of the overlap
        int M;              // Side number of figure N2
        int N;              // Vertex number of figure N1
        bool CycleMainLoop; // Sets when to cycle main loop
        Real64 HFunct;

        NIN = 0;

        for (N = 1; N <= N1NumVert; ++N) {

            CycleMainLoop = false;

            // Eliminate cases where vertex N is to the left of side M.

            for (M = 1; M <= N2NumVert; ++M) {
                HFunct = state.dataSolarShading->HCX(N1, N) * state.dataSolarShading->HCA(N2, M) + state.dataSolarShading->HCY(N1, N) * state.dataSolarShading->HCB(N2, M) + state.dataSolarShading->HCC(N2, M);
                if (HFunct > 0.0) {
                    CycleMainLoop = true; // Set to cycle to the next value of N
                    break;                // M DO loop
                }
            }

            if (CycleMainLoop) continue;
            ++NIN;

            // Check for duplication of previously determined points.

            if (NumVerticesOverlap != 0) {
                for (K = 1; K <= NumVerticesOverlap; ++K) {
                    if ((state.dataSolarShading->XTEMP(K) == state.dataSolarShading->HCX(N1, N)) && (state.dataSolarShading->YTEMP(K) == state.dataSolarShading->HCY(N1, N))) {
                        CycleMainLoop = true; // Set to cycle to the next value of N
                        break;                // K DO loop
                    }
                }
                if (CycleMainLoop) continue;
            }

            // Record enclosed vertices in temporary arrays.

            ++NumVerticesOverlap;
            state.dataSolarShading->XTEMP(NumVerticesOverlap) = state.dataSolarShading->HCX(N1, N);
            state.dataSolarShading->YTEMP(NumVerticesOverlap) = state.dataSolarShading->HCY(N1, N);
        }
    }

    void INTCPT(EnergyPlusData &state, int const NV1, // Number of vertices of figure NS1
                int const NV2, // Number of vertices of figure NS2
                int &NV3,      // Number of vertices of figure NS3
                int const NS1, // Number of the figure being overlapped
                int const NS2  // Number of the figure doing overlapping
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines all intercepts between the sides of figure NS1
        // and the sides of figure NS2.

        // METHODOLOGY EMPLOYED:
        // The requirements for intersection are that the end points of
        // line N lie on both sides of line M and vice versa.  Also
        // eliminate cases where the end point of one line lies exactly
        // on the other to reduce duplication with the enclosed points.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        Real64 W;        // Normalization factor
        Real64 XUntrunc; // Untruncated X coordinate
        Real64 YUntrunc; // Untruncated Y coordinate
        Int64 I1;        // Intermediate result for testing intersection
        Int64 I2;        // Intermediate result for testing intersection
        int K;
        int KK;
        int M; // Side number of figure NS2
        int N; // Side number of figure NS1

        for (N = 1; N <= NV1; ++N) {
            for (M = 1; M <= NV2; ++M) {

                // Eliminate cases where sides N and M do not intersect.

                I1 = state.dataSolarShading->HCA(NS1, N) * state.dataSolarShading->HCX(NS2, M) + state.dataSolarShading->HCB(NS1, N) * state.dataSolarShading->HCY(NS2, M) + state.dataSolarShading->HCC(NS1, N);
                I2 = state.dataSolarShading->HCA(NS1, N) * state.dataSolarShading->HCX(NS2, M + 1) + state.dataSolarShading->HCB(NS1, N) * state.dataSolarShading->HCY(NS2, M + 1) + state.dataSolarShading->HCC(NS1, N);
                if (I1 >= 0 && I2 >= 0) continue;
                if (I1 <= 0 && I2 <= 0) continue;

                I1 = state.dataSolarShading->HCA(NS2, M) * state.dataSolarShading->HCX(NS1, N) + state.dataSolarShading->HCB(NS2, M) * state.dataSolarShading->HCY(NS1, N) + state.dataSolarShading->HCC(NS2, M);
                I2 = state.dataSolarShading->HCA(NS2, M) * state.dataSolarShading->HCX(NS1, N + 1) + state.dataSolarShading->HCB(NS2, M) * state.dataSolarShading->HCY(NS1, N + 1) + state.dataSolarShading->HCC(NS2, M);
                if (I1 >= 0 && I2 >= 0) continue;
                if (I1 <= 0 && I2 <= 0) continue;

                // Determine the point of intersection and record in the temporary array.

                KK = NV3;
                ++NV3;
                W = state.dataSolarShading->HCB(NS2, M) * state.dataSolarShading->HCA(NS1, N) - state.dataSolarShading->HCA(NS2, M) * state.dataSolarShading->HCB(NS1, N);
                XUntrunc = (state.dataSolarShading->HCC(NS2, M) * state.dataSolarShading->HCB(NS1, N) - state.dataSolarShading->HCB(NS2, M) * state.dataSolarShading->HCC(NS1, N)) / W;
                YUntrunc = (state.dataSolarShading->HCA(NS2, M) * state.dataSolarShading->HCC(NS1, N) - state.dataSolarShading->HCC(NS2, M) * state.dataSolarShading->HCA(NS1, N)) / W;
                if (NV3 > isize(state.dataSolarShading->XTEMP)) {
                    //        write(outputfiledebug,*) 'nv3=',nv3,' SIZE(xtemp)=',SIZE(xtemp)
                    state.dataSolarShading->XTEMP.redimension(isize(state.dataSolarShading->XTEMP) + 10, 0.0);
                    state.dataSolarShading->YTEMP.redimension(isize(state.dataSolarShading->YTEMP) + 10, 0.0);
                }
                state.dataSolarShading->XTEMP(NV3) = nint64(XUntrunc);
                state.dataSolarShading->YTEMP(NV3) = nint64(YUntrunc);

                // Eliminate near-duplicate points.

                if (KK != 0) {
                    auto const x(state.dataSolarShading->XTEMP(NV3));
                    auto const y(state.dataSolarShading->YTEMP(NV3));
                    for (K = 1; K <= KK; ++K) {
                        if (std::abs(x - state.dataSolarShading->XTEMP(K)) > 2.0) continue;
                        if (std::abs(y - state.dataSolarShading->YTEMP(K)) > 2.0) continue;
                        NV3 = KK;
                        break; // K DO loop
                    }
                }
            }
        }
    }

    inline bool neq(Real64 a, Real64 b) {
        return std::abs(a-b) > 2.0;
    }

    inline bool d_eq(Real64 a, Real64 b) {
        return std::abs(a-b) < 2.0;
    }

    void CLIPLINE(Real64 &x1, Real64 &x2, Real64 &y1, Real64 &y2,
                  Real64 maxX, Real64 minX, Real64 maxY, Real64 minY, bool &visible, bool &rev)  {
        // Line segment clipping
        // Reference:
        // Slater, M., Barsky, B.A.
        // 2D line and polygon clipping based on space subdivision.
        // The Visual Computer 10, 407–422 (1994).
        Real64 dx, dy, e, xinc, yinc, tempVar;
        bool needX = true, needY = true;
        int c1, c2;

        if (x1 > x2) { //reverse for efficiency
            tempVar = x1;
            x1 = x2;
            x2 = tempVar;
            tempVar = y1;
            y1 = y2;
            y2 = tempVar;
            rev = true;
        }
        if (x1 > maxX || x2 < minX) return; // x is positive
        if (x1 < minX) {
            if (y1 < minY) {
                if (y2 < minY) return;
                c1 = 0;
                dx = x2 - x1;
                dy = y2 - y1;
                e = dy*(minX-x1) + dx*(y1-minY);
            } else if (y1 > maxY) {
                if (y2 > maxY) return;
                c1 = 6;
                dx = x2 - x1;
                dy = y2 - y1;
                e = dy*(minX-x1) + dx*(y1-maxY);
            } else {
                c1 = 3;
                dx = x2 - x1;
                dy = y2 - y1;
                if (dy > 0) {
                    e = dy*(minX-x1) + dx*(y1-maxY);
                } else {
                    e = dy*(minX-x1) + dx*(y1-minY);
                }
            }
        } else {
            if (y1 < minY) {
                if (y2 < minY) return;
                c1 = 1;
                dx = x2 - x1;
                dy = y2 - y1;
                e = dy*(maxX-x1) + dx*(y1-minY);
            } else if (y1 > maxY) {
                if (y2 > maxY) return;
                c1 = 7;
                dx = x2 - x1;
                dy = y2 - y1;
                e = dy*(maxX-x1) + dx*(y1-maxY);
            } else {
                visible = true;
                if (x2 <= maxX && (y2 >= minY && y2 <= maxY)) return;
                c1 = 4;
                dx = x2 - x1;
                dy = y2 - y1;
                if (dy > 0) {
                    e = dy*(maxX-x1) + dx*(y1-maxY);
                } else {
                    e = dy*(maxX-x1) + dx*(y1-minY);
                }
            }
        }
        c2 = c1;
        if (dy > 0) {
            while(true) {
                if (e < 0.0) {
                    if (c2 == 1) return;
                    else if (c2 == 3) {
                        visible = true;
                        x1 = minX;
                        y1 = maxY + e/dx;
                        if (x2 <= maxX && y2 <= maxY) return;
                    } else if (c2 == 4) {
                        x2 = maxX;
                        y2 = maxY + e/dx;
                        return;
                    }
                    if (needX) {
                        xinc = dy*(maxX - minX);
                        needX = false;
                    }
                    e += xinc;
                    c2 += 1;
                } else {
                    if (c2 == 3) return;
                    else if (c2 == 1) {
                        visible = true;
                        x1 = maxX - e/dy;
                        y1 = minY;
                        if (x2 <= maxX && y2 <= maxY) return;
                    } else if (c2 == 4) {
                        x2 = maxX - e/dy;
                        y2 = maxY;
                        return;
                    }
                    if (needY) {
                        yinc = dx*(maxY - minY);
                        needY = false;
                    }
                    e -= yinc;
                    c2 += 3;
                }
            }
        } else {
            while(true) {
                if (e >= 0.0) {
                    if (c2 == 7) return;
                    else if (c2 == 3) {
                        visible = true;
                        x1 = minX;
                        y1 = minY + e/dx;
                        if (x2 <= maxX && y2 >= minY) return;
                    } else if (c2 == 4) {
                        x2 = maxX;
                        y2 = minY + e/dx;
                        return;
                    }
                    if (needX) {
                        xinc = dy*(maxX - minX);
                        needX = false;
                    }
                    e += xinc;
                    c2 += 1;
                } else {
                    if (c2 == 3) return;
                    else if (c2 == 7) {
                        visible = true;
                        x1 = maxX - e/dy;
                        y1 = maxY;
                        if (x2 <= maxX && y2 >= minY) return;
                    } else if (c2 == 4) {
                        x2 = maxX - e/dy;
                        y2 = minY;
                        return;
                    }
                    if (needY) {
                        yinc = dx*(maxY - minY);
                        needY = false;
                    }
                    e += yinc;
                    c2 -= 3;
                }
            }
        }
    }

    void CLIPRECT(EnergyPlusData &state, int const NS2, int const NV1, int &NV3) {
        // Polygon clipping by line segment clipping for rectangles
        // Reference:
        // Slater, M., Barsky, B.A.
        // 2D line and polygon clipping based on space subdivision.
        // The Visual Computer 10, 407–422 (1994).
        bool INTFLAG = false;
        auto l(state.dataSolarShading->HCA.index(NS2, 1));
        Real64 maxX, minX, maxY, minY;
        if (state.dataSolarShading->HCX[l] > state.dataSolarShading->HCX[l+2]) {
            maxX = state.dataSolarShading->HCX[l];
            minX = state.dataSolarShading->HCX[l+2];
        } else {
            maxX = state.dataSolarShading->HCX[l+2];
            minX = state.dataSolarShading->HCX[l];
        }
        if (state.dataSolarShading->HCY[l] > state.dataSolarShading->HCY[l+2]) {
            maxY = state.dataSolarShading->HCY[l];
            minY = state.dataSolarShading->HCY[l+2];
        } else {
            maxY = state.dataSolarShading->HCY[l+2];
            minY = state.dataSolarShading->HCY[l];
        }

        Real64 arrx[20]; //Temp array for output X
        Real64 arry[20]; //Temp array for output Y
        int arrc = 0; //Number of items in output

        for (int j = 0; j < NV1; ++j) {
            Real64 x_1 = state.dataSolarShading->XTEMP[j];
            Real64 y_1 = state.dataSolarShading->YTEMP[j];
            Real64 x_2 = state.dataSolarShading->XTEMP[(j+1) % NV1];
            Real64 y_2 = state.dataSolarShading->YTEMP[(j+1) % NV1];
            Real64 x1 = x_1, x2 = x_2, y1 = y_1, y2 = y_2;

            bool visible = false;
            bool rev = false;
            CLIPLINE(x_1, x_2,y_1, y_2, maxX, minX, maxY, minY, visible, rev);
            if (visible) {
                if ((x_1 != x1 || y_1 != y1) || (x_2 != x2 || y_2 != y2)) {
                    INTFLAG = true;
                }
                if (rev) { //undo reverse
                    auto tempVar = x_1;
                    x_1 = x_2;
                    x_2 = tempVar;
                    tempVar = y_1;
                    y_1 = y_2;
                    y_2 = tempVar;
                }
                //if line on edge, or inside, add both points
                if (arrc == 0 || ((neq(arrx[arrc-1], x_1) || neq(arry[arrc-1], y_1)) && (neq(arrx[0], x_1) || neq(arry[0], y_1)))) {
                    arrx[arrc] = x_1;
                    arry[arrc] = y_1;
                    arrc += 1;
                    if ((neq(x_1, x_2) || neq(y_1, y_2)) && (neq(arrx[0], x_2) || neq(arry[0], y_2))) {
                        arrx[arrc] = x_2;
                        arry[arrc] = y_2;
                        arrc += 1;
                    }
                } else if ((neq(arrx[arrc-1], x_2) || neq(arry[arrc-1], y_2)) && (neq(arrx[0], x_2) || neq(arry[0], y_2))) {
                    arrx[arrc] = x_2;
                    arry[arrc] = y_2;
                    arrc += 1;
                }
            }
        }
        NV3 = arrc;

        // Re-populate XTEMP/YTEMP
        if (NV3 > 1) {
            int LastEdgeIndex = -1, incr = 0;
            double cornerXs[4] = {minX, minX, maxX, maxX};
            double cornerYs[4] = {minY, maxY, maxY, minY};
            Real64 edges[4] = { minX, maxY, maxX, minY };
            Real64 LastEdgeX, LastEdgeY;
            for (int i = 0; i <= arrc; i++) {
                int k = i % arrc;

                Real64 currX = arrx[k], currY = arry[k];

                int edgeCount = 0, EdgeIndex = -1;
                for (int m = 0; m < 4; m++) {
                    if (m%2 == 0 && d_eq(currX, edges[m])) { //MinX or MaxX
                        edgeCount ++;
                        EdgeIndex = m;
                    } else if (m%2 == 1 && d_eq(currY, edges[m])) {
                        edgeCount ++;
                        EdgeIndex = m;
                    }
                }
                if (edgeCount == 0) { //On inside
                    if (i != arrc) {
                        state.dataSolarShading->XTEMP[incr] = currX;
                        state.dataSolarShading->YTEMP[incr] = currY;
                        incr ++;
                    }
                    continue;
                } else if (edgeCount > 1) { //On corner
                    if (d_eq(currX, minX)) {
                        if (d_eq(currY, minY)) {
                            EdgeIndex = 3;
                        } else {
                            EdgeIndex = 0;
                        }
                    } else {
                        if (d_eq(currY, maxY)) {
                            EdgeIndex = 1;
                        } else {
                            EdgeIndex = 2;
                        }
                    }
                }
                if ((LastEdgeIndex > -1 && EdgeIndex > -1) && LastEdgeIndex != EdgeIndex) {
                    int jumpCount = 0;
                    if ((EdgeIndex == 0 && LastEdgeIndex == 3) || (EdgeIndex - LastEdgeIndex == 1)) {
                        jumpCount = 1;
                    } else if (EdgeIndex%2 == LastEdgeIndex%2) {
                        //Clockwise double jump
                        jumpCount = 2;
                    } else if ((EdgeIndex == 3 && LastEdgeIndex == 0) || (LastEdgeIndex - EdgeIndex == 1)) {
                        //Clockwise triple jump
                        jumpCount = 3;
                    }
                    if (jumpCount > 0) {
                        Real64 cornerX;
                        Real64 cornerY;
                        int startIndex = (LastEdgeIndex + 1) % 4;
                        int added = 0;
                        for (int i1 = startIndex, j1 = 0; j1 < jumpCount; i1 = (i1 + 1) % 4, j1++) {
                            cornerX = cornerXs[i1];
                            cornerY = cornerYs[i1];
                            if (cornerX == LastEdgeX && cornerY == LastEdgeY) continue; //skip if jump started on corner

                            bool insideFlag = true;
                            for (int j = 0; j < NV1; ++j) {
                                if ((state.dataSolarShading->ATEMP[j] * cornerX) + (cornerY * state.dataSolarShading->BTEMP[j]) + state.dataSolarShading->CTEMP[j] > 0.0) {
                                    insideFlag = false;
                                    break;
                                }
                            }

                            if (insideFlag && (incr == 0 || ((neq(cornerX, state.dataSolarShading->XTEMP[incr-1]) || neq(cornerY, state.dataSolarShading->YTEMP[incr-1])) && (neq(cornerX, state.dataSolarShading->XTEMP[0]) || neq(cornerY, state.dataSolarShading->YTEMP[0])))))
                            {
                                state.dataSolarShading->XTEMP[incr] = cornerX;
                                state.dataSolarShading->YTEMP[incr] = cornerY;
                                incr ++;
                                added ++;
                            }
                        }
                        if (jumpCount > 2 && (added == jumpCount && edgeCount == 1)) {
                            if (i != arrc) {
                                state.dataSolarShading->XTEMP[incr] = currX;
                                state.dataSolarShading->YTEMP[incr] = currY;
                                incr ++;
                            }
                            break;
                        }
                    }
                }
                if (i != arrc) {
                    state.dataSolarShading->XTEMP[incr] = currX;
                    state.dataSolarShading->YTEMP[incr] = currY;
                    incr ++;
                }
                LastEdgeIndex = EdgeIndex;
                LastEdgeX = currX;
                LastEdgeY = currY;
            }
            NV3 = incr;

        } else {
            if (NV3 == 1) {
                state.dataSolarShading->XTEMP[0] = arrx[0];
                state.dataSolarShading->YTEMP[0] = arry[0];
            } if (NV3 == 0) {
                double cornerXs[4] = {minX, minX, maxX, maxX};
                double cornerYs[4] = {minY, maxY, maxY, minY};
                Real64 cornerX = cornerXs[0];
                Real64 cornerY = cornerYs[0];
                bool insideFlag = true;
                for (int j = 0; j < NV1; ++j) {
                    if ((state.dataSolarShading->ATEMP[j] * cornerX) + (cornerY * state.dataSolarShading->BTEMP[j]) + state.dataSolarShading->CTEMP[j] >= 0.0) {
                        insideFlag = false;
                        break;
                    }
                }
                if (insideFlag) {
                    for (int i1 = 0; i1 < 4; i1++){
                        state.dataSolarShading->XTEMP[i1] = cornerXs[i1];
                        state.dataSolarShading->YTEMP[i1] = cornerYs[i1];
                    }
                    NV3 = 4;
                    INTFLAG = true;
                }
            }
        }

        // update homogenous edges A,B,C
        if (NV3 > 0) {
            Real64 const X_0(state.dataSolarShading->XTEMP[0]);
            Real64 const Y_0(state.dataSolarShading->YTEMP[0]);
            Real64 XP_0 = X_0, XP_1;
            Real64 YP_0 = Y_0, YP_1;
            for (int P = 0; P < NV3-1; ++P) {
                XP_1 = state.dataSolarShading->XTEMP[P + 1];
                YP_1 = state.dataSolarShading->YTEMP[P + 1];

                state.dataSolarShading->ATEMP[P] = YP_0 - YP_1;
                state.dataSolarShading->BTEMP[P] = XP_1 - XP_0;
                state.dataSolarShading->CTEMP[P] = XP_0 * YP_1 - YP_0 * XP_1;
                XP_0 = XP_1;
                YP_0 = YP_1;
            }

            state.dataSolarShading->ATEMP[NV3-1] = YP_1 - Y_0;
            state.dataSolarShading->BTEMP[NV3-1] = X_0 - XP_1;
            state.dataSolarShading->CTEMP[NV3-1] = XP_1 * Y_0 - YP_1 * X_0;
        }

        //Determine overlap status
        if (NV3 < 3) { // Determine overlap status
            state.dataSolarShading->OverlapStatus = state.dataSolarShading->NoOverlap;
        } else if (!INTFLAG) {
            state.dataSolarShading->OverlapStatus = state.dataSolarShading->FirstSurfWithinSecond;
        }
    }


    void CLIPPOLY(EnergyPlusData &state, int const NS1, // Figure number of figure 1 (The subject polygon)
                  int const NS2, // Figure number of figure 2 (The clipping polygon)
                  int const NV1, // Number of vertices of figure 1
                  int const NV2, // Number of vertices of figure 2
                  int &NV3       // Number of vertices of figure 3
    ) {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Tyler Hoyt
        //       DATE WRITTEN   May 4, 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Populate global arrays XTEMP and YTEMP with the vertices
        // of the overlap between NS1 and NS2, and determine relevant
        // overlap status.

        // METHODOLOGY EMPLOYED:
        // The Sutherland-Hodgman algorithm for polygon clipping is employed.

        using General::ReallocateRealArray;
        using General::SafeDivide;

        typedef Array2D<Int64>::size_type size_type;
        bool INTFLAG; // For overlap status
        int S;        // Test vertex
        int KK;       // Duplicate test index
        int NVOUT;    // Current output length for loops
        int NVTEMP;

        Real64 W; // Normalization factor
        Real64 HFunct;

#ifdef EP_Count_Calls
        ++NumClipPoly_Calls;
#endif
        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCB));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCC));

        // Populate the arrays with the original polygon
        for (size_type j = 0, l = state.dataSolarShading->HCX.index(NS1, 1), e = NV1; j < e; ++j, ++l) {
            state.dataSolarShading->XTEMP[j] = state.dataSolarShading->HCX[l]; // [ l ] == ( NS1, j+1 )
            state.dataSolarShading->YTEMP[j] = state.dataSolarShading->HCY[l];
            state.dataSolarShading->ATEMP[j] = state.dataSolarShading->HCA[l];
            state.dataSolarShading->BTEMP[j] = state.dataSolarShading->HCB[l];
            state.dataSolarShading->CTEMP[j] = state.dataSolarShading->HCC[l];
        }

        NVOUT = NV1; // First point-loop is the length of the subject polygon.
        INTFLAG = false;
        NVTEMP = 0;
        KK = 0;

        //Check if clipping polygon is rectangle
        if (DataSystemVariables::SlaterBarsky) {
            auto l1(state.dataSolarShading->HCA.index(NS2, 1));
            bool rectFlag = ((NV2 == 4) && (((((state.dataSolarShading->HCX[l1] == state.dataSolarShading->HCX[l1 + 1] && state.dataSolarShading->HCY[l1] != state.dataSolarShading->HCY[l1 + 1]) &&
                                               ((state.dataSolarShading->HCY[l1 + 2] == state.dataSolarShading->HCY[l1 + 1] && state.dataSolarShading->HCY[l1 + 3] == state.dataSolarShading->HCY[l1]))) &&
                                              state.dataSolarShading->HCX[l1 + 2] == state.dataSolarShading->HCX[l1 + 3]) ||
                                             ((((state.dataSolarShading->HCY[l1] == state.dataSolarShading->HCY[l1 + 1] && state.dataSolarShading->HCX[l1] != state.dataSolarShading->HCX[l1 + 1]) &&
                                                (state.dataSolarShading->HCX[l1 + 2] == state.dataSolarShading->HCX[l1 + 1] && state.dataSolarShading->HCX[l1 + 3] == state.dataSolarShading->HCX[l1])) &&
                                               (state.dataSolarShading->HCY[l1 + 2] == state.dataSolarShading->HCY[l1 + 3]))))));
            if (rectFlag) {
                CLIPRECT(state, NS2, NV1, NV3);
                return;
            }
        }

        auto l(state.dataSolarShading->HCA.index(NS2, 1));
        for (int E = 1; E <= NV2; ++E, ++l) { // Loop over edges of the clipping polygon
            for (int P = 1; P <= NVOUT; ++P) {
                state.dataSolarShading->XTEMP1(P) = state.dataSolarShading->XTEMP(P);
                state.dataSolarShading->YTEMP1(P) = state.dataSolarShading->YTEMP(P);
            }
            S = NVOUT;
            Real64 const HCA_E(state.dataSolarShading->HCA[l]);
            Real64 const HCB_E(state.dataSolarShading->HCB[l]);
            Real64 const HCC_E(state.dataSolarShading->HCC[l]);
            Real64 XTEMP1_S(state.dataSolarShading->XTEMP1(S));
            Real64 YTEMP1_S(state.dataSolarShading->YTEMP1(S));
            for (int P = 1; P <= NVOUT; ++P) {
                Real64 const XTEMP1_P(state.dataSolarShading->XTEMP1(P));
                Real64 const YTEMP1_P(state.dataSolarShading->YTEMP1(P));
                HFunct = XTEMP1_P * HCA_E + YTEMP1_P * HCB_E + HCC_E;
                // S is constant within this block
                if (HFunct <= 0.0) { // Vertex is not in the clipping plane
                    HFunct = XTEMP1_S * HCA_E + YTEMP1_S * HCB_E + HCC_E;
                    if (HFunct > 0.0) { // Test vertex is in the clipping plane

                        // Find/store the intersection of the clip edge and the line connecting S and P
                        KK = NVTEMP;
                        ++NVTEMP;
                        Real64 const ATEMP_S(state.dataSolarShading->ATEMP(S));
                        Real64 const BTEMP_S(state.dataSolarShading->BTEMP(S));
                        Real64 const CTEMP_S(state.dataSolarShading->CTEMP(S));
                        W = HCB_E * ATEMP_S - HCA_E * BTEMP_S;
                        if (W != 0.0) {
                            Real64 const W_inv(1.0 / W);
                            state.dataSolarShading->XTEMP(NVTEMP) = nint64((HCC_E * BTEMP_S - HCB_E * CTEMP_S) * W_inv);
                            state.dataSolarShading->YTEMP(NVTEMP) = nint64((HCA_E * CTEMP_S - HCC_E * ATEMP_S) * W_inv);
                        } else {
                            state.dataSolarShading->XTEMP(NVTEMP) = SafeDivide(HCC_E * BTEMP_S - HCB_E * CTEMP_S, W);
                            state.dataSolarShading->YTEMP(NVTEMP) = SafeDivide(HCA_E * CTEMP_S - HCC_E * ATEMP_S, W);
                        }
                        INTFLAG = true;

                        if (E == NV2) { // Remove near-duplicates on last edge
                            if (KK != 0) {
                                auto const x(state.dataSolarShading->XTEMP(NVTEMP));
                                auto const y(state.dataSolarShading->YTEMP(NVTEMP));
                                for (int K = 1; K <= KK; ++K) {
                                    if (std::abs(x - state.dataSolarShading->XTEMP(K)) > 2.0) continue;
                                    if (std::abs(y - state.dataSolarShading->YTEMP(K)) > 2.0) continue;
                                    NVTEMP = KK;
                                    break; // K loop
                                }
                            }
                        }
                    }

                    KK = NVTEMP;
                    ++NVTEMP;
                    if (NVTEMP > state.dataSolarShading->MAXHCArrayBounds) {
                        int const NewArrayBounds(state.dataSolarShading->MAXHCArrayBounds + state.dataSolarShading->MAXHCArrayIncrement);
                        state.dataSolarShading->XTEMP.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->YTEMP.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->XTEMP1.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->YTEMP1.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->ATEMP.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->BTEMP.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->CTEMP.redimension(NewArrayBounds, 0.0);
                        state.dataSolarShading->MAXHCArrayBounds = NewArrayBounds;
                    }

                    state.dataSolarShading->XTEMP(NVTEMP) = XTEMP1_P;
                    state.dataSolarShading->YTEMP(NVTEMP) = YTEMP1_P;

                    if (E == NV2) { // Remove near-duplicates on last edge
                        if (KK != 0) {
                            auto const x(state.dataSolarShading->XTEMP(NVTEMP));
                            auto const y(state.dataSolarShading->YTEMP(NVTEMP));
                            for (int K = 1; K <= KK; ++K) {
                                if (std::abs(x - state.dataSolarShading->XTEMP(K)) > 2.0) continue;
                                if (std::abs(y - state.dataSolarShading->YTEMP(K)) > 2.0) continue;
                                NVTEMP = KK;
                                break; // K loop
                            }
                        }
                    }

                } else {
                    HFunct = XTEMP1_S * HCA_E + YTEMP1_S * HCB_E + HCC_E;
                    if (HFunct <= 0.0) {                                // Test vertex is not in the clipping plane
                        if (NVTEMP < 2 * (MaxVerticesPerSurface + 1)) { // avoid assigning to element outside of XTEMP array size
                            KK = NVTEMP;
                            ++NVTEMP;
                            Real64 const ATEMP_S(state.dataSolarShading->ATEMP(S));
                            Real64 const BTEMP_S(state.dataSolarShading->BTEMP(S));
                            Real64 const CTEMP_S(state.dataSolarShading->CTEMP(S));
                            W = HCB_E * ATEMP_S - HCA_E * BTEMP_S;
                            if (W != 0.0) {
                                Real64 const W_inv(1.0 / W);
                                state.dataSolarShading->XTEMP(NVTEMP) = nint64((HCC_E * BTEMP_S - HCB_E * CTEMP_S) * W_inv);
                                state.dataSolarShading->YTEMP(NVTEMP) = nint64((HCA_E * CTEMP_S - HCC_E * ATEMP_S) * W_inv);
                            } else {
                                state.dataSolarShading->XTEMP(NVTEMP) = SafeDivide(HCC_E * BTEMP_S - HCB_E * CTEMP_S, W);
                                state.dataSolarShading->YTEMP(NVTEMP) = SafeDivide(HCA_E * CTEMP_S - HCC_E * ATEMP_S, W);
                            }
                            INTFLAG = true;

                            if (E == NV2) { // Remove near-duplicates on last edge
                                if (KK != 0) {
                                    auto const x(state.dataSolarShading->XTEMP(NVTEMP));
                                    auto const y(state.dataSolarShading->YTEMP(NVTEMP));
                                    for (int K = 1; K <= KK; ++K) {
                                        if (std::abs(x - state.dataSolarShading->XTEMP(K)) > 2.0) continue;
                                        if (std::abs(y - state.dataSolarShading->YTEMP(K)) > 2.0) continue;
                                        NVTEMP = KK;
                                        break; // K loop
                                    }
                                }
                            }
                        }
                    }
                }
                S = P;
                XTEMP1_S = XTEMP1_P;
                YTEMP1_S = YTEMP1_P;
            } // end loop over points of subject polygon

            NVOUT = NVTEMP;
            if (NVOUT == 0) break; // Added to avoid array bounds violation of XTEMP1 and YTEMP1 and wasted looping
            NVTEMP = 0;

            if (E != NV2) {
                if (NVOUT > 2) { // Compute HC values for edges of output polygon
                    Real64 const X_1(state.dataSolarShading->XTEMP(1));
                    Real64 const Y_1(state.dataSolarShading->YTEMP(1));
                    Real64 X_P(X_1), X_P1;
                    Real64 Y_P(Y_1), Y_P1;
                    for (int P = 1; P < NVOUT; ++P) {
                        X_P1 = state.dataSolarShading->XTEMP(P + 1);
                        Y_P1 = state.dataSolarShading->YTEMP(P + 1);
                        state.dataSolarShading->ATEMP(P) = Y_P - Y_P1;
                        state.dataSolarShading->BTEMP(P) = X_P1 - X_P;
                        state.dataSolarShading->CTEMP(P) = X_P * Y_P1 - Y_P * X_P1;
                        X_P = X_P1;
                        Y_P = Y_P1;
                    }
                    state.dataSolarShading->ATEMP(NVOUT) = Y_P1 - Y_1;
                    state.dataSolarShading->BTEMP(NVOUT) = X_1 - X_P1;
                    state.dataSolarShading->CTEMP(NVOUT) = X_P1 * Y_1 - Y_P1 * X_1;
                }
            }

        } // end loop over edges in NS2

        NV3 = NVOUT;

        if (NV3 < 3) { // Determine overlap status
            state.dataSolarShading->OverlapStatus = state.dataSolarShading->NoOverlap;
        } else if (!INTFLAG) {
            state.dataSolarShading->OverlapStatus = state.dataSolarShading->FirstSurfWithinSecond;
        }
    }

    void MULTOL(EnergyPlusData &state, int const NNN,   // argument
                int const LOC0,  // Location in the homogeneous coordinate array
                int const NRFIGS // Number of figures overlapped
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines the overlaps of figure 'NS2' with previous figures
        // 'LOC0+1' through 'LOC0+NRFIGS'.  For example, if NS2
        // is a shadow, overlap with previous shadows.


        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        int I;   // Loop Control
        int NS1; // Number of the figure being overlapped
        int NS2; // Number of the figure doing overlapping
        int NS3; // Location to place results of overlap

        state.dataSolarShading->maxNumberOfFigures = max(state.dataSolarShading->maxNumberOfFigures, NRFIGS);

        NS2 = NNN;
        for (I = 1; I <= NRFIGS; ++I) {
            NS1 = LOC0 + I;
            NS3 = state.dataSolarShading->LOCHCA + 1;

            DeterminePolygonOverlap(state, NS1, NS2, NS3); // Find overlap of figure NS2 on figure NS1.

            // Process overlap cases:

            if (state.dataSolarShading->OverlapStatus == state.dataSolarShading->NoOverlap) continue;

            if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) break;

            state.dataSolarShading->LOCHCA = NS3; // Increment h.c. arrays pointer.
        }
    }

    void ORDER(EnergyPlusData &state, int const NV3, // Number of vertices of figure NS3
               int const NS3  // Location to place results of overlap
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sorts the vertices found by inclosure and
        // intercept in to clockwise order so that the overlap polygon
        // may be used in computing subsequent overlaps.

        // METHODOLOGY EMPLOYED:
        // The slopes of the lines from the left-most vertex to all
        // others are found.  The slopes are sorted into descending
        // sequence.  This sequence puts the vertices in clockwise order.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        static Array1D<Real64> SLOPE; // Slopes from left-most vertex to others
        Real64 DELTAX;                // Difference between X coordinates of two vertices
        Real64 DELTAY;                // Difference between Y coordinates of two vertices
        Real64 SAVES;                 // Temporary location for exchange of variables
        Real64 SAVEX;                 // Temporary location for exchange of variables
        Real64 SAVEY;                 // Temporary location for exchange of variables
        Real64 XMIN;                  // X coordinate of left-most vertex
        Real64 YXMIN;
        int I;   // Sort index
        int IM1; // Sort control
        int J;   // Sort index
        int M;   // Number of slopes to be sorted
        int N;   // Vertex number
        int P;   // Location of first slope to be sorted

        if (state.dataSolarShading->ORDERFirstTimeFlag) {
            SLOPE.allocate(max(10, MaxVerticesPerSurface + 1));
            state.dataSolarShading->ORDERFirstTimeFlag = false;
        }
        // Determine left-most vertex.

        XMIN = state.dataSolarShading->XTEMP(1);
        YXMIN = state.dataSolarShading->YTEMP(1);
        for (N = 2; N <= NV3; ++N) {
            if (state.dataSolarShading->XTEMP(N) >= XMIN) continue;
            XMIN = state.dataSolarShading->XTEMP(N);
            YXMIN = state.dataSolarShading->YTEMP(N);
        }

        // Determine slopes from left-most vertex to all others.  Identify
        // first and second or last points as they occur.

        P = 1;
        M = 0;
        for (N = 1; N <= NV3; ++N) {

            DELTAX = state.dataSolarShading->XTEMP(N) - XMIN;
            DELTAY = state.dataSolarShading->YTEMP(N) - YXMIN;

            if (std::abs(DELTAX) > 0.5) {

                ++M;
                SLOPE(M) = DELTAY / DELTAX;
                state.dataSolarShading->XTEMP(M) = state.dataSolarShading->XTEMP(N);
                state.dataSolarShading->YTEMP(M) = state.dataSolarShading->YTEMP(N);

            } else if (DELTAY > 0.5) {

                P = 2;
                state.dataSolarShading->HCX(NS3, 2) = nint64(state.dataSolarShading->XTEMP(N));
                state.dataSolarShading->HCY(NS3, 2) = nint64(state.dataSolarShading->YTEMP(N));

            } else if (DELTAY < -0.5) {

                state.dataSolarShading->HCX(NS3, NV3) = nint64(state.dataSolarShading->XTEMP(N));
                state.dataSolarShading->HCY(NS3, NV3) = nint64(state.dataSolarShading->YTEMP(N));

            } else {

                state.dataSolarShading->HCX(NS3, 1) = nint64(XMIN);
                state.dataSolarShading->HCY(NS3, 1) = nint64(YXMIN);
            }
        }

        // Sequence the temporary arrays in order of decreasing slopes.(bubble sort)

        if (M != 1) {

            for (I = 2; I <= M; ++I) {
                IM1 = I - 1;
                for (J = 1; J <= IM1; ++J) {
                    if (SLOPE(I) <= SLOPE(J)) continue;
                    SAVEX = state.dataSolarShading->XTEMP(I);
                    SAVEY = state.dataSolarShading->YTEMP(I);
                    SAVES = SLOPE(I);
                    state.dataSolarShading->XTEMP(I) = state.dataSolarShading->XTEMP(J);
                    state.dataSolarShading->YTEMP(I) = state.dataSolarShading->YTEMP(J);
                    SLOPE(I) = SLOPE(J);
                    state.dataSolarShading->XTEMP(J) = SAVEX;
                    state.dataSolarShading->YTEMP(J) = SAVEY;
                    SLOPE(J) = SAVES;
                }
            }
        }

        // Place sequenced points in the homogeneous coordinate arrays.

        for (N = 1; N <= M; ++N) {
            state.dataSolarShading->HCX(NS3, N + P) = nint64(state.dataSolarShading->XTEMP(N));
            state.dataSolarShading->HCY(NS3, N + P) = nint64(state.dataSolarShading->YTEMP(N));
        }
    }

    void DeterminePolygonOverlap(EnergyPlusData &state, int const NS1, // Number of the figure being overlapped
                                 int const NS2, // Number of the figure doing overlapping
                                 int const NS3  // Location to place results of overlap
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes the possible overlap of two polygons.
        // It uses homogeneous coordinate techniques to determine the overlap area
        // between two convex polygons.  Results are stored in the homogeneous coordinate (HC) arrays.

        // METHODOLOGY EMPLOYED:
        // The vertices defining the overlap between fig.1 and fig.2
        // consist of: the vertices of fig.1 enclosed by fig.2 (A)
        // plus the vertices of fig.2 enclosed by fig.1 (B)
        // plus the intercepts of fig.1 and fig.2 (C & D)

        //                               +----------------------+
        //                               !                      !
        //                               !         FIG.2        !
        //                               !                      !
        //                +--------------C----------A           !
        //                !              !         /            !
        //                !              !        /             !
        //                !              B-------D--------------+
        //                !    FIG.1            /
        //                !                    /
        //                +-------------------+

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using DataSystemVariables::SutherlandHodgman;
        using General::RoundSigDigits;

        int N;    // Loop index
        int NV1;  // Number of vertices of figure NS1
        int NV2;  // Number of vertices of figure NS2
        int NV3;  // Number of vertices of figure NS3 (the overlap of NS1 and NS2)
        int NIN1; // Number of vertices of NS1 within NS2
        int NIN2; // Number of vertices of NS2 within NS1

        // Check for exceeding array limits.
#ifdef EP_Count_Calls
        ++NumDetPolyOverlap_Calls;
#endif

        if (NS3 > state.dataSolarShading->MaxHCS) {

            state.dataSolarShading->OverlapStatus = state.dataSolarShading->TooManyFigures;

            if (!state.dataSolarShading->TooManyFiguresMessage && !DisplayExtraWarnings) {
                ShowWarningError("DeterminePolygonOverlap: Too many figures [>" + RoundSigDigits(state.dataSolarShading->MaxHCS) +
                                 "]  detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details.");
                state.dataSolarShading->TooManyFiguresMessage = true;
            }

            if (DisplayExtraWarnings) {
                state.dataSolarShading->TrackTooManyFigures.redimension(++state.dataSolarShading->NumTooManyFigures);
                state.dataSolarShading->TrackTooManyFigures(state.dataSolarShading->NumTooManyFigures).SurfIndex1 = state.dataSolarShading->CurrentShadowingSurface;
                state.dataSolarShading->TrackTooManyFigures(state.dataSolarShading->NumTooManyFigures).SurfIndex2 = state.dataSolarShading->CurrentSurfaceBeingShadowed;
            }

            return;
        }

        state.dataSolarShading->OverlapStatus = state.dataSolarShading->PartialOverlap;
        NV1 = state.dataSolarShading->HCNV(NS1);
        NV2 = state.dataSolarShading->HCNV(NS2);
        NV3 = 0;

        if (!SutherlandHodgman) {
            INCLOS(state, NS1, NV1, NS2, NV2, NV3, NIN1); // Find vertices of NS1 within NS2.

            if (NIN1 >= NV1) {

                state.dataSolarShading->OverlapStatus = state.dataSolarShading->FirstSurfWithinSecond;

            } else {

                INCLOS(state, NS2, NV2, NS1, NV1, NV3, NIN2); // Find vertices of NS2 within NS1.

                if (NIN2 >= NV2) {

                    state.dataSolarShading->OverlapStatus = state.dataSolarShading->SecondSurfWithinFirst;

                } else {

                    INTCPT(state, NV1, NV2, NV3, NS1, NS2); // Find intercepts of NS1 & NS2.

                    if (NV3 < 3) { // Overlap must have 3 or more vertices
                        state.dataSolarShading->OverlapStatus = state.dataSolarShading->NoOverlap;
                        return;
                    }
                }
            }

        } else {
            // simple polygon clipping
            CLIPPOLY(state, NS1, NS2, NV1, NV2, NV3);
        }

        if (NV3 < state.dataSolarShading->MaxHCV && NS3 <= state.dataSolarShading->MaxHCS) {

            if (!SutherlandHodgman) {
                ORDER(state, NV3, NS3); // Put vertices in clockwise order.
            } else {
                assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
                auto l(state.dataSolarShading->HCX.index(NS3, 1));
                for (N = 1; N <= NV3; ++N, ++l) {
                    state.dataSolarShading->HCX[l] = nint64(state.dataSolarShading->XTEMP(N)); // [ l ] == ( N, NS3 )
                    state.dataSolarShading->HCY[l] = nint64(state.dataSolarShading->YTEMP(N));
                }
            }

            HTRANS0(state, NS3, NV3); // Determine h.c. values of sides.
            // Skip overlaps of negligible area.

            if (std::abs(state.dataSolarShading->HCAREA(NS3)) * state.dataSolarShading->HCMULT < std::abs(state.dataSolarShading->HCAREA(NS1))) {
                state.dataSolarShading->OverlapStatus = state.dataSolarShading->NoOverlap;
            } else {
                if (state.dataSolarShading->HCAREA(NS1) * state.dataSolarShading->HCAREA(NS2) > 0.0) state.dataSolarShading->HCAREA(NS3) = -state.dataSolarShading->HCAREA(NS3); // Determine sign of area of overlap
                Real64 const HCT_1(state.dataSolarShading->HCT(NS1));
                Real64 const HCT_2(state.dataSolarShading->HCT(NS2));
                Real64 HCT_3(HCT_2 * HCT_1); // Determine transmission of overlap
                if (HCT_2 >= 0.5 && HCT_1 >= 0.5) {
                    if (HCT_2 != 1.0 && HCT_1 != 1.0) {
                        HCT_3 = 1.0 - HCT_3;
                    }
                }
                state.dataSolarShading->HCT(NS3) = HCT_3;
            }

        } else if (NV3 > state.dataSolarShading->MaxHCV) {

            state.dataSolarShading->OverlapStatus = state.dataSolarShading->TooManyVertices;

            if (!state.dataSolarShading->TooManyVerticesMessage && !DisplayExtraWarnings) {
                ShowWarningError("DeterminePolygonOverlap: Too many vertices [>" + RoundSigDigits(state.dataSolarShading->MaxHCV) +
                                 "] detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details.");
                state.dataSolarShading->TooManyVerticesMessage = true;
            }

            if (DisplayExtraWarnings) {
                state.dataSolarShading->TrackTooManyVertices.redimension(++state.dataSolarShading->NumTooManyVertices);
                state.dataSolarShading->TrackTooManyVertices(state.dataSolarShading->NumTooManyVertices).SurfIndex1 = state.dataSolarShading->CurrentShadowingSurface;
                state.dataSolarShading->TrackTooManyVertices(state.dataSolarShading->NumTooManyVertices).SurfIndex2 = state.dataSolarShading->CurrentSurfaceBeingShadowed;
            }

        } else if (NS3 > state.dataSolarShading->MaxHCS) {

            state.dataSolarShading->OverlapStatus = state.dataSolarShading->TooManyFigures;

            if (!state.dataSolarShading->TooManyFiguresMessage && !DisplayExtraWarnings) {
                ShowWarningError("DeterminePolygonOverlap: Too many figures [>" + RoundSigDigits(state.dataSolarShading->MaxHCS) +
                                 "]  detected in an overlap calculation. Use Output:Diagnostics,DisplayExtraWarnings; for more details.");
                state.dataSolarShading->TooManyFiguresMessage = true;
            }

            if (DisplayExtraWarnings) {
                state.dataSolarShading->TrackTooManyFigures.redimension(++state.dataSolarShading->NumTooManyFigures);
                state.dataSolarShading->TrackTooManyFigures(state.dataSolarShading->NumTooManyFigures).SurfIndex1 = state.dataSolarShading->CurrentShadowingSurface;
                state.dataSolarShading->TrackTooManyFigures(state.dataSolarShading->NumTooManyFigures).SurfIndex2 = state.dataSolarShading->CurrentSurfaceBeingShadowed;
            }
        }
    }

    void CalcPerSolarBeam(EnergyPlusData &state,
                          Real64 const AvgEqOfTime,       // Average value of Equation of Time for period
                          Real64 const AvgSinSolarDeclin, // Average value of Sine of Solar Declination for period
                          Real64 const AvgCosSolarDeclin  // Average value of Cosine of Solar Declination for period
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       BG, Nov 2012 - Timestep solar.  DetailedSolarTimestepIntegration
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages computation of solar gain multipliers for beam radiation.  These
        // are calculated for a period of days depending on the input "Shadowing Calculations".

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using DataGlobals::HourOfDay;
        using DataGlobals::TimeStep;
        using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
        using DataSystemVariables::DetailedSolarTimestepIntegration;

        using ScheduleManager::LookUpScheduleValue;
        using WindowComplexManager::InitComplexWindows;
        using WindowComplexManager::UpdateComplexWindows;

        int iHour;   // Hour index number
        int TS;      // TimeStep Loop Countergit

        if (state.dataSolarShading->InitComplexOnce) InitComplexWindows(state);
        state.dataSolarShading->InitComplexOnce = false;

        if (KickOffSizing || KickOffSimulation) return; // Skip solar calcs for these Initialization steps.

#ifdef EP_Count_Calls
        ++NumCalcPerSolBeam_Calls;
#endif

        // Initialize some values for the appropriate period
        if (!DetailedSolarTimestepIntegration) {
            SunlitFracHR = 0.0;
            SunlitFrac = 0.0;
            SunlitFracWithoutReveal = 0.0;
            state.dataSolarShading->CTHETA = 0.0;
            CosIncAngHR = 0.0;
            CosIncAng = 0.0;
            SurfOpaqAO = 0.0;
            BackSurfaces = 0;
            OverlapAreas = 0.0;
            for (auto &e : SurfaceWindow) {
                e.OutProjSLFracMult = 1.0;
                e.InOutProjSLFracMult = 1.0;
            }
        } else {
            SunlitFracHR(HourOfDay, {1, TotSurfaces}) = 0.0;
            SunlitFrac(TimeStep, HourOfDay, {1, TotSurfaces}) = 0.0;
            SunlitFracWithoutReveal(TimeStep, HourOfDay, {1, TotSurfaces}) = 0.0;
            state.dataSolarShading->CTHETA({1, TotSurfaces}) = 0.0;
            CosIncAngHR(HourOfDay, {1, TotSurfaces}) = 0.0;
            CosIncAng(TimeStep, HourOfDay, {1, TotSurfaces}) = 0.0;
            SurfOpaqAO({1, TotSurfaces}) = 0.0;
            BackSurfaces(TimeStep, HourOfDay, {1, MaxBkSurf}, {1, TotSurfaces}) = 0;
            OverlapAreas(TimeStep, HourOfDay, {1, MaxBkSurf}, {1, TotSurfaces}) = 0.0;
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                SurfaceWindow(SurfNum).OutProjSLFracMult(HourOfDay) = 1.0;
                SurfaceWindow(SurfNum).InOutProjSLFracMult(HourOfDay) = 1.0;
            }
        }

        if (!DetailedSolarTimestepIntegration) {
            for (iHour = 1; iHour <= 24; ++iHour) { // Do for all hours
                for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                    FigureSunCosines(state, iHour, TS, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);
                }
            }
        } else {
            FigureSunCosines(state, HourOfDay, TimeStep, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);
        }
        // Initialize/update the Complex Fenestration geometry and optical properties
        UpdateComplexWindows(state);
        if (!DetailedSolarTimestepIntegration) {
            for (iHour = 1; iHour <= 24; ++iHour) { // Do for all hours.
                for (TS = 1; TS <= NumOfTimeStepInHour; ++TS) {
                    FigureSolarBeamAtTimestep(state, iHour, TS);
                } // TimeStep Loop
            }     // Hour Loop
        } else {
            FigureSolarBeamAtTimestep(state, HourOfDay, TimeStep);
        }
    }

    void FigureSunCosines(EnergyPlusData &state, int const iHour,
                          int const iTimeStep,
                          Real64 const EqOfTime,       // value of Equation of Time for period
                          Real64 const SinSolarDeclin, // value of Sine of Solar Declination for period
                          Real64 const CosSolarDeclin  // value of Cosine of Solar Declination for period
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Determine solar position.  Default for sun below horizon.

        // METHODOLOGY EMPLOYED:
        // Given hour, timestep, equation of time, solar declination sine, and solar declination cosine,
        // determine sun directions for use elsewhere

        // Using/Aliasing
        using DataGlobals::TimeStepZone;
        using DataSystemVariables::DetailedSolarTimestepIntegration;

        Real64 CurrentTime; // Current Time for passing to Solar Position Routine

        if (NumOfTimeStepInHour != 1) {
            CurrentTime = double(iHour - 1) + double(iTimeStep) * (TimeStepZone);
        } else {
            CurrentTime = double(iHour) + TS1TimeOffset;
        }
        SUN4(state, CurrentTime, EqOfTime, SinSolarDeclin, CosSolarDeclin);

        // Save hourly values for use in DaylightingManager
        if (!DetailedSolarTimestepIntegration) {
            if (iTimeStep == NumOfTimeStepInHour) SUNCOSHR(iHour, {1, 3}) = state.dataSolarShading->SUNCOS;
        } else {
            SUNCOSHR(iHour, {1, 3}) = state.dataSolarShading->SUNCOS;
        }
        // Save timestep values for use in WindowComplexManager
        SUNCOSTS(iTimeStep, iHour, {1, 3}) = state.dataSolarShading->SUNCOS;
    }

    void FigureSolarBeamAtTimestep(EnergyPlusData &state, int const iHour, int const iTimeStep)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B.Griffith, derived from CalcPerSolarBeam, Legacy and Lawrie.
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes solar gain multipliers for beam solar

        using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
        using DataSystemVariables::DetailedSolarTimestepIntegration;
        using DataSystemVariables::ReportExtShadingSunlitFrac;
        using DataSystemVariables::ShadingMethod;
        using DataSystemVariables::shadingMethod;
        using ScheduleManager::LookUpScheduleValue;

        Real64 SurfArea;        // Surface area. For walls, includes all window frame areas.
        Real64 Fac1WoShdg;      // Intermediate calculation factor, without shading
        Real64 Fac1WithShdg;    // Intermediate calculation factor, with shading
        Real64 FracIlluminated; // Fraction of surface area illuminated by a sky patch

        // Recover the sun direction from the array stored in previous loop
        state.dataSolarShading->SUNCOS = SUNCOSTS(iTimeStep, iHour, {1, 3});

        state.dataSolarShading->CTHETA = 0.0;

        if (state.dataSolarShading->SUNCOS(3) < SunIsUpValue) return;

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            state.dataSolarShading->CTHETA(SurfNum) =
                state.dataSolarShading->SUNCOS(1) * Surface(SurfNum).OutNormVec(1) + state.dataSolarShading->SUNCOS(2) * Surface(SurfNum).OutNormVec(2) + state.dataSolarShading->SUNCOS(3) * Surface(SurfNum).OutNormVec(3);
            if (!DetailedSolarTimestepIntegration) {
                if (iTimeStep == NumOfTimeStepInHour) CosIncAngHR(iHour, SurfNum) = state.dataSolarShading->CTHETA(SurfNum);
            } else {
                CosIncAngHR(iHour, SurfNum) = state.dataSolarShading->CTHETA(SurfNum);
            }
            CosIncAng(iTimeStep, iHour, SurfNum) = state.dataSolarShading->CTHETA(SurfNum);
        }

        if ((shadingMethod == ShadingMethod::Scheduled || shadingMethod == ShadingMethod::Imported) && !DoingSizing && state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather){
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).SchedExternalShadingFrac) {
                    SunlitFrac(iTimeStep, iHour, SurfNum) = LookUpScheduleValue(state, Surface(SurfNum).ExternalShadingSchInd, iHour, iTimeStep);
                } else {
                    SunlitFrac(iTimeStep, iHour, SurfNum) = 1.0;
                }
            }
        } else {
            SHADOW(state, iHour, iTimeStep); // Determine sunlit areas and solar multipliers for all surfaces.
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).Area >= 1.e-10) {
                    SurfArea = Surface(SurfNum).NetAreaShadowCalc;
                    if (!DetailedSolarTimestepIntegration) {
                        if (iTimeStep == NumOfTimeStepInHour) SunlitFracHR(iHour, SurfNum) = state.dataSolarShading->SAREA(SurfNum) / SurfArea;
                    } else {
                        SunlitFracHR(iHour, SurfNum) = state.dataSolarShading->SAREA(SurfNum) / SurfArea;
                    }
                    SunlitFrac(iTimeStep, iHour, SurfNum) = state.dataSolarShading->SAREA(SurfNum) / SurfArea;
                    if (SunlitFrac(iTimeStep, iHour, SurfNum) < 1.e-5) SunlitFrac(iTimeStep, iHour, SurfNum) = 0.0;
                }
                // Added check
                if (SunlitFrac(iTimeStep, iHour, SurfNum) > 1.0) {
                    SunlitFrac(iTimeStep, iHour, SurfNum) = 1.0;
                }
            }
        }
        //   Note -- if not the below, values are set in SkyDifSolarShading routine (constant for simulation)
        if (DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
            WithShdgIsoSky = 0.;
            WoShdgIsoSky = 0.;
            WithShdgHoriz = 0.;
            WoShdgHoriz = 0.;

            for (int IPhi = 0; IPhi < NPhi; ++IPhi) { // Loop over patch altitude values
                state.dataSolarShading->SUNCOS(3) = sin_Phi[IPhi];

                for (int ITheta = 0; ITheta < NTheta; ++ITheta) { // Loop over patch azimuth values
                    state.dataSolarShading->SUNCOS(1) = cos_Phi[IPhi] * cos_Theta[ITheta];
                    state.dataSolarShading->SUNCOS(2) = cos_Phi[IPhi] * sin_Theta[ITheta];

                    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                        if (!Surface(SurfNum).ShadowingSurf && !Surface(SurfNum).HeatTransSurf) continue;
                        state.dataSolarShading->CTHETA(SurfNum) = state.dataSolarShading->SUNCOS(1) * Surface(SurfNum).OutNormVec(1) + state.dataSolarShading->SUNCOS(2) * Surface(SurfNum).OutNormVec(2) +
                                          state.dataSolarShading->SUNCOS(3) * Surface(SurfNum).OutNormVec(3);
                    }

                    SHADOW(state, iHour, iTimeStep); // Determine sunlit areas and solar multipliers for all surfaces.

                    for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                        if (!Surface(SurfNum).ShadowingSurf &&
                            (!Surface(SurfNum).HeatTransSurf || !Surface(SurfNum).ExtSolar ||
                             (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)))
                            continue;

                        if (state.dataSolarShading->CTHETA(SurfNum) < 0.0) continue;

                        Fac1WoShdg = cos_Phi[IPhi] * DThetaDPhi * state.dataSolarShading->CTHETA(SurfNum);
                        SurfArea = Surface(SurfNum).NetAreaShadowCalc;
                        if (SurfArea > Eps) {
                            FracIlluminated = state.dataSolarShading->SAREA(SurfNum) / SurfArea;
                        } else {
                            FracIlluminated = state.dataSolarShading->SAREA(SurfNum) / (SurfArea + Eps);
                        }
                        Fac1WithShdg = Fac1WoShdg * FracIlluminated;
                        WithShdgIsoSky(SurfNum) += Fac1WithShdg;
                        WoShdgIsoSky(SurfNum) += Fac1WoShdg;

                        // Horizon region
                        if (IPhi == 0) {
                            WithShdgHoriz(SurfNum) += Fac1WithShdg;
                            WoShdgHoriz(SurfNum) += Fac1WoShdg;
                        }
                    } // End of surface loop
                }     // End of Theta loop
            }         // End of Phi loop

            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {

                if (!Surface(SurfNum).ShadowingSurf &&
                    (!Surface(SurfNum).HeatTransSurf || !Surface(SurfNum).ExtSolar ||
                     (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)))
                    continue;

                if (std::abs(WoShdgIsoSky(SurfNum)) > Eps) {
                    DifShdgRatioIsoSkyHRTS(iTimeStep, iHour, SurfNum) = (WithShdgIsoSky(SurfNum)) / (WoShdgIsoSky(SurfNum));
                } else {
                    DifShdgRatioIsoSkyHRTS(iTimeStep, iHour, SurfNum) = (WithShdgIsoSky(SurfNum)) / (WoShdgIsoSky(SurfNum) + Eps);
                }
                if (std::abs(WoShdgHoriz(SurfNum)) > Eps) {
                    DifShdgRatioHorizHRTS(iTimeStep, iHour, SurfNum) = (WithShdgHoriz(SurfNum)) / (WoShdgHoriz(SurfNum));
                } else {
                    DifShdgRatioHorizHRTS(iTimeStep, iHour, SurfNum) = (WithShdgHoriz(SurfNum)) / (WoShdgHoriz(SurfNum) + Eps);
                }
            }

            //  ! Get IR view factors. An exterior surface can receive IR radiation from
            //  ! sky, ground or shadowing surfaces. Assume shadowing surfaces have same
            //  ! temperature as outside air (and therefore same temperature as ground),
            //  ! so that the view factor to these shadowing surfaces can be included in
            //  ! the ground view factor. Sky IR is assumed to be isotropic and shadowing
            //  ! surfaces are assumed to be opaque to IR so they totally "shade" IR from
            //  ! sky or ground.

            //  DO SurfNum = 1,TotSurfaces
            //    Surface(SurfNum)%ViewFactorSkyIR = Surface(SurfNum)%ViewFactorSkyIR * DifShdgRatioIsoSky(SurfNum,IHOUR,TS)
            //    Surface(SurfNum)%ViewFactorGroundIR = 1.0 - Surface(SurfNum)%ViewFactorSkyIR
            //  END DO

        } // test for shading surfaces

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            // For exterior windows with frame/divider that are partially or fully sunlit,
            // correct SunlitFrac due to shadowing of frame and divider projections onto window glass.
            // Note: if SunlitFrac = 0.0 the window is either completely shaded or the sun is in back
            // of the window; in either case, frame/divider shadowing doesn't have to be done.

            if (Surface(SurfNum).Class == SurfaceClass::Window && Surface(SurfNum).ExtBoundCond == ExternalEnvironment &&
                SunlitFrac(iTimeStep, iHour, SurfNum) > 0.0 && Surface(SurfNum).FrameDivider > 0)
                CalcFrameDividerShadow(state, SurfNum, Surface(SurfNum).FrameDivider, iHour);
        }
    }

    void DetermineShadowingCombinations(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         From Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       LKL; March 2002 -- another missing translation from BLAST's routine
        //                      FCW; Jan 2003 -- removed line that prevented beam solar through interior windows
        //       RE-ENGINEERED  Rick Strand; 1998
        //                      Linda Lawrie; Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This routine prepares a list of heat transfer surfaces and
        // their possible shadowers which is used to direct the hourly
        // calculation of shadows and sunlit areas.

        // METHODOLOGY EMPLOYED:
        // As appropriate surfaces are identified, they are placed into the
        // ShadowComb data structure (module level) with the accompanying lists
        // of other surface numbers.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using namespace DataErrorTracking;
        using General::TrimSigDigits;

        Array1D_int GSS;             // List of shadowing surfaces numbers for a receiving surface
        Array1D_int BKS;             // List of back surface numbers for a receiving surface
        Array1D_int SBS;             // List of subsurfaces for a receiving surface
        static int MaxGSS(50);       // Current Max for GSS array
        static int MaxBKS(50);       // Current Max for BKS array
        static int MaxSBS(50);       // Current Max for SBS array
        bool CannotShade;            // TRUE if subsurface cannot shade receiving surface
        bool HasWindow;              // TRUE if a window is present on receiving surface
        Real64 ZMIN;                 // Lowest point on the receiving surface
        int BackSurfaceNumber;       // Back surface number
        int HTS;                     // Heat transfer surface number for a receiving surface
        int GRSNR;                   // Receiving surface number
        int GSSNR;                   // Shadowing surface number
        int SBSNR;                   // Subsurface number
        int NBKS;                    // Number of back surfaces for a receiving surface
        int NGSS;                    // Number of shadowing surfaces for a receiving surface
        int NSBS;                    // Number of subsurfaces for a receiving surface
        bool ShadowingSurf;          // True if a receiving surface is a shadowing surface
        Array1D_bool CastingSurface; // tracking during setup of ShadowComb

        static int MaxDim(0);

#ifdef EP_Count_Calls
        ++NumDetShadowCombs_Calls;
#endif

        ShadowComb.dimension(TotSurfaces, ShadowingCombinations{}); // Set all elements to default constructed state

        CastingSurface.dimension(TotSurfaces, false);

        state.dataSolarShading->HCA.dimension(2 * state.dataSolarShading->MaxHCS, state.dataSolarShading->MaxHCV + 1, 0);
        state.dataSolarShading->HCB.dimension(2 * state.dataSolarShading->MaxHCS, state.dataSolarShading->MaxHCV + 1, 0);
        state.dataSolarShading->HCC.dimension(2 * state.dataSolarShading->MaxHCS, state.dataSolarShading->MaxHCV + 1, 0);
        state.dataSolarShading->HCX.dimension(2 * state.dataSolarShading->MaxHCS, state.dataSolarShading->MaxHCV + 1, 0);
        state.dataSolarShading->HCY.dimension(2 * state.dataSolarShading->MaxHCS, state.dataSolarShading->MaxHCV + 1, 0);
        state.dataSolarShading->HCAREA.dimension(2 * state.dataSolarShading->MaxHCS, 0.0);
        state.dataSolarShading->HCNS.dimension(2 * state.dataSolarShading->MaxHCS, 0);
        state.dataSolarShading->HCNV.dimension(2 * state.dataSolarShading->MaxHCS, 0);
        state.dataSolarShading->HCT.dimension(2 * state.dataSolarShading->MaxHCS, 0.0);

        GSS.dimension(MaxGSS, 0);
        BKS.dimension(MaxGSS, 0);
        SBS.dimension(MaxGSS, 0);

        state.dataSolarShading->penumbraIDs.clear();

        HTS = 0;

        // Check every surface as a possible shadow receiving surface ("RS" = receiving surface).
        if (IgnoreSolarRadiation) {
            return;
        }

        for (GRSNR = 1; GRSNR <= TotSurfaces; ++GRSNR) { // Loop through all surfaces (looking for potential receiving surfaces)...

            ShadowingSurf = Surface(GRSNR).ShadowingSurf;
            NGSS = 0;
            NSBS = 0;
            NBKS = 0;

            if (!ShadowingSurf && !Surface(GRSNR).HeatTransSurf) continue;
            HTS = GRSNR;

#ifndef EP_NO_OPENGL
            if (state.dataSolarShading->penumbra) {
                bool skipSurface = Surface(GRSNR).MirroredSurf;   // Penumbra doesn't need mirrored surfaces TODO: Don't bother creating them in the first place?

                // Skip interior surfaces if the other side has already been added to penumbra
                if (Surface(GRSNR).ExtBoundCond > 0) {
                    if (Surface(Surface(GRSNR).ExtBoundCond).PenumbraID >= 0) {
                        Surface(GRSNR).PenumbraID = Surface(Surface(GRSNR).ExtBoundCond).PenumbraID;
                        skipSurface = true;
                    }
                }

                if (!skipSurface) {
                    // Add surfaces to penumbra...
                    Pumbra::Polygon poly;

                    if (Surface(GRSNR).Reveal > 0.0) {
                        Real64 R = Surface(GRSNR).Reveal;
                        auto& norm = Surface(GRSNR).NewellSurfaceNormalVector;
                        auto& v = Surface(GRSNR).Vertex;
                        for (unsigned i = 0; i < v.size(); ++i) {
                            poly.push_back(v[i].x);
                            poly.push_back(v[i].y);
                            poly.push_back(v[i].z);


                            Vector vPrev;
                            if (i == 0) {
                                vPrev = v[v.size() - 1];
                            } else {
                                vPrev = v[i - 1];
                            }

                            Pumbra::Polygon rPoly; // Reveal surface
                            rPoly.push_back(v[i].x);
                            rPoly.push_back(v[i].y);
                            rPoly.push_back(v[i].z);


                            rPoly.push_back(v[i].x + norm.x*R);
                            rPoly.push_back(v[i].y + norm.y*R);
                            rPoly.push_back(v[i].z + norm.z*R);

                            rPoly.push_back(vPrev.x + norm.x*R);
                            rPoly.push_back(vPrev.y + norm.y*R);
                            rPoly.push_back(vPrev.z + norm.z*R);

                            rPoly.push_back(vPrev.x);
                            rPoly.push_back(vPrev.y);
                            rPoly.push_back(vPrev.z);

                            Pumbra::Surface rSurf(rPoly);
                            state.dataSolarShading->penumbra->addSurface(rSurf);

                        }
                    } else {
                        for (auto v : Surface(GRSNR).Vertex) {
                            poly.push_back(v.x);
                            poly.push_back(v.y);
                            poly.push_back(v.z);
                        }
                    }
                    Pumbra::Surface pSurf(poly);

                    // Punch holes for subsurfaces
                    if (Surface(GRSNR).BaseSurf == GRSNR) {  // Only look for subsurfaces on base surfaces
                        for (int subSurface = 1; subSurface <= TotSurfaces; ++subSurface) {
                            if (Surface(subSurface).BaseSurf != GRSNR) continue; // Ignore subsurfaces of other surfaces
                            if (!Surface(subSurface).HeatTransSurf) continue;    // Skip non heat transfer subsurfaces
                            if (subSurface == GRSNR) continue;                   // Surface itself cannot be its own subsurface

                            Pumbra::Polygon subPoly;
                            if (Surface(subSurface).Reveal > 0.0) {
                                Real64 R = Surface(subSurface).Reveal;
                                auto& norm = Surface(subSurface).NewellSurfaceNormalVector;
                                for (auto v : Surface(subSurface).Vertex) {
                                    subPoly.push_back(v.x + norm.x*R);
                                    subPoly.push_back(v.y + norm.y*R);
                                    subPoly.push_back(v.z + norm.z*R);
                                }
                            } else {
                                for (auto v : Surface(subSurface).Vertex) {
                                    subPoly.push_back(v.x);
                                    subPoly.push_back(v.y);
                                    subPoly.push_back(v.z);
                                }
                            }

                            pSurf.addHole(subPoly);
                        }
                    }
                    Surface(GRSNR).PenumbraID = state.dataSolarShading->penumbra->addSurface(pSurf);
                    state.dataSolarShading->penumbraIDs.push_back(Surface(GRSNR).PenumbraID);
                }
            }
#endif

            if (!ShadowingSurf && !Surface(GRSNR).ExtSolar) continue; // Skip surfaces with no external solar

            if (!ShadowingSurf && Surface(GRSNR).BaseSurf != GRSNR) { // Skip subsurfaces (SBS)
                continue;
            }

            // Get the lowest point of receiving surface
            ZMIN = minval(Surface(GRSNR).Vertex, &Vector::z);

            // Check every surface as a possible shadow casting surface ("SS" = shadow sending)
            NGSS = 0;
            if (SolarDistribution != MinimalShadowing) { // Except when doing simplified exterior shadowing.

                for (GSSNR = 1; GSSNR <= TotSurfaces; ++GSSNR) { // Loop through all surfaces, looking for ones that could shade GRSNR

                    if (GSSNR == GRSNR) continue; // Receiving surface cannot shade itself
                    if ((Surface(GSSNR).HeatTransSurf) && (Surface(GSSNR).BaseSurf == GRSNR))
                        continue; // A heat transfer subsurface of a receiving surface
                    // cannot shade the receiving surface
                    if (ShadowingSurf) {
                        // If receiving surf is a shadowing surface exclude matching shadow surface as sending surface
                        // IF((GSSNR == GRSNR+1 .AND. Surface(GSSNR)%Name(1:3) == 'Mir').OR. &
                        //   (GSSNR == GRSNR-1 .AND. Surface(GRSNR)%Name(1:3) == 'Mir')) CYCLE
                        if (((GSSNR == GRSNR + 1) && Surface(GSSNR).MirroredSurf) || ((GSSNR == GRSNR - 1) && Surface(GRSNR).MirroredSurf)) continue;
                    }

                    if (Surface(GSSNR).BaseSurf == GRSNR) { // Shadowing subsurface of receiving surface

                        ++NGSS;
                        if (NGSS > MaxGSS) {
                            GSS.redimension(MaxGSS *= 2, 0);
                        }
                        GSS(NGSS) = GSSNR;

                    } else if ((Surface(GSSNR).BaseSurf == 0) ||
                               ((Surface(GSSNR).BaseSurf == GSSNR) &&
                                ((Surface(GSSNR).ExtBoundCond == ExternalEnvironment) ||
                                 Surface(GSSNR).ExtBoundCond == OtherSideCondModeledExt))) { // Detached shadowing surface or | any other base surface
                                                                                             // exposed to outside environment

                        CHKGSS(GRSNR, GSSNR, ZMIN, CannotShade); // Check to see if this can shade the receiving surface
                        if (!CannotShade) {                      // Update the shadowing surface data if shading is possible
                            ++NGSS;
                            if (NGSS > MaxGSS) {
                                GSS.redimension(MaxGSS *= 2, 0);
                            }
                            GSS(NGSS) = GSSNR;
                        }
                    }

                }    // ...end of surfaces DO loop (GSSNR)
            } else { // Simplified Distribution -- still check for Shading Subsurfaces

                for (GSSNR = 1; GSSNR <= TotSurfaces; ++GSSNR) { // Loop through all surfaces (looking for surfaces which could shade GRSNR) ...

                    if (GSSNR == GRSNR) continue; // Receiving surface cannot shade itself
                    if ((Surface(GSSNR).HeatTransSurf) && (Surface(GSSNR).BaseSurf == GRSNR))
                        continue;                           // Skip heat transfer subsurfaces of receiving surface
                    if (Surface(GSSNR).BaseSurf == GRSNR) { // Shadowing subsurface of receiving surface
                        ++NGSS;
                        if (NGSS > MaxGSS) {
                            GSS.redimension(MaxGSS *= 2, 0);
                        }
                        GSS(NGSS) = GSSNR;
                    }
                }

            } // ...end of check for simplified solar distribution

            // Check every surface as a receiving subsurface of the receiving surface
            NSBS = 0;
            HasWindow = false;
            // legacy: IF (OSENV(HTS) > 10) WINDOW=.TRUE. -->Note: WINDOW was set true for roof ponds, solar walls, or other zones
            for (SBSNR = 1; SBSNR <= TotSurfaces; ++SBSNR) { // Loop through the surfaces yet again (looking for subsurfaces of GRSNR)...

                if (!Surface(SBSNR).HeatTransSurf) continue;    // Skip non heat transfer subsurfaces
                if (SBSNR == GRSNR) continue;                   // Surface itself cannot be its own subsurface
                if (Surface(SBSNR).BaseSurf != GRSNR) continue; // Ignore subsurfaces of other surfaces and other surfaces

                if (state.dataConstruction->Construct(Surface(SBSNR).Construction).TransDiff > 0.0) HasWindow = true; // Check for window
                CHKSBS(state, HTS, GRSNR, SBSNR); // Check that the receiving surface completely encloses the subsurface;
                // severe error if not
                ++NSBS;
                if (NSBS > MaxSBS) {
                    SBS.redimension(MaxSBS *= 2, 0);
                }
                SBS(NSBS) = SBSNR;

            } // ...end of surfaces DO loop (SBSNR)

            // Check every surface as a back surface
            NBKS = 0;
            //                                        Except for simplified
            //                                        interior solar distribution,
            if ((SolarDistribution == FullInteriorExterior) &&
                (HasWindow)) { // For full interior solar distribution | and a window present on base surface (GRSNR)

                for (BackSurfaceNumber = 1; BackSurfaceNumber <= TotSurfaces;
                     ++BackSurfaceNumber) { // Loop through surfaces yet again, looking for back surfaces to GRSNR

                    if (!Surface(BackSurfaceNumber).HeatTransSurf) continue;              // Skip non-heat transfer surfaces
                    if (Surface(BackSurfaceNumber).BaseSurf == GRSNR) continue;           // Skip subsurfaces of this GRSNR
                    if (BackSurfaceNumber == GRSNR) continue;                             // A back surface cannot be GRSNR itself
                    if (Surface(BackSurfaceNumber).SolarEnclIndex != Surface(GRSNR).SolarEnclIndex) continue; // Skip if back surface not in same solar enclosure

                    if (Surface(BackSurfaceNumber).Class == SurfaceClass::IntMass) continue;

                    // Following line removed 1/27/03 by FCW. Was in original code that didn't do beam solar transmitted through
                    // interior windows. Was removed to allow such beam solar but then somehow was put back in.
                    // IF (Surface(BackSurfaceNumber)%BaseSurf /= BackSurfaceNumber) CYCLE ! Not for subsurfaces of Back Surface

                    if (!state.dataSolarShading->penumbra) {
                        CHKBKS(BackSurfaceNumber, GRSNR); // CHECK FOR CONVEX ZONE; severe error if not
                    }
                    ++NBKS;
                    if (NBKS > MaxBKS) {
                        BKS.redimension(MaxBKS *= 2, 0);
                    }
                    BKS(NBKS) = BackSurfaceNumber;

                } // ...end of surfaces DO loop (BackSurfaceNumber)
            }

            // Put this into the ShadowComb data structure
            ShadowComb(GRSNR).UseThisSurf = true;
            ShadowComb(GRSNR).NumGenSurf = NGSS;
            ShadowComb(GRSNR).NumBackSurf = NBKS;
            ShadowComb(GRSNR).NumSubSurf = NSBS;
            MaxDim = max(MaxDim, NGSS, NBKS, NSBS);

            ShadowComb(GRSNR).GenSurf.allocate({0, ShadowComb(GRSNR).NumGenSurf});
            ShadowComb(GRSNR).GenSurf(0) = 0;
            if (ShadowComb(GRSNR).NumGenSurf > 0) {
                ShadowComb(GRSNR).GenSurf({1, ShadowComb(GRSNR).NumGenSurf}) = GSS({1, NGSS});
            }

            ShadowComb(GRSNR).BackSurf.allocate({0, ShadowComb(GRSNR).NumBackSurf});
            ShadowComb(GRSNR).BackSurf(0) = 0;
            if (ShadowComb(GRSNR).NumBackSurf > 0) {
                ShadowComb(GRSNR).BackSurf({1, ShadowComb(GRSNR).NumBackSurf}) = BKS({1, NBKS});
            }

            ShadowComb(GRSNR).SubSurf.allocate({0, ShadowComb(GRSNR).NumSubSurf});
            ShadowComb(GRSNR).SubSurf(0) = 0;
            if (ShadowComb(GRSNR).NumSubSurf > 0) {
                ShadowComb(GRSNR).SubSurf({1, ShadowComb(GRSNR).NumSubSurf}) = SBS({1, NSBS});
            }

        } // ...end of surfaces (GRSNR) DO loop

        GSS.deallocate();
        SBS.deallocate();
        BKS.deallocate();

        if (!state.dataSolarShading->penumbra) {
            if (shd_stream) {
                *shd_stream << "Shadowing Combinations\n";
                if (SolarDistribution == MinimalShadowing) {
                    *shd_stream << "..Solar Distribution=Minimal Shadowing, Detached Shading will not be used in shadowing calculations\n";
                } else if (SolarDistribution == FullExterior) {
                    if (CalcSolRefl) {
                        *shd_stream << "..Solar Distribution=FullExteriorWithReflectionsFromExteriorSurfaces\n";
                    } else {
                        *shd_stream << "..Solar Distribution=FullExterior\n";
                    }
                } else if (SolarDistribution == FullInteriorExterior) {
                    if (CalcSolRefl) {
                        *shd_stream << "..Solar Distribution=FullInteriorAndExteriorWithReflectionsFromExteriorSurfaces\n";
                    } else {
                        *shd_stream << "..Solar Distribution=FullInteriorAndExterior\n";
                    }
                } else {
                }

                *shd_stream << "..In the following, only the first 10 reference surfaces will be shown.\n";
                *shd_stream << "..But all surfaces are used in the calculations.\n";

                for (int HTSnum : DataSurfaces::AllSurfaceListReportOrder) {
                    *shd_stream << "==================================\n";
                    if (ShadowComb(HTSnum).UseThisSurf) {
                        if (Surface(HTSnum).IsConvex) {
                            *shd_stream << "Surface=" << Surface(HTSnum).Name << " is used as Receiving Surface in calculations and is convex.\n";
                        } else {
                            *shd_stream << "Surface=" << Surface(HTSnum).Name << " is used as Receiving Surface in calculations and is non-convex.\n";
                            if (ShadowComb(HTSnum).NumGenSurf > 0) {
                                if (DisplayExtraWarnings) {
                                    ShowWarningError("DetermineShadowingCombinations: Surface=\"" + Surface(HTSnum).Name +
                                        "\" is a receiving surface and is non-convex.");
                                    ShowContinueError("...Shadowing values may be inaccurate. Check .shd report file for more surface shading details");
                                } else {
                                    ++TotalReceivingNonConvexSurfaces;
                                }
                            }
                        }
                    } else {
                        *shd_stream << "Surface=" << Surface(HTSnum).Name << " is not used as Receiving Surface in calculations.\n";
                    }
                    *shd_stream << "Number of general casting surfaces=" << ShadowComb(HTSnum).NumGenSurf << '\n';
                    for (NGSS = 1; NGSS <= ShadowComb(HTSnum).NumGenSurf; ++NGSS) {
                        if (NGSS <= 10) *shd_stream << "..Surface=" << Surface(ShadowComb(HTSnum).GenSurf(NGSS)).Name << '\n';
                        CastingSurface(ShadowComb(HTSnum).GenSurf(NGSS)) = true;
                    }
                    *shd_stream << "Number of back surfaces=" << ShadowComb(HTSnum).NumBackSurf << '\n';
                    for (NGSS = 1; NGSS <= min(10, ShadowComb(HTSnum).NumBackSurf); ++NGSS) {
                        *shd_stream << "...Surface=" << Surface(ShadowComb(HTSnum).BackSurf(NGSS)).Name << '\n';
                    }
                    *shd_stream << "Number of receiving sub surfaces=" << ShadowComb(HTSnum).NumSubSurf << '\n';
                    for (NGSS = 1; NGSS <= min(10, ShadowComb(HTSnum).NumSubSurf); ++NGSS) {
                        *shd_stream << "....Surface=" << Surface(ShadowComb(HTSnum).SubSurf(NGSS)).Name << '\n';
                    }
                }
            }

            for (HTS = 1; HTS <= TotSurfaces; ++HTS) {
                if (CastingSurface(HTS) && !Surface(HTS).IsConvex) {
                    if (DisplayExtraWarnings) {
                        ShowSevereError("DetermineShadowingCombinations: Surface=\"" + Surface(HTS).Name + "\" is a casting surface and is non-convex.");
                        ShowContinueError("...Shadowing values may be inaccurate. Check .shd report file for more surface shading details");
                    } else {
                        ++TotalCastingNonConvexSurfaces;
                    }
                }
            }

            if (TotalReceivingNonConvexSurfaces > 0) {
                ShowWarningMessage("DetermineShadowingCombinations: There are " + TrimSigDigits(TotalReceivingNonConvexSurfaces) +
                    " surfaces which are receiving surfaces and are non-convex.");
                ShowContinueError("...Shadowing values may be inaccurate. Check .shd report file for more surface shading details");
                ShowContinueError("...Add Output:Diagnostics,DisplayExtraWarnings; to see individual warnings for each surface.");
                TotalWarningErrors += TotalReceivingNonConvexSurfaces;
            }

            if (TotalCastingNonConvexSurfaces > 0) {
                ShowSevereMessage("DetermineShadowingCombinations: There are " + TrimSigDigits(TotalCastingNonConvexSurfaces) +
                    " surfaces which are casting surfaces and are non-convex.");
                ShowContinueError("...Shadowing values may be inaccurate. Check .shd report file for more surface shading details");
                ShowContinueError("...Add Output:Diagnostics,DisplayExtraWarnings; to see individual severes for each surface.");
                TotalSevereErrors += TotalCastingNonConvexSurfaces;
            }
        }

        CastingSurface.deallocate();

#ifndef EP_NO_OPENGL
        if (state.dataSolarShading->penumbra && state.dataSolarShading->penumbra->getNumSurfaces() > 0) {
            state.dataSolarShading->penumbra->setModel();
        }
#endif
    }

    void SHADOW(EnergyPlusData &state,
                int const iHour, // Hour index
                int const TS     // Time Step
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       Nov 2003, FCW: modify to do shadowing on shadowing surfaces
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is a driving routine for calculations of shadows
        // and sunlit areas used in computing the solar beam flux multipliers.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        Real64 XS; // Intermediate result
        Real64 YS; // Intermediate result
        Real64 ZS; // Intermediate result
        int N;     // Vertex number
        int NGRS;  // Coordinate transformation index
        int NVT;
        static Array1D<Real64> XVT; // X Vertices of Shadows
        static Array1D<Real64> YVT; // Y vertices of Shadows
        static Array1D<Real64> ZVT; // Z vertices of Shadows
        int HTS;         // Heat transfer surface number of the general receiving surface
        int GRSNR;       // Surface number of general receiving surface
        int NBKS;        // Number of back surfaces
        int NGSS;        // Number of general shadowing surfaces
        int NSBS;        // Number of subsurfaces (windows and doors)
        Real64 SurfArea; // Surface area. For walls, includes all window frame areas.
        // For windows, includes divider area

        if (state.dataSolarShading->ShadowOneTimeFlag) {
            XVT.allocate(MaxVerticesPerSurface + 1);
            YVT.allocate(MaxVerticesPerSurface + 1);
            ZVT.allocate(MaxVerticesPerSurface + 1);
            XVT = 0.0;
            YVT = 0.0;
            ZVT = 0.0;
            state.dataSolarShading->ShadowOneTimeFlag = false;
        }

#ifdef EP_Count_Calls
        if (iHour == 0) {
            ++NumShadow_Calls;
        } else {
            ++NumShadowAtTS_Calls;
        }
#endif

        state.dataSolarShading->SAREA = 0.0;

#ifndef EP_NO_OPENGL
        if (state.dataSolarShading->penumbra) {
            Real64 ElevSun = DataGlobalConstants::PiOvr2() - std::acos(state.dataSolarShading->SUNCOS(3));
            Real64 AzimSun = std::atan2(state.dataSolarShading->SUNCOS(1), state.dataSolarShading->SUNCOS(2));
            state.dataSolarShading->penumbra->setSunPosition(AzimSun, ElevSun);
            state.dataSolarShading->penumbra->submitPSSA();
        }
#endif

        for (GRSNR = 1; GRSNR <= TotSurfaces; ++GRSNR) {

            if (!ShadowComb(GRSNR).UseThisSurf) continue;

            state.dataSolarShading->SAREA(GRSNR) = 0.0;

            NGSS = ShadowComb(GRSNR).NumGenSurf;
            state.dataSolarShading->NGSSHC = 0;
            NBKS = ShadowComb(GRSNR).NumBackSurf;
            state.dataSolarShading->NBKSHC = 0;
            NSBS = ShadowComb(GRSNR).NumSubSurf;
            state.dataSolarShading->NRVLHC = 0;
            state.dataSolarShading->NSBSHC = 0;
            state.dataSolarShading->LOCHCA = 1;
            // Temporarily determine the old heat transfer surface number (HTS)
            HTS = GRSNR;

            if (state.dataSolarShading->CTHETA(GRSNR) < SunIsUpValue) { //.001) THEN ! Receiving surface is not in the sun

                state.dataSolarShading->SAREA(HTS) = 0.0;
                SHDSBS(state, iHour, GRSNR, NBKS, NSBS, HTS, TS);

            } else if ((NGSS <= 0) && (NSBS <= 0)) { // Simple surface--no shaders or subsurfaces

                state.dataSolarShading->SAREA(HTS) = Surface(GRSNR).NetAreaShadowCalc;
            } else { // Surface in sun and either shading surfaces or subsurfaces present (or both)

#ifndef EP_NO_OPENGL
                auto id = Surface(HTS).PenumbraID;
                if (state.dataSolarShading->penumbra && id >= 0) {
                    // SAREA(HTS) = buildingPSSF.at(id) / CTHETA(HTS);
                    state.dataSolarShading->SAREA(HTS) = state.dataSolarShading->penumbra->fetchPSSA(id) / state.dataSolarShading->CTHETA(HTS);
                    // SAREA(HTS) = penumbra->fetchPSSA(Surface(HTS).PenumbraID)/CTHETA(HTS);
                    for (int SS = 1; SS <= NSBS; ++SS) {
                        auto HTSS = ShadowComb(HTS).SubSurf(SS);
                        id = Surface(HTSS).PenumbraID;
                        if (id >= 0) {
                            // SAREA(HTSS) = buildingPSSF.at(id) / CTHETA(HTSS);
                            state.dataSolarShading->SAREA(HTSS) = state.dataSolarShading->penumbra->fetchPSSA(id) / state.dataSolarShading->CTHETA(HTSS);
                            // SAREA(HTSS) = penumbra->fetchPSSA(Surface(HTSS).PenumbraID)/CTHETA(HTSS);
                            if (state.dataSolarShading->SAREA(HTSS) > 0.0) {
                                if (iHour > 0 && TS > 0) SunlitFracWithoutReveal(TS, iHour, HTSS) = state.dataSolarShading->SAREA(HTSS) / Surface(HTSS).Area;
                            }
                        }
                    }
                } else if (!state.dataSolarShading->penumbra) {
#else
                {
#endif
                    NGRS = Surface(GRSNR).BaseSurf;
                    if (Surface(GRSNR).ShadowingSurf) NGRS = GRSNR;

                    // Compute the X and Y displacements of a shadow.
                    XS = Surface(NGRS).lcsx.x * state.dataSolarShading->SUNCOS(1) + Surface(NGRS).lcsx.y * state.dataSolarShading->SUNCOS(2) + Surface(NGRS).lcsx.z * state.dataSolarShading->SUNCOS(3);
                    YS = Surface(NGRS).lcsy.x * state.dataSolarShading->SUNCOS(1) + Surface(NGRS).lcsy.y * state.dataSolarShading->SUNCOS(2) + Surface(NGRS).lcsy.z * state.dataSolarShading->SUNCOS(3);
                    ZS = Surface(NGRS).lcsz.x * state.dataSolarShading->SUNCOS(1) + Surface(NGRS).lcsz.y * state.dataSolarShading->SUNCOS(2) + Surface(NGRS).lcsz.z * state.dataSolarShading->SUNCOS(3);

                    if (std::abs(ZS) > 1.e-4) {
                        state.dataSolarShading->XShadowProjection = XS / ZS;
                        state.dataSolarShading->YShadowProjection = YS / ZS;
                        if (std::abs(state.dataSolarShading->XShadowProjection) < 1.e-8) state.dataSolarShading->XShadowProjection = 0.0;
                        if (std::abs(state.dataSolarShading->YShadowProjection) < 1.e-8) state.dataSolarShading->YShadowProjection = 0.0;
                    } else {
                        state.dataSolarShading->XShadowProjection = 0.0;
                        state.dataSolarShading->YShadowProjection = 0.0;
                    }

                    CTRANS(GRSNR, NGRS, NVT, XVT, YVT, ZVT); // Transform coordinates of the receiving surface to 2-D form

                    // Re-order its vertices to clockwise sequential.
                    for (N = 1; N <= NVT; ++N) {
                        state.dataSolarShading->XVS(N) = XVT(NVT + 1 - N);
                        state.dataSolarShading->YVS(N) = YVT(NVT + 1 - N);
                    }

                    HTRANS1(state, 1, NVT); // Transform to homogeneous coordinates.

                    state.dataSolarShading->HCAREA(1) = -state.dataSolarShading->HCAREA(1); // Compute (+) gross surface area.
                    state.dataSolarShading->HCT(1) = 1.0;

                    SHDGSS(state, NGRS, iHour, TS, GRSNR, NGSS, HTS); // Determine shadowing on surface.

                    if (!state.dataSolarShading->CalcSkyDifShading) {
                        SHDBKS(state, Surface(GRSNR).BaseSurf, GRSNR, NBKS, HTS); // Determine possible back surfaces.
                    }
                }

                SHDSBS(state, iHour, GRSNR, NBKS, NSBS, HTS, TS); // Subtract subsurf areas from total

                // Error checking:  require that 0 <= SAREA <= AREA.  + or - .01*AREA added for round-off errors
                SurfArea = Surface(GRSNR).NetAreaShadowCalc;
                state.dataSolarShading->SAREA(HTS) = max(0.0, state.dataSolarShading->SAREA(HTS));

                state.dataSolarShading->SAREA(HTS) = min(state.dataSolarShading->SAREA(HTS), SurfArea);
            } // ...end of surface in sun/surface with shaders and/or subsurfaces IF-THEN block

            // NOTE:
            // There used to be a call to legacy subroutine SHDCVR here when the
            // zone type was not a standard zone.
        }
    }

    void SHDBKS(EnergyPlusData &state, int const NGRS, // Number of the general receiving surface
                int const CurSurf,
                int const NBKS, // Number of back surfaces
                int const HTS   // Heat transfer surface number of the general receiving surf
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This is the driving subroutine for computing
        // the sunlit areas for back surfaces.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        typedef Array2D<Int64>::size_type size_type;
        int I;
        int M;
        int N;
        int NVR;
        int NVT;                    // Number of vertices of back surface
        static Array1D<Real64> XVT; // X,Y,Z coordinates of vertices of
        static Array1D<Real64> YVT; // back surfaces projected into system
        static Array1D<Real64> ZVT; // relative to receiving surface
        int BackSurfaceNumber;
        int NS1; // Number of the figure being overlapped
        int NS2; // Number of the figure doing overlapping
        int NS3; // Location to place results of overlap

        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));

        if (state.dataSolarShading->SHDBKSOneTimeFlag) {
            XVT.allocate(MaxVerticesPerSurface + 1);
            YVT.allocate(MaxVerticesPerSurface + 1);
            ZVT.allocate(MaxVerticesPerSurface + 1);
            XVT = 0.0;
            YVT = 0.0;
            ZVT = 0.0;
            state.dataSolarShading->SHDBKSOneTimeFlag = false;
        }

        if ((NBKS <= 0) || (state.dataSolarShading->SAREA(HTS) <= 0.0) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) return;

        state.dataSolarShading->FBKSHC = state.dataSolarShading->LOCHCA + 1;

        for (I = 1; I <= NBKS; ++I) { // Loop through all back surfaces associated with the receiving surface

            BackSurfaceNumber = ShadowComb(CurSurf).BackSurf(I);

            if (state.dataSolarShading->CTHETA(BackSurfaceNumber) > -SunIsUpValue) continue; //-0.001) CYCLE ! go to next back surface since inside of this surface
            // cannot be in sun if the outside can be

            // Transform coordinates of back surface from general system to the
            // plane of the receiving surface

            CTRANS(BackSurfaceNumber, NGRS, NVT, XVT, YVT, ZVT);

            // Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
            // become clockwise sequential.

            for (N = 1; N <= NVT; ++N) {
                state.dataSolarShading->XVS(N) = XVT(N) - state.dataSolarShading->XShadowProjection * ZVT(N);
                state.dataSolarShading->YVS(N) = YVT(N) - state.dataSolarShading->YShadowProjection * ZVT(N);
            }

            // Transform to the homogeneous coordinate system.

            NS3 = state.dataSolarShading->LOCHCA + 1;
            state.dataSolarShading->HCT(NS3) = 0.0;
            HTRANS1(state, NS3, NVT);

            // Adjust near-duplicate points.

            NVR = state.dataSolarShading->HCNV(1);
            auto l3(state.dataSolarShading->HCX.index(NS3, 1));
            for (N = 1; N <= NVT; ++N, ++l3) {
                auto const x3(state.dataSolarShading->HCX[l3]); // [ l3 ] == ( NS3, N )
                auto const y3(state.dataSolarShading->HCY[l3]);
                size_type l1(0);
                for (M = 1; M <= NVR; ++M, ++l1) {
                    if (std::abs(state.dataSolarShading->HCX[l1] - x3) > 6) continue; // [ l1 ] == ( 1, M )
                    if (std::abs(state.dataSolarShading->HCY[l1] - y3) > 6) continue;
                    state.dataSolarShading->HCX[l3] = state.dataSolarShading->HCX[l1];
                    state.dataSolarShading->HCY[l3] = state.dataSolarShading->HCY[l1];
                    break;
                }
            }

            HTRANS0(state, NS3, NVT);

            // Determine area of overlap of projected back surface and receiving surface.

            NS1 = 1;
            NS2 = NS3;
            state.dataSolarShading->HCT(NS3) = 1.0;
            DeterminePolygonOverlap(state, NS1, NS2, NS3);

            if (state.dataSolarShading->OverlapStatus == state.dataSolarShading->NoOverlap) continue;                                           // to next back surface
            if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) break; // back surfaces DO loop

            // Increment back surface count.

            state.dataSolarShading->LOCHCA = NS3;
            state.dataSolarShading->HCNS(state.dataSolarShading->LOCHCA) = BackSurfaceNumber;
            state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA) = -state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA);
            state.dataSolarShading->NBKSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FBKSHC + 1;
        }
    }

    void SHDGSS(EnergyPlusData &state,
                int const NGRS,
                int const iHour,   // Hour Counter
                int const TS,      // TimeStep
                int const CurSurf, // Current Surface
                int const NGSS,    // Number of general shadowing surfaces
                int const HTS      // Heat transfer surface number of the general receiving surf
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines the shadows on a general receiving surface.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // Using/Aliasing
        using DataSystemVariables::DisableAllSelfShading;
        using DataSystemVariables::DisableGroupSelfShading;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleMinValue;
        using ScheduleManager::GetScheduleName;
        using ScheduleManager::LookUpScheduleValue;

        typedef Array2D<Int64>::size_type size_type;
        int GSSNR;             // General shadowing surface number
        int MainOverlapStatus; // Overlap status of the main overlap calculation not the check for
        // multiple overlaps (unless there was an error)
        static Array1D<Real64> XVT;
        static Array1D<Real64> YVT;
        static Array1D<Real64> ZVT;
        int NS1;         // Number of the figure being overlapped
        int NS2;         // Number of the figure doing overlapping
        int NS3;         // Location to place results of overlap
        Real64 SchValue; // Value for Schedule of shading transmittence

        if (state.dataSolarShading->SHDGSSOneTimeFlag) {
            XVT.dimension(MaxVerticesPerSurface + 1, 0.0);
            YVT.dimension(MaxVerticesPerSurface + 1, 0.0);
            ZVT.dimension(MaxVerticesPerSurface + 1, 0.0);
            state.dataSolarShading->SHDGSSOneTimeFlag = false;
        }

        state.dataSolarShading->FGSSHC = state.dataSolarShading->LOCHCA + 1;
        MainOverlapStatus = state.dataSolarShading->NoOverlap; // Set to ensure that the value from the last surface is not saved
        state.dataSolarShading->OverlapStatus = state.dataSolarShading->NoOverlap;

        if (NGSS <= 0) { // IF NO S.S., receiving surface FULLY SUNLIT.

            state.dataSolarShading->SAREA(HTS) = state.dataSolarShading->HCAREA(1); // Surface fully sunlit

        } else {

            int ExitLoopStatus(-1);
            auto const &GenSurf(ShadowComb(CurSurf).GenSurf);
            auto const sunIsUp(SunIsUpValue);
            for (int I = 1; I <= NGSS; ++I) { // Loop through all shadowing surfaces...

                GSSNR = GenSurf(I);

                if (state.dataSolarShading->CTHETA(GSSNR) > sunIsUp) continue; //.001) CYCLE ! NO SHADOW IF GSS IN SUNLIGHT.

                auto const &surface(Surface(GSSNR));
                bool const notHeatTransSurf(!surface.HeatTransSurf);

                //     This used to check to see if the shadowing surface was not opaque (within the scheduled dates of
                //            transmittance value.  Perhaps it ignored it if it were outside the range.  (if so, was an error)
                //     The proper action seems to be delete this statement all together, but there would also be no shading if
                //            the shading surface were transparent...
                //---former stmt      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
                //---former stmt            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR) == 0.0) CYCLE

                if (notHeatTransSurf) {
                    if (surface.IsTransparent) continue; // No shadow if shading surface is transparent
                    if (surface.SchedShadowSurfIndex > 0) {
                        if (LookUpScheduleValue(state, surface.SchedShadowSurfIndex, iHour) == 1.0) continue;
                        if (!state.dataSolarShading->CalcSkyDifShading) {
                            if (LookUpScheduleValue(state, surface.SchedShadowSurfIndex, iHour, TS) == 1.0) continue;
                        }
                    }
                }
                // Elimate shawdowing surfaces that is supposed to be disabled.
                if (DisableAllSelfShading) {
                    if (surface.Zone != 0) {
                        continue; // Disable all shadowing surfaces in all zones. Attached shading surfaces are not part of a zone, zone value is 0.
                    }
                } else if (DisableGroupSelfShading) {
                    std::vector<int> DisabledZones = Surface(CurSurf).DisabledShadowingZoneList;
                    bool isDisabledShadowSurf = false;
                    for (int i : DisabledZones) {
                        if (surface.Zone == i) {
                            isDisabledShadowSurf = true;
                            break;
                        }
                    }
                    if (isDisabledShadowSurf) continue; // Disable all shadowing surfaces in all disabled zones.
                }

                //      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
                //            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex) == 1.0) CYCLE

                // Transform shadow casting surface from cartesian to homogeneous coordinates according to surface type.

                if ((notHeatTransSurf) && (surface.BaseSurf != 0)) {

                    // For shadowing subsurface coordinates of shadow casting surface are relative to the receiving surface
                    // project shadow to the receiving surface

                    state.dataSolarShading->NVS = surface.Sides;
                    auto const &XV(ShadeV(GSSNR).XV);
                    auto const &YV(ShadeV(GSSNR).YV);
                    auto const &ZV(ShadeV(GSSNR).ZV);
                    for (int N = 1; N <= state.dataSolarShading->NVS; ++N) {
                        state.dataSolarShading->XVS(N) = XV(N) - state.dataSolarShading->XShadowProjection * ZV(N);
                        state.dataSolarShading->YVS(N) = YV(N) - state.dataSolarShading->YShadowProjection * ZV(N);
                    }

                } else {
                    // Transform coordinates of shadow casting surface from general system to the system relative to the receiving surface
                    int NVT;
                    CTRANS(GSSNR, NGRS, NVT, XVT, YVT, ZVT);
                    CLIP(state, NVT, XVT, YVT, ZVT); // Clip portions of the shadow casting surface which are behind the receiving surface

                    if (state.dataSolarShading->NumVertInShadowOrClippedSurface <= 2) continue;

                    // Project shadow from shadow casting surface along sun's rays to receiving surface Shadow vertices
                    // become clockwise sequential

                    for (int N = 1; N <= state.dataSolarShading->NumVertInShadowOrClippedSurface; ++N) {
                        state.dataSolarShading->XVS(N) = state.dataSolarShading->XVC(N) - state.dataSolarShading->XShadowProjection * state.dataSolarShading->ZVC(N);
                        state.dataSolarShading->YVS(N) = state.dataSolarShading->YVC(N) - state.dataSolarShading->YShadowProjection * state.dataSolarShading->ZVC(N);
                    }
                }

                // Transform to the homogeneous coordinate system.

                NS3 = state.dataSolarShading->LOCHCA + 1;
                HTRANS1(state, NS3, state.dataSolarShading->NVS);

                // Adjust near-duplicate points.

                assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
                assert(state.dataSolarShading->HCX.index(1, 1) == 0u);
                size_type j(state.dataSolarShading->HCX.index(NS3, 1));
                size_type NVR(state.dataSolarShading->HCNV(1));
                for (int N = 1; N <= state.dataSolarShading->NumVertInShadowOrClippedSurface; ++N, ++j) { // Tuned Logic change: break after 1st "close" point found
                    auto const HCX_N(state.dataSolarShading->HCX[j]);                                     // [ j ] == ( NS3, N )
                    auto const HCY_N(state.dataSolarShading->HCY[j]);
                    for (size_type l = 0; l < NVR; ++l) { // [ l ] == ( 1, l+1 )
                        auto const delX(std::abs(state.dataSolarShading->HCX[l] - HCX_N));
                        if (delX > 6) continue;
                        auto const delY(std::abs(state.dataSolarShading->HCY[l] - HCY_N));
                        if (delY > 6) continue;
                        if (delX > 0) state.dataSolarShading->HCX[j] = state.dataSolarShading->HCX[l]; // [ j ] == ( NS3, N )
                        if (delY > 0) state.dataSolarShading->HCY[j] = state.dataSolarShading->HCY[l];
                        break;
                    }
                }
                HTRANS0(state, NS3, state.dataSolarShading->NumVertInShadowOrClippedSurface);
                if (!state.dataSolarShading->CalcSkyDifShading) {
                    if (iHour != 0) {
                        SchValue = LookUpScheduleValue(state, surface.SchedShadowSurfIndex, iHour, TS);
                    } else {
                        SchValue = surface.SchedMinValue;
                    }
                } else {
                    SchValue = surface.SchedMinValue;
                }

                state.dataSolarShading->HCT(NS3) = SchValue;

                // Determine overlap of shadow with receiving surface

                state.dataSolarShading->CurrentShadowingSurface = I;
                state.dataSolarShading->CurrentSurfaceBeingShadowed = GSSNR;
                NS1 = 1;
                NS2 = NS3;
                DeterminePolygonOverlap(state, NS1, NS2, NS3);
                //  Next statement is special to deal with transmitting shading devices
                if (state.dataSolarShading->OverlapStatus == state.dataSolarShading->FirstSurfWithinSecond && SchValue > 0.0) state.dataSolarShading->OverlapStatus = state.dataSolarShading->PartialOverlap;
                MainOverlapStatus = state.dataSolarShading->OverlapStatus;
                ExitLoopStatus = MainOverlapStatus;

                if (MainOverlapStatus == state.dataSolarShading->NoOverlap) { // No overlap of general surface shadow and receiving surface
                                                      // Continue
                } else if ((MainOverlapStatus == state.dataSolarShading->FirstSurfWithinSecond) || (MainOverlapStatus == state.dataSolarShading->TooManyVertices) ||
                           (MainOverlapStatus == state.dataSolarShading->TooManyFigures)) {
                    goto ShadowingSurfaces_exit;
                } else if ((MainOverlapStatus == state.dataSolarShading->SecondSurfWithinFirst) || (MainOverlapStatus == state.dataSolarShading->PartialOverlap)) {
                    // Determine overlaps with previous shadows.
                    state.dataSolarShading->LOCHCA = NS3;
                    state.dataSolarShading->NGSSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FGSSHC + 1;
                    if (state.dataSolarShading->NGSSHC > 1) MULTOL(state, state.dataSolarShading->LOCHCA, state.dataSolarShading->FGSSHC - 1, state.dataSolarShading->NGSSHC - 1); // HOYT - Remove this call
                } else {
                    goto ShadowingSurfaces_exit;
                }

                ExitLoopStatus = -1;
            }
        ShadowingSurfaces_exit:;

            // Compute sunlit area of surface (excluding effects of subsurfs).

            if (ExitLoopStatus == state.dataSolarShading->FirstSurfWithinSecond) { // Surface fully shaded
                state.dataSolarShading->SAREA(HTS) = 0.0;
                state.dataSolarShading->LOCHCA = state.dataSolarShading->FGSSHC;

            } else if ((ExitLoopStatus == state.dataSolarShading->TooManyVertices) || (ExitLoopStatus == state.dataSolarShading->TooManyFigures)) { // Array limits exceeded, estimate
                state.dataSolarShading->SAREA(HTS) = 0.25 * state.dataSolarShading->HCAREA(1);

            } else {

                // Compute the sunlit area here.
                // Call UnionShadow(FGSSHC,LOCHCA)

                state.dataSolarShading->NGSSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FGSSHC + 1;
                if (state.dataSolarShading->NGSSHC <= 0) {
                    state.dataSolarShading->SAREA(HTS) = state.dataSolarShading->HCAREA(1); // Surface fully sunlit
                } else {
                    Real64 A(state.dataSolarShading->HCAREA(1)); // Area
                    for (int i = state.dataSolarShading->FGSSHC, e = state.dataSolarShading->FGSSHC + state.dataSolarShading->NGSSHC - 1; i <= e; ++i) {
                        A += state.dataSolarShading->HCAREA(i) * (1.0 - state.dataSolarShading->HCT(i));
                    }
                    state.dataSolarShading->SAREA(HTS) = A;
                    if (state.dataSolarShading->SAREA(HTS) <= 0.0) { // Surface fully shaded
                        state.dataSolarShading->SAREA(HTS) = 0.0;
                        state.dataSolarShading->LOCHCA = state.dataSolarShading->FGSSHC;
                    }
                }
            }
        }

        state.dataSolarShading->NGSSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FGSSHC + 1;
    }

    void CalcInteriorSolarOverlaps(EnergyPlusData &state, int const iHour, // Hour Index
                                   int const NBKS,  // Number of back surfaces associated with this GRSNR (in general, only
                                   int const HTSS,  // Surface number of the subsurface (exterior window)
                                   int const GRSNR, // General receiving surface number (base surface of the exterior window)
                                   int const TS     // Time step Index
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   January 1999
        //       MODIFIED       Nov 2001, FW: include beam radiation overlaps with
        //                       back windows and doors; previously these subsurfaces ignored.
        //                      May 2002, FW: fix problem where reveal was not being considered
        //                       in calculating overlap areas if window is shaded only by reveal.
        //                      June 2002, FW: fix problem that gave incorrect calculation when
        //                       window is not shaded only by reveal
        //                      June 2002, FW: remove incorrect multiplication of overlap areas
        //                       by sunlit fraction when window is shaded only by reveal

        // PURPOSE OF THIS SUBROUTINE:
        // For an exterior window with surface number HTSS, determines (1) the surface numbers of back
        // surfaces receiving beam radiation from the window and (2) for each such back surface, the area
        // of the portion of the window sending beam radiation to the back surface; this is called the
        // "overlap area."

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // SUBROUTINE ARGUMENT DEFINITIONS:
        //  some of these will receive beam radiation from HTSS this hour)

        // SUBROUTINE PARAMETER DEFINITIONS:
        int const WindowShadedOnlyByReveal(2); // for use with RevealStatus

        typedef Array2D<Int64>::size_type size_type;
        int JBKS;           // Counter of back surfaces with non-zero overlap with HTSS
        int BackSurfNum;    // Back surface number

        bool UseSimpleDistribution; // TRUE means simple interior solar distribution
        // (all incoming beam assumed to strike floor),
        // FALSE means exact interior solar distribution
        // (track which back surfaces beam illuminates)

        // Tuned Linear indexing

        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCY));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCA));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCB));
        assert(equal_dimensions(state.dataSolarShading->HCX, state.dataSolarShading->HCC));

        if (state.dataSolarShading->SAREA(HTSS) > 0.0) {

            UseSimpleDistribution = false;

            if ((NBKS <= 0) || (Surface(GRSNR).ExtBoundCond > 0)) {

                UseSimpleDistribution = true;

            } else {
                // Using 'exact' distribution, replace subsurface HC entries with reveal HC entries
                // so that the reveal HC is used in calculating interior solar overlap areas

                // Adding the following line fixes a problem where, if the window was shaded only
                // by reveal, then the reveal was not considered in calculating interior solar
                // overlap areas (FCW 5/3/02).
                // IF(Surface(HTSS)%Reveal > 0.0) NRVLHC = 1
                // Changing the line to the following avoids incorrect calculation when window is not shaded
                // only by reveal (FCW 6/28/02).
                if (state.dataSolarShading->WindowRevealStatus(TS, iHour, HTSS) == WindowShadedOnlyByReveal) state.dataSolarShading->NRVLHC = 1;
                if (state.dataSolarShading->NRVLHC > 0) {
                    for (int I = 1; I <= state.dataSolarShading->NRVLHC; ++I) {
                        int const iS(state.dataSolarShading->FSBSHC - 1 + I);
                        int const iR(state.dataSolarShading->FRVLHC - 1 + I);
                        state.dataSolarShading->HCT(iS) = state.dataSolarShading->HCT(iR);
                        state.dataSolarShading->HCNV(iS) = state.dataSolarShading->HCNV(iR);
                        state.dataSolarShading->HCAREA(iS) = state.dataSolarShading->HCAREA(iR);
                        size_type lS(state.dataSolarShading->HCX.index(iS, 1));
                        size_type lR(state.dataSolarShading->HCX.index(iR, 1));
                        for (int J = 1; J <= state.dataSolarShading->MaxHCV; ++J, ++lS, ++lR) { // [ lS ] == ( iS, J ), [ lR ] == ( iR, J )
                            state.dataSolarShading->HCX[lS] = state.dataSolarShading->HCX[lR];
                            state.dataSolarShading->HCY[lS] = state.dataSolarShading->HCY[lR];
                            state.dataSolarShading->HCA[lS] = state.dataSolarShading->HCA[lR];
                            state.dataSolarShading->HCB[lS] = state.dataSolarShading->HCB[lR];
                            state.dataSolarShading->HCC[lS] = state.dataSolarShading->HCC[lR];
                        }
                    }
                    state.dataSolarShading->NSBSHC = state.dataSolarShading->NRVLHC;
                }
            }

            // Check for array space.
            if (state.dataSolarShading->FSBSHC + state.dataSolarShading->NBKSHC > state.dataSolarShading->MaxHCS) UseSimpleDistribution = true;

            if (!UseSimpleDistribution) { // Compute overlaps

                std::map<unsigned, float> pssas;

#ifndef EP_NO_OPENGL
                if (state.dataSolarShading->penumbra) {
                    // Add back surfaces to array
                    std::vector<unsigned> pbBackSurfaces;
                    for (auto bkSurfNum : ShadowComb(GRSNR).BackSurf) {
                        if (bkSurfNum == 0) continue;
                        if (state.dataSolarShading->CTHETA(bkSurfNum) < SunIsUpValue) {
                            pbBackSurfaces.push_back(Surface(bkSurfNum).PenumbraID);
                        }
                    }
                    pssas = state.dataSolarShading->penumbra->calculateInteriorPSSAs({(unsigned)Surface(HTSS).PenumbraID}, pbBackSurfaces);
                    //penumbra->renderInteriorScene({(unsigned)Surface(HTSS).PenumbraID}, pbBackSurfaces);

                    JBKS = 0;
                    for (auto bkSurfNum : ShadowComb(GRSNR).BackSurf) {
                        if (bkSurfNum == 0) continue;
                        if (pssas[Surface(bkSurfNum).PenumbraID] > 0) {
                            ++JBKS;
                            BackSurfaces(TS, iHour, JBKS, HTSS) = bkSurfNum;
                            Real64 OverlapArea = pssas[Surface(bkSurfNum).PenumbraID]/state.dataSolarShading->CTHETA(HTSS);
                            OverlapAreas(TS, iHour, JBKS, HTSS) = OverlapArea * SurfWinGlazedFrac(HTSS);
                        }
                    }

                }
#endif

                if (!state.dataSolarShading->penumbra) {

                    state.dataSolarShading->FINSHC = state.dataSolarShading->FSBSHC + state.dataSolarShading->NSBSHC;

                    JBKS = 0;

                    for (int IBKS = 1; IBKS <= state.dataSolarShading->NBKSHC; ++IBKS) { // Loop over back surfaces to GRSNR this hour. NBKSHC is the number of
                        // back surfaces that would receive beam radiation from the base surface, GRSNR,
                        // if the base surface was transparent. In general, some (at least one) or all of these
                        // will receive beam radiation from the exterior window subsurface, HTSS, of GRSNR,
                        // depending on the size of HTSS and its location on GRSNR

                        BackSurfNum = state.dataSolarShading->HCNS(state.dataSolarShading->FBKSHC - 1 + IBKS);

                        // Determine if this back surface number can receive beam radiation from the
                        // exterior window, HTSS, this hour, i.e., overlap area is positive

                        state.dataSolarShading->LOCHCA = state.dataSolarShading->FINSHC - 1;

                        MULTOL(state, state.dataSolarShading->FBKSHC - 1 + IBKS, state.dataSolarShading->FSBSHC - 1, state.dataSolarShading->NSBSHC);

                        // Compute overlap area for this back surface

                        state.dataSolarShading->NINSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FINSHC + 1;
                        if (state.dataSolarShading->NINSHC <= 0) continue;
                        Real64 OverlapArea = state.dataSolarShading->HCAREA(state.dataSolarShading->FINSHC);
                        for (int J = 2; J <= state.dataSolarShading->NINSHC; ++J) {
                            OverlapArea += state.dataSolarShading->HCAREA(state.dataSolarShading->FINSHC - 1 + J) * (1.0 - state.dataSolarShading->HCT(state.dataSolarShading->FINSHC - 1 + J));
                        }

                        if (OverlapArea > 0.001) {
                            ++JBKS;
                            if (JBKS <= MaxBkSurf) {
                                BackSurfaces(TS, iHour, JBKS, HTSS) = BackSurfNum;
                                int baseSurfaceNum = DataSurfaces::Surface(BackSurfNum).BaseSurf;
                                OverlapAreas(TS, iHour, JBKS, HTSS) = OverlapArea * SurfWinGlazedFrac(HTSS);
                                // If this is a subsurface, subtract its overlap area from its base surface
                                if (baseSurfaceNum != BackSurfNum) {
                                    for (int iBaseBKS = 1; iBaseBKS <= JBKS; ++iBaseBKS) {
                                        if (baseSurfaceNum == BackSurfaces(TS, iHour, iBaseBKS, HTSS)) {
                                            OverlapAreas(TS, iHour, iBaseBKS, HTSS) =
                                                max(0.0, OverlapAreas(TS, iHour, iBaseBKS, HTSS) - OverlapAreas(TS, iHour, JBKS, HTSS));
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    } // End of loop over back surfaces
                }
            }
        } // End of check that sunlit area > 0.
    }
    void CalcInteriorSolarDistribution(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   January 1999
        //       MODIFIED       Nov 1999, FW, for Window5 calculation method
        //                      Oct 2000, FW: add transmitted solar variables for reporting
        //                      Mar 2001, FW: add new calc of solar absorbed by window shades
        //                      May 2001, FW: add calc of solar transmitted and absorbed by window blinds
        //                      Oct 2001, LL: remove interpolation, solar now at time step
        //                      Oct 2001, FW: add solar transmitted through interior windows
        //                      Mar 24, 2001, FW: remove incorrect multiplication of Boverlap by sunlit fraction
        //                                        since effect of shadowing is already included in Aoverlap
        //                      Apr 2001, FW: add effects of beam solar reflection from outside and inside reveals
        //                      Jan 2003, FW: add between-glass shades and blinds
        //                      Dec 2003, FW: report beam incident on inside of surface
        //                      Jan 2004, FW: for blinds with horizontal slats, allow different diffuse/diffuse
        //                                    transmittance for ground and sky solar
        //                      Apr 2004, FW: allow diffusing glazing
        //                      May 2006, RR: allow external window screen
        //                      Jan 2010, TH: add calculating and reporting of WinBmBmSolar, WinBmDifSolar,
        //                                    WinBmBmSolarEnergy, and WinBmDifSolarEnergy
        //                      Jun 2013, SV: scheduled surface gains for walls and windows

        // PURPOSE OF THIS SUBROUTINE:
        // For a time step, calculates solar radiation absorbed by exterior
        // surfaces and interior solar radiation distribution

        using General::BlindBeamBeamTrans;
        using General::InterpBlind;
        using General::InterpProfSlatAng;
        using General::InterpSlatAng;
        using General::InterpSw;
        using General::POLYF;
        using ScheduleManager::GetCurrentScheduleValue;
        using namespace DataDaylightingDevices;
        using DaylightingDevices::TransTDD;
        using namespace DataWindowEquivalentLayer;

        static Array1D<Real64> AbsBeamWin;                  // Glass layer beam solar absorptance of a window
        static Array1D<Real64> AbsBeamWinEQL(CFSMAXNL + 1); // layers beam solar absorptance of a window
        static Array1D<Real64> ExtBeamAbsByShadFac; // Factor for exterior beam radiation absorbed by shade (1/m2) (absorbed radation = beam incident * ExtBeamAbsByShad
        static Array1D<Real64> IntBeamAbsByShadFac; // Like ExtBeamAbsByShadFac, but for interior beam radiation.
        static Array1D<Real64> WinTransBmSolar;     // Factor for exterior beam solar transmitted through window, or window plus shade, into zone at current time (m2)
        static Array1D<Real64> WinTransDifSolar; // Factor for exterior diffuse solar transmitted through window, or window plus shade, into zone at current time (m2)
        static Array1D<Real64> WinTransDifSolarGnd; // Factor for exterior ground diffuse solar transmitted through window with horizontally-slatted blind into zone at current time (m2)
        static Array1D<Real64> WinTransDifSolarSky; // Factor for exterior sky diffuse solar transmitted through  window with horizontally-slatted blind into zone at current time (m2)
        static Array2D<Real64> AbsSolBeamEQL(2, CFSMAXNL + 1);     // absorbed exterior beam radiation by layers fraction
        static Array2D<Real64> AbsSolDiffEQL(2, CFSMAXNL + 1);     // absorbed exterior diffuse radiation by layers fraction
        static Array2D<Real64> AbsSolBeamBackEQL(2, CFSMAXNL + 1); // absorbed interior beam radiation by layers fraction from back
        // Array2D< Real64 > AbsSolDiffBackEQL( CFSMAXNL+1, 2 ); // absorbed exterior diffuse radiation by layers fraction from back //Unused

        Array1D<Real64> CFBoverlap;    // Sum of boverlap for each back surface
        Array2D<Real64> CFDirBoverlap; // Directional boverlap (Direction, IBack)

        if (state.dataSolarShading->MustAllocSolarShading) {
            EnclSolDBIntWin.allocate(NumOfZones);
            IntBeamAbsByShadFac.allocate(TotSurfaces);
            ExtBeamAbsByShadFac.allocate(TotSurfaces);
            WinTransBmSolar.allocate(TotSurfaces);
            WinTransDifSolar.allocate(TotSurfaces);
            WinTransDifSolarGnd.allocate(TotSurfaces);
            WinTransDifSolarSky.allocate(TotSurfaces);
            state.dataSolarShading->MustAllocSolarShading = false;
        }

#ifdef EP_Count_Calls
        ++NumIntSolarDist_Calls;
#endif

        Real64 AbWin = 0.0; // Factor for front beam radiation absorbed in window glass layer
        Real64 AbWinSh = 0.0; // Like AbWin, but for shaded window
        Real64 TBmBm = 0.0; // Beam-beam solar transmittance for bare window or window with switchable glazing
        Real64 TBmDif = 0.0; // Beam-diffuse solar transmittance for bare window with diffusing glass
        Real64 TBmBmEQL = 0.0; // Beam-beam solar transmittance for equivalent layer model window W/WO shade
        Real64 TBmDiffEQL = 0.0; // Beam-diffuse solar transmittance for equivalent layer model window W/WO shade
        Real64 WinTransBmBmSolar = 0.0; // Factor for exterior beam to beam solar transmitted through window, or window plus shade, into zone at current time (m2)
        Real64 WinTransBmDifSolar = 0.0; // Factor for exterior beam to diffuse solar transmitted through window, or window plus shade, into zone at current time (m2)

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            WinTransBmSolar(SurfNum) = 0.0;
            WinTransDifSolar(SurfNum) = 0.0;
            WinTransDifSolarGnd(SurfNum) = 0.0;
            WinTransDifSolarSky(SurfNum) = 0.0;
            IntBeamAbsByShadFac(SurfNum) = 0.0;
            ExtBeamAbsByShadFac(SurfNum) = 0.0;
        }

        for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
            EnclSolDS(zoneNum) = 0.0;
            DGZone(zoneNum) = 0.0;
            DBZone(zoneNum) = 0.0;
            EnclSolDBSSG(zoneNum) = 0.0;
            EnclSolDBIntWin(zoneNum) = 0.0;
            ZoneTransSolar(zoneNum) = 0.0;
            ZoneBmSolFrExtWinsRep(zoneNum) = 0.0;
            ZoneBmSolFrIntWinsRep(zoneNum) = 0.0;
            ZoneDifSolFrExtWinsRep(zoneNum) = 0.0;
            ZoneDifSolFrIntWinsRep(zoneNum) = 0.0;
            ZoneTransSolarEnergy(zoneNum) = 0.0;
            ZoneBmSolFrExtWinsRepEnergy(zoneNum) = 0.0;
            ZoneBmSolFrIntWinsRepEnergy(zoneNum) = 0.0;
            ZoneDifSolFrExtWinsRepEnergy(zoneNum) = 0.0;
            ZoneDifSolFrIntWinsRepEnergy(zoneNum) = 0.0;
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                SurfWinA = 0.0;
                SurfWinBmSolar(SurfNum) = 0.0;
                SurfWinBmBmSolar(SurfNum) = 0.0;
                SurfWinBmDifSolar(SurfNum) = 0.0;
                // energy
                SurfWinBmSolarEnergy(SurfNum) = 0.0;
                SurfWinBmBmSolarEnergy(SurfNum) = 0.0;
                SurfWinBmDifSolarEnergy(SurfNum) = 0.0;

                SurfWinDifSolarEnergy(SurfNum) = 0.0;
                SurfWinDifSolar(SurfNum) = 0.0;
                SurfWinBmSolTransThruIntWinRep(SurfNum) = 0.0;
                SurfWinBmSolTransThruIntWinRepEnergy(SurfNum) = 0.0;
            }
            int const firstSurfOpague = Zone(zoneNum).NonWindowSurfaceFirst;
            int const lastSurfOpague = Zone(zoneNum).NonWindowSurfaceLast;
            for (int SurfNum = firstSurfOpague; SurfNum <= lastSurfOpague; ++SurfNum) {
                SurfOpaqAI(SurfNum) = 0.0;
                SurfOpaqAO(SurfNum) = 0.0;
            }
        }

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            // Solar entering a zone as beam or diffuse radiation, originating as beam solar incident on exterior windows)/(Beam normal solar) [W/(W/m2)]
            Real64 BTOTZone = 0.0;
            // Beam radiation from exterior windows absorbed in a zone or transmitted through
            Real64 BABSZone = 0.0;

            // Loop over exterior surfaces in this zone
            auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));
            // delete values from previous timestep
            if (AnyBSDF) SurfWinACFOverlap = 0.0;


            for (int const SurfNum : thisEnclosure.SurfacePtr) {

                if (((Surface(SurfNum).ExtBoundCond != ExternalEnvironment) && (Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)) &&
                    SurfWinOriginalClass(SurfNum) != SurfaceClass::TDD_Diffuser)
                    continue;
                if (!Surface(SurfNum).HeatTransSurf) continue;
                // TH added 3/24/2010 while debugging CR 7872
                if (!Surface(SurfNum).ExtSolar) continue;
                int ConstrNum = Surface(SurfNum).Construction;
                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                if (SurfWinStormWinFlag(SurfNum) == 1) {
                    ConstrNum = Surface(SurfNum).StormWinConstruction;
                    ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                }
                int BlNum = SurfWinBlindNumber(SurfNum);
                int ScNum = SurfWinScreenNumber(SurfNum);
                int ShadeFlag = SurfWinShadingFlag(SurfNum); // Set in subr. WindowShadingManager
                Real64 ProfAng = 0.0; // Window solar profile angle (radians)
                if (ShadeFlag != ExtScreenOn && BlNum > 0) ProfileAngle(SurfNum, SOLCOS, Blind(BlNum).SlatOrientation, ProfAng);
                Real64 SlatAng = SurfWinSlatAngThisTS(SurfNum);
                Real64 VarSlats = SurfWinMovableSlats(SurfNum);
                int SurfNum2 = SurfNum;
                int PipeNum = SurfWinTDDPipeNum(SurfNum);
                if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    SurfNum2 = TDDPipe(PipeNum).Dome;
                }
                int InShelfSurf = 0; // Inside daylighting shelf surface number
                int OutShelfSurf = 0; // Outside daylighting shelf surface number
                int ShelfNum = Surface(SurfNum).Shelf;
                if (ShelfNum > 0) { // Daylighting shelf
                    InShelfSurf = Shelf(ShelfNum).InSurf;
                    OutShelfSurf = Shelf(ShelfNum).OutSurf;
                }
                Real64 CosInc = CosIncAng(TimeStep, HourOfDay, SurfNum2);
                Real64 SunLitFract = SunlitFrac(TimeStep, HourOfDay, SurfNum2);

                //-------------------------------------------------------------------------
                // EXTERIOR BEAM SOLAR RADIATION ABSORBED ON THE OUTSIDE OF OPAQUE SURFACES
                //-------------------------------------------------------------------------

                if (SunLitFract > 0.0 && state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) {
                    SurfOpaqAO(SurfNum) = state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar * CosInc * SunLitFract;

                    // Note: movable insulation, if present, is accounted for in subr. InitIntSolarDistribution,
                    // where QRadSWOutMvIns is calculated from QRadSWOutAbs and insulation solar absorptance
                }

                //-------------------------------------------------------------------------------------------
                // EXTERIOR BEAM AND DIFFUSE SOLAR RADIATION ABSORBED IN THE GLASS LAYERS OF EXTERIOR WINDOWS
                //-------------------------------------------------------------------------------------------

                if (Surface(SurfNum).Class != SurfaceClass::Window && Surface(SurfNum).Class != SurfaceClass::TDD_Dome) continue;

                // Somewhat of a kludge
                if (Surface(SurfNum).Class == SurfaceClass::TDD_Dome || SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser)
                    SunlitFracWithoutReveal(TimeStep, HourOfDay, SurfNum) = SunLitFract; // Frames/dividers not allowed

                WinTransBmBmSolar = 0.0;
                WinTransBmDifSolar = 0.0;

                Real64 InOutProjSLFracMult = SurfaceWindow(SurfNum).InOutProjSLFracMult(HourOfDay);
                if (SunlitFracWithoutReveal(TimeStep, HourOfDay, SurfNum) > 0.0) {

                    if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel && SurfWinWindowModelType(SurfNum) != WindowEQLModel) {

                        // For bare glazing or switchable glazing, the following includes the effects of
                        // (1) diffuse solar produced by beam solar incident on the outside and inside reveal
                        // surfaces, and (2) absorption of beam solar by outside and inside reveal surfaces.
                        // If there is an exterior shade/blind both of these effects are ignored. If there
                        // is an interior or between-glass shade/blind the effects of beam incident on
                        // inside reveal surfaces is ignored.

                        int NGlass = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;

                        for (int Lay = 1; Lay <= NGlass; ++Lay) {
                            AbWin = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).AbsBeamCoef({1, 6}, Lay)) * CosInc * SunLitFract *
                                    SurfaceWindow(SurfNum).OutProjSLFracMult(HourOfDay);
                            Real64 ADiffWin = state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay); // Diffuse solar absorptance of glass layer, bare window
                            if (ShadeFlag <= 0 || ShadeFlag >= 10) {

                                // Bare window (ShadeFlag = -1 or 0 or shading device of off)

                                // Add contribution of beam reflected from outside and inside reveal
                                SurfWinA(Lay, SurfNum) = AbWin +
                                                         SurfWinOutsRevealDiffOntoGlazing(SurfNum) * state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay) +
                                                         SurfWinInsRevealDiffOntoGlazing(SurfNum) * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay);

                            } else {

                                // Shade, screen, blind or switchable glazing on (ShadeFlag > 0)
                                Real64 FracSunLit = SunLitFract * SurfaceWindow(SurfNum).OutProjSLFracMult(HourOfDay); // Effective fraction of window that is sunlit;
                                Real64 ADiffWinSh;     // Diffuse solar absorptance of glass layer, window with shading device
                                if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn) FracSunLit = SunLitFract;
                                if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == SwitchableGlazing) {

                                    // Shade or switchable glazing on

                                    AbWinSh = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).AbsBeamCoef({1, 6}, Lay)) * CosInc * FracSunLit;

                                    ADiffWinSh = state.dataConstruction->Construct(ConstrNumSh).AbsDiff(Lay);

                                } else {
                                    // Blind or screen on

                                    if (Lay == 1 && ShadeFlag != ExtScreenOn) ProfileAngle(SurfNum, SOLCOS, Blind(BlNum).SlatOrientation, ProfAng);

                                    if (ShadeFlag == IntBlindOn) {
                                        Real64 TGlBm = 0; // Glazing system front solar beam transmittance
                                        Real64 RGlDiffBack = 0; // Glazing system back diffuse solar reflectance
                                        Real64 RhoBlFront = 0; // Blind solar front beam reflectance
                                        Real64 RhoBlDiffFront = 0; // Blind solar front diffuse reflectance
                                        // Interior blind on
                                        if (Lay == 1) {
                                            TGlBm = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                                            RGlDiffBack = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                                            RhoBlFront = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl);
                                            RhoBlDiffFront = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                                        }
                                        Real64 AGlDiffBack = state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay); // Glass layer back diffuse solar absorptance
                                        AbWinSh = AbWin + (TGlBm * AGlDiffBack * RhoBlFront / (1.0 - RhoBlFront * RGlDiffBack)) * CosInc * FracSunLit;
                                        ADiffWinSh = ADiffWin + state.dataConstruction->Construct(ConstrNum).TransDiff * AGlDiffBack * RhoBlDiffFront /
                                                                (1.0 - RhoBlDiffFront * RGlDiffBack);
                                    } else if (ShadeFlag == ExtBlindOn) {
                                        Real64 TBlBmBm = 0; // Blind solar front beam-beam transmittance
                                        Real64 TBlDifDif = 0; // Diffuse-diffuse solar transmittance of blind
                                        Real64 TBlBmDiff = 0; // Blind solar front beam-diffuse transmittance
                                        Real64 RhoBlBack = 0; // Blind solar back beam-diffuse reflectance
                                        Real64 RhoBlDiffBack = 0; // Blind solar back diffuse reflectance
                                        Real64 RGlFront = 0; // Glazing system solar front beam-beam reflectance
                                        Real64 RGlDiffFront = 0; // Glazing system front diffuse solar reflectance
                                        Real64 RGlDifFr = 0; // Diffuse front reflectance of glass
                                        Real64 RhoBlDifDifBk = 0; // Diffuse-diffuse back refectance of blind
                                        // Exterior blind on
                                        if (Lay == 1) {
                                            TBlBmBm = BlindBeamBeamTrans(
                                                    ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                                            TBlBmDiff = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);
                                            RhoBlBack = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffRefl);
                                            RhoBlDiffBack = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                                            RGlFront = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                                            RGlDiffFront = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                            TBlDifDif = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                                            RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                            RhoBlDifDifBk = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                                        }
                                        Real64 AGlDiffFront = state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay); // Glass layer front diffuse solar absorptance
                                        AbWinSh = TBlBmBm * AbWin +
                                                  ((TBlBmBm * RGlFront * RhoBlBack + TBlBmDiff) * AGlDiffFront / (1 - RGlDiffFront * RhoBlDiffBack)) *
                                                  CosInc * FracSunLit;
                                        // ADiffWinSh = 0.0  ! Assumes no contribution from reveal reflection when exterior blind in place
                                        //  Replaced above line with (FCW, 2/10/03):
                                        ADiffWinSh = ADiffWin * TBlDifDif / (1.0 - RGlDifFr * RhoBlDifDifBk);

                                    } else if (ShadeFlag == ExtScreenOn) {

                                        Real64 TScBmBm = 0; // Screen solar front beam-beam transmittance
                                        Real64 TScBmDiff = 0; // Screen solar front beam-diffuse transmittance
                                        Real64 RScBack = 0; // Screen solar back beam-diffuse reflectance
                                        Real64 RScDifBack = 0; // Screen solar back diffuse-diffuse reflectance
                                        Real64 RGlFront = 0; // Glazing system solar front beam-beam reflectance
                                        Real64 RGlDiffFront = 0; // Glazing system front diffuse solar reflectance
                                        Real64 TScDifDif = 0; // Diffuse-diffuse solar transmittance of screen
                                        Real64 RGlDifFr = 0; // Diffuse front reflectance of glass
                                        // Exterior screen on
                                        if (Lay == 1) {
                                            TScBmBm = SurfaceScreens(ScNum).BmBmTrans;
                                            TScBmDiff = SurfaceScreens(ScNum).BmDifTrans;
                                            RScBack = SurfaceScreens(ScNum).ReflectSolBeamFront;
                                            RScDifBack = SurfaceScreens(ScNum).DifReflect;
                                            RGlFront = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                                            RGlDiffFront = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                            TScDifDif = SurfaceScreens(ScNum).DifDifTrans;
                                            RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                        }

                                        //             Reduce the bare window absorbed beam by the screen beam transmittance and then account for
                                        //             interreflections
                                        AbWinSh = TScBmBm * AbWin + (TScBmBm * RGlFront * RScBack + TScBmDiff) * state.dataConstruction->Construct(ConstrNum).AbsDiff(Lay) /
                                                                    (1.0 - RGlDiffFront * RScDifBack) * CosInc * FracSunLit;

                                        ADiffWinSh = ADiffWin * TScDifDif / (1.0 - RGlDifFr * RScDifBack);

                                    } else {
                                        // Between-glass blind on
                                        Real64 t1;       // Bare-glass beam solar transmittance for glass layers 1,2 and 3
                                        Real64 t2;
                                        Real64 t3;
                                        Real64 t1t2; // t1*t2
                                        Real64 af1;  // Bare-glass beam solar front absorptance for glass layers 1,2 and 3
                                        Real64 af2;
                                        Real64 af3;
                                        Real64 ab1; // Bare-glass beam solar back absorptance for glass layers 1,2 and 3
                                        Real64 ab2;
                                        Real64 rf1; // Bare-glass beam solar front reflectance for glass layers 1,2 and 3
                                        Real64 rf2;
                                        Real64 rf3;
                                        Real64 rb1; // Bare-glass beam solar back reflectance for glass layers 1,2 and 3
                                        Real64 rb2;
                                        Real64 td1; // Bare-glass diffuse solar transmittance for glass layers 1,2 and 3
                                        Real64 td2;
                                        Real64 td1td2; // td1*td2
                                        Real64 afd1;   // Bare-glass diffuse solar front absorptance for glass layers 1,2 and 3
                                        Real64 afd2;
                                        Real64 afd3;
                                        Real64 abd1; // Bare-glass diffuse solar back absorptance for glass layers 1,2 and 3
                                        Real64 abd2;
                                        Real64 rfd1; // Bare-glass diffuse solar front reflectance for glass layers 1,2 and 3
                                        Real64 rfd2;
                                        Real64 rfd3;
                                        Real64 rbd1; // Bare-glass diffuse solar back reflectance for glass layers 1,2 and 3
                                        Real64 rbd2;
                                        Real64 tfshBB; // Bare-blind front and back beam-beam solar transmittance
                                        Real64 tbshBB;
                                        Real64 tfshBd; // Bare-blind front and back beam-diffuse solar transmittance
                                        Real64 tbshBd;
                                        Real64 tfshd; // Bare-blind front and back diffuse-diffuse solar transmittance
                                        Real64 tbshd;
                                        Real64 afshB; // Bare-blind front and back beam solar absorptance
                                        Real64 abshB;
                                        Real64 afshd; // Bare-blind front and back diffuse solar absorptance
                                        Real64 abshd;
                                        Real64 rfshB; // Bare-blind front and back beam solar reflectance
                                        Real64 rbshB;
                                        Real64 rfshd; // Bare-blind front and back diffuse solar reflectance
                                        Real64 rbshd;

                                        // Isolated glass and blind properties at current incidence angle, profile angle and slat angle
                                        if (Lay == 1) {
                                            t1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 1));
                                            t2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 2));
                                            af1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).afBareSolCoef({1, 6}, 1));
                                            af2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).afBareSolCoef({1, 6}, 2));
                                            ab1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).abBareSolCoef({1, 6}, 1));
                                            ab2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).abBareSolCoef({1, 6}, 2));
                                            rf1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 1));
                                            rf2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 2));
                                            rb1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rbBareSolCoef({1, 6}, 1));
                                            rb2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rbBareSolCoef({1, 6}, 2));
                                            td1 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(1);
                                            td2 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(2);
                                            afd1 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(1);
                                            afd2 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(2);
                                            abd1 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(1);
                                            abd2 = state.dataConstruction->Construct(ConstrNum).abBareSolDiff(2);
                                            rfd1 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(1);
                                            rfd2 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(2);
                                            rbd1 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(1);
                                            rbd2 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(2);
                                            tfshBB = BlindBeamBeamTrans(
                                                    ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                                            tfshBd = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);
                                            tfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                                            tbshBB = BlindBeamBeamTrans(ProfAng,
                                                                        DataGlobalConstants::Pi() - SlatAng,
                                                                        Blind(BlNum).SlatWidth,
                                                                        Blind(BlNum).SlatSeparation,
                                                                        Blind(BlNum).SlatThickness);
                                            tbshBd = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffTrans);
                                            tbshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffTrans);
                                            afshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamAbs);
                                            abshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamAbs);
                                            afshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffAbs);
                                            abshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffAbs);
                                            rfshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl);
                                            rbshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffRefl);
                                            rfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                                            rbshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                                        }

                                        if (Lay == 1 && NGlass == 3) {
                                            t1t2 = t1 * t2;
                                            td1td2 = td1 * td2;
                                            t3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 3));
                                            af3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).afBareSolCoef({1, 6}, 3));
                                            rf3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 3));
                                            afd3 = state.dataConstruction->Construct(ConstrNum).afBareSolDiff(3);
                                            rfd3 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(3);
                                        }

                                        if (NGlass == 2) {
                                            if (Lay == 1) {
                                                AbWinSh =
                                                        CosInc * FracSunLit *
                                                        (af1 + t1 * tfshBB * rf2 * tbshBB * ab1 +
                                                         t1 * (rfshB + rfshB * rbd1 * rfshd + tfshBB * rf2 * tbshBd + tfshBd * rfd2 * tbshd) * abd1);
                                                ADiffWinSh = afd1 + td1 * (rfshd + rfshd * rbd1 * rfshd + tfshd * rfd2 * tbshd) * abd1;
                                            } else if (Lay == 2) {
                                                AbWinSh = CosInc * FracSunLit *
                                                          (t1 * rfshB * af2 +
                                                           t1 * (rfshB * rf2 * rbshd + tfshBd * (1 + rfd2 * rbshd) + rfshB * rbd1 * tfshd) * afd2);
                                                ADiffWinSh = td1 * (tfshd * (1 + rfd2 * rbshd) + rfshd * rbd1 * tfshd) * afd2;
                                            }
                                        } // End of check if NGlass = 2

                                        if (NGlass == 3) {
                                            if (Lay == 1) {
                                                AbWinSh = CosInc * FracSunLit *
                                                          (af1 + t1 * rf2 * ab1 + t1t2 * tfshBB * rf3 * tbshBB * t2 * ab1 +
                                                           t1t2 * (rfshB * td2 + rfshB * rbd2 * rfshd * td2 + tfshBd * rfd3 * tbshd * td2) * abd1);
                                                ADiffWinSh = afd1 + td1 * rbd2 * abd1 +
                                                             td1td2 *
                                                             (rfshd * (1 + rbd2 * rfshd + td2 * rbd1 * td2 * rfshd) +
                                                              tfshd * (rfd3 * tbshd + rfd3 * rbshd * rfd3 * tbshd)) *
                                                             td2 * abd1;
                                            } else if (Lay == 2) {
                                                AbWinSh =
                                                        CosInc * FracSunLit *
                                                        (t1 * af2 + t1t2 * (tfshBB * rf3 * tbshBB * ab2 + rfshB * td2 * rbd1 * afd2) +
                                                         t1t2 * (rfshB * (1 + rbd2 * rfshd) + tfshBB * rf3 * tbshBd + tfshBd * rfd3 * tbshd) * abd2);
                                                ADiffWinSh = td1 * afd2 + td1td2 * rfshd * td2 * rbd1 * afd2 +
                                                             td1td2 * (rfshd * (1 + rbd2 * rfshd) + tfshd * rfd3 * tbshd) * abd2;
                                            } else if (Lay == 3) {
                                                AbWinSh = CosInc * FracSunLit *
                                                          (t1t2 * tfshBB * af3 + t1t2 *
                                                                                 (tfshBB * rf3 * rbshB + tfshBd * (1 + rfd3 * rbshd) +
                                                                                  rfshB * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) *
                                                                                 afd3);
                                                ADiffWinSh =
                                                        td1td2 * (tfshd * (1 + rfd3 * rbshd) + rfshd * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) * afd3;
                                            }
                                        } // End of check if NGlass = 3

                                    } // End of check if blind is interior, exterior or between-glass
                                }     // End of check if a blind is on

                                if (ShadeFlag != SwitchableGlazing) {

                                    // Interior or between glass shade or blind on

                                    SurfWinA(Lay, SurfNum) = AbWinSh;
                                    // Add contribution of diffuse from beam on outside reveal
                                    if (ShadeFlag == IntShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn)
                                        SurfWinA(Lay, SurfNum) += ADiffWinSh * SurfWinOutsRevealDiffOntoGlazing(SurfNum);

                                } else {
                                    // Switchable glazing

                                    Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum);
                                    SurfWinA(Lay, SurfNum) = InterpSw(SwitchFac, AbWin, AbWinSh);
                                    // Add contribution of diffuse from beam on outside and inside reveal
                                    SurfWinA(Lay, SurfNum) +=
                                            InterpSw(SwitchFac, ADiffWin, ADiffWinSh) * SurfWinOutsRevealDiffOntoGlazing(SurfNum) +
                                            InterpSw(SwitchFac, state.dataConstruction->Construct(ConstrNum).AbsDiffBack(Lay), state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(Lay)) *
                                            SurfWinInsRevealDiffOntoGlazing(SurfNum);
                                }
                            } // End of check if window has shading device
                        }     // End of loop over window glass layers

                        //-----------------------------------------
                        // EXTERIOR BEAM ABSORBED BY SHADING DEVICE
                        //-----------------------------------------

                        // Exterior beam absorbed by INTERIOR SHADE

                        if (ShadeFlag == IntShadeOn) {
                            // Note that AbsBeamShadeCoef includes effect of shade/glazing inter-reflection
                            Real64 AbsShade = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).AbsBeamShadeCoef); // Interior shade or blind beam solar absorptance

                            ExtBeamAbsByShadFac(SurfNum) = (AbsShade * CosInc * SunLitFract * InOutProjSLFracMult +
                                                            SurfWinOutsRevealDiffOntoGlazing(SurfNum) * state.dataConstruction->Construct(ConstrNumSh).AbsDiffShade) *
                                                           SurfWinGlazedFrac(SurfNum);
                            // In the above, GlazedFrac corrects for shadowing of divider onto interior shade
                        }

                        // Exterior beam absorbed by EXTERIOR SHADE

                        if (ShadeFlag == ExtShadeOn) {
                            ExtBeamAbsByShadFac(SurfNum) = state.dataConstruction->Construct(ConstrNumSh).AbsDiffShade * CosInc * SunLitFract;
                        }

                        // Exterior beam absorbed by BETWEEN-GLASS SHADE

                        if (ShadeFlag == BGShadeOn) {
                            Real64 AbsShade = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).AbsBeamShadeCoef);
                            ExtBeamAbsByShadFac(SurfNum) = AbsShade * CosInc * SunLitFract +
                                                           SurfWinOutsRevealDiffOntoGlazing(SurfNum) * state.dataConstruction->Construct(ConstrNumSh).AbsDiffShade;
                        }

                        // Exterior beam absorbed by INTERIOR BLIND

                        if (ShadeFlag == IntBlindOn) {
                            TBmBm = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                            Real64 RhoBlFront = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl);
                            Real64 AbsBlFront = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamAbs); // Blind solar front beam absorptance
                            Real64 RhoBlDiffFront = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                            Real64 AbsBlDiffFront = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffAbs); // Blind solar front diffuse absorptance
                            Real64 RGlDiffBack = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                            Real64 AbsShade = TBmBm * (AbsBlFront + RhoBlFront * RGlDiffBack * AbsBlDiffFront / (1.0 - RhoBlDiffFront * RGlDiffBack));
                            Real64 AbsShadeDiff = state.dataConstruction->Construct(ConstrNum).TransDiff *
                                           (AbsBlDiffFront + RhoBlDiffFront * RGlDiffBack * AbsBlDiffFront / (1.0 - RhoBlDiffFront * RGlDiffBack)); // Interior shade or blind diffuse solar absorptance

                            ExtBeamAbsByShadFac(SurfNum) = (AbsShade * CosInc * SunLitFract * InOutProjSLFracMult +
                                                            SurfWinOutsRevealDiffOntoGlazing(SurfNum) * AbsShadeDiff) *
                                                           SurfWinGlazedFrac(SurfNum);
                            // In the above, GlazedFrac corrects for shadowing of divider onto interior blind
                        }

                        // Exterior beam absorbed by EXTERIOR BLIND

                        if (ShadeFlag == ExtBlindOn) {
                            Real64 TBlBmBm =
                                    BlindBeamBeamTrans(ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                            Real64 RGlFront = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                            Real64 AbsBlFront = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamAbs);
                            Real64 AbsBlBack = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamAbs); // Blind solar back beam absorptance
                            Real64 AbsBlDiffBack = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffAbs); // Blind solar back diffuse absorptance
                            Real64 RGlDiffFront = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                            Real64 RhoBlDiffBack = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                            Real64 RhoBlBack = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffRefl);
                            Real64 TBlBmDiff = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);
                            Real64 AbsShade =
                                    AbsBlFront + AbsBlBack * RGlFront * TBlBmBm +
                                    (AbsBlDiffBack * RGlDiffFront / (1.0 - RhoBlDiffBack * RGlDiffFront)) * (RGlFront * TBlBmBm * RhoBlBack + TBlBmDiff);
                            ExtBeamAbsByShadFac(SurfNum) = AbsShade * CosInc * SunLitFract * InOutProjSLFracMult;
                        }

                        // Exterior beam absorbed by EXTERIOR SCREEN
                        if (ShadeFlag == ExtScreenOn) {
                            Real64 TScBmBm = SurfaceScreens(SurfWinScreenNumber(SurfNum)).BmBmTrans;
                            //        TScBmDiff     = SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmDifTrans
                            Real64 RGlFront = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                            Real64 RGlDiffFront = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;

                            Real64 AbsScBeam = SurfaceScreens(ScNum).AbsorpSolarBeamFront; // Screen solar beam absorptance
                            Real64 AbsScDiffBack = SurfaceScreens(ScNum).DifScreenAbsorp; // Screen solar back diffuse absorptance
                            Real64 RScDifBack = SurfaceScreens(ScNum).DifReflect;
                            Real64 RScBack = SurfaceScreens(ScNum).ReflectSolBeamFront;

                            Real64 AbsScreen = AbsScBeam * (1.0 + TScBmBm * RGlFront) +
                                        (AbsScDiffBack * TScBmBm * RGlFront * RGlDiffFront * RScBack / (1.0 - RScDifBack * RGlDiffFront)); // Exterior screen beam solar absorptance

                            ExtBeamAbsByShadFac(SurfNum) = AbsScreen * CosInc * SunLitFract * InOutProjSLFracMult;
                        }

                        // Exterior beam absorbed by BETWEEN-GLASS BLIND
                        if (ShadeFlag == BGBlindOn) {
                            Real64 AbsShade = 0.0;
                            Real64 AbsShadeDiff = 0.0;
                            Real64 t1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 1));
                            Real64 t2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 2));
                            Real64 td1 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(1);
                            Real64 tfshBB = BlindBeamBeamTrans( ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                            Real64 tfshBd = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);
                            Real64 tfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                            Real64 afshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamAbs);
                            Real64 afshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffAbs);
                            Real64 abshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffAbs);
                            Real64 rfshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl);
                            Real64 rfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                            Real64 rfd2 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(2);
                            Real64 rbd1 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(1);
                            Real64 rbd2 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(2);
                            if (NGlass == 2) {
                                Real64 rf2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 2));
                                Real64 abshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamAbs);
                                AbsShade = t1 * (afshB + tfshBB * rf2 * abshB + tfshBd * rfd2 * abshd + rfshB * rbd1 * afshd);
                                AbsShadeDiff = td1 * (afshd * (1 + rfshd * rbd1) + tfshd * rfd2 * abshd);
                            } else if (NGlass == 3) {
                                Real64 t1t2 = t1 * t2;
                                Real64 td2 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(2);
                                Real64 td1td2 = td1 * td2;
                                Real64 rf3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 3));
                                Real64 rfd3 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(3);
                                AbsShade = t1t2 * (afshB * (1 + tfshBB * rf3) + afshd * (tfshBd * rfd3 + rfshB * (rbd2 + td2 * rbd1 * td2)));
                                AbsShadeDiff = td1td2 * (afshd + tfshd * rfd3 * abshd + rfshd * (rfd2 + td2 * rbd2 * td2) * afshd);
                            }
                            ExtBeamAbsByShadFac(SurfNum) = AbsShade * CosInc * SunLitFract * InOutProjSLFracMult +
                                                           SurfWinOutsRevealDiffOntoGlazing(SurfNum) * AbsShadeDiff;
                        } // End of check if between-glass blind

                    } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {

                        int FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum);

                        // Do not read from schedule file here since this will be called only if direct beam is hitting the window and schedule
                        // will not be loaded in that case even if diffuse part of solar radiation is entering through the window
                        if (FenSolAbsPtr == 0) {
                            // Put in the equivalent layer absorptions
                            // Simon: This should not be multiplied with CosInc since Abs coefficient already includes angular
                            // factor
                            for (int Lay = 1; Lay <= SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).NLayers;
                                 ++Lay) {
                                auto absBeamWin = SurfaceWindow(SurfNum)
                                        .ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState)
                                        .WinBmFtAbs(HourOfDay, TimeStep, Lay);
                                AbWin = absBeamWin * CosInc * SunLitFract * SurfaceWindow(SurfNum).OutProjSLFracMult(HourOfDay);

                                // Add contribution of beam reflected from outside and inside reveal
                                SurfWinA(Lay, SurfNum) =
                                        AbWin +
                                        SurfWinOutsRevealDiffOntoGlazing(SurfNum) *
                                        SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinFtHemAbs(Lay) +
                                        SurfWinInsRevealDiffOntoGlazing(SurfNum) *
                                        SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinBkHemAbs(Lay);
                            }
                        }

                    } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                        // call the ASHWAT fenestration model for optical properties
                        // determine the beam radiation absorptance and tranmittance of the
                        // the equivalent layer window model
                        WindowEquivalentLayer::CalcEQLOpticalProperty(state, SurfNum, isBEAM, AbsSolBeamEQL);

                        // recalcuate the diffuse absorptance and transmittance of the
                        // the equivalent layer window model if there is shade control
                        int EQLNum = state.dataConstruction->Construct(Surface(SurfNum).Construction).EQLConsPtr; // equivalent layer fenestration index
                        if (CFS(EQLNum).ISControlled) {
                            WindowEquivalentLayer::CalcEQLOpticalProperty(state, SurfNum, isDIFF, AbsSolDiffEQL);
                        } else {
                            AbsSolDiffEQL(_, {1, CFS(EQLNum).NL + 1}) = state.dataWindowEquivalentLayer->CFSDiffAbsTrans(_, {1, CFS(EQLNum).NL + 1}, EQLNum);
                        }
                        state.dataConstruction->Construct(ConstrNum).TransDiff = AbsSolDiffEQL(1, CFS(EQLNum).NL + 1);

                        for (int Lay = 1; Lay <= CFS(EQLNum).NL + 1; ++Lay) {
                            // Factor for front beam radiation absorbed for equivalent layer window model
                            Real64 AbWinEQL = AbsSolBeamEQL(1, Lay) * CosInc * SunLitFract * InOutProjSLFracMult;
                            if (CFS(EQLNum).L(1).LTYPE != ltyGLAZE) {
                                // if the first layer is not glazing (or it is a shade) do not
                                SurfWinA(Lay, SurfNum) = AbWinEQL;
                            } else {
                                // the first layer is a glazing, include the outside reveal reflection
                                // and the inside reveal reflection until indoor shade layer is encountered.
                                if (CFS(EQLNum).L(Lay).LTYPE == ltyGLAZE) {
                                    SurfWinA(Lay, SurfNum) = AbWinEQL + SurfWinOutsRevealDiffOntoGlazing(SurfNum) * AbsSolBeamEQL(1, Lay) +
                                                             SurfWinInsRevealDiffOntoGlazing(SurfNum) * AbsSolDiffEQL(2, Lay);
                                } else {
                                    SurfWinA(Lay, SurfNum) = AbWinEQL + SurfWinOutsRevealDiffOntoGlazing(SurfNum) * AbsSolBeamEQL(1, Lay);
                                }
                            }
                        }
                        TBmBmEQL = AbsSolBeamEQL(1, CFS(EQLNum).NL + 1);
                        // Beam-diffuse transmittance
                        TBmDiffEQL = max(0.0, AbsSolBeamEQL(2, CFS(EQLNum).NL + 1));
                        // Beam-beam transmittance: difference between beam-total and beam-diffuse transmittance
                        TBmBmEQL = max(0.0, (TBmBmEQL - TBmDiffEQL));
                    }

                } // End of SunlitFrac check

                //-----------------------------------------------------------------
                // SKY AND GROUND DIFFUSE SOLAR GAIN INTO ZONE FROM EXTERIOR WINDOW
                //-----------------------------------------------------------------

                Real64 SkySolarInc = SurfSkySolarInc(SurfNum); // Incident solar radiation on a window: sky diffuse plus beam reflected from obstruction (W/m2)
                Real64 GndSolarInc = SurfGndSolarInc(SurfNum); // Incident solar radiation on a window from the ground (W/m2)
                Real64 EnclSolDSWin; // Factor for sky diffuse solar gain into a zone from an exterior window
                Real64 DGZoneWin; // Factor for ground diffuse solar gain into a zone
                Real64 DiffTrans; // Glazing diffuse solar transmittance (including shade/blind/switching, if present)
                Real64 DiffTransGnd;   // Ground diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
                Real64 DiffTransBmGnd; // Complex fen: diffuse solar transmittance for ground-reflected beam radiation
                Real64 DiffTransSky;   // Sky diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
                Real64 NomDiffTrans;

                if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel &&
                    SurfWinWindowModelType(SurfNum) != WindowEQLModel) { // Regular window
                    DiffTrans = state.dataConstruction->Construct(ConstrNum).TransDiff;
                    if (DifSolarRad != 0.0) {
                        EnclSolDSWin = (SkySolarInc * DiffTrans * Surface(SurfNum).Area) / (DifSolarRad);
                    } else {
                        EnclSolDSWin = (SkySolarInc * DiffTrans * Surface(SurfNum).Area) / (1.e-8);
                    }
                    if (GndSolarRad != 0.0) {
                        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum).Area) / (GndSolarRad);
                    } else {
                        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum).Area) / (1.e-8);
                    }
                } else if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    DiffTrans = TransTDD(state, PipeNum, CosInc, SolarAniso);
                    EnclSolDSWin = AnisoSkyMult(SurfNum2) * DiffTrans * Surface(SurfNum).Area;
                    DGZoneWin = Surface(SurfNum2).ViewFactorGround * TDDPipe(PipeNum).TransSolIso * Surface(SurfNum).Area;

                } else if (Surface(SurfNum).Class == SurfaceClass::TDD_Dome) {
                    DiffTrans = state.dataConstruction->Construct(ConstrNum).TransDiff;

                    EnclSolDSWin = 0.0; // Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
                    DGZoneWin = 0.0; // Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER

                } else if (OutShelfSurf > 0) { // Outside daylighting shelf
                    DiffTrans = state.dataConstruction->Construct(ConstrNum).TransDiff;

                    EnclSolDSWin = AnisoSkyMult(SurfNum) * DiffTrans * Surface(SurfNum).Area;
                    // Shelf diffuse solar radiation
                    Real64 ShelfSolarRad = (BeamSolarRad * SunlitFrac(TimeStep, HourOfDay, OutShelfSurf) * CosIncAng(TimeStep, HourOfDay, OutShelfSurf) +
                                     DifSolarRad * AnisoSkyMult(OutShelfSurf)) * Shelf(ShelfNum).OutReflectSol;

                    // Add all reflected solar from the outside shelf to the ground solar
                    // NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!

                    // In order to get the effect of the daylighting shelf in here, must take into account the fact that this
                    // is ultimately multiplied by GndSolarRad to get QD and QDV in InitSolarHeatGains.
                    // DGZoneWin = (GndVF*Trans*Area*GndSolarRad + ShelfVF*Trans*Area*ShelfSolarRad) / GndSolarRad
                    if (GndSolarRad != 0.0) {
                        DGZoneWin = (Surface(SurfNum).ViewFactorGround * DiffTrans * Surface(SurfNum).Area * GndSolarRad +
                                     Shelf(ShelfNum).ViewFactor * DiffTrans * Surface(SurfNum).Area * ShelfSolarRad) /
                                    GndSolarRad;
                    } else {
                        DGZoneWin = 0.0;
                    }

                } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) { // complex fenestration
                    int FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum);
                    if (FenSolAbsPtr == 0) {
                        // Sky diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
                        DiffTransSky = SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinSkyTrans;
                        if (DifSolarRad != 0.0) {
                            EnclSolDSWin = SkySolarInc * DiffTransSky * Surface(SurfNum).Area / (DifSolarRad);
                        } else {
                            EnclSolDSWin = SkySolarInc * DiffTransSky * Surface(SurfNum).Area / (1.e-8);
                        }
                        // Ground diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
                        DiffTransGnd = SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinSkyGndTrans;
                        // Complex fen: diffuse solar transmittance for ground-reflected beam radiation
                        DiffTransBmGnd = SurfaceWindow(SurfNum)
                                .ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState)
                                .WinBmGndTrans(HourOfDay, TimeStep);
                        if (GndSolarRad != 0.0) {
                            DGZoneWin =
                                    ((SurfWinBmGndSolarInc(SurfNum) * DiffTransBmGnd + SurfWinSkyGndSolarInc(SurfNum) * DiffTransGnd) *
                                     Surface(SurfNum).Area) /
                                    (GndSolarRad);
                        } else {
                            DGZoneWin =
                                    ((SurfWinBmGndSolarInc(SurfNum) * DiffTransBmGnd + SurfWinSkyGndSolarInc(SurfNum) * DiffTransGnd) *
                                     Surface(SurfNum).Area) /
                                    (1.e-8);
                        }

                        // Define the effective transmittance for total sky and ground radiation
                        if ((SkySolarInc + SurfWinBmGndSolarInc(SurfNum) + SurfWinSkyGndSolarInc(SurfNum)) != 0.0) {
                            DiffTrans = (SkySolarInc * DiffTransSky + SurfWinBmGndSolarInc(SurfNum) * DiffTransBmGnd +
                                         SurfWinSkyGndSolarInc(SurfNum) * DiffTransGnd) /
                                        (SkySolarInc + SurfWinBmGndSolarInc(SurfNum) + SurfWinSkyGndSolarInc(SurfNum));
                        } else {
                            DiffTrans = 0.0;
                        }

                        // Also update the nominal diffuse transmittance
                        NomDiffTrans = SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinDiffTrans;

                        // Do not store in TransDiff because it is not used by BSDF and rest of the code uses it as flag for opaque
                        // surface incorrectly assuming wall heat transfer routines for windows.
                        // Construct( Surface( SurfNum ).Construction ).TransDiff = NomDiffTrans;
                    }

                } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {

                    DiffTrans = state.dataConstruction->Construct(ConstrNum).TransDiff;

                    if (DifSolarRad != 0.0) {
                        EnclSolDSWin = (SkySolarInc * DiffTrans * Surface(SurfNum).Area) / (DifSolarRad);
                    } else {
                        EnclSolDSWin = (SkySolarInc * DiffTrans * Surface(SurfNum).Area) / (1.e-8);
                    }
                    if (GndSolarRad != 0.0) {
                        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum).Area) / (GndSolarRad);
                    } else {
                        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum).Area) / (1.e-8);
                    }
                }

                if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel && SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                    Real64 EnclSolDSWinSh;  // Factor for sky diffuse solar gain into a zone from a shaded exterior window
                    Real64 DGZoneWinSh;  // Factor for ground diffuse solar gain into a zone from a shaded exterior window
                    if (ShadeFlag <= 0 || ShadeFlag >= 10) {
                        // Unshaded window
                        EnclSolDS(enclosureNum) += EnclSolDSWin;
                        DGZone(enclosureNum) += DGZoneWin;
                    } else if (ShadeFlag != SwitchableGlazing) {
                        // Shade or blind
                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                            // Shade or screen
                            DiffTrans = state.dataConstruction->Construct(ConstrNumSh).TransDiff;
                        } else {
                            // Blind
                            DiffTrans = InterpSlatAng(SlatAng, VarSlats, state.dataConstruction->Construct(ConstrNumSh).BlTransDiff);
                            // For blinds with horizontal slats, allow different diffuse/diffuse transmittance for
                            // ground and sky solar
                            if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                DiffTransGnd = InterpSlatAng(SlatAng, VarSlats, state.dataConstruction->Construct(ConstrNumSh).BlTransDiffGnd);
                                DiffTransSky = InterpSlatAng(SlatAng, VarSlats, state.dataConstruction->Construct(ConstrNumSh).BlTransDiffSky);
                            }
                        }
                        if (DifSolarRad != 0.0) {
                            EnclSolDSWinSh = SkySolarInc * DiffTrans * Surface(SurfNum).Area / (DifSolarRad);
                        } else {
                            EnclSolDSWinSh = SkySolarInc * DiffTrans * Surface(SurfNum).Area / (1.e-8);
                        }

                        if (GndSolarRad != 0.0) {
                            DGZoneWinSh = GndSolarInc * DiffTrans * Surface(SurfNum).Area / (GndSolarRad);
                        } else {
                            DGZoneWinSh = GndSolarInc * DiffTrans * Surface(SurfNum).Area / (1.e-8);
                        }

                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                            if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                Real64 CosTlt = Surface(SurfNum).CosTilt;

                                if (DifSolarRad != 0.0) {
                                    EnclSolDSWinSh = SkySolarInc * Surface(SurfNum).Area *
                                                  (0.5 * std::abs(CosTlt) * DiffTransGnd + (1.0 - 0.5 * std::abs(CosTlt)) * DiffTransSky) /
                                                  (DifSolarRad);
                                } else {
                                    EnclSolDSWinSh = SkySolarInc * Surface(SurfNum).Area *
                                                  (0.5 * std::abs(CosTlt) * DiffTransGnd + (1.0 - 0.5 * std::abs(CosTlt)) * DiffTransSky) / (1.e-8);
                                }

                                if (GndSolarRad != 0.0) {
                                    DGZoneWinSh = GndSolarInc * Surface(SurfNum).Area *
                                                  ((1.0 - 0.5 * std::abs(CosTlt)) * DiffTransGnd + 0.5 * std::abs(CosTlt) * DiffTransSky) /
                                                  (GndSolarRad);
                                } else {
                                    DGZoneWinSh = GndSolarInc * Surface(SurfNum).Area *
                                                  ((1.0 - 0.5 * std::abs(CosTlt)) * DiffTransGnd + 0.5 * std::abs(CosTlt) * DiffTransSky) / (1.e-8);
                                }
                            }
                        }
                        EnclSolDS(enclosureNum) += EnclSolDSWinSh;
                        DGZone(enclosureNum) += DGZoneWinSh;
                    } else {
                        // Switchable glazing
                        Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum); // Switching factor for a window
                        DiffTrans = InterpSw(SwitchFac, state.dataConstruction->Construct(ConstrNum).TransDiff, state.dataConstruction->Construct(ConstrNumSh).TransDiff);
                        if (DifSolarRad != 0.0) {
                            EnclSolDSWinSh = SkySolarInc * DiffTrans * Surface(SurfNum).Area / (DifSolarRad);
                        } else {
                            EnclSolDSWinSh = SkySolarInc * DiffTrans * Surface(SurfNum).Area / (1.e-8);
                        }
                        if (GndSolarRad != 0.0) {
                            DGZoneWinSh = GndSolarInc * DiffTrans * Surface(SurfNum).Area / (GndSolarRad);
                        } else {
                            DGZoneWinSh = GndSolarInc * DiffTrans * Surface(SurfNum).Area / (1.e-8);
                        }
                        EnclSolDS(enclosureNum) += InterpSw(SwitchFac, EnclSolDSWin, EnclSolDSWinSh);
                        DGZone(enclosureNum) += InterpSw(SwitchFac, DGZoneWin, DGZoneWinSh);
                    }
                } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                    EnclSolDS(enclosureNum) += EnclSolDSWin;
                    DGZone(enclosureNum) += DGZoneWin;
                } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                    // For equivalent layer model the zone total diffuse solar heat gain
                    // through exterior fenestrations are reported as single value.
                    EnclSolDSWin = SkySolarInc * DiffTrans * Surface(SurfNum).Area / (DifSolarRad + 1.e-8);
                    DGZoneWin = GndSolarInc * DiffTrans * Surface(SurfNum).Area / (GndSolarRad + 1.e-8);

                    EnclSolDS(enclosureNum) += EnclSolDSWin;
                    DGZone(enclosureNum) += DGZoneWin;
                }
                //-----------------------------------------------------------------
                // BEAM SOLAR ON EXTERIOR WINDOW TRANSMITTED AS BEAM AND/OR DIFFUSE
                //-----------------------------------------------------------------

                TBmBm = 0.0;
                TBmDif = 0.0;
                Real64 TBmAllShBlSc = 0.0; // Beam-beam + beam-diffuse transmittance for window with shade, blind, screen, or switchable glazing
                Real64 TBmBmShBlSc = 0.0; // Beam-beam transmittance for window with shade, blind, screen, or switchable glazing
                Real64 TBmDifShBlSc = 0.0; // Beam-diffuse transmittance for window with shade, blind, screen, or switchable glazing
                Real64 TBmBmBl; // Beam-beam transmittance for window with blind
                Real64 TBmBmSc; // Beam-beam transmittance for window with screen
                Real64 TDifBare; // Bare diffuse transmittance of exterior window

                // Beam-beam transmittance for bare exterior window
                if (SunLitFract > 0.0) {
                    if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                        TBmDif = TransTDD(state, PipeNum, CosInc, SolarBeam);
                        TDDPipe(PipeNum).TransSolBeam = TBmDif; // Report variable
                    } else if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel &&
                               SurfWinWindowModelType(SurfNum) != WindowEQLModel) { // Regular window
                        if (!SurfWinSolarDiffusing(SurfNum)) {                      // Clear glazing
                            TBmBm = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);  //[-]
                        } else {                                                           // Diffusing glazing
                            TBmDif = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef); //[-]
                        }
                    } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                        // Need to check what effect, if any, defining these here has
                        TBmBm = SurfaceWindow(SurfNum)
                                .ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState)
                                .WinDirSpecTrans(HourOfDay, TimeStep);
                        TBmDif = SurfaceWindow(SurfNum)
                                         .ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState)
                                         .WinDirHemiTrans(HourOfDay, TimeStep) -
                                 TBmBm;
                    } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                        // get ASHWAT fenestration model beam-beam and beam-diffuse properties
                        TBmBm = TBmBmEQL;
                        TBmDif = TBmDiffEQL;
                    }
                }

                // Report variables
                SurfWinGlTsolBmBm(SurfNum) = TBmBm;
                SurfWinGlTsolBmDif(SurfNum) = TBmDif;

                // Diffuse-diffuse transmittance for bare exterior window
                if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    TDifBare = TransTDD(state, PipeNum, CosInc, SolarAniso);
                } else {
                    if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                        // Complex Fenestration: use hemispherical ave of directional-hemispherical transmittance
                        // Note: this is not quite the same as the effective transmittance for total of sky and ground radiation
                        TDifBare = SurfaceWindow(SurfNum).ComplexFen.State(SurfaceWindow(SurfNum).ComplexFen.CurrentState).WinDiffTrans;
                    } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {
                        // get ASHWAT fenestration model diffuse-diffuse properties includes shade if present
                        TDifBare = state.dataConstruction->Construct(ConstrNum).TransDiff;
                    } else { // Regular window
                        TDifBare = state.dataConstruction->Construct(ConstrNum).TransDiff;
                    }
                }
                SurfWinGlTsolDifDif(SurfNum) = TDifBare;

                if (SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                    if (ShadeFlag > 0 && ShadeFlag < 10) {

                        // Shade or screen or blind on, or switchable glazing
                        // (note in the following that diffusing glass is not allowed in a window with
                        // shade, blind or switchable glazing)
                        if (ConstrNumSh != 0) {
                            if (ShadeFlag != IntBlindOn && ShadeFlag != ExtBlindOn && ShadeFlag != BGBlindOn && ShadeFlag != ExtScreenOn) {

                                // Shade on or switchable glazing

                                if (SunLitFract > 0.0) TBmAllShBlSc = POLYF(CosInc, state.dataConstruction->Construct(ConstrNumSh).TransSolBeamCoef);

                            } else {

                                // Blind or Screen on
                                int NGlass = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                                SurfWinBlGlSysTsolDifDif(SurfNum) = DiffTrans;
                                SurfWinScGlSysTsolDifDif(SurfNum) = DiffTrans;
                                if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                    SurfWinBlTsolDifDif(SurfNum) = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                                } else if (ShadeFlag == ExtScreenOn) {
                                    SurfWinScTsolDifDif(SurfNum) = SurfaceScreens(ScNum).DifDifTrans;
                                }

                                if (SunLitFract > 0.0) {
                                    Real64 TScBmDif; // Beam-diffuse solar transmittance of screen
                                    Real64 TBlBmDif; // Beam-diffuse solar transmittance of blind
                                    Real64 TBlDifDif; // Diffuse-diffuse solar transmittance of blind
                                    Real64 RhoBlBmDifBk; // Beam-diffuse back reflectance of blind
                                    Real64 RhoBlDifDifFr; // Diffuse-diffuse front refectance of blind
                                    Real64 RhoBlDifDifBk; // Diffuse-diffuse back refectance of blind
                                    Real64 RScDifDifBk; // Diffuse-diffuse back refectance of screen
                                    Real64 RGlBmFr; // Beam front reflectance of glass
                                    Real64 RGlDifFr; // Diffuse front reflectance of glass
                                    Real64 RGlDifBk; // Diffuse back reflectance of glass
                                    Real64 TScBmBm;
                                    Real64 TBlBmBm;
                                    if (ShadeFlag == ExtScreenOn) {
                                        // beam transmittance (written in subroutine CalcScreenTransmittance each time step)
                                        TScBmBm = SurfaceScreens(ScNum).BmBmTrans;
                                        SurfWinScTsolBmBm(SurfNum) = TScBmBm;
                                    } else {
                                        TBlBmBm = BlindBeamBeamTrans(
                                                ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                                        SurfWinBlTsolBmBm(SurfNum) = TBlBmBm;
                                    }
                                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn) {
                                        // Interior or exterior blind
                                        TBmBmBl = TBmBm * TBlBmBm;
                                    } else if (ShadeFlag == ExtScreenOn) {
                                        // Exterior screen
                                        TBmBmSc = TBmBm * TScBmBm;
                                    } else {
                                        // Between-glass blind
                                        Real64 t1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 1));
                                        Real64 t2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 2));
                                        Real64 tfshBB = BlindBeamBeamTrans(ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                                        if (NGlass == 2) {
                                            TBmBmBl = t1 * tfshBB * t2;
                                        } else { // NGlass = 3
                                            Real64 t3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 3));
                                            TBmBmBl = t1 * t2 * tfshBB * t3;
                                        }
                                    }
                                    if (ShadeFlag == ExtScreenOn) {
                                        //           Report variable for Beam-to-Beam transmittance
                                        SurfWinScGlSysTsolBmBm(SurfNum) = TBmBmSc;
                                    } else {
                                        SurfWinBlGlSysTsolBmBm(SurfNum) = TBmBmBl;
                                    }

                                    if (ShadeFlag == ExtScreenOn) {
                                        TScBmDif = SurfaceScreens(ScNum).BmDifTrans;
                                        //           Report variable for Beam-to-Diffuse transmittance (scattered transmittance)
                                        SurfWinScTsolBmDif(SurfNum) = TScBmDif;
                                    } else {
                                        TBlBmDif = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);
                                        SurfWinBlTsolBmDif(SurfNum) = TBlBmDif;
                                        // CR6913     SurfaceWindow(SurfNum)%BlTsolDifDif =
                                        // InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
                                    }

                                    // added TH 12/9/2009
                                    TBmBmShBlSc = 0.0;
                                    TBmDifShBlSc = 0.0;

                                    if (ShadeFlag == IntBlindOn) {

                                        // Interior blind on: beam-beam and diffuse transmittance of exterior beam

                                        TBlDifDif = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                                        Real64 RhoBlBmDifFr = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl); // Beam-diffuse front reflectance of blind
                                        RGlDifBk = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                                        RhoBlDifDifFr = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                                        TBmAllShBlSc =
                                                TBmBm * (TBlBmBm + TBlBmDif + TBlDifDif * RhoBlBmDifFr * RGlDifBk / (1 - RhoBlDifDifFr * RGlDifBk));

                                        // added TH 12/9/2009
                                        TBmBmShBlSc = TBmBmBl; // TBmBm * TBlBmBm
                                        TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;
                                        if (TBmDifShBlSc < 0.0) TBmDifShBlSc = 0.0;

                                    } else if (ShadeFlag == ExtBlindOn) {

                                        // Exterior blind on: beam-beam and diffuse transmittance of exterior beam

                                        RhoBlBmDifBk = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffRefl);
                                        RhoBlDifDifBk = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                                        RGlBmFr = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                                        RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                        TBmAllShBlSc = TBlBmBm * (TBmBm + TDifBare * RGlBmFr * RhoBlBmDifBk / (1 - RGlDifFr * RhoBlDifDifBk)) +
                                                       TBlBmDif * TDifBare / (1 - RGlDifFr * RhoBlDifDifBk);

                                        // added TH 12/9/2009
                                        TBmBmShBlSc = TBmBmBl; // TBmBm * TBlBmBm
                                        TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;

                                    } else if (ShadeFlag == ExtScreenOn) {

                                        // Exterior screen on: beam-beam and diffuse transmittance of exterior beam

                                        Real64 RScBack = SurfaceScreens(ScNum).ReflectSolBeamFront;
                                        RScDifDifBk = SurfaceScreens(ScNum).DifReflect;
                                        RGlBmFr = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).ReflSolBeamFrontCoef);
                                        RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                        TBmAllShBlSc = TScBmBm * (TBmBm + RGlBmFr * RScBack * TDifBare / (1 - RGlDifFr * RScDifDifBk)) +
                                                       TScBmDif * TDifBare / (1 - RGlDifFr * RScDifDifBk);

                                        // added TH 12/9/2009
                                        TBmBmShBlSc = TBmBmSc;
                                        TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;

                                    } else {
                                        // Between-glass blind on: beam-beam and diffuse transmittance of exterior beam
                                        Real64 t1 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 1));
                                        Real64 t2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 2));
                                        Real64 td2 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(2);
                                        Real64 rbd1 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(1);

                                        Real64 tfshBB = BlindBeamBeamTrans(ProfAng, SlatAng, Blind(BlNum).SlatWidth, Blind(BlNum).SlatSeparation, Blind(BlNum).SlatThickness);
                                        Real64 rbshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolBackBeamDiffRefl);
                                        Real64 rfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffRefl);
                                        Real64 rbshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolBackDiffDiffRefl);
                                        Real64 tfshBd = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffTrans);


                                        if (NGlass == 2) {
                                            Real64 rf2 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 2));
                                            Real64 rfshB = InterpProfSlatAng(ProfAng, SlatAng, VarSlats, Blind(BlNum).SolFrontBeamDiffRefl);



                                            Real64 rfd2 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(2);
                                            TBmAllShBlSc = t1 * tfshBB * t2 + t1 * (tfshBB * rf2 * rbshB + tfshBd * (1.0 + rfd2 * rbshd) + rfshB * rbd1 * rfshd) * td2;
                                        } else { // NGlass = 3
                                            Real64 t1t2 = t1 * t2;
                                            Real64 t3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).tBareSolCoef({1, 6}, 3));
                                            Real64 td3 = state.dataConstruction->Construct(ConstrNum).tBareSolDiff(3);
                                            Real64 rf3 = POLYF(CosInc, state.dataConstruction->Construct(ConstrNum).rfBareSolCoef({1, 6}, 3));
                                            Real64 rbd2 = state.dataConstruction->Construct(ConstrNum).rbBareSolDiff(2);
                                            Real64 rfd3 = state.dataConstruction->Construct(ConstrNum).rfBareSolDiff(3);
                                            Real64 tfshd = InterpSlatAng(SlatAng, VarSlats, Blind(BlNum).SolFrontDiffDiffTrans);
                                            TBmAllShBlSc = t1t2 * tfshBB * t3 + t1t2 * (tfshBB * rf3 * rbshB + tfshBd * (1.0 + rfd3 * rbshd) + rbshB * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) * td3;
                                        }

                                        // added TH 12/9/2009
                                        TBmBmShBlSc = TBmBmBl;
                                        TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc;
                                    }
                                }
                            }
                        }
                    } // End of check if ShadeFlag > 0 and ShadeFlag < 10
                }

                if (ShadeFlag == SwitchableGlazing) {

                    // Switchable glazing

                    Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum);
                    if (!SurfWinSolarDiffusing(SurfNum)) {
                        TBmBm = InterpSw(SwitchFac, TBmBm, TBmAllShBlSc);
                    } else {
                        TBmDif = InterpSw(SwitchFac, TBmDif, TBmAllShBlSc);
                    }
                }

                // The following WinTransBmSolar and WinTransDifSolar will be combined later to give
                // WinTransSolar for reporting
                if (SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                    WinTransDifSolar(SurfNum) = DiffTrans * Surface(SurfNum).Area;
                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                        if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                            WinTransDifSolarGnd(SurfNum) = DiffTransGnd * Surface(SurfNum).Area;
                            WinTransDifSolarSky(SurfNum) = DiffTransSky * Surface(SurfNum).Area;
                        }
                    }
                } else {
                    // In equivalent layer window model system diffuse transmittance is based on unit
                    // diffuse radiation flux, and hence doesn't distinguish between sky and
                    // ground reflected diffuse radiations
                    WinTransDifSolar(SurfNum) = DiffTrans * Surface(SurfNum).Area;
                    WinTransDifSolarGnd(SurfNum) = DiffTrans * Surface(SurfNum).Area;
                    WinTransDifSolarSky(SurfNum) = DiffTrans * Surface(SurfNum).Area;
                }
                if (ShadeFlag < 1 || ShadeFlag == SwitchableGlazing || ShadeFlag >= 10) { // Unshaded or switchable glazing
                    // Note: with previous defs of TBmBm & TBmDif, these come out right for Complex Fenestration
                    // WinTransBmSolar uses the directional-hemispherical transmittance
                    WinTransBmSolar(SurfNum) = (TBmBm + TBmDif) * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;

                    // added TH 12/9/2009
                    WinTransBmBmSolar = TBmBm * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;   // m2
                    WinTransBmDifSolar = TBmDif * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult; // m2

                } else {
                    WinTransBmSolar(SurfNum) = TBmAllShBlSc * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;

                    // added TH 12/9/2009
                    WinTransBmBmSolar = TBmBmShBlSc * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;
                    WinTransBmDifSolar = TBmDifShBlSc * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;
                }

                // Add diffuse transmitted by window from beam reflected from outside reveal

                if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) { // Complex Fenestration
                    int FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum);
                    if (FenSolAbsPtr == 0) {
                        WinTransBmSolar(SurfNum) = (TBmBm + TBmDif) * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;

                        // added TH 12/9/2009
                        WinTransBmBmSolar = TBmBm * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult;   // m2
                        WinTransBmDifSolar = TBmDif * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult; // m2
                        WinTransBmSolar(SurfNum) += SurfWinOutsRevealDiffOntoGlazing(SurfNum) * NomDiffTrans * Surface(SurfNum).Area;

                        WinTransBmDifSolar += SurfWinOutsRevealDiffOntoGlazing(SurfNum) * NomDiffTrans * Surface(SurfNum).Area;
                    } else {
                        WinTransBmSolar(SurfNum) = 0.0;
                        WinTransBmDifSolar = 0.0;
                    }
                } else { // Regular window
                    // this is also valid for equivalent layer window
                    WinTransBmSolar(SurfNum) += SurfWinOutsRevealDiffOntoGlazing(SurfNum) * DiffTrans * Surface(SurfNum).Area;

                    // added TH 12/9/2009
                    WinTransBmDifSolar += SurfWinOutsRevealDiffOntoGlazing(SurfNum) * DiffTrans * Surface(SurfNum).Area;
                }

                // Increment factor for total exterior beam solar entering zone through window as beam or diffuse

                if (SunLitFract > 0.0 && Surface(SurfNum).Class != SurfaceClass::TDD_Dome) {
                    Real64 TBmAll; // Window beam-to-(beam+diffuse) transmittance
                    if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel &&
                        (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn ||
                         ShadeFlag == BGShadeOn || ShadeFlag == BGBlindOn || ShadeFlag == ExtScreenOn)) {
                        TBmAll = TBmAllShBlSc;
                    } else {
                        TBmAll = TBmBm + TBmDif;
                    }

                    int FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum);

                    // Window is schedule surface gained. Do not make addition to what enters into zone since that information is not
                    // available
                    if (FenSolAbsPtr == 0) {
                        BTOTZone += TBmAll * SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult; // [m2]
                    }
                }

                // Correct for effect of (1) beam absorbed by inside reveal, (2) diffuse entering zone from beam
                // reflected by inside reveal and (3) diffuse transmitted by window from beam reflected from
                // outside reveal.
                if (CosInc > 0.0) {
                    // old code
                    // BTOTZone = BTOTZone + (SurfaceWindow(SurfNum)%InsRevealDiffIntoZone &
                    //                       - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal &
                    //                       + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans) * Surface(SurfNum)%Area

                    // CR 7596. TH 5/27/2009
                    // The BTOTZone is the solar into zone assuming no inside or outside reveals
                    // The inside reveals receive solar (reflected part + absorbed part) from the window, this amount should be
                    // deducted from the BTOTZone, then adds the InsRevealDiffIntoZone
                    if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) { // Complex Fenestration
                        int SurfSolIncPtr = SurfaceScheduledSolarInc(SurfNum, ConstrNum);

                        // Do not add total into zone from scheduled surface gains.  That will be added later
                        if (SurfSolIncPtr == 0) {
                            BTOTZone = BTOTZone - SurfWinBmSolRefldInsReveal(SurfNum) - SurfWinBmSolAbsdInsReveal(SurfNum) +
                                       SurfWinInsRevealDiffIntoZone(SurfNum) +
                                       SurfWinOutsRevealDiffOntoGlazing(SurfNum) * NomDiffTrans * Surface(SurfNum).Area;
                        }
                    } else { // Regular window
                        BTOTZone = BTOTZone - SurfWinBmSolRefldInsReveal(SurfNum) - SurfWinBmSolAbsdInsReveal(SurfNum) +
                                   SurfWinInsRevealDiffIntoZone(SurfNum) +
                                   SurfWinOutsRevealDiffOntoGlazing(SurfNum) * DiffTrans * Surface(SurfNum).Area;
                    }
                    // Add beam solar absorbed by outside reveal to outside of window's base surface.
                    // Add beam solar absorbed by inside reveal to inside of window's base surface.
                    // This ignores 2-D heat transfer effects.
                    int BaseSurfNum = Surface(SurfNum).BaseSurf;
                    SurfOpaqAI(BaseSurfNum) += SurfWinBmSolAbsdInsReveal(SurfNum) / Surface(BaseSurfNum).Area;
                    SurfOpaqAO(BaseSurfNum) += SurfWinBmSolAbsdOutsReveal(SurfNum) / Surface(BaseSurfNum).Area;
                }

                if (SunLitFract > 0.0) {

                    //---------------------------------------------------------------------------------
                    // INTERIOR BEAM FROM EXTERIOR WINDOW THAT IS ABSORBED/TRANSMITTED BY BACK SURFACES
                    //---------------------------------------------------------------------------------

                    // If shade is in place or there is a diffusing glass layer there is no interior beam
                    // from this exterior window since the beam-beam transmittance of shades and diffusing glass
                    // is assumed to be zero. The beam-beam transmittance of tubular daylighting devices is also
                    // assumed to be zero.

                    if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel)
                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || SurfWinSolarDiffusing(SurfNum) ||
                            SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser || Surface(SurfNum).Class == SurfaceClass::TDD_Dome)
                            continue;

                    // Find interior beam radiation that is:
                    // (1) absorbed by opaque back surfaces;
                    // (2) absorbed by glass layers of back surfaces that are interior or exterior windows;
                    // (3) absorbed by interior, exterior or between-glass shades or blinds of back surfaces
                    //       that are exterior windows; and
                    // (4) transmitted through back surfaces that are interior or exterior windows.

                    // Beam-beam transmittance of exterior window
                    Real64 TBm; // Window beam-beam transmittance
                    Real64 TBmDenom; // TBmDenominator
                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                        TBm = TBmBmBl; // Interior, exterior or between-glass blind on
                    } else if (ShadeFlag == ExtScreenOn) {
                        TBm = TBmBmSc; // Exterior screen on
                    } else {
                        TBm = TBmBm; // Bare glass or switchable glazing

                        // Correction for beam absorbed by inside reveal
                        TBmDenom = (SunLitFract * CosInc * Surface(SurfNum).Area * InOutProjSLFracMult);
                        if (TBmDenom != 0.0) { // when =0.0, no correction
                            TBm -= SurfWinBmSolAbsdInsReveal(SurfNum) / TBmDenom;
                        }

                        TBm = max(0.0, TBm);
                    }

                    if (TBm == 0.0) continue;

                    if (InShelfSurf > 0) { // Inside daylighting shelf
                        // Inside daylighting shelves assume that no beam will pass the end of the shelf.
                        // Since all beam is absorbed on the shelf, this might cause them to get unrealistically hot at times.
                        // BTOTWinZone - Transmitted beam solar factor for a window [m2]
                        Real64 BTOTWinZone = TBm * SunLitFract * Surface(SurfNum).Area * CosInc * InOutProjSLFracMult;
                        // Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
                        SurfOpaqAI(InShelfSurf) += BTOTWinZone / (0.5 * Surface(InShelfSurf).Area); //[-]
                        BABSZone += BTOTWinZone;                                                //[m2]

                        continue;
                    }

                    if (SolarDistribution == FullInteriorExterior) { // Full interior solar distribution

                        if (SurfWinWindowModelType(SurfNum) != WindowBSDFModel && SurfWinWindowModelType(SurfNum) != WindowEQLModel) {
                            // Loop over back surfaces irradiated by beam from this exterior window

                            for (int IBack = 1; IBack <= MaxBkSurf; ++IBack) {

                                int BackSurfNum = BackSurfaces(TimeStep, HourOfDay, IBack, SurfNum);

                                if (BackSurfNum == 0) break; // No more irradiated back surfaces for this exterior window
                                int ConstrNumBack = Surface(BackSurfNum).Construction;
                                int NBackGlass = state.dataConstruction->Construct(ConstrNumBack).TotGlassLayers;
                                // Irradiated (overlap) area for this back surface, projected onto window plane
                                // (includes effect of shadowing on exterior window)
                                Real64 AOverlap = OverlapAreas(TimeStep, HourOfDay, IBack, SurfNum);
                                // Back surface area irradiated by beam solar from an exterior window, projected onto window plane
                                Real64 BOverlap = TBm * AOverlap * CosInc; //[m2]
                                // AOverlap multiplied by exterior window beam transmittance and cosine of incidence angle
                                if (state.dataConstruction->Construct(ConstrNumBack).TransDiff <= 0.0) {

                                    // Back surface is opaque interior or exterior wall
                                    // Interior solar absorptance of opaque surface
                                    Real64 AbsIntSurf = state.dataConstruction->Construct(ConstrNumBack).InsideAbsorpSolar;

                                    // Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                                    // Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                                    // HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                                    Real64 HMovInsul = 0.0; // Conductance of movable wall insulation
                                    Real64 AbsInt = 0.0;
                                    if (Surface(BackSurfNum).MaterialMovInsulInt > 0) {
                                        Real64 MovInsulSchedVal = GetCurrentScheduleValue(Surface(BackSurfNum).SchedMovInsulInt);
                                        if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
                                            HMovInsul = 0.0;
                                        } else { // Movable insulation present
                                            HMovInsul = 1.0 / (MovInsulSchedVal * dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).Resistance);
                                            AbsInt = dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).AbsorpSolar;
                                        }
                                    }
                                    if (HMovInsul > 0.0) AbsIntSurf = AbsInt; // Movable inside insulation present

                                    SurfOpaqAI(BackSurfNum) += BOverlap * AbsIntSurf / Surface(BackSurfNum).Area; //[-]
                                    BABSZone += BOverlap * AbsIntSurf;                                        //[m2]

                                } else {

                                    // Back surface is an interior or exterior window

                                    // Note that exterior back windows can have a shading device but interior back windows
                                    // are assumed to be bare, i.e., they have no shading device and are non-switchable.
                                    // The layer order for interior windows is "outside" to "inside," where "outside" refers to
                                    // the adjacent zone and "inside" refers to the current zone.

                                    int ShadeFlagBack = SurfWinShadingFlag(BackSurfNum);
                                    Real64 SlatAngBack = SurfWinSlatAngThisTS(BackSurfNum);
                                    bool VarSlatsBack = SurfWinMovableSlats(BackSurfNum);
                                    Real64 CosIncBack = std::abs(CosIncAng(TimeStep, HourOfDay, BackSurfNum));
                                    if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                                        // Transmitting window is complex fen, change the incident angle to one for ray joining
                                        // transmitting and back window centers
                                        CosIncBack = std::abs(ComplexWind(SurfNum).sdotN(IBack));
                                    }
                                    int ConstrNumBackSh = Surface(BackSurfNum).activeShadedConstruction;
                                    if (SurfWinStormWinFlag(BackSurfNum) == 1) {
                                        ConstrNum = Surface(BackSurfNum).StormWinConstruction;
                                        ConstrNumSh = Surface(BackSurfNum).activeStormWinShadedConstruction;
                                    }
                                    AbsBeamWin.dimension(DataHeatBalance::MaxSolidWinLayers, 0.0);
                                    Real64 TransBeamWin = 0.0; // Beam solar transmittance of a window
                                    Real64 AbsBeamTotWin = 0.0; // Sum of window glass layer beam solar absorptances

                                    // Interior beam absorptance of glass layers and beam transmittance of back exterior  &
                                    // or interior window WITHOUT SHADING this timestep

                                    if (state.dataConstruction->Construct(ConstrNumBack).TypeIsAirBoundaryInteriorWindow) {
                                        TransBeamWin = 1.0;
                                        AbsBeamWinEQL = 0.0;
                                        AbsBeamTotWin = 0.0;
                                    } else if (ShadeFlagBack <= 0) {
                                        for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                            AbsBeamWin(Lay) = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).AbsBeamBackCoef({1, 6}, Lay));
                                        }
                                        TransBeamWin = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).TransSolBeamCoef);
                                    }

                                    // Interior beam absorptance of glass layers and beam transmittance
                                    // of back exterior window with SHADE

                                    if (ShadeFlagBack == IntShadeOn || ShadeFlagBack == ExtShadeOn || ShadeFlagBack == BGShadeOn) {
                                        for (int Lay = 1; Lay <= state.dataConstruction->Construct(ConstrNumBackSh).TotGlassLayers; ++Lay) {
                                            AbsBeamWin(Lay) = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBackSh).AbsBeamBackCoef({1, 6}, Lay));
                                        }
                                        TransBeamWin = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBackSh).TransSolBeamCoef);
                                    }

                                    // Interior beam absorbed by INTERIOR SHADE of back exterior window

                                    if (ShadeFlagBack == IntShadeOn) {
                                        IntBeamAbsByShadFac(BackSurfNum) = BOverlap * state.dataConstruction->Construct(ConstrNumBackSh).AbsDiffBackShade /
                                                                           (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                        BABSZone += BOverlap * state.dataConstruction->Construct(ConstrNumBackSh).AbsDiffBackShade;
                                    }

                                    // Interior beam absorbed by EXTERIOR SHADE of back exterior window

                                    if (ShadeFlagBack == ExtShadeOn) {
                                        Real64 RGlFront = state.dataConstruction->Construct(ConstrNumBack).ReflectSolDiffFront;
                                        Real64 AbsSh = dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(1)).AbsorpSolar;
                                        Real64 RhoSh = 1.0 - AbsSh - dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(1)).Trans;
                                        Real64 AShBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).TransSolBeamCoef) * AbsSh / (1.0 - RGlFront * RhoSh);
                                        BABSZone += BOverlap * AShBack;
                                        IntBeamAbsByShadFac(BackSurfNum) =
                                                BOverlap * AShBack / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                    }

                                    // Interior beam absorbed by BETWEEN-GLASS SHADE of back exterior window

                                    if (ShadeFlagBack == BGShadeOn) {
                                        Real64 rbd1k = state.dataConstruction->Construct(ConstrNumBack).rbBareSolDiff(1);
                                        Real64 rfd2k = state.dataConstruction->Construct(ConstrNumBack).rfBareSolDiff(2);
                                        Real64 AShBack; // System shade absorptance for interior beam solar
                                        if (NBackGlass == 2) {
                                            Real64 t2k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).tBareSolCoef({1, 6}, 2));
                                            Real64 TrSh = dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(3)).Trans; // Shade material solar transmittance
                                            Real64 RhoSh = dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(3)).ReflectShade; // Shade material solar absorptance
                                            Real64 AbsSh = min(1.0, max(0.0, 1 - TrSh - RhoSh)); // Shade material solar absorptance
                                            AShBack = t2k * (1 + RhoSh * rfd2k + TrSh * rbd1k) * AbsSh;
                                        } else { // NBackGlass = 3
                                            Real64 t3k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).tBareSolCoef({1, 6}, 3));
                                            Real64 td2k = state.dataConstruction->Construct(ConstrNumBack).tBareSolDiff(2);
                                            Real64 rbd2k = state.dataConstruction->Construct(ConstrNumBack).rbBareSolDiff(2);
                                            Real64 rfd3k = state.dataConstruction->Construct(ConstrNumBack).rfBareSolDiff(3);
                                            Real64 TrSh = dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(5)).Trans;
                                            Real64 RhoSh = dataMaterial.Material(state.dataConstruction->Construct(ConstrNumBackSh).LayerPoint(5)).ReflectShade;
                                            Real64 AbsSh = min(1.0, max(0.0, 1 - TrSh - RhoSh));
                                            AShBack = t3k * (1 + RhoSh * rfd3k + TrSh * (rbd2k + td2k * rbd1k * td2k)) * AbsSh;
                                        }
                                        IntBeamAbsByShadFac(BackSurfNum) = BOverlap * AShBack / Surface(BackSurfNum).Area;
                                        BABSZone += BOverlap * AShBack;
                                    }

                                    // Interior beam absorptance of glass layers and beam absorbed in blind
                                    // of back exterior window with BLIND
                                    if (ShadeFlagBack == IntBlindOn || ShadeFlagBack == ExtBlindOn || ShadeFlagBack == BGBlindOn) {
                                        int BlNumBack = SurfWinBlindNumber(BackSurfNum); // Back surface blind number
                                        Real64 ProfAngBack; // Back window solar profile angle (radians)
                                        ProfileAngle(BackSurfNum, SOLCOS, Blind(BlNumBack).SlatOrientation, ProfAngBack);
                                        Real64 TGlBmBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).TransSolBeamCoef);
                                        Real64 TBlBmBmBack = BlindBeamBeamTrans(ProfAngBack,
                                                                         DataGlobalConstants::Pi() - SlatAngBack,
                                                                         Blind(BlNumBack).SlatWidth,
                                                                         Blind(BlNumBack).SlatSeparation,
                                                                         Blind(BlNumBack).SlatThickness); // Blind solar back beam-beam transmittance
                                        Real64 TBlBmDiffBack = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack,
                                                Blind(BlNumBack).SolBackBeamDiffTrans); // Blind solar back beam-diffuse transmittance

                                        if (ShadeFlagBack == IntBlindOn) {

                                            // Interior beam absorptance of GLASS LAYERS of exterior back window with INTERIOR BLIND
                                            // Blind solar front beam reflectance
                                            Real64 RhoBlFront =
                                                    InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontBeamDiffRefl);
                                            // Blind solar front diffuse reflectance
                                            Real64 RhoBlDiffFront = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontDiffDiffRefl);
                                            // Glazing system solar back beam-beam reflectance
                                            Real64 RGlBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).ReflSolBeamBackCoef({1, 6}));
                                            // Glazing system back diffuse solar reflectance
                                            Real64 RGlDiffBack = state.dataConstruction->Construct(ConstrNumBack).ReflectSolDiffBack;
                                            for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                                // Factor for back beam radiation absorbed in window glass layer
                                                Real64 AbWinBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).AbsBeamBackCoef({1, 6}, Lay));
                                                // Glass layer back diffuse solar absorptance
                                                Real64 AGlDiffBack = state.dataConstruction->Construct(ConstrNumBack).AbsDiffBack(Lay);
                                                AbsBeamWin(Lay) = TBlBmBmBack * AbWinBack + ((TBlBmBmBack * RGlBack * RhoBlFront + TBlBmDiffBack) *
                                                                                             AGlDiffBack / (1.0 - RGlDiffBack * RhoBlDiffFront));
                                            }

                                            // Interior beam transmitted by exterior back window with INTERIOR BLIND

                                            Real64 TGlDif = state.dataConstruction->Construct(ConstrNumBack).TransDiff; // Bare diffuse transmittance of back window
                                            TransBeamWin =
                                                    TBlBmBmBack * (TGlBmBack + TGlDif * RGlBack * RhoBlFront / (1.0 - RGlDiffBack * RhoBlDiffFront)) +
                                                    TBlBmDiffBack * TGlDif / (1.0 - RGlDiffBack * RhoBlDiffFront);

                                            // Interior beam absorbed by BLIND on exterior back window with INTERIOR BLIND
                                            // Blind solar front beam absorptance
                                            Real64 AbsBlFront = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontBeamAbs);
                                            // Blind solar back beam absorptance
                                            Real64 AbsBlBack = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamAbs);
                                            // Blind solar front diffuse absorptance
                                            Real64 AbsBlDiffFront = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontDiffAbs);
                                            // Blind solar back absorptance for interior solar
                                            Real64 ABlBack = AbsBlBack + TBlBmBmBack * RGlBack * AbsBlFront +
                                                      (AbsBlDiffFront * RGlDiffBack / (1 - RhoBlDiffFront * RGlDiffBack)) *
                                                      (RGlBack * TBlBmBmBack * RhoBlFront + TBlBmDiffBack);
                                            IntBeamAbsByShadFac(BackSurfNum) =
                                                    BOverlap * ABlBack / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                            BABSZone += BOverlap * ABlBack;
                                        }

                                        if (ShadeFlagBack == ExtBlindOn) {

                                            // Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR BLIND
                                            // Glazing system front diffuse solar reflectance
                                            Real64 RGlDiffFront = state.dataConstruction->Construct(ConstrNumBack).ReflectSolDiffFront;
                                            // Blind solar back beam-diffuse reflectance
                                            Real64 RhoBlBack =
                                                    InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamDiffRefl);
                                            for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                                Real64 AbWinBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).AbsBeamBackCoef({1, 6}, Lay));
                                                Real64 AGlDiffFront = state.dataConstruction->Construct(ConstrNumBack).AbsDiff(Lay);
                                                AbsBeamWin(Lay) =
                                                        AbWinBack + (TGlBmBack * AGlDiffFront * RhoBlBack / (1.0 - RhoBlBack * RGlDiffFront));
                                            }

                                            // Interior beam transmitted by exterior back window with EXTERIOR BLIND

                                            Real64 TBlDifDif = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffDiffTrans);
                                            Real64 RhoBlBmDifBk =
                                                    InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamDiffRefl);
                                            Real64 RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                            Real64 RhoBlDifDifBk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffDiffRefl);
                                            TransBeamWin = TGlBmBack * (TBlBmBmBack + TBlBmDiffBack +
                                                                        TBlDifDif * RhoBlBmDifBk * RGlDifFr / (1.0 - RhoBlDifDifBk * RGlDifFr));

                                            // Interior beam absorbed by EXTERIOR BLIND on exterior back window

                                            Real64 AbsBlBack = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamAbs);
                                            Real64 AbsBlDiffBack = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffAbs);
                                            Real64 RhoBlDiffBack = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffDiffRefl);
                                            Real64 ABlBack = TGlBmBack *
                                                      (AbsBlBack + RhoBlBack * RGlDiffFront * AbsBlDiffBack / (1 - RhoBlDiffBack * RGlDiffFront));
                                            BABSZone += BOverlap * ABlBack;
                                            IntBeamAbsByShadFac(BackSurfNum) =
                                                    BOverlap * ABlBack / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                        } // End of check if exterior blind on back window

                                        if (ShadeFlagBack == BGBlindOn) {

                                            Real64 t1k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).tBareSolCoef({1, 6}, 1));
                                            Real64 t2k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).tBareSolCoef({1, 6}, 2));
                                            Real64 af2k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).afBareSolCoef({1, 6}, 2));
                                            Real64 ab1k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).abBareSolCoef({1, 6}, 1));
                                            Real64 ab2k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).abBareSolCoef({1, 6}, 2));
                                            Real64 rb1k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).rbBareSolCoef({1, 6}, 1));
                                            Real64 rb2k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).rbBareSolCoef({1, 6}, 2));
                                            Real64 td1k = state.dataConstruction->Construct(ConstrNumBack).tBareSolDiff(1);
                                            Real64 td2k = state.dataConstruction->Construct(ConstrNumBack).tBareSolDiff(2);
                                            Real64 afd2k = state.dataConstruction->Construct(ConstrNumBack).afBareSolDiff(2);
                                            Real64 abd1k = state.dataConstruction->Construct(ConstrNumBack).abBareSolDiff(1);
                                            Real64 abd2k = state.dataConstruction->Construct(ConstrNumBack).abBareSolDiff(2);
                                            Real64 rfd2k = state.dataConstruction->Construct(ConstrNumBack).rfBareSolDiff(2);
                                            Real64 rbd1k = state.dataConstruction->Construct(ConstrNumBack).rbBareSolDiff(1);
                                            Real64 rbd2k = state.dataConstruction->Construct(ConstrNumBack).rbBareSolDiff(2);
                                            Real64 tfshBBk = BlindBeamBeamTrans(ProfAngBack,
                                                                         SlatAngBack,
                                                                         Blind(BlNumBack).SlatWidth,
                                                                         Blind(BlNumBack).SlatSeparation,
                                                                         Blind(BlNumBack).SlatThickness);
                                            Real64 tfshBdk = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontBeamDiffTrans);
                                            Real64 tfshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontDiffDiffTrans);
                                            Real64 tbshBBk = BlindBeamBeamTrans(ProfAngBack,
                                                                         DataGlobalConstants::Pi() - SlatAngBack,
                                                                         Blind(BlNumBack).SlatWidth,
                                                                         Blind(BlNumBack).SlatSeparation,
                                                                         Blind(BlNumBack).SlatThickness);
                                            Real64 tbshBdk =
                                                    InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamDiffTrans);
                                            Real64 tbshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffDiffTrans);
                                            Real64 rfshBk = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontBeamDiffRefl);
                                            Real64 rbshBk = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamDiffRefl);
                                            Real64 rfshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontDiffDiffRefl);
                                            Real64 rbshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffDiffRefl);
                                            Real64 afshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontDiffAbs);
                                            Real64 abshdk = InterpSlatAng(SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackDiffAbs);
                                            Real64 afshBk = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolFrontBeamAbs);
                                            Real64 abshBk = InterpProfSlatAng(ProfAngBack, SlatAngBack, VarSlatsBack, Blind(BlNumBack).SolBackBeamAbs);
                                            Real64 ABlBack;
                                            if (NBackGlass == 2) {
                                                // Interior beam absorptance of GLASS LAYERS of exterior back window with BETWEEN-GLASS BLIND
                                                AbsBeamWin(2) =
                                                        ab2k + t2k * tbshBBk * rb1k * tfshBBk * af2k +
                                                        t2k * (tbshBBk * rb1k * tfshBdk + tbshBdk * rbd1k * tfshdk + rbshBk * (1.0 + rfd2k * rbshdk)) *
                                                        afd2k;
                                                AbsBeamWin(1) =
                                                        t2k * tbshBBk * ab1k + t2k * (rbshBk * rfd2k * tbshdk + tbshBdk * (1.0 + rbd1k * rfshdk)) * abd1k;
                                                // Interior beam transmitted by exterior back window with BETWEEN-GLASS BLIND
                                                TransBeamWin =
                                                        t2k * tbshBBk * t1k +
                                                        t2k * (tbshBBk * rb1k * rfshBk + rbshBk * rfd2k * tbshdk + tbshBdk * (1.0 + rbd1k * rfshdk)) *
                                                        td1k;
                                                // Interior beam absorbed by BLIND on exterior back window with BETWEEN-GLASS BLIND
                                                ABlBack = t2k * (abshBk + tbshBBk * rb1k * afshBk + rbshBk * rfd2k * abshdk + tbshBdk * rbd1k * afshdk);
                                            } else { // NBackGlass = 3
                                                Real64 t3k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).tBareSolCoef({1, 6}, 3));
                                                Real64 af3k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).afBareSolCoef({1, 6}, 3));
                                                Real64 ab3k = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).abBareSolCoef({1, 6}, 3));
                                                Real64 afd3k = state.dataConstruction->Construct(ConstrNumBack).afBareSolDiff(3);
                                                Real64 rfd3k = state.dataConstruction->Construct(ConstrNumBack).rfBareSolDiff(3);
                                                AbsBeamWin(3) = ab3k + t3k * tbshBBk * (rb2k + t2k * rb1k * t2k) * tfshBBk * af3k +
                                                                t3k *
                                                                (tbshBdk * rbd2k * tfshdk + tbshBdk * td2k * rbd1k * td2k * tfshdk +
                                                                 rbshBk * (1.0 + rfd3k * rbshdk)) *
                                                                afd3k;
                                                AbsBeamWin(2) =
                                                        t3k * tbshBBk * (ab2k + t2k * rb1k * (af2k + t2k * rfshBk * abd2k)) +
                                                        t3k * (tbshBdk + tbshBdk * (rbd2k + td2k * rbd1k * td2k) * rfshdk + rbshBk * rfd3k * tbshdk) *
                                                        abd2k +
                                                        t3k * tbshBdk * td2k * rbd1k * afd2k;
                                                AbsBeamWin(1) =
                                                        t3k * tbshBBk * (t2k * ab1k + (rb2k + t2k * rb1k * t2k) * rfshBk * td2k * abd1k) +
                                                        t3k *
                                                        (rbshBk * rfd3k * tbshdk + tbshBdk * (1.0 + rbd2k * rfshdk + td2k * rbd2k * td2k * rfshdk)) *
                                                        td2k * abd1k;
                                                TransBeamWin = t3k * tbshBBk * t2k * t1k +
                                                               t3k *
                                                               (tbshBBk * (rb2k * rfshBk + t2k * rb1k * t2k * rfshBk) + rbshBk * rfd3k * tbshdk +
                                                                tbshBdk * (1.0 + rbd2k * rfshdk + td2k * rbd1k * td2k * rfshdk)) *
                                                               td2k * td1k;
                                                ABlBack = t3k * abshBk + t3k * tbshBBk * (rb2k + t2k * rb1k * t2k) * afshBk +
                                                          t3k * rbshBk * rfd3k * abshdk + t3k * tbshBdk * (rbd2k + td2k * rbd1k * td2k) * afshdk;
                                            }

                                            BABSZone += BOverlap * ABlBack;
                                            IntBeamAbsByShadFac(BackSurfNum) = BOverlap * ABlBack / Surface(BackSurfNum).Area;

                                        } // End of check if between-glass blind is on back window

                                    } // End of check if blind is on back window

                                    if (ShadeFlagBack == ExtScreenOn) {

                                        // Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR SCREEN
                                        int ScNumBack = SurfWinScreenNumber(BackSurfNum); // Back surface screen number
                                        Real64 TGlBmBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).TransSolBeamCoef);
                                        Real64 RGlDiffFront = state.dataConstruction->Construct(ConstrNumBack).ReflectSolDiffFront;
                                        Real64 TScBmBmBack = SurfaceScreens(ScNumBack).BmBmTransBack; // Screen solar back beam-beam transmittance
                                        Real64 TScBmDiffBack = SurfaceScreens(ScNumBack).BmDifTransBack; // Screen solar back beam-diffuse transmittance
                                        Real64 RScBack = SurfaceScreens(ScNumBack).ReflectSolBeamFront;
                                        Real64 RScDifBack = SurfaceScreens(ScNumBack).DifReflect;
                                        for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                            Real64 AbWinBack = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBack).AbsBeamBackCoef({1, 6}, Lay));
                                            Real64 AGlDiffFront = state.dataConstruction->Construct(ConstrNumBack).AbsDiff(Lay);
                                            AbsBeamWin(Lay) = AbWinBack + (TGlBmBack * AGlDiffFront * RScBack / (1.0 - RScDifBack * RGlDiffFront));
                                        }

                                        // Interior beam transmitted by exterior back window with EXTERIOR SCREEN

                                        Real64 TScDifDif = SurfaceScreens(ScNumBack).DifDifTrans;
                                        Real64 RScBmDifBk = SurfaceScreens(ScNumBack).ReflectSolBeamBack; // Beam-diffuse back reflectance of blind
                                        Real64 RGlDifFr = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffFront;
                                        Real64 RScDifDifBk = SurfaceScreens(ScNumBack).DifReflect;
                                        TransBeamWin = TGlBmBack * (TScBmBmBack + TScBmDiffBack +
                                                                    TScDifDif * RScBmDifBk * RGlDifFr / (1.0 - RScDifDifBk * RGlDifFr));

                                        // Interior beam absorbed by EXTERIOR SCREEN on exterior back window

                                        Real64 AbsScBack = SurfaceScreens(ScNumBack).AbsorpSolarBeamBack; // Screen solar back beam absorptance
                                        Real64 AbsScDiffBack = SurfaceScreens(ScNumBack).DifScreenAbsorp; // Screen solar back diffuse absorptance
                                        Real64 RScDiffBack = SurfaceScreens(ScNumBack).ReflectSolBeamFront; // Screen solar back diffuse reflectance
                                        Real64 AScBack = TGlBmBack * (AbsScBack + RScBack * RGlDiffFront * AbsScDiffBack / (1.0 - RScDiffBack * RGlDiffFront)); // Screen solar back absorptance for interior solar
                                        BABSZone += BOverlap * AScBack;
                                        IntBeamAbsByShadFac(BackSurfNum) = BOverlap * AScBack / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                    } // End of check if exterior screen on back window

                                    // Interior beam absorptance of glass layers of back exterior window with SWITCHABLE GLAZING

                                    if (ShadeFlagBack == SwitchableGlazing && Surface(BackSurfNum).ExtBoundCond == 0) {
                                        Real64 SwitchFac = SurfWinSwitchingFactor(SurfNum); // Switching factor for a window
                                        Real64 AbsBeamWinSh;  // Glass layer beam solar absorptance of a shaded window
                                        for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                            AbsBeamWinSh = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBackSh).AbsBeamBackCoef({1, 6}, Lay));
                                            AbsBeamWin(Lay) = InterpSw(SwitchFac, AbsBeamWin(Lay), AbsBeamWinSh);
                                        }
                                        // Beam solar transmittance of a shaded window
                                        Real64 TransBeamWinSh = POLYF(CosIncBack, state.dataConstruction->Construct(ConstrNumBackSh).TransSolBeamCoef);
                                        TransBeamWin = InterpSw(SwitchFac, TransBeamWin, TransBeamWinSh);
                                    }

                                    // Sum of interior beam absorbed by all glass layers of back window

                                    AbsBeamTotWin = 0.0;
                                    for (int Lay = 1; Lay <= NBackGlass; ++Lay) {
                                        AbsBeamTotWin += AbsBeamWin(Lay);
                                        SurfWinA(Lay, BackSurfNum) +=
                                                BOverlap * AbsBeamWin(Lay) / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum)); //[-]
                                    }

                                    // To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

                                    BABSZone += BOverlap * (AbsBeamTotWin + TransBeamWin);

                                    // Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
                                    // this beam radiation is categorized as diffuse radiation in the adjacent zone.

                                    int AdjSurfNum = Surface(BackSurfNum).ExtBoundCond;
                                    if (AdjSurfNum > 0) {
                                        int adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;
                                        EnclSolDBIntWin(adjEnclosureNum) += BOverlap * TransBeamWin;                                          //[m2]
                                        SurfWinBmSolTransThruIntWinRep(BackSurfNum) += BOverlap * TransBeamWin * BeamSolarRad; //[W]
                                        SurfWinBmSolTransThruIntWinRepEnergy(BackSurfNum) =
                                                SurfWinBmSolTransThruIntWinRep(BackSurfNum) * TimeStepZoneSec;
                                    }
                                } // End of check if back surface is opaque or window
                                SurfBmIncInsSurfAmountRep(BackSurfNum) += BOverlap;
                                SurfBmIncInsSurfAmountRepEnergy(BackSurfNum) = SurfBmIncInsSurfAmountRep(BackSurfNum) * TimeStepZoneSec;
                            } // End of loop over back surfaces
                        } else if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                            // For complex window calculation goes over outgoing basis directions for current state
                            int CurCplxFenState = SurfaceWindow(SurfNum).ComplexFen.CurrentState; // Current state for complex fenestration
                            // Get construction number which keeps transmittance properties
                            int IConst = SurfaceWindow(SurfNum).ComplexFen.State(CurCplxFenState).Konst; // Current surface construction number (it depends of state too)
                            int FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, IConst);

                            // Solar radiation from this window will be calculated only in case when this window is not scheduled surface gained
                            if (FenSolAbsPtr == 0) {
                                // Current incoming direction number (Sun direction)
                                int IBm = ComplexWind(SurfNum).Geom(CurCplxFenState).SolBmIndex(HourOfDay, TimeStep);

                                // Report variables for complex fenestration here
                                SurfWinBSDFBeamDirectionRep(SurfNum) = IBm;
                                SurfWinBSDFBeamThetaRep(SurfNum) = ComplexWind(SurfNum).Geom(CurCplxFenState).ThetaBm(HourOfDay, TimeStep);
                                SurfWinBSDFBeamPhiRep(SurfNum) = ComplexWind(SurfNum).Geom(CurCplxFenState).PhiBm(HourOfDay, TimeStep);

                                int BaseSurf = Surface(SurfNum).BaseSurf;  // Base surface number for current complex window
                                // Get total number of back surfaces for current window (surface)
                                // Note that it is organized by base surface
                                int NBkSurf = ShadowComb(BaseSurf).NumBackSurf;

                                if (!allocated(CFBoverlap)) {
                                    CFBoverlap.allocate(NBkSurf);
                                }

                                if (!allocated(CFDirBoverlap)) {
                                    CFDirBoverlap.allocate(NBkSurf, ComplexWind(SurfNum).Geom(CurCplxFenState).Trn.NBasis);
                                }

                                CFBoverlap = 0.0;

                                // Calculate effects on all back surfaces for each of basis directions.  Each of basis directions from the back of the
                                // window has to be considered as beam and therefore calcualte CFBoverlap for each of them
                                for (int CurTrnDir = 1; CurTrnDir <= ComplexWind(SurfNum).Geom(CurCplxFenState).Trn.NBasis; ++CurTrnDir) {
                                    Real64 CurLambda = ComplexWind(SurfNum).Geom(CurCplxFenState).Trn.Lamda(CurTrnDir); // Current lambda value in BSDF outgoing directions
                                    Real64 DirTrans = state.dataConstruction->Construct(IConst).BSDFInput.SolFrtTrans(IBm, CurTrnDir); // Current BSDF directional transmittance
                                    // Now calculate effect of this direction on all back surfaces
                                    for (int IBack = 1; IBack <= NBkSurf; ++IBack) {
                                        CFDirBoverlap(IBack, CurTrnDir) =
                                                ComplexWind(SurfNum).Geom(CurCplxFenState).AOverlap(IBack, CurTrnDir) * DirTrans * CurLambda * CosInc;
                                        CFBoverlap(IBack) += CFDirBoverlap(IBack, CurTrnDir);
                                    } // DO IBack = 1,MaxBkSurf
                                }

                                // Summarizing results
                                for (int IBack = 1; IBack <= NBkSurf; ++IBack) {
                                    int BackSurfaceNumber = ShadowComb(BaseSurf).BackSurf(IBack);
                                    int ConstrNumBack = Surface(BackSurfaceNumber).Construction;

                                    // Do not perform any calculation if surface is scheduled for incoming solar radiation
                                    int SurfSolIncPtr = SurfaceScheduledSolarInc(BackSurfaceNumber, ConstrNumBack);

                                    if (SurfSolIncPtr == 0) {
                                        // Surface hit is another complex fenestration
                                        if (SurfWinWindowModelType(BackSurfaceNumber) == WindowBSDFModel) {
                                            int CurBackState = SurfaceWindow(BackSurfaceNumber).ComplexFen.CurrentState; // Current state for back surface if that surface is complex fenestration

                                            // Do not take into account this window if it is scheduled for surface gains
                                            FenSolAbsPtr = WindowScheduledSolarAbs(BackSurfaceNumber, ConstrNumBack);

                                            if (FenSolAbsPtr == 0) {
                                                // Calculate energy loss per each outgoing orientation
                                                for (int CurTrnDir = 1; CurTrnDir <= ComplexWind(SurfNum).Geom(CurCplxFenState).Trn.NBasis; ++CurTrnDir) {
                                                    Real64 bestDot; // complex fenestration hits other complex fenestration, it is important to find
                                                    // matching beam directions.  Beam leving one window will have certaing number for it's basis
                                                    // while same beam reaching back surface will have different beam number.  This value is used
                                                    // to keep best matching dot product for those directions
                                                    Real64 curDot;   // temporary variable for current dot product
                                                    int bestTrn;     // Direction corresponding best dot product for master window
                                                    int bestBackTrn; // Direction corresponding best dot product for back surface window
                                                    for (int CurBackDir = 1; CurBackDir <= ComplexWind(BackSurfaceNumber).Geom(CurBackState).Trn.NBasis;
                                                         ++CurBackDir) {
                                                        // Purpose of this part is to find best match for outgoing beam number of window back surface
                                                        // and incoming beam number of complex fenestration which this beam will hit on (back surface
                                                        // again)
                                                        curDot = dot(ComplexWind(SurfNum).Geom(CurCplxFenState).sTrn(CurTrnDir),
                                                                     ComplexWind(BackSurfaceNumber).Geom(CurBackState).sTrn(CurBackDir));
                                                        if (CurBackDir == 1) {
                                                            bestDot = curDot;
                                                            bestTrn = CurTrnDir;
                                                            bestBackTrn = CurBackDir;
                                                        } else {
                                                            if (curDot < bestDot) {
                                                                bestDot = curDot;
                                                                bestTrn = CurTrnDir;
                                                                bestBackTrn = CurBackDir;
                                                            }
                                                        }
                                                    }
                                                    // CurLambda = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%Trn%Lamda(CurTrnDir)
                                                    // Add influence of this exact direction to what stays in the zone.  It is important to note that
                                                    // this needs to be done for each outgoing direction
                                                    BABSZone += CFDirBoverlap(IBack, CurTrnDir) * (1 - SurfaceWindow(BackSurfaceNumber)
                                                            .ComplexFen.State(CurBackState)
                                                            .IntegratedBkRefl(bestBackTrn));

                                                    // Absorptance from current back direction
                                                    int TotSolidLay = state.dataConstruction->Construct(ConstrNumBack).TotSolidLayers;
                                                    for (int Lay = 1; Lay <= TotSolidLay; ++Lay) {
                                                        // IF (ALLOCATED(Construct(ConstrNumBack)%BSDFInput)) THEN
                                                        // CFDirBoverlap is energy transmitted for current basis beam.  It is important to note that
                                                        // AWinOverlap array needs to contain flux and not absorbed energy because later in the code
                                                        // this will be multiplied with window area
                                                        SurfWinACFOverlap(Lay, BackSurfaceNumber) +=
                                                                state.dataConstruction->Construct(ConstrNumBack).BSDFInput.Layer(Lay).BkAbs(bestBackTrn, 1) *
                                                                CFDirBoverlap(IBack, CurTrnDir) / Surface(BackSurfaceNumber).Area;
                                                        // END IF
                                                    }

                                                    // Interior beam transmitted to adjacent zone through an interior back window;
                                                    // This beam radiation is categorized as diffuse radiation in the adjacent zone.
                                                    // Note that this is done for each outgoing direction of exterior window

                                                    int AdjSurfNum = Surface(BackSurfaceNumber).ExtBoundCond;
                                                    if (AdjSurfNum > 0) {
                                                        int adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;
                                                        EnclSolDBIntWin(adjEnclosureNum) +=
                                                                CFDirBoverlap(IBack, CurTrnDir) * SurfaceWindow(BackSurfaceNumber)
                                                                        .ComplexFen.State(CurBackState)
                                                                        .IntegratedBkTrans(bestBackTrn);
                                                        SurfWinBmSolTransThruIntWinRep(BackSurfaceNumber) +=
                                                                CFDirBoverlap(IBack, CurTrnDir) *
                                                                SurfaceWindow(BackSurfaceNumber)
                                                                        .ComplexFen.State(CurBackState)
                                                                        .IntegratedBkTrans(bestBackTrn) *
                                                                BeamSolarRad; //[W]
                                                        SurfWinBmSolTransThruIntWinRepEnergy(BackSurfaceNumber) =
                                                                SurfWinBmSolTransThruIntWinRep(BackSurfaceNumber) * TimeStepZoneSec;
                                                    }
                                                }
                                            }
                                        } else {
                                            if (state.dataConstruction->Construct(ConstrNumBack).TransDiff <= 0.0) {
                                                // Do not take into account this window if it is scheduled for surface gains
                                                SurfSolIncPtr = SurfaceScheduledSolarInc(BackSurfaceNumber, ConstrNumBack);

                                                if (SurfSolIncPtr == 0) {
                                                    Real64 AbsIntSurf = state.dataConstruction->Construct(ConstrNumBack).InsideAbsorpSolar;
                                                    SurfOpaqAI(BackSurfaceNumber) += CFBoverlap(IBack) * AbsIntSurf / Surface(BackSurfaceNumber).Area;
                                                    BABSZone += CFBoverlap(IBack) * AbsIntSurf;
                                                }
                                            } else {
                                                // Code for mixed windows goes here.  It is same as above code for "ordinary" windows.
                                                // Try to do something which will not produce duplicate code.
                                            }
                                        }
                                    }
                                }

                                if (allocated(CFBoverlap)) CFBoverlap.deallocate();
                                if (allocated(CFDirBoverlap)) CFDirBoverlap.deallocate();
                            }

                        } else if (SurfWinWindowModelType(SurfNum) == WindowEQLModel) {

                            for (int IBack = 1; IBack <= MaxBkSurf; ++IBack) {

                                int BackSurfNum = BackSurfaces(TimeStep, HourOfDay, IBack, SurfNum);

                                if (BackSurfNum == 0) break; // No more irradiated back surfaces for this exterior window
                                if (SurfWinWindowModelType(IBack) != WindowEQLModel) continue; // only EQL back window is allowed

                                int ConstrNumBack = Surface(BackSurfNum).Construction;
                                int NBackGlass = state.dataConstruction->Construct(ConstrNumBack).TotGlassLayers;
                                // Irradiated (overlap) area for this back surface, projected onto window plane
                                // (includes effect of shadowing on exterior window)
                                Real64 AOverlap = OverlapAreas(TimeStep, HourOfDay, IBack, SurfNum);
                                Real64 BOverlap = TBm * AOverlap * CosInc; //[m2]

                                if (state.dataConstruction->Construct(ConstrNumBack).TransDiff <= 0.0) {

                                    // Back surface is opaque interior or exterior wall

                                    Real64 AbsIntSurf = state.dataConstruction->Construct(ConstrNumBack).InsideAbsorpSolar;

                                    // Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                                    // Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                                    // HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                                    Real64 HMovInsul = 0.0;
                                    Real64 AbsInt = 0.0;
                                    if (Surface(BackSurfNum).MaterialMovInsulInt > 0) {
                                        Real64 MovInsulSchedVal = GetCurrentScheduleValue(Surface(BackSurfNum).SchedMovInsulInt);
                                        if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
                                            HMovInsul = 0.0;
                                        } else { // Movable insulation present
                                            HMovInsul = 1.0 / (MovInsulSchedVal * dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).Resistance);
                                            AbsInt = dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).AbsorpSolar;
                                        }
                                    }
                                    if (HMovInsul > 0.0) AbsIntSurf = AbsInt; // Movable inside insulation present

                                    SurfOpaqAI(BackSurfNum) += BOverlap * AbsIntSurf / Surface(BackSurfNum).Area; //[-]
                                    BABSZone += BOverlap * AbsIntSurf;                                        //[m2]

                                } else {

                                    // Back surface is an interior or exterior window
                                    // Note that exterior back windows with and without shades are treated as defined.
                                    // Equivalent Layer window model has no distinction when treating windows with and
                                    // without shades (interior, inbetween and exterior shades)

                                    //  Note in equivalent layer window model if storm window exists it is defined as part of
                                    //  window construction, hence it does not require a separate treatment
                                    AbsBeamWinEQL = 0.0;
                                    Real64 TransBeamWin = 0.0; // Beam solar transmittance of a window

                                    // Interior beam absorptance of glass layers and beam transmittance of back exterior  &
                                    // or interior window (treates windows with/without shades as defined) for this timestep

                                    // call the ASHWAT fenestration model for beam radiation here
                                    WindowEquivalentLayer::CalcEQLOpticalProperty(state, BackSurfNum, isBEAM, AbsSolBeamBackEQL);

                                    int EQLNum = state.dataConstruction->Construct(ConstrNumBack).EQLConsPtr;
                                    AbsBeamWinEQL({ 1, CFS(EQLNum).NL }) = AbsSolBeamBackEQL(1, { 1, CFS(EQLNum).NL });
                                    // get the interior beam transmitted through back exterior or interior EQL window
                                    TransBeamWin = AbsSolBeamBackEQL(1, CFS(EQLNum).NL + 1);
                                    //   Absorbed by the interior shade layer of back exterior window
                                    if (CFS(EQLNum).L(CFS(EQLNum).NL).LTYPE != ltyGLAZE) {
                                        IntBeamAbsByShadFac(BackSurfNum) = BOverlap * AbsSolBeamBackEQL(1, CFS(EQLNum).NL) /
                                                                           (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                        BABSZone += BOverlap * AbsSolBeamBackEQL(1, CFS(EQLNum).NL);
                                    }
                                    //   Absorbed by the exterior shade layer of back exterior window
                                    if (CFS(EQLNum).L(1).LTYPE != ltyGLAZE) {
                                        IntBeamAbsByShadFac(BackSurfNum) =
                                                BOverlap * AbsSolBeamBackEQL(1, 1) / (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum));
                                        BABSZone += BOverlap * AbsSolBeamBackEQL(1, 1);
                                    }

                                    // determine the number of glass layers
                                    NBackGlass = 0;
                                    for (int Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {
                                        if (CFS(EQLNum).L(Lay).LTYPE != ltyGLAZE) continue;
                                        ++NBackGlass;
                                    }
                                    if (NBackGlass >= 2) {
                                        // If the number of glass is greater than 2, in between glass shade can be present
                                        for (int Lay = 2; Lay <= CFS(EQLNum).NL - 1; ++Lay) {
                                            if (CFS(EQLNum).L(CFS(EQLNum).NL).LTYPE != ltyGLAZE) {
                                                // if there is in between shade glass determine the shade absorptance
                                                IntBeamAbsByShadFac(BackSurfNum) += BOverlap * AbsSolBeamBackEQL(1, Lay) / Surface(BackSurfNum).Area;
                                                BABSZone += BOverlap * AbsSolBeamBackEQL(1, Lay);
                                            }
                                        }
                                    }
                                    // Sum of interior beam absorbed by all glass layers of back window
                                    Real64 AbsBeamTotWin = 0.0; // Glass layer beam solar absorptance of a shaded window
                                    for (int Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {
                                        AbsBeamTotWin += AbsBeamWinEQL(Lay);
                                        SurfWinA(Lay, BackSurfNum) += BOverlap * AbsBeamWinEQL(Lay) /
                                                                      (Surface(BackSurfNum).Area + SurfWinDividerArea(BackSurfNum)); //[-]
                                    }

                                    // To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

                                    BABSZone += BOverlap * (AbsBeamTotWin + TransBeamWin);

                                    // Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
                                    // this beam radiation is categorized as diffuse radiation in the adjacent zone.

                                    int AdjSurfNum = Surface(BackSurfNum).ExtBoundCond;
                                    if (AdjSurfNum > 0) {
                                        int adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;
                                        EnclSolDBIntWin(adjEnclosureNum) += BOverlap * TransBeamWin;                                          //[m2]
                                        SurfWinBmSolTransThruIntWinRep(BackSurfNum) += BOverlap * TransBeamWin * BeamSolarRad; //[W]
                                        SurfWinBmSolTransThruIntWinRepEnergy(BackSurfNum) =
                                                SurfWinBmSolTransThruIntWinRep(BackSurfNum) * TimeStepZoneSec;
                                    }
                                } // End of check if back surface is opaque or window
                                SurfBmIncInsSurfAmountRep(BackSurfNum) += BOverlap;
                                SurfBmIncInsSurfAmountRepEnergy(BackSurfNum) = SurfBmIncInsSurfAmountRep(BackSurfNum) * TimeStepZoneSec;
                            } // End of loop over back surfaces

                            //  *****************************

                        }    // IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel) THEN
                    } else { // Simple interior solar distribution. All beam from exterior windows falls on floor;
                        // some of this is absorbed/transmitted, rest is reflected to other surfaces.

                        for (int const FloorNum : thisEnclosure.SurfacePtr) {
                            // In following, ISABSF is zero except for nominal floor surfaces
                            if (state.dataSolarShading->ISABSF(FloorNum) <= 0.0 || FloorNum == SurfNum) continue; // Keep only floor surfaces
                            int FlConstrNum = Surface(FloorNum).Construction;

                            Real64 BTOTWinZone = TBm * SunLitFract * Surface(SurfNum).Area * CosInc * InOutProjSLFracMult; //[m2]
                            Real64 AbsBeamTotWin = 0.0;

                            if (state.dataConstruction->Construct(FlConstrNum).TransDiff <= 0.0) {
                                // Opaque surface
                                SurfOpaqAI(FloorNum) += BTOTWinZone * state.dataSolarShading->ISABSF(FloorNum) / Surface(FloorNum).Area; //[-]
                            } else if (state.dataConstruction->Construct(FlConstrNum).TypeIsAirBoundaryInteriorWindow) {
//                                TransBeamWin = 1.0;
                                AbsBeamWinEQL = 0.0;
                                AbsBeamTotWin = 0.0;
                            } else {
                                // Window

                                // Note that diffuse solar absorptance is used here for floor windows even though we're
                                // dealing with incident beam radiation. This is because, for this simple interior distribution,
                                // the beam radiation from exterior windows is assumed to be uniformly distributed over the
                                // floor and so it makes no sense to use directional absorptances. Note also that floor windows
                                // are assumed to not have blinds or shades in this calculation.
                                // For the case of the floor window a complex fenestration (strange situation) the correct back
                                // diffuse layer absorptions have already been put into the construction
                                if (SurfWinStormWinFlag(FloorNum) == 1) FlConstrNum = Surface(FloorNum).StormWinConstruction;

                                for (int Lay = 1; Lay <= state.dataConstruction->Construct(FlConstrNum).TotGlassLayers; ++Lay) {
                                    AbsBeamTotWin += state.dataConstruction->Construct(FlConstrNum).AbsDiffBack(Lay);
                                }
                                // In the following we have to multiply by the AbsDiffBack(Lay)/AbsBeamTotWin ratio to get the
                                // layer by layer absorbed beam since ISABSF(FloorNum) is proportional to AbsBeamTotWin
                                // (see ComputeIntSolarAbsorpFactors).

                                for (int Lay = 1; Lay <= state.dataConstruction->Construct(FlConstrNum).TotGlassLayers; ++Lay) {
                                    SurfWinA(Lay, FloorNum) += state.dataConstruction->Construct(FlConstrNum).AbsDiffBack(Lay) / AbsBeamTotWin * BTOTWinZone *
                                                               state.dataSolarShading->ISABSF(FloorNum) / Surface(FloorNum).Area; //[-]
                                }
                            }

                            BABSZone += BTOTWinZone * state.dataSolarShading->ISABSF(FloorNum); //[m2]

                            int AdjSurfNum = Surface(FloorNum).ExtBoundCond;
                            if (state.dataConstruction->Construct(FlConstrNum).TransDiff > 0.0 && AdjSurfNum > 0) {

                                // Window in an interior floor

                                int adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;

                                // Contribution (assumed diffuse) to adjacent zone of beam radiation passing
                                // through this window
                                EnclSolDBIntWin(adjEnclosureNum) += BTOTWinZone * state.dataSolarShading->ISABSF(FloorNum) * state.dataConstruction->Construct(FlConstrNum).TransDiff / AbsBeamTotWin;

                                BABSZone += BTOTWinZone * state.dataSolarShading->ISABSF(FloorNum) * state.dataConstruction->Construct(FlConstrNum).TransDiff / AbsBeamTotWin;
                            }

                        } // End of loop over floor sections
                    }     // End of check on complex vs. simple interior solar distribution

                } // End of sunlit fraction > 0 test

            } // End of first loop over surfaces in zone

            Real64 BABSZoneSSG = 0.0; // Beam radiation from exterior windows absorbed in a zone (only for scheduled surface gains)
            Real64 BTOTZoneSSG = 0.0; // Solar entering a zone in case of scheduled surface gains
            for (int iSSG = 1; iSSG <= TotSurfIncSolSSG; ++iSSG) {
                int SurfNum = SurfIncSolSSG(iSSG).SurfPtr;
                // do calculation only if construction number match.
                if (SurfIncSolSSG(iSSG).ConstrPtr == Surface(SurfNum).Construction) {
                    if (Surface(SurfNum).SolarEnclIndex == enclosureNum) {
                        Real64 AbsIntSurf = state.dataConstruction->Construct(Surface(SurfNum).Construction).InsideAbsorpSolar;
                        // SolarIntoZone = GetCurrentScheduleValue(SurfIncSolSSG(iSSG)%SchedPtr) * Surface(SurfNum)%Area
                        Real64 SolarIntoZone = GetCurrentScheduleValue(SurfIncSolSSG(iSSG).SchedPtr); // Solar radiation into zone to current surface
                        SurfOpaqAI(SurfNum) = SolarIntoZone * AbsIntSurf;
                        BABSZoneSSG += SurfOpaqAI(SurfNum) * Surface(SurfNum).Area;
                        BTOTZoneSSG += SolarIntoZone * Surface(SurfNum).Area;
                    }
                }
            }
            EnclSolDBSSG(enclosureNum) = BTOTZoneSSG - BABSZoneSSG;

            DBZone(enclosureNum) = BTOTZone - BABSZone;

            if (DBZone(enclosureNum) < 0.0) {
                DBZone(enclosureNum) = 0.0;
            }

            // Variables for reporting
            for (int const SurfNum : thisEnclosure.SurfacePtr) {
                if (SolarDistribution == FullInteriorExterior) {
                    SurfBmIncInsSurfAmountRep(SurfNum) *= BeamSolarRad;
                    SurfBmIncInsSurfAmountRepEnergy(SurfNum) = SurfBmIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec;
                    SurfBmIncInsSurfIntensRep(SurfNum) = SurfBmIncInsSurfAmountRep(SurfNum) / (Surface(SurfNum).Area + SurfWinDividerArea(SurfNum));
                } else { // Simple interior solar distribution. All beam falls on floor.
                    if (state.dataSolarShading->ISABSF(SurfNum) > 0.0 && Surface(SurfNum).HeatTransSurf) {
                        if (thisEnclosure.FloorArea > 0.0) {
                            // spread onto all floor surfaces, these may or may not be called "floor"
                            SurfBmIncInsSurfIntensRep(SurfNum) = BeamSolarRad * BTOTZone / thisEnclosure.FloorArea;
                        } else if (thisEnclosure.TotalSurfArea > 0.0) {
                            // spread onto all interior surfaces
                            SurfBmIncInsSurfIntensRep(SurfNum) = BeamSolarRad * BTOTZone / thisEnclosure.TotalSurfArea;
                        } else { // divide be zero otherwise
                            SurfBmIncInsSurfIntensRep(SurfNum) = 0.0;
                        }
                    }
                    SurfBmIncInsSurfAmountRep(SurfNum) = Surface(SurfNum).Area * SurfBmIncInsSurfIntensRep(SurfNum);
                    SurfBmIncInsSurfAmountRepEnergy(SurfNum) = SurfBmIncInsSurfAmountRep(SurfNum) * TimeStepZoneSec;
                }
                if (Surface(SurfNum).Class == SurfaceClass::Window || Surface(SurfNum).Class == SurfaceClass::TDD_Dome) {

                    SurfWinIntBeamAbsByShade(SurfNum) = IntBeamAbsByShadFac(SurfNum);
                    SurfWinExtBeamAbsByShade(SurfNum) = BeamSolarRad * ExtBeamAbsByShadFac(SurfNum);

                    if ((Surface(SurfNum).ExtBoundCond == ExternalEnvironment) || (Surface(SurfNum).ExtBoundCond == OtherSideCondModeledExt)) {

                        int ShadeFlag = SurfWinShadingFlag(SurfNum);
                        int ShelfNum = Surface(SurfNum).Shelf;
                        int OutShelfSurf = 0;
                        if (ShelfNum > 0) { // Outside daylighting shelf
                            OutShelfSurf = Shelf(ShelfNum).OutSurf;
                        }

                        // This lookup may be avoid if this 2nd surf loop can be combined with the 1st
                        if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                            int PipeNum = SurfWinTDDPipeNum(SurfNum);
                            int SurfNum2 = TDDPipe(PipeNum).Dome;
                            Real64 CosInc = CosIncAng(TimeStep, HourOfDay, SurfNum2);
                            // Exterior diffuse solar incident on window (W/m2)
                            Real64 DifSolarInc = DifSolarRad * AnisoSkyMult(SurfNum2) + GndSolarRad * Surface(SurfNum2).ViewFactorGround;
                            // Exterior diffuse sky solar transmitted by TDD (W/m2)
                            Real64 SkySolarTrans = DifSolarRad * TransTDD(state, PipeNum, CosInc, SolarAniso) * AnisoSkyMult(SurfNum2);
                            // Exterior diffuse ground solar transmitted by TDD (W/m2)
                            Real64 GndSolarTrans = GndSolarRad * TDDPipe(PipeNum).TransSolIso * Surface(SurfNum2).ViewFactorGround;

                            SurfWinBmSolar(SurfNum) = BeamSolarRad * WinTransBmSolar(SurfNum);
                            SurfWinDifSolar(SurfNum) = SkySolarTrans * Surface(SurfNum).Area + GndSolarTrans * Surface(SurfNum).Area;
                            SurfWinBmSolarEnergy(SurfNum) = SurfWinBmSolar(SurfNum) * TimeStepZoneSec;
                            SurfWinDifSolarEnergy(SurfNum) = SurfWinDifSolar(SurfNum) * TimeStepZoneSec;

                            SurfWinTransSolar(SurfNum) = SurfWinBmSolar(SurfNum) + SurfWinDifSolar(SurfNum); //[W]
                            SurfWinTransSolarEnergy(SurfNum) = SurfWinTransSolar(SurfNum) * TimeStepZoneSec;

                            TDDPipe(PipeNum).TransmittedSolar = SurfWinTransSolar(SurfNum);
                            // TDDPipe(PipeNum)%TransSolBeam = TBmBm ! Reported above
                            if (DifSolarInc > 0) {
                                TDDPipe(PipeNum).TransSolDiff = (SkySolarTrans + GndSolarTrans) / DifSolarInc;
                            } else {
                                TDDPipe(PipeNum).TransSolDiff = 0.0;
                            }

                        } else if (OutShelfSurf > 0) { // Outside daylighting shelf
                            Real64 ShelfSolarRad =
                                    (BeamSolarRad * SunlitFrac(TimeStep, HourOfDay, OutShelfSurf) * CosIncAng(TimeStep, HourOfDay, OutShelfSurf) +
                                     DifSolarRad * AnisoSkyMult(OutShelfSurf)) *
                                    Shelf(ShelfNum).OutReflectSol;

                            Real64 DifSolarInc = DifSolarRad * AnisoSkyMult(SurfNum) + GndSolarRad * Surface(SurfNum).ViewFactorGround +
                                          ShelfSolarRad * Shelf(ShelfNum).ViewFactor;

                            SurfWinBmSolar(SurfNum) = BeamSolarRad * WinTransBmSolar(SurfNum);
                            SurfWinDifSolar(SurfNum) = DifSolarInc * WinTransDifSolar(SurfNum);
                            SurfWinBmSolarEnergy(SurfNum) = SurfWinBmSolar(SurfNum) * TimeStepZoneSec;
                            SurfWinDifSolarEnergy(SurfNum) = SurfWinDifSolar(SurfNum) * TimeStepZoneSec;

                            SurfWinTransSolar(SurfNum) = SurfWinBmSolar(SurfNum) + SurfWinDifSolar(SurfNum); //[W]
                            SurfWinTransSolarEnergy(SurfNum) = SurfWinTransSolar(SurfNum) * TimeStepZoneSec;

                        } else { // Regular window
                            Real64 SkySolarInc = SurfSkySolarInc(SurfNum);
                            Real64 GndSolarInc = SurfGndSolarInc(SurfNum);
                            Real64 DifSolarInc = SkySolarInc + GndSolarInc;
                            SurfWinBmSolar(SurfNum) = BeamSolarRad * WinTransBmSolar(SurfNum);
                            // Note: for complex fenestration, WinTransDifSolar has previously been defined using the effective
                            // transmittance for sky and ground diffuse radiation (including beam radiation reflected from the ground)
                            // so these calculations should be correct
                            SurfWinDifSolar(SurfNum) = DifSolarInc * WinTransDifSolar(SurfNum);
                            SurfWinBmSolarEnergy(SurfNum) = SurfWinBmSolar(SurfNum) * TimeStepZoneSec;
                            SurfWinDifSolarEnergy(SurfNum) = SurfWinDifSolar(SurfNum) * TimeStepZoneSec;
                            if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                if (Blind(SurfWinBlindNumber(SurfNum)).SlatOrientation == Horizontal) {
                                    SurfWinDifSolar(SurfNum) = SkySolarInc * WinTransDifSolarSky(SurfNum) + GndSolarInc * WinTransDifSolarGnd(SurfNum);
                                    SurfWinDifSolarEnergy(SurfNum) = SurfWinDifSolar(SurfNum) * TimeStepZoneSec;
                                }
                            }

                            SurfWinTransSolar(SurfNum) = SurfWinBmSolar(SurfNum) + SurfWinDifSolar(SurfNum); //[W]
                            SurfWinTransSolarEnergy(SurfNum) = SurfWinTransSolar(SurfNum) * TimeStepZoneSec;
                        }

                        // added TH 12/9/2009, CR 7907 & 7809
                        SurfWinBmBmSolar(SurfNum) = BeamSolarRad * WinTransBmBmSolar;

                        SurfWinBmDifSolar(SurfNum) = BeamSolarRad * WinTransBmDifSolar;
                        SurfWinBmBmSolarEnergy(SurfNum) = SurfWinBmBmSolar(SurfNum) * TimeStepZoneSec;
                        SurfWinBmDifSolarEnergy(SurfNum) = SurfWinBmDifSolar(SurfNum) * TimeStepZoneSec;

                        SurfWinDirSolTransAtIncAngle(SurfNum) = TBmBm + TBmDif; // For TDD:DIFFUSER this is the TDD transmittance

                        // Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
                        if (Surface(SurfNum).Class != SurfaceClass::TDD_Dome) {
                            ZoneTransSolar(enclosureNum) += SurfWinTransSolar(SurfNum);                         //[W]
                            ZoneTransSolarEnergy(enclosureNum) = ZoneTransSolar(enclosureNum) * TimeStepZoneSec; //[J]
                            ZoneBmSolFrExtWinsRep(enclosureNum) += SurfWinBmSolar(SurfNum);
                            ZoneDifSolFrExtWinsRep(enclosureNum) += SurfWinDifSolar(SurfNum);
                            ZoneBmSolFrExtWinsRepEnergy(enclosureNum) = ZoneBmSolFrExtWinsRep(enclosureNum) * TimeStepZoneSec;   //[J]
                            ZoneDifSolFrExtWinsRepEnergy(enclosureNum) = ZoneDifSolFrExtWinsRep(enclosureNum) * TimeStepZoneSec; //[J]
                        }
                    }
                }
            } // End of second loop over surfaces in zone

        } // End of first zone loop

        // Add interior window contribution to DBZone

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            DBZone(enclosureNum) += EnclSolDBIntWin(enclosureNum);
            ZoneBmSolFrIntWinsRep(enclosureNum) = EnclSolDBIntWin(enclosureNum) * BeamSolarRad;
            ZoneBmSolFrIntWinsRepEnergy(enclosureNum) = ZoneBmSolFrIntWinsRep(enclosureNum) * TimeStepZoneSec; //[J]
        }

        // RJH - Calculate initial distribution of diffuse solar transmitted by exterior windows into each zone
        //       to all interior surfaces in the zone
        //       Includes subsequent transmittance of diffuse solar to adjacent zones through interior windows
        CalcWinTransDifSolInitialDistribution(state);
    }
    void CalcAbsorbedOnExteriorOpaqueSurfaces(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates solar energy absorbed on exterior opaque surfaces

        using namespace DataDaylightingDevices;

        for (int ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            for (int SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
                if (((Surface(SurfNum).ExtBoundCond != ExternalEnvironment) && (Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)) &&
                    SurfWinOriginalClass(SurfNum) != SurfaceClass::TDD_Diffuser)
                    continue;
                if (!Surface(SurfNum).HeatTransSurf) continue;
                // TH added 3/24/2010 while debugging CR 7872
                if (!Surface(SurfNum).ExtSolar) continue;
                int ConstrNum = Surface(SurfNum).Construction;
                int ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                if (SurfWinStormWinFlag(SurfNum) == 1) {
                    ConstrNum = Surface(SurfNum).StormWinConstruction;
                    ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                }

                int SurfNum2 = 0;
                if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    int PipeNum = SurfWinTDDPipeNum(SurfNum);
                    SurfNum2 = TDDPipe(PipeNum).Dome;
                } else {
                    SurfNum2 = SurfNum;
                }

                Real64 CosInc = CosIncAng(TimeStep, HourOfDay, SurfNum2);
                Real64 SunLitFract = SunlitFrac(TimeStep, HourOfDay, SurfNum2);

                //-------------------------------------------------------------------------
                // EXTERIOR BEAM SOLAR RADIATION ABSORBED ON THE OUTSIDE OF OPAQUE SURFACES
                //-------------------------------------------------------------------------

                if (SunLitFract > 0.0 && state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) {
                    SurfOpaqAO(SurfNum) = state.dataConstruction->Construct(ConstrNum).OutsideAbsorpSolar * CosInc * SunLitFract;

                    // Note: movable insulation, if present, is accounted for in subr. InitIntSolarDistribution,
                    // where QRadSWOutMvIns is calculated from QRadSWOutAbs and insulation solar absorptance
                }
            }
        }
    }

    void CalcInteriorSolarDistributionWCE(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2017
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates solar distribution

        CalcAbsorbedOnExteriorOpaqueSurfaces(state);

        if (state.dataWindowManager->winOpticalModel->isSimplifiedModel()) {
            CalcInteriorSolarDistributionWCESimple(state);
        } // else for built in BSDF (possible future implementation)
    }

    void CalcInteriorSolarDistributionWCESimple(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2017

        // PURPOSE OF THIS SUBROUTINE:
        // For a time step, calculates solar radiation absorbed by window layers, sky and diffuse solar
        // gain into zone from exterior window, beam solar on exterior window transmitted as beam and/or diffuse
        // and interior beam from exterior window that is absorbed/transmitted by back surfaces

        using ScheduleManager::GetCurrentScheduleValue;
        using namespace DataDaylightingDevices;
        using namespace MultiLayerOptics;

        if (state.dataSolarShading->MustAllocSolarShading) {
            EnclSolDBIntWin.allocate(NumOfZones);
        }

        EnclSolDS = 0.0;
        DGZone = 0.0;
        DBZone = 0.0;
        EnclSolDBIntWin = 0.0;
        SurfOpaqAI = 0.0;
        SurfOpaqAO = 0.0;

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {

            Real64 BABSZone = 0;
            Real64 BTOTZone = 0;
            ZoneTransSolar(enclosureNum) = 0;
            ZoneTransSolarEnergy(enclosureNum) = 0;
            ZoneBmSolFrExtWinsRep(enclosureNum) = 0;
            ZoneDifSolFrExtWinsRep(enclosureNum) = 0;
            ZoneBmSolFrExtWinsRepEnergy(enclosureNum) = 0;
            ZoneDifSolFrExtWinsRepEnergy(enclosureNum) = 0;
            auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));

            for (int const SurfNum : thisEnclosure.SurfacePtr) {
                if (Surface(SurfNum).Class != SurfaceClass::Window) continue;
                int SurfNum2 = 0;
                if (SurfWinOriginalClass(SurfNum) == SurfaceClass::TDD_Diffuser) {
                    int PipeNum = SurfWinTDDPipeNum(SurfNum);
                    SurfNum2 = TDDPipe(PipeNum).Dome;
                } else {
                    SurfNum2 = SurfNum;
                }
                auto &window = SurfaceWindow(SurfNum2);
                Real64 CosInc = CosIncAng(TimeStep, HourOfDay, SurfNum2);
                std::pair<Real64, Real64> incomingAngle = getSunWCEAngles(state, SurfNum2, BSDFHemisphere::Incoming);
                Real64 Theta = incomingAngle.first;
                Real64 Phi = incomingAngle.second;
                Real64 SunLitFract = SunlitFrac(TimeStep, HourOfDay, SurfNum2);

                int ConstrNum = Surface(SurfNum2).Construction;
                if (Surface(SurfNum2).activeShadedConstruction > 0) ConstrNum = Surface(SurfNum2).activeShadedConstruction;
                auto aLayer = CWindowConstructionsSimplified::instance().getEquivalentLayer(state, WavelengthRange::Solar, ConstrNum);

                ///////////////////////////////////////////////
                // Solar absorbed in window layers
                ///////////////////////////////////////////////
                if (SunlitFracWithoutReveal(TimeStep, HourOfDay, SurfNum2) > 0.0) {
                    auto numOfLayers = aLayer->getNumOfLayers();
                    if (SurfWinWindowModelType(SurfNum) == WindowBSDFModel) {
                        auto CurrentState = SurfaceWindow(SurfNum).ComplexFen.CurrentState;
                        auto &cplxState = SurfaceWindow(SurfNum).ComplexFen.State(CurrentState);
                        for (size_t Lay = 1; Lay <= numOfLayers; ++Lay) {
                            // Simon: Imporant note about this equation is to use BeamSolarRad and not QRadSWOutIncident
                            // is becuase BeamSolarRad is direct normal radiation (looking at the Sun) while QRadSWOutIncident
                            // is normal to window incidence. Since BSDF coefficients are taking into account angle of incidence,
                            // BeamSolarRad should be used in this case
                            SurfWinQRadSWwinAbs(Lay, SurfNum) = cplxState.WinSkyFtAbs(Lay) * SurfSkySolarInc(SurfNum2) +
                                                         cplxState.WinSkyGndAbs(Lay) * SurfGndSolarInc(SurfNum2) + SurfWinA(Lay, SurfNum) * BeamSolarRad +
                                                         SurfWinACFOverlap(Lay, SurfNum) * BeamSolarRad;
                            SurfWinQRadSWwinAbsLayer(Lay, SurfNum) = SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                            SurfWinADiffFront(Lay, SurfNum) = cplxState.WinSkyGndAbs(Lay);
                        }
                    } else {
                        for (size_t Lay = 1; Lay <= numOfLayers; ++Lay) {
                            auto AbWinBeam = aLayer->getAbsorptanceLayer(Lay, Side::Front, ScatteringSimple::Direct, Theta, Phi) *
                                             window.OutProjSLFracMult(HourOfDay);
                            auto AbWinDiffFront = aLayer->getAbsorptanceLayer(Lay, Side::Front, ScatteringSimple::Diffuse, Theta, Phi);
                            auto AbWinDiffBack = aLayer->getAbsorptanceLayer(Lay, Side::Back, ScatteringSimple::Diffuse, Theta, Phi);

                            // Simon: This should not be multiplied with cosine of incident angle. This however gives same
                            // results as BSDF and Winkelmann models.
                            SurfWinA(Lay, SurfNum) = AbWinBeam * CosInc * SunLitFract * SurfaceWindow(SurfNum).OutProjSLFracMult(HourOfDay);
                            SurfWinADiffFront(Lay, SurfNum) = AbWinDiffFront;
                            SurfWinADiffBack(Lay, SurfNum) = AbWinDiffBack;

                            // Simon: Same not as for BSDF. Normal solar radiation should be taken here because angle of
                            // incidence is already taken into account
                            auto absBeam = SurfWinA(Lay, SurfNum) * BeamSolarRad;
                            auto absDiff = SurfWinADiffFront(Lay, SurfNum) * (SurfSkySolarInc(SurfNum2) + SurfGndSolarInc(SurfNum2));
                            SurfWinQRadSWwinAbs(Lay, SurfNum) = (absBeam + absDiff);
                            SurfWinQRadSWwinAbsLayer(Lay, SurfNum) = SurfWinQRadSWwinAbs(Lay, SurfNum) * Surface(SurfNum).Area;
                        }
                    }
                }

                ////////////////////////////////////////////////////////////////////
                // SKY AND GROUND DIFFUSE SOLAR GAIN INTO ZONE FROM EXTERIOR WINDOW
                ////////////////////////////////////////////////////////////////////
                Real64 Tdiff = aLayer->getPropertySimple(PropertySimple::T, Side::Front, Scattering::DiffuseDiffuse, Theta, Phi);
                state.dataConstruction->Construct(ConstrNum).TransDiff = Tdiff;
                Real64 EnclSolDSWin = SurfSkySolarInc(SurfNum2) * Tdiff * Surface(SurfNum2).Area;
                if ((DifSolarRad != 0)) {
                    EnclSolDSWin /= DifSolarRad;
                } else {
                    EnclSolDSWin /= 1e-8;
                }

                Real64 DGZoneWin = SurfGndSolarInc(SurfNum2) * Tdiff * Surface(SurfNum2).Area;
                (GndSolarRad != 0) ? DGZoneWin /= GndSolarRad : DGZoneWin /= 1e-8;

                EnclSolDS(enclosureNum) = EnclSolDSWin;
                DGZone(enclosureNum) = DGZoneWin;

                ////////////////////////////////////////////////////////////////////
                // BEAM SOLAR ON EXTERIOR WINDOW TRANSMITTED AS BEAM AND/OR DIFFUSE
                ////////////////////////////////////////////////////////////////////
                Real64 TBmBm = aLayer->getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDirect, Theta, Phi);
                Real64 TBmDif = aLayer->getPropertySimple(PropertySimple::T, Side::Front, Scattering::DirectDiffuse, Theta, Phi);
                Real64 WinTransBmBmSolar = TBmBm * SunLitFract * CosInc * Surface(SurfNum).Area * window.InOutProjSLFracMult(HourOfDay);
                Real64 WinTransBmDifSolar = TBmDif * SunLitFract * CosInc * Surface(SurfNum).Area * window.InOutProjSLFracMult(HourOfDay);
                BTOTZone += WinTransBmBmSolar + WinTransBmDifSolar;

                Real64 DifSolarRadiation = SurfSkySolarInc(SurfNum2) + SurfGndSolarInc(SurfNum2);
                SurfWinBmSolar(SurfNum) = BeamSolarRad * (TBmBm + TBmDif) * Surface(SurfNum).Area * CosInc;
                SurfWinDifSolar(SurfNum) = DifSolarRadiation * Tdiff * Surface(SurfNum).Area;
                SurfWinBmSolarEnergy(SurfNum) = SurfWinBmSolar(SurfNum) * TimeStepZoneSec;
                SurfWinDifSolarEnergy(SurfNum) = SurfWinDifSolar(SurfNum) * TimeStepZoneSec;
                SurfWinTransSolar(SurfNum) = SurfWinBmSolar(SurfNum) + SurfWinDifSolar(SurfNum);
                SurfWinTransSolarEnergy(SurfNum) = SurfWinTransSolar(SurfNum) * TimeStepZoneSec;

                // Add beam solar absorbed by outside reveal to outside of window's base surface.
                // Add beam solar absorbed by inside reveal to inside of window's base surface.
                // This ignores 2-D heat transfer effects.
                int BaseSurfNum = Surface(SurfNum).BaseSurf;
                SurfOpaqAI(BaseSurfNum) = SurfWinBmSolAbsdInsReveal(SurfNum2) / Surface(BaseSurfNum).Area;
                SurfOpaqAO(BaseSurfNum) = SurfWinBmSolAbsdOutsReveal(SurfNum2) / Surface(BaseSurfNum).Area;

                ////////////////////////////////////////////////////////////////////
                // BEAM SOLAR ON EXTERIOR WINDOW TRANSMITTED AS BEAM AND/OR DIFFUSE
                ////////////////////////////////////////////////////////////////////
                Real64 TBm = TBmBm;
                // Correction for beam absorbed by inside reveal
                Real64 TBmDenom = SunLitFract * CosInc * Surface(SurfNum).Area * window.InOutProjSLFracMult(HourOfDay);
                if (TBmDenom != 0.0) { // when =0.0, no correction
                    TBm -= SurfWinBmSolAbsdInsReveal(SurfNum) / TBmDenom;
                }

                TBm = max(0.0, TBm);

                int NumOfBackSurf = ShadowComb(BaseSurfNum).NumBackSurf;

                if (SolarDistribution == FullInteriorExterior) {
                    for (int IBack = 1; IBack <= NumOfBackSurf; ++IBack) {

                        int const BackSurfNum = BackSurfaces(TimeStep, HourOfDay, IBack, SurfNum);

                        if (BackSurfNum == 0) break; // No more irradiated back surfaces for this exterior window
                        int ConstrNumBack = Surface(BackSurfNum).Construction;
                        // NBackGlass = Construct( ConstrNumBack ).TotGlassLayers;
                        // Irradiated (overlap) area for this back surface, projected onto window plane
                        // (includes effect of shadowing on exterior window)
                        Real64 AOverlap = OverlapAreas(TimeStep, HourOfDay, IBack, SurfNum);
                        Real64 BOverlap = TBm * AOverlap * CosInc; //[m2]

                        if (state.dataConstruction->Construct(ConstrNumBack).TransDiff <= 0.0) {
                            // Back surface is opaque interior or exterior wall

                            Real64 AbsIntSurf = state.dataConstruction->Construct(ConstrNumBack).InsideAbsorpSolar;

                            // Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                            // Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                            // HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                            Real64 HMovInsul = 0.0;
                            Real64 AbsInt = 0;
                            if (Surface(BackSurfNum).MaterialMovInsulInt > 0) {
                                Real64 MovInsulSchedVal = GetCurrentScheduleValue(Surface(BackSurfNum).SchedMovInsulInt);
                                if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
                                    HMovInsul = 0.0;
                                } else { // Movable insulation present
                                    HMovInsul = 1.0 / (MovInsulSchedVal * dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).Resistance);
                                    AbsInt = dataMaterial.Material(Surface(BackSurfNum).MaterialMovInsulInt).AbsorpSolar;
                                }
                            }
                            if (HMovInsul > 0.0) AbsIntSurf = AbsInt; // Movable inside insulation present

                            SurfOpaqAI(BackSurfNum) += BOverlap * AbsIntSurf / Surface(BackSurfNum).Area; //[-]
                            BABSZone += BOverlap * AbsIntSurf;                                        //[m2]
                        }
                    }
                } else {
                    for (int const FloorNum : thisEnclosure.SurfacePtr) {
                        // In following, ISABSF is zero except for nominal floor surfaces
                        if (!Surface(FloorNum).HeatTransSurf) continue;
                        if (state.dataSolarShading->ISABSF(FloorNum) <= 0.0 || FloorNum == SurfNum) continue; // Keep only floor surfaces

                        Real64 BTOTWinZone = TBm * SunLitFract * Surface(SurfNum).Area * CosInc * window.InOutProjSLFracMult(HourOfDay); //[m2]

                        if (state.dataConstruction->Construct(Surface(FloorNum).Construction).TransDiff <= 0.0) {
                            // Opaque surface
                            SurfOpaqAI(FloorNum) += BTOTWinZone * state.dataSolarShading->ISABSF(FloorNum) / Surface(FloorNum).Area; //[-]
                        }
                    }
                }
                ZoneTransSolar(enclosureNum) += SurfWinTransSolar(SurfNum);                         //[W]
                ZoneTransSolarEnergy(enclosureNum) = ZoneTransSolar(enclosureNum) * TimeStepZoneSec; //[J]
                ZoneBmSolFrExtWinsRep(enclosureNum) += SurfWinBmSolar(SurfNum);
                ZoneDifSolFrExtWinsRep(enclosureNum) += SurfWinDifSolar(SurfNum);
                ZoneBmSolFrExtWinsRepEnergy(enclosureNum) = ZoneBmSolFrExtWinsRep(enclosureNum) * TimeStepZoneSec;   //[J]
                ZoneDifSolFrExtWinsRepEnergy(enclosureNum) = ZoneDifSolFrExtWinsRep(enclosureNum) * TimeStepZoneSec; //[J]
            }
            DBZone(enclosureNum) = BTOTZone - BABSZone;
        }
    }

    int WindowScheduledSolarAbs(int const SurfNum, // Surface number
                                int const ConstNum // Construction number
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   June 2013

        // PURPOSE OF THIS SUBROUTINE:
        // Returns scheduled surface gain object for given surface-construction combination

        // Return value
        int WindowScheduledSolarAbs;

        WindowScheduledSolarAbs = 0;

        for (int i = 1; i <= TotFenLayAbsSSG; ++i) {
            if ((FenLayAbsSSG(i).SurfPtr == SurfNum) && (FenLayAbsSSG(i).ConstrPtr == ConstNum)) {
                WindowScheduledSolarAbs = i;
                return WindowScheduledSolarAbs;
            }
        }

        return WindowScheduledSolarAbs;
    }

    int SurfaceScheduledSolarInc(int const SurfNum, // Surface number
                                 int const ConstNum // Construction number
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   June 2013

        // PURPOSE OF THIS SUBROUTINE:
        // Returns scheduled surface gain pointer for given surface-construction combination

        // Return value
        int SurfaceScheduledSolarInc;

        SurfaceScheduledSolarInc = 0;

        for (int i = 1; i <= TotSurfIncSolSSG; ++i) {
            if ((SurfIncSolSSG(i).SurfPtr == SurfNum) && (SurfIncSolSSG(i).ConstrPtr == ConstNum)) {
                SurfaceScheduledSolarInc = i;
                return SurfaceScheduledSolarInc;
            }
        }

        return SurfaceScheduledSolarInc;
    }

    void PerformSolarCalculations(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda K. Lawrie
        //       DATE WRITTEN   July 1999
        //       MODIFIED       Sept 2003, FCW: add calls to CalcBeamSolDiffuseReflFactors and
        //                       CalcBeamSolSpecularReflFactors
        //                      Jan 2004, FCW: call CalcDayltgCoefficients if storm window status on
        //                       any window has changed
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines if new solar/shading calculations need
        // to be performed and calls the proper routines to do the job.

        // METHODOLOGY EMPLOYED:
        // Users are allowed to enter a value for number of days in each period that
        // will be used for calculating solar.  (Later, this could be more complicated as
        // in allowing a number of days in a month or something).  Using this value or the
        // default (20 days) if nothing is entered by the user, the routine will use the
        // number of days left to determine if a new set of calculations should be done.
        // The calculations use the average of "equation of time" and "solar declination"
        // to perform the calculations.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataSystemVariables::DetailedSolarTimestepIntegration;
        using DaylightingManager::CalcDayltgCoefficients;
        using DaylightingManager::TotWindowsWithDayl;

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
        Real64 SumDec;
        Real64 SumET;
        Real64 AvgEqOfTime;
        Real64 AvgSinSolarDeclin;
        Real64 AvgCosSolarDeclin;
        int PerDayOfYear;
        int Count;
        Real64 SinDec;
        Real64 EqTime;
        // not used INTEGER SurfNum

        // Calculate sky diffuse shading

        if (BeginSimFlag) {
            state.dataSolarShading->CalcSkyDifShading = true;
            SkyDifSolarShading(state); // Calculate factors for shading of sky diffuse solar
            state.dataSolarShading->CalcSkyDifShading = false;
        }

        if (BeginEnvrnFlag) {
            state.dataSolarShading->ShadowingDaysLeft = 0;
        }

        if (state.dataSolarShading->ShadowingDaysLeft <= 0 || DetailedSolarTimestepIntegration) {

            if (!DetailedSolarTimestepIntegration) {
                //  Perform calculations.
                state.dataSolarShading->ShadowingDaysLeft = state.dataSolarShading->ShadowingCalcFrequency;
                if (DayOfSim + state.dataSolarShading->ShadowingDaysLeft > NumOfDayInEnvrn) {
                    state.dataSolarShading->ShadowingDaysLeft = NumOfDayInEnvrn - DayOfSim + 1;
                }

                //  Calculate average Equation of Time, Declination Angle for this period

                if (!WarmupFlag) {
                    if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) {
                        DisplayString("Updating Shadowing Calculations, Start Date=" + CurMnDyYr);
                    } else {
                        DisplayString("Updating Shadowing Calculations, Start Date=" + CurMnDy);
                    }
                    DisplayPerfSimulationFlag = true;
                }

                PerDayOfYear = DayOfYear;
                SumDec = 0.0;
                SumET = 0.0;
                for (Count = 1; Count <= state.dataSolarShading->ShadowingDaysLeft; ++Count) {
                    SUN3(PerDayOfYear, SinDec, EqTime);
                    SumDec += SinDec;
                    SumET += EqTime;
                    ++PerDayOfYear;
                }

                //  Compute Period Values
                AvgSinSolarDeclin = SumDec / double(state.dataSolarShading->ShadowingDaysLeft);
                AvgCosSolarDeclin = std::sqrt(1.0 - pow_2(AvgSinSolarDeclin));
                AvgEqOfTime = SumET / double(state.dataSolarShading->ShadowingDaysLeft);
            } else {
                SUN3(DayOfYear, AvgSinSolarDeclin, AvgEqOfTime);
                AvgCosSolarDeclin = std::sqrt(1.0 - pow_2(AvgSinSolarDeclin));
                // trigger display of progress in the simulation every two weeks
                if (!WarmupFlag && BeginDayFlag && (DayOfSim % 14 == 0)) {
                    DisplayPerfSimulationFlag = true;
                }
            }

            CalcPerSolarBeam(state, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin);

            // Calculate factors for solar reflection
            if (CalcSolRefl) {
                CalcBeamSolDiffuseReflFactors(state);
                CalcBeamSolSpecularReflFactors(state);
                if (BeginSimFlag) CalcSkySolDiffuseReflFactors(state);
            }

            //  Calculate daylighting coefficients
            CalcDayltgCoefficients(state);
        }

        if (!WarmupFlag) {
            --state.dataSolarShading->ShadowingDaysLeft;
        }

        // Recalculate daylighting coefficients if storm window has been added
        // or removed from one or more windows at beginning of day
        if (TotWindowsWithDayl > 0 && !BeginSimFlag && !BeginEnvrnFlag && !WarmupFlag && TotStormWin > 0 && StormWinChangeThisDay) {
            CalcDayltgCoefficients(state);
        }
    }

    void SHDRVL(EnergyPlusData &state, int const HTSS,  // Heat transfer surface number of the subsurface
                int const SBSNR, // Subsurface number
                int const Hour,
                int const TS)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       May 2002 (FCW): allow triangular windows to have reveal.
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes the shadowing from a reveal onto a subsurface.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        int NVS; // Number of verticies

        int const None(0);                       // for use with RevealStatus
        int const EntireWindowShadedByReveal(1); // for use with RevealStatus
        int const WindowShadedOnlyByReveal(2);   // for use with RevealStatus

        Real64 A; // Area
        Real64 R; // Depth of the reveal (m)
        int I;    // Loop control
        int N;    // Vertex number
        int NS1;  // Locations in homogeneous coordinate array
        int NS2;
        // note, below dimensions not changed because subsurface still max 4
        Array1D<Real64> XVT(5); // Projected X coordinates of vertices
        Array1D<Real64> YVT(5); // Projected Y coordinates of vertices
        bool RevealStatusSet;   // Used to control flow through this subroutine.
        // Certain operations performed only if reveal status not yet set.
        int RevealStatus; // Status of the reveal, takes the parameter values above

        // FLOW:
        RevealStatus = None;
        RevealStatusSet = false;

        if (!state.dataSolarShading->CalcSkyDifShading) {
            state.dataSolarShading->WindowRevealStatus(TS, Hour, SBSNR) = None;
        }

        R = Surface(SBSNR).Reveal;
        if (R <= 0.0) {
            RevealStatus = None;
            RevealStatusSet = true;
        }

        if (!RevealStatusSet) {

            state.dataSolarShading->FRVLHC = state.dataSolarShading->LOCHCA + 1;
            ++state.dataSolarShading->LOCHCA;
            NVS = Surface(SBSNR).Sides;

            // Currently (06May02) windows are either rectangles (NVS=4) or triangles (NVS=3)

            if (NVS == 4) { // Rectangular subsurface

                // Determine vertices of reveal.
                // Project the subsurface up to the plane of the wall.

                XVT(1) = ShadeV(SBSNR).XV(1) + R * max(state.dataSolarShading->XShadowProjection, 0.0);
                XVT(2) = ShadeV(SBSNR).XV(2) + R * max(state.dataSolarShading->XShadowProjection, 0.0);
                XVT(3) = ShadeV(SBSNR).XV(3) + R * min(state.dataSolarShading->XShadowProjection, 0.0);
                XVT(4) = ShadeV(SBSNR).XV(4) + R * min(state.dataSolarShading->XShadowProjection, 0.0);
                YVT(1) = ShadeV(SBSNR).YV(1) + R * min(state.dataSolarShading->YShadowProjection, 0.0);
                YVT(2) = ShadeV(SBSNR).YV(2) + R * max(state.dataSolarShading->YShadowProjection, 0.0);
                YVT(3) = ShadeV(SBSNR).YV(3) + R * max(state.dataSolarShading->YShadowProjection, 0.0);
                YVT(4) = ShadeV(SBSNR).YV(4) + R * min(state.dataSolarShading->YShadowProjection, 0.0);

                // Check for complete shadowing.

                if ((XVT(2) >= XVT(3)) || (YVT(2) >= YVT(1))) {

                    RevealStatus = EntireWindowShadedByReveal;
                    RevealStatusSet = true;

                } else {
                    // Re-order vertices to clockwise.

                    for (N = 1; N <= NVS; ++N) {
                        state.dataSolarShading->XVS(N) = XVT(NVS + 1 - N);
                        state.dataSolarShading->YVS(N) = YVT(NVS + 1 - N);
                    }

                    // Transform to homogeneous coordinates

                    HTRANS1(state, state.dataSolarShading->FRVLHC, NVS);
                    state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC) = -state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC);
                    state.dataSolarShading->HCT(state.dataSolarShading->FRVLHC) = 1.0;

                    if (state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC) <= 0.0) {
                        RevealStatus = EntireWindowShadedByReveal;
                        RevealStatusSet = true;
                    }
                }

            } else if (NVS == 3) { // Triangular window

                // Project window to outside plane of parent surface

                for (N = 1; N <= 3; ++N) {
                    XVT(N) = ShadeV(SBSNR).XV(N) + R * state.dataSolarShading->XShadowProjection;
                    YVT(N) = ShadeV(SBSNR).YV(N) + R * state.dataSolarShading->YShadowProjection;
                }

                // Find the overlap between the original window and the projected window
                // Put XVT,YVT in clockwise order

                for (N = 1; N <= NVS; ++N) {
                    state.dataSolarShading->XVS(N) = XVT(NVS + 1 - N);
                    state.dataSolarShading->YVS(N) = YVT(NVS + 1 - N);
                }

                // Transform to homogeneous coordinates

                NS1 = state.dataSolarShading->LOCHCA + 1;
                state.dataSolarShading->LOCHCA = NS1;
                HTRANS1(state, NS1, NVS);

                // Put XV,YV in clockwise order

                for (N = 1; N <= NVS; ++N) {
                    state.dataSolarShading->XVS(N) = ShadeV(SBSNR).XV(NVS + 1 - N);
                    state.dataSolarShading->YVS(N) = ShadeV(SBSNR).YV(NVS + 1 - N);
                }

                // Transform to homogenous coordinates

                NS2 = state.dataSolarShading->LOCHCA + 1;
                state.dataSolarShading->LOCHCA = NS2;
                HTRANS1(state, NS2, NVS);
                state.dataSolarShading->HCT(state.dataSolarShading->FRVLHC) = 1.0;

                // Find overlap

                DeterminePolygonOverlap(state, NS1, NS2, state.dataSolarShading->FRVLHC);
                if (state.dataSolarShading->OverlapStatus == state.dataSolarShading->NoOverlap) {
                    RevealStatus = EntireWindowShadedByReveal;
                    RevealStatusSet = true;
                }
            }
        }

        if (!RevealStatusSet) {

            // Check for no shadows on window.

            if (state.dataSolarShading->NSBSHC <= 1) {
                RevealStatus = WindowShadedOnlyByReveal;
                RevealStatusSet = true;
            } else {
                // Reduce all previous shadows to size of reveal opening.
                state.dataSolarShading->LOCHCA = state.dataSolarShading->FRVLHC;
                MULTOL(state, state.dataSolarShading->LOCHCA, state.dataSolarShading->FSBSHC, state.dataSolarShading->NSBSHC - 1);
                if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) {
                    RevealStatus = None;
                    RevealStatusSet = true;
                } else {
                    state.dataSolarShading->NRVLHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FRVLHC + 1;
                    if (state.dataSolarShading->NRVLHC <= 1) {
                        RevealStatus = WindowShadedOnlyByReveal;
                        RevealStatusSet = true;
                    }
                }
            }
        }

        if (!RevealStatusSet) {
            // Compute sunlit area.
            A = state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC);
            for (I = 2; I <= state.dataSolarShading->NRVLHC; ++I) {
                A += state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC - 1 + I) * (1.0 - state.dataSolarShading->HCT(state.dataSolarShading->FRVLHC - 1 + I));
            }
            state.dataSolarShading->SAREA(HTSS) = A;
        }

        if ((RevealStatus == EntireWindowShadedByReveal) || (state.dataSolarShading->SAREA(HTSS) < 0.0)) {
            state.dataSolarShading->SAREA(HTSS) = 0.0; // Window entirely shaded by reveal.
        } else if (RevealStatus == WindowShadedOnlyByReveal) {
            state.dataSolarShading->SAREA(HTSS) = state.dataSolarShading->HCAREA(state.dataSolarShading->FRVLHC); // Window shaded only by reveal.
        }

        if (!state.dataSolarShading->CalcSkyDifShading) {
            state.dataSolarShading->WindowRevealStatus(TS, Hour, SBSNR) = RevealStatus;
        }
    }

    void SHDSBS(EnergyPlusData &state,
                int const iHour, // Hour Index
                int const CurSurf,
                int const NBKS, // Number of back surfaces
                int const NSBS, // Number of subsurfaces
                int const HTS,  // Heat transfer surface number of the general receiving surf
                int const TS    // Time step Index
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       FCW, Oct 2002: Surface%Area --> Surface%Area + SurfaceWindow%DividerArea
        //                       in calculation of SunlitFracWithoutReveal (i.e., use full window area, not
        //                       just glass area.
        //                      TH, May 2009: Bug fixed to address part of CR 7596 - inside reveals
        //                       causing high cooling loads
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine determines the shadowing on subsurfaces and
        // revises the base surface area accordingly.  It also computes
        // the effect of transparent subsurfaces.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        Real64 A;        // Area
        int I;           // Loop control
        int J;           // Loop control
        int K;           // Window construction number
        int N;           // Vertex number
        Real64 SurfArea; // Surface area. For walls, includes all window frame areas.
        // For windows, includes divider area
        //  REAL(r64) FrameAreaAdd    ! Additional frame area sunlit
        //  REAL(r64) DividerAreaAdd  ! Additional frame area sunlit
        int HTSS;  // Heat transfer surface number of the subsurface
        int SBSNR; // Subsurface number

        if (NSBS > 0) { // Action taken only if subsurfaces present

            state.dataSolarShading->FSBSHC = state.dataSolarShading->LOCHCA + 1;

            for (I = 1; I <= NSBS; ++I) { // Do for all subsurfaces (sbs).

                SBSNR = ShadowComb(CurSurf).SubSurf(I);

                HTSS = SBSNR;

                K = Surface(SBSNR).Construction;

                if (!state.dataSolarShading->penumbra) {
                    if ((state.dataSolarShading->OverlapStatus != state.dataSolarShading->TooManyVertices) && (state.dataSolarShading->OverlapStatus != state.dataSolarShading->TooManyFigures) && (state.dataSolarShading->SAREA(HTS) > 0.0)) {

                        // Re-order vertices to clockwise sequential; compute homogeneous coordinates.
                        state.dataSolarShading->NVS = Surface(SBSNR).Sides;
                        for (N = 1; N <= state.dataSolarShading->NVS; ++N) {
                            state.dataSolarShading->XVS(N) = ShadeV(SBSNR).XV(state.dataSolarShading->NVS + 1 - N);
                            state.dataSolarShading->YVS(N) = ShadeV(SBSNR).YV(state.dataSolarShading->NVS + 1 - N);
                        }
                        state.dataSolarShading->LOCHCA = state.dataSolarShading->FSBSHC;
                        HTRANS1(state, state.dataSolarShading->LOCHCA, state.dataSolarShading->NVS);
                        state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA) = -state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA);
                        state.dataSolarShading->HCT(state.dataSolarShading->LOCHCA) = 1.0;
                        state.dataSolarShading->NSBSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FSBSHC + 1;

                        // Determine sunlit area of subsurface due to shadows on general receiving surface.
                        if (state.dataSolarShading->NGSSHC > 0) {
                            MULTOL(state, state.dataSolarShading->LOCHCA, state.dataSolarShading->FGSSHC - 1, state.dataSolarShading->NGSSHC);
                            if ((state.dataSolarShading->OverlapStatus != state.dataSolarShading->TooManyVertices) && (state.dataSolarShading->OverlapStatus != state.dataSolarShading->TooManyFigures)) state.dataSolarShading->NSBSHC = state.dataSolarShading->LOCHCA - state.dataSolarShading->FSBSHC + 1;
                        }
                    }

                    if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures) ||
                        (state.dataSolarShading->SAREA(HTS) <= 0.0)) { // General receiving surface totally shaded.

                        state.dataSolarShading->SAREA(HTSS) = 0.0;

                        if (iHour > 0 && TS > 0) SunlitFracWithoutReveal(TS, iHour, HTSS) = 0.0;

                    } else if ((state.dataSolarShading->NGSSHC <= 0) || (state.dataSolarShading->NSBSHC == 1)) { // No shadows.

                        state.dataSolarShading->SAREA(HTSS) = state.dataSolarShading->HCAREA(state.dataSolarShading->FSBSHC);
                        state.dataSolarShading->SAREA(HTS) -= state.dataSolarShading->SAREA(HTSS); // Revise sunlit area of general receiving surface.

                        // TH. This is a bug.  SunLitFracWithoutReveal should be a ratio of area
                        // IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = &
                        //      Surface(HTSS)%NetAreaShadowCalc

                        // new code fixed part of CR 7596. TH 5/29/2009
                        if (iHour > 0 && TS > 0) SunlitFracWithoutReveal(TS, iHour, HTSS) = state.dataSolarShading->SAREA(HTSS) / Surface(HTSS).NetAreaShadowCalc;

                        SHDRVL(state, HTSS, SBSNR, iHour, TS); // Determine shadowing from reveal.

                        if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) state.dataSolarShading->SAREA(HTSS) = 0.0;

                    } else { // Compute area.

                        A = state.dataSolarShading->HCAREA(state.dataSolarShading->FSBSHC);
                        for (J = 2; J <= state.dataSolarShading->NSBSHC; ++J) {
                            A += state.dataSolarShading->HCAREA(state.dataSolarShading->FSBSHC - 1 + J) * (1.0 - state.dataSolarShading->HCT(state.dataSolarShading->FSBSHC - 1 + J));
                        }
                        state.dataSolarShading->SAREA(HTSS) = A;
                        if (state.dataSolarShading->SAREA(HTSS) > 0.0) {

                            state.dataSolarShading->SAREA(HTS) -= state.dataSolarShading->SAREA(HTSS); // Revise sunlit area of general receiving surface.

                            if (iHour > 0 && TS > 0) SunlitFracWithoutReveal(TS, iHour, HTSS) = state.dataSolarShading->SAREA(HTSS) / Surface(HTSS).Area;

                            SHDRVL(state, HTSS, SBSNR, iHour, TS); // Determine shadowing from reveal.

                            if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) state.dataSolarShading->SAREA(HTSS) = 0.0;

                        } else { // General receiving surface totally shaded.

                            state.dataSolarShading->SAREA(HTSS) = 0.0;
                        }
                    }
                }

                // Determine transmittance and absorptances of sunlit window.
                if (state.dataConstruction->Construct(K).TransDiff > 0.0) {

                    if (!state.dataSolarShading->CalcSkyDifShading) { // Overlaps calculation is only done for beam solar
                        // shading, not for sky diffuse solar shading

                        CalcInteriorSolarOverlaps(state, iHour, NBKS, HTSS, CurSurf, TS);
                    }
                }

                // Error checking.
                SurfArea = Surface(SBSNR).NetAreaShadowCalc;
                state.dataSolarShading->SAREA(HTSS) = max(0.0, state.dataSolarShading->SAREA(HTSS));

                state.dataSolarShading->SAREA(HTSS) = min(state.dataSolarShading->SAREA(HTSS), SurfArea);

            } // End of subsurface loop
        }
    }

    void SUN3(int const JulianDayOfYear,      // Julian Day Of Year
              Real64 &SineOfSolarDeclination, // Sine of Solar Declination
              Real64 &EquationOfTime          // Equation of Time (Degrees)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Linda K. Lawrie

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes the coefficients for determining
        // the solar position.

        // METHODOLOGY EMPLOYED:
        // The expressions are based on least-squares fits of data on p.316 of 'Thermal
        // Environmental Engineering' by Threlkeld and on p.387 of the ASHRAE Handbook
        // of Fundamentals (need date of ASHRAE HOF).

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static Array1D<Real64> const SineSolDeclCoef(9,
                                                     {0.00561800,
                                                      0.0657911,
                                                      -0.392779,
                                                      0.00064440,
                                                      -0.00618495,
                                                      -0.00010101,
                                                      -0.00007951,
                                                      -0.00011691,
                                                      0.00002096}); // Fitted coefficients of Fourier series | SINE OF DECLINATION | COEFFICIENTS
        static Array1D<Real64> const EqOfTimeCoef(9,
                                                  {0.00021971,
                                                   -0.122649,
                                                   0.00762856,
                                                   -0.156308,
                                                   -0.0530028,
                                                   -0.00388702,
                                                   -0.00123978,
                                                   -0.00270502,
                                                   -0.00167992}); // Fitted coefficients of Fourier Series | EQUATION OF TIME | COEFFICIENTS

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 X;     // Day of Year in Radians (Computed from Input JulianDayOfYear)
        Real64 CosX;  // COS(X)
        Real64 SineX; // SIN(X)

        X = 0.017167 * JulianDayOfYear; // Convert julian date to angle X

        // Calculate sines and cosines of X
        SineX = std::sin(X);
        CosX = std::cos(X);

        SineOfSolarDeclination = SineSolDeclCoef(1) + SineSolDeclCoef(2) * SineX + SineSolDeclCoef(3) * CosX +
                                 SineSolDeclCoef(4) * (SineX * CosX * 2.0) + SineSolDeclCoef(5) * (pow_2(CosX) - pow_2(SineX)) +
                                 SineSolDeclCoef(6) * (SineX * (pow_2(CosX) - pow_2(SineX)) + CosX * (SineX * CosX * 2.0)) +
                                 SineSolDeclCoef(7) * (CosX * (pow_2(CosX) - pow_2(SineX)) - SineX * (SineX * CosX * 2.0)) +
                                 SineSolDeclCoef(8) * (2.0 * (SineX * CosX * 2.0) * (pow_2(CosX) - pow_2(SineX))) +
                                 SineSolDeclCoef(9) * (pow_2(pow_2(CosX) - pow_2(SineX)) - pow_2(SineX * CosX * 2.0));

        EquationOfTime = EqOfTimeCoef(1) + EqOfTimeCoef(2) * SineX + EqOfTimeCoef(3) * CosX + EqOfTimeCoef(4) * (SineX * CosX * 2.0) +
                         EqOfTimeCoef(5) * (pow_2(CosX) - pow_2(SineX)) +
                         EqOfTimeCoef(6) * (SineX * (pow_2(CosX) - pow_2(SineX)) + CosX * (SineX * CosX * 2.0)) +
                         EqOfTimeCoef(7) * (CosX * (pow_2(CosX) - pow_2(SineX)) - SineX * (SineX * CosX * 2.0)) +
                         EqOfTimeCoef(8) * (2.0 * (SineX * CosX * 2.0) * (pow_2(CosX) - pow_2(SineX))) +
                         EqOfTimeCoef(9) * (pow_2(pow_2(CosX) - pow_2(SineX)) - pow_2(SineX * CosX * 2.0));
    }

    void SUN4(EnergyPlusData &state, Real64 const CurrentTime,    // Time to use in shadowing calculations
              Real64 const EqOfTime,       // Equation of time for current day
              Real64 const SinSolarDeclin, // Sine of the Solar declination (current day)
              Real64 const CosSolarDeclin  // Cosine of the Solar declination (current day)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Legacy Code
        //       DATE WRITTEN
        //       MODIFIED       na
        //       RE-ENGINEERED  Lawrie, Oct 2000

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine computes solar direction cosines for a given hour.  These
        // cosines are used in the shadowing calculations.

        // REFERENCES:
        // BLAST/IBLAST code, original author George Walton

        Real64 H;       // Hour angle (before noon = +) (in radians)
        Real64 HrAngle; // Basic hour angle

        // Compute the hour angle
        HrAngle = (15.0 * (12.0 - (CurrentTime + EqOfTime)) + (TimeZoneMeridian - Longitude));
        H = HrAngle * DataGlobalConstants::DegToRadians();

        // Compute the cosine of the solar zenith angle.
        state.dataSolarShading->SUNCOS(3) = SinSolarDeclin * SinLatitude + CosSolarDeclin * CosLatitude * std::cos(H);
        state.dataSolarShading->SUNCOS(2) = 0.0;
        state.dataSolarShading->SUNCOS(1) = 0.0;

        if (state.dataSolarShading->SUNCOS(3) < SunIsUpValue) return; // Return if sun not above horizon.

        // Compute other direction cosines.
        state.dataSolarShading->SUNCOS(2) = SinSolarDeclin * CosLatitude - CosSolarDeclin * SinLatitude * std::cos(H);
        state.dataSolarShading->SUNCOS(1) = CosSolarDeclin * std::sin(H);
    }

    void WindowShadingManager(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   December 1998
        //       MODIFIED       November 1999 (FW)
        //                      Aug 2001 (FW): change shading control names, change approach
        //                       to scheduling and glare control, add movable
        //                       insulation controls (mainly for heating reduction)
        //                      Dec 2001 (FW): add slat angle control for blinds
        //                      Aug 2002 (FW): add four new control types:
        //                        OnIfHighOutsideAirTempAndHighSolarOnWindow
        //                        OnIfHighOutsideAirTempAndHighHorizontalSolar
        //                        OnIfHighZoneAirTempAndHighSolarOnWindow
        //                        OnIfHighZoneAirTempAndHighHorizontalSolar
        //                      Dec 2002 (FW): add between-glass shade/blind
        //                      Mar 2003 (FW): allow GlareControlIsActive = .TRUE. only for daylit zones
        //                      Apr 2003 (FW): use SNLoadCoolRate or SNLoadHeatRate only if not first time step
        //                                     (fixes problem when used first time thru and not allocated)
        //                      May 2006 (RR): add exterior window screen
        //                      May 2009 (BG): add EMS actuator override for shade flag and slat angle
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For windows with shading, selects the shaded construction
        // that is used in the heat balance calculation, and sets
        // the window shading flag, which is:
        //  -1: if window has no shading device
        //   0: if shading device is off
        //   1: if interior shade is on
        //   2: if glazing is switched to darker state
        //   3: if exterior shade is on
        //   6: if interior blind is on
        //   7: if exterior blind is on
        //   8: if between-glass shade is on
        //   9: if between-glass blind is on
        //  10: window has interior shade that is off but may be triggered on later
        //       to control daylight glare
        //  20: window has switchable glazing that is unswitched but may be switched later
        //       to control daylight glare or daylight illuminance
        //  30: window has exterior shade that is off but may be triggered on later
        //       to control daylight glare or daylight illuminance
        //  60: window has interior blind that is off but may be triggered on later
        //       to control daylight glare or daylight illuminance
        //  70: window has exterior blind that is off but may be triggered on later
        //       to control daylight glare or daylight illuminance
        //  80: window has between-glass shade that is off but may be triggered on later
        //       to control daylight glare or daylight illuminance
        //  90: window has between-glass blind that is off but may be triggered on later
        //       to control daylight glare or daylight illuminance
        // A "shading device" may be an exterior, interior or between-glass shade or blind,
        // or the lower-transmitting (dark) state of switchable glazing (e.g., electrochromic).
        // In all cases, the unshaded condition is represented
        // by the construction given by window's Surface()%Construction and
        // the shaded condition is represented by the construction given by
        // the window's Surface()%ShadedConstruction
        // REFERENCES:
        // na

        // Using/Aliasing
        using DataDaylighting::ZoneDaylight;
        using DataHeatBalFanSys::MAT;
        using DataWindowEquivalentLayer::CFS;
        using General::POLYF;
        using ScheduleManager::GetCurrentScheduleValue;

        static Real64 ThetaBig(0.0);   // Larger of ThetaBlock1 and ThetaBlock2 	//Autodesk Used uninitialized in some runs
        static Real64 ThetaSmall(0.0); // Smaller of ThetaBlock1 and ThetaBlock2 //Autodesk Used uninitialized in some runs
        static Real64 ThetaMin(0.0);   // Minimum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs
        static Real64 ThetaMax(0.0);   // Maximum allowed slat angle, resp. (rad)  //Autodesk Used uninitialized in some runs
        int IConst; // Construction

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            for (int ISurf = firstSurfWin; ISurf <= lastSurfWin; ++ISurf) {
                SurfWinExtIntShadePrevTS(ISurf) = SurfWinShadingFlag(ISurf);

                SurfWinShadingFlag(ISurf) = NoShade;
                SurfWinFracTimeShadingDeviceOn(ISurf) = 0.0;
                if (SurfWinWindowModelType(ISurf) == WindowEQLModel) {
                    int EQLNum = state.dataConstruction->Construct(Surface(ISurf).Construction).EQLConsPtr;
                    if (CFS(EQLNum).VBLayerPtr > 0) {
                        if (CFS(EQLNum).L(CFS(EQLNum).VBLayerPtr).CNTRL == state.dataWindowEquivalentLayer->lscNONE) {
                            SurfWinSlatAngThisTSDeg(ISurf) = CFS(EQLNum).L(CFS(EQLNum).VBLayerPtr).PHI_DEG;
                        } else {
                            SurfWinSlatAngThisTSDeg(ISurf) = 0.0;
                        }
                    }
                }

                // Initialization of complex fenestration shading device
                if (SurfWinWindowModelType(ISurf) == WindowBSDFModel) {
                    auto &construction(state.dataConstruction->Construct(Surface(ISurf).Construction));
                    auto &surface_window(SurfaceWindow(ISurf));
                    int TotLayers = construction.TotLayers;
                    for (auto Lay = 1; Lay <= TotLayers; ++Lay) {
                        const int LayPtr = construction.LayerPoint(Lay);
                        auto &material(dataMaterial.Material(LayPtr));
                        const bool isShading = material.Group == ComplexWindowShade;
                        if (isShading && Lay == 1) SurfWinShadingFlag(ISurf) = ExtShadeOn;
                        if (isShading && Lay == TotLayers) SurfWinShadingFlag(ISurf) = IntShadeOn;
                    }
                    if (SurfWinShadingFlag(ISurf) == IntShadeOn) {
                        auto &construction(state.dataConstruction->Construct(Surface(ISurf).Construction));
                        const int TotLay = construction.TotLayers;
                        int ShadingLayerPtr = construction.LayerPoint(TotLay);
                        ShadingLayerPtr = dataMaterial.Material(ShadingLayerPtr).ComplexShadePtr;
                        auto &complexShade = ComplexShade(ShadingLayerPtr);
                        auto TauShadeIR = complexShade.IRTransmittance;
                        auto EpsShadeIR = complexShade.BackEmissivity;
                        auto RhoShadeIR = max(0.0, 1.0 - TauShadeIR - EpsShadeIR);
                        // Get properties of glass next to inside shading layer
                        int GlassLayPtr = construction.LayerPoint(TotLay - 2);
                        auto EpsGlassIR = dataMaterial.Material(GlassLayPtr).AbsorpThermalBack;
                        auto RhoGlassIR = 1 - EpsGlassIR;

                        auto EffShBlEmiss =
                                EpsShadeIR * (1.0 + RhoGlassIR * TauShadeIR / (1.0 - RhoGlassIR * RhoShadeIR));
                        surface_window.EffShBlindEmiss[0] = EffShBlEmiss;
                        auto EffGlEmiss = EpsGlassIR * TauShadeIR / (1.0 - RhoGlassIR * RhoShadeIR);
                        surface_window.EffGlassEmiss[0] = EffGlEmiss;
                    }
                }

                if (Surface(ISurf).Class != SurfaceClass::Window) continue;
                if (Surface(ISurf).ExtBoundCond != ExternalEnvironment) continue;
                if (!Surface(ISurf).HasShadeControl) continue;

                // Initialize switching factor (applicable only to switchable glazing) to unswitched
                SurfWinSwitchingFactor(ISurf) = 0.0;

                IConst = Surface(ISurf).Construction;
                // Vis trans at normal incidence of unswitched glass. Counting the GlazedFrac
                if (IConst > 0)
                    SurfWinVisTransSelected(ISurf) = POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef) *
                                                     SurfWinGlazedFrac(ISurf);


                // Window has shading control
                // select the active window shading control and corresponding contructions
                size_t indexWindowShadingControl = selectActiveWindowShadingControlIndex(ISurf);
                if (!Surface(ISurf).windowShadingControlList.empty() && indexWindowShadingControl <= Surface(ISurf).windowShadingControlList.size() - 1) {
                    Surface(ISurf).activeWindowShadingControl = Surface(ISurf).windowShadingControlList[indexWindowShadingControl];
                }
                if (!Surface(ISurf).shadedConstructionList.empty() && indexWindowShadingControl <= Surface(ISurf).shadedConstructionList.size() - 1) {
                    Surface(ISurf).activeShadedConstruction = Surface(ISurf).shadedConstructionList[indexWindowShadingControl];
                }
                if (!Surface(ISurf).shadedStormWinConstructionList.empty() && indexWindowShadingControl <= Surface(ISurf).shadedStormWinConstructionList.size() - 1) {
                    Surface(ISurf).activeStormWinShadedConstruction = Surface(ISurf).shadedStormWinConstructionList[indexWindowShadingControl];
                }
                int IShadingCtrl = Surface(ISurf).activeWindowShadingControl;

                int ShadingType = WindowShadingControl(IShadingCtrl).ShadingType; // Type of shading (interior shade, interior blind, etc.)
                SurfWinShadingFlag(ISurf) = ShadeOff; // Initialize shading flag to off

                int IZone = Surface(ISurf).Zone;
                // Setpoint for shading
                Real64 SetPoint = WindowShadingControl(IShadingCtrl).SetPoint; // Control setpoint
                Real64 SetPoint2 = WindowShadingControl(IShadingCtrl).SetPoint2; // Second control setpoint


                // ShType = NoShade           ! =-1 (see DataHeatBalance)
                // ShType = ShadeOff          ! =0
                int ShType;
                // 1 = interior shade is on,
                // 2 = glass is switched to dark state,
                // 3 = exterior shade is on,
                // 4 = exterior screen is on,
                // 6 = interior blind is on,
                // 7 = exterior blind is on,
                // 8 = between-glass shade is on,
                // 9 = between-glass blind is on.
                //  CHARACTER(len=32)  :: ShadingType     ! Type of shading (interior shade, interior blind, etc.)
                if (ShadingType == WSC_ST_InteriorShade) ShType = IntShadeOn;            // =1
                if (ShadingType == WSC_ST_SwitchableGlazing) ShType = SwitchableGlazing; // =2
                if (ShadingType == WSC_ST_ExteriorShade) ShType = ExtShadeOn;            // =3
                if (ShadingType == WSC_ST_ExteriorScreen) ShType = ExtScreenOn;          // =4
                if (ShadingType == WSC_ST_InteriorBlind) ShType = IntBlindOn;            // =6
                if (ShadingType == WSC_ST_ExteriorBlind) ShType = ExtBlindOn;            // =7
                if (ShadingType == WSC_ST_BetweenGlassShade) ShType = BGShadeOn;         // =8
                if (ShadingType == WSC_ST_BetweenGlassBlind) ShType = BGBlindOn;         // =9

                bool SchedAllowsControl = true; // True if control schedule is not specified or is specified and schedule value = 1
                int SchedulePtr = WindowShadingControl(IShadingCtrl).Schedule;
                if (SchedulePtr != 0) {
                    if (WindowShadingControl(IShadingCtrl).ShadingControlIsScheduled &&
                        GetCurrentScheduleValue(SchedulePtr) <= 0.0)
                        SchedAllowsControl = false;
                }

                Real64 GlareControlIsActive = (ZoneDaylight(IZone).TotalDaylRefPoints > 0 && SunIsUp &&
                                        WindowShadingControl(IShadingCtrl).GlareControlIsActive); // True if glare control is active

                Real64 SolarOnWindow = 0.0; // Direct plus diffuse solar intensity on window (W/m2)
                Real64 BeamSolarOnWindow = 0.0; // Direct solar intensity on window (W/m2)
                Real64 HorizSolar = 0.0; // Horizontal direct plus diffuse solar intensity
                if (SunIsUp) {
                    Real64 SkySolarOnWindow = AnisoSkyMult(ISurf) * DifSolarRad;  // Sky diffuse solar intensity on window (W/m2)
                    BeamSolarOnWindow = BeamSolarRad * CosIncAng(TimeStep, HourOfDay, ISurf) *
                                        SunlitFrac(TimeStep, HourOfDay, ISurf);
                    SolarOnWindow =
                            BeamSolarOnWindow + SkySolarOnWindow + GndSolarRad * Surface(ISurf).ViewFactorGround;
                    HorizSolar = BeamSolarRad * SOLCOS(3) + DifSolarRad;
                }

                // Determine whether to deploy shading depending on type of control


                auto const SELECT_CASE_var(WindowShadingControl(IShadingCtrl).ShadingControlType);

                if (SELECT_CASE_var == WSCT_AlwaysOn) { // 'ALWAYSON'
                    SurfWinShadingFlag(ISurf) = ShType;

                } else if (SELECT_CASE_var == WSCT_AlwaysOff) { // 'ALWAYSOFF'
                    SurfWinShadingFlag(ISurf) = ShadeOff;

                } else if (SELECT_CASE_var == WSCT_OnIfScheduled) { // 'ONIFSCHEDULEALLOWS'
                    if (SchedAllowsControl) SurfWinShadingFlag(ISurf) = ShType;

                } else if (SELECT_CASE_var == WSCT_HiSolar) {
                    // 'ONIFHIGHSOLARONWINDOW'  ! Direct plus diffuse solar intensity on window
                    if (SunIsUp) {
                        if (SolarOnWindow > SetPoint && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_HiHorzSolar) {
                    // 'ONIFHIGHHORIZONTALSOLAR'  ! Direct plus diffuse exterior horizontal solar intensity
                    if (SunIsUp) {
                        if (HorizSolar > SetPoint && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_HiOutAirTemp) { // 'OnIfHighOutdoorAirTemperature'
                    if (Surface(ISurf).OutDryBulbTemp > SetPoint && SchedAllowsControl) {
                        SurfWinShadingFlag(ISurf) = ShType;
                    } else if (GlareControlIsActive) {
                        SurfWinShadingFlag(ISurf) = 10 * ShType;
                    }

                } else if (SELECT_CASE_var == WSCT_HiZoneAirTemp) {
                    // 'OnIfHighZoneAirTemperature'  ! Previous time step zone air temperature
                    if (MAT(IZone) > SetPoint && SchedAllowsControl) {
                        SurfWinShadingFlag(ISurf) = ShType;
                    } else if (GlareControlIsActive) {
                        SurfWinShadingFlag(ISurf) = 10 * ShType;
                    }

                } else if (SELECT_CASE_var == WSCT_OnHiOutTemp_HiSolarWindow) {
                    // 'OnIfHighOutdoorAirTempAndHighSolarOnWindow'  ! Outside air temp and solar on window
                    if (SunIsUp) {
                        if (Surface(ISurf).OutDryBulbTemp > SetPoint && SolarOnWindow > SetPoint2 &&
                            SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_OnHiOutTemp_HiHorzSolar) {
                    // 'OnIfHighOutdoorAirTempAndHighHorizontalSolar'  ! Outside air temp and horizontal solar
                    if (SunIsUp) {
                        if (Surface(ISurf).OutDryBulbTemp > SetPoint && HorizSolar > SetPoint2 &&
                            SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_OnHiZoneTemp_HiSolarWindow) {
                    // 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW'  ! Zone air temp and solar on window
                    if (SunIsUp) {
                        if (MAT(IZone) > SetPoint && SolarOnWindow > SetPoint2 && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var ==  WSCT_OnHiZoneTemp_HiHorzSolar) {
                    // 'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR'  ! Zone air temp and horizontal solar
                    if (SunIsUp) {
                        if (MAT(IZone) > SetPoint && HorizSolar > SetPoint2 && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_HiZoneCooling) {
                    // 'ONIFHIGHZONECOOLING'  ! Previous time step zone sensible cooling rate [W]
                    // In the following, the check on BeginSimFlag is needed since SNLoadCoolRate (and SNLoadHeatRate,
                    // used in other CASEs) are not allocated at this point for the first time step of the simulation.
                    if (!BeginSimFlag) {
                        if (SNLoadCoolRate(IZone) > SetPoint && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_HiGlare) {
                    // 'ONIFHIGHGLARE'  ! Daylight glare index at first reference point in the zone.
                    // This type of shading control is done in DayltgInteriorIllum. Glare control is not affected
                    // by control schedule.
                    if (SunIsUp) SurfWinShadingFlag(ISurf) = 10 * ShType;

                } else if (SELECT_CASE_var == WSCT_MeetDaylIlumSetp) {
                    // 'MEETDAYLIGHTILLUMINANCESETPOINT')  !  Daylight illuminance test is done in DayltgInteriorIllum
                    // Only switchable glazing does daylight illuminance control
                    if (SunIsUp && SchedAllowsControl) SurfWinShadingFlag(ISurf) = GlassConditionallyLightened;

                } else if (SELECT_CASE_var == WSCT_OnNightLoOutTemp_OffDay) { // 'OnNightIfLowOutdoorTempAndOffDay'
                    if (!SunIsUp && Surface(ISurf).OutDryBulbTemp < SetPoint && SchedAllowsControl) {
                        SurfWinShadingFlag(ISurf) = ShType;
                    } else if (GlareControlIsActive) {
                        SurfWinShadingFlag(ISurf) = 10 * ShType;
                    }

                } else if (SELECT_CASE_var == WSCT_OnNightLoInTemp_OffDay) { // 'OnNightIfLowInsideTempAndOffDay')
                    if (!SunIsUp && MAT(IZone) < SetPoint && SchedAllowsControl) {
                        SurfWinShadingFlag(ISurf) = ShType;
                    } else if (GlareControlIsActive) {
                        SurfWinShadingFlag(ISurf) = 10 * ShType;
                    }

                } else if (SELECT_CASE_var == WSCT_OnNightIfHeating_OffDay) { // 'OnNightIfHeatingAndOffDay'
                    if (!BeginSimFlag) {
                        if (!SunIsUp && SNLoadHeatRate(IZone) > SetPoint && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var == WSCT_OnNightLoOutTemp_OnDayCooling) {
                    // 'OnNightIfLowOutdoorTempAndOnDayIfCooling'
                    if (!BeginSimFlag) {
                        if (!SunIsUp) { // Night
                            if (Surface(ISurf).OutDryBulbTemp < SetPoint && SchedAllowsControl)
                                SurfWinShadingFlag(ISurf) = ShType;
                        } else { // Day
                            if (SNLoadCoolRate(IZone) > 0.0 && SchedAllowsControl) {
                                SurfWinShadingFlag(ISurf) = ShType;
                            } else if (GlareControlIsActive) {
                                SurfWinShadingFlag(ISurf) = 10 * ShType;
                            }
                        }
                    }

                } else if (SELECT_CASE_var ==
                           WSCT_OnNightIfHeating_OnDayCooling) { // 'OnNightIfHeatingAndOnDayIfCooling'
                    if (!BeginSimFlag) {
                        if (!SunIsUp) { // Night
                            if (SNLoadHeatRate(IZone) > SetPoint && SchedAllowsControl)
                                SurfWinShadingFlag(ISurf) = ShType;
                        } else { // Day
                            if (SNLoadCoolRate(IZone) > 0.0 && SchedAllowsControl) {
                                SurfWinShadingFlag(ISurf) = ShType;
                            } else if (GlareControlIsActive) {
                                SurfWinShadingFlag(ISurf) = 10 * ShType;
                            }
                        }
                    }

                } else if (SELECT_CASE_var ==
                           WSCT_OffNight_OnDay_HiSolarWindow) { // 'OffNightAndOnDayIfCoolingAndHighSolarOnWindow'
                    if (!BeginSimFlag) {
                        if (SunIsUp && SNLoadCoolRate(IZone) > 0.0 && SchedAllowsControl) {
                            if (SolarOnWindow > SetPoint) SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }

                } else if (SELECT_CASE_var ==
                           WSCT_OnNight_OnDay_HiSolarWindow) { // 'OnNightAndOnDayIfCoolingAndHighSolarOnWindow'
                    if (!BeginSimFlag) {
                        if (SunIsUp && SNLoadCoolRate(IZone) > 0.0 && SchedAllowsControl) {
                            if (SolarOnWindow > SetPoint) SurfWinShadingFlag(ISurf) = ShType;
                        } else if (!SunIsUp && SchedAllowsControl) {
                            SurfWinShadingFlag(ISurf) = ShType;
                        } else if (GlareControlIsActive) {
                            SurfWinShadingFlag(ISurf) = 10 * ShType;
                        }
                    }
                }

                // Set switching factor to fully switched if ShadingFlag = 2
                if (SurfWinShadingFlag(ISurf) == SwitchableGlazing) {
                    SurfWinSwitchingFactor(ISurf) = 1.0;

                    // Added TH 1/20/2010
                    // Vis trans at normal incidence of fully switched glass
                    IConst = Surface(ISurf).activeShadedConstruction;
                    SurfWinVisTransSelected(ISurf) =
                            POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef) * SurfWinGlazedFrac(ISurf);
                }

                // Slat angle control for blinds

                SurfWinSlatAngThisTS(ISurf) = 0.0;
                SurfWinSlatAngThisTSDeg(ISurf) = 0.0;
                SurfWinSlatsBlockBeam(ISurf) = false;
                if (SurfWinShadingFlag(ISurf) == IntBlindOn || SurfWinShadingFlag(ISurf) == 10 * IntBlindOn ||
                    SurfWinShadingFlag(ISurf) == ExtBlindOn || SurfWinShadingFlag(ISurf) == 10 * ExtBlindOn ||
                    SurfWinShadingFlag(ISurf) == BGBlindOn || SurfWinShadingFlag(ISurf) == 10 * BGBlindOn) {
                    // Blind in place or may be in place due to glare control
                    int BlNum = SurfWinBlindNumber(ISurf);
                    if (BlNum > 0) {
                        Real64 InputSlatAngle = Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians(); // Slat angle of associated Material:WindowBlind (rad)
                        Real64 ProfAng;            // Solar profile angle (rad)
                        Real64 SlatAng;            // Slat angle this time step (rad)
                        Real64 PermeabilityA;      // Intermediate variables in blind permeability calc
                        Real64 PermeabilityB;
                        Real64 ThetaBase;   // Intermediate slat angle variable (rad)
                        Real64 ThetaBlock1; // Slat angles that just block beam solar (rad)
                        Real64 ThetaBlock2;


                        if (Blind(BlNum).SlatWidth > Blind(BlNum).SlatSeparation && BeamSolarOnWindow > 0.0) {
                            ProfileAngle(ISurf, SOLCOS, Blind(BlNum).SlatOrientation, ProfAng);
                            Real64 ThetaBase = std::acos(
                                    std::cos(ProfAng) * Blind(BlNum).SlatSeparation / Blind(BlNum).SlatWidth);
                            // There are two solutions for the slat angle that just blocks beam radiation
                            ThetaBlock1 = ProfAng + ThetaBase;
                            ThetaBlock2 = ProfAng + DataGlobalConstants::Pi() - ThetaBase;
                            ThetaSmall = min(ThetaBlock1, ThetaBlock2);
                            ThetaBig = max(ThetaBlock1, ThetaBlock2);
                            ThetaMin = Blind(BlNum).MinSlatAngle * DataGlobalConstants::DegToRadians();
                            ThetaMax = Blind(BlNum).MaxSlatAngle * DataGlobalConstants::DegToRadians();
                        }

                        // TH 5/20/2010, CR 8064: Slat Width <= Slat Separation
                        if (Blind(BlNum).SlatWidth <= Blind(BlNum).SlatSeparation && BeamSolarOnWindow > 0.0) {
                            if (WindowShadingControl(IShadingCtrl).SlatAngleControlForBlinds == WSC_SAC_BlockBeamSolar) {

                                ProfileAngle(ISurf, SOLCOS, Blind(BlNum).SlatOrientation, ProfAng);

                                if (std::abs(std::cos(ProfAng) * Blind(BlNum).SlatSeparation / Blind(BlNum).SlatWidth) <= 1.0) {
                                    // set to block 100% of beam solar, not necessarily to block maximum solar (beam + diffuse)
                                    ThetaBase = std::acos(std::cos(ProfAng) * Blind(BlNum).SlatSeparation / Blind(BlNum).SlatWidth);
                                    SurfWinSlatsBlockBeam(ISurf) = true;
                                } else {
                                    // cannot block 100% of beam solar, turn slats to be perpendicular to sun beam to block maximal beam solar
                                    ThetaBase = 0.0;
                                }

                                // There are two solutions for the slat angle that just blocks beam radiation
                                ThetaBlock1 = ProfAng + ThetaBase;
                                ThetaBlock2 = ProfAng - ThetaBase + DataGlobalConstants::Pi();

                                ThetaSmall = min(ThetaBlock1, ThetaBlock2);
                                ThetaBig = max(ThetaBlock1, ThetaBlock2);
                                ThetaMin = Blind(BlNum).MinSlatAngle * DataGlobalConstants::DegToRadians();
                                ThetaMax = Blind(BlNum).MaxSlatAngle * DataGlobalConstants::DegToRadians();
                            }
                        }

                        auto const SELECT_CASE_var(WindowShadingControl(IShadingCtrl).SlatAngleControlForBlinds);

                        if (SELECT_CASE_var == WSC_SAC_FixedSlatAngle) { // 'FIXEDSLATANGLE'
                            SurfWinSlatAngThisTS(ISurf) = InputSlatAngle;
                            if ((SurfWinSlatAngThisTS(ISurf) <= ThetaSmall ||
                                 SurfWinSlatAngThisTS(ISurf) >= ThetaBig) &&
                                (Blind(BlNum).SlatWidth > Blind(BlNum).SlatSeparation) && (BeamSolarOnWindow > 0.0))
                                SurfWinSlatsBlockBeam(ISurf) = true;

                        } else if (SELECT_CASE_var == WSC_SAC_ScheduledSlatAngle) { // 'SCHEDULEDSLATANGLE'
                            SurfWinSlatAngThisTS(ISurf) = GetCurrentScheduleValue(
                                    WindowShadingControl(IShadingCtrl).SlatAngleSchedule);
                            SurfWinSlatAngThisTS(ISurf) = max(Blind(BlNum).MinSlatAngle,
                                                              min(SurfWinSlatAngThisTS(ISurf),
                                                                  Blind(BlNum).MaxSlatAngle)) * DataGlobalConstants::DegToRadians();
                            if ((SurfWinSlatAngThisTS(ISurf) <= ThetaSmall ||
                                 SurfWinSlatAngThisTS(ISurf) >= ThetaBig) &&
                                (Blind(BlNum).SlatWidth > Blind(BlNum).SlatSeparation) && (BeamSolarOnWindow > 0.0))
                                SurfWinSlatsBlockBeam(ISurf) = true;

                        } else if (SELECT_CASE_var == WSC_SAC_BlockBeamSolar) { // 'BLOCKBEAMSOLAR'
                            if (BeamSolarOnWindow > 0.0) {
                                if (Blind(BlNum).SlatSeparation >= Blind(BlNum).SlatWidth) {
                                    // TH 5/20/2010. CR 8064.
                                    // The following line of code assumes slats are always vertical/closed to minimize solar penetration
                                    // The slat angle can however change if the only goal is to block maximum amount of direct beam solar
                                    // SurfaceWindow(ISurf)%SlatAngThisTS = 0.0  ! Allows beam penetration but minimizes it

                                    if (ThetaSmall >= ThetaMin && ThetaSmall <= ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaSmall;
                                    } else if (ThetaBig >= ThetaMin && ThetaBig <= ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaBig;
                                    } else if (ThetaSmall < ThetaMin && ThetaBig < ThetaMin) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMin;
                                    } else if (ThetaSmall > ThetaMax && ThetaBig > ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMax;
                                    } else { // ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMin;
                                    }

                                } else { // Usual case -- slat width greater than slat separation
                                    if (ThetaSmall >= ThetaMin && ThetaSmall <= ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaSmall;
                                        SurfWinSlatsBlockBeam(ISurf) = true;
                                    } else if (ThetaBig >= ThetaMin && ThetaBig <= ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaBig;
                                        SurfWinSlatsBlockBeam(ISurf) = true;
                                    } else if (ThetaSmall < ThetaMin && ThetaBig < ThetaMin) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMin;
                                        SurfWinSlatsBlockBeam(ISurf) = true;
                                    } else if (ThetaSmall > ThetaMax && ThetaBig > ThetaMax) {
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMax;
                                        SurfWinSlatsBlockBeam(ISurf) = true;
                                    } else { // ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
                                        SurfWinSlatAngThisTS(ISurf) = ThetaMin;
                                    }
                                }
                            } else {
                                SurfWinSlatAngThisTS(ISurf) = InputSlatAngle;
                            }
                        }

                        SurfWinSlatAngThisTSDeg(ISurf) = SurfWinSlatAngThisTS(ISurf) / DataGlobalConstants::DegToRadians();
                        if (SurfWinSlatAngThisTSDegEMSon(ISurf)) {
                            SurfWinSlatAngThisTSDeg(ISurf) = SurfWinSlatAngThisTSDegEMSValue(ISurf);
                            SurfWinSlatAngThisTS(ISurf) = DataGlobalConstants::DegToRadians() * SurfWinSlatAngThisTSDeg(ISurf);
                        }
                        // Air flow permeability for calculation of convective air flow between blind and glass
                        SlatAng = SurfWinSlatAngThisTS(ISurf);
                        PermeabilityA = std::sin(SlatAng) - Blind(BlNum).SlatThickness / Blind(BlNum).SlatSeparation;
                        PermeabilityB = 1.0 - (std::abs(Blind(BlNum).SlatWidth * std::cos(SlatAng)) +
                                               Blind(BlNum).SlatThickness * std::sin(SlatAng)) /
                                              Blind(BlNum).SlatSeparation;
                        SurfWinBlindAirFlowPermeability(ISurf) = min(1.0, max(0.0, PermeabilityA, PermeabilityB));
                    }
                } // End of check if interior or exterior blind in place

                //   CALL CalcScreenTransmittance to intialized all screens prior to HB calc's
                if (SurfWinShadingFlag(ISurf) == ExtScreenOn && SunIsUp) {
                    CalcScreenTransmittance(ISurf);
                }

                // EMS Actuator Point: override setting if ems flag on
                if (SurfWinShadingFlagEMSOn(ISurf)) {
                    SurfWinShadingFlag(ISurf) = SurfWinShadingFlagEMSValue(ISurf);
                }

            } // End of surface loop
        }
    }

    int selectActiveWindowShadingControlIndex(int curSurface)
    {
        // For a given surface, determine based on the schedules which index to the window shading control list vector should be active
        int selected = 0; // presume it is the first shading control - even if it is not active it needs to be some shading control which is then turned off in the WindowShadingManager
        if (Surface(curSurface).windowShadingControlList.size() > 1) {
            for (std::size_t listIndex = 0; listIndex < Surface(curSurface).windowShadingControlList.size(); ++listIndex) {
                int wsc = Surface(curSurface).windowShadingControlList[listIndex];
                //pick the first WindowShadingControl that has a non-zero schedule value
                if (ScheduleManager::GetCurrentScheduleValue(WindowShadingControl(wsc).Schedule) > 0.0) {
                    selected = listIndex;
                    break;
                }
            }
        }
        return (selected);
    }

    void WindowGapAirflowControl()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   February 2003
        //       MODIFIED       June 2003, FCW: add fatal error for illegal schedule value
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For airflow windows, determines the airflow in the gap of
        // double glazing and in the inner gap of triple glazing.

        // REFERENCES:
        // na

        // Using/Aliasing
        using ScheduleManager::GetCurrentScheduleValue;

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int ISurf = firstSurfWin; ISurf <= lastSurfWin; ++ISurf) {

                SurfWinAirflowThisTS(ISurf) = 0.0;
                if (SurfWinMaxAirflow(ISurf) == 0.0) continue;
                if (Surface(ISurf).ExtBoundCond != ExternalEnvironment) continue;
                {
                    auto const SELECT_CASE_var(SurfWinAirflowControlType(ISurf));

                    if (SELECT_CASE_var == AirFlowWindow_ControlType_MaxFlow) {
                        SurfWinAirflowThisTS(ISurf) = SurfWinMaxAirflow(ISurf);

                    } else if (SELECT_CASE_var == AirFlowWindow_ControlType_AlwaysOff) {
                        SurfWinAirflowThisTS(ISurf) = 0.0;

                    } else if (SELECT_CASE_var == AirFlowWindow_ControlType_Schedule) {
                        if (SurfWinAirflowHasSchedule(ISurf)) {
                            int SchedulePtr = SurfWinAirflowSchedulePtr(ISurf); // Schedule pointer
                            Real64 ScheduleMult = GetCurrentScheduleValue(SchedulePtr); // Multiplier value from schedule
                            if (ScheduleMult < 0.0 || ScheduleMult > 1.0) {
                                ShowFatalError("Airflow schedule has a value outside the range 0.0 to 1.0 for window=" +
                                               Surface(ISurf).Name);
                            }
                            SurfWinAirflowThisTS(ISurf) = ScheduleMult * SurfWinMaxAirflow(ISurf);
                        }
                    }
                }
            }

        } // End of surface loop
    }

    void SkyDifSolarShading(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   May 1999
        //       MODIFIED       Sep 2000, FCW: add IR view factor calc
        //                      Sep 2002, FCW: correct error in expression for ground IR view factor.
        //                         Affects only non-vertical surfaces that are shadowed. For these surfaces
        //                         error caused underestimate of IR from ground and shadowing surfaces.
        //                      Dec 2002; LKL: Sky Radiance Distribution now only anisotropic
        //                      Nov 2003: FCW: modify to do sky solar shading of shadowing surfaces
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates factors that account for shading of sky diffuse
        // solar radiation by shadowing surfaces such as overhangs and detached
        // shades.
        // Called by PerformSolarCalculations
        // For each exterior heat transfer surface calculates the following
        // ratio (called DifShdgRatioIsoSky in this subroutine):
        //  R1 = (Diffuse solar from sky dome on surface, with shading)/
        //       (Diffuse solar from sky dome on surface, without shading)
        // To calculate the incident diffuse radiation on a surface the sky
        // hemisphere is divided into source elements ("patches"). Each patch
        // is assumed to have the same radiance, i.e. the sky radiance is isotropic.
        // The irradiance from each patch on a surface is calculated. Then these
        // irradiances are summed to get the net irradiance on a surface, which
        // the denominator of R1.
        // To get the numerator of R1 the same summation is done, but for each surface
        // and each patch the Shadow subroutine is called to determine how much
        // radiation from a patch is blocked by shading surfaces.
        // Also calculated is the following ratio (called DifShdgRatioHoriz in this routine):
        //  R2 = (Diffuse solar from sky horizon band on surface, with shading)/
        //       (Diffuse solar from sky horizon band on surface, without shading)
        // For this ratio only a band of sky just above the horizon is considered.
        // R1 and R2 are used in SUBROUTINE AnisoSkyViewFactors, which determines the
        // sky diffuse solar irradiance on each exterior heat transfer surface each
        // time step. In that routine the sky radiance distribution is a superposition
        // of an isotropic distribution,
        // a horizon brightening distribution and a circumsolar brightening distribution,
        // where the proportion of each distribution depends
        // on cloud cover, sun position and other factors. R1 multiplies the irradiance
        // due to the isotropic component and R2 multiplies the irradiance due to the
        // horizon brightening component.
        // Calculates sky and ground IR view factors assuming sky IR is isotropic and
        // shadowing surfaces are opaque to IR.

        // Using/Aliasing
        using DataSystemVariables::DetailedSkyDiffuseAlgorithm;

        int SrdSurfsNum;        // Srd surface counter
        Real64 Fac1WoShdg;      // Intermediate calculation factor, without shading
        Real64 FracIlluminated; // Fraction of surface area illuminated by a sky patch
        Real64 Fac1WithShdg;    // Intermediate calculation factor, with shading
        Real64 SurfArea;        // Surface area (m2)
        bool ShadowingSurf;     // True if surface is a shadowing surface
        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgIsoSky     ! Diffuse solar irradiance from isotropic
        //                                                          ! sky on surface, with shading
        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgIsoSky       ! Diffuse solar from isotropic
        //                                                           ! sky on surface, without shading
        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgHoriz      ! Diffuse solar irradiance from horizon portion of
        //                                                           ! sky on surface, with shading
        // REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgHoriz        ! Diffuse solar irradiance from horizon portion of
        //                                                           ! sky on surface, without shading
        // INTEGER iHour,iTS

        // FLOW:

        // Initialize Surfaces Arrays
        state.dataSolarShading->SAREA = 0.0;
        WithShdgIsoSky.dimension(TotSurfaces, 0.0);
        WoShdgIsoSky.dimension(TotSurfaces, 0.0);
        WithShdgHoriz.dimension(TotSurfaces, 0.0);
        WoShdgHoriz.dimension(TotSurfaces, 0.0);
        DifShdgRatioIsoSky.allocate(TotSurfaces);
        DifShdgRatioHoriz.allocate(TotSurfaces);
        // initialized as no shading
        DifShdgRatioIsoSky = 1.0;
        DifShdgRatioHoriz = 1.0;
        if (DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
            curDifShdgRatioIsoSky.dimension(TotSurfaces, 1.0);
        }

        // only for detailed.
        if (DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
            DifShdgRatioIsoSkyHRTS.allocate(NumOfTimeStepInHour, 24, TotSurfaces);
            DifShdgRatioIsoSkyHRTS = 1.0;
            DifShdgRatioHorizHRTS.allocate(NumOfTimeStepInHour, 24, TotSurfaces);
            DifShdgRatioHorizHRTS = 1.0;
        }

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!Surface(SurfNum).ExtSolar) continue;

            // CurrentModuleObject='Surfaces'
            if (DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
                SetupOutputVariable(state, "Debug Surface Solar Shading Model DifShdgRatioIsoSky",
                                    OutputProcessor::Unit::None,
                                    curDifShdgRatioIsoSky(SurfNum),
                                    "Zone",
                                    "Average",
                                    Surface(SurfNum).Name);
            } else {
                SetupOutputVariable(state, "Debug Surface Solar Shading Model DifShdgRatioIsoSky",
                                    OutputProcessor::Unit::None,
                                    DifShdgRatioIsoSky(SurfNum),
                                    "Zone",
                                    "Average",
                                    Surface(SurfNum).Name);
            }
            SetupOutputVariable(state, "Debug Surface Solar Shading Model DifShdgRatioHoriz",
                                OutputProcessor::Unit::None,
                                DifShdgRatioHoriz(SurfNum),
                                "Zone",
                                "Average",
                                Surface(SurfNum).Name);
            SetupOutputVariable(state, "Debug Surface Solar Shading Model WithShdgIsoSky",
                                OutputProcessor::Unit::None,
                                WithShdgIsoSky(SurfNum),
                                "Zone",
                                "Average",
                                Surface(SurfNum).Name);
            SetupOutputVariable(state, "Debug Surface Solar Shading Model WoShdgIsoSky",
                                OutputProcessor::Unit::None,
                                WoShdgIsoSky(SurfNum),
                                "Zone",
                                "Average",
                                Surface(SurfNum).Name);
        }

        for (int IPhi = 0; IPhi < NPhi; ++IPhi) { // Loop over patch altitude values
            state.dataSolarShading->SUNCOS(3) = sin_Phi[IPhi];

            for (int ITheta = 0; ITheta < NTheta; ++ITheta) { // Loop over patch azimuth values
                state.dataSolarShading->SUNCOS(1) = cos_Phi[IPhi] * cos_Theta[ITheta];
                state.dataSolarShading->SUNCOS(2) = cos_Phi[IPhi] * sin_Theta[ITheta];

                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) { // Cosine of angle of incidence on surface of solar
                    // radiation from patch
                    ShadowingSurf = Surface(SurfNum).ShadowingSurf;

                    if (!ShadowingSurf && !Surface(SurfNum).HeatTransSurf) continue;

                    state.dataSolarShading->CTHETA(SurfNum) = state.dataSolarShading->SUNCOS(1) * Surface(SurfNum).OutNormVec(1) + state.dataSolarShading->SUNCOS(2) * Surface(SurfNum).OutNormVec(2) +
                                      state.dataSolarShading->SUNCOS(3) * Surface(SurfNum).OutNormVec(3);
                }

                SHADOW(state, 24, 0);

                for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                    ShadowingSurf = Surface(SurfNum).ShadowingSurf;

                    if (!ShadowingSurf &&
                        (!Surface(SurfNum).HeatTransSurf || !Surface(SurfNum).ExtSolar ||
                         (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)))
                        continue;

                    if (state.dataSolarShading->CTHETA(SurfNum) < 0.0) continue;

                    Fac1WoShdg = cos_Phi[IPhi] * DThetaDPhi * state.dataSolarShading->CTHETA(SurfNum);
                    SurfArea = Surface(SurfNum).NetAreaShadowCalc;
                    if (SurfArea > Eps) {
                        FracIlluminated = state.dataSolarShading->SAREA(SurfNum) / SurfArea;
                    } else {
                        FracIlluminated = state.dataSolarShading->SAREA(SurfNum) / (SurfArea + Eps);
                    }
                    Fac1WithShdg = Fac1WoShdg * FracIlluminated;
                    WithShdgIsoSky(SurfNum) += Fac1WithShdg;
                    WoShdgIsoSky(SurfNum) += Fac1WoShdg;

                    // Horizon region
                    if (IPhi == 0) {
                        WithShdgHoriz(SurfNum) += Fac1WithShdg;
                        WoShdgHoriz(SurfNum) += Fac1WoShdg;
                    }
                } // End of surface loop
            }     // End of Theta loop
        }         // End of Phi loop

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            ShadowingSurf = Surface(SurfNum).ShadowingSurf;

            if (!ShadowingSurf &&
                (!Surface(SurfNum).HeatTransSurf || !Surface(SurfNum).ExtSolar ||
                 (Surface(SurfNum).ExtBoundCond != ExternalEnvironment && Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)))
                continue;

            if (std::abs(WoShdgIsoSky(SurfNum)) > Eps) {
                DifShdgRatioIsoSky(SurfNum) = (WithShdgIsoSky(SurfNum)) / (WoShdgIsoSky(SurfNum));
            } else {
                DifShdgRatioIsoSky(SurfNum) = (WithShdgIsoSky(SurfNum)) / (WoShdgIsoSky(SurfNum) + Eps);
            }
            if (std::abs(WoShdgHoriz(SurfNum)) > Eps) {
                DifShdgRatioHoriz(SurfNum) = (WithShdgHoriz(SurfNum)) / (WoShdgHoriz(SurfNum));
            } else {
                DifShdgRatioHoriz(SurfNum) = (WithShdgHoriz(SurfNum)) / (WoShdgHoriz(SurfNum) + Eps);
            }
        }

        // Get IR view factors. An exterior surface can receive IR radiation from
        // sky, ground or shadowing surfaces. Assume shadowing surfaces have same
        // temperature as outside air (and therefore same temperature as ground),
        // so that the view factor to these shadowing surfaces can be included in
        // the ground view factor. Sky IR is assumed to be isotropic and shadowing
        // surfaces are assumed to be opaque to IR so they totally "shade" IR from
        // sky or ground.

        for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            if (!DetailedSkyDiffuseAlgorithm || !ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing) {
                Surface(SurfNum).ViewFactorSkyIR *= DifShdgRatioIsoSky(SurfNum);
            } else {
                Surface(SurfNum).ViewFactorSkyIR *= DifShdgRatioIsoSkyHRTS(1, 1, SurfNum);
            }
            Surface(SurfNum).ViewFactorGroundIR = 1.0 - Surface(SurfNum).ViewFactorSkyIR;

            if (Surface(SurfNum).HasSurroundingSurfProperties) {
                SrdSurfsNum = Surface(SurfNum).SurroundingSurfacesNum;
                if (SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor != -1) {
                    Surface(SurfNum).ViewFactorSkyIR *= SurroundingSurfsProperty(SrdSurfsNum).SkyViewFactor;
                }
                if (SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor != -1) {
                    Surface(SurfNum).ViewFactorGroundIR *= SurroundingSurfsProperty(SrdSurfsNum).GroundViewFactor;
                }
            }
        }

        //  DEALLOCATE(WithShdgIsoSky)
        //  DEALLOCATE(WoShdgIsoSky)
        //  DEALLOCATE(WithShdgHoriz)
        //  DEALLOCATE(WoShdgHoriz)

        if (DetailedSkyDiffuseAlgorithm && ShadingTransmittanceVaries && SolarDistribution != MinimalShadowing) {
            for (int SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                DifShdgRatioIsoSkyHRTS({1, NumOfTimeStepInHour}, {1, 24}, SurfNum) = DifShdgRatioIsoSky(SurfNum);
                DifShdgRatioHorizHRTS({1, NumOfTimeStepInHour}, {1, 24}, SurfNum) = DifShdgRatioHoriz(SurfNum);
            }
        }
    }

    void CalcWindowProfileAngles()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   April 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
        // Calculates horizontal and vertical beam solar profile angles

        Real64 ElevSun;       // Sun elevation; angle between sun and horizontal
        Real64 ElevWin;       // Window elevation: angle between window outward normal and horizontal
        Real64 AzimWin;       // Window azimuth (radians)
        Real64 AzimSun;       // Sun azimuth (radians)
        Real64 ProfileAngHor; // Solar profile angle (radians) for horizontally oriented window elements
        // such as the top and bottom of a frame.
        // This is the incidence angle in a plane that is normal to the window
        // and parallel to the Y-axis of the window (the axis along
        // which the height of the window is measured).
        Real64 ProfileAngVert; // Solar profile angle (radians) for vertically oriented elements
        // such as the sides of a frame.
        // This is the incidence angle in a plane that is normal to the window
        // and parallel to the X-axis of the window (the axis along
        // which the width of the window is measured).
        Vector3<Real64> WinNorm;                 // Unit vector normal to window
        Vector3<Real64> WinNormCrossBase;        // Cross product of WinNorm and vector along window baseline
        Vector3<Real64> SunPrime;                // Projection of sun vector onto plane (perpendicular to
        Vector3<Real64> const SolCosVec(SOLCOS); // Local Vector3 copy for speed (until SOLCOS mig to Vector3)
        //  window plane) determined by WinNorm and vector along
        //  baseline of window
        Real64 ThWin; // Azimuth angle of WinNorm (radians)
        Real64 dot1;
        Real64 dot2;
        Real64 dot3;

        ElevSun = DataGlobalConstants::PiOvr2() - std::acos(SolCosVec.z);
        AzimSun = std::atan2(SolCosVec.x, SolCosVec.y);

        Real64 const cos_ElevSun = std::cos(ElevSun);
        Real64 const sin_ElevSun = std::sin(ElevSun);

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {

                if (Surface(SurfNum).ExtBoundCond != ExternalEnvironment &&
                    Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt)
                    continue;

                SurfWinProfileAngHor(SurfNum) = 0.0;
                SurfWinProfileAngVert(SurfNum) = 0.0;
                if (CosIncAng(TimeStep, HourOfDay, SurfNum) <= 0.0) continue;

                ElevWin = DataGlobalConstants::PiOvr2() - Surface(SurfNum).Tilt * DataGlobalConstants::DegToRadians();
                AzimWin = Surface(SurfNum).Azimuth * DataGlobalConstants::DegToRadians();

                ProfileAngHor = std::atan(sin_ElevSun / std::abs(cos_ElevSun * std::cos(AzimWin - AzimSun))) - ElevWin;

                // CR9280 - were having negative profile angles on west sides.  commenting out previous code (original code) for
                // vertical windows
                //  IF(ABS(ElevWin) < 0.1d0) THEN  ! Near-vertical window
                //    ProfileAngVert = ABS(AzimWin-AzimSun)
                //  ELSE
                WinNorm = Surface(SurfNum).OutNormVec;
                ThWin = AzimWin - DataGlobalConstants::PiOvr2();
                Real64 const sin_Elevwin(std::sin(ElevWin));
                WinNormCrossBase.x = -(sin_Elevwin * std::cos(ThWin));
                WinNormCrossBase.y = sin_Elevwin * std::sin(ThWin);
                WinNormCrossBase.z = std::cos(ElevWin);
                SunPrime = SolCosVec - WinNormCrossBase * dot(SolCosVec, WinNormCrossBase);
                dot1 = dot(WinNorm, SunPrime);
                dot2 = SunPrime.magnitude();
                dot3 = dot1 / dot2;
                if (dot3 > 1.0) {
                    dot3 = 1.0;
                } else if (dot3 < -1.0) {
                    dot3 = -1.0;
                }
                //    ProfileAngVert = ABS(ACOS(DOT_PRODUCT(WinNorm,SunPrime)/SQRT(DOT_PRODUCT(SunPrime,SunPrime))))
                ProfileAngVert = std::abs(std::acos(dot3));
                //  END IF
                // Constrain to 0 to pi
                if (ProfileAngVert > DataGlobalConstants::Pi()) ProfileAngVert = DataGlobalConstants::TwoPi() - ProfileAngVert;

                SurfWinProfileAngHor(SurfNum) = ProfileAngHor / DataGlobalConstants::DegToRadians();
                SurfWinProfileAngVert(SurfNum) = ProfileAngVert / DataGlobalConstants::DegToRadians();
                SurfWinTanProfileAngHor(SurfNum) = std::abs(std::tan(ProfileAngHor));
                SurfWinTanProfileAngVert(SurfNum) = std::abs(std::tan(ProfileAngVert));
            }
        }
    }

    void CalcFrameDividerShadow(EnergyPlusData &state, int const SurfNum,  // Surface number
                                int const FrDivNum, // Frame/divider number
                                int const HourNum   // Hour number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   June 2000
        //       MODIFIED       Aug 2000, FW: add effective shadowing by inside
        //                      projections
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
        // with a frame and/or divider. Using beam solar profile angles,
        // calculates fraction of glass shaded by exterior frame and divider projections,
        // The frame and divider profiles are assumed to be rectangular.
        // A similar shadowing approach is used to calculate the fraction of glass area
        // that produces beam solar illumination on interior frame and divider projections.
        // This fraction is used in CalcWinFrameAndDividerTemps to determine the
        // beam solar absorbed by inside projections. Beam solar reflected by inside projections
        // is assumed to stay in the zone (as beam solar) although in actuality roughly
        // half of this is reflected back onto the glass and the half that is reflected
        // into the zone is diffuse.
        // For multipane glazing the effect of solar absorbed by the exposed portion of
        // frame or divider between the panes is not calculated. Beam solar incident on
        // these portions is assumed to be transmitted into the zone unchanged.
        // The shadowing of diffuse solar radiation by projections is not considered.

        Real64 ElevSun;       // Sun elevation; angle between sun and horizontal
        Real64 ElevWin;       // Window elevation: angle between window outward normal and horizontal
        Real64 AzimWin;       // Window azimuth (radians)
        Real64 AzimSun;       // Sun azimuth (radians)
        Real64 ProfileAngHor; // Solar profile angle (radians) for horizontally oriented projections
        // such as the top and bottom of a frame or horizontal dividers.
        // This is the incidence angle in a plane that is normal to the window
        // and parallel to the Y-axis of the window (the axis along
        // which the height of the window is measured).
        Real64 ProfileAngVert; // Solar profile angle (radians) for vertically oriented projections
        // such as the top and bottom of a frame or horizontal dividers.
        // This is the incidence angle in a plane that is normal to the window
        // and parallel to the X-axis of the window (the axis along
        // which the width of the window is measured).
        Real64 TanProfileAngHor;  // Tangent of ProfileAngHor
        Real64 TanProfileAngVert; // Tangent of ProfileAngVert
        Real64 FrWidth;           // Frame width (m)
        Real64 DivWidth;          // Divider width (m)
        Real64 FrProjOut;         // Outside frame projection (m)
        Real64 DivProjOut;        // Outside divider projection (m)
        Real64 FrProjIn;          // Inside frame projection (m)
        Real64 DivProjIn;         // Inside divider projection (m)
        int NHorDiv;              // Number of horizontal dividers
        int NVertDiv;             // Number of vertical dividers
        Real64 GlArea;            // Glazed area (m2)
        Real64 Arealite;          // Area of a single lite of glass (m2); glazed area, GlArea,
        // if there is no divider (in which case there is only one lite).
        Real64 ArealiteCol; // Area of a vertical column of lites (m2)
        Real64 ArealiteRow; // Area of a horizontal row of lites (m2)
        Real64 AshVDout;    // Shaded area from all vertical divider outside projections (m2)
        Real64 AshVDin;     // Shaded area from all vertical divider inside projections (m2)
        Real64 AshHDout;    // Shaded area from all horizontal divider outside projections (m2)
        Real64 AshHDin;     // Shaded area from all horizontal divider inside projections (m2)
        Real64 AshVFout;    // Shaded area from outside projection of vertical sides of frame (m2)
        Real64 AshVFin;     // Shaded area from inside projection of vertical sides of frame (m2)
        Real64 AshHFout;    // Shaded area from outside projection of horizontal sides
        //   (top) of frame (m2)
        Real64 AshHFin; // Shaded area from inside projection of horizontal sides
        //   (top) of frame (m2)
        Real64 AshDDover;   // Divider/divider shadow overlap area (m2)
        Real64 AshFFover;   // Frame/frame shadow overlap area (m2)
        Real64 AshFVDover;  // Frame/vertical divider overlap area (m2)
        Real64 AshFHDover;  // Frame/horizontal divider overlap area (m2)
        Real64 AshFDtotOut; // Total outside projection shadow area (m2)
        Real64 AshFDtotIn;  // Total inside projection shadow area (m2)
        Real64 FracShFDOut; // Fraction of glazing shadowed by frame and divider
        //  outside projections
        Real64 FracShFDin; // Fraction of glazing that illuminates frame and divider
        //  inside projections with beam radiation

        Array1D<Real64> WinNorm(3);  // Window outward normal unit vector
        Real64 ThWin;                // Azimuth angle of WinNorm
        Array1D<Real64> SunPrime(3); // Projection of sun vector onto plane (perpendicular to
        //  window plane) determined by WinNorm and vector along
        //  baseline of window
        Array1D<Real64> WinNormCrossBase(3); // Cross product of WinNorm and vector along window baseline

        if (FrameDivider(FrDivNum).FrameProjectionOut == 0.0 && FrameDivider(FrDivNum).FrameProjectionIn == 0.0 &&
            FrameDivider(FrDivNum).DividerProjectionOut == 0.0 && FrameDivider(FrDivNum).DividerProjectionIn == 0.0)
            return;

        FrProjOut = FrameDivider(FrDivNum).FrameProjectionOut;
        FrProjIn = FrameDivider(FrDivNum).FrameProjectionIn;
        DivProjOut = FrameDivider(FrDivNum).DividerProjectionOut;
        DivProjIn = FrameDivider(FrDivNum).DividerProjectionIn;

        GlArea = Surface(SurfNum).Area;
        ElevWin = DataGlobalConstants::PiOvr2() - Surface(SurfNum).Tilt * DataGlobalConstants::DegToRadians();
        ElevSun = DataGlobalConstants::PiOvr2() - std::acos(state.dataSolarShading->SUNCOS(3));
        AzimWin = Surface(SurfNum).Azimuth * DataGlobalConstants::DegToRadians();
        AzimSun = std::atan2(state.dataSolarShading->SUNCOS(1), state.dataSolarShading->SUNCOS(2));

        ProfileAngHor = std::atan(std::sin(ElevSun) / std::abs(std::cos(ElevSun) * std::cos(AzimWin - AzimSun))) - ElevWin;
        if (std::abs(ElevWin) < 0.1) { // Near-vertical window
            ProfileAngVert = std::abs(AzimWin - AzimSun);
        } else {
            WinNorm = Surface(SurfNum).OutNormVec;
            ThWin = AzimWin - DataGlobalConstants::PiOvr2();
            WinNormCrossBase(1) = -std::sin(ElevWin) * std::cos(ThWin);
            WinNormCrossBase(2) = std::sin(ElevWin) * std::sin(ThWin);
            WinNormCrossBase(3) = std::cos(ElevWin);
            SunPrime = state.dataSolarShading->SUNCOS - WinNormCrossBase * dot(state.dataSolarShading->SUNCOS, WinNormCrossBase);
            ProfileAngVert = std::abs(std::acos(dot(WinNorm, SunPrime) / magnitude(SunPrime)));
        }
        // Constrain to 0 to pi
        if (ProfileAngVert > DataGlobalConstants::Pi()) ProfileAngVert = 2 * DataGlobalConstants::Pi() - ProfileAngVert;
        TanProfileAngHor = std::abs(std::tan(ProfileAngHor));
        TanProfileAngVert = std::abs(std::tan(ProfileAngVert));

        NHorDiv = FrameDivider(FrDivNum).HorDividers;
        NVertDiv = FrameDivider(FrDivNum).VertDividers;
        FrWidth = FrameDivider(FrDivNum).FrameWidth;
        DivWidth = FrameDivider(FrDivNum).DividerWidth;

        Arealite = (Surface(SurfNum).Height / (NHorDiv + 1.0) - DivWidth / 2.0) * (Surface(SurfNum).Width / (NVertDiv + 1.0) - DivWidth / 2.0);
        if (DivProjOut > 0.0 || DivProjIn > 0.0) {
            ArealiteCol = (NHorDiv + 1) * Arealite;
            ArealiteRow = (NVertDiv + 1) * Arealite;
        } else {
            ArealiteCol = GlArea;
            ArealiteRow = GlArea;
        }
        AshVDout = 0.0;
        AshVDin = 0.0;
        AshHDout = 0.0;
        AshHDin = 0.0;
        AshVFout = 0.0;
        AshVFin = 0.0;
        AshHFout = 0.0;
        AshHFin = 0.0;
        AshDDover = 0.0;
        AshFFover = 0.0;
        AshFVDover = 0.0;
        AshFHDover = 0.0;

        if (DivProjOut > 0.0 || DivProjIn > 0.0) {

            // Shaded area from all vertical dividers
            AshVDout = NVertDiv * min((Surface(SurfNum).Height - NHorDiv * DivWidth) * DivProjOut * TanProfileAngVert, ArealiteCol);
            AshVDin = NVertDiv * min((Surface(SurfNum).Height - NHorDiv * DivWidth) * DivProjIn * TanProfileAngVert, ArealiteCol);

            // Shaded area from all horizontal dividers
            AshHDout = NHorDiv * min((Surface(SurfNum).Width - NVertDiv * DivWidth) * DivProjOut * TanProfileAngHor, ArealiteRow);
            AshHDin = NHorDiv * min((Surface(SurfNum).Width - NVertDiv * DivWidth) * DivProjIn * TanProfileAngHor, ArealiteRow);

            // Horizontal divider/vertical divider shadow overlap
            AshDDover = min(DivProjOut * TanProfileAngHor * DivProjOut * TanProfileAngVert, Arealite) * NHorDiv * NVertDiv;
        }

        if (FrProjOut > 0.0 || FrProjIn > 0.0) {

            // Shaded area from sides of frame; to avoid complications from possible overlaps between
            // shadow from side of frame and shadow from vertical divider the shaded area from side of
            // frame is restricted to the area of one column of lites.
            AshVFout = min((Surface(SurfNum).Height - NHorDiv * DivWidth) * FrProjOut * TanProfileAngVert, ArealiteCol);
            AshVFin = min((Surface(SurfNum).Height - NHorDiv * DivWidth) * FrProjIn * TanProfileAngVert, ArealiteCol);

            // Shaded area from top or bottom of frame; to avoid complications from possible overlaps
            // between shadow from top or bottom of frame and shadow from horizontal divider, the shaded
            // area from the top or bottom of frame is restricted to the area of one row of lites.
            AshHFout = min((Surface(SurfNum).Width - NVertDiv * DivWidth) * FrProjOut * TanProfileAngHor, ArealiteRow);
            AshHFin = min((Surface(SurfNum).Width - NVertDiv * DivWidth) * FrProjIn * TanProfileAngHor, ArealiteRow);

            // Top/bottom of frame/side of frame shadow overlap
            AshFFover = min(FrProjOut * TanProfileAngHor * FrProjOut * TanProfileAngVert, Arealite);

            if (DivProjOut > 0.0) {
                // Frame/vertical divider shadow overlap
                AshFVDover = min(FrProjOut * DivProjOut * TanProfileAngHor * TanProfileAngVert, Arealite) * NVertDiv;

                // Frame/horizontal divider shadow overlap
                AshFHDover = min(FrProjOut * DivProjOut * TanProfileAngHor * TanProfileAngVert, Arealite) * NHorDiv;
            }
        }

        AshFDtotOut = AshVDout + AshHDout + AshVFout + AshHFout - (AshDDover + AshFFover + AshFVDover + AshFHDover);
        AshFDtotIn = (AshVDin + AshHDin) * FrameDivider(FrDivNum).DividerSolAbsorp + (AshVFin + AshHFin) * FrameDivider(FrDivNum).FrameSolAbsorp;

        // Divide by the glazed area of the window
        FracShFDOut = AshFDtotOut / GlArea;
        FracShFDin = AshFDtotIn / GlArea;
        SurfaceWindow(SurfNum).OutProjSLFracMult(HourNum) = 1.0 - FracShFDOut;
        SurfaceWindow(SurfNum).InOutProjSLFracMult(HourNum) = 1.0 - (FracShFDin + FracShFDOut);
    }

    void CalcBeamSolarOnWinRevealSurface(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         F. Winkelmann
        //       DATE WRITTEN   April 2002
        //       MODIFIED:na
        //       RE-ENGINEERED:na

        // PURPOSE OF THIS SUBROUTINE
        // Called by InitHeatGains when the sun is up.
        // Calculates beam solar radiation absorbed and reflected by top, bottom,
        // right and left sides of outside and inside window reveal surfaces.
        // In doing this calculation, the shadowing on a reveal surface by other reveal surfaces
        // is determined using the orientation of the reveal surfaces and the sun position.
        // It is assumed that:
        // (1) The window is an exterior window and is rectangular.
        // (2) The reveal surfaces are perpendicular to the window plane.
        // (3) If an exterior shade or blind is in place, there is no beam solar on
        //     on exterior or interior reveal surfaces.
        // (3) If an interior shade or blind is in place, there is no beam solar on
        //     interior reveal surfaces.
        // (4) The effect of window divider, if present, is ignored, including shadowing
        //     of divider on inside reveal surfaces.

        // In the variable names, the "subscript" 1 = outside reveal, 2 = inside reveal
        // The outside reveal surfaces (top, bottom, left, right) are assumed to have the same depth
        // (given by Surface%Reveal and determined from vertices of window and vertices of parent
        // wall) and the same solar absorptance. The inside reveal surfaces are divided into
        // two categories: (1) the bottom reveal surface, called here the "inside sill;" and
        // the other reveal surfaces (left, right and top). The left, right and top inside reveal
        // surfaces are assumed to have the same depth and solar absorptance.
        // The depth of the outside reveal is measured from the outside surface of the glazing;
        // The depth of the inside sill and the other reveal surfaces is measured from the inside
        // surface of the glazing. The inside sill is
        // allowed to have depth and solar absorptance values that are different from the corresponding
        // values for the other inside reveal surfaces. The inside sill depth is required to be
        // greater than or equal to the depth of the other inside reveal surfaces. If the inside sill
        // depth is greater than zero the depth of the other inside reveal surfaces is required to
        // to be greater than zero.
        // The reflection of beam solar radiation from all reveal surfaces is assumed to be isotropic
        // diffuse; there is no specular component. Half of the beam solar reflected from outside
        // reveal surfaces is assumed to go towards the window; the other half is assumed to go back
        // to the exterior environment (i.e., reflection of this outward-going component from
        // other outside reveal surfaces is not considered). The half that goes towards the window
        // is added to the other radiation incident on the window.
        // Correspondingly, half of the beam solar reflected from inside reveal surfaces is assumed
        // to go towards the window, with the other half going into the zone (this half, and the portion
        // going towards the window that is reflected) is added in CalcInteriorSolarDistribution
        // to the variable BTOTzone, which is the total beam solar entering the zone as beam or diffuse.
        // The portion going towards the window that is not reflected is absorbed in the glazing or
        // transmitted back out into the exterior environment.
        // The beam solar that is absorbed by outside reveal surfaces is added to the solar absorbed
        // by the outside surface of the window's parent wall; similarly, the beam solar absorbed
        // by the inside reveal surfaces is added to the solar absorbed by the inside surface of the
        // parent wall (and is subtracted from BTOTzone).
        // The net effect of beam solar reflected from outside reveal surfaces is to INCREASE the
        // the heat gain to the zone, whereas the effect of beam solar reflected from interior reveal
        // surfaces is to DECREASE the heat gain to the zone since part of this reflected solar is
        // transmitted back out the window.
        // If the window has a frame, the absorption of reflected beam solar by the inside and outside
        // surfaces of the frame is considered. The shadowing of the frame onto interior reveal
        // surfaces is also considered.

        // The total glazing thickness is taken to be the sum of the thickness of the glass layers
        // and between-glass gas layers. If the window has an exterior, movable, storm window glass layer
        // the presence of this layer and its adjacent air gap is considered in calculating the glazing
        // properties (solar transmittance, etc.). But the storm window glass is assumed to be close
        // enough to the rest of the glazing that its effect on total glazing thickness and outside
        // reveal depth can be ignored.

        // METHODOLOGY EMPLOYED
        // na

        // REFERENCES
        // na

        // USE STATEMENTS
        // Using/Aliasing
        using General::InterpSw;
        using General::POLYF;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS

        int ConstrNum;        // Construction number
        int ConstrNumSh;      // Shaded construction number
        Real64 CosBetaBottom; // Cosine of beam solar angle of incidence on bottom reveal
        Real64 CosBetaLeft;   // Cosine of beam solar angle of incidence on left reveal
        Real64 CosBeta;       // ABS of CosBetaBottom or CosBetaLeft
        Real64 d1;            // Depth of outside reveal + half of glazing thickness (m)
        Real64 d2;            // Depth of inside sill or of inside reveal plus half of glazing thickness (m)
        Real64 d2prime;       // Depth of shadow cast on a reveal surface by opposite reveal (m)
        Real64 d2prime2;      // Depth of shadow cast by frame onto inside reveal (m)
        Real64 d12;           // d12 = d1 + d2 - d2prime (m)
        Real64 TanAlpha;      // Tangent of horizontal or vertical profile angle
        Real64 TanGamma;      // Tangent of vertical or horizontal profile angle
        Real64 H;             // Window height, width (m)
        Real64 W;
        Real64 L;                    // Window height or width (m)
        Real64 A1sh;                 // Shadowed area of outside horizontal or vertical reveal (m2)
        Real64 A2sh;                 // Shadowed area of inside horizontal or vertical reveal (m2)
        Real64 A1ill;                // Illuminated area of outside horizontal or vertical reveal (m2)
        Real64 A2ill;                // Illuminated area of inside horizontal or vertical reveal (m2)
        Real64 SolTransGlass;        // Beam solar transmittance of glazing
        Real64 SolTransGlassSh;      // For switchable glazing, beam solar trans in switched state
        Real64 DiffReflGlass;        // Diffuse back reflectance of glazing
        Real64 DiffReflGlassSh;      // For switchable glazing, diffuse back refl in switched state
        int HorVertReveal;           // Index: 1 = horizontal reveal, 2 = vertical reveal
        Real64 OutsReveal;           // Depth of outside reveal (from outside glazing plane to outside wall plane) (m)
        Real64 InsReveal;            // Depth of inside reveal (from inside glazing plane to inside wall plane (m)
        Real64 InsSillDepth;         // Depth of inside sill, measured from innermost face of glazing (m)
        Real64 GlazingThickness;     // Thickness of glazing, measured from innermost face to outermost face (m)
        Real64 InsideRevealSolAbs;   // Solar absorptance of inside reveal or inside sill
        Real64 BmSolRefldOutsReveal; // Multiplied by beam solar gives beam solar reflected by horiz or vertical
        //  outside reveal surface (m2)
        Real64 BmSolRefldInsReveal; // Multiplied by beam solar gives beam solar reflected by horiz or vertical
        //  inside reveal surface (m2)
        int ShadeFlag;     // Shading flag
        int FrameDivNum;   // Frame/Divider number
        Real64 FrameWidth; // Frame width (m)
        Real64 P1;         // Frame outside/inside projection plus half of glazing thickness (m)
        Real64 P2;
        Real64 f1; // f1=d1-P1, f2=d2-P2 (m)
        Real64 f2;
        Real64 L1; // Average distance of outside/inside illuminated area to frame;
        Real64 L2;
        // used in calculating view factor to frame (m)
        Real64 FracToGlassOuts;   // View factor from outside horizontal or vertical reveal to glass
        Real64 FracToGlassIns;    // View factor from inside horizontal or vertical reveal to glass
        Real64 TanProfileAngVert; // Tangent of vertical profile angle (the profile angle appropriate for
        // vertical reveal surfaces.
        Real64 TanProfileAngHor; // Tangent of horizontal profile angle (the profile angle appropriate for
        // horizontal reveal surfaces.

        Real64 tmp_SunlitFracWithoutReveal; // Temporary variable

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                // Added TH for initialization. CR 7596 inside reveal causing high cooling loads
                // for outside reveals
                SurfWinBmSolAbsdOutsReveal(SurfNum) = 0.0;
                SurfWinBmSolRefldOutsRevealReport(SurfNum) = 0.0;
                SurfWinBmSolRefldOutsRevealRepEnergy(SurfNum) = 0.0;
                SurfWinOutsRevealDiffOntoGlazing(SurfNum) = 0.0;
                SurfWinOutsRevealDiffOntoFrame(SurfNum) = 0.0;
                // for inside reveals
                SurfWinBmSolAbsdInsReveal(SurfNum) = 0.0;
                SurfWinBmSolAbsdInsRevealReport(SurfNum) = 0.0;
                SurfWinBmSolRefldInsReveal(SurfNum) = 0.0;
                SurfWinBmSolRefldInsRevealReport(SurfNum) = 0.0;
                SurfWinBmSolRefldInsRevealRepEnergy(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoGlazing(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoGlazingReport(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoFrame(SurfNum) = 0.0;
                SurfWinInsRevealDiffOntoFrameReport(SurfNum) = 0.0;
                SurfWinInsRevealDiffIntoZone(SurfNum) = 0.0;
                SurfWinInsRevealDiffIntoZoneReport(SurfNum) = 0.0;

                if ((Surface(SurfNum).ExtBoundCond != ExternalEnvironment &&
                     Surface(SurfNum).ExtBoundCond != OtherSideCondModeledExt))
                    continue;
                if (Surface(SurfNum).Reveal == 0.0 && SurfWinInsideReveal(SurfNum) == 0.0 &&
                    SurfWinInsideSillDepth(SurfNum) == 0.0)
                    continue;
                if (Surface(SurfNum).Sides != 4) continue;
                if (SurfWinInsideSillDepth(SurfNum) < SurfWinInsideReveal(SurfNum)) continue;

                ShadeFlag = SurfWinShadingFlag(SurfNum);
                if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn) continue;

                if (CosIncAng(TimeStep, HourOfDay, SurfNum) <= 0.0) continue;

                tmp_SunlitFracWithoutReveal = SunlitFracWithoutReveal(TimeStep, HourOfDay, SurfNum);

                // Calculate cosine of angle of incidence of beam solar on reveal surfaces,
                // assumed to be perpendicular to window plane

                CosBetaBottom = -SOLCOS(1) * Surface(SurfNum).SinAzim * Surface(SurfNum).CosTilt -
                                SOLCOS(2) * Surface(SurfNum).CosAzim * Surface(SurfNum).CosTilt +
                                SOLCOS(3) * Surface(SurfNum).SinTilt;

                CosBetaLeft = -SOLCOS(1) * Surface(SurfNum).CosAzim - SOLCOS(2) * Surface(SurfNum).SinAzim;

                // Note: CosBetaTop = -CosBetaBottom, CosBetaRight = -CosBetaLeft

                OutsReveal = Surface(SurfNum).Reveal;
                InsReveal = SurfWinInsideReveal(SurfNum);
                InsideRevealSolAbs = 0.0;
                GlazingThickness = SurfWinTotGlazingThickness(SurfNum);
                H = Surface(SurfNum).Height;
                W = Surface(SurfNum).Width;
                d1 = OutsReveal + 0.5 * GlazingThickness;
                ConstrNum = Surface(SurfNum).Construction;
                ConstrNumSh = Surface(SurfNum).activeShadedConstruction;
                if (SurfWinStormWinFlag(SurfNum) == 1) {
                    ConstrNum = Surface(SurfNum).StormWinConstruction;
                    ConstrNumSh = Surface(SurfNum).activeStormWinShadedConstruction;
                }
                SolTransGlass = POLYF(CosIncAng(TimeStep, HourOfDay, SurfNum),
                                      state.dataConstruction->Construct(ConstrNum).TransSolBeamCoef);
                TanProfileAngVert = SurfWinTanProfileAngVert(SurfNum);
                TanProfileAngHor = SurfWinTanProfileAngHor(SurfNum);
                FrameDivNum = Surface(SurfNum).FrameDivider;
                FrameWidth = 0.0;
                if (FrameDivNum != 0) {
                    FrameWidth = FrameDivider(FrameDivNum).FrameWidth;
                    if (FrameWidth > 0.0) {
                        P1 = FrameDivider(FrameDivNum).FrameProjectionOut + 0.5 * GlazingThickness;
                        P2 = FrameDivider(FrameDivNum).FrameProjectionIn + 0.5 * GlazingThickness;
                        if (OutsReveal + 0.5 * GlazingThickness <= P1) d1 = P1 + 0.001;
                    }
                }
                // Loop over vertical and horizontal reveal surfaces
                for (HorVertReveal = 1; HorVertReveal <= 2; ++HorVertReveal) {

                    FracToGlassOuts = 0.5;
                    FracToGlassIns = 0.5;
                    BmSolRefldOutsReveal = 0.0;
                    BmSolRefldInsReveal = 0.0;
                    A1ill = 0.0;
                    A2ill = 0.0;

                    // Added TH. 5/27/2009
                    A1sh = 0.0;
                    A2sh = 0.0;

                    if (HorVertReveal == 1) { // Vertical reveal
                        TanAlpha = TanProfileAngHor;
                        TanGamma = TanProfileAngVert;
                        CosBeta = std::abs(CosBetaLeft);
                        L = Surface(SurfNum).Height;
                        d2 = InsReveal + 0.5 * GlazingThickness;
                        d2prime = d1 + d2 - W / TanGamma;
                        InsideRevealSolAbs = SurfWinInsideRevealSolAbs(SurfNum);
                    } else { // Horizontal reveal
                        InsSillDepth = SurfWinInsideSillDepth(SurfNum);
                        TanAlpha = TanProfileAngVert;
                        TanGamma = TanProfileAngHor;
                        CosBeta = std::abs(CosBetaBottom);
                        L = Surface(SurfNum).Width;
                        if (CosBetaBottom > 0.0) { // Bottom reveal surfaces may be illuminated
                            d2 = InsSillDepth + 0.5 * GlazingThickness;
                            InsideRevealSolAbs = SurfWinInsideSillSolAbs(SurfNum);
                        } else { // Top reveal surfaces may be illuminated
                            d2 = InsReveal + 0.5 * GlazingThickness;
                            InsideRevealSolAbs = SurfWinInsideRevealSolAbs(SurfNum);
                        }
                        d2prime = d1 + d2 - H / TanGamma;
                    }
                    if (d2prime < 0.0) d2prime = 0.0; // No shadow from opposing reveal
                    d12 = d1 + d2 - d2prime;

                    if (FrameWidth <= 0.001) {
                        // Window without frame

                        // Find inside and outside shadowed area of vertical or horizontal reveal surfaces
                        // that can be illuminated by beam solar; shadowing is by other reveal surfaces.

                        if (d2prime <= d2) {
                            if (d12 * TanAlpha <= L) {
                                A1sh = 0.5 * TanAlpha * pow_2(d1);
                                A2sh = d2prime * L + 0.5 * TanAlpha * pow_2(d12) - A1sh;
                            } else { // d12*TanAlpha > L
                                if (d1 * TanAlpha <= L) {
                                    A1sh = 0.5 * TanAlpha * pow_2(d1);
                                    A2sh = d2 * L - 0.5 * TanAlpha * pow_2(L / TanAlpha - d1);
                                } else { // d1*TanAlpha > L
                                    A1sh = d1 * L - (0.5 / TanAlpha) * pow_2(L);
                                    A2sh = d2 * L;
                                }
                            }
                        } else { // d2prime > d2
                            A2sh = d2 * L;
                            if (d2prime < d1 + d2) {
                                if (d12 * TanAlpha <= L) {
                                    A1sh = L * (d2prime - d2) + 0.5 * TanAlpha * pow_2(d12);
                                } else { // d12*TanAlpha > L
                                    A1sh = d1 * L - 0.5 * pow_2(L) / TanAlpha;
                                }
                            } else { // d2prime >= d1+d2
                                A1sh = d1 * L;
                            }
                        }

                        // Added TH. 5/27/2009
                        if (A1sh < 0.0) A1sh = 0.0;
                        if (A2sh < 0.0) A2sh = 0.0;

                        if (OutsReveal >= 0.001) A1ill = d1 * L - A1sh; // A1ill = 0.0 if OutsReveal < 0.001
                        if (InsReveal >= 0.001) A2ill = d2 * L - A2sh;  // A2ill = 0.0 if InsReveal < 0.001

                    } else { // Window with frame; take into account shadowing
                        // of inside reveal surfaces by frame
                        f1 = d1 - P1;
                        f2 = d2 - P2;
                        d2prime2 = FrameWidth / TanGamma;
                        if (HorVertReveal == 1) { // Vertical reveal
                            if (InsReveal + 0.5 * GlazingThickness <= P2) d2 = P2 + 0.001;
                        } else {                       // Horizontal
                            if (CosBetaBottom > 0.0) { // Bottom reveal surfaces may be illuminated
                                if (InsSillDepth + 0.5 * GlazingThickness <= P2) d2 = P2 + 0.001;
                            } else { // Top reveal surfaces may be illuminated
                                if (InsReveal + 0.5 * GlazingThickness <= P2) d2 = P2 + 0.001;
                            }
                        }

                        if (d2prime <= f2) { // Shadow from opposing reveal does not go beyond inside surface of frame

                            if (d12 * TanAlpha <= L) {
                                A1sh = 0.5 * TanAlpha * pow_2(f1);
                                L1 = f1 * (f1 * TanAlpha / (6.0 * L) + 0.5);
                                if (d2 - (d2prime + d2prime2 + P2) >= 0.0) {
                                    A2sh = (d2prime + d2prime2) * L +
                                           0.5 * TanAlpha * (pow_2(d1 + d2 - d2prime) - pow_2(d1 + P2 + d2prime2));
                                    L2 = d2prime2 + 0.5 * (d2 - (d2prime + d2prime2 + P2));
                                } else { // d2-(d2prime+d2prime2+P2) < 0.  ! Inside reveal is fully shadowed by frame and/or opposing reveal
                                    A2sh = f2 * L;
                                    L2 = f2;
                                }
                            } else { // d12*TanAlpha >= L
                                if ((d1 + P2) * TanAlpha <= L) {
                                    A1sh = 0.5 * TanAlpha * pow_2(f1);
                                    L1 = f1 * ((f1 * TanAlpha) / (6.0 * L) + 0.5);
                                    if ((d1 + P2 + d2prime2) * TanAlpha >= L) {
                                        A2sh = f2 * L;
                                        L2 = f2;
                                    } else { // (d1+P2+d2prime2)*TanAlpha < L
                                        A2sh = f2 * L - 0.5 * pow_2(L - (d1 + P2) * TanAlpha) / TanAlpha +
                                               d2prime2 * (L - (d1 + P2 + d2prime2 / 2.0) * TanAlpha);
                                        L2 = d2prime2 + (L / TanAlpha - (d1 + P2 + d2prime2)) / 3.0;
                                    }
                                } else { // (d1+P2)*TanAlpha > L
                                    L2 = f2;
                                    A2sh = f2 * L;
                                    if (f1 * TanAlpha <= L) {
                                        A1sh = 0.5 * TanAlpha * pow_2(f1);
                                        L1 = f1 * ((f1 * TanAlpha) / (6.0 * L) + 0.5);
                                    } else { // f1*TanAlpha > L
                                        A1sh = f1 * L - 0.5 * pow_2(L) / TanAlpha;
                                        L1 = f1 - (L / TanAlpha) / 3.0;
                                    }
                                }
                            }

                        } else { // d2prime > f2   ! Shadow from opposing reveal goes beyond inside of frame

                            A2sh = f2 * L;
                            L2 = f2;
                            if (d2prime >= d1 + d2) {
                                A1sh = 0.0;
                                L1 = f1;
                            } else { // d2prime < d1+d2
                                if (d2prime <= d2 + P1) {
                                    if (f1 * TanAlpha <= L) {
                                        A1sh = 0.5 * TanAlpha * pow_2(f1);
                                        L1 = f1 * ((f1 * TanAlpha) / (6.0 * L) + 0.5);
                                    } else { // f1*TanAlpha > L
                                        A1sh = f1 * L - 0.5 * pow_2(L) / TanAlpha;
                                        L1 = f1 - (L / TanAlpha) / 3.0;
                                    }
                                } else { // d2prime > d2+P1
                                    if (d12 * TanAlpha <= L) {
                                        A1sh = L * (d2prime - (d2 + P1)) + 0.5 * TanAlpha * pow_2(d12);
                                        L1 = (L * (f1 - d12 / 2.0) - d12 * TanAlpha * (f1 / 2 - d12 / 3.0)) /
                                             (L - d12 * TanAlpha / 2.0);
                                    } else { // d12*TanAlpha > L
                                        A1sh = f1 * L - 0.5 * pow_2(L) / TanAlpha;
                                        L1 = f1 - (L / TanAlpha) / 3.0;
                                    }
                                }
                            }
                        }

                        // Added TH. 5/27/2009
                        if (A1sh < 0.0) A1sh = 0.0;
                        if (A2sh < 0.0) A2sh = 0.0;

                        if (OutsReveal >= P1 + 0.5 * GlazingThickness + 0.001) A1ill = L * f1 - A1sh;
                        if (InsReveal >= P2 + 0.5 * GlazingThickness + 0.001) A2ill = L * f2 - A2sh;
                        if (L1 == 0.0) {
                            FracToGlassOuts = 0.0;
                        } else {
                            FracToGlassOuts = 0.5 * (1.0 - std::atan(FrameWidth / L1) / DataGlobalConstants::PiOvr2());
                        }
                        if (L2 == 0.0) {
                            FracToGlassIns = 0.0;
                        } else {
                            FracToGlassIns = 0.5 * (1.0 - std::atan(FrameWidth / L2) / DataGlobalConstants::PiOvr2());
                        }
                    } // End of check if window has frame

                    // Added TH. 5/27/2009
                    if (A1ill < 0.0) A1ill = 0.0;
                    if (A2ill < 0.0) A2ill = 0.0;

                    // Quantities related to outside reveal
                    if (A1ill > 1.0e-6) {

                        SurfWinBmSolAbsdOutsReveal(SurfNum) +=
                                A1ill * SurfWinOutsideRevealSolAbs(SurfNum) * CosBeta * tmp_SunlitFracWithoutReveal;

                        BmSolRefldOutsReveal = A1ill * (1.0 - SurfWinOutsideRevealSolAbs(SurfNum)) * CosBeta *
                                               tmp_SunlitFracWithoutReveal;

                        SurfWinBmSolRefldOutsRevealReport(SurfNum) += BeamSolarRad * BmSolRefldOutsReveal;
                        SurfWinBmSolRefldOutsRevealRepEnergy(SurfNum) =
                                SurfWinBmSolRefldOutsRevealReport(SurfNum) * TimeStepZoneSec;

                        // Reflected solar from outside horizontal and vertical reveal incident on glazing
                        SurfWinOutsRevealDiffOntoGlazing(SurfNum) +=
                                FracToGlassOuts * BmSolRefldOutsReveal / Surface(SurfNum).Area;

                        if (FrameWidth > 0.0) {
                            // Reflected solar from outside horizontal and vertical reveal incident on frame
                            SurfWinOutsRevealDiffOntoFrame(SurfNum) +=
                                    (0.5 - FracToGlassOuts) * BmSolRefldOutsReveal / SurfWinFrameArea(SurfNum);
                        }

                    } // End of check if A1ill > 0.0 (actually 10^-6)

                    // Quantities related to inside reveal; inside reveal reflection/absorption is assumed
                    // to occur only if an interior shade or blind is not in place.

                    if (ShadeFlag <= 0 || ShadeFlag == SwitchableGlazing) {

                        if (A2ill > 1.0e-6) {

                            DiffReflGlass = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                            if (ShadeFlag == SwitchableGlazing) {
                                SolTransGlassSh = POLYF(CosIncAng(TimeStep, HourOfDay, SurfNum),
                                                        state.dataConstruction->Construct(ConstrNumSh).TransSolBeamCoef);
                                SolTransGlass = InterpSw(SurfWinSwitchingFactor(SurfNum), SolTransGlass,
                                                         SolTransGlassSh);
                                DiffReflGlassSh = state.dataConstruction->Construct(ConstrNumSh).ReflectSolDiffBack;
                                DiffReflGlass = InterpSw(SurfWinSwitchingFactor(SurfNum), DiffReflGlass,
                                                         DiffReflGlassSh);
                            }

                            // Calc beam solar sbsorbed (m2)
                            SurfWinBmSolAbsdInsReveal(SurfNum) +=
                                    A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal;

                            // Added TH 5/26/2009 for reporting purpose - Beam solar absorbed by the inside reveal (W)
                            SurfWinBmSolAbsdInsRevealReport(SurfNum) +=
                                    BeamSolarRad * A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta *
                                    tmp_SunlitFracWithoutReveal;

                            // in m2 = Area * solar transmitted fraction * inside reveal reflection fraction
                            BmSolRefldInsReveal = A2ill * SolTransGlass * (1.0 - InsideRevealSolAbs) * CosBeta *
                                                  tmp_SunlitFracWithoutReveal;

                            SurfWinBmSolRefldInsReveal(SurfNum) += BmSolRefldInsReveal;

                            SurfWinBmSolRefldInsRevealReport(SurfNum) +=
                                    BeamSolarRad * BmSolRefldInsReveal; // W, BeamSolarRad in W/m2
                            SurfWinBmSolRefldInsRevealRepEnergy(SurfNum) =
                                    SurfWinBmSolRefldInsRevealReport(SurfNum) * TimeStepZoneSec;

                            // Reflected solar from inside horizontal and vertical reveal incident on glazing
                            SurfWinInsRevealDiffOntoGlazing(SurfNum) +=
                                    FracToGlassIns * BmSolRefldInsReveal / Surface(SurfNum).Area;

                            // Added TH 5/26/2009 for reporting purpose - diffuse on window glass from inside reveal (W)
                            SurfWinInsRevealDiffOntoGlazingReport(SurfNum) +=
                                    BeamSolarRad * FracToGlassIns * BmSolRefldInsReveal;

                            // Reflected solar from inside horizontal and vertical reveal incident on frame
                            if (FrameWidth > 0.0) {
                                SurfWinInsRevealDiffOntoFrame(SurfNum) +=
                                        (0.5 - FracToGlassIns) * BmSolRefldInsReveal / SurfWinFrameArea(SurfNum);

                                // Added TH 5/26/2009 for reporting purpose - diffuse on window frame from inside reveal (W)
                                SurfWinInsRevealDiffOntoFrameReport(SurfNum) +=
                                        BeamSolarRad * (0.5 - FracToGlassIns) * BmSolRefldInsReveal;
                            }

                            // Reflected solar from inside reveal going directly into zone and reflected from glass.
                            // Assumes half of solar reflected from inside reveal goes as diffuse radiation into the zone and
                            // half goes as diffuse radiation towards window.
                            SurfWinInsRevealDiffIntoZone(SurfNum) +=
                                    BmSolRefldInsReveal * (0.5 + DiffReflGlass * FracToGlassIns);

                            // Added TH 5/26/2009 for reporting purpose - diffuse into zone from inside reveal (W)
                            SurfWinInsRevealDiffIntoZoneReport(SurfNum) +=
                                    BeamSolarRad * BmSolRefldInsReveal * (0.5 + DiffReflGlass * FracToGlassIns);

                        } // End of check if A2ill > 0.0 (actually 10^-6)

                    } // End of check if interior shade or blind is in place

                } // End of loop over vertical and horizontal reveal
            }

        } // End of surface loop
    }

    void ReportSurfaceShading()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine uses the internal variables used in the Shading
        // calculations and prepares them for reporting (at timestep level).

        // METHODOLOGY EMPLOYED:
        // Because all of the calculations are done on a "daily" basis in this
        // module, it is difficult to formulate the values that might be useful
        // for reporting.  SunlitFrac was the first of these two arrays to be
        // made into "two dimensions".  It is not clear that both have to be
        // two dimensions.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace OutputReportPredefined;

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
        int SurfNum; // Loop Counter
        int RepCol;  // the column of the predefined report

        for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
            SurfSunlitFrac(SurfNum) = SunlitFrac(TimeStep, HourOfDay, SurfNum);
            SurfSunlitArea(SurfNum) = SunlitFrac(TimeStep, HourOfDay, SurfNum) * Surface(SurfNum).Area;
        }
        // added for predefined reporting
        RepCol = 0;
        if (Month == 3 && DayOfMonth == 21) {
            if ((HourOfDay == 9) && (TimeStep == 4)) {
                RepCol = pdchSlfMar21_9;
            } else if ((HourOfDay == 12) && (TimeStep == 4)) {
                RepCol = pdchSlfMar21_12;
            } else if ((HourOfDay == 15) && (TimeStep == 4)) {
                RepCol = pdchSlfMar21_15;
            }
        } else if (Month == 6 && DayOfMonth == 21) {
            if ((HourOfDay == 9) && (TimeStep == 4)) {
                RepCol = pdchSlfJun21_9;
            } else if ((HourOfDay == 12) && (TimeStep == 4)) {
                RepCol = pdchSlfJun21_12;
            } else if ((HourOfDay == 15) && (TimeStep == 4)) {
                RepCol = pdchSlfJun21_15;
            }
        } else if (Month == 12 && DayOfMonth == 21) {
            if ((HourOfDay == 9) && (TimeStep == 4)) {
                RepCol = pdchSlfDec21_9;
            } else if ((HourOfDay == 12) && (TimeStep == 4)) {
                RepCol = pdchSlfDec21_12;
            } else if ((HourOfDay == 15) && (TimeStep == 4)) {
                RepCol = pdchSlfDec21_15;
            }
        }
        if (RepCol != 0) {
            for (SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum) {
                if (Surface(SurfNum).Class == SurfaceClass::Window) {
                    PreDefTableEntry(RepCol, Surface(SurfNum).Name, SurfSunlitFrac(SurfNum));
                }
            }
        }
    }

    void ReportSurfaceErrors(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   November 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine reports some recurring type errors that can get mixed up with more important
        // errors in the error file.

        using namespace DataErrorTracking; // for error tracking
        using General::RoundSigDigits;

        static Array1D_string const MSG(4, {"misses", "", "within", "overlaps"});

        int Loop1;
        int Loop2;
        int Count;
        int TotCount;
        Array1D_bool SurfErrorReported;
        Array1D_bool SurfErrorReported2;

        if (state.dataSolarShading->NumTooManyFigures + state.dataSolarShading->NumTooManyVertices + state.dataSolarShading->NumBaseSubSurround > 0) {
            ShowMessage("");
            ShowMessage("===== Recurring Surface Error Summary =====");
            ShowMessage("The following surface error messages occurred.");
            ShowMessage("");

            if (state.dataSolarShading->NumBaseSubSurround > 0) {
                ShowMessage("Base Surface does not surround subsurface errors occuring...");
                ShowMessage(
                    "Check that the GlobalGeometryRules object is expressing the proper starting corner and direction [CounterClockwise/Clockwise]");
                ShowMessage("");
            }

            SurfErrorReported.dimension(TotSurfaces, false);
            TotCount = 0;
            for (Loop1 = 1; Loop1 <= state.dataSolarShading->NumBaseSubSurround; ++Loop1) {
                Count = 0;
                if (SurfErrorReported(state.dataSolarShading->TrackBaseSubSurround(Loop1).SurfIndex1)) continue;
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumBaseSubSurround; ++Loop2) {
                    if (state.dataSolarShading->TrackBaseSubSurround(Loop1).SurfIndex1 == state.dataSolarShading->TrackBaseSubSurround(Loop2).SurfIndex1 &&
                        state.dataSolarShading->TrackBaseSubSurround(Loop1).MiscIndex == state.dataSolarShading->TrackBaseSubSurround(Loop2).MiscIndex) {
                        ++Count;
                    }
                }
                TotCount += Count;
                TotalWarningErrors += Count - 1;
                ShowWarningError("Base surface does not surround subsurface (CHKSBS), Overlap Status=" +
                                 state.dataSolarShading->cOverLapStatus(state.dataSolarShading->TrackBaseSubSurround(Loop1).MiscIndex));
                ShowContinueError("  The base surround errors occurred " + std::to_string(Count) + " times.");
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumBaseSubSurround; ++Loop2) {
                    if (state.dataSolarShading->TrackBaseSubSurround(Loop1).SurfIndex1 == state.dataSolarShading->TrackBaseSubSurround(Loop2).SurfIndex1 &&
                        state.dataSolarShading->TrackBaseSubSurround(Loop1).MiscIndex == state.dataSolarShading->TrackBaseSubSurround(Loop2).MiscIndex) {
                        ShowContinueError("Surface \"" + Surface(state.dataSolarShading->TrackBaseSubSurround(Loop1).SurfIndex1).Name + "\" " +
                                          MSG(state.dataSolarShading->TrackBaseSubSurround(Loop1).MiscIndex) + " SubSurface \"" +
                                          Surface(state.dataSolarShading->TrackBaseSubSurround(Loop2).SurfIndex2).Name + "\"");
                    }
                }
                SurfErrorReported(state.dataSolarShading->TrackBaseSubSurround(Loop1).SurfIndex1) = true;
            }
            if (TotCount > 0) {
                ShowMessage("");
                ShowContinueError("  The base surround errors occurred " + std::to_string(TotCount) + " times (total).");
                ShowMessage("");
            }

            SurfErrorReported2.allocate(TotSurfaces);
            SurfErrorReported = false;
            TotCount = 0;
            if (state.dataSolarShading->NumTooManyVertices > 0) {
                ShowMessage("Too many vertices [>=" + RoundSigDigits(state.dataSolarShading->MaxHCV) + "] in shadow overlap errors occurring...");
                ShowMessage("These occur throughout the year and may occur several times for the same surfaces. You may be able to reduce them by "
                            "adding Output:Diagnostics,DoNotMirrorDetachedShading;");
            }
            for (Loop1 = 1; Loop1 <= state.dataSolarShading->NumTooManyVertices; ++Loop1) {
                Count = 0;
                SurfErrorReported2 = false;
                if (SurfErrorReported(state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1)) continue;
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumTooManyVertices; ++Loop2) {
                    if (state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1 == state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex1) {
                        ++Count;
                    }
                }
                TotCount += Count;
                TotalWarningErrors += Count - 1;
                ShowMessage("");
                ShowWarningError("Too many vertices [>=" + RoundSigDigits(state.dataSolarShading->MaxHCV) + "] in a shadow overlap");
                ShowContinueError("Overlapping figure=" + Surface(state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1).Name + ", Surface Class=[" +
                                  cSurfaceClass(Surface(state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1).Class) + ']');
                ShowContinueError("  This error occurred " + std::to_string(Count) + " times.");
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumTooManyVertices; ++Loop2) {
                    if (state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1 == state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex1) {
                        if (SurfErrorReported2(state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex2)) continue;
                        ShowContinueError("Figure being Overlapped=" + Surface(state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex2).Name + ", Surface Class=[" +
                                          cSurfaceClass(Surface(state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex2).Class) + ']');
                        SurfErrorReported2(state.dataSolarShading->TrackTooManyVertices(Loop2).SurfIndex2) = true;
                    }
                }
                SurfErrorReported(state.dataSolarShading->TrackTooManyVertices(Loop1).SurfIndex1) = true;
            }
            if (TotCount > 0) {
                ShowMessage("");
                ShowContinueError("  The too many vertices errors occurred " + std::to_string(TotCount) + " times (total).");
                ShowMessage("");
            }

            SurfErrorReported = false;
            TotCount = 0;
            if (state.dataSolarShading->NumTooManyFigures > 0) {
                ShowMessage("Too many figures [>=" + RoundSigDigits(state.dataSolarShading->MaxHCS) + "] in shadow overlap errors occurring...");
                ShowMessage("These occur throughout the year and may occur several times for the same surfaces. You may be able to reduce them by "
                            "adding OutputDiagnostics,DoNotMirrorDetachedShading;");
            }
            for (Loop1 = 1; Loop1 <= state.dataSolarShading->NumTooManyFigures; ++Loop1) {
                Count = 0;
                SurfErrorReported2 = false;
                if (SurfErrorReported(state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1)) continue;
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumTooManyFigures; ++Loop2) {
                    if (state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1 == state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex1) {
                        ++Count;
                    }
                }
                TotCount += Count;
                TotalWarningErrors += Count - 1;
                ShowMessage("");
                ShowWarningError("Too many figures [>=" + RoundSigDigits(state.dataSolarShading->MaxHCS) + "] in a shadow overlap");
                ShowContinueError("Overlapping figure=" + Surface(state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1).Name + ", Surface Class=[" +
                                  cSurfaceClass(Surface(state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1).Class) + ']');
                ShowContinueError("  This error occurred " + std::to_string(Count) + " times.");
                for (Loop2 = 1; Loop2 <= state.dataSolarShading->NumTooManyFigures; ++Loop2) {
                    if (state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1 == state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex1) {
                        if (SurfErrorReported2(state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex2)) continue;
                        ShowContinueError("Figure being Overlapped=" + Surface(state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex2).Name + ", Surface Class=[" +
                                          cSurfaceClass(Surface(state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex2).Class) + ']');
                        SurfErrorReported2(state.dataSolarShading->TrackTooManyFigures(Loop2).SurfIndex2) = true;
                    }
                }
                SurfErrorReported(state.dataSolarShading->TrackTooManyFigures(Loop1).SurfIndex1) = true;
            }
            if (TotCount > 0) {
                ShowMessage("");
                ShowContinueError("  The too many figures errors occurred " + std::to_string(TotCount) + " times (total).");
                ShowMessage("");
            }
            SurfErrorReported.deallocate();
            SurfErrorReported2.deallocate();
        }
    }

    void ComputeWinShadeAbsorpFactors(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Winkelmann
        //       DATE WRITTEN   Mar 2001
        //       MODIFIED       Oct 2002,FCW: change ConstrNumSh = WindowShadingControl(WinShadeCtrlNum)%ShadedConstruction
        //                      to Surface(SurfNum)%ShadedConstruction
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Called by InitSolarCalculations. Finds fractions that apportion radiation absorbed by a
        // window shade to the two faces of the shade. For radiation incident from the left,
        // ShadeAbsFacFace(1) is the fraction of radiation absorbed in the left-hand half of the
        // of the shade and ShadeAbsFacFace(2) is the fraction absorbed in the right-hand half.
        // The shade is assumed to be homogeneous.

        // REFERENCES: See EnergyPlus engineering documentation
        // USE STATEMENTS: na

        for (int zoneNum = 1; zoneNum <= NumOfZones; ++zoneNum) {
            int const firstSurfWin = Zone(zoneNum).WindowSurfaceFirst;
            int const lastSurfWin = Zone(zoneNum).WindowSurfaceLast;
            if (firstSurfWin == -1) continue;
            for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
                if (Surface(SurfNum).Class == SurfaceClass::Window && Surface(SurfNum).HasShadeControl) {
                    int WinShadeCtrlNum = Surface(SurfNum).activeWindowShadingControl; // Window shading control number

                    int MatNumSh = 0; // Shade layer material number
                    Real64 AbsorpEff = 0.0;    // Effective absorptance of isolated shade layer (fraction of
                    //  of incident radiation remaining after reflected portion is
                    //  removed that is absorbed
                    if (WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_InteriorShade ||
                        WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_ExteriorShade ||
                        WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_BetweenGlassShade) {
                        int ConstrNumSh = Surface(SurfNum).activeShadedConstruction; // Window construction number with shade
                        int TotLay = state.dataConstruction->Construct(ConstrNumSh).TotLayers; // Total layers in a construction


                        if (WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_InteriorShade) {
                            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(TotLay); // Interior shade
                        } else if (WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_ExteriorShade) {
                            MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(1); // Exterior shade
                        } else if (WindowShadingControl(WinShadeCtrlNum).ShadingType == WSC_ST_BetweenGlassShade) {
                            if (state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers == 2) {
                                // Double pane with between-glass shade
                                MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(3);
                            } else {
                                // Triple pane with between-glass shade
                                MatNumSh = state.dataConstruction->Construct(ConstrNumSh).LayerPoint(5);
                            }
                        }
                        AbsorpEff = dataMaterial.Material(MatNumSh).AbsorpSolar /
                                    (dataMaterial.Material(MatNumSh).AbsorpSolar +
                                     dataMaterial.Material(MatNumSh).Trans + 0.0001);
                        AbsorpEff = min(max(AbsorpEff, 0.0001),
                                        0.999); // Constrain to avoid problems with following log eval
                        SurfWinShadeAbsFacFace1(SurfNum) = (1.0 - std::exp(0.5 * std::log(1.0 - AbsorpEff))) / AbsorpEff;
                        SurfWinShadeAbsFacFace2(SurfNum) = 1.0 - SurfWinShadeAbsFacFace1(SurfNum);
                    }
                }
            }
        }
    }

    void CalcWinTransDifSolInitialDistribution(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rob Hitchcock
        //       DATE WRITTEN   July 2007
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the initial distribution
        // of diffuse solar transmitted through exterior windows
        // to individual heat transfer surfaces in each zone(or enclosure).

        // METHODOLOGY EMPLOYED:
        // Apportions diffuse solar transmitted through each exterior window
        // that is then absorbed, reflected, and/or transmitted
        // by other heat transfer surfaces in the zone.
        // Calculations use:
        // 1. WinDifSolar calculated in SUBROUTINE CalcInteriorSolarDistribution,
        // 2. view factors between each exterior window and
        // other heat transfer surfaces in a zone
        // calculated in SUBROUTINE CalcApproximateViewFactors, and
        // 3. surface absorptances, reflectances, and transmittances
        // determined here using revised code from SUBROUTINE InitIntSolarDistribution

        // Using/Aliasing
        using General::InterpSlatAng;
        using General::InterpSw;
        using ScheduleManager::GetCurrentScheduleValue;
        using namespace DataViewFactorInformation;
        using DataHeatBalance::SurfWinInitialDifSolwinAbs;
        using DataHeatBalance::InitialZoneDifSolReflW;
        using DataHeatBalSurface::SurfOpaqInitialDifSolInAbs;
        using DataHeatBalSurface::SurfWinInitialDifSolInTrans;
        using namespace DataWindowEquivalentLayer;

        Real64 AbsInt;               // Tmp var for Inside surface short-wave absorptance
        Real64 MovInsulSchedVal;     // Value of the movable insulation schedule for current time
        Real64 HMovInsul;            // Conductance of movable insulation
        Real64 InsideDifAbsorptance; // Inside diffuse solar absorptance of a surface
        Real64 InsideDifReflectance; // Inside diffuse solar reflectance of a surface
        int BlNum;                   // Blind number
        Real64 BlAbsDiffBk;          // Glass layer back diffuse solar absorptance when blind in place
        Real64 AbsDiffBkBl;          // Blind diffuse back solar absorptance as part of glazing system

        //  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
        //  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
        //  INTEGER :: MatNumGl           ! Glass layer material number
        //  INTEGER :: MatNumSh           ! Shade layer material number
        //  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

        Real64 ViewFactor;                  // temp var for view factor
        Real64 ViewFactorTotal;             // debug var for view factor total
        Real64 WinDifSolarTrans;            // debug var for WinDifSolar() [W]
        Real64 WinDifSolarDistTotl;         // debug var for window total distributed diffuse solar [W]
        Real64 WinDifSolarDistAbsorbedTotl; // debug var for individual exterior window total distributed
        //    diffuse solar absorbed [W]
        Real64 WinDifSolarDistReflectedTotl; // debug var for individual exterior window total distributed
        //    diffuse solar reflected [W]
        Real64 WinDifSolarDistTransmittedTotl; // debug var for individual exterior window total distributed
        //    diffuse solar transmitted [W]
        Real64 WinDifSolLayAbsW;                // temp var for diffuse solar absorbed by individual glass layer [W]
        Real64 ZoneDifSolarTrans;               // debug var for WinDifSolar() [W]
        Real64 ZoneDifSolarDistTotl;            // debug var for zone total distributed diffuse solar [W]
        Real64 ZoneDifSolarDistAbsorbedTotl;    // debug var for zone total distributed diffuse solar absorbed [W]
        Real64 ZoneDifSolarDistReflectedTotl;   // debug var for zone total distributed diffuse solar reflected [W]
        Real64 ZoneDifSolarDistTransmittedTotl; // debug var for zone total distributed diffuse solar transmitted [W]

        Real64 DifSolarAbsW;     // temp var for diffuse solar absorbed by surface [W]
        Real64 DifSolarAbs;      // temp var for diffuse solar absorbed by surface [W/m2]
        Real64 DifSolarReflW;    // temp var for diffuse solar reflected by surface [W]
        Real64 DifSolarTransW;   // temp var for diffuse solar transmitted through interior window surface [W]
        Real64 ShBlDifSolarAbsW; // temp var for diffuse solar absorbed by shade/blind [W]

        Array2D<Real64> AbsSolBeamEQL(2, CFSMAXNL + 1);     // absorbed exterior beam radiation by layers fraction
        Array2D<Real64> AbsSolDiffEQL(2, CFSMAXNL + 1);     // absorbed exterior diffuse radiation by layers fraction
        Array2D<Real64> AbsSolBeamBackEQL(2, CFSMAXNL + 1); // absorbed interior beam radiation by layers fraction from back
        Array2D<Real64> AbsSolDiffBackEQL(2, CFSMAXNL + 1); // absorbed exterior diffuse radiation by layers fraction from back
        int EQLNum;                                         // equivalent layer fenestration index
        int Lay;                                            // equivalent layer fenestration layer index

        // Init accumulators for absorbed diffuse solar for all surfaces for later heat balance calcs
        SurfOpaqInitialDifSolInAbs = 0.0;
        SurfWinInitialDifSolwinAbs = 0.0;

        // Init accumulator for total reflected diffuse solar within each zone for interreflection calcs
        InitialZoneDifSolReflW = 0.0;

        // Init accumulator for transmitted diffuse solar for all surfaces for reporting
        SurfWinInitialDifSolInTrans = 0.0;

        // Loop over all zones doing initial distribution of diffuse solar to interior heat transfer surfaces
        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++enclosureNum) {
            auto & thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));
            // Init Zone accumulators for debugging
            ZoneDifSolarTrans = 0.0;
            ZoneDifSolarDistAbsorbedTotl = 0.0;
            ZoneDifSolarDistReflectedTotl = 0.0;
            ZoneDifSolarDistTransmittedTotl = 0.0;
            // Loop over all diffuse solar transmitting surfaces (i.e., exterior windows and TDDs) in the current zone
            for (int const DifTransSurfNum : thisEnclosure.SurfacePtr) {
                // Skip surfaces that are not exterior, except for TDD_Diffusers
                if (((Surface(DifTransSurfNum).ExtBoundCond != ExternalEnvironment) &&
                     (Surface(DifTransSurfNum).ExtBoundCond != OtherSideCondModeledExt)) &&
                    SurfWinOriginalClass(DifTransSurfNum) != SurfaceClass::TDD_Diffuser)
                    continue;

                // Do I need to do anything special for TDDs?
                //				if ( SurfaceWindow( DifTransSurfNum ).OriginalClass == SurfaceClass::TDD_Diffuser ) {
                //				}

                // Skip surfaces that are not exterior windows or TDD diffusers
                if (Surface(DifTransSurfNum).Class != SurfaceClass::Window &&
                    SurfWinOriginalClass(DifTransSurfNum) != SurfaceClass::TDD_Diffuser)
                    continue;

                //----------------------------------------------------------------------------------------------------------
                // DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH EXTERIOR WINDOWS AND TDDS TO INTERIOR HEAT TRANSFER SURFACES
                //----------------------------------------------------------------------------------------------------------

                // Init transmitted solar debug vars
                ViewFactorTotal = 0.0;
                WinDifSolarTrans = SurfWinDifSolar(DifTransSurfNum);
                ZoneDifSolarTrans += WinDifSolarTrans;

                // Init Exterior Window accumulators for debugging
                WinDifSolarDistAbsorbedTotl = 0.0;
                WinDifSolarDistReflectedTotl = 0.0;
                WinDifSolarDistTransmittedTotl = 0.0;

                // Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
                for (int const HeatTransSurfNum : thisEnclosure.SurfacePtr) {
                    // Skip surfaces that are not heat transfer surfaces
                    // Skip tubular daylighting device domes
                    if (Surface(HeatTransSurfNum).Class == SurfaceClass::TDD_Dome) continue;

                    // View factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                    int const HTenclosureSurfNum = Surface(HeatTransSurfNum).SolarEnclSurfIndex; // HT surface index for ZoneSolarInfo.SurfacePtr and F arrays
                    int const enclosureNum = Surface(HeatTransSurfNum).SolarEnclIndex; // index for ZoneSolarInfo
                    int const DTenclSurfNum = Surface(DifTransSurfNum).SolarEnclSurfIndex; // Window surface index for ZoneSolarInfo.SurfacePtr and F arrays

                    ViewFactor = DataViewFactorInformation::ZoneSolarInfo(enclosureNum).F(HTenclosureSurfNum, DTenclSurfNum);
                    // debug ViewFactorTotal
                    ViewFactorTotal += ViewFactor; // debug

                    // Skip receiving surfaces with 0.0 view factor
                    if (ViewFactor <= 0.0) continue;

                    Real64 const WinDifSolarTrans_Factor(WinDifSolarTrans * ViewFactor);
                    Real64 const win_SwitchingFactor(SurfWinSwitchingFactor(HeatTransSurfNum));
                    Real64 const per_HTSurfaceArea(1.0 / Surface(HeatTransSurfNum).Area);
                    Real64 const HTsurf_slat_ang(SurfWinSlatAngThisTS(HeatTransSurfNum));
                    bool const HTsurf_movable_slats(SurfWinMovableSlats(HeatTransSurfNum));

                    // Calculate diffuse solar from current exterior window absorbed and reflected by current heat transfer surface
                    // And calculate transmitted diffuse solar to adjacent zones through interior windows
                    int ConstrNum = Surface(HeatTransSurfNum).Construction;
                    if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) { // Interior Opaque Surface

                        // Determine the inside (back) diffuse solar absorptance
                        // and reflectance of the current heat transfer surface
                        InsideDifAbsorptance = state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar;
                        // Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                        // Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                        // HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                        HMovInsul = 0.0;
                        if (Surface(HeatTransSurfNum).MaterialMovInsulInt > 0) {
                            MovInsulSchedVal = GetCurrentScheduleValue(Surface(HeatTransSurfNum).SchedMovInsulInt);
                            if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
                                HMovInsul = 0.0;
                            } else { // Movable insulation present
                                HMovInsul = 1.0 / (MovInsulSchedVal * dataMaterial.Material(Surface(HeatTransSurfNum).MaterialMovInsulInt).Resistance);
                                AbsInt = dataMaterial.Material(Surface(HeatTransSurfNum).MaterialMovInsulInt).AbsorpSolar;
                            }
                        }
                        if (HMovInsul > 0.0) InsideDifAbsorptance = AbsInt; // Movable inside insulation present
                        // Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
                        InsideDifReflectance = 1.0 - InsideDifAbsorptance;

                        // Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
                        //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                        //    * current surface inside solar absorptance
                        DifSolarAbsW = WinDifSolarTrans_Factor * InsideDifAbsorptance; // [W]

                        // Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
                        //                                 / current surface net area
                        DifSolarAbs = DifSolarAbsW * per_HTSurfaceArea;

                        // Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
                        SurfOpaqInitialDifSolInAbs(HeatTransSurfNum) += DifSolarAbs;

                        // Reflected diffuse solar [W] = current window transmitted diffuse solar
                        //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                        //    * current window inside solar reflectance
                        DifSolarReflW = WinDifSolarTrans_Factor * InsideDifReflectance;

                        // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                        InitialZoneDifSolReflW(enclosureNum) += DifSolarReflW; // [W]

                        // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                        // For opaque surfaces all incident diffuse is either absorbed or reflected
                        WinDifSolarDistAbsorbedTotl += DifSolarAbsW;    // debug [W]
                        WinDifSolarDistReflectedTotl += DifSolarReflW;  // debug [W]
                        ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW;   // debug [W]
                        ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]
                    } else { // Exterior or Interior Window

                        int ConstrNumSh = Surface(HeatTransSurfNum).activeShadedConstruction;
                        if (SurfWinStormWinFlag(HeatTransSurfNum) == 1) {
                            ConstrNum = Surface(HeatTransSurfNum).StormWinConstruction;
                            ConstrNumSh = Surface(HeatTransSurfNum).activeStormWinShadedConstruction;
                        }
                        int TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                        int ShadeFlag = SurfWinShadingFlag(HeatTransSurfNum);

                        if (SurfWinWindowModelType(HeatTransSurfNum) != WindowEQLModel) {
                            if (ShadeFlag <= 0) { // No window shading
                                // Init accumulator for transmittance calc below
                                DifSolarAbsW = 0.0;

                                // Calc diffuse solar absorbed by all window glass layers
                                // Note: I am assuming here that individual glass layer absorptances have been corrected
                                //       to account for layer by layer transmittance and reflection effects.
                                for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                                    // Calc diffuse solar absorbed from the inside by each window glass layer [W]
                                    AbsInt = state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                                    WinDifSolLayAbsW = WinDifSolarTrans_Factor * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);

                                    // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                                    DifSolarAbsW += WinDifSolLayAbsW;

                                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                    WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW;  // debug
                                    ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

                                    // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                                    SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += WinDifSolLayAbsW * per_HTSurfaceArea;
                                }

                                // Calc diffuse solar reflected back to zone
                                // I don't really care if this is a window or opaque surface since I am just
                                // accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
                                // Reflected diffuse solar [W] = current window transmitted diffuse solar
                                //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                                //    * current window inside solar reflectance
                                InsideDifReflectance = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                                DifSolarReflW = WinDifSolarTrans_Factor * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;

                                // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                                InitialZoneDifSolReflW(enclosureNum) += DifSolarReflW; // [W]

                                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistReflectedTotl += DifSolarReflW;  // debug
                                ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                                //------------------------------------------------------------------------------
                                // DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
                                //------------------------------------------------------------------------------

                                // If this receiving window surface (HeatTransSurfNum) is an interior window,
                                // calc distributed solar transmitted to adjacent zone [W]
                                // NOTE: This calc is here because interior windows are currently assumed to have no shading

                                // Get the adjacent surface number for this receiving window surface
                                int AdjSurfNum = Surface(HeatTransSurfNum).ExtBoundCond;
                                // If the adjacent surface number is > 0, this is an interior window
                                if (AdjSurfNum > 0) { // this is an interior window surface

                                    // Calc diffuse solar from current exterior window
                                    // transmitted through this interior window to adjacent zone [W]
                                    // Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
                                    //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                                    //    - diffuse absorbed by this interior window
                                    //    - diffuse reflected by this interior window
                                    DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
                                    // HERE 8/15/07 Note Construct(AdjConstrNum)%TransDiff could be used here since the "front" transmittance for an
                                    // interior window in the adjacent zone is the correct direction as long as I use the Construct() of the Surface
                                    // in the adjacent zone. However, the above calculation better conserves energy, although possibly at the expense
                                    // of less accurate transmittance calcs. Preliminary tests showed fairly good agreement between the two
                                    // DifSolarTransW calculation methods, but for consistency I stuck with the above.
                                    // int AdjConstrNum = Surface(AdjSurfNum).Construction;
                                    //              DifSolarTransW = WinDifSolar(DifTransSurfNum) &
                                    //                                * ViewFactor &
                                    //                                * Construct(AdjConstrNum)%TransDiff

                                    // Get the adjacent zone index
                                    int const adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;

                                    // Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
                                    CalcInteriorWinTransDifSolInitialDistribution(state, adjEnclosureNum, AdjSurfNum, DifSolarTransW);

                                } else { // this is an exterior window surface

                                    // Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                                    // This is not very effective since it assigns whatever distributed diffuse solar has not been
                                    // absorbed or reflected to transmitted.
                                    DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;

                                } // this is an interior window surface

                                // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistTransmittedTotl += DifSolarTransW;  // debug [W]
                                ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                                // Accumulate transmitted diffuse solar for reporting
                                SurfWinInitialDifSolInTrans(HeatTransSurfNum) += DifSolarTransW * per_HTSurfaceArea;

                            } else if (ConstrNumSh != 0 && (ShadeFlag == IntShadeOn || ShadeFlag >= 3)) {
                                // Interior, exterior or between-glass shade, screen or blind in place

                                // Init accumulator for transmittance calc below
                                DifSolarAbsW = 0.0;
                                WinDifSolLayAbsW = 0.0;

                                // First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
                                auto const &construct_sh(state.dataConstruction->Construct(ConstrNumSh));
                                auto const &construct_sh_AbsDiffBack(construct_sh.AbsDiffBack);
                                auto const &construct_sh_BlAbsDiffBack(construct_sh.BlAbsDiffBack);
                                for (int IGlass = 1; IGlass <= construct_sh.TotGlassLayers; ++IGlass) {
                                    if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                                        // Calc diffuse solar absorbed in each window glass layer and shade
                                        WinDifSolLayAbsW = WinDifSolarTrans_Factor * construct_sh_AbsDiffBack(IGlass);
                                    }

                                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                        BlAbsDiffBk = InterpSlatAng(HTsurf_slat_ang, HTsurf_movable_slats, construct_sh_BlAbsDiffBack(_, IGlass));
                                        // Calc diffuse solar absorbed in each window glass layer and shade
                                        WinDifSolLayAbsW = WinDifSolarTrans_Factor * BlAbsDiffBk;
                                    }

                                    // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                                    DifSolarAbsW += WinDifSolLayAbsW;

                                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                    WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW;  // debug
                                    ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

                                    // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                                    SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += WinDifSolLayAbsW * per_HTSurfaceArea;
                                }

                                // Next calc diffuse solar reflected back to zone from window with shade or blind on
                                // Diffuse back solar reflectance, bare glass or shade on
                                InsideDifReflectance = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                                if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn) {
                                    // Diffuse back solar reflectance, blind present, vs. slat angle
                                    InsideDifReflectance =
                                            InterpSlatAng(HTsurf_slat_ang, HTsurf_movable_slats, state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack);
                                }
                                DifSolarReflW = WinDifSolarTrans_Factor * InsideDifReflectance;

                                // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                                InitialZoneDifSolReflW(enclosureNum) += DifSolarReflW; // [W]

                                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistReflectedTotl += DifSolarReflW;  // debug
                                ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                                // Now calc diffuse solar absorbed by shade/blind itself
                                BlNum = SurfWinBlindNumber(HeatTransSurfNum);
                                if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                                    // Calc diffuse solar absorbed by shade or screen [W]
                                    ShBlDifSolarAbsW = WinDifSolarTrans_Factor * construct_sh.AbsDiffBackShade;
                                }
                                if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                                    // Calc diffuse solar absorbed by blind [W]
                                    AbsDiffBkBl = InterpSlatAng(HTsurf_slat_ang, HTsurf_movable_slats, construct_sh.AbsDiffBackBlind);
                                    ShBlDifSolarAbsW = WinDifSolarTrans_Factor * AbsDiffBkBl;
                                }
                                // Correct for divider shadowing
                                if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn)
                                    ShBlDifSolarAbsW *= SurfWinGlazedFrac(HeatTransSurfNum);

                                // Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
                                SurfWinInitialDifSolAbsByShade(HeatTransSurfNum) += ShBlDifSolarAbsW * per_HTSurfaceArea;

                                // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                                DifSolarAbsW += ShBlDifSolarAbsW;

                                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW;  // debug
                                ZoneDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug

                                // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                                // This is not very effective since it assigns whatever distributed diffuse solar has not been
                                // absorbed or reflected to transmitted.
                                DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
                                WinDifSolarDistTransmittedTotl += DifSolarTransW;  // debug [W]
                                ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                                // Accumulate transmitted diffuse solar for reporting
                                SurfWinInitialDifSolInTrans(HeatTransSurfNum) += DifSolarTransW * per_HTSurfaceArea;

                            } else if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                                // Init accumulator for transmittance calc below
                                DifSolarAbsW = 0.0;

                                auto const &construct(state.dataConstruction->Construct(ConstrNum));
                                auto const &construct_AbsDiffBack(construct.AbsDiffBack);
                                auto const &construct_sh(state.dataConstruction->Construct(ConstrNumSh));
                                auto const &construct_sh_AbsDiffBack(construct_sh.AbsDiffBack);
                                for (int IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                                    // Calc diffuse solar absorbed in each window glass layer
                                    WinDifSolLayAbsW = WinDifSolarTrans_Factor *
                                                       InterpSw(win_SwitchingFactor, construct_AbsDiffBack(IGlass), construct_sh_AbsDiffBack(IGlass));

                                    // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                                    DifSolarAbsW += WinDifSolLayAbsW;

                                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                    WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW;  // debug
                                    ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

                                    // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                                    SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += WinDifSolLayAbsW * per_HTSurfaceArea;
                                }

                                // Calc diffuse solar reflected back to zone
                                DifSolarReflW = WinDifSolarTrans_Factor *
                                                InterpSw(win_SwitchingFactor, construct.ReflectSolDiffBack, construct_sh.ReflectSolDiffBack);

                                // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                                InitialZoneDifSolReflW(enclosureNum) += DifSolarReflW; // [W]

                                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistReflectedTotl += DifSolarReflW;  // debug
                                ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                                // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                                // This is not very effective since it assigns whatever distributed diffuse solar has not been
                                // absorbed or reflected to transmitted.
                                DifSolarTransW = WinDifSolarTrans_Factor - DifSolarAbsW - DifSolarReflW;
                                WinDifSolarDistTransmittedTotl += DifSolarTransW;  // debug [W]
                                ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                                // Accumulate transmitted diffuse solar for reporting
                                SurfWinInitialDifSolInTrans(HeatTransSurfNum) += DifSolarTransW * per_HTSurfaceArea;

                            } // End of shading flag check

                        } else {
                            // SurfaceWindow(HeatTransSurfNum)%WindowModelType == WindowEQLModel
                            // ConstrNum=Surface(HeatTransSurfNum)%Construction
                            // call the ASHWAT fenestration model for diffuse radiation here
                            WindowEquivalentLayer::CalcEQLOpticalProperty(state, HeatTransSurfNum, isDIFF, AbsSolDiffBackEQL);

                            EQLNum = state.dataConstruction->Construct(ConstrNum).EQLConsPtr;
                            for (Lay = 1; Lay <= CFS(EQLNum).NL; ++Lay) {

                                // Calc diffuse solar absorbed from the inside by each layer of EQL model [W]
                                // WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNum)%AbsDiffBack(Lay)
                                WinDifSolLayAbsW = WinDifSolarTrans_Factor * AbsSolDiffBackEQL(2, Lay);

                                // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                                DifSolarAbsW += WinDifSolLayAbsW;

                                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                                WinDifSolarDistAbsorbedTotl += WinDifSolLayAbsW;  // debug
                                ZoneDifSolarDistAbsorbedTotl += WinDifSolLayAbsW; // debug

                                // Accumulate diffuse solar absorbed from the inside by each window layer [W/m2] for heat balance calcs
                                SurfWinInitialDifSolwinAbs(Lay, HeatTransSurfNum) += WinDifSolLayAbsW * per_HTSurfaceArea;

                                // ASHWAT equivalent layer model may require not the individual layer absorption but the flux
                                // InitialDifSolwinEQL(HeatTransSurfNum) = WinDifSolar(DifTransSurfNum)* ViewFactor
                            }

                            // Calc diffuse solar reflected back to zone
                            // I don't really care if this is a window or opaque surface since I am just
                            // accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
                            // Reflected diffuse solar [W] = current window transmitted diffuse solar
                            //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                            //    * current window inside solar reflectance
                            InsideDifReflectance = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                            DifSolarReflW = WinDifSolarTrans_Factor * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;

                            // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                            InitialZoneDifSolReflW(enclosureNum) += DifSolarReflW; // [W]

                            // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                            WinDifSolarDistReflectedTotl += DifSolarReflW;  // debug
                            ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                            //------------------------------------------------------------------------------
                            // DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
                            //------------------------------------------------------------------------------

                            // If this receiving window surface (HeatTransSurfNum) is an interior window,
                            // calc distributed solar transmitted to adjacent zone [W]
                            // NOTE: This calc is here because interior windows are currently assumed to have no shading

                            // Get the adjacent surface number for this receiving window surface
                            int const AdjSurfNum = Surface(HeatTransSurfNum).ExtBoundCond;
                            // If the adjacent surface number is > 0, this is an interior window
                            if (AdjSurfNum > 0) { // this is an interior window surface

                                // Calc diffuse solar from current exterior window
                                // transmitted through this interior window to adjacent zone [W]
                                // Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
                                //    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                                DifSolarTransW = AbsSolDiffBackEQL(2, CFS(EQLNum).NL + 1) * ViewFactor;
                                //int AdjConstrNum = Surface(AdjSurfNum).Construction;
                                // Get the adjacent zone index
                                int adjEnclosureNum = Surface(AdjSurfNum).SolarEnclIndex;
                                // Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
                                CalcInteriorWinTransDifSolInitialDistribution(state, adjEnclosureNum, AdjSurfNum, DifSolarTransW);

                            } else { // this is an exterior window surface

                                // Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                                // This is not very effective since it assigns whatever distributed diffuse solar has not been
                                // absorbed or reflected to transmitted.
                                DifSolarTransW = AbsSolDiffBackEQL(2, CFS(EQLNum).NL + 1) * ViewFactor;

                            } // this is an interior window surface

                            // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                            WinDifSolarDistTransmittedTotl += DifSolarTransW;  // debug [W]
                            ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
                            // Accumulate transmitted diffuse solar for reporting
                            SurfWinInitialDifSolInTrans(HeatTransSurfNum) += DifSolarTransW * per_HTSurfaceArea;

                        } // IF (SurfaceWindow(HeatTransSurfNum)%WindowModelType /= WindowEQLModel) THEN

                        // HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
                        // I would need revised view factors that included these surface types.
                        // By ignoring them here, the diffuse solar is accounted for on the other surfaces

                        //          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
                        // Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
                        //          END IF

                        //          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
                        //            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
                        //            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
                        //              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
                        //              TransGl = dataMaterial.Material(MatNumGl)%Trans
                        //              ReflGl = dataMaterial.Material(MatNumGl)%ReflectSolDiffBack
                        //              AbsGl = 1.0d0-TransGl-ReflGl
                        //              DividerSolRefl = 1.0d0-DividerSolAbs
                        //              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.0d0-DividerSolRefl*ReflGl)
                        //            END IF
                        // Correct for interior shade transmittance
                        //            IF(ShadeFlag == IntShadeOn) THEN
                        //              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
                        //              DividerSolAbs = DividerSolAbs * dataMaterial.Material(MatNumSh)%Trans
                        //            ELSE IF(ShadeFlag == IntBlindOn) THEN
                        //              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
                        //                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
                        //            END IF
                        // Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

                        //          END IF  ! Window has dividers

                    } // opaque or window heat transfer surface

                } // HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

                // Check debug var for view factors here
                // ViewFactorTotal
                // Check debug vars for individual transmitting surfaces here
                WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl;
                // WinDifSolarTrans

            } // DifTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

            // Check debug vars for zone totals here
            ZoneDifSolarDistTotl = ZoneDifSolarDistAbsorbedTotl + ZoneDifSolarDistReflectedTotl + ZoneDifSolarDistTransmittedTotl;
            // ZoneDifSolarTrans
            // ZoneDifSolarDistAbsorbedTotl
            // ZoneDifSolarDistReflectedTotl
            // ZoneDifSolarDistTransmittedTotl
            //    CALL DisplayString('Diffuse Solar Distribution Zone Totals')

        } // ZoneNum = 1, NumOfZones
    }
    void CalcInteriorWinTransDifSolInitialDistribution(
        EnergyPlusData &state,
        int const IntWinEnclosureNum,     // Interior Window Enclosure index number
        int const IntWinSurfNum,          // Interior Window Surface number
        Real64 const IntWinDifSolarTransW // Diffuse Solar transmitted through Interior Window IntWinSurfNum from adjacent enclosure [W]
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rob Hitchcock
        //       DATE WRITTEN   August 2007
        //       MODIFIED       N/A
        //       RE-ENGINEERED  N/A

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates the initial distribution
        // of diffuse solar transmitted through the given interior window
        // to individual heat transfer surfaces in the given enclosure.
        // Diffuse solar transmitted through interior windows in this enclosure
        // to adjacent enclosures, is added to the InitialZoneDifSolReflW
        // of the adjacent enclosure for subsequent interreflection calcs

        // METHODOLOGY EMPLOYED:
        // Similar to method used in CalcWinTransDifSolInitialDistribution.
        // Apportions diffuse solar transmitted through an interior window
        // that is then absorbed, reflected, and/or transmitted
        // by other heat transfer surfaces in the given enclosure.
        // Calculations use:
        // 1. DifSolarTransW calculated in SUBROUTINE CalcWinTransDifSolInitialDistribution,
        // 2. view factors between the interior window and
        // other heat transfer surfaces in the given enclosure
        // calculated in SUBROUTINE CalcApproximateViewFactors, and
        // 3. surface absorptances, reflectances, and transmittances
        // determined here using revised code from SUBROUTINE InitIntSolarDistribution

        // Using/Aliasing
        using General::InterpSlatAng;
        using General::InterpSw;
        using ScheduleManager::GetCurrentScheduleValue;
        using namespace DataViewFactorInformation;
        using DataHeatBalance::SurfWinInitialDifSolwinAbs;
        using DataHeatBalance::InitialZoneDifSolReflW;
        using DataHeatBalSurface::SurfOpaqInitialDifSolInAbs;
        using DataHeatBalSurface::SurfWinInitialDifSolInTrans;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ConstrNumSh;             // Shaded construction number
        int IGlass;                  // Glass layer counter
        int TotGlassLayers;          // Number of glass layers in a window construction
        int ShadeFlag;               // Shading flag
        Real64 AbsInt;               // Tmp var for Inside surface short-wave absorptance
        Real64 MovInsulSchedVal;     // Value of the movable insulation schedule for current time
        Real64 HMovInsul;            // Conductance of movable insulation
        Real64 InsideDifAbsorptance; // Inside diffuse solar absorptance of a surface
        Real64 InsideDifReflectance; // Inside diffuse solar reflectance of a surface
        int BlNum;                   // Blind number
        Real64 BlAbsDiffBk;          // Glass layer back diffuse solar absorptance when blind in place
        Real64 AbsDiffBkBl;          // Blind diffuse back solar absorptance as part of glazing system

        //  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
        //  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
        //  INTEGER :: MatNumGl           ! Glass layer material number
        //  INTEGER :: MatNumSh           ! Shade layer material number
        //  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

        Real64 ViewFactor;       // temp var for view factor
        Real64 ViewFactorTotal;  // debug var for view factor total
        Real64 WinDifSolarTrans; // debug var for WinDifSolar() [W]
                                 //		Real64 WinDifSolarDistTotl; // debug var for window total distributed diffuse solar [W]
                                 //		Real64 WinDifSolarDistAbsorbedTotl( 0.0 ); // debug var for individual exterior window total
                                 // distributed
        //           diffuse solar absorbed [W]
        //		Real64 WinDifSolarDistReflectedTotl( 0.0 ); // debug var for individual exterior window total distributed
        //           diffuse solar reflected [W]
        //		Real64 WinDifSolarDistTransmittedTotl( 0.0 ); // debug var for individual exterior window total distributed
        //           diffuse solar transmitted [W]
        Real64 WinDifSolLayAbsW; // temp var for diffuse solar absorbed by individual glass layer [W]
                                 //		Real64 ZoneDifSolarTrans( 0.0 ); // debug var for WinDifSolar() [W]
        //  REAL(r64)    :: ZoneDifSolarDistTotl    ! debug var for zone total distributed diffuse solar [W]
        //		Real64 ZoneDifSolarDistAbsorbedTotl( 0.0 ); // debug var for zone total distributed diffuse solar absorbed [W]
        //		Real64 ZoneDifSolarDistReflectedTotl( 0.0 ); // debug var for zone total distributed diffuse solar reflected [W]
        //		Real64 ZoneDifSolarDistTransmittedTotl( 0.0 ); // debug var for zone total distributed diffuse solar transmitted [W]

        Real64 DifSolarAbsW;     // temp var for diffuse solar absorbed by surface [W]
        Real64 DifSolarAbs;      // temp var for diffuse solar absorbed by surface [W/m2]
        Real64 DifSolarReflW;    // temp var for diffuse solar reflected by surface [W]
        Real64 DifSolarTransW;   // temp var for diffuse solar transmitted through interior window surface [W]
        Real64 ShBlDifSolarAbsW; // temp var for diffuse solar absorbed by shade/blind [W]

        //-------------------------------------------------------------------------------------------------
        // DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO INTERIOR HEAT TRANSFER SURFACES
        //-------------------------------------------------------------------------------------------------

        // Init debug vars
        ViewFactorTotal = 0.0;
        WinDifSolarTrans = IntWinDifSolarTransW;

        auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(IntWinEnclosureNum));
        // Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
        Real64 InitialZoneDifSolReflW_zone(0.0);
        for (int const HeatTransSurfNum : thisEnclosure.SurfacePtr) {
            // Skip surfaces that are not heat transfer surfaces
            if (!Surface(HeatTransSurfNum).HeatTransSurf) continue;
            // Skip tubular daylighting device domes
            if (Surface(HeatTransSurfNum).Class == SurfaceClass::TDD_Dome) continue;

            // View factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
            int HTenclosureSurfNum = Surface(HeatTransSurfNum).SolarEnclSurfIndex; // HT surface index for ZoneSolarInfo.SurfacePtr and F arrays
            int enclosureNum = Surface(HeatTransSurfNum).SolarEnclIndex; // index for ZoneSolarInfo
            int IntWinEnclSurfNum = Surface(IntWinSurfNum).SolarEnclSurfIndex; // Window surface index for ZoneSolarInfo.SurfacePtr and F arrays

            ViewFactor = DataViewFactorInformation::ZoneSolarInfo(enclosureNum).F(HTenclosureSurfNum, IntWinEnclSurfNum);
            // debug ViewFactorTotal
            ViewFactorTotal += ViewFactor; // debug

            // Skip receiving surfaces with 0.0 view factor
            if (ViewFactor <= 0.0) continue;
            Real64 const SolarTrans_ViewFactor(IntWinDifSolarTransW * ViewFactor);

            // Calculate diffuse solar from current interior window absorbed and reflected by current heat transfer surface
            // And calculate transmitted diffuse solar to adjacent zones through interior windows
            int ConstrNum = Surface(HeatTransSurfNum).Construction;
            if (state.dataConstruction->Construct(ConstrNum).TransDiff <= 0.0) { // Interior Opaque Surface

                // Determine the inside (back) diffuse solar absorptance
                // and reflectance of the current heat transfer surface
                InsideDifAbsorptance = state.dataConstruction->Construct(ConstrNum).InsideAbsorpSolar;
                // Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                // Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                // HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                HMovInsul = 0.0;
                if (Surface(HeatTransSurfNum).MaterialMovInsulInt > 0) {
                    MovInsulSchedVal = GetCurrentScheduleValue(Surface(HeatTransSurfNum).SchedMovInsulInt);
                    if (MovInsulSchedVal <= 0.0) { // Movable insulation not present at current time
                        HMovInsul = 0.0;
                    } else { // Movable insulation present
                        HMovInsul = 1.0 / (MovInsulSchedVal * dataMaterial.Material(Surface(HeatTransSurfNum).MaterialMovInsulInt).Resistance);
                        AbsInt = dataMaterial.Material(Surface(HeatTransSurfNum).MaterialMovInsulInt).AbsorpSolar;
                    }
                }
                if (HMovInsul > 0.0) InsideDifAbsorptance = AbsInt; // Movable inside insulation present
                // Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
                InsideDifReflectance = 1.0 - InsideDifAbsorptance;

                // Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
                //    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
                //    * current surface inside solar absorptance
                DifSolarAbsW = SolarTrans_ViewFactor * InsideDifAbsorptance; // [W]

                // Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
                //                                 / current surface net area
                DifSolarAbs = DifSolarAbsW / Surface(HeatTransSurfNum).Area;

                // Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
                SurfOpaqInitialDifSolInAbs(HeatTransSurfNum) += DifSolarAbs;

                // Reflected diffuse solar [W] = current window transmitted diffuse solar
                //    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
                //    * current window inside solar reflectance
                DifSolarReflW = SolarTrans_ViewFactor * InsideDifReflectance;

                // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

                // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                // For opaque surfaces all incident diffuse is either absorbed or reflected
                //				WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
                //				WinDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]
                //				ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug [W]
                //				ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug [W]

            } else { // Exterior or Interior Window

                ConstrNumSh = Surface(HeatTransSurfNum).activeShadedConstruction;
                if (SurfWinStormWinFlag(HeatTransSurfNum) == 1) {
                    ConstrNum = Surface(HeatTransSurfNum).StormWinConstruction;
                    ConstrNumSh = Surface(HeatTransSurfNum).activeStormWinShadedConstruction;
                }
                TotGlassLayers = state.dataConstruction->Construct(ConstrNum).TotGlassLayers;
                ShadeFlag = SurfWinShadingFlag(HeatTransSurfNum);

                if (ShadeFlag <= 0) { // No window shading
                    // Init accumulator for transmittance calc below
                    DifSolarAbsW = 0.0;

                    // Calc diffuse solar absorbed by all window glass layers
                    // Note: I am assuming here that individual glass layer absorptances have been corrected
                    //       to account for layer by layer transmittance and reflection effects.
                    for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        // Calc diffuse solar absorbed from the inside by each window glass layer [W]
                        AbsInt = state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);
                        WinDifSolLayAbsW = SolarTrans_ViewFactor * state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass);

                        // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                        DifSolarAbsW += WinDifSolLayAbsW;

                        // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                        SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += (WinDifSolLayAbsW / Surface(HeatTransSurfNum).Area);
                    }
                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
                    //					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

                    // Calc diffuse solar reflected back to zone
                    // I don't really care if this is a window or opaque surface since I am just
                    // accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
                    // Reflected diffuse solar [W] = current window transmitted diffuse solar
                    //    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
                    //    * current window inside solar reflectance
                    DifSolarReflW = SolarTrans_ViewFactor * state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;

                    // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                    InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
                    //					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                    // Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                    // This is not very effective since it assigns whatever distributed diffuse solar has not been
                    // absorbed or reflected to transmitted.
                    DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;

                    // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
                    //					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                    // Accumulate transmitted diffuse solar for reporting
                    SurfWinInitialDifSolInTrans(HeatTransSurfNum) += (DifSolarTransW / Surface(HeatTransSurfNum).Area);

                    //-----------------------------------------------------------------------------------
                    // ADD TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
                    // TOTAL REFLECTED DIFFUSE SOLAR FOR SUBSEQUENT INTERREFLECTION CALCS
                    //-----------------------------------------------------------------------------------

                    // If this receiving window surface (HeatTransSurfNum) is an interior window,
                    // add transmitted diffuse solar to adjacent zone total reflected distributed
                    // diffuse solar for subsequent interreflection calcs
                    // NOTE: This calc is here because interior windows are currently assumed to have no shading

                    // Get the adjacent surface number for this receiving window surface
                    int const AdjSurfNum = Surface(HeatTransSurfNum).ExtBoundCond;
                    // If the adjacent surface number is > 0, this is an interior window
                    if (AdjSurfNum > 0) { // this is an interior window surface

                        // Get the adjacent zone/enclosure index
                        // Add transmitted diffuse solar to total reflected distributed diffuse solar for each zone
                        // for subsequent interreflection calcs
                        InitialZoneDifSolReflW(Surface(AdjSurfNum).SolarEnclIndex) += DifSolarTransW; // [W]
                    }

                } else if (ShadeFlag == IntShadeOn || ShadeFlag >= 3) {
                    // Interior, exterior or between-glass shade, screen or blind in place

                    // Init accumulator for transmittance calc below
                    DifSolarAbsW = 0.0;
                    WinDifSolLayAbsW = 0.0;

                    // First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
                    for (IGlass = 1; IGlass <= state.dataConstruction->Construct(ConstrNumSh).TotGlassLayers; ++IGlass) {
                        if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                            // Calc diffuse solar absorbed in each window glass layer and shade
                            WinDifSolLayAbsW = SolarTrans_ViewFactor * state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(IGlass);
                        }

                        if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                            BlAbsDiffBk = InterpSlatAng(SurfWinSlatAngThisTS(HeatTransSurfNum),
                                                        SurfWinMovableSlats(HeatTransSurfNum),
                                                        state.dataConstruction->Construct(ConstrNumSh).BlAbsDiffBack(_, IGlass));
                            // Calc diffuse solar absorbed in each window glass layer and shade
                            WinDifSolLayAbsW = SolarTrans_ViewFactor * BlAbsDiffBk;
                        }

                        // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                        DifSolarAbsW += WinDifSolLayAbsW;

                        // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                        SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += (WinDifSolLayAbsW / Surface(HeatTransSurfNum).Area);
                    }
                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
                    //					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

                    // Next calc diffuse solar reflected back to zone from window with shade or blind on
                    // Diffuse back solar reflectance, bare glass or shade on
                    InsideDifReflectance = state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack;
                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn) {
                        // Diffuse back solar reflectance, blind present, vs. slat angle
                        InsideDifReflectance = InterpSlatAng(SurfWinSlatAngThisTS(HeatTransSurfNum),
                                                             SurfWinMovableSlats(HeatTransSurfNum),
                                                             state.dataConstruction->Construct(ConstrNum).BlReflectSolDiffBack);
                    }
                    DifSolarReflW = SolarTrans_ViewFactor * InsideDifReflectance;

                    // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                    InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
                    //					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                    // Now calc diffuse solar absorbed by shade/blind itself
                    BlNum = SurfWinBlindNumber(HeatTransSurfNum);
                    if (ShadeFlag == IntShadeOn || ShadeFlag == ExtShadeOn || ShadeFlag == BGShadeOn || ShadeFlag == ExtScreenOn) {
                        // Calc diffuse solar absorbed by shade or screen [W]
                        ShBlDifSolarAbsW = SolarTrans_ViewFactor * state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackShade;
                    }
                    if (ShadeFlag == IntBlindOn || ShadeFlag == ExtBlindOn || ShadeFlag == BGBlindOn) {
                        // Calc diffuse solar absorbed by blind [W]
                        AbsDiffBkBl = InterpSlatAng(SurfWinSlatAngThisTS(HeatTransSurfNum),
                                                    SurfWinMovableSlats(HeatTransSurfNum),
                                                    state.dataConstruction->Construct(ConstrNumSh).AbsDiffBackBlind);
                        ShBlDifSolarAbsW = SolarTrans_ViewFactor * AbsDiffBkBl;
                    }
                    // Correct for divider shadowing
                    if (ShadeFlag == ExtShadeOn || ShadeFlag == ExtBlindOn || ShadeFlag == ExtScreenOn)
                        ShBlDifSolarAbsW *= SurfWinGlazedFrac(HeatTransSurfNum);

                    // Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
                    SurfWinInitialDifSolAbsByShade(HeatTransSurfNum) += (ShBlDifSolarAbsW / Surface(HeatTransSurfNum).Area);

                    // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                    DifSolarAbsW += ShBlDifSolarAbsW;

                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug
                    //					ZoneDifSolarDistAbsorbedTotl += ShBlDifSolarAbsW; // debug

                    // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                    // This is not very effective since it assigns whatever distributed diffuse solar has not been
                    // absorbed or reflected to transmitted.
                    DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;
                    //					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
                    //					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                    // Accumulate transmitted diffuse solar for reporting
                    SurfWinInitialDifSolInTrans(HeatTransSurfNum) += (DifSolarTransW / Surface(HeatTransSurfNum).Area);

                } else if (ShadeFlag == SwitchableGlazing) { // Switchable glazing
                    // Init accumulator for transmittance calc below
                    DifSolarAbsW = 0.0;

                    for (IGlass = 1; IGlass <= TotGlassLayers; ++IGlass) {
                        // Calc diffuse solar absorbed in each window glass layer
                        WinDifSolLayAbsW = SolarTrans_ViewFactor * InterpSw(SurfWinSwitchingFactor(HeatTransSurfNum),
                                                                            state.dataConstruction->Construct(ConstrNum).AbsDiffBack(IGlass),
                                                                            state.dataConstruction->Construct(ConstrNumSh).AbsDiffBack(IGlass));

                        // Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                        DifSolarAbsW += WinDifSolLayAbsW;

                        // Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                        SurfWinInitialDifSolwinAbs(IGlass, HeatTransSurfNum) += (WinDifSolLayAbsW / Surface(HeatTransSurfNum).Area);
                    }
                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug
                    //					ZoneDifSolarDistAbsorbedTotl += DifSolarAbsW; // debug

                    // Calc diffuse solar reflected back to zone
                    DifSolarReflW = SolarTrans_ViewFactor * InterpSw(SurfWinSwitchingFactor(HeatTransSurfNum),
                                                                     state.dataConstruction->Construct(ConstrNum).ReflectSolDiffBack,
                                                                     state.dataConstruction->Construct(ConstrNumSh).ReflectSolDiffBack);

                    // Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                    InitialZoneDifSolReflW_zone += DifSolarReflW; // [W]

                    // Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                    //					WinDifSolarDistReflectedTotl += DifSolarReflW; // debug
                    //					ZoneDifSolarDistReflectedTotl += DifSolarReflW; // debug

                    // Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                    // This is not very effective since it assigns whatever distributed diffuse solar has not been
                    // absorbed or reflected to transmitted.
                    DifSolarTransW = SolarTrans_ViewFactor - DifSolarAbsW - DifSolarReflW;
                    //					WinDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]
                    //					ZoneDifSolarDistTransmittedTotl += DifSolarTransW; // debug [W]

                    // Accumulate transmitted diffuse solar for reporting
                    SurfWinInitialDifSolInTrans(HeatTransSurfNum) += (DifSolarTransW / Surface(HeatTransSurfNum).Area);

                } // End of shading flag check

                // HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
                // I would need revised view factors that included these surface types.
                // By ignoring them here, the diffuse solar is accounted for on the other surfaces

                //          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
                // Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
                //          END IF

                //          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
                //            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
                //            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
                //              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
                //              TransGl = dataMaterial.Material(MatNumGl)%Trans
                //              ReflGl = dataMaterial.Material(MatNumGl)%ReflectSolDiffBack
                //              AbsGl = 1.0d0-TransGl-ReflGl
                //              DividerSolRefl = 1.0d0-DividerSolAbs
                //              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.0d0-DividerSolRefl*ReflGl)
                //            END IF
                // Correct for interior shade transmittance
                //            IF(ShadeFlag == IntShadeOn) THEN
                //              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
                //              DividerSolAbs = DividerSolAbs * dataMaterial.Material(MatNumSh)%Trans
                //            ELSE IF(ShadeFlag == IntBlindOn) THEN
                //              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
                //                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
                //            END IF
                // Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

                //          END IF  ! Window has dividers
            } // opaque or window heat transfer surface

        } // HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
        InitialZoneDifSolReflW(IntWinEnclosureNum) += InitialZoneDifSolReflW_zone;

        // Check debug var for view factors here
        // ViewFactorTotal
        // Check debug vars for individual transmitting surfaces here
        //		WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl; //Debug
        // WinDifSolarTrans
    }

    void CalcComplexWindowOverlap(EnergyPlusData &state,
                                  BSDFGeomDescr &Geom,               // State Geometry
                                  BSDFWindowGeomDescr const &Window, // Window Geometry
                                  int const ISurf                    // Surface number of the complex fenestration
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2012
        //       MODIFIED       Simon Vidanovic (May 2013) - added overlaps calculations for daylighting
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // For each of basis directions on back surface of the window calculates
        // overlap areas. It also calculates overlap areas and reflectances for daylighting calculations

        using namespace Vectors;


        Real64 XShadowProjection; // temporary buffer
        Real64 YShadowProjection; // temporary buffer

        Real64 XSp;                 // for calc BSDF projection direction
        Real64 YSp;                 // for calc BSDF projection direction
        Real64 ZSp;                 // for calc BSDF projection direction
        Real64 SdotX;               // temporary variable for manipulating .dot. product
        Real64 SdotY;               // temporary variable for manipulating .dot. product
        Real64 SdotZ;               // temporary variable for manipulating .dot. product
        int BackSurfaceNumber;      // current back surface number
        int NVT;                    // Number of vertices of back surface
        static Array1D<Real64> XVT; // X,Y,Z coordinates of vertices of
        static Array1D<Real64> YVT; // back surfaces projected into system
        static Array1D<Real64> ZVT; // relative to receiving surface
        int NS1;                    // Number of the figure being overlapped
        int NS2;                    // Number of the figure doing overlapping
        int NS3;                    // Location to place results of overlap
        int IRay;                   // Current ray of BSDF direction
        int KBkSurf;                // Current back surface
        int N;

        // Daylighting
        int IConst;                // Construction number of back surface
        int InsideConLay;          // Construction's inside material layer number
        Real64 VisibleReflectance; // Visible reflectance for inside surface material
        Real64 TotAOverlap;        // Total overlap area for given outgoing direction
        Real64 TotARhoVisOverlap;  // Total overlap area time reflectance for given outgoing direction

        XVT.dimension(MaxVerticesPerSurface + 1, 0.0);
        YVT.dimension(MaxVerticesPerSurface + 1, 0.0);
        ZVT.dimension(MaxVerticesPerSurface + 1, 0.0);

        Geom.AOverlap.dimension(Window.NBkSurf, Geom.Trn.NBasis, 0.0);
        Geom.ARhoVisOverlap.dimension(Window.NBkSurf, Geom.Trn.NBasis, 0.0);
        Geom.AveRhoVisOverlap.dimension(Geom.Trn.NBasis, 0.0);

        // First to calculate and store coordinates of the window surface
        state.dataSolarShading->LOCHCA = 1;
        int BaseSurf = Surface(ISurf).BaseSurf;  // Base surface number

        // Base surface contains current window surface (ISurf).
        // Since that is case, below transformation should always return ZVT = 0.0
        // for every possible transformation
        CTRANS(ISurf, BaseSurf, NVT, XVT, YVT, ZVT);

        // HTRANS routine is using coordinates stored in XVS and YVS in order to calculate
        // surface area.  Since both projections are equal to zero, then simply
        // compy these values into XVS and YVS arrays
        for (N = 1; N <= NVT; ++N) {
            state.dataSolarShading->XVS(N) = XVT(N);
            state.dataSolarShading->YVS(N) = YVT(N);
        }

        // This calculates the area stored in XVS and YVS
        // CALL HTRANS(1,LOCHCA,NVT)
        HTRANS1(state, state.dataSolarShading->LOCHCA, NVT);
        // HCAREA(LOCHCA) = -HCAREA(LOCHCA)

        // Calculation of overlap areas for each outgoing basis direction
        for (IRay = 1; IRay <= Geom.Trn.NBasis; ++IRay) { // basis directions loop (on back surface)
            // For current basis direction calculate dot product between window surface
            // and basis direction.  This will be used to calculate projection of each
            // of the back surfaces to window surface for given basis direciton
            SdotX = dot(Surface(ISurf).lcsx, Geom.sTrn(IRay));
            SdotY = dot(Surface(ISurf).lcsy, Geom.sTrn(IRay));
            SdotZ = dot(Surface(ISurf).lcsz, Geom.sTrn(IRay));
            XSp = -SdotX;
            YSp = -SdotY;
            ZSp = -SdotZ;

            // Projection of shadows for current basis direciton
            if (std::abs(ZSp) > 1.e-4) {
                XShadowProjection = XSp / ZSp;
                YShadowProjection = YSp / ZSp;
                if (std::abs(XShadowProjection) < 1.e-8) XShadowProjection = 0.0;
                if (std::abs(YShadowProjection) < 1.e-8) YShadowProjection = 0.0;
            } else {
                XShadowProjection = 0.0;
                YShadowProjection = 0.0;
            }

            for (KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf) { // back surf loop
                // BaseSurf = Surface(ISurf).BaseSurf
                BackSurfaceNumber = ShadowComb(BaseSurf).BackSurf(KBkSurf);

                // Transform coordinates of back surface from general system to the
                // plane of the receiving surface
                CTRANS(BackSurfaceNumber, BaseSurf, NVT, XVT, YVT, ZVT);

                // Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
                // become clockwise sequential.

                for (N = 1; N <= NVT; ++N) {
                    state.dataSolarShading->XVS(N) = XVT(N) - XShadowProjection * ZVT(N);
                    state.dataSolarShading->YVS(N) = YVT(N) - YShadowProjection * ZVT(N);
                }

                // Transform to the homogeneous coordinate system.

                NS3 = state.dataSolarShading->LOCHCA + 1;
                // NS3      = LOCHCA
                state.dataSolarShading->HCT(NS3) = 0.0;
                // CALL HTRANS(1,NS3,NVT)
                HTRANS1(state, NS3, NVT);

                // Determine area of overlap of projected back surface and receiving surface.

                NS1 = 1;
                NS2 = NS3;
                state.dataSolarShading->HCT(NS3) = 1.0;
                DeterminePolygonOverlap(state, NS1, NS2, NS3);

                if (state.dataSolarShading->OverlapStatus == state.dataSolarShading->NoOverlap) continue;                                           // to next back surface
                if ((state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyVertices) || (state.dataSolarShading->OverlapStatus == state.dataSolarShading->TooManyFigures)) break; // back surfaces DO loop

                state.dataSolarShading->LOCHCA = NS3;
                state.dataSolarShading->HCNS(state.dataSolarShading->LOCHCA) = BackSurfaceNumber;
                state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA) = -state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA);

                Geom.AOverlap(KBkSurf, IRay) = state.dataSolarShading->HCAREA(state.dataSolarShading->LOCHCA);
            } // DO KBkSurf  = 1 , NBkSurf

            // If some of back surfaces is contained in base surface, then need to substract shadow of subsurface
            // from shadow on base surface.  Reson is that above shadowing algorithm is calculating shadow wihtout
            // influence of subsurfaces
            for (KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf) { // back surf loop
                BackSurfaceNumber = ShadowComb(BaseSurf).BackSurf(KBkSurf);
                // CurBaseSurf is Current base surface number for shadow overlap calcualtions
                int CurBaseSurf = Surface(BackSurfaceNumber).BaseSurf;
                if (CurBaseSurf != BackSurfaceNumber) {
                    // Search if that base surface in list of back surfaces for current window
                    // CurBackSurface is Current back surface number for base surface
                    int CurBackSurface = 0;
                    for (N = 1; N <= Window.NBkSurf; ++N) {
                        if (ShadowComb(BaseSurf).BackSurf(N) == CurBaseSurf) {
                            CurBackSurface = N;
                            break;
                        }
                    }
                    if (CurBackSurface != 0) {
                        Geom.AOverlap(CurBackSurface, IRay) -= Geom.AOverlap(KBkSurf, IRay);
                    }
                }
            }

            // Calculate overlap area times reflectance.  This is necessary for complex fenestration daylighting calculations
            TotAOverlap = 0.0;
            TotARhoVisOverlap = 0.0;
            for (KBkSurf = 1; KBkSurf <= Window.NBkSurf; ++KBkSurf) { // back surf loop
                BackSurfaceNumber = ShadowComb(BaseSurf).BackSurf(KBkSurf);
                IConst = Surface(BackSurfaceNumber).Construction;
                InsideConLay = state.dataConstruction->Construct(IConst).TotLayers;
                if (SurfWinWindowModelType(BackSurfaceNumber) == WindowBSDFModel) {
                    VisibleReflectance = state.dataConstruction->Construct(IConst).ReflectVisDiffBack;
                } else {
                    VisibleReflectance = (1.0 - dataMaterial.Material(InsideConLay).AbsorpVisible);
                }
                Geom.ARhoVisOverlap(KBkSurf, IRay) = Geom.AOverlap(KBkSurf, IRay) * VisibleReflectance;
                TotAOverlap += Geom.AOverlap(KBkSurf, IRay);
                TotARhoVisOverlap += Geom.ARhoVisOverlap(KBkSurf, IRay);
            }

            if (TotAOverlap != 0.0) {
                Geom.AveRhoVisOverlap(IRay) = TotARhoVisOverlap / TotAOverlap;
            }

        } // DO IRay = 1, Geom%Trn%NBasis

        // Reset back shadowing counter since complex windows do not need it anymore
        state.dataSolarShading->LOCHCA = 1;
    }

    void TimestepInitComplexFenestration(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Simon Vidanovic
        //       DATE WRITTEN   May 2012
        //       MODIFIED       May 2012 (Initialize complex fenestration in case of EMS)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Performs initialization of complex fenestration. It also performs check if current surface containing
        // complex fenestration have construction changed (by EMS) in which case performs addition of current states
        // into complex fenestration array

        using WindowComplexManager::CheckCFSStates;

        // Locals
        int iSurf;       // Current surface number
        int iState;      // current state number
        int NumOfStates; // number of states for current window

        for (iSurf = 1; iSurf <= TotSurfaces; ++iSurf) {
            if (SurfWinWindowModelType(iSurf) == WindowBSDFModel) {
                // This will check complex fenestrations state and add new one if necessary (EMS case)
                CheckCFSStates(state, iSurf);

                NumOfStates = ComplexWind(iSurf).NumStates;

                // Check for overlap areas and initialize if necessary
                for (iState = 1; iState <= NumOfStates; ++iState) {
                    // do initialization only once
                    if (ComplexWind(iSurf).Geom(iState).InitState) {
                        CalcComplexWindowOverlap(state, ComplexWind(iSurf).Geom(iState), ComplexWind(iSurf), iSurf);
                        ComplexWind(iSurf).Geom(iState).InitState = false;
                    }
                }
            }
        }
    }

} // namespace SolarShading

} // namespace EnergyPlus
