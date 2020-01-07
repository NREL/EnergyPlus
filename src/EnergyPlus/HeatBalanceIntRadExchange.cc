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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataTimings.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceMovableInsulation.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Timer.h>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowEquivalentLayer.hh>

namespace EnergyPlus {

#define EP_HBIRE_SEQ

namespace HeatBalanceIntRadExchange {
    // Module containing the routines dealing with the interior radiant exchange
    // between surfaces.

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   September 2000
    //       MODIFIED       Aug 2001, FW: recalculate ScriptF for a zone if window interior
    //                       shade/blind status is different from previous time step. This is
    //                       because ScriptF, which is used to calculate interior LW
    //                       exchange between surfaces, depends on inside surface emissivities,
    //                       which, for a window, depends on whether or not an interior
    //                       shade or blind is in place.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Part of the heat balance modularization/re-engineering.  Purpose of this
    // module is to replace the MRT with RBAL method of modeling radiant exchange
    // between interior surfaces.

    // METHODOLOGY EMPLOYED:
    // Standard EnergyPlus methodology

    // REFERENCES:
    // ASHRAE Loads Toolkit "Script F" routines by Curt Pedersen
    // Hottel, H.C., and A.F. Sarofim. "Radiative Transfer" (mainly chapter 3),
    //  McGraw-Hill, Inc., New York, 1967.

    // OTHER NOTES: none

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataGlobals;
    using namespace DataEnvironment;
    using namespace DataHeatBalance;
    using namespace DataSurfaces;
    using namespace DataSystemVariables;
    using namespace DataViewFactorInformation;
    using namespace DataTimings;

    // Data
    // MODULE PARAMETER DEFINITIONS
    static ObjexxFCL::gio::Fmt fmtLD("*");
    static ObjexxFCL::gio::Fmt fmtA("(A)");
    static ObjexxFCL::gio::Fmt fmtx("(A,I4,1x,A,1x,6f16.8)");
    static ObjexxFCL::gio::Fmt fmty("(A,1x,6f16.8)");

    // DERIVED TYPE DEFINITIONS
    // na

    // MODULE VARIABLE DECLARATIONS:
    int MaxNumOfRadEnclosureSurfs(0); // Max saved to get large enough space for SendSurfaceTempInKto4thPrecalc
    namespace {
        bool CalcInteriorRadExchangefirstTime(true); // Logical flag for one-time initializations
    }
    // SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

    // Functions
    void clear_state()
    {
        MaxNumOfRadEnclosureSurfs = 0;
        CalcInteriorRadExchangefirstTime = true;
    }

    void CalcInteriorRadExchange(Array1S<Real64> const SurfaceTemp,   // Current surface temperatures
                                 int const SurfIterations,            // Number of iterations in calling subroutine
                                 Array1<Real64> &NetLWRadToSurf,      // Net long wavelength radiant exchange from other surfaces
                                 Optional_int_const ZoneToResimulate, // if passed in, then only calculate for this zone
#ifdef EP_Count_Calls
                                 std::string const &CalledFrom)
#else
                                 std::string const &EP_UNUSED(CalledFrom))
#endif
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   September 2000
        //       MODIFIED       6/18/01, FCW: calculate IR on windows
        //                      Jan 2002, FCW: add blinds with movable slats
        //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems

        // PURPOSE OF THIS SUBROUTINE:
        // Determines the interior radiant exchange between surfaces using
        // Hottel's ScriptF method for the grey interchange between surfaces
        // in an enclosure.

        // REFERENCES:
        // Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

        // Types
        typedef Array1<Real64>::size_type size_type;

        // Using/Aliasing
        using General::InterpSlatAng; // Function for slat angle interpolation
        using namespace DataTimings;
        using HeatBalanceMovableInsulation::EvalInsideMovableInsulation;
        using WindowEquivalentLayer::EQLWindowInsideEffectiveEmiss;

        Real64 const StefanBoltzmannConst(5.6697e-8); // Stefan-Boltzmann constant in W/(m2*K4)
        static ObjexxFCL::gio::Fmt fmtLD("*");

        Real64 RecSurfTemp;                // Receiving surface temperature (C)
        Real64 SendSurfTemp;               // Sending surface temperature (C)
        Real64 RecSurfEmiss;               // Inside surface emissivity
        bool IntShadeOrBlindStatusChanged; // True if status of interior shade or blind on at least
        // one window in a zone has changed from previous time step
        int ShadeFlag;     // Window shading status current time step
        int ShadeFlagPrev; // Window shading status previous time step

        // variables added as part of strategy to reduce calculation time - Glazer 2011-04-22
        Real64 RecSurfTempInKTo4th; // Receiving surface temperature in K to 4th power
        static Array1D<Real64> SendSurfaceTempInKto4thPrecalc;

        // FLOW:

#ifdef EP_Detailed_Timings
        epStartTime("CalcInteriorRadExchange=");
#endif
        if (CalcInteriorRadExchangefirstTime) {
#ifdef EP_HBIRE_SEQ
            SendSurfaceTempInKto4thPrecalc.allocate(MaxNumOfRadEnclosureSurfs);
#else
            SendSurfaceTempInKto4thPrecalc.allocate(TotSurfaces);
#endif
            CalcInteriorRadExchangefirstTime = false;
            if (DeveloperFlag) {
                std::string tdstring;
                ObjexxFCL::gio::write(tdstring, fmtLD) << " OMP turned off, HBIRE loop executed in serial";
                DisplayString(tdstring);
            }
        }

        if (KickOffSimulation || KickOffSizing) return;

        bool const PartialResimulate(present(ZoneToResimulate));

#ifdef EP_Count_Calls
        if (!PartialResimulate) {
            ++NumIntRadExchange_Calls;
        } else {
            ++NumIntRadExchangeZ_Calls;
        }
        if (CalledFrom.empty()) {
            // do nothing
        } else if (CalledFrom == "Main") {
            ++NumIntRadExchangeMain_Calls;
        } else if (CalledFrom == "Outside") {
            ++NumIntRadExchangeOSurf_Calls;
        } else if (CalledFrom == "Inside") {
            ++NumIntRadExchangeISurf_Calls;
        }
#endif

        int startEnclosure = 1;
        int endEnclosure = DataViewFactorInformation::NumOfRadiantEnclosures;
        if (PartialResimulate) {
            startEnclosure = endEnclosure = Zone(ZoneToResimulate).RadiantEnclosureNum;
            auto const &enclosure(ZoneRadiantInfo(startEnclosure));
            for (int i : enclosure.SurfacePtr) {
                NetLWRadToSurf(i) = 0.0;
                SurfaceWindow(i).IRfromParentZone = 0.0;
            }
        } else {
            NetLWRadToSurf = 0.0;
            for (auto &e : SurfaceWindow)
                e.IRfromParentZone = 0.0;
        }

        for (int enclosureNum = startEnclosure; enclosureNum <= endEnclosure; ++enclosureNum) {

            auto &zone_info(ZoneRadiantInfo(enclosureNum));
            auto &zone_ScriptF(zone_info.ScriptF); // Tuned Transposed
            auto &zone_SurfacePtr(zone_info.SurfacePtr);
            int const n_zone_Surfaces(zone_info.NumOfSurfaces);
            size_type const s_zone_Surfaces(n_zone_Surfaces);

            // Calculate ScriptF if first time step in environment and surface heat-balance iterations not yet started;
            // recalculate ScriptF if status of window interior shades or blinds has changed from
            // previous time step. This recalculation is required since ScriptF depends on the inside
            // emissivity of the inside surfaces, which, for windows, is (1) the emissivity of the
            // inside face of the inside glass layer if there is no interior shade/blind, or (2) the effective
            // emissivity of the shade/blind if the shade/blind is in place. (The "effective emissivity"
            // in this case is (1) the shade/blind emissivity if the shade/blind IR transmittance is zero,
            // or (2) a weighted average of the shade/blind emissivity and inside glass emissivity if the
            // shade/blind IR transmittance is not zero (which is sometimes the case for a "shade" and
            // usually the case for a blind). It assumed for switchable glazing that the inside surface
            // emissivity does not change if the glazing is switched on or off.

            // Determine if status of interior shade/blind on one or more windows in the zone has changed
            // from previous time step.  Also make a check for any changes in interior movable insulation.

            if (SurfIterations == 0) {

                Real64 HMovInsul; // "Resistance" value of movable insulation (if present)
                Real64 AbsInt; // Absorptivity of movable insulation material (supercedes that of the construction if interior movable insulation is
                               // present)
                bool IntMovInsulChanged; // True if the status of interior movable insulation has changed

                IntShadeOrBlindStatusChanged = false;
                IntMovInsulChanged = false;

                if (!BeginEnvrnFlag) { // Check for change in shade/blind status
                    for (int const SurfNum : zone_SurfacePtr) {
                        if (IntShadeOrBlindStatusChanged || IntMovInsulChanged)
                            break; // Need only check if one window's status or one movable insulation status has changed
                        if (Construct(Surface(SurfNum).Construction).TypeIsWindow) {
                            ShadeFlag = SurfaceWindow(SurfNum).ShadingFlag;
                            ShadeFlagPrev = SurfaceWindow(SurfNum).ExtIntShadePrevTS;
                            if ((ShadeFlagPrev != IntShadeOn && ShadeFlag == IntShadeOn) ||
                                (ShadeFlagPrev != IntBlindOn && ShadeFlag == IntBlindOn) ||
                                (ShadeFlagPrev == IntShadeOn && ShadeFlag != IntShadeOn) || (ShadeFlagPrev == IntBlindOn && ShadeFlag != IntBlindOn))
                                IntShadeOrBlindStatusChanged = true;
                            if (SurfaceWindow(SurfNum).WindowModelType == WindowEQLModel &&
                                DataWindowEquivalentLayer::CFS(Construct(Surface(SurfNum).Construction).EQLConsPtr).ISControlled) {
                                IntShadeOrBlindStatusChanged = true;
                            }
                        } else {
                            UpdateMovableInsulationFlag(IntMovInsulChanged, SurfNum);
                        }
                    }
                }

                if (IntShadeOrBlindStatusChanged || IntMovInsulChanged || BeginEnvrnFlag) { // Calc inside surface emissivities for this time step
                    for (int ZoneSurfNum = 1; ZoneSurfNum <= n_zone_Surfaces; ++ZoneSurfNum) {
                        int const SurfNum = zone_SurfacePtr(ZoneSurfNum);
                        int const ConstrNum = Surface(SurfNum).Construction;
                        zone_info.Emissivity(ZoneSurfNum) = Construct(ConstrNum).InsideAbsorpThermal;
                        auto const &surface_window(SurfaceWindow(SurfNum));
                        if (Construct(ConstrNum).TypeIsWindow &&
                            (surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn)) {
                            zone_info.Emissivity(ZoneSurfNum) =
                                InterpSlatAng(surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffShBlindEmiss) +
                                InterpSlatAng(surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffGlassEmiss);
                        }
                        if (Surface(SurfNum).MovInsulIntPresent) {
                            HeatBalanceMovableInsulation::EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
                            zone_info.Emissivity(ZoneSurfNum) = Material(Surface(SurfNum).MaterialMovInsulInt).AbsorpThermal;
                        }
                        if (surface_window.WindowModelType == WindowEQLModel &&
                            DataWindowEquivalentLayer::CFS(Construct(ConstrNum).EQLConsPtr).ISControlled) {
                            zone_info.Emissivity(ZoneSurfNum) = EQLWindowInsideEffectiveEmiss(ConstrNum);
                        }
                    }

                    CalcScriptF(n_zone_Surfaces, zone_info.Area, zone_info.F, zone_info.Emissivity, zone_ScriptF);
                    // precalc - multiply by StefanBoltzmannConstant
                    zone_ScriptF *= StefanBoltzmannConst;
                }

            } // End of check if SurfIterations = 0

            // precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
            for (size_type SendZoneSurfNum = 0; SendZoneSurfNum < s_zone_Surfaces; ++SendZoneSurfNum) {
                int const SendSurfNum = zone_SurfacePtr[SendZoneSurfNum];
                auto const &surface_window(SurfaceWindow(SendSurfNum));
                int const ConstrNumSend = Surface(SendSurfNum).Construction;
                auto const &construct(Construct(ConstrNumSend));
                if (construct.WindowTypeEQL || construct.WindowTypeBSDF) {
                    SendSurfTemp = surface_window.EffInsSurfTemp;
                } else if (construct.TypeIsWindow && surface_window.OriginalClass != SurfaceClass_TDD_Diffuser) {
                    if (SurfIterations == 0 && surface_window.ShadingFlag <= 0) {
                        SendSurfTemp = surface_window.ThetaFace(2 * construct.TotGlassLayers) - KelvinConv;
                    } else if (surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn) {
                        SendSurfTemp = surface_window.EffInsSurfTemp;
                    } else {
                        SendSurfTemp = SurfaceTemp(SendSurfNum);
                    }
                } else {
                    SendSurfTemp = SurfaceTemp(SendSurfNum);
                }
#ifdef EP_HBIRE_SEQ
                SendSurfaceTempInKto4thPrecalc[SendZoneSurfNum] = pow_4(SendSurfTemp + KelvinConv);
#else
                SendSurfaceTempInKto4thPrecalc(SendSurfNum) = pow_4(SendSurfTemp + KelvinConv);
#endif
            }

            // These are the money loops
            size_type lSR(0u);
            for (size_type RecZoneSurfNum = 0; RecZoneSurfNum < s_zone_Surfaces; ++RecZoneSurfNum) {
                int const RecSurfNum = zone_SurfacePtr[RecZoneSurfNum];
                int const ConstrNumRec = Surface(RecSurfNum).Construction;
                auto const &construct(Construct(ConstrNumRec));
                auto &surface_window(SurfaceWindow(RecSurfNum));
                auto &netLWRadToRecSurf(NetLWRadToSurf(RecSurfNum));
                if (construct.WindowTypeEQL) {
                    RecSurfEmiss = EQLWindowInsideEffectiveEmiss(ConstrNumRec);
                    RecSurfTemp = surface_window.EffInsSurfTemp;
                } else if (construct.WindowTypeBSDF && surface_window.ShadingFlag == IntShadeOn) {
                    RecSurfTemp = surface_window.EffInsSurfTemp;
                    RecSurfEmiss = surface_window.EffShBlindEmiss[0] + surface_window.EffGlassEmiss[0];
                } else if (construct.TypeIsWindow && surface_window.OriginalClass != SurfaceClass_TDD_Diffuser) {
                    if (SurfIterations == 0 && surface_window.ShadingFlag <= 0) {
                        // If the window is bare this TS and it is the first time through we use the previous TS glass
                        // temperature whether or not the window was shaded in the previous TS. If the window was shaded
                        // the previous time step this temperature is a better starting value than the shade temperature.
                        RecSurfTemp = surface_window.ThetaFace(2 * construct.TotGlassLayers) - KelvinConv;
                        RecSurfEmiss = construct.InsideAbsorpThermal;
                        // For windows with an interior shade or blind an effective inside surface temp
                        // and emiss is used here that is a weighted combination of shade/blind and glass temp and emiss.
                    } else if (surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn) {
                        RecSurfTemp = surface_window.EffInsSurfTemp;
                        RecSurfEmiss = InterpSlatAng(surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffShBlindEmiss) +
                                       InterpSlatAng(surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffGlassEmiss);
                    } else {
                        RecSurfTemp = SurfaceTemp(RecSurfNum);
                        RecSurfEmiss = construct.InsideAbsorpThermal;
                    }
                } else {
                    RecSurfTemp = SurfaceTemp(RecSurfNum);
                    RecSurfEmiss = construct.InsideAbsorpThermal;
                }
                // precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
                RecSurfTempInKTo4th = pow_4(RecSurfTemp + KelvinConv);
                //      IF (ABS(RecSurfTempInKTo4th) > 1.d100) THEN
                //        SendZoneSurfNum=0
                //      ENDIF

                // Calculate net long-wave radiation for opaque surfaces and incident
                // long-wave radiation for windows.
                if (construct.TypeIsWindow) {          // Window
                    Real64 scriptF_acc(0.0);           // Local accumulator
                    Real64 netLWRadToRecSurf_cor(0.0); // Correction
                    Real64 IRfromParentZone_acc(0.0);  // Local accumulator
                    for (size_type SendZoneSurfNum = 0; SendZoneSurfNum < s_zone_Surfaces; ++SendZoneSurfNum, ++lSR) {
                        Real64 const scriptF(zone_ScriptF[lSR]); // [ lSR ] == ( SendZoneSurfNum+1, RecZoneSurfNum+1 )
#ifdef EP_HBIRE_SEQ
                        Real64 const scriptF_temp_ink_4th(scriptF * SendSurfaceTempInKto4thPrecalc[SendZoneSurfNum]);
#else
                        SendSurfNum = zone_SurfacePtr[SendZoneSurfNum] - 1;
                        Real64 const scriptF_temp_ink_4th(scriptF * SendSurfaceTempInKto4thPrecalc[SendSurfNum]);
#endif
                        // Calculate interior LW incident on window rather than net LW for use in window layer heat balance calculation.
                        IRfromParentZone_acc += scriptF_temp_ink_4th;

                        if (RecZoneSurfNum != SendZoneSurfNum) {
                            scriptF_acc += scriptF;
                        } else {
                            netLWRadToRecSurf_cor = scriptF_temp_ink_4th;
                        }

                        // Per BG -- this should never happened.  (CR6346,CR6550 caused this to be put in.  Now removed. LKL 1/2013)
                        //          IF (SurfaceWindow(RecSurfNum)%IRfromParentZone < 0.0) THEN
                        //            CALL ShowRecurringWarningErrorAtEnd('CalcInteriorRadExchange: Window_IRFromParentZone negative, Window="'// &
                        //                TRIM(Surface(RecSurfNum)%Name)//'"',  &
                        //                SurfaceWindow(RecSurfNum)%IRErrCount)
                        //            CALL ShowRecurringContinueErrorAtEnd('..occurs in Zone="'//TRIM(Surface(RecSurfNum)%ZoneName)//  &
                        //                '", reset to 0.0 for remaining calculations.',SurfaceWindow(RecSurfNum)%IRErrCountC)
                        //            SurfaceWindow(RecSurfNum)%IRfromParentZone=0.0
                        //          ENDIF
                    }
                    netLWRadToRecSurf += IRfromParentZone_acc - netLWRadToRecSurf_cor - (scriptF_acc * RecSurfTempInKTo4th);
                    surface_window.IRfromParentZone += IRfromParentZone_acc / RecSurfEmiss;
                } else {
                    Real64 netLWRadToRecSurf_acc(0.0); // Local accumulator
                    for (size_type SendZoneSurfNum = 0; SendZoneSurfNum < s_zone_Surfaces; ++SendZoneSurfNum, ++lSR) {
                        if (RecZoneSurfNum != SendZoneSurfNum) {
#ifdef EP_HBIRE_SEQ
                            netLWRadToRecSurf_acc += zone_ScriptF[lSR] * (SendSurfaceTempInKto4thPrecalc[SendZoneSurfNum] -
                                                                          RecSurfTempInKTo4th); // [ lSR ] == ( SendZoneSurfNum+1, RecZoneSurfNum+1 )
#else
                            SendSurfNum = zone_SurfacePtr[SendZoneSurfNum] - 1;
                            netLWRadToRecSurf_acc += zone_ScriptF[lSR] * (SendSurfaceTempInKto4thPrecalc[SendSurfNum] -
                                                                          RecSurfTempInKTo4th); // [ lSR ] == ( SendZoneSurfNum+1, RecZoneSurfNum+1 )
#endif
                        }
                    }
                    netLWRadToRecSurf += netLWRadToRecSurf_acc;
                }
            }
        }

#ifdef EP_Detailed_Timings
        epStopTime("CalcInteriorRadExchange=");
#endif
    }

    void UpdateMovableInsulationFlag(bool &MovableInsulationChange, int const SurfNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   July 2016

        // PURPOSE OF THIS SUBROUTINE:
        // To determine if any changes in interior movable insulation have happened.
        // If there have been changes due to a schedule change AND a change in properties,
        // then the matrices which are used to calculate interior radiation must be recalculated.

        MovableInsulationChange = false;
        if (Surface(SurfNum).MaterialMovInsulInt > 0) {
            Real64 HMovInsul; // "Resistance" value of movable insulation (if present)
            Real64 AbsInt;    // Absorptivity of movable insulation material
                              // (supercedes that of the construction if interior movable insulation is present)
            HeatBalanceMovableInsulation::EvalInsideMovableInsulation(SurfNum, HMovInsul, AbsInt);
        } else {
            Surface(SurfNum).MovInsulIntPresent = false;
        }
        if ((Surface(SurfNum).MovInsulIntPresent != Surface(SurfNum).MovInsulIntPresentPrevTS)) {
            auto const &thissurf(Surface(SurfNum));
            Real64 AbsorpDiff = std::abs(Construct(thissurf.Construction).InsideAbsorpThermal - Material(thissurf.MaterialMovInsulInt).AbsorpThermal);
            if (AbsorpDiff > 0.01) {
                MovableInsulationChange = true;
            }
        }
    }

    void InitInteriorRadExchange()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   September 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Initializes the various parameters for Hottel's ScriptF method for
        // the grey interchange between surfaces in an enclosure.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::RoundSigDigits;
        using General::ScanForReports;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static ObjexxFCL::gio::Fmt AFormat("(A)");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Findex;                   // index to print view factors
        int Vindex;                   // index for vertices
        bool NoUserInputF;            // Logical flag signifying no input F's for zone
        static bool ViewFactorReport; // Flag to output view factor report in eio file
        static bool ErrorsFound(false);
        Real64 CheckValue1;
        Real64 CheckValue2;
        Real64 FinalCheckValue;
        Array2D<Real64> SaveApproximateViewFactors; // Save for View Factor reporting
        Real64 RowSum;
        Real64 FixedRowSum;
        int NumIterations;
        std::string Option1; // view factor report option

        // FLOW:

        ScanForReports("ViewFactorInfo", ViewFactorReport, _, Option1);

        if (ViewFactorReport) { // Print heading
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Surface View Factor and Grey Interchange Information>";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <View Factor - Zone/Enclosure Information>,Zone/Enclosure Name,Number of Surfaces";
            ObjexxFCL::gio::write(OutputFileInits, fmtA)
                << "! <View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth,Tilt,Thermal Emissivity,#Sides,Vertices";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <View Factor / Grey Interchange Type>,Surface Name(s)";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface";
        }

        MaxNumOfRadEnclosureSurfs = 0;
        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++enclosureNum) {
            auto &thisEnclosure(DataViewFactorInformation::ZoneRadiantInfo(enclosureNum));
            if (enclosureNum == 1) {
                if (DisplayAdvancedReportVariables)
                    ObjexxFCL::gio::write(OutputFileInits, fmtA)
                        << "! <Surface View Factor Check Values>,Zone/Enclosure Name,Original Check Value,Calculated Fixed Check "
                           "Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,Used RowSum "
                           "Convergence";
            }
            int numEnclosureSurfaces = 0;
            for (int zoneNum : thisEnclosure.ZoneNums) {
                for (int surfNum = Zone(zoneNum).SurfaceFirst, surfNum_end = Zone(zoneNum).SurfaceLast; surfNum <= surfNum_end; ++surfNum) {
                    if (Surface(surfNum).HeatTransSurf) ++numEnclosureSurfaces;
                }
            }
            thisEnclosure.NumOfSurfaces = numEnclosureSurfaces;
            MaxNumOfRadEnclosureSurfs = max(MaxNumOfRadEnclosureSurfs, numEnclosureSurfaces);
            if (numEnclosureSurfaces < 1) ShowFatalError("No surfaces in an enclosure in InitInteriorRadExchange");

            // Allocate the parts of the derived type
            thisEnclosure.F.dimension(numEnclosureSurfaces, numEnclosureSurfaces, 0.0);
            thisEnclosure.ScriptF.dimension(numEnclosureSurfaces, numEnclosureSurfaces, 0.0);
            thisEnclosure.Area.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.Emissivity.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.Azimuth.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.Tilt.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.SurfacePtr.dimension(numEnclosureSurfaces, 0);

            // Initialize the surface pointer array
            int enclosureSurfNum = 0;
            for (int const zoneNum : thisEnclosure.ZoneNums) {
                for (int surfNum = Zone(zoneNum).SurfaceFirst, surfNum_end = Zone(zoneNum).SurfaceLast; surfNum <= surfNum_end; ++surfNum) {
                    if (!Surface(surfNum).HeatTransSurf) continue;
                    ++enclosureSurfNum;
                    thisEnclosure.SurfacePtr(enclosureSurfNum) = surfNum;
                }
            }
            // Initialize the area and emissivity arrays
            for (int enclSurfNum = 1; enclSurfNum <= thisEnclosure.NumOfSurfaces; ++enclSurfNum) {
                int const SurfNum = thisEnclosure.SurfacePtr(enclSurfNum);
                thisEnclosure.Area(enclSurfNum) = Surface(SurfNum).Area;
                thisEnclosure.Emissivity(enclSurfNum) = Construct(Surface(SurfNum).Construction).InsideAbsorpThermal;
                thisEnclosure.Azimuth(enclSurfNum) = Surface(SurfNum).Azimuth;
                thisEnclosure.Tilt(enclSurfNum) = Surface(SurfNum).Tilt;
            }

            if (thisEnclosure.NumOfSurfaces == 1) {
                // If there is only one surface in a zone, then there is no radiant exchange
                thisEnclosure.F = 0.0;
                thisEnclosure.ScriptF = 0.0;
                if (DisplayAdvancedReportVariables)
                    ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Surface View Factor Check Values," + thisEnclosure.Name + ",0,0,0,-1,0,0";
                continue; // Go to the next enclosure in the loop
            }

            //  Get user supplied view factors if available in idf.

            NoUserInputF = true;

            std::string cCurrentModuleObject = "ZoneProperty:UserViewFactors:bySurfaceName";
            int NumZonesWithUserFbyS = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
            if (NumZonesWithUserFbyS > 0) {

                GetInputViewFactorsbyName(thisEnclosure.Name,
                                          thisEnclosure.NumOfSurfaces,
                                          thisEnclosure.F,
                                          thisEnclosure.SurfacePtr,
                                          NoUserInputF,
                                          ErrorsFound); // Obtains user input view factors from input file
            }

            if (NoUserInputF) {

                // Calculate the view factors and make sure they satisfy reciprocity
                CalcApproximateViewFactors(thisEnclosure.NumOfSurfaces,
                                           thisEnclosure.Area,
                                           thisEnclosure.Azimuth,
                                           thisEnclosure.Tilt,
                                           thisEnclosure.F,
                                           thisEnclosure.SurfacePtr);
            }

            if (ViewFactorReport) { // Allocate and save user or approximate view factors for reporting.
                SaveApproximateViewFactors.allocate(thisEnclosure.NumOfSurfaces, thisEnclosure.NumOfSurfaces);
                SaveApproximateViewFactors = thisEnclosure.F;
            }

            FixViewFactors(thisEnclosure.NumOfSurfaces,
                           thisEnclosure.Area,
                           thisEnclosure.F,
                           thisEnclosure.Name,
                           thisEnclosure.ZoneNums,
                           CheckValue1,
                           CheckValue2,
                           FinalCheckValue,
                           NumIterations,
                           FixedRowSum);

            // Calculate the script F factors
            CalcScriptF(thisEnclosure.NumOfSurfaces, thisEnclosure.Area, thisEnclosure.F, thisEnclosure.Emissivity, thisEnclosure.ScriptF);

            if (ViewFactorReport) { // Write to SurfInfo File
                // Zone Surface Information Output
                ObjexxFCL::gio::write(OutputFileInits, fmtA)
                    << "Surface View Factor - Zone/Enclosure Information," + thisEnclosure.Name + ',' + RoundSigDigits(thisEnclosure.NumOfSurfaces);

                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(A,',',A,$)")
                        << "Surface View Factor - Surface Information," + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                               cSurfaceClass(Surface(thisEnclosure.SurfacePtr(SurfNum)).Class)
                        << RoundSigDigits(thisEnclosure.Area(SurfNum), 4) + ',' + RoundSigDigits(thisEnclosure.Azimuth(SurfNum), 4) + ',' +
                               RoundSigDigits(thisEnclosure.Tilt(SurfNum), 4) + ',' + RoundSigDigits(thisEnclosure.Emissivity(SurfNum), 4) + ',' +
                               RoundSigDigits(Surface(thisEnclosure.SurfacePtr(SurfNum)).Sides);
                    for (Vindex = 1; Vindex <= Surface(thisEnclosure.SurfacePtr(SurfNum)).Sides; ++Vindex) {
                        auto &Vertex = Surface(thisEnclosure.SurfacePtr(SurfNum)).Vertex(Vindex);
                        ObjexxFCL::gio::write(OutputFileInits, "(3(',',A),$)")
                            << RoundSigDigits(Vertex.x, 4) << RoundSigDigits(Vertex.y, 4) << RoundSigDigits(Vertex.z, 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                ObjexxFCL::gio::write(OutputFileInits, "(A,A,$)") << "Approximate or User Input ViewFactors"
                                                                  << ",To Surface,Surface Class,RowSum";
                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << Surface(thisEnclosure.SurfacePtr(SurfNum)).Name;
                }
                ObjexxFCL::gio::write(OutputFileInits);

                for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                    RowSum = sum(SaveApproximateViewFactors(_, Findex));
                    ObjexxFCL::gio::write(OutputFileInits, "(A,3(',',A),$)")
                        << "View Factor" << Surface(thisEnclosure.SurfacePtr(Findex)).Name
                        << cSurfaceClass(Surface(thisEnclosure.SurfacePtr(Findex)).Class) << RoundSigDigits(RowSum, 4);
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << RoundSigDigits(SaveApproximateViewFactors(SurfNum, Findex), 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }
            }

            if (ViewFactorReport) {
                ObjexxFCL::gio::write(OutputFileInits, "(A,A,$)") << "Final ViewFactors"
                                                                  << ",To Surface,Surface Class,RowSum";
                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << Surface(thisEnclosure.SurfacePtr(SurfNum)).Name;
                }
                ObjexxFCL::gio::write(OutputFileInits);

                for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                    RowSum = sum(thisEnclosure.F(_, Findex));
                    ObjexxFCL::gio::write(OutputFileInits, "(A,3(',',A),$)")
                        << "View Factor" << Surface(thisEnclosure.SurfacePtr(Findex)).Name
                        << cSurfaceClass(Surface(thisEnclosure.SurfacePtr(Findex)).Class) << RoundSigDigits(RowSum, 4);
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << RoundSigDigits(thisEnclosure.F(SurfNum, Findex), 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                if (Option1 == "IDF") {
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!======== original input factors ===========================";
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "ZoneProperty:UserViewFactors:bySurfaceName," + thisEnclosure.Name + ',';
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                            if (!(SurfNum == thisEnclosure.NumOfSurfaces && Findex == thisEnclosure.NumOfSurfaces)) {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                                                                                    Surface(thisEnclosure.SurfacePtr(Findex)).Name + ',' +
                                                                                    RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ',';
                            } else {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                                                                                    Surface(thisEnclosure.SurfacePtr(Findex)).Name + ',' +
                                                                                    RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ';';
                            }
                        }
                    }
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============= end of data ======================";

                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============ final view factors =======================";
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "ZoneProperty:UserViewFactors:bySurfaceName," + thisEnclosure.Name + ',';
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                            if (!(SurfNum == thisEnclosure.NumOfSurfaces && Findex == thisEnclosure.NumOfSurfaces)) {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                                                                                    Surface(thisEnclosure.SurfacePtr(Findex)).Name + ',' +
                                                                                    RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ',';
                            } else {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                                                                                    Surface(thisEnclosure.SurfacePtr(Findex)).Name + ',' +
                                                                                    RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ';';
                            }
                        }
                    }
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============= end of data ======================";
                }
            }

            if (ViewFactorReport) {
                ObjexxFCL::gio::write(OutputFileInits, "(A,A,$)") << "Script F Factors"
                                                                  << ",X Surface";
                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << Surface(thisEnclosure.SurfacePtr(SurfNum)).Name;
                }
                ObjexxFCL::gio::write(OutputFileInits);
                for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                    ObjexxFCL::gio::write(OutputFileInits, "(A,',',A,$)") << "Script F Factor" << Surface(thisEnclosure.SurfacePtr(Findex)).Name;
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << RoundSigDigits(thisEnclosure.ScriptF(Findex, SurfNum), 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }
            }

            if (ViewFactorReport) { // Deallocate saved approximate/user view factors
                SaveApproximateViewFactors.deallocate();
            }

            RowSum = 0.0;
            for (Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                RowSum += sum(thisEnclosure.F(_, Findex));
            }
            RowSum = std::abs(RowSum - thisEnclosure.NumOfSurfaces);
            FixedRowSum = std::abs(FixedRowSum - thisEnclosure.NumOfSurfaces);
            if (DisplayAdvancedReportVariables) {
                ObjexxFCL::gio::write(OutputFileInits, "(8A)") << "Surface View Factor Check Values," + thisEnclosure.Name + ',' +
                                                                      RoundSigDigits(CheckValue1, 6) + ',' + RoundSigDigits(CheckValue2, 6) + ',' +
                                                                      RoundSigDigits(FinalCheckValue, 6) + ',' + RoundSigDigits(NumIterations) + ',' +
                                                                      RoundSigDigits(FixedRowSum, 6) + ',' + RoundSigDigits(RowSum, 6);
            }
        }

        if (ErrorsFound) {
            ShowFatalError("InitInteriorRadExchange: Errors found during initialization of radiant exchange.  Program terminated.");
        }
    }

    void InitSolarViewFactors()
    {

        // Initializes view factors for diffuse solar distribution between surfaces in an enclosure.

        static ObjexxFCL::gio::Fmt AFormat("(A)");

        Array2D<Real64> SaveApproximateViewFactors; // Save for View Factor reporting
        std::string Option1;                        // view factor report option

        bool ErrorsFound = false;
        bool ViewFactorReport = false;
        General::ScanForReports("ViewFactorInfo", ViewFactorReport, _, Option1);

        if (ViewFactorReport) { // Print heading
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Solar View Factor Information>";
            ObjexxFCL::gio::write(OutputFileInits, fmtA)
                << "! <Solar View Factor - Zone/Enclosure Information>,Zone/Enclosure Name,Number of Surfaces";
            ObjexxFCL::gio::write(OutputFileInits, fmtA)
                << "! <Solar View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth,Tilt,Solar Absorbtance,#Sides,Vertices";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Solar View Factor / Interchange Type>,Surface Name(s)";
            ObjexxFCL::gio::write(OutputFileInits, fmtA) << "! <Solar View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface";
        }

        std::string cCurrentModuleObject = "ZoneProperty:UserViewFactors:bySurfaceName";
        int NumZonesWithUserFbyS = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        if (NumZonesWithUserFbyS > 0) AlignInputViewFactors(cCurrentModuleObject, ErrorsFound);

        for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
            auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));
            if (enclosureNum == 1) {
                if (DisplayAdvancedReportVariables)
                    ObjexxFCL::gio::write(OutputFileInits, fmtA)
                        << "! <Solar View Factor Check Values>,Zone/Enclosure Name,Original Check Value,Calculated Fixed Check "
                           "Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,Used RowSum "
                           "Convergence";
            }
            int numEnclosureSurfaces = 0;
            for (int zoneNum : thisEnclosure.ZoneNums) {
                for (int surfNum = Zone(zoneNum).SurfaceFirst, surfNum_end = Zone(zoneNum).SurfaceLast; surfNum <= surfNum_end; ++surfNum) {
                    // Do not include non-heat transfer surfaces, unless it is an air boundary interior window
                    if (Surface(surfNum).Construction > 0) {
                        if (!Surface(surfNum).HeatTransSurf && !Construct(Surface(surfNum).Construction).TypeIsAirBoundaryInteriorWindow) {
                            continue;
                        }
                    } else if (!Surface(surfNum).HeatTransSurf) {
                        continue;
                    }
                    ++numEnclosureSurfaces;
                }
            }
            thisEnclosure.NumOfSurfaces = numEnclosureSurfaces;
            if (numEnclosureSurfaces < 1) ShowFatalError("No surfaces in an enclosure in InitSolarViewFactors");

            // Allocate the parts of the derived type
            thisEnclosure.F.dimension(numEnclosureSurfaces, numEnclosureSurfaces, 0.0);
            thisEnclosure.Area.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.SolAbsorptance.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.Azimuth.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.Tilt.dimension(numEnclosureSurfaces, 0.0);
            thisEnclosure.SurfacePtr.dimension(numEnclosureSurfaces, 0);

            // Initialize the surface pointer array
            int enclosureSurfNum = 0;
            for (int const zoneNum : thisEnclosure.ZoneNums) {
                for (int surfNum = Zone(zoneNum).SurfaceFirst, surfNum_end = Zone(zoneNum).SurfaceLast; surfNum <= surfNum_end; ++surfNum) {
                    // Do not include non-heat transfer surfaces, unless it is an air boundary interior window
                    if (Surface(surfNum).Construction > 0) {
                        if (!Surface(surfNum).HeatTransSurf && !Construct(Surface(surfNum).Construction).TypeIsAirBoundaryInteriorWindow) {
                            continue;
                        }
                    } else if (!Surface(surfNum).HeatTransSurf) {
                        continue;
                    }
                    ++enclosureSurfNum;
                    thisEnclosure.SurfacePtr(enclosureSurfNum) = surfNum;
                    // Store pointers back to here
                    Surface(surfNum).SolarEnclSurfIndex = enclosureSurfNum;
                    Surface(surfNum).SolarEnclIndex = enclosureNum;
                }
            }
            // Initialize the area and related arrays
            for (int enclSurfNum = 1; enclSurfNum <= thisEnclosure.NumOfSurfaces; ++enclSurfNum) {
                int const SurfNum = thisEnclosure.SurfacePtr(enclSurfNum);
                thisEnclosure.Area(enclSurfNum) = Surface(SurfNum).Area;
                thisEnclosure.SolAbsorptance(enclSurfNum) = Construct(Surface(SurfNum).Construction).InsideAbsorpSolar;
                thisEnclosure.Azimuth(enclSurfNum) = Surface(SurfNum).Azimuth;
                thisEnclosure.Tilt(enclSurfNum) = Surface(SurfNum).Tilt;
            }

            if (thisEnclosure.NumOfSurfaces == 1) {
                // If there is only one surface in a zone, then there is no solar distribution
                if (DisplayAdvancedReportVariables)
                    ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Solar View Factor Check Values," + thisEnclosure.Name + ",0,0,0,-1,0,0";
                continue; // Go to the next enclosure in the loop
            }

            //  Get user supplied view factors if available in idf.

            bool NoUserInputF = true;

            if (NumZonesWithUserFbyS > 0) {

                GetInputViewFactorsbyName(thisEnclosure.Name,
                                          thisEnclosure.NumOfSurfaces,
                                          thisEnclosure.F,
                                          thisEnclosure.SurfacePtr,
                                          NoUserInputF,
                                          ErrorsFound); // Obtains user input view factors from input file
            }

            if (NoUserInputF) {

                // Calculate the view factors and make sure they satisfy reciprocity
                CalcApproximateViewFactors(thisEnclosure.NumOfSurfaces,
                                           thisEnclosure.Area,
                                           thisEnclosure.Azimuth,
                                           thisEnclosure.Tilt,
                                           thisEnclosure.F,
                                           thisEnclosure.SurfacePtr);
            }

            if (ViewFactorReport) { // Allocate and save user or approximate view factors for reporting.
                SaveApproximateViewFactors.allocate(thisEnclosure.NumOfSurfaces, thisEnclosure.NumOfSurfaces);
                SaveApproximateViewFactors = thisEnclosure.F;
            }

            Real64 CheckValue1 = 0.0;
            Real64 CheckValue2 = 0.0;
            Real64 FinalCheckValue = 0.0;
            Real64 FixedRowSum = 0.0;
            int NumIterations = 0;

            FixViewFactors(thisEnclosure.NumOfSurfaces,
                           thisEnclosure.Area,
                           thisEnclosure.F,
                           thisEnclosure.Name,
                           thisEnclosure.ZoneNums,
                           CheckValue1,
                           CheckValue2,
                           FinalCheckValue,
                           NumIterations,
                           FixedRowSum);

            if (ViewFactorReport) { // Write to SurfInfo File
                // Zone Surface Information Output
                ObjexxFCL::gio::write(OutputFileInits, fmtA) << "Solar View Factor - Zone/Enclosure Information," + thisEnclosure.Name + ',' +
                                                                    General::RoundSigDigits(thisEnclosure.NumOfSurfaces);

                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(A,',',A,$)")
                        << "Solar View Factor - Surface Information," + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' +
                               cSurfaceClass(Surface(thisEnclosure.SurfacePtr(SurfNum)).Class)
                        << General::RoundSigDigits(thisEnclosure.Area(SurfNum), 4) + ',' +
                               General::RoundSigDigits(thisEnclosure.Azimuth(SurfNum), 4) + ',' +
                               General::RoundSigDigits(thisEnclosure.Tilt(SurfNum), 4) + ',' +
                               General::RoundSigDigits(thisEnclosure.SolAbsorptance(SurfNum), 4) + ',' +
                               General::RoundSigDigits(Surface(thisEnclosure.SurfacePtr(SurfNum)).Sides);
                    for (int Vindex = 1; Vindex <= Surface(thisEnclosure.SurfacePtr(SurfNum)).Sides; ++Vindex) {
                        auto &Vertex = Surface(thisEnclosure.SurfacePtr(SurfNum)).Vertex(Vindex);
                        ObjexxFCL::gio::write(OutputFileInits, "(3(',',A),$)")
                            << General::RoundSigDigits(Vertex.x, 4) << General::RoundSigDigits(Vertex.y, 4) << General::RoundSigDigits(Vertex.z, 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                ObjexxFCL::gio::write(OutputFileInits, "(A,A,$)") << "Approximate or User Input Solar ViewFactors"
                                                                  << ",To Surface,Surface Class,RowSum";
                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << Surface(thisEnclosure.SurfacePtr(SurfNum)).Name;
                }
                ObjexxFCL::gio::write(OutputFileInits);

                for (int Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                    Real64 RowSum = sum(SaveApproximateViewFactors(_, Findex));
                    ObjexxFCL::gio::write(OutputFileInits, "(A,3(',',A),$)")
                        << "Solar View Factor" << Surface(thisEnclosure.SurfacePtr(Findex)).Name
                        << cSurfaceClass(Surface(thisEnclosure.SurfacePtr(Findex)).Class) << General::RoundSigDigits(RowSum, 4);
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)")
                            << General::RoundSigDigits(SaveApproximateViewFactors(SurfNum, Findex), 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }
            }

            if (ViewFactorReport) {
                ObjexxFCL::gio::write(OutputFileInits, "(A,A,$)") << "Final Solar ViewFactors"
                                                                  << ",To Surface,Surface Class,RowSum";
                for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                    ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << Surface(thisEnclosure.SurfacePtr(SurfNum)).Name;
                }
                ObjexxFCL::gio::write(OutputFileInits);

                for (int Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                    Real64 RowSum = sum(thisEnclosure.F(_, Findex));
                    ObjexxFCL::gio::write(OutputFileInits, "(A,3(',',A),$)")
                        << "Solar View Factor" << Surface(thisEnclosure.SurfacePtr(Findex)).Name
                        << cSurfaceClass(Surface(thisEnclosure.SurfacePtr(Findex)).Class) << General::RoundSigDigits(RowSum, 4);
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        ObjexxFCL::gio::write(OutputFileInits, "(',',A,$)") << General::RoundSigDigits(thisEnclosure.F(SurfNum, Findex), 4);
                    }
                    ObjexxFCL::gio::write(OutputFileInits);
                }

                if (Option1 == "IDF") {
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!======== original input factors ===========================";
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "ZoneProperty:UserViewFactors:bySurfaceName," + thisEnclosure.Name + ',';
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        for (int Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                            if (!(SurfNum == thisEnclosure.NumOfSurfaces && Findex == thisEnclosure.NumOfSurfaces)) {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA)
                                    << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' + Surface(thisEnclosure.SurfacePtr(Findex)).Name +
                                           ',' + General::RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ',';
                            } else {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA)
                                    << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' + Surface(thisEnclosure.SurfacePtr(Findex)).Name +
                                           ',' + General::RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ';';
                            }
                        }
                    }
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============= end of data ======================";

                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============ final view factors =======================";
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "ZoneProperty:UserViewFactors:bySurfaceName," + thisEnclosure.Name + ',';
                    for (int SurfNum = 1; SurfNum <= thisEnclosure.NumOfSurfaces; ++SurfNum) {
                        for (int Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                            if (!(SurfNum == thisEnclosure.NumOfSurfaces && Findex == thisEnclosure.NumOfSurfaces)) {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA)
                                    << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' + Surface(thisEnclosure.SurfacePtr(Findex)).Name +
                                           ',' + General::RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ',';
                            } else {
                                ObjexxFCL::gio::write(OutputFileDebug, fmtA)
                                    << "  " + Surface(thisEnclosure.SurfacePtr(SurfNum)).Name + ',' + Surface(thisEnclosure.SurfacePtr(Findex)).Name +
                                           ',' + General::RoundSigDigits(thisEnclosure.F(Findex, SurfNum), 6) + ';';
                            }
                        }
                    }
                    ObjexxFCL::gio::write(OutputFileDebug, fmtA) << "!============= end of data ======================";
                }
            }

            if (ViewFactorReport) { // Deallocate saved approximate/user view factors
                SaveApproximateViewFactors.deallocate();
            }

            Real64 RowSum = 0.0;
            for (int Findex = 1; Findex <= thisEnclosure.NumOfSurfaces; ++Findex) {
                RowSum += sum(thisEnclosure.F(_, Findex));
            }
            RowSum = std::abs(RowSum - thisEnclosure.NumOfSurfaces);
            FixedRowSum = std::abs(FixedRowSum - thisEnclosure.NumOfSurfaces);
            if (DisplayAdvancedReportVariables) {
                ObjexxFCL::gio::write(OutputFileInits, "(8A)")
                    << "Solar View Factor Check Values," + thisEnclosure.Name + ',' + General::RoundSigDigits(CheckValue1, 6) + ',' +
                           General::RoundSigDigits(CheckValue2, 6) + ',' + General::RoundSigDigits(FinalCheckValue, 6) + ',' +
                           General::RoundSigDigits(NumIterations) + ',' + General::RoundSigDigits(FixedRowSum, 6) + ',' +
                           General::RoundSigDigits(RowSum, 6);
            }
        }

        if (ErrorsFound) {
            ShowFatalError("InitSolarViewFactors: Errors found during initialization of diffuse solar distribution.  Program terminated.");
        }
    }

    void GetInputViewFactors(std::string const &ZoneName, // Needed to check for user input view factors.
                             int const N,                 // NUMBER OF SURFACES
                             Array2A<Real64> F,           // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
                             Array1A_int const SPtr,      // pointer to actual surface number
                             bool &NoUserInputF,          // Flag signifying no input F's for this
                             bool &ErrorsFound            // True when errors are found in number of fields vs max args
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   September 2005
        //       MODIFIED       Linda Lawrie;September 2010
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the user view factor info.

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::TrimSigDigits;

        // Argument array dimensioning
        F.dim(N, N);
        SPtr.dim(N);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        //  INTEGER   :: NumZonesWithUserF
        int UserFZoneIndex;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int index;
        int inx1;
        int inx2;
        // unused  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneSurfaceNames

        NoUserInputF = true;
        UserFZoneIndex = inputProcessor->getObjectItemNum("ZoneProperty:UserViewFactors", ZoneName);

        if (UserFZoneIndex > 0) {
            NoUserInputF = false;

            inputProcessor->getObjectItem("ZoneProperty:UserViewFactors",
                                          UserFZoneIndex,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (NumNums < 3 * pow_2(N)) {
                ShowSevereError("GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", not enough values.");
                ShowContinueError("...Number of input values [" + TrimSigDigits(NumNums) + "] is less than the required number=[" +
                                  TrimSigDigits(3 * pow_2(N)) + "].");
                ErrorsFound = true;
                NumNums = 0;
            }
            F = 0.0;
            for (index = 1; index <= NumNums; index += 3) {
                inx1 = rNumericArgs(index);
                inx2 = rNumericArgs(index + 1);
                F(inx2, inx1) = rNumericArgs(index + 2);
            }
        }
    }

    void AlignInputViewFactors(std::string const &cCurrentModuleObject, // Object type
                               bool &ErrorsFound                        // True when errors are found
    )
    {
        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            std::string const thisZoneOrZoneListName = fields.at("zone_or_zonelist_name");
            // do not mark object as used here - let GetInputViewFactorsbyName do that

            // Look for matching solar enclosure name
            bool enclMatchFound = false;
            for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++enclosureNum) {
                auto &thisEnclosure(DataViewFactorInformation::ZoneRadiantInfo(enclosureNum));
                if (UtilityRoutines::SameString(thisZoneOrZoneListName, thisEnclosure.Name)) {
                    // View factor zone name matches enclosure name
                    enclMatchFound = true;
                    break;
                }
            }
            if (enclMatchFound) continue; // We're done with this instance
            // Look for matching solar enclosure name
            for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
                auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));
                if (UtilityRoutines::SameString(thisZoneOrZoneListName, thisEnclosure.Name)) {
                    // View factor zone name matches enclosure name
                    enclMatchFound = true;
                    break;
                }
            }
            if (enclMatchFound) continue; // We're done with this instance
            // Find matching ZoneList name
            int zoneListNum = UtilityRoutines::FindItemInList(
                UtilityRoutines::MakeUPPERCase(thisZoneOrZoneListName), DataHeatBalance::ZoneList, DataHeatBalance::NumOfZoneLists);
            if (zoneListNum > 0) {
                // Look for radiant enclosure with same list of zones
                auto &thisZoneList(DataHeatBalance::ZoneList(zoneListNum));
                for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfRadiantEnclosures; ++enclosureNum) {
                    auto &thisEnclosure(DataViewFactorInformation::ZoneRadiantInfo(enclosureNum));
                    bool anyZoneNotFound = false;
                    // If the number of enclosure zones is not the same as the number of zonelist zone, go to the next enclosure
                    int zlistNumZones = thisEnclosure.ZoneNums.size();
                    if (thisZoneList.NumOfZones != zlistNumZones) continue;
                    for (int zListZoneNum : thisZoneList.Zone) {
                        // Search for matching zones
                        bool thisZoneFound = false;
                        for (int enclZoneNum : thisEnclosure.ZoneNums) {
                            if (enclZoneNum == zListZoneNum) {
                                thisZoneFound = true;
                                break;
                            }
                        }
                        if (!thisZoneFound) {
                            anyZoneNotFound = true;
                            break;
                        }
                    }
                    if (anyZoneNotFound) {
                        continue; // On to the next enclosure
                    } else {
                        enclMatchFound = true;
                        // If matching ZoneList found, set the enclosure name to match
                        thisEnclosure.Name = thisZoneOrZoneListName;
                        break; // We're done with radiant enclosures
                    }
                }
                if (!enclMatchFound) {
                    // Look for solar enclosure with same list of zones
                    for (int enclosureNum = 1; enclosureNum <= DataViewFactorInformation::NumOfSolarEnclosures; ++enclosureNum) {
                        auto &thisEnclosure(DataViewFactorInformation::ZoneSolarInfo(enclosureNum));
                        bool anyZoneNotFound = false;
                        // If the number of enclosure zones is not the same as the number of zonelist zone, go to the next enclosure
                        int zlistNumZones = thisEnclosure.ZoneNums.size();
                        if (thisZoneList.NumOfZones != zlistNumZones) continue;
                        for (int zListZoneNum : thisZoneList.Zone) {
                            // Search for matching zones
                            bool thisZoneFound = false;
                            for (int enclZoneNum : thisEnclosure.ZoneNums) {
                                if (enclZoneNum == zListZoneNum) {
                                    thisZoneFound = true;
                                    break;
                                }
                            }
                            if (!thisZoneFound) {
                                anyZoneNotFound = true;
                                break;
                            }
                        }
                        if (anyZoneNotFound) {
                            continue; // On to the next enclosure
                        } else {
                            enclMatchFound = true;
                            // If matching ZoneList found, set the enclosure name to match
                            thisEnclosure.Name = thisZoneOrZoneListName;
                            break; // We're done with radiant enclosures
                        }
                    }
                }
            }
            if (!enclMatchFound) {
                if (zoneListNum > 0) {
                    ShowSevereError("AlignInputViewFactors: " + cCurrentModuleObject + "=\"" + thisZoneOrZoneListName +
                                    "\" found a matching ZoneList, but did not find a matching radiant or solar enclosure with the same zones.");
                    ErrorsFound = true;

                } else {
                    ShowSevereError("AlignInputViewFactors: " + cCurrentModuleObject + "=\"" + thisZoneOrZoneListName +
                                    "\" did not find a matching radiant or solar enclosure name.");
                    ErrorsFound = true;
                }
            }
        }
    }

    void GetInputViewFactorsbyName(std::string const &EnclosureName, // Needed to check for user input view factors.
                                   int const N,                      // NUMBER OF SURFACES
                                   Array2A<Real64> F,                // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
                                   Array1A_int const SPtr,           // pointer to actual surface number
                                   bool &NoUserInputF,               // Flag signifying no input F's for this
                                   bool &ErrorsFound                 // True when errors are found in number of fields vs max args
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   September 2005
        //       MODIFIED       Linda Lawrie;September 2010

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the user view factor info for an enclosure which could be a zone or a group of zones

        // Using/Aliasing
        using namespace DataIPShortCuts;
        using General::TrimSigDigits;

        // Argument array dimensioning
        F.dim(N, N);
        SPtr.dim(N);

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UserFZoneIndex;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int index;
        int numinx1;
        int inx1;
        int inx2;
        Array1D_string enclosureSurfaceNames;

        NoUserInputF = true;
        UserFZoneIndex = inputProcessor->getObjectItemNum("ZoneProperty:UserViewFactors:bySurfaceName", "zone_or_zonelist_name", EnclosureName);

        if (UserFZoneIndex > 0) {
            enclosureSurfaceNames.allocate(N);
            for (index = 1; index <= N; ++index) {
                enclosureSurfaceNames(index) = Surface(SPtr(index)).Name;
            }
            NoUserInputF = false;

            inputProcessor->getObjectItem("ZoneProperty:UserViewFactors:bySurfaceName",
                                          UserFZoneIndex,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);

            if (NumNums < pow_2(N)) {
                ShowWarningError("GetInputViewFactors: " + cCurrentModuleObject + "=\"" + EnclosureName + "\", not enough values.");
                ShowContinueError("...Number of input values [" + TrimSigDigits(NumNums) + "] is less than the required number=[" +
                                  TrimSigDigits(pow_2(N)) + "] Missing surface pairs will have a zero view factor.");
            }
            F = 0.0;
            numinx1 = 0;

            for (index = 2; index <= NumAlphas; index += 2) {
                inx1 = UtilityRoutines::FindItemInList(cAlphaArgs(index), enclosureSurfaceNames, N);
                inx2 = UtilityRoutines::FindItemInList(cAlphaArgs(index + 1), enclosureSurfaceNames, N);
                if (inx1 == 0) {
                    ShowSevereError("GetInputViewFactors: " + cCurrentModuleObject + "=\"" + EnclosureName + "\", invalid surface name.");
                    ShowContinueError("...Surface name=\"" + cAlphaArgs(index) + "\", not in this zone or enclosure.");
                    ErrorsFound = true;
                }
                if (inx2 == 0) {
                    ShowSevereError("GetInputViewFactors: " + cCurrentModuleObject + "=\"" + EnclosureName + "\", invalid surface name.");
                    ShowContinueError("...Surface name=\"" + cAlphaArgs(index + 2) + "\", not in this zone or enclosure.");
                    ErrorsFound = true;
                }
                ++numinx1;
                if (inx1 > 0 && inx2 > 0) F(inx2, inx1) = rNumericArgs(numinx1);
            }
            enclosureSurfaceNames.deallocate();
        }
    }

    void CalcApproximateViewFactors(int const N,                   // NUMBER OF SURFACES
                                    Array1A<Real64> const A,       // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
                                    Array1A<Real64> const Azimuth, // Facing angle of the surface (in degrees)
                                    Array1A<Real64> const Tilt,    // Tilt angle of the surface (in degrees)
                                    Array2A<Real64> F,             // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
                                    Array1A_int const SPtr         // pointer to REAL(r64) surface number (for error message)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       March 2001 (RKS) to disallow surfaces facing the same direction to interact radiatively
        //                      May 2002 (COP) to include INTMASS, FLOOR, ROOF and CEILING.
        //       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine approximates view factors using an area weighting.
        // This is improved by one degree by not allowing surfaces facing the same
        // direction to "see" each other.

        // METHODOLOGY EMPLOYED:
        // Each surface sees some area of other surfaces within the zone.  The view
        // factors from the surface to the other seen surfaces are defined by their
        // area over the summed area of seen surfaces.  Surfaces facing the same angle
        // are assumed to not be able to see each other.
        //  Modified May 2002 to cover poorly defined surface orientation.  Now all thermal masses, roofs and
        //  ceilings are "seen" by other surfaces. Floors are seen by all other surfaces, but
        //  not by other floors.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning
        A.dim(N);
        Azimuth.dim(N);
        Tilt.dim(N);
        F.dim(N, N);
        SPtr.dim(N);

        // Locals
        // SUBROUTINE ARGUMENTS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const SameAngleLimit(10.0); // If the difference in the azimuth angles are above this value (degrees),
        // then the surfaces are assumed to be facing different directions.

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int i; // DO loop counters for surfaces in the zone
        int j;
        Array1D<Real64> ZoneArea; // Sum of the area of all zone surfaces seen

        // FLOW:
        // Calculate the sum of the areas seen by all zone surfaces
        ZoneArea.dimension(N, 0.0);
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {
                // Assumption is that a surface cannot see itself or any other surface
                // that is facing the same direction (has the same azimuth)
                //  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
                //  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
                //  Skip same surface
                if (i == j) continue;
                //  Include INTMASS, FLOOR(for others), CEILING, ROOF  and different facing surfaces.
                //  Roofs/ceilings always see floors
                if ((Surface(SPtr(j)).Class == SurfaceClass_IntMass) || (Surface(SPtr(j)).Class == SurfaceClass_Floor) ||
                    (Surface(SPtr(j)).Class == SurfaceClass_Roof && Surface(SPtr(i)).Class == SurfaceClass_Floor) ||
                    ((std::abs(Azimuth(i) - Azimuth(j)) > SameAngleLimit) ||
                     (std::abs(Tilt(i) - Tilt(j)) >
                      SameAngleLimit))) { // Everything sees internal mass surfaces | Everything except other floors sees floors

                    ZoneArea(i) += A(j);
                }
            }
            if (ZoneArea(i) <= 0.0) {
                ShowWarningError("CalcApproximateViewFactors: Zero area for all other zone surfaces.");
                ShowContinueError("Happens for Surface=\"" + Surface(SPtr(i)).Name + "\" in Zone=" + Zone(Surface(SPtr(i)).Zone).Name);
            }
        }

        // Set up the approximate view factors.  First these are initialized to all zero.
        // This will clear out any junk leftover from whenever.  Then, for each zone
        // surface, set the view factor from that surface to other surfaces as the
        // area of the other surface divided by the sum of the area of all zone surfaces
        // that the original surface can actually see (calculated above).  This will
        // allow that the sum of all view factors from the original surface to all other
        // surfaces will equal unity.  F(I,J)=0 if I=J or if the surfaces face the same
        // direction.
        //  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
        //  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
        // The second IF statement is intended to avoid a divide by zero if
        // there are no other surfaces in the zone that can be seen.
        F = 0.0;
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {

                //  Skip same surface

                if (i == j) continue;
                //  Include INTMASS, FLOOR(for others), CEILING/ROOF  and different facing surfaces.
                if ((Surface(SPtr(j)).Class == SurfaceClass_IntMass) || (Surface(SPtr(j)).Class == SurfaceClass_Floor) ||
                    (Surface(SPtr(j)).Class == SurfaceClass_Roof) ||
                    ((std::abs(Azimuth(i) - Azimuth(j)) > SameAngleLimit) || (std::abs(Tilt(i) - Tilt(j)) > SameAngleLimit))) {
                    if (ZoneArea(i) > 0.0) F(j, i) = A(j) / (ZoneArea(i));
                }
            }
        }

        ZoneArea.deallocate();
    }

    void FixViewFactors(int const N,                     // NUMBER OF SURFACES
                        Array1A<Real64> const A,         // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
                        Array2A<Real64> F,               // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
                        std::string &enclName,           // Name of Enclosure being fixed
                        std::vector<int> const zoneNums, // Zones which are part of this enclosure
                        Real64 &OriginalCheckValue,      // check of SUM(F) - N
                        Real64 &FixedCheckValue,         // check after fixed of SUM(F) - N
                        Real64 &FinalCheckValue,         // the one to go with
                        int &NumIterations,              // number of iterations to fixed
                        Real64 &RowSum                   // RowSum of Fixed
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       September 2000 (RKS for EnergyPlus)
        //                      April 2005,COP added capability to handle a
        //                      surface larger than sum of all others (nonenclosure)
        //                      by using a Fii view factor for that surface. Process is
        //                      now much more robust and stable.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine fixes approximate view factors and enforces reciprocity
        // and completeness.

        // METHODOLOGY EMPLOYED:
        // A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
        // Subroutine takes approximate view factors and enforces reciprocity by
        // averaging AiFij and AjFji.  Then it determines a set of row coefficients
        // which can be multipled by each AF product to force the sum of AiFij for
        // each row to equal Ai, and applies them. Completeness is checked, and if
        // not satisfied, the AF averaging and row modifications are repeated until
        // completeness is within a preselected small deviation from 1.0
        // The routine also checks the number of surfaces and if N<=3, just enforces reciprocity.

        // REFERENCES:
        // na

        // Using/Aliasing
        using General::RoundSigDigits;

        // Argument array dimensioning
        A.dim(N);
        F.dim(N, N);

        // Locals
        // SUBROUTINE ARGUMENTS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const PrimaryConvergence(0.001);
        Real64 const DifferenceConvergence(0.00001);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 LargestArea;
        Real64 ConvrgNew;
        Real64 ConvrgOld;
        Real64 Accelerator;            // RowCoefficient multipler to accelerate convergence
        Real64 CheckConvergeTolerance; // check value for actual warning

        bool Converged;
        int i;
        int j;
        static int LargestSurf(0);

        // FLOW:
        OriginalCheckValue = std::abs(sum(F) - N);

        //  Allocate and zero arrays
        Array2D<Real64> FixedAF(F); // store for largest area check

        Accelerator = 1.0;
        ConvrgOld = 10.0;
        LargestArea = maxval(A);

        //  Check for Strange Geometry
        if (LargestArea > (sum(A) - LargestArea)) {
            for (i = 1; i <= N; ++i) {
                if (LargestArea != A(i)) continue;
                LargestSurf = i;
                break;
            }
            FixedAF(LargestSurf, LargestSurf) = min(0.9, 1.2 * LargestArea / sum(A)); // Give self view to big surface
        }

        //  Set up AF matrix.
        Array2D<Real64> AF(N, N); // = (AREA * DIRECT VIEW FACTOR) MATRIX
        for (i = 1; i <= N; ++i) {
            for (j = 1; j <= N; ++j) {
                AF(j, i) = FixedAF(j, i) * A(i);
            }
        }

        //  Enforce reciprocity by averaging AiFij and AjFji
        FixedAF = 0.5 * (AF + transpose(AF)); // Performance Slow way to average with transpose (heap use)

        AF.deallocate();

        Array2D<Real64> FixedF(N, N); // CORRECTED MATRIX OF VIEW FACTORS (N X N)

        NumIterations = 0;
        RowSum = 0.0;
        //  Check for physically unreasonable enclosures.

        if (N <= 3) {
            for (i = 1; i <= N; ++i) {
                for (j = 1; j <= N; ++j) {
                    FixedF(j, i) = FixedAF(j, i) / A(i);
                }
            }

            ShowWarningError("Surfaces in Zone/Enclosure=\"" + enclName + "\" do not define an enclosure.");
            ShowContinueError("Number of surfaces <= 3, view factors are set to force reciprocity but may not fulfill completeness.");
            ShowContinueError("Reciprocity means that radiant exchange between two surfaces will match and not lead to an energy loss.");
            ShowContinueError("Completeness means that all of the view factors between a surface and the other surfaces in a zone add up to unity.");
            ShowContinueError("So, when there are three or less surfaces in a zone, EnergyPlus will make sure there are no losses of energy but");
            ShowContinueError(
                "it will not exchange the full amount of radiation with the rest of the zone as it would if there was a completed enclosure.");

            RowSum = sum(FixedF);
            if (RowSum > (N + 0.01)) {
                // Reciprocity enforced but there is more radiation than possible somewhere since the sum of one of the rows
                // is now greater than unity.  This should not be allowed as it can cause issues with the heat balance.
                // Correct this by finding the largest row summation and dividing all of the elements in the F matrix by
                // this max summation.  This will provide a cap on radiation so that no row has a sum greater than unity
                // and will still maintain reciprocity.
                Array1D<Real64> sumFixedF;
                Real64 MaxFixedFRowSum;
                sumFixedF.allocate(N);
                sumFixedF = 0.0;
                for (i = 1; i <= N; ++i) {
                    for (j = 1; j <= N; ++j) {
                        sumFixedF(i) += FixedF(i, j);
                    }
                    if (i == 1) {
                        MaxFixedFRowSum = sumFixedF(i);
                    } else {
                        if (sumFixedF(i) > MaxFixedFRowSum) MaxFixedFRowSum = sumFixedF(i);
                    }
                }
                sumFixedF.deallocate();
                if (MaxFixedFRowSum < 1.0) {
                    ShowFatalError(" FixViewFactors: Three surface or less zone failing ViewFactorFix correction which should never happen.");
                } else {
                    FixedF *= (1.0 / MaxFixedFRowSum);
                }
                RowSum = sum(FixedF); // needs to be recalculated
            }
            FinalCheckValue = FixedCheckValue = std::abs(RowSum - N);
            F = FixedF;
            for (int zoneNum : zoneNums) {
                Zone(zoneNum).EnforcedReciprocity = true;
            }
            return; // Do not iterate, stop with reciprocity satisfied.

        } //  N <= 3 Case

        //  Regular fix cases
        Array1D<Real64> RowCoefficient(N);
        Converged = false;
        while (!Converged) {
            ++NumIterations;
            for (i = 1; i <= N; ++i) {
                // Determine row coefficients which will enforce closure.
                Real64 const sum_FixedAF_i(sum(FixedAF(_, i)));
                if (std::abs(sum_FixedAF_i) > 1.0e-10) {
                    RowCoefficient(i) = A(i) / sum_FixedAF_i;
                } else {
                    RowCoefficient(i) = 1.0;
                }
                FixedAF(_, i) *= RowCoefficient(i);
            }

            //  Enforce reciprocity by averaging AiFij and AjFji
            FixedAF = 0.5 * (FixedAF + transpose(FixedAF));

            //  Form FixedF matrix
            for (i = 1; i <= N; ++i) {
                for (j = 1; j <= N; ++j) {
                    FixedF(j, i) = FixedAF(j, i) / A(i);
                    if (std::abs(FixedF(j, i)) < 1.e-10) {
                        FixedF(j, i) = 0.0;
                        FixedAF(j, i) = 0.0;
                    }
                }
            }

            ConvrgNew = std::abs(sum(FixedF) - N);
            if (std::abs(ConvrgOld - ConvrgNew) < DifferenceConvergence || ConvrgNew <= PrimaryConvergence) { //  Change in sum of Fs must be small.
                Converged = true;
            }
            ConvrgOld = ConvrgNew;
            if (NumIterations > 400) { //  If everything goes bad,enforce reciprocity and go home.
                //  Enforce reciprocity by averaging AiFij and AjFji
                FixedAF = 0.5 * (FixedAF + transpose(FixedAF));

                //  Form FixedF matrix
                for (i = 1; i <= N; ++i) {
                    for (j = 1; j <= N; ++j) {
                        FixedF(j, i) = FixedAF(j, i) / A(i);
                    }
                }
                Real64 const sum_FixedF(sum(FixedF));
                FinalCheckValue = FixedCheckValue = CheckConvergeTolerance = std::abs(sum_FixedF - N);
                if (CheckConvergeTolerance > 0.005) {
                    ShowWarningError("FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" +
                                     enclName + "\".");
                    ShowContinueError("Enforced reciprocity has tolerance (ideal is 0)=[" + RoundSigDigits(CheckConvergeTolerance, 6) +
                                      "], Row Sum (ideal is " + RoundSigDigits(N) + ")=[" + RoundSigDigits(RowSum, 2) + "].");
                    ShowContinueError("If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK.");
                }
                if (std::abs(FixedCheckValue) < std::abs(OriginalCheckValue)) {
                    F = FixedF;
                    FinalCheckValue = FixedCheckValue;
                }
                RowSum = sum_FixedF;
                return;
            }
        }
        FixedCheckValue = ConvrgNew;
        if (FixedCheckValue < OriginalCheckValue) {
            F = FixedF;
            FinalCheckValue = FixedCheckValue;
        } else {
            FinalCheckValue = OriginalCheckValue;
            RowSum = sum(FixedF);
            if (std::abs(RowSum - N) < PrimaryConvergence) {
                F = FixedF;
                FinalCheckValue = FixedCheckValue;
            } else {
                ShowWarningError("FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" + enclName +
                                 "\".");
            }
        }
    }

    void CalcScriptF(int const N,             // Number of surfaces
                     Array1<Real64> const &A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
                     Array2<Real64> const &F, // DIRECT VIEW FACTOR MATRIX (N X N)
                     Array1<Real64> &EMISS,   // VECTOR OF SURFACE EMISSIVITIES
                     Array2<Real64> &ScriptF  // MATRIX OF SCRIPT F FACTORS (N X N) //Tuned Transposed
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Curt Pedersen
        //       DATE WRITTEN   1980
        //       MODIFIED       July 2000 (COP for the ASHRAE Loads Toolkit)
        //       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)
        //       RE-ENGINEERED  June 2014 (Stuart Mentzer): Performance tuned

        // PURPOSE OF THIS SUBROUTINE:
        // Determines Hottel's ScriptF coefficients which account for the total
        // grey interchange between surfaces in an enclosure.

        // METHODOLOGY EMPLOYED:
        // See reference

        // REFERENCES:
        // Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENTS:
        // --Must satisfy reciprocity and completeness:
        //  A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const MaxEmissLimit(0.99999); // Limit the emissivity internally/avoid a divide by zero error

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // Validate argument array dimensions
        assert(N >= 0); // Do we need to allow for N==0?
        assert((A.l() == 1) && (A.u() == N));
        assert((F.l1() == 1) && (F.u1() == N));
        assert((F.l2() == 1) && (F.u2() == N));
        assert((EMISS.l() == 1) && (EMISS.u() == N));
        assert(equal_dimensions(F, ScriptF));

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // FLOW:

#ifdef EP_Count_Calls
        ++NumCalcScriptF_Calls;
#endif

        // Load Cmatrix with AF (AREA * DIRECT VIEW FACTOR) matrix
        Array2D<Real64> Cmatrix(N, N);        // = (AF - EMISS/REFLECTANCE) matrix (but plays other roles)
        assert(equal_dimensions(Cmatrix, F)); // For linear indexing
        Array2D<Real64>::size_type l(0u);
        for (int j = 1; j <= N; ++j) {
            for (int i = 1; i <= N; ++i, ++l) {
                Cmatrix[l] = A(i) * F[l]; // [ l ] == ( i, j )
            }
        }

        // Load Cmatrix with (AF - EMISS/REFLECTANCE) matrix
        Array1D<Real64> Excite(N); // Excitation vector = A*EMISS/REFLECTANCE
        l = 0u;
        for (int i = 1; i <= N; ++i, l += N + 1) {
            Real64 EMISS_i(EMISS(i));
            if (EMISS_i > MaxEmissLimit) { // Check/limit EMISS for this surface to avoid divide by zero below
                EMISS_i = EMISS(i) = MaxEmissLimit;
                ShowWarningError("A thermal emissivity above 0.99999 was detected. This is not allowed. Value was reset to 0.99999");
            }
            Real64 const EMISS_i_fac(A(i) / (1.0 - EMISS_i));
            Excite(i) = -EMISS_i * EMISS_i_fac; // Set up matrix columns for partial radiosity calculation
            Cmatrix[l] -= EMISS_i_fac;          // Coefficient matrix for partial radiosity calculation // [ l ] == ( i, i )
        }

        Array2D<Real64> Cinverse(N, N);       // Inverse of Cmatrix
        CalcMatrixInverse(Cmatrix, Cinverse); // SOLVE THE LINEAR SYSTEM
        Cmatrix.clear();                      // Release memory ASAP

        // Scale Cinverse colums by excitation to get partial radiosity matrix
        l = 0u;
        for (int j = 1; j <= N; ++j) {
            Real64 const e_j(Excite(j));
            for (int i = 1; i <= N; ++i, ++l) {
                Cinverse[l] *= e_j; // [ l ] == ( i, j )
            }
        }
        Excite.clear(); // Release memory ASAP

        // Form Script F matrix transposed
        assert(equal_dimensions(Cinverse, ScriptF)); // For linear indexing
        Array2D<Real64>::size_type m(0u);
        for (int i = 1; i <= N; ++i) { // Inefficient order for cache but can reuse multiplier so faster choice depends on N
            Real64 const EMISS_i(EMISS(i));
            Real64 const EMISS_fac(EMISS_i / (1.0 - EMISS_i));
            l = static_cast<Array2D<Real64>::size_type>(i - 1);
            for (int j = 1; j <= N; ++j, l += N, ++m) {
                if (i == j) {
                    //        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=1
                    ScriptF[m] = EMISS_fac * (Cinverse[l] - EMISS_i); // [ l ] = ( i, j ), [ m ] == ( j, i )
                } else {
                    //        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=0
                    ScriptF[m] = EMISS_fac * Cinverse[l]; // [ l ] == ( i, j ), [ m ] == ( j, i )
                }
            }
        }
    }

    void CalcMatrixInverse(Array2<Real64> &A, // Matrix: Gets reduced to L\U form
                           Array2<Real64> &I  // Returned as inverse matrix
    )
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Jakob Asmundsson
        //       DATE WRITTEN   January 1999
        //       MODIFIED       September 2000 (RKS for EnergyPlus)
        //       RE-ENGINEERED  June 2014 (Stuart Mentzer): Performance/memory tuning rewrite

        // PURPOSE OF THIS SUBROUTINE:
        // To find the inverse of Matrix, using partial pivoting.

        // METHODOLOGY EMPLOYED:
        // Inverse is found using partial pivoting and Gauss elimination

        // REFERENCES:
        // Any Linear Algebra book

        // Validation
        assert(A.square());
        assert(A.I1() == A.I2());
        assert(equal_dimensions(A, I));

        // Initialization
        int const l(A.l1());
        int const u(A.u1());
        int const n(u - l + 1);
        I.to_identity(); // I starts out as identity

        // Could do row scaling here to improve condition and then check min pivot isn't too small

        // Compute in-place LU decomposition of [A|I] with row pivoting
        for (int i = l; i <= u; ++i) {

            // Find pivot row in column i below diagonal
            int iPiv = i;
            Real64 aPiv(std::abs(A(i, i)));
            auto ik(A.index(i, i + 1));
            for (int k = i + 1; k <= u; ++k, ++ik) {
                Real64 const aAki(std::abs(A[ik])); // [ ik ] == ( i, k )
                if (aAki > aPiv) {
                    iPiv = k;
                    aPiv = aAki;
                }
            }
            assert(aPiv != 0.0); //? Is zero pivot possible for some user inputs? If so if test/handler needed

            // Swap row i with pivot row
            if (iPiv != i) {
                auto ji(A.index(l, i));    // [ ji ] == ( j, i )
                auto pj(A.index(l, iPiv)); // [ pj ] == ( j, iPiv )
                for (int j = l; j <= u; ++j, ji += n, pj += n) {
                    Real64 const Aij(A[ji]);
                    A[ji] = A[pj];
                    A[pj] = Aij;
                    Real64 const Iij(I[ji]);
                    I[ji] = I[pj];
                    I[pj] = Iij;
                }
            }

            // Put multipliers in column i and reduce block below A(i,i)
            Real64 const Aii_inv(1.0 / A(i, i));
            for (int k = i + 1; k <= u; ++k) {
                Real64 const multiplier(A(i, k) * Aii_inv);
                A(i, k) = multiplier;
                if (multiplier != 0.0) {
                    auto ji(A.index(i + 1, i)); // [ ji ] == ( j, i )
                    auto jk(A.index(i + 1, k)); // [ jk ] == ( j, k )
                    for (int j = i + 1; j <= u; ++j, ji += n, jk += n) {
                        A[jk] -= multiplier * A[ji];
                    }
                    ji = A.index(l, i);
                    jk = A.index(l, k);
                    for (int j = l; j <= u; ++j, ji += n, jk += n) {
                        Real64 const Iij(I[ji]);
                        if (Iij != 0.0) {
                            I[jk] -= multiplier * Iij;
                        }
                    }
                }
            }
        }

        // Perform back-substitution on [U|I] to put inverse in I
        for (int k = u; k >= l; --k) {
            Real64 const Akk_inv(1.0 / A(k, k));
            auto jk(A.index(l, k)); // [ jk ] == ( j, k )
            for (int j = l; j <= u; ++j, jk += n) {
                I[jk] *= Akk_inv;
            }
            auto ik(A.index(k, l));             // [ ik ] == ( i, k )
            for (int i = l; i < k; ++i, ++ik) { // Eliminate kth column entries from I in rows above k
                Real64 const Aik(A[ik]);
                auto ji(A.index(l, i)); // [ ji ] == ( j, i )
                auto jk(A.index(l, k)); // [ jk ] == ( k, j )
                for (int j = l; j <= u; ++j, ji += n, jk += n) {
                    I[ji] -= Aik * I[jk];
                }
            }
        }
    }

    int GetRadiantSystemSurface(std::string const &cCurrentModuleObject, // Calling Object type
                                std::string const &RadSysName,           // Calling Object name
                                int const RadSysZoneNum,                 // Radiant system zone number
                                std::string const &SurfaceName,          // Referenced surface name
                                bool &ErrorsFound                        // True when errors are found
    )
    {
        static std::string const routineName("GetRadiantSystemSurface: "); // include trailing blank space

        // For radiant zone equipment, find the referenced surface and check if it is in the same zone or radiant enclosure
        int const surfNum = UtilityRoutines::FindItemInList(SurfaceName, DataSurfaces::Surface);

        // Trap for surfaces that do not exist
        if (surfNum == 0) {
            ShowSevereError(routineName + "Invalid Surface name = " + SurfaceName);
            ShowContinueError("Occurs for " + cCurrentModuleObject + " = " + RadSysName);
            ErrorsFound = true;
            return surfNum;
        }

        if (RadSysZoneNum == 0) {
            ShowSevereError(routineName + "Invalid Zone number passed by " + cCurrentModuleObject + " = " + RadSysName);
            ErrorsFound = true;
            return surfNum;
        }

        // Check if the surface and equipment are in the same zone or radiant enclosure
        int const surfRadEnclNum = DataHeatBalance::Zone(DataSurfaces::Surface(surfNum).Zone).RadiantEnclosureNum;
        int const radSysEnclNum = DataHeatBalance::Zone(RadSysZoneNum).RadiantEnclosureNum;
        if (radSysEnclNum == 0) {
            // This should never happen - but it does in some simple unit tests that are designed to throw errors
            ShowSevereError(routineName + "Somehow the radiant system enclosure number is zero for" + cCurrentModuleObject + " = " + RadSysName);
            ErrorsFound = true;
        } else if (surfRadEnclNum == 0) {
            // This should never happen
            ShowSevereError(routineName + "Somehow  the surface enclosure number is zero for" + cCurrentModuleObject + " = " + RadSysName +
                            " and Surface = " + SurfaceName); // LCOV_EXCL_LINE
            ErrorsFound = true;                               // LCOV_EXCL_LINE
        } else if (surfRadEnclNum != radSysEnclNum) {
            ShowSevereError(routineName + "Surface = " + SurfaceName + " is not in the same zone or enclosure as the radiant equipment.");
            ShowContinueError("Surface zone or enclosure = " + DataViewFactorInformation::ZoneRadiantInfo(surfRadEnclNum).Name);
            ShowContinueError("Radiant equipment zone or enclosure = " + DataViewFactorInformation::ZoneRadiantInfo(radSysEnclNum).Name);
            ShowContinueError("Occurs for " + cCurrentModuleObject + " = " + RadSysName);
            ErrorsFound = true;
        }
        return surfNum;
    }

} // namespace HeatBalanceIntRadExchange

} // namespace EnergyPlus
