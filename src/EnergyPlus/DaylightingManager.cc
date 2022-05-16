// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Vector4.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/random.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/DElightManagerF.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBSDFWindow.hh>
#include <EnergyPlus/DataDElight.hh>
#include <EnergyPlus/DataDaylighting.hh>
#include <EnergyPlus/DataDaylightingDevices.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataViewFactorInformation.hh>
#include <EnergyPlus/DaylightingDevices.hh>
#include <EnergyPlus/DaylightingManager.hh>
#include <EnergyPlus/DisplayRoutines.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/InternalHeatGains.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PierceSurface.hh>
#include <EnergyPlus/SQLiteProcedures.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SolarReflectionManager.hh>
#include <EnergyPlus/SolarShading.hh>
#include <EnergyPlus/SurfaceOctree.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WindowComplexManager.hh>

namespace EnergyPlus::DaylightingManager {

// MODULE INFORMATION
//       AUTHOR         Fred Winkelmann
//       DATE WRITTEN   July 1997, December 1998
//       MODIFIED       Oct 2004; LKL -- Efficiencies and code restructure
//                      Aug 2012: BG -- Added availability schedule
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// Manages the daylighting calculations for each thermal zone that has an associated
// Daylighting:Controls object.

// Includes calculation of interior daylight illuminance and glare
// from each of the windows in a zone, control of window shading devices
// to reduce glare, and control of overhead electric lighting in response
// to interior daylight illuminance level at one or two user-specified
// reference points at which sensors are located.

// METHODOLOGY EMPLOYED:
// REFERENCES:
// "Daylighting Calculation in DOE-2," F.C.Winkelmann, LBL-11353, May 1983
// "Daylighting Simulation in the DOE-2 Building Energy Analysis Program,"
// F.C. Winkelmann and S. Selkowitz, Energy and Buildings 8(1985)271-286

// OTHER NOTES:
// This module was created from DOE-2.1E subroutines.

// Correspondence between DOE-2.1E and EnergyPlus subroutine names:

// DOE-2.1E    EnergyPlus                      In Module           Called from Module
// DAVREF      DayltgAveInteriorReflectance    DaylightingManager DaylightingManager
// DCOF        CalcDayltgCoefficients          DaylightingManager DaylightingManager
// DCROSS      DayltgCrossProduct              DaylightingManager DaylightingManager
// DEXTIL      DayltgCurrentExtHorizIllum      WeatherManager     WeatherManager
// DGLARE      DayltgGlare                     DaylightingManager DaylightingManager
// DHILL       DayltgExtHorizIllum             DaylightingManager DaylightingManager
// DHITSH      DayltgHitObstruction            DaylightingManager DaylightingManager
// DINTIL      DayltgInteriorIllum             DaylightingManager HeatBalanceSurfaceManager
// DLTSYS      DayltgElecLightingControl       DaylightingManager HeatBalanceSurfaceManager
// DNSOL       not used
// DPFAC       DayltgPositionFactor            DaylightingManager DaylightingManager
// DPIERC      PierceSurface                   PierceSurface      DaylightingManager
// DREFLT      DayltgInterReflectedIllum       DaylightingManager DaylightingManager
// DSKYLU      DayltgSkyLuminance              DaylightingManager DaylightingManager
// DTHLIM      DayltgAzimuthLimits             DaylightingManager DaylightingManager
// DLUMEF      DayltgLuminousEfficacy          WeatherManager     WeatherManager

// Using/Aliasing
using namespace DataHeatBalance;
using namespace DataSurfaces;

void DayltgAveInteriorReflectance(EnergyPlusData &state, int const enclNum) // Enclosure number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       Mar 2004, FCW: add calculation of following SurfaceWindow variables:
    //                        EnclAreaMinusThisSurf, EnclAreaReflProdMinusThisSurf, RhoCeilingWall,
    //                        RhoFloorWall, FractionUpgoing. Add calculation of ZoneDaylight%floorVisRefl.

    // PURPOSE OF THIS SUBROUTINE:
    // Called by CalcDayltgCoefficients for each daylit zone. Determines total
    // area and area-weighted average visible reflectance of
    // all inside faces of the surfaces of a zone.  In addition, finds
    // area and average reflectance of interzone, underground and exterior
    // heat-transfer surfaces in the following categories: floor (tilt > 170 deg),
    // ceiling (tilt < 10 deg), and wall (10 < tilt < 170 deg).
    // The window reflectance values used here assume the windows have no shading
    // devices. This information is used in the calculation of the
    // internally-reflected daylighting component.

    // Finds total number of exterior windows in the space.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DAVREF

    SurfaceClass IType; // Surface type/class
    Real64 AREA;        // Inside surface area (m2)
    Real64 AInsTot;     // Total inside surface area of an enclosure (m2)
    Real64 ARHTOT;      // Sum over surfaces of AREA*(inside visible reflectance) (m2)
    int ITILT;          // Surface tilt category (1 = floor, 2 = wall, 3 = ceiling)
    int IT;             // Tilt index
    Real64 ATWL;        // Opaque surface area (m2)
    Real64 ARHTWL;      // ATWL times inside visible reflectance of surface (m2)
    Real64 ETA;         // Ratio of floor-to-window-center height and average floor-to-ceiling height

    // Total inside surface area, including windows
    AInsTot = 0.0;
    // Sum of products of inside surface area * vis reflectance
    ARHTOT = 0.0;
    // Area sum and area * reflectance sum for different orientations
    state.dataDaylightingManager->AR = 0.0;
    state.dataDaylightingManager->ARH = 0.0;
    // Loop over surfaces in the zone's enclosure

    auto &thisEnclosure(state.dataViewFactor->EnclSolInfo(enclNum));
    for (int ISurf : thisEnclosure.SurfacePtr) {
        IType = state.dataSurface->Surface(ISurf).Class;
        // Error if window has multiplier > 1 since this causes incorrect illuminance calc
        if (IType == SurfaceClass::Window && state.dataSurface->Surface(ISurf).Multiplier > 1.0) {
            if (thisEnclosure.TotalEnclosureDaylRefPoints > 0) {
                ShowSevereError(state,
                                "DayltgAveInteriorReflectance: Multiplier > 1.0 for window " + state.dataSurface->Surface(ISurf).Name +
                                    " in Zone=" + state.dataSurface->Surface(ISurf).ZoneName);
                ShowContinueError(state, "...not allowed since it is in a zone or enclosure with daylighting.");
                ShowFatalError(state, "Program terminates due to preceding conditions.");
            } else {
                ShowSevereError(state,
                                "DayltgAveInteriorReflectance: Multiplier > 1.0 for window " + state.dataSurface->Surface(ISurf).Name +
                                    " in Zone=" + state.dataSurface->Surface(ISurf).ZoneName);
                ShowContinueError(state, "...an adjacent Zone has daylighting. Simulation cannot proceed.");
                ShowFatalError(state, "Program terminates due to preceding conditions.");
            }
        }
        if (IType == SurfaceClass::Wall || IType == SurfaceClass::Floor || IType == SurfaceClass::Roof || IType == SurfaceClass::Window ||
            IType == SurfaceClass::Door) {
            AREA = state.dataSurface->Surface(ISurf).Area;
            // In following, FrameArea and DividerArea can be non-zero only for exterior windows
            AInsTot += AREA + state.dataSurface->SurfWinFrameArea(ISurf) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(ISurf)) +
                       state.dataSurface->SurfWinDividerArea(ISurf) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(ISurf));
            ARHTOT += AREA * state.dataConstruction->Construct(state.dataSurface->Surface(ISurf).Construction).ReflectVisDiffBack +
                      state.dataSurface->SurfWinFrameArea(ISurf) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(ISurf)) *
                          (1.0 - state.dataSurface->SurfWinFrameSolAbsorp(ISurf)) +
                      state.dataSurface->SurfWinDividerArea(ISurf) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(ISurf)) *
                          (1.0 - state.dataSurface->SurfWinDividerSolAbsorp(ISurf));
            ITILT = 3;                                                                                                      // Ceiling
            if (state.dataSurface->Surface(ISurf).Tilt > 10.0 && state.dataSurface->Surface(ISurf).Tilt < 170.0) ITILT = 2; // Wall
            if (state.dataSurface->Surface(ISurf).Tilt >= 170.0) ITILT = 1;                                                 // Floor
            state.dataDaylightingManager->AR(ITILT) +=
                AREA + state.dataSurface->SurfWinFrameArea(ISurf) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(ISurf)) +
                state.dataSurface->SurfWinDividerArea(ISurf) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(ISurf));
            state.dataDaylightingManager->ARH(ITILT) +=
                AREA * state.dataConstruction->Construct(state.dataSurface->Surface(ISurf).Construction).ReflectVisDiffBack +
                state.dataSurface->SurfWinFrameArea(ISurf) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(ISurf)) *
                    (1.0 - state.dataSurface->SurfWinFrameSolAbsorp(ISurf)) +
                state.dataSurface->SurfWinDividerArea(ISurf) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(ISurf)) *
                    (1.0 - state.dataSurface->SurfWinDividerSolAbsorp(ISurf));
        }
    }

    // Average inside surface reflectance of enclosure
    if (AInsTot <= 0.0) {
        ShowSevereError(state, "DayltgAveInteriorReflectance: Total opaque surface area is <=0.0 in solar enclosure=" + thisEnclosure.Name);
        ShowFatalError(state, "Program terminates due to preceding conditions.");
    }
    state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect = ARHTOT / AInsTot;
    // Total inside surface area of enclosure
    state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea = AInsTot;
    // Average floor visible reflectance
    state.dataDaylightingData->enclDaylight(enclNum).floorVisRefl =
        state.dataDaylightingManager->ARH(3) / (state.dataDaylightingManager->AR(3) + 1.e-6);

    for (int ISurf : thisEnclosure.SurfacePtr) {
        IType = state.dataSurface->Surface(ISurf).Class;
        if (IType == SurfaceClass::Wall || IType == SurfaceClass::Floor || IType == SurfaceClass::Roof) {
            // Remove this surface from the space inside surface area and area*reflectivity
            // The resulting areas are AP(ITILT). The resulting area*reflectivity is ARHP(ITILT).
            // Initialize gross area of surface (including subsurfaces)
            ATWL = state.dataSurface->Surface(ISurf).Area; // This is the surface area less subsurfaces
            // Area * reflectance for this surface, excluding attached windows and doors
            ARHTWL = state.dataSurface->Surface(ISurf).Area *
                     state.dataConstruction->Construct(state.dataSurface->Surface(ISurf).Construction).ReflectVisDiffBack;
            // Tilt index
            if (state.dataSurface->Surface(ISurf).Tilt > 45.0 && state.dataSurface->Surface(ISurf).Tilt < 135.0) {
                ITILT = 2; // Wall
            } else if (state.dataSurface->Surface(ISurf).Tilt >= 135.0) {
                ITILT = 1; // Floor
            } else {
                ITILT = 3; // Ceiling
            }
            // Loop over windows and doors on this wall
            for (int IWinDr : thisEnclosure.SurfacePtr) {
                if ((state.dataSurface->Surface(IWinDr).Class == SurfaceClass::Window ||
                     state.dataSurface->Surface(IWinDr).Class == SurfaceClass::Door) &&
                    state.dataSurface->Surface(IWinDr).BaseSurf == ISurf) {
                    ATWL += state.dataSurface->Surface(IWinDr).Area +
                            state.dataSurface->SurfWinFrameArea(IWinDr) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(IWinDr)) +
                            state.dataSurface->SurfWinDividerArea(IWinDr) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(IWinDr));
                    ARHTWL += state.dataSurface->Surface(IWinDr).Area *
                                  state.dataConstruction->Construct(state.dataSurface->Surface(IWinDr).Construction).ReflectVisDiffBack +
                              state.dataSurface->SurfWinFrameArea(IWinDr) * (1.0 + 0.5 * state.dataSurface->SurfWinProjCorrFrIn(IWinDr)) *
                                  (1.0 - state.dataSurface->SurfWinFrameSolAbsorp(IWinDr)) +
                              state.dataSurface->SurfWinDividerArea(IWinDr) * (1.0 + state.dataSurface->SurfWinProjCorrDivIn(IWinDr)) *
                                  (1.0 - state.dataSurface->SurfWinDividerSolAbsorp(IWinDr));
                }
            }
            // Inside surface area of floor, walls and ceilings, minus surface ISurf and its subsurfaces
            for (IT = 1; IT <= 3; ++IT) {
                if (IT == ITILT) {
                    state.dataDaylightingManager->AP(IT) = state.dataDaylightingManager->AR(IT) - ATWL;
                    state.dataDaylightingManager->ARHP(IT) = state.dataDaylightingManager->ARH(IT) - ARHTWL;
                } else {
                    state.dataDaylightingManager->AP(IT) = state.dataDaylightingManager->AR(IT);
                    state.dataDaylightingManager->ARHP(IT) = state.dataDaylightingManager->ARH(IT);
                }
            }
            state.dataSurface->SurfaceWindow(ISurf).EnclAreaMinusThisSurf = state.dataDaylightingManager->AP;
            state.dataSurface->SurfaceWindow(ISurf).EnclAreaReflProdMinusThisSurf = state.dataDaylightingManager->ARHP;
        }
    } // End of loop over opaque surfaces in enclosure

    for (int IWin : thisEnclosure.SurfacePtr) {
        if (state.dataSurface->Surface(IWin).Class == SurfaceClass::Window) {
            int ISurf = state.dataSurface->Surface(IWin).BaseSurf;
            int zoneNum = state.dataSurface->Surface(IWin).Zone;
            // Ratio of floor-to-window-center height and average floor-to-ceiling height
            ETA = max(0.0,
                      min(1.0,
                          (state.dataSurface->SurfaceWindow(IWin).WinCenter(3) - state.dataHeatBal->Zone(zoneNum).OriginZ) *
                              state.dataHeatBal->Zone(zoneNum).FloorArea / state.dataHeatBal->Zone(zoneNum).Volume));
            state.dataDaylightingManager->AP = state.dataSurface->SurfaceWindow(ISurf).EnclAreaMinusThisSurf;
            state.dataDaylightingManager->ARHP = state.dataSurface->SurfaceWindow(ISurf).EnclAreaReflProdMinusThisSurf;
            // Average reflectance seen by light moving up (RhoCeilingWall) and down (RhoFloorWall)
            // across horizontal plane through center of window
            state.dataSurface->SurfWinRhoCeilingWall(IWin) =
                (state.dataDaylightingManager->ARHP(2) * (1.0 - ETA) + state.dataDaylightingManager->ARHP(3)) /
                (state.dataDaylightingManager->AP(2) * (1.0 - ETA) + state.dataDaylightingManager->AP(3) + 1.0e-5);
            state.dataSurface->SurfWinRhoFloorWall(IWin) = (state.dataDaylightingManager->ARHP(2) * ETA + state.dataDaylightingManager->ARHP(1)) /
                                                           (state.dataDaylightingManager->AP(2) * ETA + state.dataDaylightingManager->AP(1) + 1.e-9);

            // Angle factor for windows with diffusing shades. SurfaceWindow(IWin)%FractionUpgoing is
            // fraction of light from the shade that goes up toward ceiling and upper part of walls.
            // 1 - SurfaceWindow(IWin)%FractionUpgoing is fraction that goes down toward floor and lower part of walls.
            state.dataSurface->SurfWinFractionUpgoing(IWin) = state.dataSurface->Surface(IWin).Tilt / 180.0;

            // Daylighting shelf simplification:  All light goes up to the ceiling regardless of orientation of shelf
            if (state.dataSurface->SurfDaylightingShelfInd(IWin) > 0) {
                if (state.dataDaylightingDevicesData->Shelf(state.dataSurface->SurfDaylightingShelfInd(IWin)).InSurf > 0)
                    state.dataSurface->SurfWinFractionUpgoing(IWin) = 1.0;
            }
        }
    }
}

void CalcDayltgCoefficients(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       FW, Jan 2002: add variable slat angle blinds
    //                      FW, Mar 2002: add triangular windows
    //                      FW, Oct 2002: remove warning on window discretization relative to
    //                                    reference point distance to window plane
    //                      FW, Jan 2003: add between-glass shades and blinds
    //                      FW, Apr 2003: initialize shading type to 'NOSHADE' in window loop
    //                      PE, May 2003: add light pipes (tubular daylighting devices)
    //                      FW, Jul 2003: account for possible non-zero transmittance of
    //                                    shading surfaces (previously all shading surfaces were
    //                                    assumed to be opaque)
    //                      PE, Aug 2003: add daylighting shelves
    //                      FW, Sep 2003: write the bare-window overcast sky daylight factors to the eio file
    //                      FW, Nov 2003: add exterior beam and sky solar diffuse reflection from obstructions;
    //                                    add beam solar and sky solar reflection from ground with obstructions.
    //                      FW, Nov 2003: change expression for NDIVX, NDIVY (no. of window elements in X,Y) to
    //                                    round up to nearest integer rather than down
    //                      FW, Nov 2003: add specular reflection of beam solar from obstructions
    //                      RJH, Jan 2004: add alternative daylighting analysis using DElight
    //                                     All modifications demarked with RJH (Rob Hitchcock)
    //                      FW, Feb 2004: add daylighting through interior windows
    //                      FW, Apr 2004: add light well efficiency that multiplies glazing transmittance
    //                      FW, Apr 2004: add diffusing glazing
    //                      RJH, Jul 2004: add error handling for warnings/errors returned from DElight
    //                      LKL, Oct 2004: Separate "map" and "ref" point calculations -- move some input routines to
    //                                     separate routines.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates daylighting factors for later use in the time-step loop.

    // METHODOLOGY EMPLOYED:

    // For each combination of exterior window and reference point in a zone,
    // calculates daylighting factors (interior illuminance / exterior illuminance)
    // and glare factors for clear and overcast skies and for windows with and
    // without shading devices. These factors are calculated for each hourly
    // sun position for design days and for selected days throughout the year.

    // If a target zone has one or more interior windows, also calculates daylighting
    // factors for the target zone that are associated with exterior windows in adjacent
    // zones that share interior windows with the target zone.

    // The daylight illuminance at a reference point from a window is determined
    // by dividing the window into rectangular elements and calculating the illuminance
    // reaching the reference point directly from each element. The illumination
    // from an element can come from the sky or ground if the window is unshaded, or from
    // a shading device illuminated by solar radiation. Also considered are the
    // illuminance contribution from interreflection among the zone's interior surfaces
    // and sunlight striking the reference point.

    // In calculating sky-related interior illuminance and luminance quantities,
    // the sky luminance for the different sky types are determined from distributions
    // in which the zenith luminance is normalized to 1.0 cd/m2. Similarly, sun-related
    // illuminance and luminance quantities are based on beam normal solar illuminance
    // normalized to 1.0 lux.
    // The daylight and glare factors calculated in this subroutine are used in DayltgInteriorIllum
    // to get the daylight illuminance and glare at each time step.
    // Based on this information and user-input lighting setpoint and type of lighting
    // control system, DayltgElecLightingControl then determines how much the overhead electric lighting
    // can be reduced.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DCOF.

    //    int IHR;        // Hour of day counter
    //    int IWin;       // Window counter
    //    int loop;       // DO loop indices
    Real64 DaylFac; // sky daylight factor at ref pt i

    // added for output all daylight factors
    Real64 DFClrSky;
    Real64 DFClrTbSky;
    Real64 DFIntSky;
    Real64 DFOcSky;

    Real64 SlatAngle;
    int ISA;
    int ISlatAngle;

    if (state.dataDaylightingManager->CalcDayltghCoefficients_firstTime) {
        GetDaylightingParametersInput(state);
        CheckTDDsAndLightShelvesInDaylitZones(state);
        AssociateWindowShadingControlWithDaylighting(state);
        state.dataDaylightingManager->CalcDayltghCoefficients_firstTime = false;
    } // End of check if firstTime

    // Find the total number of exterior windows associated with all Daylighting:Detailed enclosures.
    // An exterior window is associated with such a enclosure if (1) it is an exterior window in the enclosure, or
    // (2) it is an exterior window in an adjacent enclosure that shares an interior window with the enclosure.
    // Note that exterior windows in category (2) may be counted more than once if an adjacent enclosure
    // is adjacent to more than one daylit enclosure with which the adjacent enclosure shares interior windows.
    // If there are no interior windows in a building, than TotWindowsWithDayl is just the total number of
    // exterior windows in Daylighting:Detailed enclosures. Note that it is possible for a
    // Daylighting:Detailed enclosure to have zero exterior windows of its own, but it may have an interior
    // through which daylight passes from adjacent enclosures with exterior windows.
    if ((int)state.dataDaylightingData->DaylRefPt.size() == 0) return;
    if (state.dataGlobal->BeginSimFlag) {
        state.dataDaylightingManager->TotWindowsWithDayl = 0;
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            state.dataDaylightingManager->TotWindowsWithDayl += state.dataDaylightingData->enclDaylight(enclNum).NumOfDayltgExtWins;
        }
    }

    if (state.dataDaylightingManager->TotWindowsWithDayl == 0) return;

    //-----------------------------------------!
    // Detailed daylighting factor calculation !
    //-----------------------------------------!
    if (!state.dataSysVars->DetailedSolarTimestepIntegration && !state.dataGlobal->KickOffSizing && !state.dataGlobal->KickOffSimulation) {
        if (state.dataGlobal->WarmupFlag) {
            DisplayString(state, "Calculating Detailed Daylighting Factors, Start Date=" + state.dataEnvrn->CurMnDy);
        } else {
            DisplayString(state, "Updating Detailed Daylighting Factors, Start Date=" + state.dataEnvrn->CurMnDy);
        }
    }

    if (state.dataGlobal->BeginSimFlag) {

        // Find minimum solid angle subtended by an interior window in Daylighting:Detailed zones.
        // Used in calculating daylighting through interior windows.
        CalcMinIntWinSolidAngs(state);

        // Warning if detailed daylighting has been requested for a zone with no associated exterior windows.
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
            if (thisEnclDaylight.NumOfDayltgExtWins == 0 && thisEnclDaylight.TotalExtWindows == 0) {
                for (int daylightCtrlNum : thisEnclDaylight.daylightControlIndexes) {
                    if (state.dataDaylightingData->daylightControl(daylightCtrlNum).TotalDaylRefPoints > 0) {
                        ShowWarningError(state,
                                         "Detailed daylighting will not be done for Daylighting:Controls=" +
                                             state.dataDaylightingData->daylightControl(daylightCtrlNum).Name);
                        ShowContinueError(state, "because it has no associated exterior windows.");
                    }
                }
            }

            // Find area and reflectance quantities used in calculating inter-reflected illuminance.
            // TH 9/10/2009. Need to calculate for zones without daylighting controls (TotalDaylRefPoints = 0)
            // but with adjacent zones having daylighting controls.
            if ((thisEnclDaylight.NumOfDayltgExtWins > 0) || thisEnclDaylight.adjEnclHasDayltgCtrl) {
                DayltgAveInteriorReflectance(state, enclNum);
            }
        }
    }

    // Zero daylighting factor arrays
    if ((int)state.dataDaylightingDevicesData->TDDPipe.size() > 0) {
        if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
            state.dataDaylightingManager->TDDTransVisBeam = 0.0;
            state.dataDaylightingManager->TDDFluxInc = 0.0;
            state.dataDaylightingManager->TDDFluxTrans = 0.0;
        } else {
            state.dataDaylightingManager->TDDTransVisBeam(state.dataGlobal->HourOfDay, {1, (int)state.dataDaylightingDevicesData->TDDPipe.size()}) =
                0.0;
            state.dataDaylightingManager->TDDFluxInc(
                state.dataGlobal->HourOfDay, {1, 4}, {1, (int)state.dataDaylightingDevicesData->TDDPipe.size()}) = 0.0;
            state.dataDaylightingManager->TDDFluxTrans(
                state.dataGlobal->HourOfDay, {1, 4}, {1, (int)state.dataDaylightingDevicesData->TDDPipe.size()}) = 0.0;
        }
    }

    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
        if (state.dataGlobal->BeginDayFlag) {
            // Calculate hourly sun angles, clear sky zenith luminance, and exterior horizontal illuminance
            state.dataDaylightingManager->PHSUN = 0.0;
            state.dataDaylightingManager->SPHSUN = 0.0;
            state.dataDaylightingManager->CPHSUN = 0.0;
            state.dataDaylightingManager->THSUN = 0.0;

            state.dataDaylightingManager->PHSUNHR = 0.0;
            state.dataDaylightingManager->SPHSUNHR = 0.0;
            state.dataDaylightingManager->CPHSUNHR = 0.0;
            state.dataDaylightingManager->THSUNHR = 0.0;
            state.dataDaylightingManager->GILSK = 0.0;
            state.dataDaylightingManager->GILSU = 0.0;
            for (int IHR = 1; IHR <= 24; ++IHR) {
                if (state.dataSurface->SurfSunCosHourly(IHR)(3) < DataEnvironment::SunIsUpValue)
                    continue; // Skip if sun is below horizon //Autodesk SurfSunCosHourly was uninitialized here
                state.dataDaylightingManager->PHSUN = DataGlobalConstants::PiOvr2 - std::acos(state.dataSurface->SurfSunCosHourly(IHR)(3));
                state.dataDaylightingManager->PHSUNHR(IHR) = state.dataDaylightingManager->PHSUN;
                state.dataDaylightingManager->SPHSUNHR(IHR) = std::sin(state.dataDaylightingManager->PHSUN);
                state.dataDaylightingManager->CPHSUNHR(IHR) = std::cos(state.dataDaylightingManager->PHSUN);
                state.dataDaylightingManager->THSUNHR(IHR) =
                    std::atan2(state.dataSurface->SurfSunCosHourly(IHR)(2), state.dataSurface->SurfSunCosHourly(IHR)(1));
                // Get exterior horizontal illuminance from sky and sun
                state.dataDaylightingManager->THSUN = state.dataDaylightingManager->THSUNHR(IHR);
                state.dataDaylightingManager->SPHSUN = state.dataDaylightingManager->SPHSUNHR(IHR);
                state.dataDaylightingManager->CPHSUN = state.dataDaylightingManager->CPHSUNHR(IHR);
                DayltgExtHorizIllum(state, state.dataDaylightingManager->GILSK(IHR, 1), state.dataDaylightingManager->GILSU(IHR));
            }
        }
    } else { // timestep integrated calculations
        state.dataDaylightingManager->PHSUN = 0.0;
        state.dataDaylightingManager->SPHSUN = 0.0;
        state.dataDaylightingManager->CPHSUN = 0.0;
        state.dataDaylightingManager->THSUN = 0.0;

        state.dataDaylightingManager->PHSUNHR(state.dataGlobal->HourOfDay) = 0.0;
        state.dataDaylightingManager->SPHSUNHR(state.dataGlobal->HourOfDay) = 0.0;
        state.dataDaylightingManager->CPHSUNHR(state.dataGlobal->HourOfDay) = 0.0;
        state.dataDaylightingManager->THSUNHR(state.dataGlobal->HourOfDay) = 0.0;
        state.dataDaylightingManager->GILSK(state.dataGlobal->HourOfDay, {1, 4}) = 0.0;
        state.dataDaylightingManager->GILSU(state.dataGlobal->HourOfDay) = 0.0;
        if (!(state.dataSurface->SurfSunCosHourly(state.dataGlobal->HourOfDay)(3) < DataEnvironment::SunIsUpValue)) { // Skip if sun is below horizon
            state.dataDaylightingManager->PHSUN =
                DataGlobalConstants::PiOvr2 - std::acos(state.dataSurface->SurfSunCosHourly(state.dataGlobal->HourOfDay)(3));
            state.dataDaylightingManager->PHSUNHR(state.dataGlobal->HourOfDay) = state.dataDaylightingManager->PHSUN;
            state.dataDaylightingManager->SPHSUNHR(state.dataGlobal->HourOfDay) = std::sin(state.dataDaylightingManager->PHSUN);
            state.dataDaylightingManager->CPHSUNHR(state.dataGlobal->HourOfDay) = std::cos(state.dataDaylightingManager->PHSUN);
            state.dataDaylightingManager->THSUNHR(state.dataGlobal->HourOfDay) =
                std::atan2(state.dataSurface->SurfSunCosHourly(state.dataGlobal->HourOfDay)(2),
                           state.dataSurface->SurfSunCosHourly(state.dataGlobal->HourOfDay)(1));
            // Get exterior horizontal illuminance from sky and sun
            state.dataDaylightingManager->THSUN = state.dataDaylightingManager->THSUNHR(state.dataGlobal->HourOfDay);
            state.dataDaylightingManager->SPHSUN = state.dataDaylightingManager->SPHSUNHR(state.dataGlobal->HourOfDay);
            state.dataDaylightingManager->CPHSUN = state.dataDaylightingManager->CPHSUNHR(state.dataGlobal->HourOfDay);
            DayltgExtHorizIllum(state,
                                state.dataDaylightingManager->GILSK(state.dataGlobal->HourOfDay, 1),
                                state.dataDaylightingManager->GILSU(state.dataGlobal->HourOfDay));
        }
    }

    CalcDayltgCoeffsRefMapPoints(state);

    if (state.dataDaylightingManager->doSkyReporting) {
        if (!state.dataGlobal->KickOffSizing && !state.dataGlobal->KickOffSimulation) {
            if (state.dataDaylightingManager->FirstTimeDaylFacCalc && state.dataDaylightingManager->TotWindowsWithDayl > 0) {
                // Write the bare-window four sky daylight factors at noon time to the eio file; this is done only
                // for first time that daylight factors are calculated and so is insensitive to possible variation
                // due to change in ground reflectance from month to month, or change in storm window status.
                static constexpr std::string_view Format_700(
                    "! <Sky Daylight Factors>, Sky Type, MonthAndDay, Daylighting Control Name, Enclosure Name, "
                    "Window Name, Reference Point, Daylight Factor\n");
                print(state.files.eio, Format_700);
                for (int controlNum = 1; controlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++controlNum) {
                    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(controlNum);
                    int enclNum = thisDaylightControl.enclIndex;
                    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);

                    if (thisEnclDaylight.NumOfDayltgExtWins == 0 || !thisEnclDaylight.hasSplitFluxDaylighting) continue;
                    for (int windowCounter = 1; windowCounter <= thisEnclDaylight.NumOfDayltgExtWins; ++windowCounter) {
                        int windowSurfNum = thisEnclDaylight.DayltgExtWinSurfNums(windowCounter);
                        // For this report, do not include ext wins in zone adjacent to ZoneNum since the inter-reflected
                        // component will not be calculated for these windows until the time-step loop.
                        if (state.dataSurface->Surface(windowSurfNum).SolarEnclIndex == enclNum) {
                            // Output for each reference point, for each sky. Group by sky type first
                            for (const DataDaylighting::SkyType &skyType : {DataDaylighting::SkyType::Clear,
                                                                            DataDaylighting::SkyType::ClearTurbid,
                                                                            DataDaylighting::SkyType::Intermediate,
                                                                            DataDaylighting::SkyType::Overcast}) {
                                std::string skyTypeString;
                                if (skyType == DataDaylighting::SkyType::Clear) {
                                    skyTypeString = "Clear Sky";
                                } else if (skyType == DataDaylighting::SkyType::ClearTurbid) {
                                    skyTypeString = "Clear Turbid Sky";
                                } else if (skyType == DataDaylighting::SkyType::Intermediate) {
                                    skyTypeString = "Intermediate Sky";
                                } else if (skyType == DataDaylighting::SkyType::Overcast) {
                                    skyTypeString = "Overcast Sky";
                                    //} else {
                                    //    // Should never happen
                                    //    skyTypeString = "ERROR_SKY_TYPE_NOT_HANDLED";
                                }

                                for (int refPtNum = 1; refPtNum <= thisDaylightControl.TotalDaylRefPoints; ++refPtNum) {
                                    DaylFac = thisDaylightControl.DaylIllFacSky(12, 1, static_cast<int>(skyType), refPtNum, windowCounter);
                                    print(state.files.eio,
                                          " Sky Daylight Factors,{},{},{},{},{},{},{:.4R}\n",
                                          skyTypeString,
                                          state.dataEnvrn->CurMnDy,
                                          thisDaylightControl.Name,
                                          state.dataViewFactor->EnclSolInfo(thisDaylightControl.enclIndex).Name,
                                          state.dataSurface->Surface(windowSurfNum).Name,
                                          state.dataDaylightingData->DaylRefPt(thisDaylightControl.DaylRefPtNum(refPtNum)).Name,
                                          DaylFac);
                                }
                            }
                        }
                    }
                }
                state.dataDaylightingManager->FirstTimeDaylFacCalc = false;
                state.dataDaylightingManager->doSkyReporting = false;
            }
        }
    }

    // Skip if no daylight windows
    if (state.dataDaylightingManager->TotWindowsWithDayl == 0) return;

    // Skip if no request of reporting
    if ((!state.dataDaylightingData->DFSReportSizingDays) && (!state.dataDaylightingData->DFSReportAllShadowCalculationDays)) return;

    // Skip duplicate calls
    if (state.dataGlobal->KickOffSizing) return;
    if (state.dataGlobal->DoingSizing) return;
    if (state.dataGlobal->KickOffSimulation) return;

    if (state.dataDaylightingData->DFSReportSizingDays) {
        if (state.dataGlobal->DoWeathSim && state.dataGlobal->DoDesDaySim) {
            if (state.dataGlobal->KindOfSim == DataGlobalConstants::KindOfSim::RunPeriodWeather) return;
        }
    }

    if (state.dataDaylightingData->DFSReportAllShadowCalculationDays) {
        if (state.dataGlobal->KindOfSim != DataGlobalConstants::KindOfSim::RunPeriodWeather) return;
    }

    // open a new file eplusout.dfs for saving the daylight factors
    if (state.dataDaylightingManager->CreateDFSReportFile) {
        InputOutputFile &dfs = state.files.dfs.ensure_open(state, "CalcDayltgCoefficients", state.files.outputControl.dfs);
        print(dfs, "{}\n", "This file contains daylight factors for all exterior windows of daylight enclosures.");
        print(dfs, "{}\n", "MonthAndDay,Enclosure Name,Zone Name,Window Name,Window State");
        print(dfs,
              "{}\n",
              "Hour,Reference Point,Daylight Factor for Clear Sky,Daylight Factor for Clear Turbid Sky,"
              "Daylight Factor for Intermediate Sky,Daylight Factor for Overcast Sky");
        state.dataDaylightingManager->CreateDFSReportFile = false;
    }

    for (int controlNum = 1; controlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++controlNum) {
        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(controlNum);
        int enclNum = thisDaylightControl.enclIndex;
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (thisEnclDaylight.NumOfDayltgExtWins == 0) continue;

        for (int windowCounter = 1; windowCounter <= thisEnclDaylight.NumOfDayltgExtWins; ++windowCounter) {
            int windowSurfNum = thisEnclDaylight.DayltgExtWinSurfNums(windowCounter);

            // For this report, do not include ext wins in zone/enclosure adjacent to ZoneNum since the inter-reflected
            // component will not be calculated for these windows until the time-step loop.
            if (state.dataSurface->Surface(windowSurfNum).SolarEnclIndex == enclNum) {

                if (state.dataSurface->SurfWinMovableSlats(windowSurfNum)) {
                    // variable slat angle - MaxSlatangle sets
                    ISA = MaxSlatAngs + 1;
                } else if (state.dataSurface->Surface(windowSurfNum).HasShadeControl) {
                    // window shade or blind with fixed slat angle
                    ISA = 2;
                } else {
                    // base window
                    ISA = 1;
                }

                // loop over each slat angle
                for (ISlatAngle = 1; ISlatAngle <= ISA; ++ISlatAngle) {
                    if (ISlatAngle == 1) {
                        // base window without shades, screens, or blinds
                        print(state.files.dfs,
                              "{},{},{},{},Base Window\n",
                              state.dataEnvrn->CurMnDy,
                              state.dataViewFactor->EnclSolInfo(enclNum).Name,
                              state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).Name,
                              state.dataSurface->Surface(windowSurfNum).Name);
                    } else if (ISlatAngle == 2 && ISA == 2) {
                        // window shade or blind with fixed slat angle
                        print(state.files.dfs,
                              "{},{},{},{},Blind or Slat Applied\n",
                              state.dataEnvrn->CurMnDy,
                              state.dataViewFactor->EnclSolInfo(enclNum).Name,
                              state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).Name,
                              state.dataSurface->Surface(windowSurfNum).Name);
                    } else {
                        // blind with variable slat angle
                        SlatAngle = 180.0 / double(MaxSlatAngs - 1) * double(ISlatAngle - 2);
                        print(state.files.dfs,
                              "{},{},{},{},{:.1R}\n",
                              state.dataEnvrn->CurMnDy,
                              state.dataViewFactor->EnclSolInfo(enclNum).Name,
                              state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).Name,
                              state.dataSurface->Surface(windowSurfNum).Name,
                              SlatAngle);
                    }

                    for (int IHR = 1; IHR <= 24; ++IHR) {
                        // For each Daylight Reference Point
                        for (int refPtNum = 1; refPtNum <= thisDaylightControl.TotalDaylRefPoints; ++refPtNum) {
                            DFClrSky = thisDaylightControl.DaylIllFacSky(
                                IHR, ISlatAngle, static_cast<int>(DataDaylighting::SkyType::Clear), refPtNum, windowCounter);
                            DFClrTbSky = thisDaylightControl.DaylIllFacSky(
                                IHR, ISlatAngle, static_cast<int>(DataDaylighting::SkyType::ClearTurbid), refPtNum, windowCounter);
                            DFIntSky = thisDaylightControl.DaylIllFacSky(
                                IHR, ISlatAngle, static_cast<int>(DataDaylighting::SkyType::Intermediate), refPtNum, windowCounter);
                            DFOcSky = thisDaylightControl.DaylIllFacSky(
                                IHR, ISlatAngle, static_cast<int>(DataDaylighting::SkyType::Overcast), refPtNum, windowCounter);

                            // write daylight factors - 4 sky types for each daylight ref point
                            print(state.files.dfs,
                                  "{},{},{:.5R},{:.5R},{:.5R},{:.5R}\n",
                                  IHR,
                                  state.dataDaylightingData->DaylRefPt(thisDaylightControl.DaylRefPtNum(refPtNum)).Name,
                                  DFClrSky,
                                  DFClrTbSky,
                                  DFIntSky,
                                  DFOcSky);
                        } // for (refPtNum) Reference Point
                    }     // for (IHR) hour
                }         // for (ISlatAngle) slat angle
            }             // if (SolarEnclIndex == enclNum)
        }                 // for (windowCounter) exterior windows in enclosure
    }                     // for (controlNum) daylighting control
}

void CalcDayltgCoeffsRefMapPoints(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2004
    //       MODIFIED       May 2006 (RR): added exterior window screens
    //                      April 2012 (LKL); change to allow multiple maps per zone

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine does the daylighting coefficient calculation for the
    // daylighting and illuminance map reference points.

    bool ErrorsFound;

    if (state.dataDaylightingManager->VeryFirstTime) {
        // make sure all necessary surfaces match to pipes
        ErrorsFound = false;
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            for (int loopwin = 1; loopwin <= state.dataDaylightingData->enclDaylight(enclNum).NumOfDayltgExtWins; ++loopwin) {
                int IWin = state.dataDaylightingData->enclDaylight(enclNum).DayltgExtWinSurfNums(loopwin);
                if (state.dataSurface->SurfWinOriginalClass(IWin) != SurfaceClass::TDD_Diffuser) continue;
                // Look up the TDD:DOME object
                int PipeNum = state.dataSurface->SurfWinTDDPipeNum(IWin);
                if (PipeNum == 0) {
                    ShowSevereError(state,
                                    "GetTDDInput: Surface=" + state.dataSurface->Surface(IWin).Name +
                                        ", TDD:Dome object does not reference a valid Diffuser object.");
                    ShowContinueError(state, "...needs DaylightingDevice:Tubular of same name as Surface.");
                    ErrorsFound = true;
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Not all TubularDaylightDome objects have corresponding DaylightingDevice:Tubular objects. Program terminates.");
        }
        state.dataDaylightingManager->VeryFirstTime = false;
    }

    // Calc for daylighting reference points for daylighting controls that use SplitFlux method
    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        if (state.dataDaylightingData->daylightControl(daylightCtrlNum).DaylightMethod != DataDaylighting::DaylightingMethod::SplitFlux) continue;
        // Skip enclosures with no exterior windows or in adjacent enclosure(s) with which an interior window is shared
        if (state.dataDaylightingData->enclDaylight(state.dataDaylightingData->daylightControl(daylightCtrlNum).enclIndex).NumOfDayltgExtWins == 0)
            continue;
        CalcDayltgCoeffsRefPoints(state, daylightCtrlNum);
    }
    if (!state.dataGlobal->DoingSizing && !state.dataGlobal->KickOffSimulation) {
        // Calc for illuminance maps
        if ((int)state.dataDaylightingData->IllumMap.size() > 0) {
            for (int MapNum = 1; MapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++MapNum) {
                int mapZoneNum = state.dataDaylightingData->IllumMapCalc(MapNum).zoneIndex;
                if (state.dataGlobal->WarmupFlag) {
                    DisplayString(state, "Calculating Daylighting Coefficients (Map Points), Zone=" + state.dataHeatBal->Zone(mapZoneNum).Name);
                } else {
                    DisplayString(state, "Updating Daylighting Coefficients (Map Points), Zone=" + state.dataHeatBal->Zone(mapZoneNum).Name);
                }
                CalcDayltgCoeffsMapPoints(state, MapNum);
            }
        }
    }
}

void CalcDayltgCoeffsRefPoints(EnergyPlusData &state, int const daylightCtrlNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   April 2012
    //       MODIFIED       November 2012 (B. Griffith), refactor for detailed timestep integration and remove duplicate code
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides calculations for Daylighting Coefficients for daylighting reference points

    int IHR;       // Hour of day counter
    int NRF;       // Number of daylighting reference points in a zone
    int IL;        // Reference point counter
    Real64 AZVIEW; // Azimuth of view vector in absolute coord system for
    //  glare calculation (radians)
    int IConst;            // Construction counter
    int ICtrl;             // Window control counter
    int IWin;              // Window counter
    int IWin2;             // Secondary window counter (for TDD:DOME object, if exists)
    int InShelfSurf;       // Inside daylighting shelf surface number
    WinShadingType ShType; // Window shading type
    int BlNum;             // Window Blind Number
    int LSHCAL;            // Interior shade calculation flag: 0=not yet
    //  calculated, 1=already calculated
    int NWX;     // Number of window elements in x direction for dayltg calc
    int NWY;     // Number of window elements in y direction for dayltg calc
    int NWYlim;  // For triangle, largest NWY for a given IX
    int IX;      // Counter for window elements in the x direction
    int IY;      // Counter for window elements in the y direction
    Real64 COSB; // Cosine of angle between window outward normal and ray from
    //  reference point to window element
    Real64 PHRAY;  // Altitude of ray from reference point to window element (radians)
    Real64 THRAY;  // Azimuth of ray from reference point to window element (radians)
    Real64 DOMEGA; // Solid angle subtended by window element wrt reference point (steradians)
    Real64 TVISB;  // Visible transmittance of window for COSB angle of incidence (times light well
    //   efficiency, if appropriate)
    int ISunPos; // Sun position counter; used to avoid calculating various
    //  quantities that do not depend on sun position.
    Real64 ObTrans; // Product of solar transmittances of exterior obstructions hit by ray
    // from reference point through a window element
    int loopwin;                            // loop index for exterior windows associated with a daylit zone
    bool is_Rectangle;                      // True if window is rectangular
    bool is_Triangle;                       // True if window is triangular
    Real64 DWX;                             // Horizontal dimension of window element (m)
    Real64 DWY;                             // Vertical dimension of window element (m)
    Real64 DAXY;                            // Area of window element
    Real64 SkyObstructionMult;              // Ratio of obstructed to unobstructed sky diffuse at a ground point
    DataDaylighting::ExtWinType ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    int BRef;
    int ILB;
    bool hitIntObs;        // True iff interior obstruction hit
    bool hitExtObs;        // True iff ray from ref pt to ext win hits an exterior obstruction
    Real64 TVISIntWin;     // Visible transmittance of int win at COSBIntWin for light from ext win
    Real64 TVISIntWinDisk; // Visible transmittance of int win at COSBIntWin for sun

    auto &W2 = state.dataDaylightingManager->W2;
    auto &W3 = state.dataDaylightingManager->W3;
    auto &W21 = state.dataDaylightingManager->W21;
    auto &W23 = state.dataDaylightingManager->W23;
    auto &RREF = state.dataDaylightingManager->RREF;
    auto &RREF2 = state.dataDaylightingManager->RREF2;
    auto &RWIN = state.dataDaylightingManager->RWIN;
    auto &RWIN2 = state.dataDaylightingManager->RWIN2;
    auto &Ray = state.dataDaylightingManager->Ray;
    auto &WNORM2 = state.dataDaylightingManager->WNORM2;
    auto &VIEWVC = state.dataDaylightingManager->VIEWVC;
    auto &U2 = state.dataDaylightingManager->U2;
    auto &U21 = state.dataDaylightingManager->U21;
    auto &U23 = state.dataDaylightingManager->U23;
    auto &VIEWVC2 = state.dataDaylightingManager->VIEWVC2;

    int WinEl; // Current window element

    if (state.dataDaylightingManager->refFirstTime && (state.dataDaylightingData->maxRefPointsPerControl > 0)) {
        state.dataDaylightingManager->RefErrIndex.allocate(state.dataDaylightingData->maxRefPointsPerControl, state.dataSurface->TotSurfaces);
        state.dataDaylightingManager->RefErrIndex = 0;
        state.dataDaylightingManager->refFirstTime = false;
    }

    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(thisDaylightControl.enclIndex);
    int zoneNum = thisDaylightControl.zoneIndex;
    // Azimuth of view vector in absolute coord sys
    AZVIEW = (thisDaylightControl.ViewAzimuthForGlare + state.dataHeatBal->Zone(zoneNum).RelNorth + state.dataHeatBal->BuildingAzimuth +
              state.dataHeatBal->BuildingRotationAppendixG) *
             DataGlobalConstants::DegToRadians;
    // View vector components in absolute coord sys
    VIEWVC(1) = std::sin(AZVIEW);
    VIEWVC(2) = std::cos(AZVIEW);
    VIEWVC(3) = 0.0;

    thisDaylightControl.DaylIllumAtRefPt = 0.0;  // Daylight illuminance at reference points (lux)
    thisDaylightControl.GlareIndexAtRefPt = 0.0; // Glare index at reference points
    thisDaylightControl.SolidAngAtRefPt = 0.0;
    thisDaylightControl.SolidAngAtRefPtWtd = 0.0;
    thisDaylightControl.IllumFromWinAtRefPt = 0.0;
    thisDaylightControl.BackLumFromWinAtRefPt = 0.0;
    thisDaylightControl.SourceLumFromWinAtRefPt = 0.0;

    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {

        thisDaylightControl.DaylIllFacSky = 0.0;
        thisDaylightControl.DaylSourceFacSky = 0.0;
        thisDaylightControl.DaylBackFacSky = 0.0;
        thisDaylightControl.DaylIllFacSun = 0.0;
        thisDaylightControl.DaylIllFacSunDisk = 0.0;
        thisDaylightControl.DaylSourceFacSun = 0.0;
        thisDaylightControl.DaylSourceFacSunDisk = 0.0;
        thisDaylightControl.DaylBackFacSun = 0.0;
        thisDaylightControl.DaylBackFacSunDisk = 0.0;
    } else {
        int numRefPts = thisDaylightControl.TotalDaylRefPoints;
        thisDaylightControl.DaylIllFacSky(state.dataGlobal->HourOfDay,
                                          {1, state.dataSurface->actualMaxSlatAngs + 1},
                                          {1, 4},
                                          {1, numRefPts},
                                          {1, thisEnclDaylight.NumOfDayltgExtWins}) = 0.0;
        thisDaylightControl.DaylSourceFacSky(state.dataGlobal->HourOfDay,
                                             {1, state.dataSurface->actualMaxSlatAngs + 1},
                                             {1, 4},
                                             {1, numRefPts},
                                             {1, thisEnclDaylight.NumOfDayltgExtWins}) = 0.0;
        thisDaylightControl.DaylBackFacSky(state.dataGlobal->HourOfDay,
                                           {1, state.dataSurface->actualMaxSlatAngs + 1},
                                           {1, 4},
                                           {1, numRefPts},
                                           {1, thisEnclDaylight.NumOfDayltgExtWins}) = 0.0;
        thisDaylightControl.DaylIllFacSun(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        thisDaylightControl.DaylIllFacSunDisk(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        thisDaylightControl.DaylSourceFacSun(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        thisDaylightControl.DaylSourceFacSunDisk(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        thisDaylightControl.DaylBackFacSun(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        thisDaylightControl.DaylBackFacSunDisk(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
    }

    NRF = thisDaylightControl.TotalDaylRefPoints;
    BRef = 0;

    for (IL = 1; IL <= NRF; ++IL) {
        // Reference point in absolute coordinate system
        RREF = thisDaylightControl.DaylRefPtAbsCoord({1, 3}, IL); // ( x, y, z )

        //           -------------
        // ---------- WINDOW LOOP ----------
        //           -------------
        for (loopwin = 1; loopwin <= thisEnclDaylight.NumOfDayltgExtWins; ++loopwin) {

            FigureDayltgCoeffsAtPointsSetupForWindow(state,
                                                     daylightCtrlNum,
                                                     IL,
                                                     loopwin,
                                                     DataDaylighting::CalledFor::RefPoint,
                                                     RREF,
                                                     VIEWVC,
                                                     IWin,
                                                     IWin2,
                                                     NWX,
                                                     NWY,
                                                     W2,
                                                     W3,
                                                     W21,
                                                     W23,
                                                     LSHCAL,
                                                     InShelfSurf,
                                                     ICtrl,
                                                     ShType,
                                                     BlNum,
                                                     WNORM2,
                                                     ExtWinType,
                                                     IConst,
                                                     RREF2,
                                                     DWX,
                                                     DWY,
                                                     DAXY,
                                                     U2,
                                                     U23,
                                                     U21,
                                                     VIEWVC2,
                                                     is_Rectangle,
                                                     is_Triangle);
            //           ---------------------
            // ---------- WINDOW ELEMENT LOOP ----------
            //           ---------------------

            WinEl = 0;

            for (IX = 1; IX <= NWX; ++IX) {
                if (is_Rectangle) {
                    NWYlim = NWY;
                } else if (is_Triangle) {
                    NWYlim = NWY - IX + 1;
                }

                for (IY = 1; IY <= NWYlim; ++IY) {

                    ++WinEl;

                    FigureDayltgCoeffsAtPointsForWindowElements(state,
                                                                daylightCtrlNum,
                                                                IL,
                                                                loopwin,
                                                                DataDaylighting::CalledFor::RefPoint,
                                                                WinEl,
                                                                IWin,
                                                                IWin2,
                                                                IX,
                                                                IY,
                                                                SkyObstructionMult,
                                                                W2,
                                                                W21,
                                                                W23,
                                                                RREF,
                                                                NWYlim,
                                                                VIEWVC2,
                                                                DWX,
                                                                DWY,
                                                                DAXY,
                                                                U2,
                                                                U23,
                                                                U21,
                                                                RWIN,
                                                                RWIN2,
                                                                Ray,
                                                                PHRAY,
                                                                LSHCAL,
                                                                COSB,
                                                                ObTrans,
                                                                TVISB,
                                                                DOMEGA,
                                                                THRAY,
                                                                hitIntObs,
                                                                hitExtObs,
                                                                WNORM2,
                                                                ExtWinType,
                                                                IConst,
                                                                RREF2,
                                                                is_Triangle,
                                                                TVISIntWin,
                                                                TVISIntWinDisk);

                    //           -------------------
                    // ---------- SUN POSITION LOOP ----------
                    //           -------------------

                    // Sun position counter. Used to avoid calculating various quantities
                    // that do not depend on sun position.

                    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
                        ISunPos = 0;
                        for (IHR = 1; IHR <= 24; ++IHR) {

                            FigureDayltgCoeffsAtPointsForSunPosition(state,
                                                                     daylightCtrlNum,
                                                                     IL,
                                                                     IX,
                                                                     NWX,
                                                                     IY,
                                                                     NWYlim,
                                                                     WinEl,
                                                                     IWin,
                                                                     IWin2,
                                                                     IHR,
                                                                     ISunPos,
                                                                     SkyObstructionMult,
                                                                     RWIN2,
                                                                     Ray,
                                                                     PHRAY,
                                                                     LSHCAL,
                                                                     InShelfSurf,
                                                                     COSB,
                                                                     ObTrans,
                                                                     TVISB,
                                                                     DOMEGA,
                                                                     ICtrl,
                                                                     ShType,
                                                                     BlNum,
                                                                     THRAY,
                                                                     WNORM2,
                                                                     ExtWinType,
                                                                     IConst,
                                                                     AZVIEW,
                                                                     RREF2,
                                                                     hitIntObs,
                                                                     hitExtObs,
                                                                     DataDaylighting::CalledFor::RefPoint,
                                                                     TVISIntWin,
                                                                     TVISIntWinDisk);

                        }    // End of hourly sun position loop, IHR
                    } else { // timestep integrated
                        if (state.dataEnvrn->SunIsUp && !state.dataDaylightingManager->MySunIsUpFlag) {
                            ISunPos = 0;
                            state.dataDaylightingManager->MySunIsUpFlag = true;
                        } else if (state.dataEnvrn->SunIsUp && state.dataDaylightingManager->MySunIsUpFlag) {
                            ISunPos = 1;
                        } else if (!state.dataEnvrn->SunIsUp && state.dataDaylightingManager->MySunIsUpFlag) {
                            state.dataDaylightingManager->MySunIsUpFlag = false;
                            ISunPos = -1;
                        } else if (!state.dataEnvrn->SunIsUp && !state.dataDaylightingManager->MySunIsUpFlag) {
                            ISunPos = -1;
                        }

                        FigureDayltgCoeffsAtPointsForSunPosition(state,
                                                                 daylightCtrlNum,
                                                                 IL,
                                                                 IX,
                                                                 NWX,
                                                                 IY,
                                                                 NWYlim,
                                                                 WinEl,
                                                                 IWin,
                                                                 IWin2,
                                                                 state.dataGlobal->HourOfDay,
                                                                 ISunPos,
                                                                 SkyObstructionMult,
                                                                 RWIN2,
                                                                 Ray,
                                                                 PHRAY,
                                                                 LSHCAL,
                                                                 InShelfSurf,
                                                                 COSB,
                                                                 ObTrans,
                                                                 TVISB,
                                                                 DOMEGA,
                                                                 ICtrl,
                                                                 ShType,
                                                                 BlNum,
                                                                 THRAY,
                                                                 WNORM2,
                                                                 ExtWinType,
                                                                 IConst,
                                                                 AZVIEW,
                                                                 RREF2,
                                                                 hitIntObs,
                                                                 hitExtObs,
                                                                 DataDaylighting::CalledFor::RefPoint,
                                                                 TVISIntWin,
                                                                 TVISIntWinDisk);
                    }

                } // End of window Y-element loop, IY
            }     // End of window X-element loop, IX

            // Loop again over hourly sun positions and calculate daylight factors by adding
            // direct and inter-reflected illum components, then dividing by exterior horiz illum.
            // Also calculate corresponding glare factors.

            ILB = BRef + IL;

            if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
                ISunPos = 0;
                for (IHR = 1; IHR <= 24; ++IHR) {
                    FigureRefPointDayltgFactorsToAddIllums(state, daylightCtrlNum, ILB, IHR, ISunPos, IWin, loopwin, NWX, NWY, ICtrl);

                } // End of sun position loop, IHR
            } else {
                if (state.dataEnvrn->SunIsUp && !state.dataDaylightingManager->MySunIsUpFlag) {
                    ISunPos = 0;
                    state.dataDaylightingManager->MySunIsUpFlag = true;
                } else if (state.dataEnvrn->SunIsUp && state.dataDaylightingManager->MySunIsUpFlag) {
                    ISunPos = 1;
                } else if (!state.dataEnvrn->SunIsUp && state.dataDaylightingManager->MySunIsUpFlag) {
                    state.dataDaylightingManager->MySunIsUpFlag = false;
                    ISunPos = -1;
                } else if (!state.dataEnvrn->SunIsUp && !state.dataDaylightingManager->MySunIsUpFlag) {
                    ISunPos = -1;
                }
                FigureRefPointDayltgFactorsToAddIllums(
                    state, daylightCtrlNum, ILB, state.dataGlobal->HourOfDay, ISunPos, IWin, loopwin, NWX, NWY, ICtrl);
            }
        } // End of window loop, loopwin - IWin

    } // End of reference point loop, IL
}

void CalcDayltgCoeffsMapPoints(EnergyPlusData &state, int const mapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   April 2012
    //       MODIFIED      November 2012 (B. Griffith), refactor for detailed timestep integration and remove duplicate code
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Provides calculations for Daylighting Coefficients for map illuminance points

    // METHODOLOGY EMPLOYED:
    // Was previously part of CalcDayltgCoeffsRefMapPoints -- broken out to all multiple
    // maps per zone

    //  In the following four variables, I=1 for clear sky, 2 for overcast.
    int IHR;       // Hour of day counter
    int numRefPts; // Number of daylighting reference points in a zone
    int IL;        // Reference point counter
    Real64 AZVIEW; // Azimuth of view vector in absolute coord system for
    //  glare calculation (radians)
    int IConst;            // Construction counter
    int ICtrl;             // Window control counter
    int IWin;              // Window counter
    int IWin2;             // Secondary window counter (for TDD:DOME object, if exists)
    int InShelfSurf;       // Inside daylighting shelf surface number
    WinShadingType ShType; // Window shading type
    int BlNum;             // Window Blind Number
    int LSHCAL;            // Interior shade calculation flag: 0=not yet
    //  calculated, 1=already calculated
    int NWX;     // Number of window elements in x direction for dayltg calc
    int NWY;     // Number of window elements in y direction for dayltg calc
    int NWYlim;  // For triangle, largest NWY for a given IX
    Real64 DWX;  // Horizontal dimension of window element (m)
    Real64 DWY;  // Vertical dimension of window element (m)
    int IX;      // Counter for window elements in the x direction
    int IY;      // Counter for window elements in the y direction
    Real64 COSB; // Cosine of angle between window outward normal and ray from
    //  reference point to window element
    Real64 PHRAY;  // Altitude of ray from reference point to window element (radians)
    Real64 THRAY;  // Azimuth of ray from reference point to window element (radians)
    Real64 DOMEGA; // Solid angle subtended by window element wrt reference point (steradians)
    Real64 TVISB;  // Visible transmittance of window for COSB angle of incidence (times light well
    //   efficiency, if appropriate)
    int ISunPos; // Sun position counter; used to avoid calculating various
    //  quantities that do not depend on sun position.
    Real64 ObTrans; // Product of solar transmittances of exterior obstructions hit by ray
    // from reference point through a window element
    int loopwin;                            // loop index for exterior windows associated with a daylit zone
    bool is_Rectangle;                      // True if window is rectangular
    bool is_Triangle;                       // True if window is triangular
    Real64 DAXY;                            // Area of window element
    Real64 SkyObstructionMult;              // Ratio of obstructed to unobstructed sky diffuse at a ground point
    DataDaylighting::ExtWinType ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    int ILB;
    bool hitIntObs;        // True iff interior obstruction hit
    bool hitExtObs;        // True iff ray from ref pt to ext win hits an exterior obstruction
    Real64 TVISIntWin;     // Visible transmittance of int win at COSBIntWin for light from ext win
    Real64 TVISIntWinDisk; // Visible transmittance of int win at COSBIntWin for sun
    auto &MySunIsUpFlag(state.dataDaylightingManager->CalcDayltgCoeffsMapPointsMySunIsUpFlag);
    int WinEl; // window elements counter

    auto &W2 = state.dataDaylightingManager->W2;
    auto &W3 = state.dataDaylightingManager->W3;
    auto &W21 = state.dataDaylightingManager->W21;
    auto &W23 = state.dataDaylightingManager->W23;
    auto &RREF = state.dataDaylightingManager->RREF;
    auto &RREF2 = state.dataDaylightingManager->RREF2;
    auto &RWIN = state.dataDaylightingManager->RWIN;
    auto &RWIN2 = state.dataDaylightingManager->RWIN2;
    auto &Ray = state.dataDaylightingManager->Ray;
    auto &WNORM2 = state.dataDaylightingManager->WNORM2;
    auto &VIEWVC = state.dataDaylightingManager->VIEWVC;
    auto &U2 = state.dataDaylightingManager->U2;
    auto &U21 = state.dataDaylightingManager->U21;
    auto &U23 = state.dataDaylightingManager->U23;
    auto &VIEWVC2 = state.dataDaylightingManager->VIEWVC2;

    if (state.dataDaylightingManager->mapFirstTime && (int)state.dataDaylightingData->IllumMap.size() > 0) {
        IL = -999;
        for (int MapNum = 1; MapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++MapNum) {
            IL = max(IL, state.dataDaylightingData->IllumMapCalc(MapNum).TotalMapRefPoints);
        }
        state.dataDaylightingManager->MapErrIndex.dimension(IL, state.dataSurface->TotSurfaces, 0);
        state.dataDaylightingManager->mapFirstTime = false;
    }

    int enclNum = state.dataDaylightingData->IllumMapCalc(mapNum).enclIndex;
    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);

    // Azimuth of view vector in absolute coord sys - set to zero here, because glare isn't calculated for map points
    // but these are arguments to some of the functions that are shared with regular reference points, so initalize here.
    AZVIEW = 0.0;
    // View vector components in absolute coord sys
    VIEWVC(1) = 0.0;
    VIEWVC(2) = 0.0;
    VIEWVC(3) = 0.0;

    numRefPts = state.dataDaylightingData->IllumMapCalc(mapNum).TotalMapRefPoints;

    state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPt = 0.0; // Daylight illuminance at reference points (lux)
    state.dataDaylightingData->IllumMapCalc(mapNum).IllumFromWinAtMapPt = 0.0;
    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSky = 0.0;
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSun = 0.0;
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSunDisk = 0.0;
    } else {
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSky(state.dataGlobal->HourOfDay,
                                                                      {1, state.dataSurface->actualMaxSlatAngs + 1},
                                                                      {1, 4},
                                                                      {1, numRefPts},
                                                                      {1, thisEnclDaylight.NumOfDayltgExtWins}) = 0.0;
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSun(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
        state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSunDisk(
            state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, numRefPts}, {1, thisEnclDaylight.NumOfDayltgExtWins}) =
            0.0;
    }

    for (IL = 1; IL <= numRefPts; ++IL) {

        RREF = state.dataDaylightingData->IllumMapCalc(mapNum).MapRefPtAbsCoord({1, 3}, IL); // (x, y, z)

        //           -------------
        // ---------- WINDOW LOOP ----------
        //           -------------

        for (loopwin = 1; loopwin <= thisEnclDaylight.NumOfDayltgExtWins; ++loopwin) {

            // daylightingCtrlNum parameter is unused for map points
            FigureDayltgCoeffsAtPointsSetupForWindow(state,
                                                     0,
                                                     IL,
                                                     loopwin,
                                                     DataDaylighting::CalledFor::MapPoint,
                                                     RREF,
                                                     VIEWVC,
                                                     IWin,
                                                     IWin2,
                                                     NWX,
                                                     NWY,
                                                     W2,
                                                     W3,
                                                     W21,
                                                     W23,
                                                     LSHCAL,
                                                     InShelfSurf,
                                                     ICtrl,
                                                     ShType,
                                                     BlNum,
                                                     WNORM2,
                                                     ExtWinType,
                                                     IConst,
                                                     RREF2,
                                                     DWX,
                                                     DWY,
                                                     DAXY,
                                                     U2,
                                                     U23,
                                                     U21,
                                                     VIEWVC2,
                                                     is_Rectangle,
                                                     is_Triangle,
                                                     mapNum);
            //           ---------------------
            // ---------- WINDOW ELEMENT LOOP ----------
            //           ---------------------
            WinEl = 0;

            for (IX = 1; IX <= NWX; ++IX) {
                if (is_Rectangle) {
                    NWYlim = NWY;
                } else if (is_Triangle) {
                    NWYlim = NWY - IX + 1;
                }

                for (IY = 1; IY <= NWYlim; ++IY) {

                    ++WinEl;

                    // daylightingCtrlNum parameter is unused for map points
                    FigureDayltgCoeffsAtPointsForWindowElements(state,
                                                                0,
                                                                IL,
                                                                loopwin,
                                                                DataDaylighting::CalledFor::MapPoint,
                                                                WinEl,
                                                                IWin,
                                                                IWin2,
                                                                IX,
                                                                IY,
                                                                SkyObstructionMult,
                                                                W2,
                                                                W21,
                                                                W23,
                                                                RREF,
                                                                NWYlim,
                                                                VIEWVC2,
                                                                DWX,
                                                                DWY,
                                                                DAXY,
                                                                U2,
                                                                U23,
                                                                U21,
                                                                RWIN,
                                                                RWIN2,
                                                                Ray,
                                                                PHRAY,
                                                                LSHCAL,
                                                                COSB,
                                                                ObTrans,
                                                                TVISB,
                                                                DOMEGA,
                                                                THRAY,
                                                                hitIntObs,
                                                                hitExtObs,
                                                                WNORM2,
                                                                ExtWinType,
                                                                IConst,
                                                                RREF2,
                                                                is_Triangle,
                                                                TVISIntWin,
                                                                TVISIntWinDisk,
                                                                mapNum);
                    //           -------------------
                    // ---------- SUN POSITION LOOP ----------
                    //           -------------------

                    // Sun position counter. Used to avoid calculating various quantities
                    // that do not depend on sun position.
                    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
                        ISunPos = 0;
                        for (IHR = 1; IHR <= 24; ++IHR) {
                            // daylightingCtrlNum parameter is unused for map points
                            FigureDayltgCoeffsAtPointsForSunPosition(state,
                                                                     0,
                                                                     IL,
                                                                     IX,
                                                                     NWX,
                                                                     IY,
                                                                     NWYlim,
                                                                     WinEl,
                                                                     IWin,
                                                                     IWin2,
                                                                     IHR,
                                                                     ISunPos,
                                                                     SkyObstructionMult,
                                                                     RWIN2,
                                                                     Ray,
                                                                     PHRAY,
                                                                     LSHCAL,
                                                                     InShelfSurf,
                                                                     COSB,
                                                                     ObTrans,
                                                                     TVISB,
                                                                     DOMEGA,
                                                                     ICtrl,
                                                                     ShType,
                                                                     BlNum,
                                                                     THRAY,
                                                                     WNORM2,
                                                                     ExtWinType,
                                                                     IConst,
                                                                     AZVIEW,
                                                                     RREF2,
                                                                     hitIntObs,
                                                                     hitExtObs,
                                                                     DataDaylighting::CalledFor::MapPoint,
                                                                     TVISIntWin,
                                                                     TVISIntWinDisk,
                                                                     mapNum);
                        } // End of hourly sun position loop, IHR
                    } else {
                        if (state.dataEnvrn->SunIsUp && !MySunIsUpFlag) {
                            ISunPos = 0;
                            MySunIsUpFlag = true;
                        } else if (state.dataEnvrn->SunIsUp && MySunIsUpFlag) {
                            ISunPos = 1;
                        } else if (!state.dataEnvrn->SunIsUp && MySunIsUpFlag) {
                            MySunIsUpFlag = false;
                            ISunPos = -1;
                        } else if (!state.dataEnvrn->SunIsUp && !MySunIsUpFlag) {
                            ISunPos = -1;
                        }
                        // daylightingCtrlNum parameter is unused for map points
                        FigureDayltgCoeffsAtPointsForSunPosition(state,
                                                                 0,
                                                                 IL,
                                                                 IX,
                                                                 NWX,
                                                                 IY,
                                                                 NWYlim,
                                                                 WinEl,
                                                                 IWin,
                                                                 IWin2,
                                                                 state.dataGlobal->HourOfDay,
                                                                 ISunPos,
                                                                 SkyObstructionMult,
                                                                 RWIN2,
                                                                 Ray,
                                                                 PHRAY,
                                                                 LSHCAL,
                                                                 InShelfSurf,
                                                                 COSB,
                                                                 ObTrans,
                                                                 TVISB,
                                                                 DOMEGA,
                                                                 ICtrl,
                                                                 ShType,
                                                                 BlNum,
                                                                 THRAY,
                                                                 WNORM2,
                                                                 ExtWinType,
                                                                 IConst,
                                                                 AZVIEW,
                                                                 RREF2,
                                                                 hitIntObs,
                                                                 hitExtObs,
                                                                 DataDaylighting::CalledFor::MapPoint,
                                                                 TVISIntWin,
                                                                 TVISIntWinDisk,
                                                                 mapNum);
                    }
                } // End of window Y-element loop, IY
            }     // End of window X-element loop, IX

            if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
                // Loop again over hourly sun positions and calculate daylight factors by adding
                // direct and inter-reflected illum components, then dividing by exterior horiz illum.
                // Also calculate corresponding glare factors.
                ILB = IL;
                for (IHR = 1; IHR <= 24; ++IHR) {
                    FigureMapPointDayltgFactorsToAddIllums(state, mapNum, ILB, IHR, IWin, loopwin, ICtrl);
                } // End of sun position loop, IHR
            } else {
                ILB = IL;
                FigureMapPointDayltgFactorsToAddIllums(state, mapNum, ILB, state.dataGlobal->HourOfDay, IWin, loopwin, ICtrl);
            }

        } // End of window loop, loopwin - IWin

    } // End of reference point loop, IL
}

void FigureDayltgCoeffsAtPointsSetupForWindow(
    EnergyPlusData &state,
    int const daylightCtrlNum, // zero if called for map points
    int const iRefPoint,
    int const loopwin,
    DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
    Vector3<Real64> const &RREF,                 // Location of a reference point in absolute coordinate system
    Vector3<Real64> const &VIEWVC,               // View vector in absolute coordinate system
    int &IWin,
    int &IWin2,
    int &NWX,
    int &NWY,
    Vector3<Real64> &W2,                     // Second vertex of window
    Vector3<Real64> &W3,                     // Third vertex of window
    Vector3<Real64> &W21,                    // Vector from window vertex 2 to window vertex 1
    Vector3<Real64> &W23,                    // Vector from window vertex 2 to window vertex 3
    int &LSHCAL,                             // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
    int &InShelfSurf,                        // Inside daylighting shelf surface number
    int &ICtrl,                              // Window control counter
    WinShadingType &ShType,                  // Window shading type
    int &BlNum,                              // Window blind number
    Vector3<Real64> &WNORM2,                 // Unit vector normal to window
    DataDaylighting::ExtWinType &ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    int &IConst,                             // Construction counter
    Vector3<Real64> &RREF2,                  // Location of virtual reference point in absolute coordinate system
    Real64 &DWX,                             // Horizontal dimension of window element (m)
    Real64 &DWY,                             // Vertical dimension of window element (m)
    Real64 &DAXY,                            // Area of window element
    Vector3<Real64> &U2,                     // Second vertex of window for TDD:DOME (if exists)
    Vector3<Real64> &U23,                    // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
    Vector3<Real64> &U21,                    // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
    Vector3<Real64> &VIEWVC2,                // Virtual view vector in absolute coordinate system
    bool &is_Rectangle,                      // True if window is rectangular
    bool &is_Triangle,                       // True if window is triangular
    int const MapNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann

    // PURPOSE OF THIS SUBROUTINE:
    // collect code to setup calculations for each window for daylighting coefficients

    // METHODOLOGY EMPLOYED:
    // switch as need to serve both reference points and map points based on calledFrom

    int ShelfNum;     // Daylighting shelf object number
    int IConstShaded; // Shaded construction counter
    Real64 WW;        // Window width (m)
    Real64 HW;        // Window height (m)

    int NDIVX;  // Number of window x divisions for daylighting calc
    int NDIVY;  // Number of window y divisions for daylighting calc
    Real64 ALF; // Distance from reference point to window plane (m)
    Real64 D1a; // Projection of vector from window origin to reference
    //  on window X  axis (m)
    Real64 D1b; // Projection of vector from window origin to reference
    //  on window Y axis (m)
    Real64 SolidAngExtWin;    // Approx. solid angle subtended by an ext. window wrt ref pt
    Real64 SolidAngMinIntWin; // Approx. smallest solid angle subtended by an int. window wrt ref pt
    Real64 SolidAngRatio;     // Ratio of SolidAngExtWin and SolidAngMinIntWin
    int PipeNum;              // TDD pipe object number
    Real64 SinCornerAng;      // For triangle, sine of corner angle of window element

    int zoneNum = 0; // zone number
    int enclNum = 0; // enclosure number

    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        state.dataDaylightingData->daylightControl(daylightCtrlNum).SolidAngAtRefPt(loopwin, iRefPoint) = 0.0;
        state.dataDaylightingData->daylightControl(daylightCtrlNum).SolidAngAtRefPtWtd(loopwin, iRefPoint) = 0.0;
        zoneNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).zoneIndex;
        enclNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).enclIndex;
    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
        assert(MapNum > 0);
        zoneNum = state.dataDaylightingData->IllumMapCalc(MapNum).zoneIndex;
        enclNum = state.dataDaylightingData->IllumMapCalc(MapNum).enclIndex;
    }
    IWin = state.dataDaylightingData->enclDaylight(enclNum).DayltgExtWinSurfNums(loopwin);

    if (state.dataSurface->Surface(state.dataSurface->Surface(IWin).BaseSurf).SolarEnclIndex == enclNum) {
        ExtWinType = DataDaylighting::ExtWinType::InZoneExtWin;
    } else {
        ExtWinType = DataDaylighting::ExtWinType::AdjZoneExtWin;
    }

    IConst = state.dataSurface->SurfActiveConstruction(IWin);

    // For thermochromic windows, the daylight and glare factors are calculated for a base window cosntruction
    //  at base TC layer temperature. During each time step calculations at DayltgInteriorIllum,
    //  DayltgInteriorMapIllum, and DayltgGlare, the daylight and glare factors are adjusted by the visible
    //  transmittance ratio = VT of actual TC window based on last hour TC layer temperature / VT of the base TC window
    if (state.dataConstruction->Construct(IConst).TCFlag == 1) {
        // For thermochromic windows, use the base window construction at base temperature of the TC layer
        IConst = state.dataConstruction->Construct(IConst).TCMasterConst;
    }

    ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
    ShType = WinShadingType::NoShade; // 'NOSHADE'
    BlNum = 0;
    // ScNum = 0; //Unused Set but never used
    if (state.dataSurface->Surface(IWin).HasShadeControl) ShType = state.dataSurface->WindowShadingControl(ICtrl).ShadingType;
    BlNum = state.dataSurface->SurfWinBlindNumber(IWin);
    // ScNum = SurfaceWindow( IWin ).ScreenNumber; //Unused Set but never used

    ShelfNum = state.dataSurface->SurfDaylightingShelfInd(IWin);
    if (ShelfNum > 0) {
        InShelfSurf = state.dataDaylightingDevicesData->Shelf(state.dataSurface->SurfDaylightingShelfInd(IWin))
                          .InSurf; // Inside daylighting shelf present if > 0
    } else {
        InShelfSurf = 0;
    }

    is_Rectangle = false;
    is_Triangle = false;
    if (state.dataSurface->Surface(IWin).Sides == 3) is_Triangle = true;
    if (state.dataSurface->Surface(IWin).Sides == 4) is_Rectangle = true;

    if (is_Rectangle) {
        // Vertices of window (numbered counter-clockwise starting at upper left as viewed
        // from inside of room). Assumes original vertices are numbered counter-clockwise from
        // upper left as viewed from outside.
        W3 = state.dataSurface->Surface(IWin).Vertex(2);
        W2 = state.dataSurface->Surface(IWin).Vertex(3);
        state.dataDaylightingManager->W1 = state.dataSurface->Surface(IWin).Vertex(4);
    } else if (is_Triangle) {
        W3 = state.dataSurface->Surface(IWin).Vertex(2);
        W2 = state.dataSurface->Surface(IWin).Vertex(3);
        state.dataDaylightingManager->W1 = state.dataSurface->Surface(IWin).Vertex(1);
    }

    // Shade/blind calculation flag
    LSHCAL = 0;

    // Visible transmittance at normal incidence
    state.dataSurface->SurfWinVisTransSelected(IWin) =
        General::POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef) * state.dataSurface->SurfWinGlazedFrac(IWin);
    // For windows with switchable glazing, ratio of visible transmittance at normal
    // incidence for fully switched (dark) state to that of unswitched state
    state.dataSurface->SurfWinVisTransRatio(IWin) = 1.0;
    if (ICtrl > 0) {
        if (ShType == WinShadingType::SwitchableGlazing) {
            IConstShaded = state.dataSurface->Surface(IWin).activeShadedConstruction;
            state.dataSurface->SurfWinVisTransRatio(IWin) =
                General::SafeDivide(General::POLYF(1.0, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef),
                                    General::POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef));
        }
    }

    // Unit vectors from window vertex 2 to 1 and 2 to 3,
    // center point of window, and vector from ref pt to center of window
    W21 = state.dataDaylightingManager->W1 - W2;
    W23 = W3 - W2;
    HW = W21.magnitude();
    WW = W23.magnitude();
    if (is_Rectangle) {
        state.dataDaylightingManager->WC = W2 + (W23 + W21) / 2.0;
    } else if (is_Triangle) {
        state.dataDaylightingManager->WC = W2 + (W23 + W21) / 3.0;
    }
    state.dataSurface->SurfaceWindow(IWin).WinCenter = state.dataDaylightingManager->WC;
    state.dataDaylightingManager->REFWC = state.dataDaylightingManager->WC - RREF;
    // Unit vectors
    W21 /= HW;
    W23 /= WW;

    // Unit vector normal to window (pointing away from room)
    state.dataDaylightingManager->WNORM = state.dataSurface->Surface(IWin).lcsz;

    // Initialize number of window elements
    NDIVX = 40;
    NDIVY = 40;

    // Distance from ref point to window plane
    ALF = std::abs(dot(state.dataDaylightingManager->WNORM, state.dataDaylightingManager->REFWC));
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        // Check if ref point to close to window due to input error (0.1524 m below is 0.5 ft)
        if (ALF < 0.1524 && ExtWinType == DataDaylighting::ExtWinType::InZoneExtWin) {
            // Ref pt is close to window plane. Get vector from window
            // origin to projection of ref pt on window plane.
            state.dataDaylightingManager->W2REF = RREF + ALF * state.dataDaylightingManager->WNORM - W2;

            D1a = dot(state.dataDaylightingManager->W2REF, W23);
            D1b = dot(state.dataDaylightingManager->W2REF, W21);

            //            ! Error message if ref pt is too close to window.
            if (D1a > 0.0 && D1b > 0.0 && D1b <= HW && D1a <= WW) {
                ShowSevereError(
                    state,
                    format("CalcDaylightCoeffRefPoints: Daylighting calculation cannot be done for Daylighting:Controls={} because reference point "
                           "#{} is less than 0.15m (6\") from window plane {}",
                           state.dataDaylightingData->daylightControl(daylightCtrlNum).Name,
                           iRefPoint,
                           state.dataSurface->Surface(IWin).Name));
                ShowContinueError(state, format("Distance=[{:.5R}]. This is too close; check position of reference point.", ALF));
                ShowFatalError(state, "Program terminates due to preceding condition.");
            }
        } else if (ALF < 0.1524 && ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) {
            if (state.dataDaylightingManager->RefErrIndex(iRefPoint, IWin) == 0) { // only show error message once
                ShowWarningError(state,
                                 "CalcDaylightCoeffRefPoints: For Daylghting:Controls=\"" +
                                     state.dataDaylightingData->daylightControl(daylightCtrlNum).Name + "\" External Window=\"" +
                                     state.dataSurface->Surface(IWin).Name + "\"in Zone=\"" +
                                     state.dataHeatBal->Zone(state.dataSurface->Surface(IWin).Zone).Name +
                                     "\" reference point is less than 0.15m (6\") from window plane ");
                ShowContinueError(state,
                                  format("Distance=[{:.1R} m] to ref point=[{:.1R},{:.1R},{:.1R}], Inaccuracy in Daylighting Calcs may result.",
                                         ALF,
                                         RREF(1),
                                         RREF(2),
                                         RREF(3)));
                state.dataDaylightingManager->RefErrIndex(iRefPoint, IWin) = 1;
            }
        }
    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
        if (ALF < 0.1524 && ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) {
            if (state.dataDaylightingManager->MapErrIndex(iRefPoint, IWin) == 0) { // only show error message once
                ShowWarningError(state,
                                 "CalcDaylightCoeffMapPoints: For Zone=\"" + state.dataHeatBal->Zone(zoneNum).Name + "\" External Window=\"" +
                                     state.dataSurface->Surface(IWin).Name + "\"in Zone=\"" +
                                     state.dataHeatBal->Zone(state.dataSurface->Surface(IWin).Zone).Name +
                                     "\" map point is less than 0.15m (6\") from window plane ");
                ShowContinueError(
                    state,
                    format(
                        "Distance=[{:.1R} m] map point=[{:.1R},{:.1R},{:.1R}], Inaccuracy in Map Calcs may result.", ALF, RREF(1), RREF(2), RREF(3)));
                state.dataDaylightingManager->MapErrIndex(iRefPoint, IWin) = 1;
            }
        }
    }
    // Number of window elements in X and Y for daylighting calculation
    if (ALF > 0.1524) {
        NDIVX = 1 + int(4.0 * WW / ALF);
        NDIVY = 1 + int(4.0 * HW / ALF);
    }

    if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) {
        // Adjust number of exterior window elements to give acceptable number of rays through
        // interior windows in the zone (for accuracy of interior window daylighting calculation)
        SolidAngExtWin = General::SafeDivide(
            ((state.dataSurface->Surface(IWin).Area + state.dataSurface->SurfWinDividerArea(IWin)) / state.dataSurface->Surface(IWin).Multiplier),
            pow_2(ALF));
        SolidAngMinIntWin = state.dataDaylightingData->enclDaylight(enclNum).MinIntWinSolidAng;
        SolidAngRatio = max(1.0, SolidAngExtWin / SolidAngMinIntWin);
        NDIVX *= std::sqrt(SolidAngRatio);
        NDIVY *= std::sqrt(SolidAngRatio);
    }

    NWX = min(40, NDIVX);
    NWY = min(40, NDIVY);

    // Discretization of triangle is simpler if NWX = NWY
    if (is_Triangle) {
        NWX = max(NWX, NWY);
        NWY = NWX;
    }

    // Edge lengths of window elements
    DWX = WW / NWX;
    DWY = HW / NWY;

    // Azimuth and altitude of window normal
    state.dataSurface->SurfWinPhi(IWin) = std::asin(state.dataDaylightingManager->WNORM(3));
    if (std::abs(state.dataDaylightingManager->WNORM(1)) > 1.0e-5 || std::abs(state.dataDaylightingManager->WNORM(2)) > 1.0e-5) {
        state.dataSurface->SurfWinTheta(IWin) = std::atan2(state.dataDaylightingManager->WNORM(2), state.dataDaylightingManager->WNORM(1));
    } else {
        state.dataSurface->SurfWinTheta(IWin) = 0.0;
    }

    // Recalculation of values for TDD:DOME
    if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Diffuser) {

        // Look up the TDD:DOME object
        PipeNum = state.dataSurface->SurfWinTDDPipeNum(IWin);
        IWin2 = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Dome;

        // Calculate reference point coords relative to the diffuser coordinate system
        // W21, W23, and WNORM are the unit vectors
        state.dataDaylightingManager->REFD(1) = dot(state.dataDaylightingManager->REFWC, W21);
        state.dataDaylightingManager->REFD(2) = dot(state.dataDaylightingManager->REFWC, W23);
        state.dataDaylightingManager->REFD(3) = dot(state.dataDaylightingManager->REFWC, state.dataDaylightingManager->WNORM);

        // Calculate view vector coords relative to the diffuser coordinate system
        state.dataDaylightingManager->VIEWVD(1) = dot(VIEWVC, W21);
        state.dataDaylightingManager->VIEWVD(2) = dot(VIEWVC, W23);
        state.dataDaylightingManager->VIEWVD(3) = dot(VIEWVC, state.dataDaylightingManager->WNORM);

        state.dataDaylightingManager->U3 = state.dataSurface->Surface(IWin2).Vertex(2);
        U2 = state.dataSurface->Surface(IWin2).Vertex(3);

        if (state.dataSurface->Surface(IWin2).Sides == 4) {
            // Vertices of window (numbered counter-clockwise starting
            // at upper left as viewed from inside of room)
            // Assumes original vertices are numbered counter-clockwise from
            // upper left as viewed from outside.
            state.dataDaylightingManager->U3 = state.dataSurface->Surface(IWin2).Vertex(2);
            U2 = state.dataSurface->Surface(IWin2).Vertex(3);
            state.dataDaylightingManager->U1 = state.dataSurface->Surface(IWin2).Vertex(4);
        } else if (state.dataSurface->Surface(IWin2).Sides == 3) {
            state.dataDaylightingManager->U3 = state.dataSurface->Surface(IWin2).Vertex(2);
            U2 = state.dataSurface->Surface(IWin2).Vertex(3);
            state.dataDaylightingManager->U1 = state.dataSurface->Surface(IWin2).Vertex(1);
        }

        // Unit vectors from window vertex 2 to 1 and 2 to 3,
        // center point of window, and vector from ref pt to center of window
        U21 = state.dataDaylightingManager->U1 - U2;
        U23 = state.dataDaylightingManager->U3 - U2;
        HW = U21.magnitude();
        WW = U23.magnitude();
        if (state.dataSurface->Surface(IWin2).Sides == 4) {
            state.dataDaylightingManager->WC = U2 + (U23 + U21) / 2.0;
        } else if (state.dataSurface->Surface(IWin2).Sides == 3) {
            state.dataDaylightingManager->WC = U2 + (U23 + U21) / 3.0;
        }
        state.dataSurface->SurfaceWindow(IWin2).WinCenter = state.dataDaylightingManager->WC;
        // Unit vectors
        U21 /= HW;
        U23 /= WW;

        // Unit vector normal to dome (pointing away from TDD)
        // These are specific to the exterior.
        // NOTE:  Preserve WNORM for later in the code.
        WNORM2 = cross(U21, U23).normalize();

        // Azimuth and altitude of dome normal
        // These are specific to the exterior.
        state.dataSurface->SurfWinPhi(IWin2) = std::asin(WNORM2(3));
        if (std::abs(WNORM2(1)) > 1.0e-5 || std::abs(WNORM2(2)) > 1.0e-5) {
            state.dataSurface->SurfWinTheta(IWin2) = std::atan2(WNORM2(2), WNORM2(1));
        } else {
            state.dataSurface->SurfWinTheta(IWin2) = 0.0;
        }

        // Calculate new virtual reference point coords relative to dome coord system
        // W21, W23, and WNORM2 are now the unit vectors for the dome coord system
        state.dataDaylightingManager->REFWC = state.dataDaylightingManager->REFD(1) * U21 + state.dataDaylightingManager->REFD(2) * U23 +
                                              state.dataDaylightingManager->REFD(3) * WNORM2;
        RREF2 = state.dataDaylightingManager->WC - state.dataDaylightingManager->REFWC;

        // Calculate new virtual view vector coords relative to dome coord system
        VIEWVC2 = state.dataDaylightingManager->VIEWVD(1) * U21 + state.dataDaylightingManager->VIEWVD(2) * U23 +
                  state.dataDaylightingManager->VIEWVD(3) * WNORM2;

        // Copy several values from the diffuser so that DayltgInterReflectedIllum works correctly
        // These are specific to the interior.
        state.dataSurface->SurfWinRhoCeilingWall(IWin2) = state.dataSurface->SurfWinRhoCeilingWall(IWin);
        state.dataSurface->SurfWinRhoFloorWall(IWin2) = state.dataSurface->SurfWinRhoFloorWall(IWin);
        state.dataSurface->SurfWinFractionUpgoing(IWin2) = state.dataSurface->SurfWinFractionUpgoing(IWin);
        state.dataSurface->SurfWinGlazedFrac(IWin2) = state.dataSurface->SurfWinGlazedFrac(IWin);

    } else {
        // This is not a TDD:DIFFUSER.  Make sure nothing is messed up for a regular window.
        IWin2 = IWin;
        WNORM2 = state.dataDaylightingManager->WNORM;
        RREF2 = RREF;
        VIEWVC2 = VIEWVC;

        U2 = W2;
        U21 = W21;
        U23 = W23;
    }

    // Initialize bsdf daylighting coefficients here.  Only one time initialization
    if (state.dataSurface->SurfWinWindowModelType(IWin) == WindowBSDFModel) {
        if (!state.dataBSDFWindow->ComplexWind(IWin).DaylightingInitialized) {
            int NRefPts = 0;
            if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
                NRefPts = state.dataDaylightingData->IllumMapCalc(MapNum).TotalMapRefPoints;
            } else if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                NRefPts = state.dataDaylightingData->daylightControl(daylightCtrlNum).TotalDaylRefPoints;
            }
            InitializeCFSDaylighting(state, daylightCtrlNum, IWin, NWX, NWY, RREF, NRefPts, iRefPoint, CalledFrom, MapNum);
            // if ((WinEl == (NWX * NWY)).and.(CalledFrom == CalledForMapPoint).and.(NRefPts == iRefPoint)) then
            if ((CalledFrom == DataDaylighting::CalledFor::MapPoint) && (NRefPts == iRefPoint)) {
                state.dataBSDFWindow->ComplexWind(IWin).DaylightingInitialized = true;
            }
        }
    }

    if (!state.dataSysVars->DetailedSolarTimestepIntegration) {
        // Initialize sky and sun components of direct illuminance (arrays EDIRSK, EDIRSU, EDIRSUdisk)
        // and average window luminance (arrays AVWLSK, AVWLSU, AVWLSUdisk), at ref pt.
        state.dataDaylightingManager->EDIRSK = 0.0;
        state.dataDaylightingManager->EDIRSU = 0.0;
        state.dataDaylightingManager->EDIRSUdisk = 0.0;
        state.dataDaylightingManager->AVWLSK = 0.0;
        state.dataDaylightingManager->AVWLSU = 0.0;
        state.dataDaylightingManager->AVWLSUdisk = 0.0;
    } else {
        state.dataDaylightingManager->EDIRSK(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, 4}) = 0.0;
        state.dataDaylightingManager->EDIRSU(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}) = 0.0;
        state.dataDaylightingManager->EDIRSUdisk(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}) = 0.0;
        state.dataDaylightingManager->AVWLSK(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}, {1, 4}) = 0.0;
        state.dataDaylightingManager->AVWLSU(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}) = 0.0;
        state.dataDaylightingManager->AVWLSUdisk(state.dataGlobal->HourOfDay, {1, state.dataSurface->actualMaxSlatAngs + 1}) = 0.0;
    }
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        // Initialize solid angle subtended by window wrt ref pt
        // and solid angle weighted by glare position factor
        state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPt(iRefPoint) = 0.0;
        state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPtWtd(iRefPoint) = 0.0;
    }
    // Area of window element
    if (is_Rectangle) {
        DAXY = DWX * DWY;
    } else if (is_Triangle) {
        SinCornerAng = std::sqrt(1.0 - pow_2(dot(W21, W23)));
        DAXY = DWX * DWY * SinCornerAng;
    }
}

void FigureDayltgCoeffsAtPointsForWindowElements(
    EnergyPlusData &state,
    int const daylightCtrlNum, // Current daylighting control number (only used when called from RefPoint)
    int const iRefPoint,
    int const loopwin,
    DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
    int const WinEl,                             // Current window element number
    int const IWin,
    int const IWin2,
    int const iXelement,
    int const iYelement,
    Real64 &SkyObstructionMult,
    Vector3<Real64> const &W2,                    // Second vertex of window
    Vector3<Real64> const &W21,                   // Vector from window vertex 2 to window vertex 1
    Vector3<Real64> const &W23,                   // Vector from window vertex 2 to window vertex 3
    Vector3<Real64> const &RREF,                  // Location of a reference point in absolute coordinate system
    int const NWYlim,                             // For triangle, largest NWY for a given IX
    Vector3<Real64> const &VIEWVC2,               // Virtual view vector in absolute coordinate system
    Real64 const DWX,                             // Horizontal dimension of window element (m)
    Real64 const DWY,                             // Vertical dimension of window element (m)
    Real64 const DAXY,                            // Area of window element
    Vector3<Real64> const &U2,                    // Second vertex of window for TDD:DOME (if exists)
    Vector3<Real64> const &U23,                   // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
    Vector3<Real64> const &U21,                   // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
    Vector3<Real64> &RWIN,                        // Center of a window element for TDD:DOME (if exists) in abs coord sys
    Vector3<Real64> &RWIN2,                       // Center of a window element for TDD:DOME (if exists) in abs coord sys
    Vector3<Real64> &Ray,                         // Unit vector along ray from reference point to window element
    Real64 &PHRAY,                                // Altitude of ray from reference point to window element (radians)
    int &LSHCAL,                                  // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
    Real64 &COSB,                                 // Cosine of angle between window outward normal and ray from reference point to window element
    Real64 &ObTrans,                              // Product of solar transmittances of exterior obstructions hit by ray
    Real64 &TVISB,                                // Visible transmittance of window for COSB angle of incidence (times light well
    Real64 &DOMEGA,                               // Solid angle subtended by window element wrt reference point (steradians)
    Real64 &THRAY,                                // Azimuth of ray from reference point to window element (radians)
    bool &hitIntObs,                              // True iff interior obstruction hit
    bool &hitExtObs,                              // True iff ray from ref pt to ext win hits an exterior obstruction
    Vector3<Real64> const &WNORM2,                // Unit vector normal to window
    DataDaylighting::ExtWinType const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    int const IConst,                             // Construction counter
    Vector3<Real64> const &RREF2,                 // Location of virtual reference point in absolute coordinate system
    bool const is_Triangle,
    Real64 &TVISIntWin,     // Visible transmittance of int win at COSBIntWin for light from ext win
    Real64 &TVISIntWinDisk, // Visible transmittance of int win at COSBIntWin for sun
    int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann

    // PURPOSE OF THIS SUBROUTINE:
    // collect code to do calculations for each window element for daylighting coefficients

    // REFERENCES:
    // switch as need to serve both reference points and map points based on calledFrom

    Real64 DIS;    // Distance between reference point and center of window element (m)
    Real64 DAXY1;  // For triangle, area of window element at end of column
    Real64 POSFAC; // Position factor for a window element / ref point / view vector combination
    Real64 RR;     // Distance from ref point to intersection of view vector
    //  and plane normal to view vector and window element (m)
    Real64 ASQ; // Square of distance from above intersection to window element (m2)
    Real64 YD;  // Vertical displacement of window element wrt ref point
    Real64 XR;  // Horizontal displacement ratio
    Real64 YR;  // Vertical displacement ratio

    int IntWinHitNum;  // Surface number of interior window that is intersected
    bool hitIntWin;    // Ray from ref pt passes through interior window
    int PipeNum;       // TDD pipe object number
    int IntWin;        // Interior window surface index
    Real64 COSBIntWin; // Cos of angle between int win outward normal and ray betw ref pt and
    //  exterior window element or between ref pt and sun

    Real64 Alfa;   // Intermediate variable
    Real64 Beta;   // Intermediate variable
    Real64 HorDis; // Distance between ground hit point and proj'n of center
    //  of window element onto ground (m)

    // Local complex fenestration variables
    int CplxFenState;  // Current complex fenestration state
    int NReflSurf = 0; // Number of blocked beams for complex fenestration
    int ICplxFen;      // Complex fenestration counter
    int RayIndex;
    Real64 TransBeam; // Obstructions transmittance for incoming BSDF rays (temporary variable)

    ++LSHCAL;
    SkyObstructionMult = 1.0;

    // Center of win element in absolute coord sys
    RWIN = W2 + (double(iXelement) - 0.5) * W23 * DWX + (double(iYelement) - 0.5) * W21 * DWY;

    // Center of win element on TDD:DOME in absolute coord sys
    // If no TDD, RWIN2 = RWIN
    RWIN2 = U2 + (double(iXelement) - 0.5) * U23 * DWX + (double(iYelement) - 0.5) * U21 * DWY;

    // Distance between ref pt and window element
    DIS = distance(RWIN, RREF);

    // Unit vector along ray from ref pt to element
    Ray = (RWIN - RREF) / DIS;

    // Cosine of angle between ray and window outward normal
    COSB = dot(WNORM2, Ray);

    // If COSB > 0, direct light from window can reach ref pt. Otherwise go to loop
    // over sun position and calculate inter-reflected component of illuminance
    if (COSB > 0.0) {
        // Azimuth (-pi to pi) and altitude (-pi/2 to pi/2) of ray. Azimuth = 0 is along east.
        PHRAY = std::asin(Ray(3));
        if (std::abs(Ray(1)) > 1.0e-5 || std::abs(Ray(2)) > 1.0e-5) {
            THRAY = std::atan2(Ray(2), Ray(1));
        } else {
            THRAY = 0.0;
        }

        // Solid angle subtended by element wrt ref pt.
        DAXY1 = DAXY;
        // For triangle, at end of Y column only one half of parallelopiped's area contributes
        if (is_Triangle && iYelement == NWYlim) DAXY1 = 0.5 * DAXY;
        DOMEGA = DAXY1 * COSB / (DIS * DIS);

        // Calculate position factor (used in glare calculation) for this
        // win element / ref pt / view-vector combination
        POSFAC = 0.0;

        // Distance from ref pt to intersection of view vector and plane
        // normal to view vector containing the window element

        if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            RR = DIS * dot(Ray, VIEWVC2);
            if (RR > 0.0) {
                // Square of distance from above intersection point to win element
                ASQ = DIS * DIS - RR * RR;
                // Vertical displacement of win element wrt ref pt
                YD = RWIN2(3) - RREF2(3);
                // Horizontal and vertical displacement ratio and position factor
                XR = std::sqrt(std::abs(ASQ - YD * YD)) / RR;
                YR = std::abs(YD / RR);
                POSFAC = DayltgGlarePositionFactor(XR, YR);
            }
        }

        hitIntObs = false;
        IntWinHitNum = 0;
        hitIntWin = false;
        TVISIntWinDisk = 0.0; // Init Value
        TVISIntWin = 0.0;

        if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Diffuser) {
            // Look up the TDD:DOME object
            PipeNum = state.dataSurface->SurfWinTDDPipeNum(IWin);
            // Unshaded visible transmittance of TDD for a single ray from sky/ground element
            TVISB = DaylightingDevices::TransTDD(state, PipeNum, COSB, DataDaylightingDevices::RadType::VisibleBeam) *
                    state.dataSurface->SurfWinGlazedFrac(IWin);

        } else { // Regular window
            if (state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel) {
                // Vis trans of glass for COSB incidence angle
                TVISB = General::POLYF(COSB, state.dataConstruction->Construct(IConst).TransVisBeamCoef) *
                        state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
            } else {
                // Complex fenestration needs to use different equation for visible transmittance.  That will be calculated later
                // in the code since it depends on different incoming directions.  For now, just put zero to differentiate from
                // regular windows
                TVISB = 0.0;
            }
            if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) {
                int zoneNum = 0;
                if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                    zoneNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).zoneIndex;
                } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
                    assert(MapNum > 0);
                    zoneNum = state.dataDaylightingData->IllumMapCalc(MapNum).zoneIndex;
                }
                // Does ray pass through an interior window in zone (ZoneNum) containing the ref point?
                for (IntWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst; IntWin <= state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
                     ++IntWin) {
                    if (state.dataSurface->Surface(IntWin).ExtBoundCond >=
                        1) { // in develop this was Surface(IntWin).Class == SurfaceClass::Window && Surface(IntWin).ExtBoundCond >= 1
                        if (state.dataSurface->Surface(state.dataSurface->Surface(IntWin).ExtBoundCond).Zone ==
                            state.dataSurface->Surface(IWin).Zone) {
                            PierceSurface(state, IntWin, RREF, Ray, state.dataDaylightingManager->HitPtIntWin, hitIntWin);
                            if (hitIntWin) {
                                IntWinHitNum = IntWin;
                                COSBIntWin = dot(state.dataSurface->Surface(IntWin).OutNormVec, Ray);
                                if (COSBIntWin <= 0.0) {
                                    hitIntWin = false;
                                    IntWinHitNum = 0;
                                    continue;
                                }
                                TVISIntWin = General::POLYF(
                                    COSBIntWin, state.dataConstruction->Construct(state.dataSurface->Surface(IntWin).Construction).TransVisBeamCoef);
                                TVISB *= TVISIntWin;
                                break; // Ray passes thru interior window; exit from DO loop
                            }
                        }
                    }
                } // End of loop over surfaces in zone ZoneNum

                if (!hitIntWin) {
                    // Ray does not pass through an int win in ZoneNum. Therefore, it hits the opaque part
                    // of a surface between ref point in ZoneNum and ext win element in adjacent zone.
                    hitIntObs = true;
                }
            } // End of check if this is an ext win in an adjacent zone
        }     // End of check if TDD:Diffuser or regular exterior window or complex fenestration

        // Check for interior obstructions
        if (ExtWinType == DataDaylighting::ExtWinType::InZoneExtWin && !hitIntObs) {
            // Check for obstruction between reference point and window element
            // Returns hitIntObs = true iff obstruction is hit
            // (Example of interior obstruction is a wall in an L-shaped room that lies
            // between reference point and window.)
            DayltgHitInteriorObstruction(state, IWin, RREF, RWIN, hitIntObs);
        }

        if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin && IntWinHitNum > 0 && !hitIntObs) {
            // Check for obstruction between ref point and interior window through which ray passes
            DayltgHitInteriorObstruction(state, IntWinHitNum, RREF, state.dataDaylightingManager->HitPtIntWin, hitIntObs);
            if (!hitIntObs) {
                // Check for obstruction between intersection point on int window and ext win element
                DayltgHitBetWinObstruction(state, IntWinHitNum, IWin, state.dataDaylightingManager->HitPtIntWin, RWIN, hitIntObs);
            }
        }
        if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            // Glare calculations only done for regular reference points, not for maps
            if (!hitIntObs) {
                if (ExtWinType == DataDaylighting::ExtWinType::InZoneExtWin ||
                    (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin && hitIntWin)) {
                    // Increment solid angle subtended by portion of window above ref pt
                    state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPt(iRefPoint) += DOMEGA;
                    state.dataDaylightingData->daylightControl(daylightCtrlNum).SolidAngAtRefPt(loopwin, iRefPoint) += DOMEGA;
                    // Increment position-factor-modified solid angle
                    state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPtWtd(iRefPoint) += DOMEGA * POSFAC;
                    state.dataDaylightingData->daylightControl(daylightCtrlNum).SolidAngAtRefPtWtd(loopwin, iRefPoint) += DOMEGA * POSFAC;
                }
            }
        }
        if (hitIntObs) ObTrans = 0.0;

        hitExtObs = false;
        if (!hitIntObs) {
            // No interior obstruction was hit.
            // Check for exterior obstructions between window element and sky/ground.
            // Get product of transmittances of obstructions hit by ray.
            // ObTrans = 1.0 will be returned if no exterior obstructions are hit.

            if (state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel) {
                // the IHR (now HourOfDay) here is/was not correct, this is outside of hour loop
                // the hour is used to query schedule for transmission , not sure what to do
                // it will work for detailed and never did work correctly before.
                DayltgHitObstruction(state, state.dataGlobal->HourOfDay, IWin2, RWIN2, Ray, ObTrans);
                if (ObTrans < 1.0) hitExtObs = true;
            } else {
                // Transmittance from exterior obstruction surfaces is calculated here. This needs to be done for each timestep
                // in order to account for changes in exterior surface transmittances
                CplxFenState = state.dataSurface->SurfaceWindow(IWin).ComplexFen.CurrentState;
                if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                    NReflSurf = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CplxFenState).RefPoint(iRefPoint).NReflSurf(WinEl);
                } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
                    NReflSurf = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CplxFenState).IlluminanceMap(iRefPoint, MapNum).NReflSurf(WinEl);
                }
                for (ICplxFen = 1; ICplxFen <= NReflSurf; ++ICplxFen) {
                    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                        RayIndex =
                            state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CplxFenState).RefPoint(iRefPoint).RefSurfIndex(ICplxFen, WinEl);
                    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
                        RayIndex = state.dataBSDFWindow->ComplexWind(IWin)
                                       .DaylghtGeom(CplxFenState)
                                       .IlluminanceMap(iRefPoint, MapNum)
                                       .RefSurfIndex(ICplxFen, WinEl);
                    }
                    state.dataDaylightingManager->RayVector = state.dataBSDFWindow->ComplexWind(IWin).Geom(CplxFenState).sInc(RayIndex);
                    // It will get product of all transmittances
                    DayltgHitObstruction(state, state.dataGlobal->HourOfDay, IWin, RWIN, state.dataDaylightingManager->RayVector, TransBeam);
                    // IF (TransBeam > 0.0d0) ObTrans = TransBeam
                    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                        state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CplxFenState).RefPoint(iRefPoint).TransOutSurf(ICplxFen, WinEl) =
                            TransBeam;
                    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
                        state.dataBSDFWindow->ComplexWind(IWin)
                            .DaylghtGeom(CplxFenState)
                            .IlluminanceMap(iRefPoint, MapNum)
                            .TransOutSurf(ICplxFen, WinEl) = TransBeam;
                    }
                }
                // This will avoid obstruction multiplier calculations for non-CFS window
                ObTrans = 0.0;
            }
        }

        if (state.dataSurface->CalcSolRefl && PHRAY < 0.0 && ObTrans > 1.0e-6) {
            // Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
            // by the ray. This effect is given by the ratio SkyObstructionMult =
            // (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
            // This ratio is calculated for an isotropic sky.
            // Ground point hit by the ray:
            Alfa = std::acos(-Ray(3));
            Beta = std::atan2(Ray(2), Ray(1));
            HorDis = (RWIN2(3) - state.dataSurface->GroundLevelZ) * std::tan(Alfa);
            state.dataDaylightingManager->GroundHitPt(3) = state.dataSurface->GroundLevelZ;
            state.dataDaylightingManager->GroundHitPt(1) = RWIN2(1) + HorDis * std::cos(Beta);
            state.dataDaylightingManager->GroundHitPt(2) = RWIN2(2) + HorDis * std::sin(Beta);

            SkyObstructionMult =
                CalcObstrMultiplier(state, state.dataDaylightingManager->GroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc);
        } // End of check if solar reflection calculation is in effect

    } // End of check if COSB > 0
}

void InitializeCFSDaylighting(EnergyPlusData &state,
                              int const daylightCtrlNum,       // Current daylighting control number
                              int const IWin,                  // Complex fenestration number
                              int const NWX,                   // Number of horizontal divisions
                              int const NWY,                   // Number of vertical divisions
                              Vector3<Real64> const &RefPoint, // reference point coordinates
                              int const NRefPts,               // Number of reference points
                              int const iRefPoint,             // Reference points counter
                              DataDaylighting::CalledFor const CalledFrom,
                              int const MapNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // For incoming BSDF window direction calculates whether bin is coming from sky, ground or reflected surface.
    // Routine also calculates intersection points with ground and exterior reflection surfaces.

    int NumOfWinEl;  // Number of window elements
    int CurFenState; // Current fenestration state

    Real64 DWX;       // Window element width
    Real64 DWY;       // Window element height
    Real64 WinElArea; // Window element area

    auto &W1 = state.dataDaylightingManager->W1;
    auto &W2 = state.dataDaylightingManager->W2;
    auto &W3 = state.dataDaylightingManager->W3;
    auto &W21 = state.dataDaylightingManager->W21;
    auto &W23 = state.dataDaylightingManager->W23;
    auto &WNorm = state.dataDaylightingManager->WNorm; // unit vector from window (point towards outside)

    int NBasis;    // number of incident basis directions for current state
    int NTrnBasis; // number of outgoing basis directions for current state

    // reference point variables

    // Position factor variables
    Real64 AZVIEW; // Azimuth of view vector

    // Object Data
    DataBSDFWindow::BSDFDaylghtPosition elPos; // altitude and azimuth of intersection element
    Vector Vec;                                // temporary vector variable

    NumOfWinEl = NWX * NWY;

    DWX = state.dataSurface->Surface(IWin).Width / NWX;
    DWY = state.dataSurface->Surface(IWin).Height / NWY;

    int zoneNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).zoneIndex;
    AZVIEW = (state.dataDaylightingData->daylightControl(daylightCtrlNum).ViewAzimuthForGlare + state.dataHeatBal->Zone(zoneNum).RelNorth +
              state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) *
             DataGlobalConstants::DegToRadians;

    // Perform necessary calculations for window coordinates and vectors.  This will be used to calculate centroids for
    // each window element
    W1 = 0.0;
    W2 = 0.0;
    W3 = 0.0;

    if (state.dataSurface->Surface(IWin).Sides == 4) {
        W3 = state.dataSurface->Surface(IWin).Vertex(2);
        W2 = state.dataSurface->Surface(IWin).Vertex(3);
        W1 = state.dataSurface->Surface(IWin).Vertex(4);
    } else if (state.dataSurface->Surface(IWin).Sides == 3) {
        W3 = state.dataSurface->Surface(IWin).Vertex(2);
        W2 = state.dataSurface->Surface(IWin).Vertex(3);
        W1 = state.dataSurface->Surface(IWin).Vertex(1);
    }

    W21 = W1 - W2;
    W23 = W3 - W2;

    W21 /= state.dataSurface->Surface(IWin).Height;
    W23 /= state.dataSurface->Surface(IWin).Width;

    WNorm = state.dataSurface->Surface(IWin).lcsz;

    WinElArea = DWX * DWY;
    if (state.dataSurface->Surface(IWin).Sides == 3) {
        WinElArea *= std::sqrt(1.0 - pow_2(dot(W21, W23)));
    }

    if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {

        if (!allocated(state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap)) {
            state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap.allocate(NRefPts, (int)state.dataDaylightingData->IllumMap.size());
        }

        AllocateForCFSRefPointsGeometry(state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap(iRefPoint, MapNum), NumOfWinEl);

    } else if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        if (!allocated(state.dataBSDFWindow->ComplexWind(IWin).RefPoint)) {
            state.dataBSDFWindow->ComplexWind(IWin).RefPoint.allocate(NRefPts);
        }

        AllocateForCFSRefPointsGeometry(state.dataBSDFWindow->ComplexWind(IWin).RefPoint(iRefPoint), NumOfWinEl);
    }

    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //! Allocation for each complex fenestration state reference points
    //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (!allocated(state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom)) {
        state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom.allocate(state.dataBSDFWindow->ComplexWind(IWin).NumStates);
    }

    // Calculation needs to be performed for each state
    for (CurFenState = 1; CurFenState <= state.dataBSDFWindow->ComplexWind(IWin).NumStates; ++CurFenState) {
        NBasis = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurFenState).Inc.NBasis;
        NTrnBasis = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurFenState).Trn.NBasis;

        if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
            if ((int)state.dataDaylightingData->IllumMap.size() > 0) {
                // illuminance map for each state
                if (!allocated(state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).IlluminanceMap)) {
                    state.dataBSDFWindow->ComplexWind(IWin)
                        .DaylghtGeom(CurFenState)
                        .IlluminanceMap.allocate(NRefPts, (int)state.dataDaylightingData->IllumMap.size());
                }

                AllocateForCFSRefPointsState(state,
                                             state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).IlluminanceMap(iRefPoint, MapNum),
                                             NumOfWinEl,
                                             NBasis,
                                             NTrnBasis);

                InitializeCFSStateData(state,
                                       state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).IlluminanceMap(iRefPoint, MapNum),
                                       state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap(iRefPoint, MapNum),
                                       daylightCtrlNum,
                                       IWin,
                                       RefPoint,
                                       CurFenState,
                                       NBasis,
                                       NTrnBasis,
                                       AZVIEW,
                                       NWX,
                                       NWY,
                                       W2,
                                       W21,
                                       W23,
                                       DWX,
                                       DWY,
                                       WNorm,
                                       WinElArea);
            }

        } else if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            if (!allocated(state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).RefPoint)) {
                state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).RefPoint.allocate(NRefPts);
            }

            AllocateForCFSRefPointsState(
                state, state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).RefPoint(iRefPoint), NumOfWinEl, NBasis, NTrnBasis);

            InitializeCFSStateData(state,
                                   state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurFenState).RefPoint(iRefPoint),
                                   state.dataBSDFWindow->ComplexWind(IWin).RefPoint(iRefPoint),
                                   daylightCtrlNum,
                                   IWin,
                                   RefPoint,
                                   CurFenState,
                                   NBasis,
                                   NTrnBasis,
                                   AZVIEW,
                                   NWX,
                                   NWY,
                                   W2,
                                   W21,
                                   W23,
                                   DWX,
                                   DWY,
                                   WNorm,
                                   WinElArea);
        }
    }
}

void InitializeCFSStateData(EnergyPlusData &state,
                            DataBSDFWindow::BSDFRefPoints &StateRefPoint,
                            DataBSDFWindow::BSDFRefPointsGeomDescr &DaylghtGeomDescr,
                            [[maybe_unused]] int const daylightCtrlNum, // Current daylighting control number
                            int const iWin,
                            Vector3<Real64> const &RefPoint, // reference point
                            int const CurFenState,
                            int const NBasis,
                            int const NTrnBasis,
                            Real64 const AZVIEW,
                            int const NWX,
                            int const NWY,
                            Vector3<Real64> const &W2,
                            Vector3<Real64> const &W21,
                            Vector3<Real64> const &W23,
                            Real64 const DWX,
                            Real64 const DWY,
                            Vector3<Real64> const &WNorm, // unit vector from window (point towards outside)
                            Real64 const WinElArea)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initialize daylight state data for current

    // SUBROUTINE LOCAL VARIABLES
    int curWinEl;
    int IRay;
    bool hit;
    int TotHits;
    int JSurf;
    Real64 DotProd; // Temporary variable for manipulating dot product .dot.
    int NSky;
    int NGnd;
    int NReflSurf;
    int MaxTotHits;
    int IX;
    int IY;
    Real64 LeastHitDsq; // dist^2 from window element center to hit point
    Real64 HitDsq;
    Real64 TransRSurf;
    int I;
    int J;

    auto &RWin = state.dataDaylightingManager->RWin;
    auto &V = state.dataDaylightingManager->V;
    auto &GroundHitPt = state.dataDaylightingManager->GroundHitPt;

    // temporary arrays for surfaces
    // Each complex fenestration state can have different number of basis elements
    // This is the reason for making these temporary arrays local
    Array1D_int TmpSkyInd(NBasis, 0);                                         // Temporary sky index list
    Array1D_int TmpGndInd(NBasis, 0);                                         // Temporary gnd index list
    Array1D<Real64> TmpGndMultiplier(NBasis, 0.0);                            // Temporary ground obstruction multiplier
    Array1D_int TmpRfSfInd(NBasis, 0);                                        // Temporary RefSurfIndex
    Array1D_int TmpRfRyNH(NBasis, 0);                                         // Temporary RefRayNHits
    Array2D_int TmpHSurfNo(state.dataSurface->TotSurfaces, NBasis, 0);        // Temporary HitSurfNo
    Array2D<Real64> TmpHSurfDSq(state.dataSurface->TotSurfaces, NBasis, 0.0); // Temporary HitSurfDSq

    // Object Data
    Vector Centroid;                                                                         // current window element centroid
    Vector HitPt;                                                                            // surface hit point
    Array1D<Vector> TmpGndPt(NBasis, Vector(0.0, 0.0, 0.0));                                 // Temporary ground intersection list
    Array2D<Vector> TmpHitPt(state.dataSurface->TotSurfaces, NBasis, Vector(0.0, 0.0, 0.0)); // Temporary HitPt

    CFSRefPointPosFactor(state, RefPoint, StateRefPoint, iWin, CurFenState, NTrnBasis, AZVIEW);

    curWinEl = 0;
    // loop through window elements. This will calculate sky, ground and reflection bins for each window element
    for (IX = 1; IX <= NWX; ++IX) {
        for (IY = 1; IY <= NWY; ++IY) {

            ++curWinEl;

            // centroid coordinates for current window element
            Centroid = W2 + (double(IX) - 0.5) * W23 * DWX + (double(IY) - 0.5) * W21 * DWY;
            RWin = Centroid;

            CFSRefPointSolidAngle(state, RefPoint, RWin, WNorm, StateRefPoint, DaylghtGeomDescr, iWin, CurFenState, NTrnBasis, curWinEl, WinElArea);

            NSky = 0;
            NGnd = 0;
            NReflSurf = 0;
            MaxTotHits = 0;
            // Calculation of potential surface obstruction for each incoming direction
            for (IRay = 1; IRay <= NBasis; ++IRay) {

                hit = false;
                TotHits = 0;
                for (JSurf = 1; JSurf <= state.dataSurface->TotSurfaces; ++JSurf) {
                    // the following test will cycle on anything except exterior surfaces and shading surfaces
                    if (state.dataSurface->Surface(JSurf).HeatTransSurf && state.dataSurface->Surface(JSurf).ExtBoundCond != ExternalEnvironment)
                        continue;
                    //  skip the base surface containing the window and any other subsurfaces of that surface
                    if (JSurf == state.dataSurface->Surface(iWin).BaseSurf ||
                        state.dataSurface->Surface(JSurf).BaseSurf == state.dataSurface->Surface(iWin).BaseSurf)
                        continue;
                    //  skip surfaces that face away from the window
                    DotProd = dot(state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay),
                                  state.dataSurface->Surface(JSurf).NewellSurfaceNormalVector);
                    if (DotProd >= 0) continue;
                    PierceSurface(state, JSurf, Centroid, state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay), HitPt, hit);
                    if (!hit) continue; // Miss: Try next surface
                    if (TotHits == 0) {
                        // First hit for this ray
                        TotHits = 1;
                        ++NReflSurf;
                        TmpRfSfInd(NReflSurf) = IRay;
                        TmpRfRyNH(NReflSurf) = 1;
                        TmpHSurfNo(1, NReflSurf) = JSurf;
                        TmpHitPt(1, NReflSurf) = HitPt;
                        V = HitPt - Centroid;                // vector array from window ctr to hit pt
                        LeastHitDsq = V.magnitude_squared(); // dist^2 window ctr to hit pt
                        TmpHSurfDSq(1, NReflSurf) = LeastHitDsq;
                        if (!state.dataSurface->Surface(JSurf).HeatTransSurf && state.dataSurface->Surface(JSurf).SchedShadowSurfIndex != 0) {
                            TransRSurf = 1.0; // If a shadowing surface may have a scheduled transmittance, treat it here as completely transparent
                        } else {
                            TransRSurf = 0.0;
                        }
                    } else {
                        V = HitPt - Centroid;
                        HitDsq = V.magnitude_squared();
                        if (HitDsq >= LeastHitDsq) {
                            if (TransRSurf > 0.0) { // forget the new hit if the closer hit is opaque
                                J = TotHits + 1;
                                if (TotHits > 1) {
                                    for (I = 2; I <= TotHits; ++I) {
                                        if (HitDsq < TmpHSurfDSq(I, NReflSurf)) {
                                            J = I;
                                            break;
                                        }
                                    }
                                    if (!state.dataSurface->Surface(JSurf).HeatTransSurf &&
                                        state.dataSurface->Surface(JSurf).SchedShadowSurfIndex == 0) {
                                        //  The new hit is opaque, so we can drop all the hits further away
                                        TmpHSurfNo(J, NReflSurf) = JSurf;
                                        TmpHitPt(J, NReflSurf) = HitPt;
                                        TmpHSurfDSq(J, NReflSurf) = HitDsq;
                                        TotHits = J;
                                    } else {
                                        //  The new hit is scheduled (presumed transparent), so keep the more distant hits
                                        //     Note that all the hists in the list will be transparent except the last,
                                        //       which may be either transparent or opaque
                                        if (TotHits >= J) {
                                            for (I = TotHits; I >= J; --I) {
                                                TmpHSurfNo(I + 1, NReflSurf) = TmpHSurfNo(I, NReflSurf);
                                                TmpHitPt(I + 1, NReflSurf) = TmpHitPt(I, NReflSurf);
                                                TmpHSurfDSq(I + 1, NReflSurf) = TmpHSurfDSq(I, NReflSurf);
                                            }
                                            TmpHSurfNo(J, NReflSurf) = JSurf;
                                            TmpHitPt(J, NReflSurf) = HitPt;
                                            TmpHSurfDSq(J, NReflSurf) = HitDsq;
                                            ++TotHits;
                                        }
                                    } // if (.NOT.Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%SchedShadowSurfIndex == 0)  then
                                }     // if (TotHits > 1) then
                            }         // if (TransRSurf  > 0.0d0) then
                        } else {      // if (HitDsq >= LeastHitDsq) then
                            //  A new closest hit.  If it is opaque, drop the current hit list,
                            //    otherwise add it at the front
                            LeastHitDsq = HitDsq;
                            if (!state.dataSurface->Surface(JSurf).HeatTransSurf && state.dataSurface->Surface(JSurf).SchedShadowSurfIndex != 0) {
                                TransRSurf = 1.0; // New closest hit is transparent, keep the existing hit list
                                for (I = TotHits; I >= 1; --I) {
                                    TmpHSurfNo(I + 1, NReflSurf) = TmpHSurfNo(I, NReflSurf);
                                    TmpHitPt(I + 1, NReflSurf) = TmpHitPt(I, NReflSurf);
                                    TmpHSurfDSq(I + 1, NReflSurf) = TmpHSurfDSq(I, NReflSurf);
                                    ++TotHits;
                                }
                            } else {
                                TransRSurf = 0.0; // New closest hit is opaque, drop the existing hit list
                                TotHits = 1;
                            }
                            TmpHSurfNo(1, NReflSurf) = JSurf; // In either case the new hit is put in position 1
                            TmpHitPt(1, NReflSurf) = HitPt;
                            TmpHSurfDSq(1, NReflSurf) = LeastHitDsq;
                        }
                    }
                } // do JSurf = 1, TotSurfaces
                if (TotHits <= 0) {
                    // This ray reached the sky or ground unobstructed
                    if (state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay).z < 0.0) {
                        // A ground ray
                        ++NGnd;
                        TmpGndInd(NGnd) = IRay;
                        TmpGndPt(NGnd).x = Centroid.x - (state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay).x /
                                                         state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay).z) *
                                                            Centroid.z;
                        TmpGndPt(NGnd).y = Centroid.y - (state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay).y /
                                                         state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sInc(IRay).z) *
                                                            Centroid.z;
                        TmpGndPt(NGnd).z = 0.0;

                        // for solar reflectance calculations, need to precalculate obstruction multipliers
                        if (state.dataSurface->CalcSolRefl) {
                            GroundHitPt = TmpGndPt(NGnd);
                            TmpGndMultiplier(NGnd) = CalcObstrMultiplier(state, GroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc);
                        }
                    } else {
                        // A sky ray
                        ++NSky;
                        TmpSkyInd(NSky) = IRay;
                    }
                } else {
                    // Save the number of hits for this ray
                    TmpRfRyNH(NReflSurf) = TotHits;
                }
                MaxTotHits = max(MaxTotHits, TotHits);
            } // do IRay = 1, ComplexWind(IWin)%Geom(CurFenState)%Inc%NBasis

            // Fill up state data for current window element data
            StateRefPoint.NSky(curWinEl) = NSky;
            StateRefPoint.SkyIndex({1, NSky}, curWinEl) = TmpSkyInd({1, NSky});

            StateRefPoint.NGnd(curWinEl) = NGnd;
            StateRefPoint.GndIndex({1, NGnd}, curWinEl) = TmpGndInd({1, NGnd});
            StateRefPoint.GndPt({1, NGnd}, curWinEl) = TmpGndPt({1, NGnd});
            StateRefPoint.GndObstrMultiplier({1, NGnd}, curWinEl) = TmpGndMultiplier({1, NGnd});

            StateRefPoint.NReflSurf(curWinEl) = NReflSurf;
            StateRefPoint.RefSurfIndex({1, NReflSurf}, curWinEl) = TmpRfSfInd({1, NReflSurf});
            StateRefPoint.RefRayNHits({1, NReflSurf}, curWinEl) = TmpRfRyNH({1, NReflSurf});
            StateRefPoint.HitSurfNo({1, MaxTotHits}, {1, NReflSurf}, curWinEl) = TmpHSurfNo({1, MaxTotHits}, {1, NReflSurf});
            StateRefPoint.HitSurfDSq({1, MaxTotHits}, {1, NReflSurf}, curWinEl) = TmpHSurfDSq({1, MaxTotHits}, {1, NReflSurf});
            StateRefPoint.HitPt({1, MaxTotHits}, {1, NReflSurf}, curWinEl) = TmpHitPt({1, MaxTotHits}, {1, NReflSurf});
        } // do IY = 1, NWY
    }     // do IX = 1, NWX
}

void AllocateForCFSRefPointsState(
    EnergyPlusData &state, DataBSDFWindow::BSDFRefPoints &StateRefPoint, int const NumOfWinEl, int const NBasis, int const NTrnBasis)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Memory allocation for complex fenestration systems reference points geometry

    if (!allocated(StateRefPoint.NSky)) {
        StateRefPoint.NSky.allocate(NumOfWinEl);
        StateRefPoint.NSky = 0;
    }

    if (!allocated(StateRefPoint.SkyIndex)) {
        StateRefPoint.SkyIndex.allocate(NBasis, NumOfWinEl);
        StateRefPoint.SkyIndex = 0;
    }

    if (!allocated(StateRefPoint.NGnd)) {
        StateRefPoint.NGnd.allocate(NumOfWinEl);
        StateRefPoint.NGnd = 0;
    }

    if (!allocated(StateRefPoint.GndIndex)) {
        StateRefPoint.GndIndex.allocate(NBasis, NumOfWinEl);
        StateRefPoint.GndIndex = 0;
    }

    if (!allocated(StateRefPoint.GndPt)) {
        StateRefPoint.GndPt.allocate(NBasis, NumOfWinEl);
        StateRefPoint.GndPt = Vector(0.0, 0.0, 0.0);
    }

    if (!allocated(StateRefPoint.GndObstrMultiplier)) {
        StateRefPoint.GndObstrMultiplier.allocate(NBasis, NumOfWinEl);
        StateRefPoint.GndObstrMultiplier = 0.0;
    }

    if (!allocated(StateRefPoint.NReflSurf)) {
        StateRefPoint.NReflSurf.allocate(NumOfWinEl);
        StateRefPoint.NReflSurf = 0;
    }

    if (!allocated(StateRefPoint.RefSurfIndex)) {
        StateRefPoint.RefSurfIndex.allocate(NBasis, NumOfWinEl);
        StateRefPoint.RefSurfIndex = 0;
    }

    if (!allocated(StateRefPoint.TransOutSurf)) {
        StateRefPoint.TransOutSurf.allocate(NBasis, NumOfWinEl);
        StateRefPoint.TransOutSurf = 1.0;
    }

    if (!allocated(StateRefPoint.RefRayNHits)) {
        StateRefPoint.RefRayNHits.allocate(NBasis, NumOfWinEl);
        StateRefPoint.RefRayNHits = 0;
    }

    if (!allocated(StateRefPoint.HitSurfNo)) {
        StateRefPoint.HitSurfNo.allocate(state.dataSurface->TotSurfaces, NBasis, NumOfWinEl);
        StateRefPoint.HitSurfNo = 0;
    }

    if (!allocated(StateRefPoint.HitSurfDSq)) {
        StateRefPoint.HitSurfDSq.allocate(state.dataSurface->TotSurfaces, NBasis, NumOfWinEl);
        StateRefPoint.HitSurfDSq = 0.0;
    }

    if (!allocated(StateRefPoint.HitPt)) {
        StateRefPoint.HitPt.allocate(state.dataSurface->TotSurfaces, NBasis, NumOfWinEl);
        StateRefPoint.HitPt = Vector(0.0, 0.0, 0.0);
    }

    if (!allocated(StateRefPoint.RefPointIndex)) {
        StateRefPoint.RefPointIndex.allocate(NumOfWinEl);
        StateRefPoint.RefPointIndex = 0;
    }

    if (!allocated(StateRefPoint.RefPointIntersection)) {
        StateRefPoint.RefPointIntersection.allocate(NTrnBasis);
        StateRefPoint.RefPointIntersection = false;
    }

    if (!allocated(StateRefPoint.RefPtIntPosFac)) {
        StateRefPoint.RefPtIntPosFac.allocate(NTrnBasis);
        StateRefPoint.RefPtIntPosFac = 0.0;
    }
}

void AllocateForCFSRefPointsGeometry(DataBSDFWindow::BSDFRefPointsGeomDescr &RefPointsGeomDescr, int const NumOfWinEl)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Memory allocation for complex fenestration systems reference points geometry

    // METHODOLOGY EMPLOYED:
    // <description>

    // REFERENCES:
    // na

    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // integer, intent(in) :: NRefPts

    // SUBROUTINE LOCAL VARIABLES

    if (!allocated(RefPointsGeomDescr.SolidAngle)) {
        RefPointsGeomDescr.SolidAngle.allocate(NumOfWinEl);
        RefPointsGeomDescr.SolidAngle = 0.0;
    }

    if (!allocated(RefPointsGeomDescr.SolidAngleVec)) {
        RefPointsGeomDescr.SolidAngleVec.allocate(NumOfWinEl);
        RefPointsGeomDescr.SolidAngleVec = Vector(0.0, 0.0, 0.0);
    }
}

void CFSRefPointSolidAngle(EnergyPlusData &state,
                           Vector3<Real64> const &RefPoint,
                           Vector3<Real64> const &RWin,
                           Vector3<Real64> const &WNorm,
                           DataBSDFWindow::BSDFRefPoints &RefPointMap,
                           DataBSDFWindow::BSDFRefPointsGeomDescr &RefPointGeomMap,
                           int const iWin,
                           int const CurFenState,
                           int const NTrnBasis,
                           int const curWinEl,
                           Real64 const WinElArea)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate position factor for given reference point.

    // SUBROUTINE LOCAL VARIABLES
    auto &Ray = state.dataDaylightingManager->Ray;
    auto &RayNorm = state.dataDaylightingManager->RayNorm;
    auto &V = state.dataDaylightingManager->V;
    Real64 BestMatch;
    int iTrnRay;
    Real64 temp;
    Real64 Dist;
    Real64 CosB;

    // calculate vector from center of window element to the current reference point
    Ray = RefPoint - RWin;

    // figure out outgoing beam direction from current reference point
    BestMatch = 0.0;
    for (iTrnRay = 1; iTrnRay <= NTrnBasis; ++iTrnRay) {
        V = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sTrn(iTrnRay);
        temp = dot(Ray, V);
        if (temp > BestMatch) {
            BestMatch = temp;
            RefPointMap.RefPointIndex(curWinEl) = iTrnRay;
        }
    }

    // calculate solid view angle
    Dist = Ray.magnitude();
    RayNorm = Ray / (-Dist);
    RefPointGeomMap.SolidAngleVec(curWinEl) = RayNorm;
    CosB = dot(WNorm, RayNorm);
    RefPointGeomMap.SolidAngle(curWinEl) = WinElArea * CosB / (Dist * Dist);
}

void CFSRefPointPosFactor(EnergyPlusData &state,
                          Vector3<Real64> const &RefPoint,
                          DataBSDFWindow::BSDFRefPoints &RefPointMap,
                          int const iWin,
                          int const CurFenState,
                          int const NTrnBasis,
                          Real64 const AZVIEW)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate position factor for given reference point.

    // Using/Aliasing
    using WindowComplexManager::DaylghtAltAndAzimuth;

    // SUBROUTINE LOCAL VARIABLES
    int iTrnRay;
    Real64 XR;
    Real64 YR;
    auto &V = state.dataDaylightingManager->V;
    auto &InterPoint = state.dataDaylightingManager->InterPoint;
    bool hit;

    // Object Data
    DataBSDFWindow::BSDFDaylghtPosition elPos; // altitude and azimuth of intersection element

    auto const &sTrn(state.dataBSDFWindow->ComplexWind(iWin).Geom(CurFenState).sTrn);
    for (iTrnRay = 1; iTrnRay <= NTrnBasis; ++iTrnRay) {
        V = sTrn(iTrnRay);
        V.negate();
        PierceSurface(state, iWin, RefPoint, V, InterPoint, hit);
        if (hit) {
            RefPointMap.RefPointIntersection(iTrnRay) = true;

            elPos = DaylghtAltAndAzimuth(V);

            XR = std::tan(std::abs(DataGlobalConstants::PiOvr2 - AZVIEW - elPos.Azimuth) + 0.001);
            YR = std::tan(elPos.Altitude + 0.001);
            RefPointMap.RefPtIntPosFac(iTrnRay) = DayltgGlarePositionFactor(XR, YR);
        }
    }
}

Real64 CalcObstrMultiplier(EnergyPlusData &state,
                           Vector3<Real64> const &GroundHitPt, // Coordinates of point that ray hits ground (m)
                           int const AltSteps,                 // Number of steps in altitude angle for solar reflection calc
                           int const AzimSteps                 // Number of steps in azimuth angle of solar reflection calc
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   April 2013, refactor from legacy code by Fred Winklemann
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // collect code to do obstruction multiplier from ground point

    // METHODOLOGY EMPLOYED:
    // Send rays upward from hit point and see which ones are unobstructed and so go to sky.
    // Divide hemisphere centered at ground hit point into elements of altitude Phi and
    // azimuth Theta and create upward-going ground ray unit vector at each Phi,Theta pair.
    // Phi = 0 at the horizon; Phi = Pi/2 at the zenith.

    // USE STATEMENTS:
    using DataSurfaces::AzimAngStepsForSolReflCalc;

    // Return value
    Real64 ObstrMultiplier;

    // Locals
    Real64 DPhi;        // Phi increment (radians)
    Real64 DTheta;      // Theta increment (radians)
    Real64 SkyGndUnObs; // Unobstructed sky irradiance at a ground point
    Real64 SkyGndObs;   // Obstructed sky irradiance at a ground point

    Real64 Phi;   // Altitude  angle of ray from a ground point (radians)
    Real64 SPhi;  // Sin of Phi
    Real64 CPhi;  // cos of Phi
    Real64 Theta; // Azimuth angle of ray from a ground point (radians)

    Real64 CosIncAngURay;                                              // Cosine of incidence angle of URay on ground plane
    Real64 dOmegaGnd;                                                  // Solid angle element of ray from ground point (steradians)
    Real64 IncAngSolidAngFac;                                          // CosIncAngURay*dOmegaGnd/Pi
    bool hitObs;                                                       // True iff obstruction is hit
    auto &URay = state.dataDaylightingManager->URay;                   // Unit vector in (Phi,Theta) direction
    auto &ObsHitPt = state.dataDaylightingManager->ObsHitPt;           // Unit vector in (Phi,Theta) direction
    auto &AltSteps_last = state.dataDaylightingManager->AltSteps_last; // Unit vector in (Phi,Theta) direction
    auto &cos_Phi = state.dataDaylightingManager->cos_Phi;             // Unit vector in (Phi,Theta) direction
    auto &sin_Phi = state.dataDaylightingManager->sin_Phi;             // Unit vector in (Phi,Theta) direction
    auto &cos_Theta = state.dataDaylightingManager->cos_Theta;         // Unit vector in (Phi,Theta) direction
    auto &sin_Theta = state.dataDaylightingManager->sin_Theta;         // Unit vector in (Phi,Theta) direction
    auto &AzimSteps_last = state.dataDaylightingManager->AzimSteps_last;

    assert(AzimSteps <= AzimAngStepsForSolReflCalc);

    DPhi = DataGlobalConstants::PiOvr2 / (AltSteps / 2.0);
    DTheta = DataGlobalConstants::Pi / AzimSteps;
    SkyGndObs = 0.0;
    SkyGndUnObs = 0.0;

    // Tuned Precompute Phi trig table
    if (AltSteps != AltSteps_last) {
        for (int IPhi = 1, IPhi_end = (AltSteps / 2); IPhi <= IPhi_end; ++IPhi) {
            Phi = (IPhi - 0.5) * DPhi;
            cos_Phi(IPhi) = std::cos(Phi);
            sin_Phi(IPhi) = std::sin(Phi);
        }
        AltSteps_last = AltSteps;
    }
    // Tuned Precompute Theta trig table
    if (AzimSteps != AzimSteps_last) {
        for (int ITheta = 1; ITheta <= 2 * AzimSteps; ++ITheta) {
            Theta = (ITheta - 0.5) * DTheta;
            cos_Theta(ITheta) = std::cos(Theta);
            sin_Theta(ITheta) = std::sin(Theta);
        }
        AzimSteps_last = AzimSteps;
    }

    // Altitude loop
    for (int IPhi = 1, IPhi_end = (AltSteps / 2); IPhi <= IPhi_end; ++IPhi) {
        SPhi = sin_Phi(IPhi);
        CPhi = cos_Phi(IPhi);

        // Third component of ground ray unit vector in (Theta,Phi) direction
        URay(3) = SPhi;
        dOmegaGnd = CPhi * DTheta * DPhi;
        // Cosine of angle of incidence of ground ray on ground plane
        CosIncAngURay = SPhi;
        IncAngSolidAngFac = CosIncAngURay * dOmegaGnd / DataGlobalConstants::Pi;
        // Azimuth loop
        for (int ITheta = 1; ITheta <= 2 * AzimSteps; ++ITheta) {
            URay(1) = CPhi * cos_Theta(ITheta);
            URay(2) = CPhi * sin_Theta(ITheta);
            SkyGndUnObs += IncAngSolidAngFac;
            // Does this ground ray hit an obstruction?
            hitObs = false;
            if (state.dataSurface->TotSurfaces < octreeCrossover) { // Linear search through surfaces

                for (int ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
                    if (state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) {
                        PierceSurface(state, ObsSurfNum, GroundHitPt, URay, ObsHitPt, hitObs); // Check if ray pierces surface
                        if (hitObs) break;
                    }
                }

            } else { // Surface octree search

                // Lambda function for the octree to test for surface hit
                auto surfaceHit = [&GroundHitPt, &hitObs, &URay, &ObsHitPt](SurfaceData const &surface) -> bool {
                    if (surface.IsShadowPossibleObstruction) {
                        PierceSurface(surface, GroundHitPt, URay, ObsHitPt, hitObs); // Check if ray pierces surface
                        return hitObs;                                               // Ray pierces surface
                    } else {
                        return false;
                    }
                };

                // Check octree surface candidates until a hit is found, if any
                Vector3<Real64> const URay_inv(SurfaceOctreeCube::safe_inverse(URay));
                state.dataHeatBalMgr->surfaceOctree.hasSurfaceRayIntersectsCube(GroundHitPt, URay, URay_inv, surfaceHit);
            }

            if (hitObs) continue; // Obstruction hit
            // Sky is hit
            SkyGndObs += IncAngSolidAngFac;
        } // End of azimuth loop
    }     // End of altitude loop

    // in case ground point is surrounded by obstructions (SkyGndUnObs == 0), then multiplier will be equal to zero
    // This should not happen anyway because in that case ray would not be able to reach ground point
    ObstrMultiplier = 0.0;

    if (SkyGndUnObs != 0.0) {
        ObstrMultiplier = SkyGndObs / SkyGndUnObs;
    }

    return ObstrMultiplier;
}

void FigureDayltgCoeffsAtPointsForSunPosition(
    EnergyPlusData &state,
    int const daylightCtrlNum, // Daylighting control index
    int const iRefPoint,
    int const iXelement,
    int const NWX, // Number of window elements in x direction for dayltg calc
    int const iYelement,
    int const NWY,   // Number of window elements in y direction for dayltg calc
    int const WinEl, // Current window element counter
    int const IWin,
    int const IWin2,
    int const iHour,
    int &ISunPos,
    Real64 const SkyObstructionMult,
    Vector3<Real64> const &RWIN2, // Center of a window element for TDD:DOME (if exists) in abs coord sys
    Vector3<Real64> const &Ray,   // Unit vector along ray from reference point to window element
    Real64 const PHRAY,           // Altitude of ray from reference point to window element (radians)
    int const LSHCAL,             // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
    int const InShelfSurf,        // Inside daylighting shelf surface number
    Real64 const COSB,            // Cosine of angle between window outward normal and ray from reference point to window element
    Real64 const ObTrans,         // Product of solar transmittances of exterior obstructions hit by ray from reference point through a window element
    Real64 const TVISB,           // Visible transmittance of window for COSB angle of incidence (times light well efficiency, if appropriate)
    Real64 const DOMEGA,          // Solid angle subtended by window element wrt reference point (steradians)
    int const ICtrl,              // Window control counter
    WinShadingType const ShType,  // Window shading type
    int const BlNum,              // Window blind number
    Real64 const THRAY,           // Azimuth of ray from reference point to window element (radians)
    Vector3<Real64> const &WNORM2,                // Unit vector normal to window
    DataDaylighting::ExtWinType const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    int const IConst,                             // Construction counter
    Real64 const AZVIEW,                          // Azimuth of view vector in absolute coord system for glare calculation (radians)
    Vector3<Real64> const &RREF2,                 // Location of virtual reference point in absolute coordinate system
    bool const hitIntObs,                         // True iff interior obstruction hit
    bool const hitExtObs,                         // True iff ray from ref pt to ext win hits an exterior obstruction
    DataDaylighting::CalledFor const CalledFrom,  // indicate  which type of routine called this routine
    Real64 &TVISIntWin,                           // Visible transmittance of int win at COSBIntWin for light from ext win
    Real64 &TVISIntWinDisk,                       // Visible transmittance of int win at COSBIntWin for sun
    int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // collect code for calculations sun position aspects for daylighting coefficients

    // METHODOLOGY EMPLOYED:
    // switch as need to serve both reference points and map points based on calledFrom
    using General::POLYF;

    if (state.dataSurface->SurfSunCosHourly(iHour)(3) < DataEnvironment::SunIsUpValue) return;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    static Vector3<Real64> const RREF(0.0); // Location of a reference point in absolute coordinate system //Autodesk Was used uninitialized:
                                            // Never set here // Made static for performance and const for now until issue addressed
    auto &XEDIRSK = state.dataDaylightingManager->XEDIRSK;
    auto &XAVWLSK = state.dataDaylightingManager->XAVWLSK;
    auto &RAYCOS = state.dataDaylightingManager->RAYCOS;
    auto &TransBmBmMult = state.dataDaylightingManager->TransBmBmMult;
    auto &TransBmBmMultRefl = state.dataDaylightingManager->TransBmBmMultRefl;
    auto &HP = state.dataDaylightingManager->HP;
    int JB;         // Slat angle counter
    Real64 ProfAng; // Solar profile angle on a window (radians)
    Real64 POSFAC;  // Position factor for a window element / ref point / view vector combination
    Real64 XR;      // Horizontal displacement ratio
    Real64 YR;      // Vertical displacement ratio
    bool hit;       // True iff ray from ref point thru window element hits an obstruction

    Real64 ObTransDisk;     // Product of solar transmittances of exterior obstructions hit by ray from reference point to sun
    Real64 LumAtHitPtFrSun; // Luminance at hit point of obstruction by reflection of direct light from sun (cd/m2)
    int ISky;               // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast

    Real64 ELUM;  // Sky or ground luminance (cd/m2)
    Real64 DEDIR; // Illuminance contribution at reference point from window element (lux)
    Real64 COSI;  // Cosine of angle between direct sun and window outward normal
    bool hitWin;  // True iff ray passes thru window
    Real64 TVISS; // Direct solar visible transmittance of window at given angle of incidence
    //  (times light well efficiency, if appropriate)
    Real64 XAVWL; // XAVWL*TVISS is contribution of window luminance from solar disk (cd/m2)

    Real64 SlatAng;                                                        // Blind slat angle (rad)
    int NearestHitSurfNum;                                                 // Surface number of nearest obstruction
    int NearestHitSurfNumX;                                                // Surface number to use when obstruction is a shadowing surface
    auto &NearestHitPt = state.dataDaylightingManager->NearestHitPt;       // Hit point of ray on nearest obstruction
    auto &GroundHitPt = state.dataDaylightingManager->GroundHitPt;         // Coordinates of point that ray hits ground (m)
    auto &ObsHitPt = state.dataDaylightingManager->ObsHitPt;               // Coordinates of hit point on an obstruction (m)
    auto &ReflNorm = state.dataDaylightingManager->ReflNorm;               // Normal vector to reflecting surface
    auto &SunVecMir = state.dataDaylightingManager->SunVecMir;             // Sun ray mirrored in reflecting surface
    auto &HitPtRefl = state.dataDaylightingManager->HitPtRefl;             // Point that ray hits reflecting surface
    auto &HitPtObs = state.dataDaylightingManager->HitPtObs;               // Hit point on obstruction
    auto &HitPtIntWinDisk = state.dataDaylightingManager->HitPtIntWinDisk; // Intersection point on an interior window for ray from ref pt to sun (m)
    Real64 Alfa;                                                           // Intermediate variables
    bool hitObs;                                                           // True iff obstruction is hit
    Real64 ObsVisRefl;                                                     // Visible reflectance of obstruction
    Real64 SkyReflVisLum;                                                  // Reflected sky luminance at hit point divided by

    int RecSurfNum;  // Receiving surface number
    int ReflSurfNum; // Reflecting surface number
    int ReflSurfNumX;
    Real64 CosIncAngRefl;   // Cos of angle of incidence of beam on reflecting surface
    Real64 CosIncAngRec;    // Cos of angle of incidence of reflected beam on receiving window
    bool hitRefl;           // True iff ray hits reflecting surface
    Real64 ReflDistanceSq;  // Distance squared between ref pt and hit point on reflecting surf (m^2)
    Real64 ReflDistance;    // Distance between ref pt and hit point on reflecting surf (m)
    bool hitObsRefl;        // True iff obstruction hit between ref pt and reflection point
    int ReflSurfRecNum;     // Receiving surface number for a reflecting window
    Real64 SpecReflectance; // Specular reflectance of a reflecting surface
    Real64 TVisRefl;        // Bare window vis trans for reflected beam
    //  (times light well efficiency, if appropriate)
    Real64 PHSUNrefl; // Altitude angle of reflected sun (radians)
    Real64 THSUNrefl; // Azimuth anggle of reflected sun (radians)

    bool hitIntWinDisk; // True iff ray from ref pt to sun passes thru an int window
    bool hitIntObsDisk; // True iff ray from ref pt to sun hits an interior obstruction
    //        bool hitExtObsDisk; // True iff ray from ref pt to sun hits an exterior obstruction //Unused Set but never
    // used

    int IntWinDiskHitNum; // Surface number of int window intersected by ray betw ref pt and sun
    Real64 COSBIntWin;    // Cos of angle between int win outward normal and ray betw ref pt and
    //  exterior window element or between ref pt and sun
    Real64 TVisIntWinMult;     // Interior window vis trans multiplier for ext win in adjacent zone
    Real64 TVisIntWinDiskMult; // Interior window vis trans solar disk multiplier for ext win in adj zone
    Real64 WindowSolidAngleDaylightPoint;

    ++ISunPos;

    // Altitude of sun (degrees)
    state.dataDaylightingManager->PHSUN = state.dataDaylightingManager->PHSUNHR(iHour);
    state.dataDaylightingManager->SPHSUN = state.dataDaylightingManager->SPHSUNHR(iHour);
    state.dataDaylightingManager->CPHSUN = state.dataDaylightingManager->CPHSUNHR(iHour);

    // Azimuth of sun in absolute coord sys
    state.dataDaylightingManager->THSUN = state.dataDaylightingManager->THSUNHR(iHour);

    // First time through, call routine to calculate inter-reflected illuminance
    // at reference point and luminance of window with shade, screen or blind.

    // Rob/TH - Not sure whether this call is necessary for interior zones with interior windows only.
    //  new code would be -
    // IF (LSHCAL == 1 .AND. ExtWinType /= AdjZoneExtWin) CALL DayltgInterReflectedIllum(ISunPos,IHR,ZoneNum,IWin2)
    int enclNum = 0; // enclosure index
    int zoneNum = 0; // zone index
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        zoneNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).zoneIndex;
        enclNum = state.dataDaylightingData->daylightControl(daylightCtrlNum).enclIndex;
    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
        assert(MapNum > 0);
        zoneNum = state.dataDaylightingData->IllumMapCalc(MapNum).zoneIndex;
        enclNum = state.dataDaylightingData->IllumMapCalc(MapNum).enclIndex;
    }
    if (state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel) {
        if (LSHCAL == 1) DayltgInterReflectedIllum(state, ISunPos, iHour, enclNum, IWin2);
    } else {
        if (LSHCAL == 1) DayltgInterReflectedIllumComplexFenestration(state, IWin2, WinEl, iHour, daylightCtrlNum, iRefPoint, CalledFrom, MapNum);
        if (COSB <= 0.0) return;
        DayltgDirectIllumComplexFenestration(state, IWin, WinEl, iHour, iRefPoint, CalledFrom, MapNum);
        // Call direct sun component only once since calculation is done for entire window
        if (WinEl == (NWX * NWY)) {
            DayltgDirectSunDiskComplexFenestration(state, IWin2, iHour, iRefPoint, WinEl, AZVIEW, CalledFrom, MapNum);
        }
        return;
    }

    // Daylighting shelf simplification:  The shelf completely blocks all view of the window,
    // only interrelflected illumination is allowed (see DayltgInterReflectedIllum above).
    // Everything else in this loop has to do with direct luminance from the window.
    if (InShelfSurf > 0) return;

    if (COSB <= 0.0) return;

    XEDIRSK = 0.0;
    //        XEDIRSU = 0.0; //Unused Set but never used
    XAVWLSK = 0.0;
    Real64 const Ray_3(Ray(3));
    Real64 const DOMEGA_Ray_3(DOMEGA * Ray_3);

    // Add contribution of this window element to glare and to
    // direct illuminance at reference point

    // The I,J,K indices for sky and sun components of direct illuminance
    // (EDIRSK, EDIRSU) and average window luminance (AVWLSK, AVWLSU) are:
    // I=1 for clear sky, =2 Clear turbid, =3 Intermediate, =4 Overcast;
    // J=1 for bare window, =2 for window with shade or fixed slat-angle blind;
    //  = 2,3,...,MaxSlatAngs+1 for window with variable slat-angle blind;
    // K = sun position index.

    // ----- CASE I -- BARE WINDOW (no shading device)

    // Beam solar and sky solar reflected from nearest obstruction.
    // In the following hitIntObs == false  ==> no interior obstructions hit, and
    //                  hitExtObs == true  ==> one or more exterior obstructions hit.
    if (state.dataSurface->CalcSolRefl && !hitIntObs && hitExtObs) {
        // One or more exterior obstructions was hit; get contribution of reflection
        // from nearest obstruction.
        // Find obstruction whose hit point is closest to this ray's window element
        DayltgClosestObstruction(state, RWIN2, Ray, NearestHitSurfNum, NearestHitPt);
        if (NearestHitSurfNum > 0) {

            // Beam solar reflected from nearest obstruction

            DayltgSurfaceLumFromSun(state, iHour, Ray, NearestHitSurfNum, NearestHitPt, LumAtHitPtFrSun);
            state.dataDaylightingManager->AVWLSU(iHour, 1) += LumAtHitPtFrSun * TVISB;
            if (PHRAY >= 0.0) state.dataDaylightingManager->EDIRSU(iHour, 1) += LumAtHitPtFrSun * DOMEGA_Ray_3 * TVISB;

            // Sky solar reflected from nearest obstruction

            int const ObsConstrNum = state.dataSurface->SurfActiveConstruction(NearestHitSurfNum);
            if (ObsConstrNum > 0) {
                // Exterior building surface is nearest hit
                if (!state.dataConstruction->Construct(ObsConstrNum).TypeIsWindow) {
                    // Obstruction is not a window, i.e., is an opaque surface
                    ObsVisRefl = 1.0 - state.dataMaterial->Material(state.dataConstruction->Construct(ObsConstrNum).LayerPoint(1)).AbsorpVisible;
                } else {
                    // Obstruction is a window; assume it is bare
                    ObsVisRefl = state.dataConstruction->Construct(ObsConstrNum).ReflectVisDiffFront;
                }
            } else {
                // Shadowing surface is nearest hit
                if (state.dataSurface->SurfDaylightingShelfInd(NearestHitSurfNum) > 0) {
                    // This is a daylighting shelf, for which reflection is separately calculated
                    ObsVisRefl = 0.0;
                } else {
                    ObsVisRefl = state.dataSurface->SurfShadowDiffuseVisRefl(NearestHitSurfNum);
                    if (state.dataSurface->SurfShadowGlazingConstruct(NearestHitSurfNum) > 0)
                        ObsVisRefl +=
                            state.dataSurface->SurfShadowGlazingFrac(NearestHitSurfNum) *
                            state.dataConstruction->Construct(state.dataSurface->SurfShadowGlazingConstruct(NearestHitSurfNum)).ReflectVisDiffFront;
                }
            }
            NearestHitSurfNumX = NearestHitSurfNum;
            // Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
            // The following gets the correct side of a shadowing surface for reflection.
            if (state.dataSurface->Surface(NearestHitSurfNum).IsShadowing) {
                if (dot(Ray, state.dataSurface->Surface(NearestHitSurfNum).OutNormVec) > 0.0) NearestHitSurfNumX = NearestHitSurfNum + 1;
            }
            if (!state.dataSysVars->DetailedSkyDiffuseAlgorithm || !state.dataSurface->ShadingTransmittanceVaries ||
                state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
                SkyReflVisLum = ObsVisRefl * state.dataSurface->Surface(NearestHitSurfNumX).ViewFactorSky *
                                state.dataSolarShading->SurfDifShdgRatioIsoSky(NearestHitSurfNumX) / DataGlobalConstants::Pi;
            } else {
                SkyReflVisLum = ObsVisRefl * state.dataSurface->Surface(NearestHitSurfNumX).ViewFactorSky *
                                state.dataSolarShading->SurfDifShdgRatioIsoSkyHRTS(1, iHour, NearestHitSurfNumX) / DataGlobalConstants::Pi;
            }
            assert(equal_dimensions(state.dataDaylightingManager->AVWLSK, state.dataDaylightingManager->EDIRSK));
            auto l2(state.dataDaylightingManager->GILSK.index(iHour, 1));
            auto l3(state.dataDaylightingManager->AVWLSK.index(iHour, 1, 1));
            for (ISky = 1; ISky <= 4; ++ISky, ++l2, ++l3) { // [ l2 ] == ( ISky, iHour ) // [ l3 ] == ( ISky, 1, iHour )
                XAVWLSK(ISky) = state.dataDaylightingManager->GILSK[l2] * SkyReflVisLum;
                state.dataDaylightingManager->AVWLSK[l3] += XAVWLSK(ISky) * TVISB;
                if (PHRAY >= 0.0) {
                    XEDIRSK(ISky) = state.dataDaylightingManager->GILSK[l2] * SkyReflVisLum * DOMEGA_Ray_3;
                    state.dataDaylightingManager->EDIRSK[l3] += XEDIRSK(ISky) * TVISB;
                }
            }
        }
    } // End of check if solar reflection calculation is in effect

    if (ObTrans > 1.e-6) {
        // Ray did not hit an obstruction or the transmittance product of hit obstructions is non-zero.
        // Contribution of sky or ground luminance in cd/m2
        if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Diffuser) {
            // Make all transmitted light diffuse for a TDD with a bare diffuser
            assert(equal_dimensions(state.dataDaylightingManager->AVWLSK, state.dataDaylightingManager->WLUMSK));
            assert(equal_dimensions(state.dataDaylightingManager->AVWLSK, state.dataDaylightingManager->EDIRSK));
            auto l3(state.dataDaylightingManager->AVWLSK.index(iHour, 1, 1));
            for (ISky = 1; ISky <= 4; ++ISky, ++l3) { // [ l3 ] == ( ISky, 1, iHour )
                state.dataDaylightingManager->AVWLSK[l3] += state.dataDaylightingManager->WLUMSK[l3];
                if (ISky == 1) {
                    state.dataDaylightingManager->AVWLSU(iHour, 1) += state.dataDaylightingManager->WLUMSU(iHour, 1);
                    state.dataDaylightingManager->AVWLSUdisk(iHour, 1) += state.dataDaylightingManager->WLUMSUdisk(iHour, 1);
                }
                if (PHRAY > 0.0) {
                    state.dataDaylightingManager->EDIRSK[l3] += state.dataDaylightingManager->WLUMSK[l3] * DOMEGA_Ray_3;
                    if (ISky == 1) state.dataDaylightingManager->EDIRSU(iHour, 1) += state.dataDaylightingManager->WLUMSU(iHour, 1) * DOMEGA_Ray_3;
                }
            }

        } else { // Bare window
            // Tuned Hoisted operations out of loop and linear indexing
            if (state.dataSurface->CalcSolRefl) { // Coordinates of ground point hit by the ray
                Alfa = std::acos(-Ray_3);
                Real64 const Ray_1(Ray(1));
                Real64 const Ray_2(Ray(2));
                //                    Beta = std::atan2( Ray_2, Ray_1 ); //Unused Tuning below eliminated use
                Real64 HorDis((RWIN2(3) - state.dataSurface->GroundLevelZ) *
                              std::tan(Alfa)); // Distance between ground hit point and proj'n of center
                GroundHitPt(3) = state.dataSurface->GroundLevelZ;
                // Tuned Replaced by below: sqrt is faster than sincos
                //                    GroundHitPt( 1 ) = RWIN2( 1 ) + HorDis * std::cos( Beta );
                //                    GroundHitPt( 2 ) = RWIN2( 2 ) + HorDis * std::sin( Beta );
                Real64 const Ray_r(std::sqrt(square(Ray_1) + square(Ray_2)));
                if (Ray_r > 0.0) {
                    HorDis /= Ray_r;
                    GroundHitPt(1) = RWIN2(1) + HorDis * Ray_1;
                    GroundHitPt(2) = RWIN2(2) + HorDis * Ray_2;
                } else { // Treat as angle==0
                    GroundHitPt(1) = RWIN2(1) + HorDis;
                    GroundHitPt(2) = RWIN2(2);
                }
            }
            Real64 const GILSK_mult((state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi) * ObTrans * SkyObstructionMult);
            Real64 const TVISB_ObTrans(TVISB * ObTrans);
            Real64 const AVWLSU_add(TVISB_ObTrans * state.dataDaylightingManager->GILSU(iHour) *
                                    (state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi));
            Vector3<Real64> const SUNCOS_iHour(state.dataSurface->SurfSunCosHourly(iHour));
            assert(equal_dimensions(state.dataDaylightingManager->EDIRSK, state.dataDaylightingManager->AVWLSK));
            auto l(state.dataDaylightingManager->EDIRSK.index(iHour, 1, 1));
            for (ISky = 1; ISky <= 4; ++ISky, ++l) { // [ l ] == ( iHour, 1, ISky )
                if (PHRAY > 0.0) {                   // Ray heads upward to sky
                    ELUM = DayltgSkyLuminance(state, ISky, THRAY, PHRAY);
                    XEDIRSK(ISky) = ELUM * DOMEGA_Ray_3;
                    DEDIR = XEDIRSK(ISky) * TVISB;
                    state.dataDaylightingManager->EDIRSK[l] += DEDIR * ObTrans;
                    state.dataDaylightingManager->AVWLSK[l] += ELUM * TVISB_ObTrans;
                    XAVWLSK(ISky) = ELUM * ObTrans;
                } else { // PHRAY <= 0.
                    // Ray heads downward to ground.
                    // Contribution from sky diffuse reflected from ground
                    XAVWLSK(ISky) = state.dataDaylightingManager->GILSK(iHour, ISky) * GILSK_mult;
                    state.dataDaylightingManager->AVWLSK[l] += TVISB * XAVWLSK(ISky);
                    // Contribution from beam solar reflected from ground (beam reaching ground point
                    // can be obstructed [SunObstructionMult < 1.0] if CalcSolRefl = .TRUE.)
                    if (ISky == 1) {
                        // SunObstructionMult = 1.0; //Tuned
                        if (state.dataSurface->CalcSolRefl) { // Coordinates of ground point hit by the ray
                            // Sun reaches ground point if vector from this point to the sun is unobstructed
                            hitObs = false;
                            for (int ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
                                if (!state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) continue;
                                PierceSurface(state, ObsSurfNum, GroundHitPt, SUNCOS_iHour, ObsHitPt, hitObs);
                                if (hitObs) break;
                            }
                            // if ( hitObs ) SunObstructionMult = 0.0;
                            if (!hitObs) state.dataDaylightingManager->AVWLSU(iHour, 1) += AVWLSU_add;
                        } else {
                            state.dataDaylightingManager->AVWLSU(iHour, 1) += AVWLSU_add;
                        }
                    } // End of check if ISky = 1
                }     // End of check if ray is going up or down
            }         // End of loop over sky types
        }             // End of check if bare window or TDD:DIFFUSER
    }                 // End of check if ObTrans > 1.E-6

    // Illuminance from beam solar (without interior reflection)
    // Just run this once on the last pass
    if (iXelement == NWX && iYelement == NWY) { // Last pass

        // Beam solar reaching reference point directly without exterior reflection

        // Unit vector from ref. pt. to sun
        RAYCOS(1) = state.dataDaylightingManager->CPHSUN * std::cos(state.dataDaylightingManager->THSUN);
        RAYCOS(2) = state.dataDaylightingManager->CPHSUN * std::sin(state.dataDaylightingManager->THSUN);
        RAYCOS(3) = state.dataDaylightingManager->SPHSUN;

        // Is sun on front side of exterior window?
        COSI = dot(WNORM2, RAYCOS);
        if (COSI > 0.0) {

            // Does RAYCOS pass thru exterior window? HP is point that RAYCOS intersects window plane.
            PierceSurface(state, IWin2, RREF2, RAYCOS, HP, hitWin);
            hitIntObsDisk = false;
            if (hitWin) {
                if (ExtWinType == DataDaylighting::ExtWinType::InZoneExtWin) {
                    // Check for interior obstructions between reference point and HP.
                    DayltgHitInteriorObstruction(state, IWin2, RREF2, HP, hitIntObsDisk);
                }
                ObTransDisk = 0.0; // Init value
                // Init flag for vector from RP to sun passing through interior window
                hitIntWinDisk = false;
                if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) { // This block is for RPs in zones with interior windows
                    // adjacent to zones with exterior windows
                    // Does RAYCOS pass through interior window in zone containing RP?
                    // Loop over zone surfaces looking for interior windows between reference point and sun
                    auto &thisZone = state.dataHeatBal->Zone(zoneNum);
                    for (int IntWinDisk = thisZone.WindowSurfaceFirst, IntWinDisk_end = thisZone.WindowSurfaceLast; IntWinDisk <= IntWinDisk_end;
                         ++IntWinDisk) {
                        if (state.dataSurface->Surface(IntWinDisk).ExtBoundCond >= 1) {
                            if (state.dataSurface->Surface(state.dataSurface->Surface(IntWinDisk).ExtBoundCond).Zone ==
                                state.dataSurface->Surface(IWin2).Zone) {
                                PierceSurface(state, IntWinDisk, RREF, RAYCOS, HitPtIntWinDisk, hitIntWinDisk);
                                if (hitIntWinDisk) {
                                    IntWinDiskHitNum = IntWinDisk;
                                    COSBIntWin = dot(state.dataSurface->Surface(IntWinDisk).OutNormVec, RAYCOS);
                                    if (COSBIntWin <= 0.0) {
                                        hitIntWinDisk = false;
                                        IntWinDiskHitNum = 0;
                                        continue;
                                    }
                                    TVISIntWinDisk = POLYF(
                                        COSBIntWin,
                                        state.dataConstruction->Construct(state.dataSurface->Surface(IntWinDisk).Construction).TransVisBeamCoef);
                                    break;
                                }
                            }
                        }
                    }

                    if (!hitIntWinDisk) { // Vector from RP to sun does not pass through interior window
                        ObTransDisk = 0.0;
                        hit = true; //! fcw Is this needed?
                    }

                    // Check for interior obstructions between ref point and interior window
                    hitIntObsDisk = false;
                    if (hitIntWinDisk) {
                        DayltgHitInteriorObstruction(state, IntWinDiskHitNum, RREF, HitPtIntWinDisk, hitIntObsDisk);
                        // If no obstruction between RP and hit int win, check for obstruction
                        // between int win and ext win
                        if (!hitIntObsDisk) {
                            DayltgHitBetWinObstruction(state, IntWinDiskHitNum, IWin2, HitPtIntWinDisk, HP, hitIntObsDisk);
                        }
                    }
                    if (hitIntObsDisk) ObTransDisk = 0.0;
                } // case where RP is in zone with interior window adjacent to zone with exterior window

                //                    hitExtObsDisk = false; //Unused Set but never used
                // RJH 08-25-07 hitIntWinDisk should not be reset to false here, and should be tested below.
                // This is to correct logic flaw causing direct solar to reach adjacent zone refpt
                // when vector to sun does not pass through interior window
                // hitIntWinDisk = false
                if (!hitIntObsDisk) { // No interior obstruction was hit
                    // Net transmittance of exterior obstructions encountered by RAYCOS
                    // ObTransDisk = 1.0 will be returned if no exterior obstructions are hit.
                    DayltgHitObstruction(state, iHour, IWin2, RREF2, RAYCOS, ObTransDisk);
                    //                        if ( ObTransDisk < 1.0 ) hitExtObsDisk = true; //Unused Set but never used
                    // RJH 08-26-07 However, if this is a case of interior window
                    // and vector to sun does not pass through interior window
                    // then reset ObTransDisk to 0.0 since it is the key test for adding
                    // contribution of sun to RP below.
                    if ((ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) && (!hitIntWinDisk)) {
                        ObTransDisk = 0.0;
                    }
                }

                // PETER: need side wall mounted TDD to test this
                // PETER: probably need to replace RREF2 with RWIN2
                // PETER: need to check for interior obstructions too.

                if (ObTransDisk > 1.e-6) {

                    // Sun reaches reference point;  increment illuminance.
                    // Direct normal illuminance is normalized to 1.0

                    if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Diffuser) {
                        // No beam is transmitted.  Takes care of TDD with a bare diffuser and all types of blinds.
                        TVISS = 0.0;
                    } else {
                        // Beam transmittance for bare window and all types of blinds
                        TVISS = POLYF(COSI, state.dataConstruction->Construct(IConst).TransVisBeamCoef) * state.dataSurface->SurfWinGlazedFrac(IWin) *
                                state.dataSurface->SurfWinLightWellEff(IWin);
                        if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin && hitIntWinDisk) TVISS *= TVISIntWinDisk;
                    }

                    state.dataDaylightingManager->EDIRSUdisk(iHour, 1) = RAYCOS(3) * TVISS * ObTransDisk; // Bare window

                    TransBmBmMult = 0.0;
                    if (ANY_BLIND(ShType)) {
                        ProfileAngle(state, IWin, RAYCOS, state.dataHeatBal->Blind(BlNum).SlatOrientation, ProfAng);
                        // Contribution of beam passing through slats and reaching reference point
                        for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                            // IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                            if (state.dataSurface->SurfWinMovableSlats(IWin)) {
                                SlatAng = (JB - 1) * DataGlobalConstants::Pi / (MaxSlatAngs - 1);
                            } else {
                                SlatAng = state.dataHeatBal->Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians;
                            }
                            TransBmBmMult(JB) = General::BlindBeamBeamTrans(ProfAng,
                                                                            SlatAng,
                                                                            state.dataHeatBal->Blind(BlNum).SlatWidth,
                                                                            state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                                                            state.dataHeatBal->Blind(BlNum).SlatThickness);
                            state.dataDaylightingManager->EDIRSUdisk(iHour, JB + 1) = RAYCOS(3) * TVISS * TransBmBmMult(JB) * ObTransDisk;

                            // do this only once for fixed slat blinds
                            if (!state.dataSurface->SurfWinMovableSlats(IWin)) break;
                        }
                    } else if (ShType == WinShadingType::ExtScreen) {
                        //                          pass angle from sun to window normal here using PHSUN and THSUN from above and surface angles
                        //                          SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
                        //                          SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
                        CalcScreenTransmittance(state,
                                                IWin,
                                                (state.dataDaylightingManager->PHSUN - state.dataSurface->SurfWinPhi(IWin)),
                                                (state.dataDaylightingManager->THSUN - state.dataSurface->SurfWinTheta(IWin)));
                        TransBmBmMult(1) = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).BmBmTrans;
                        state.dataDaylightingManager->EDIRSUdisk(iHour, 2) = RAYCOS(3) * TVISS * TransBmBmMult(1) * ObTransDisk;
                    }

                    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                        // Glare from solar disk

                        // Position factor for sun (note that AZVIEW is wrt y-axis and THSUN is wrt
                        // x-axis of absolute coordinate system.
                        XR = std::tan(std::abs(DataGlobalConstants::PiOvr2 - AZVIEW - state.dataDaylightingManager->THSUN) + 0.001);
                        YR = std::tan(state.dataDaylightingManager->PHSUN + 0.001);
                        POSFAC = DayltgGlarePositionFactor(XR, YR);

                        WindowSolidAngleDaylightPoint = state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPtWtd(iRefPoint);

                        if (POSFAC != 0.0 && WindowSolidAngleDaylightPoint > 0.000001) {
                            // Increment window luminance.  Luminance of solar disk (cd/m2)
                            // is 1.47*10^4*(direct normal solar illuminance) for direct normal solar
                            // illuminance in lux (lumens/m2). For purposes of calculating daylight factors
                            // direct normal solar illuminance = 1.0.
                            // Solid angle subtended by sun is 0.000068 steradians

                            XAVWL = 14700.0 * std::sqrt(0.000068 * POSFAC) * double(NWX * NWY) / std::pow(WindowSolidAngleDaylightPoint, 0.8);
                            state.dataDaylightingManager->AVWLSUdisk(iHour, 1) = XAVWL * TVISS * ObTransDisk; // Bare window

                            if (ANY_BLIND(ShType)) {
                                for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                                    // IF (.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                    state.dataDaylightingManager->AVWLSUdisk(iHour, JB + 1) = XAVWL * TVISS * TransBmBmMult(JB) * ObTransDisk;
                                    if (!state.dataSurface->SurfWinMovableSlats(IWin)) break;
                                }
                            } else if (ShType == WinShadingType::ExtScreen) {
                                state.dataDaylightingManager->AVWLSUdisk(iHour, 2) = XAVWL * TVISS * TransBmBmMult(1) * ObTransDisk;
                            }
                        } // Position Factor
                    }
                } // Beam avoids all obstructions
            }     // Beam passes thru window
        }         // Sun on front side

        // Beam solar reaching reference point after beam-beam (specular) reflection from
        // an exterior surface

        if (state.dataSurface->CalcSolRefl) {
            // Receiving surface number corresponding this window
            RecSurfNum = state.dataSurface->SurfShadowRecSurfNum(IWin2);
            if (RecSurfNum > 0) { // interior windows do not apply
                if (state.dataSolarReflectionManager->SolReflRecSurf(RecSurfNum).NumPossibleObs > 0) {
                    // This window has associated obstructions that could reflect beam onto the window
                    for (int loop = 1, loop_end = state.dataSolarReflectionManager->SolReflRecSurf(RecSurfNum).NumPossibleObs; loop <= loop_end;
                         ++loop) {
                        ReflSurfNum = state.dataSolarReflectionManager->SolReflRecSurf(RecSurfNum).PossibleObsSurfNums(loop);
                        ReflSurfNumX = ReflSurfNum;
                        // Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
                        // The following gets the correct side of a shadowing surface for reflection.
                        if (state.dataSurface->Surface(ReflSurfNum).IsShadowing) {
                            if (dot(RAYCOS, state.dataSurface->Surface(ReflSurfNum).OutNormVec) < 0.0) ReflSurfNumX = ReflSurfNum + 1;
                        }
                        // Require that the surface can have specular reflection
                        if (state.dataSurface->Surface(ReflSurfNum).Class == SurfaceClass::Window ||
                            state.dataSurface->SurfShadowGlazingFrac(ReflSurfNum) > 0.0) {
                            ReflNorm = state.dataSurface->Surface(ReflSurfNumX).OutNormVec;
                            // Vector to sun that is mirrored in obstruction
                            SunVecMir = RAYCOS - 2.0 * dot(RAYCOS, ReflNorm) * ReflNorm;
                            // Skip if reflecting surface is not sunlit
                            if (state.dataHeatBal->SurfSunlitFrac(iHour, 1, ReflSurfNumX) < 0.01) continue;
                            // Skip if altitude angle of mirrored sun is negative since reflected sun cannot
                            // reach reference point in this case
                            if (SunVecMir(3) <= 0.0) continue;
                            // Cosine of incidence angle of reflected beam on window
                            CosIncAngRec = dot(state.dataSurface->Surface(IWin2).OutNormVec, SunVecMir);
                            if (CosIncAngRec <= 0.0) continue;
                            // Does ray from ref. pt. along SunVecMir pass through window?
                            PierceSurface(state, IWin2, RREF2, SunVecMir, HP, hitWin);
                            if (!hitWin) continue; // Ray did not pass through window
                            // Check if this ray hits interior obstructions
                            DayltgHitInteriorObstruction(state, IWin2, RREF2, HP, hit);
                            if (hit) continue; // Interior obstruction was hit
                            // Does ray hit this reflecting surface?
                            PierceSurface(state, ReflSurfNum, RREF2, SunVecMir, HitPtRefl, hitRefl);
                            if (!hitRefl) continue; // Ray did not hit this reflecting surface
                            ReflDistanceSq = distance_squared(HitPtRefl, RREF2);
                            ReflDistance = std::sqrt(ReflDistanceSq);
                            // Is ray from ref. pt. to reflection point (HitPtRefl) obstructed?
                            hitObsRefl = false;
                            for (int loop2 = 1, loop2_end = state.dataSolarReflectionManager->SolReflRecSurf(RecSurfNum).NumPossibleObs;
                                 loop2 <= loop2_end;
                                 ++loop2) {
                                int const ObsSurfNum = state.dataSolarReflectionManager->SolReflRecSurf(RecSurfNum).PossibleObsSurfNums(loop2);
                                if (ObsSurfNum == ReflSurfNum || ObsSurfNum == state.dataSurface->Surface(ReflSurfNum).BaseSurf) continue;
                                PierceSurface(state, ObsSurfNum, RREF2, SunVecMir, ReflDistance, HitPtObs, hitObs); // ReflDistance cutoff added
                                if (hitObs) { // => Could skip distance check (unless < vs <= ReflDistance really matters)
                                    if (distance_squared(HitPtObs, RREF2) < ReflDistanceSq) { // Distance squared from ref pt to reflection point
                                        hitObsRefl = true;
                                        break;
                                    }
                                }
                            }
                            if (hitObsRefl) continue; // Obstruction closer than reflection pt. was hit; go to next obstruction
                            // There is no obstruction for this ray between ref pt and hit pt on reflecting surface.
                            // See if ray from hit pt on reflecting surface to original (unmirrored) sun position is obstructed
                            hitObs = false;
                            if (state.dataSurface->Surface(ReflSurfNum).Class == SurfaceClass::Window) {
                                // Reflecting surface is a window.
                                // Receiving surface number for this reflecting window.
                                ReflSurfRecNum = state.dataSurface->SurfShadowRecSurfNum(ReflSurfNum);
                                if (ReflSurfRecNum > 0) {
                                    // Loop over possible obstructions for this reflecting window
                                    for (int loop2 = 1, loop2_end = state.dataSolarReflectionManager->SolReflRecSurf(ReflSurfRecNum).NumPossibleObs;
                                         loop2 <= loop2_end;
                                         ++loop2) {
                                        int const ObsSurfNum =
                                            state.dataSolarReflectionManager->SolReflRecSurf(ReflSurfRecNum).PossibleObsSurfNums(loop2);
                                        PierceSurface(state, ObsSurfNum, HitPtRefl, RAYCOS, HitPtObs, hitObs);
                                        if (hitObs) break;
                                    }
                                }
                            } else {
                                // Reflecting surface is a building shade
                                for (int ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
                                    if (!state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) continue;
                                    if (ObsSurfNum == ReflSurfNum) continue;
                                    PierceSurface(state, ObsSurfNum, HitPtRefl, RAYCOS, HitPtObs, hitObs);
                                    if (hitObs) break;
                                }
                            } // End of check if reflector is a window or shadowing surface

                            if (hitObs) continue; // Obstruction hit between reflection hit point and sun; go to next obstruction

                            // No obstructions. Calculate reflected beam illuminance at ref. pt. from this reflecting surface.
                            SpecReflectance = 0.0;
                            CosIncAngRefl = std::abs(dot(RAYCOS, ReflNorm));
                            if (state.dataSurface->Surface(ReflSurfNum).Class == SurfaceClass::Window) {
                                int const ConstrNumRefl = state.dataSurface->SurfActiveConstruction(ReflSurfNum);
                                SpecReflectance =
                                    POLYF(std::abs(CosIncAngRefl), state.dataConstruction->Construct(ConstrNumRefl).ReflSolBeamFrontCoef);
                            }
                            if (state.dataSurface->Surface(ReflSurfNum).IsShadowing && state.dataSurface->SurfShadowGlazingConstruct(ReflSurfNum) > 0)
                                SpecReflectance = state.dataSurface->SurfShadowGlazingFrac(ReflSurfNum) *
                                                  POLYF(std::abs(CosIncAngRefl),
                                                        state.dataConstruction->Construct(state.dataSurface->SurfShadowGlazingConstruct(ReflSurfNum))
                                                            .ReflSolBeamFrontCoef);
                            TVisRefl = POLYF(CosIncAngRec, state.dataConstruction->Construct(IConst).TransVisBeamCoef) *
                                       state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
                            state.dataDaylightingManager->EDIRSUdisk(iHour, 1) += SunVecMir(3) * SpecReflectance * TVisRefl; // Bare window

                            TransBmBmMultRefl = 0.0;
                            if (ANY_BLIND(ShType)) {
                                ProfileAngle(state, IWin, SunVecMir, state.dataHeatBal->Blind(BlNum).SlatOrientation, ProfAng);
                                // Contribution of reflected beam passing through slats and reaching reference point
                                Real64 const Pi_SlatAng_fac(DataGlobalConstants::Pi / (MaxSlatAngs - 1));
                                for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                                    // IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                    if (state.dataSurface->SurfWinMovableSlats(IWin)) {
                                        SlatAng = double(JB - 1) * Pi_SlatAng_fac;
                                    } else {
                                        SlatAng = state.dataHeatBal->Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians;
                                    }
                                    TransBmBmMultRefl(JB) = General::BlindBeamBeamTrans(ProfAng,
                                                                                        SlatAng,
                                                                                        state.dataHeatBal->Blind(BlNum).SlatWidth,
                                                                                        state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                                                                        state.dataHeatBal->Blind(BlNum).SlatThickness);
                                    state.dataDaylightingManager->EDIRSUdisk(iHour, JB + 1) +=
                                        SunVecMir(3) * SpecReflectance * TVisRefl * TransBmBmMultRefl(JB);

                                    if (!state.dataSurface->SurfWinMovableSlats(IWin)) break;
                                }
                            } else if (ShType == WinShadingType::ExtScreen) {
                                //                             pass angle from sun to window normal here using PHSUN and THSUN from above and
                                //                             surface angles SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
                                //                             SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
                                CalcScreenTransmittance(state,
                                                        IWin,
                                                        (state.dataDaylightingManager->PHSUN - state.dataSurface->SurfWinPhi(IWin)),
                                                        (state.dataDaylightingManager->THSUN - state.dataSurface->SurfWinTheta(IWin)));
                                TransBmBmMultRefl(1) = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).BmBmTrans;
                                state.dataDaylightingManager->EDIRSUdisk(iHour, 2) +=
                                    SunVecMir(3) * SpecReflectance * TVisRefl * TransBmBmMultRefl(1);
                            } // End of check if window has a blind or screen

                            // Glare from reflected solar disk

                            PHSUNrefl = SunVecMir(3);
                            THSUNrefl = std::atan2(SunVecMir(2), SunVecMir(1));
                            XR = std::tan(std::abs(DataGlobalConstants::PiOvr2 - AZVIEW - THSUNrefl) + 0.001);
                            YR = std::tan(PHSUNrefl + 0.001);
                            POSFAC = DayltgGlarePositionFactor(XR, YR);
                            if (POSFAC != 0.0 && state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPtWtd(iRefPoint) > 0.000001) {
                                XAVWL = 14700.0 * std::sqrt(0.000068 * POSFAC) * double(NWX * NWY) /
                                        std::pow(state.dataSurface->SurfaceWindow(IWin).SolidAngAtRefPtWtd(iRefPoint), 0.8);
                                state.dataDaylightingManager->AVWLSUdisk(iHour, 1) += XAVWL * TVisRefl * SpecReflectance; // Bare window
                                if (ANY_BLIND(ShType)) {
                                    for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                                        // IF(.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                        state.dataDaylightingManager->AVWLSUdisk(iHour, JB + 1) +=
                                            XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(JB);
                                        if (!state.dataSurface->SurfWinMovableSlats(IWin)) break;
                                    }
                                } else if (ShType == WinShadingType::ExtScreen) {
                                    state.dataDaylightingManager->AVWLSUdisk(iHour, 2) += XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(1);
                                }
                            }
                        } // End of check that obstruction can specularly reflect
                    }     // End of loop over obstructions associated with this window

                } // End of check if this window has associated obstructions
            }     // End of check to see if this is exterior type window
        }         // End of check if exterior reflection calculation is in effect

    } // Last pass

    if ((ICtrl > 0 && (ANY_BLIND(ShType) || ANY_SHADE_SCREEN(ShType))) || state.dataSurface->SurfWinSolarDiffusing(IWin)) {

        // ----- CASE II -- WINDOW WITH SCREEN, SHADE, BLIND, OR DIFFUSING WINDOW

        // Interior window visible transmittance multiplier for exterior window in adjacent zone
        TVisIntWinMult = 1.0;
        TVisIntWinDiskMult = 1.0;
        if (state.dataSurface->Surface(IWin).SolarEnclIndex != state.dataDaylightingData->daylightControl(daylightCtrlNum).enclIndex) {
            TVisIntWinMult = TVISIntWin;
            TVisIntWinDiskMult = TVISIntWinDisk;
        }

        Real64 const DOMEGA_Ray_3_TVisIntWinMult(DOMEGA_Ray_3 * TVisIntWinMult);
        for (ISky = 1; ISky <= 4; ++ISky) {
            for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                // IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                state.dataDaylightingManager->AVWLSK(iHour, JB + 1, ISky) +=
                    state.dataDaylightingManager->WLUMSK(iHour, JB + 1, ISky) * TVisIntWinMult;
                if (ISky == 1) {
                    state.dataDaylightingManager->AVWLSU(iHour, JB + 1) += state.dataDaylightingManager->WLUMSU(iHour, JB + 1) * TVisIntWinMult;
                    state.dataDaylightingManager->AVWLSUdisk(iHour, JB + 1) +=
                        state.dataDaylightingManager->WLUMSUdisk(iHour, JB + 1) * TVisIntWinDiskMult;
                }
                if (PHRAY > 0.0) {
                    state.dataDaylightingManager->EDIRSK(iHour, JB + 1, ISky) +=
                        state.dataDaylightingManager->WLUMSK(iHour, JB + 1, ISky) * DOMEGA_Ray_3_TVisIntWinMult;
                    if (ISky == 1)
                        state.dataDaylightingManager->EDIRSU(iHour, JB + 1) +=
                            state.dataDaylightingManager->WLUMSU(iHour, JB + 1) * DOMEGA_Ray_3_TVisIntWinMult;
                }
                if (!state.dataSurface->SurfWinMovableSlats(IWin)) break;
            }
        }
    }
}

void FigureRefPointDayltgFactorsToAddIllums(EnergyPlusData &state,
                                            int const daylightCtrlNum, // Current daylighting control number
                                            int const iRefPoint,
                                            int const iHour,
                                            int &ISunPos,
                                            int const IWin,
                                            int const loopwin,
                                            int const NWX,  // Number of window elements in x direction for dayltg calc
                                            int const NWY,  // Number of window elements in y direction for dayltg calc
                                            int const ICtrl // Window control counter
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith, Oct 2012, derived from legacy code by Fred Winkelmann
    //       DATE WRITTEN   Oct. 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculation worker routine to fill daylighting coefficients

    // METHODOLOGY EMPLOYED:
    // this version is just for reference points.

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr tmpDFCalc(0.05); // cut off illuminance (lux) for exterior horizontal in calculating the daylighting and glare factors

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ISky;   // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
    int JSH;    // Shading index: J=1 is unshaded window, J=2 is shaded window
    Real64 VTR; // For switchable glazing, ratio of visible transmittance of fully-switched state to that of the unswitched state

    if (state.dataSurface->SurfSunCosHourly(iHour)(3) < DataEnvironment::SunIsUpValue) return;

    ++ISunPos;

    // Altitude of sun (degrees)
    state.dataDaylightingManager->PHSUN = state.dataDaylightingManager->PHSUNHR(iHour);
    state.dataDaylightingManager->SPHSUN = state.dataDaylightingManager->SPHSUNHR(iHour);
    state.dataDaylightingManager->CPHSUN = state.dataDaylightingManager->CPHSUNHR(iHour);

    // Azimuth of sun in absolute coord sys
    state.dataDaylightingManager->THSUN = state.dataDaylightingManager->THSUNHR(iHour);

    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
    int const enclNum = state.dataSurface->Surface(IWin).SolarEnclIndex;

    for (ISky = 1; ISky <= 4; ++ISky) { // Loop over sky types

        // Loop over shading index (1=bare window; 2=diffusing glazing, shade, screen or fixed slat-angle blind;
        // 2 to MaxSlatAngs+1 for variable slat-angle blind)

        // TH. 9/22/2009. CR 7625 - daylight illuminance spikes during some sunset hours due to the calculated sky and sun
        //  related daylight factors > 1, which theoretically can occur when sun is perpendicular to the window
        //  and interior surfaces with high visible reflectance.
        // Added tmpDFCalc (default to 0.05 lux) as the cap for GILSK and GILSU in calculating the daylight factors
        //  the assumption behind it is if exterior horizontal surface does not get daylight, spaces do not get daylight.

        for (JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH) {
            if (!state.dataSurface->SurfWinMovableSlats(IWin) && JSH > 2) break;

            if (state.dataDaylightingManager->GILSK(iHour, ISky) > tmpDFCalc) {
                thisDaylightControl.DaylIllFacSky(iHour, JSH, ISky, iRefPoint, loopwin) =
                    (state.dataDaylightingManager->EDIRSK(iHour, JSH, ISky) + state.dataDaylightingManager->EINTSK(iHour, JSH, ISky)) /
                    state.dataDaylightingManager->GILSK(iHour, ISky);
                thisDaylightControl.DaylSourceFacSky(iHour, JSH, ISky, iRefPoint, loopwin) =
                    state.dataDaylightingManager->AVWLSK(iHour, JSH, ISky) / (NWX * NWY * state.dataDaylightingManager->GILSK(iHour, ISky));
                thisDaylightControl.DaylBackFacSky(iHour, JSH, ISky, iRefPoint, loopwin) =
                    state.dataDaylightingManager->EINTSK(iHour, JSH, ISky) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect /
                    (DataGlobalConstants::Pi * state.dataDaylightingManager->GILSK(iHour, ISky));
            } else {
                thisDaylightControl.DaylIllFacSky(iHour, JSH, ISky, iRefPoint, loopwin) = 0.0;
                thisDaylightControl.DaylSourceFacSky(iHour, JSH, ISky, iRefPoint, loopwin) = 0.0;
                thisDaylightControl.DaylBackFacSky(iHour, JSH, ISky, iRefPoint, loopwin) = 0.0;
            }

            if (ISky == 1) {
                if (state.dataDaylightingManager->GILSU(iHour) > tmpDFCalc) {
                    thisDaylightControl.DaylIllFacSun(iHour, JSH, iRefPoint, loopwin) =
                        (state.dataDaylightingManager->EDIRSU(iHour, JSH) + state.dataDaylightingManager->EINTSU(iHour, JSH)) /
                        (state.dataDaylightingManager->GILSU(iHour) + 0.0001);
                    thisDaylightControl.DaylIllFacSunDisk(iHour, JSH, iRefPoint, loopwin) =
                        (state.dataDaylightingManager->EDIRSUdisk(iHour, JSH) + state.dataDaylightingManager->EINTSUdisk(iHour, JSH)) /
                        (state.dataDaylightingManager->GILSU(iHour) + 0.0001);

                    thisDaylightControl.DaylSourceFacSun(iHour, JSH, iRefPoint, loopwin) =
                        state.dataDaylightingManager->AVWLSU(iHour, JSH) / (NWX * NWY * (state.dataDaylightingManager->GILSU(iHour) + 0.0001));
                    thisDaylightControl.DaylSourceFacSunDisk(iHour, JSH, iRefPoint, loopwin) =
                        state.dataDaylightingManager->AVWLSUdisk(iHour, JSH) / (NWX * NWY * (state.dataDaylightingManager->GILSU(iHour) + 0.0001));

                    thisDaylightControl.DaylBackFacSun(iHour, JSH, iRefPoint, loopwin) =
                        state.dataDaylightingManager->EINTSU(iHour, JSH) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect /
                        (DataGlobalConstants::Pi * (state.dataDaylightingManager->GILSU(iHour) + 0.0001));
                    thisDaylightControl.DaylBackFacSunDisk(iHour, JSH, iRefPoint, loopwin) =
                        state.dataDaylightingManager->EINTSUdisk(iHour, JSH) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect /
                        (DataGlobalConstants::Pi * (state.dataDaylightingManager->GILSU(iHour) + 0.0001));
                } else {
                    thisDaylightControl.DaylIllFacSun(iHour, JSH, iRefPoint, loopwin) = 0.0;
                    thisDaylightControl.DaylIllFacSunDisk(iHour, JSH, iRefPoint, loopwin) = 0.0;

                    thisDaylightControl.DaylSourceFacSun(iHour, JSH, iRefPoint, loopwin) = 0.0;
                    thisDaylightControl.DaylSourceFacSunDisk(iHour, JSH, iRefPoint, loopwin) = 0.0;

                    thisDaylightControl.DaylBackFacSun(iHour, JSH, iRefPoint, loopwin) = 0.0;
                    thisDaylightControl.DaylBackFacSunDisk(iHour, JSH, iRefPoint, loopwin) = 0.0;
                }
            }
        } // End of shading index loop, JSH

        // For switchable glazing put daylighting factors for switched (dark) state in IS=2 location
        if (ICtrl > 0) {
            if (state.dataSurface->WindowShadingControl(ICtrl).ShadingType == WinShadingType::SwitchableGlazing) {
                VTR = state.dataSurface->SurfWinVisTransRatio(IWin);
                thisDaylightControl.DaylIllFacSky(iHour, 2, ISky, iRefPoint, loopwin) =
                    thisDaylightControl.DaylIllFacSky(iHour, 1, ISky, iRefPoint, loopwin) * VTR;
                thisDaylightControl.DaylSourceFacSky(iHour, 2, ISky, iRefPoint, loopwin) =
                    thisDaylightControl.DaylSourceFacSky(iHour, 1, ISky, iRefPoint, loopwin) * VTR;
                thisDaylightControl.DaylBackFacSky(iHour, 2, ISky, iRefPoint, loopwin) =
                    thisDaylightControl.DaylBackFacSky(iHour, 1, ISky, iRefPoint, loopwin) * VTR;
                if (ISky == 1) {
                    thisDaylightControl.DaylIllFacSun(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylIllFacSun(iHour, 1, iRefPoint, loopwin) * VTR;
                    thisDaylightControl.DaylSourceFacSun(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylSourceFacSun(iHour, 1, iRefPoint, loopwin) * VTR;
                    thisDaylightControl.DaylBackFacSun(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylBackFacSun(iHour, 1, iRefPoint, loopwin) * VTR;
                    thisDaylightControl.DaylIllFacSunDisk(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylIllFacSunDisk(iHour, 1, iRefPoint, loopwin) * VTR;
                    thisDaylightControl.DaylSourceFacSunDisk(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylSourceFacSunDisk(iHour, 1, iRefPoint, loopwin) * VTR;
                    thisDaylightControl.DaylBackFacSunDisk(iHour, 2, iRefPoint, loopwin) =
                        thisDaylightControl.DaylBackFacSunDisk(iHour, 1, iRefPoint, loopwin) * VTR;
                }
            }
        } // ICtrl > 0

    } // End of sky type loop, ISky
}

void FigureMapPointDayltgFactorsToAddIllums(EnergyPlusData &state,
                                            int const MapNum,
                                            int const iMapPoint,
                                            int const iHour,
                                            int const IWin,
                                            int const loopwin,
                                            int const ICtrl // Window control counter
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith, Oct 2012, derived from legacy code by Fred Winkelmann, Peter Ellis, Linda Lawrie
    //       DATE WRITTEN   Nov. 2012

    // PURPOSE OF THIS SUBROUTINE:
    // calculation worker routine to fill daylighting coefficients

    // METHODOLOGY EMPLOYED:
    // this version is just for map points.

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 constexpr tmpDFCalc(0.05); // cut off illuminance (lux) for exterior horizontal in calculating
    // the daylighting and glare factors

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ISky;   // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
    int JSH;    // Shading index: J=1 is unshaded window, J=2 is shaded window
    Real64 VTR; // For switchable glazing, ratio of visible transmittance of
    //  fully-switched state to that of the unswitched state

    if (state.dataSurface->SurfSunCosHourly(iHour)(3) < DataEnvironment::SunIsUpValue) return;

    for (ISky = 1; ISky <= 4; ++ISky) { // Loop over sky types

        // Loop over shading index (1=bare window; 2=diffusing glazing, shade, screen or fixed slat-angle blind;
        // 2 to MaxSlatAngs+1 for variable slat-angle blind)

        // TH. 9/22/2009. CR 7625 - daylight illuminance spikes during some sunset hours due to the calculated sky and sun
        //  related daylight factors > 1, which theoretically can occur when sun is perpendicular to the window
        //  and interior surfaces with high visible reflectance.
        // Added tmpDFCalc (default to 0.05 lux) as the cap for GILSK and GILSU in calculating the daylight factors
        //  the assumption behind it is if exterior horizontal surface does not get daylight, spaces do not get daylight.

        for (JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH) {
            if (!state.dataSurface->SurfWinMovableSlats(IWin) && JSH > 2) break;

            if (state.dataDaylightingManager->GILSK(iHour, ISky) > tmpDFCalc) {
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSky(iHour, JSH, ISky, iMapPoint, loopwin) =
                    (state.dataDaylightingManager->EDIRSK(iHour, JSH, ISky) + state.dataDaylightingManager->EINTSK(iHour, JSH, ISky)) /
                    state.dataDaylightingManager->GILSK(iHour, ISky);
            } else {
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSky(iHour, JSH, ISky, iMapPoint, loopwin) = 0.0;
            }

            if (ISky == 1) {
                if (state.dataDaylightingManager->GILSU(iHour) > tmpDFCalc) {
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSun(iHour, JSH, iMapPoint, loopwin) =
                        (state.dataDaylightingManager->EDIRSU(iHour, JSH) + state.dataDaylightingManager->EINTSU(iHour, JSH)) /
                        (state.dataDaylightingManager->GILSU(iHour) + 0.0001);
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSunDisk(iHour, JSH, iMapPoint, loopwin) =
                        (state.dataDaylightingManager->EDIRSUdisk(iHour, JSH) + state.dataDaylightingManager->EINTSUdisk(iHour, JSH)) /
                        (state.dataDaylightingManager->GILSU(iHour) + 0.0001);

                } else {
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSun(iHour, JSH, iMapPoint, loopwin) = 0.0;
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSunDisk(iHour, JSH, iMapPoint, loopwin) = 0.0;
                }
            }
        } // End of shading index loop, JSH

        // For switchable glazing put daylighting factors for switched (dark) state in IS=2 location
        if (ICtrl > 0) {
            if (state.dataSurface->WindowShadingControl(ICtrl).ShadingType == WinShadingType::SwitchableGlazing) {
                VTR = state.dataSurface->SurfWinVisTransRatio(IWin);
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSky(iHour, 2, ISky, iMapPoint, loopwin) =
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSky(iHour, 1, ISky, iMapPoint, loopwin) * VTR;
                if (ISky == 1) {
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSun(iHour, 2, iMapPoint, loopwin) =
                        state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSun(iHour, 1, iMapPoint, loopwin) * VTR;
                    state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSunDisk(iHour, 2, iMapPoint, loopwin) =
                        state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllFacSunDisk(iHour, 1, iMapPoint, loopwin) * VTR;
                }
            }
        } // ICtrl > 0

    } // End of sky type loop, ISky
}

void GetDaylightingParametersInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   Oct 2004

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine provides a simple structure to get all daylighting
    // parameters.

    using namespace DElightManagerF; // Module for managing DElight subroutines

    int TotDaylightingControls; // Total Daylighting:Controls inputs (splitflux or delight type)
    bool ErrorsFound;           // Error flag
    Real64 dLatitude;           // double for argument passing
    int iErrorFlag;             // Error Flag for warning/errors returned from DElight
    std::string cErrorMsg;      // Each DElight Error Message can be up to 200 characters long
    bool bEndofErrFile;         // End of Error File flag
    bool bRecordsOnErrFile;     // true if there are records on the error file
    int NumReports;
    int NumNames;
    int NumNumbers;
    int IOStat;

    if (!state.dataDaylightingManager->getDaylightingParametersInputFlag) return;
    state.dataDaylightingManager->getDaylightingParametersInputFlag = false;

    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    ErrorsFound = false;
    cCurrentModuleObject = "Daylighting:Controls";
    TotDaylightingControls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (TotDaylightingControls > 0) {
        state.dataDaylightingData->enclDaylight.allocate(state.dataViewFactor->NumOfSolarEnclosures);
        GetInputDayliteRefPt(state, ErrorsFound);
        GetDaylightingControls(state, ErrorsFound);
        GeometryTransformForDaylighting(state);
        GetInputIlluminanceMap(state, ErrorsFound);
        GetLightWellData(state, ErrorsFound);
        if (ErrorsFound) ShowFatalError(state, "Program terminated for above reasons, related to DAYLIGHTING");
        DayltgSetupAdjZoneListsAndPointers(state);
    }

    state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl = 0;
    state.dataDaylightingManager->maxNumRefPtInAnyEncl = 0;
    // Loop through all daylighting controls to find total reference points in each enclosure
    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        int numRefPoints = state.dataDaylightingData->daylightControl(daylightCtrlNum).TotalDaylRefPoints;
        state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl = max(numRefPoints, state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
    }
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        state.dataDaylightingManager->maxNumRefPtInAnyEncl =
            max(state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints, state.dataDaylightingManager->maxNumRefPtInAnyEncl);
    }

    for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
        if (state.dataSurface->Surface(SurfNum).Class != SurfaceClass::Window) continue;
        int const surfEnclNum = state.dataSurface->Surface(SurfNum).SolarEnclIndex;
        int const numEnclRefPoints = state.dataViewFactor->EnclSolInfo(surfEnclNum).TotalEnclosureDaylRefPoints;
        if (numEnclRefPoints > 0) {
            if (!state.dataSurface->SurfWinSurfDayLightInit(SurfNum)) {
                state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPt.allocate(numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPt = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPtWtd.allocate(numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPtWtd = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPt.allocate(2, numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPt = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).BackLumFromWinAtRefPt.allocate(2, numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).BackLumFromWinAtRefPt = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).SourceLumFromWinAtRefPt.allocate(2, numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).SourceLumFromWinAtRefPt = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep.allocate(numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep.allocate(numEnclRefPoints);
                state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep = 0.0;
                state.dataSurface->SurfWinSurfDayLightInit(SurfNum) = true;
            }
        } else {
            int SurfNumAdj = state.dataSurface->Surface(SurfNum).ExtBoundCond;
            if (SurfNumAdj > 0) {
                int const adjSurfEnclNum = state.dataSurface->Surface(SurfNumAdj).SolarEnclIndex;
                int const numAdjEnclRefPoints = state.dataViewFactor->EnclSolInfo(adjSurfEnclNum).TotalEnclosureDaylRefPoints;
                if (numAdjEnclRefPoints > 0) {
                    if (!state.dataSurface->SurfWinSurfDayLightInit(SurfNum)) {
                        state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPt.allocate(numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPt = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPtWtd.allocate(numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).SolidAngAtRefPtWtd = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPt.allocate(2, numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPt = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).BackLumFromWinAtRefPt.allocate(2, numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).BackLumFromWinAtRefPt = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).SourceLumFromWinAtRefPt.allocate(2, numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).SourceLumFromWinAtRefPt = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep.allocate(numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep = 0.0;
                        state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep.allocate(numAdjEnclRefPoints);
                        state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep = 0.0;
                        state.dataSurface->SurfWinSurfDayLightInit(SurfNum) = true;
                    }
                }
            }
        }

        if (state.dataSurface->Surface(SurfNum).ExtBoundCond == ExternalEnvironment) {

            if (state.dataSurface->Surface(SurfNum).HasShadeControl) {
                auto &thisSurfEnclosure(state.dataViewFactor->EnclSolInfo(state.dataSurface->Surface(SurfNum).SolarEnclIndex));
                if (state.dataSurface->WindowShadingControl(state.dataSurface->Surface(SurfNum).activeWindowShadingControl).GlareControlIsActive) {
                    // Error if GlareControlIsActive but window is not in a Daylighting:Detailed zone
                    if (thisSurfEnclosure.TotalEnclosureDaylRefPoints == 0) {
                        ShowSevereError(state, "Window=" + state.dataSurface->Surface(SurfNum).Name + " has Window Shading Control with");
                        ShowContinueError(state, "GlareControlIsActive = Yes but it is not in a Daylighting zone or enclosure.");
                        ShowContinueError(state,
                                          "Zone or enclosure indicated=" +
                                              state.dataViewFactor->EnclSolInfo(state.dataSurface->Surface(SurfNum).SolarEnclIndex).Name);
                        ErrorsFound = true;
                    }
                    // Error if GlareControlIsActive and window is in a Daylighting:Detailed zone/enclosure with
                    // an interior window adjacent to another Daylighting:Detailed zone/enclosure
                    if (thisSurfEnclosure.TotalEnclosureDaylRefPoints > 0) {
                        for (int const intWin : thisSurfEnclosure.SurfacePtr) {
                            int const SurfNumAdj = state.dataSurface->Surface(intWin).ExtBoundCond;
                            if (state.dataSurface->Surface(intWin).Class == SurfaceClass::Window && SurfNumAdj > 0) {
                                auto &adjSurfEnclosure(state.dataViewFactor->EnclSolInfo(state.dataSurface->Surface(SurfNumAdj).SolarEnclIndex));
                                if (adjSurfEnclosure.TotalEnclosureDaylRefPoints > 0) {
                                    ShowSevereError(state, "Window=" + state.dataSurface->Surface(SurfNum).Name + " has Window Shading Control with");
                                    ShowContinueError(state, "GlareControlIsActive = Yes and is in a Daylighting zone or enclosure");
                                    ShowContinueError(state, "that shares an interior window with another Daylighting zone or enclosure");
                                    ShowContinueError(state, "Adjacent Zone or Enclosure indicated=" + adjSurfEnclosure.Name);
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                }

                if (state.dataSurface->WindowShadingControl(state.dataSurface->Surface(SurfNum).activeWindowShadingControl).ShadingControlType ==
                    WindowShadingControlType::MeetDaylIlumSetp) {
                    // Error if window has ShadingControlType = MeetDaylightingIlluminanceSetpoint &
                    // but is not in a Daylighting:Detailed zone
                    if (thisSurfEnclosure.TotalEnclosureDaylRefPoints == 0) {
                        ShowSevereError(state, "Window=" + state.dataSurface->Surface(SurfNum).Name + " has Window Shading Control with");
                        ShowContinueError(state, "MeetDaylightingIlluminanceSetpoint but it is not in a Daylighting zone or enclosure.");
                        ShowContinueError(state, "Zone or enclosure indicated=" + thisSurfEnclosure.Name);
                        ErrorsFound = true;
                    }
                    // Error if window has ShadingControlType = MeetDaylightIlluminanceSetpoint and is in a &
                    // Daylighting:Detailed zone with an interior window adjacent to another Daylighting:Detailed zone
                    if (thisSurfEnclosure.TotalEnclosureDaylRefPoints > 0) {
                        for (int const intWin : thisSurfEnclosure.SurfacePtr) {
                            int const SurfNumAdj = state.dataSurface->Surface(intWin).ExtBoundCond;
                            if (state.dataSurface->Surface(intWin).Class == SurfaceClass::Window && SurfNumAdj > 0) {
                                auto &adjSurfEnclosure(state.dataViewFactor->EnclSolInfo(state.dataSurface->Surface(SurfNumAdj).SolarEnclIndex));
                                if (adjSurfEnclosure.TotalEnclosureDaylRefPoints > 0) {
                                    ShowSevereError(state, "Window=" + state.dataSurface->Surface(SurfNum).Name + " has Window Shading Control with");
                                    ShowContinueError(state, "MeetDaylightIlluminanceSetpoint and is in a Daylighting zone or enclosure");
                                    ShowContinueError(state, "that shares an interior window with another Daylighting zone or enclosure");
                                    ShowContinueError(state, "Adjacent Zone or enclosure indicated=" + adjSurfEnclosure.Name);
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if (!state.dataHeatBal->AnyAirBoundary) {
        for (int SurfLoop = 1; SurfLoop <= state.dataSurface->TotSurfaces; ++SurfLoop) {
            if (state.dataSurface->Surface(SurfLoop).Class == SurfaceClass::Window && state.dataSurface->Surface(SurfLoop).ExtSolar) {
                int const enclOfSurf = state.dataSurface->Surface(SurfLoop).SolarEnclIndex;
                if (state.dataViewFactor->EnclSolInfo(enclOfSurf).TotalEnclosureDaylRefPoints > 0 &&
                    !state.dataViewFactor->EnclSolInfo(enclOfSurf).HasInterZoneWindow &&
                    state.dataDaylightingData->enclDaylight(enclOfSurf).hasSplitFluxDaylighting) {
                    for (int refPtNum = 1; refPtNum <= state.dataViewFactor->EnclSolInfo(enclOfSurf).TotalEnclosureDaylRefPoints; ++refPtNum) {
                        SetupOutputVariable(state,
                                            format("Daylighting Window Reference Point {} Illuminance", refPtNum),
                                            OutputProcessor::Unit::lux,
                                            state.dataSurface->SurfaceWindow(SurfLoop).IllumFromWinAtRefPtRep(refPtNum),
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataSurface->Surface(SurfLoop).Name);
                        SetupOutputVariable(state,
                                            format("Daylighting Window Reference Point {} View Luminance", refPtNum),
                                            OutputProcessor::Unit::cd_m2,
                                            state.dataSurface->SurfaceWindow(SurfLoop).LumWinFromRefPtRep(refPtNum),
                                            OutputProcessor::SOVTimeStepType::Zone,
                                            OutputProcessor::SOVStoreType::Average,
                                            state.dataSurface->Surface(SurfLoop).Name);
                    }
                }
            }
        }
    } else {
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            for (int const enclSurfNum : state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr) {
                if (state.dataSurface->Surface(enclSurfNum).Class == SurfaceClass::Window && state.dataSurface->Surface(enclSurfNum).ExtSolar) {
                    if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints > 0 &&
                        !state.dataViewFactor->EnclSolInfo(enclNum).HasInterZoneWindow &&
                        state.dataDaylightingData->enclDaylight(enclNum).hasSplitFluxDaylighting) {
                        int refPtCount = 0;
                        for (int controlNum : state.dataDaylightingData->enclDaylight(enclNum).daylightControlIndexes) {
                            for (int refPtNum = 1; refPtNum <= state.dataDaylightingData->daylightControl(controlNum).TotalDaylRefPoints;
                                 ++refPtNum) {
                                ++refPtCount; // Count reference points across each daylighting control in the same enclosure
                                std::string const varKey =
                                    state.dataSurface->Surface(enclSurfNum).Name + " to " +
                                    state.dataDaylightingData
                                        ->DaylRefPt(state.dataDaylightingData->daylightControl(controlNum).DaylRefPtNum(refPtNum))
                                        .Name;
                                SetupOutputVariable(state,
                                                    "Daylighting Window Reference Point Illuminance",
                                                    OutputProcessor::Unit::lux,
                                                    state.dataSurface->SurfaceWindow(enclSurfNum).IllumFromWinAtRefPtRep(refPtCount),
                                                    OutputProcessor::SOVTimeStepType::Zone,
                                                    OutputProcessor::SOVStoreType::Average,
                                                    varKey);
                                SetupOutputVariable(state,
                                                    "Daylighting Window Reference Point View Luminance",
                                                    OutputProcessor::Unit::cd_m2,
                                                    state.dataSurface->SurfaceWindow(enclSurfNum).LumWinFromRefPtRep(refPtCount),
                                                    OutputProcessor::SOVTimeStepType::Zone,
                                                    OutputProcessor::SOVStoreType::Average,
                                                    varKey);
                            }
                        }
                    }
                }
            }
        }
    }

    // RJH DElight Modification Begin - Calls to DElight preprocessing subroutines
    if (doesDayLightingUseDElight(state)) {
        dLatitude = state.dataEnvrn->Latitude;
        DisplayString(state, "Calculating DElight Daylighting Factors");
        DElightInputGenerator(state);
        // Init Error Flag to 0 (no Warnings or Errors)
        DisplayString(state, "ReturnFrom DElightInputGenerator");
        iErrorFlag = 0;
        DisplayString(state, "Calculating DElight DaylightCoefficients");
        GenerateDElightDaylightCoefficients(dLatitude, iErrorFlag);
        // Check Error Flag for Warnings or Errors returning from DElight
        // RJH 2008-03-07: open file for READWRITE and DELETE file after processing
        DisplayString(state, "ReturnFrom DElight DaylightCoefficients Calc");
        if (iErrorFlag != 0) {
            // Open DElight Daylight Factors Error File for reading
            auto iDElightErrorFile = state.files.outputDelightDfdmpFilePath.try_open(state.files.outputControl.delightdfdmp);

            // Sequentially read lines in DElight Daylight Factors Error File
            // and process them using standard EPlus warning/error handling calls
            // Process all error/warning messages first
            // Then, if any error has occurred, ShowFatalError to terminate processing
            bEndofErrFile = !iDElightErrorFile.good();
            bRecordsOnErrFile = false;
            while (!bEndofErrFile) {
                auto cErrorLine = iDElightErrorFile.readLine();
                if (cErrorLine.eof) {
                    bEndofErrFile = true;
                    continue;
                }
                bRecordsOnErrFile = true;
                // Is the current line a Warning message?
                if (has_prefix(cErrorLine.data, "WARNING: ")) {
                    cErrorMsg = cErrorLine.data.substr(9);
                    ShowWarningError(state, cErrorMsg);
                }
                // Is the current line an Error message?
                if (has_prefix(cErrorLine.data, "ERROR: ")) {
                    cErrorMsg = cErrorLine.data.substr(7);
                    ShowSevereError(state, cErrorMsg);
                    iErrorFlag = 1;
                }
            }

            // Close and Delete DElight Error File
            if (iDElightErrorFile.is_open()) {
                iDElightErrorFile.close();
                FileSystem::removeFile(iDElightErrorFile.filePath);
            }

            // If any DElight Error occurred then ShowFatalError to terminate
            if (iErrorFlag > 0) {
                ErrorsFound = true;
            }
        } else {
            if (FileSystem::fileExists(state.files.outputDelightDfdmpFilePath.filePath)) {
                FileSystem::removeFile(state.files.outputDelightDfdmpFilePath.filePath);
            }
        }
    }
    // RJH DElight Modification End - Calls to DElight preprocessing subroutines

    // TH 6/3/2010, added to report daylight factors
    cCurrentModuleObject = "Output:DaylightFactors";
    NumReports = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (NumReports > 0) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumNames,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (has_prefix(state.dataIPShortCut->cAlphaArgs(1), "SIZINGDAYS")) {
            state.dataDaylightingData->DFSReportSizingDays = true;
        } else if (has_prefix(state.dataIPShortCut->cAlphaArgs(1), "ALLSHADOWCALCULATIONDAYS")) {
            state.dataDaylightingData->DFSReportAllShadowCalculationDays = true;
        }
    }

    if (ErrorsFound) ShowFatalError(state, "Program terminated for above reasons");
}

void GetInputIlluminanceMap(EnergyPlusData &state, bool &ErrorsFound)
{
    // Perform the GetInput function for the Output:IlluminanceMap
    // Glazer - June 2016 (moved from GetDaylightingControls)
    using DataStringGlobals::CharComma;
    using DataStringGlobals::CharSpace;
    using DataStringGlobals::CharTab;

    int MapNum;
    int IOStat;
    int NumAlpha;
    int NumNumber;
    int MapStyleIn;
    int AddMapPoints;
    int RefPt;
    int X;
    int Y;
    Real64 CosBldgRelNorth;         // Cosine of Building rotation
    Real64 SinBldgRelNorth;         // Sine of Building rotation
    Real64 CosZoneRelNorth;         // Cosine of Zone rotation
    Real64 SinZoneRelNorth;         // Sine of Zone rotation
    Real64 CosBldgRotAppGonly(0.0); // Cosine of the building rotation for appendix G only ( relative north )
    Real64 SinBldgRotAppGonly(0.0); // Sine of the building rotation for appendix G only ( relative north )
    Real64 Xb;                      // temp var for transformation calc
    Real64 Yb;                      // temp var for transformation calc
    Real64 Xo;
    Real64 XnoRot;
    Real64 Xtrans;
    Real64 Yo;
    Real64 YnoRot;
    Real64 Ytrans;
    bool doTransform;
    Real64 OldAspectRatio;
    Real64 NewAspectRatio;
    Array1D_bool ZoneMsgDone;

    auto &Zone(state.dataHeatBal->Zone);

    CosBldgRelNorth =
        std::cos(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
    SinBldgRelNorth =
        std::sin(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
    // these are only for Building Rotation for Appendix G when using world coordinate system
    CosBldgRotAppGonly = std::cos(-state.dataHeatBal->BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);
    SinBldgRotAppGonly = std::sin(-state.dataHeatBal->BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);

    doTransform = false;
    OldAspectRatio = 1.0;
    NewAspectRatio = 1.0;

    CheckForGeometricTransform(state, doTransform, OldAspectRatio, NewAspectRatio);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Output:IlluminanceMap";
    int TotIllumMaps = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    state.dataDaylightingData->IllumMap.allocate(TotIllumMaps);
    state.dataDaylightingData->IllumMapCalc.allocate(TotIllumMaps);

    if (TotIllumMaps > 0) {
        for (MapNum = 1; MapNum <= TotIllumMaps; ++MapNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     MapNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            state.dataDaylightingData->IllumMap(MapNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataDaylightingData->IllumMap(MapNum).zoneIndex = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), Zone);

            if (state.dataDaylightingData->IllumMap(MapNum).zoneIndex == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            } else {
                // set enclosure index for first space in zone
                int zoneNum = state.dataDaylightingData->IllumMap(MapNum).zoneIndex;
                int enclNum = state.dataHeatBal->space(state.dataHeatBal->Zone(zoneNum).spaceIndexes(1)).solarEnclosureNum;
                state.dataDaylightingData->IllumMap(MapNum).enclIndex = enclNum;
                // check that all spaces in the zone are in the same enclosure
                for (int spaceCounter = 2; spaceCounter <= state.dataHeatBal->Zone(zoneNum).numSpaces; ++spaceCounter) {
                    int spaceNum = state.dataHeatBal->Zone(zoneNum).spaceIndexes(spaceCounter);
                    if (enclNum != state.dataHeatBal->space(spaceNum).solarEnclosureNum) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                            "\" All spaces in the zone must be in the same enclosure for daylighting illuminance maps.");
                        ErrorsFound = true;
                        break;
                    }
                }
            }

            state.dataDaylightingData->IllumMapCalc(MapNum).zoneIndex = state.dataDaylightingData->IllumMap(MapNum).zoneIndex;
            state.dataDaylightingData->IllumMapCalc(MapNum).enclIndex = state.dataDaylightingData->IllumMap(MapNum).enclIndex;
            state.dataDaylightingData->IllumMap(MapNum).Z = state.dataIPShortCut->rNumericArgs(1);

            state.dataDaylightingData->IllumMap(MapNum).Xmin = state.dataIPShortCut->rNumericArgs(2);
            state.dataDaylightingData->IllumMap(MapNum).Xmax = state.dataIPShortCut->rNumericArgs(3);
            if (state.dataIPShortCut->rNumericArgs(2) > state.dataIPShortCut->rNumericArgs(3)) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state,
                                  format("...{} {:.2R} must be <= {} {:.2R}.",
                                         state.dataIPShortCut->cNumericFieldNames(2),
                                         state.dataIPShortCut->rNumericArgs(2),
                                         state.dataIPShortCut->cNumericFieldNames(3),
                                         state.dataIPShortCut->rNumericArgs(3)));
                ErrorsFound = true;
            }
            state.dataDaylightingData->IllumMap(MapNum).Xnum = state.dataIPShortCut->rNumericArgs(4);
            if (state.dataDaylightingData->IllumMap(MapNum).Xnum != 1) {
                state.dataDaylightingData->IllumMap(MapNum).Xinc =
                    (state.dataDaylightingData->IllumMap(MapNum).Xmax - state.dataDaylightingData->IllumMap(MapNum).Xmin) /
                    (state.dataDaylightingData->IllumMap(MapNum).Xnum - 1);
            } else {
                state.dataDaylightingData->IllumMap(MapNum).Xinc = 0.0;
            }

            state.dataDaylightingData->IllumMap(MapNum).Ymin = state.dataIPShortCut->rNumericArgs(5);
            state.dataDaylightingData->IllumMap(MapNum).Ymax = state.dataIPShortCut->rNumericArgs(6);
            if (state.dataIPShortCut->rNumericArgs(5) > state.dataIPShortCut->rNumericArgs(6)) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid entry.");
                ShowContinueError(state,
                                  format("...{} {:.2R} must be <= {} {:.2R}.",
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         state.dataIPShortCut->rNumericArgs(5),
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         state.dataIPShortCut->rNumericArgs(6)));
                ErrorsFound = true;
            }
            state.dataDaylightingData->IllumMap(MapNum).Ynum = state.dataIPShortCut->rNumericArgs(7);
            if (state.dataDaylightingData->IllumMap(MapNum).Ynum != 1) {
                state.dataDaylightingData->IllumMap(MapNum).Yinc =
                    (state.dataDaylightingData->IllumMap(MapNum).Ymax - state.dataDaylightingData->IllumMap(MapNum).Ymin) /
                    (state.dataDaylightingData->IllumMap(MapNum).Ynum - 1);
            } else {
                state.dataDaylightingData->IllumMap(MapNum).Yinc = 0.0;
            }
            if (state.dataDaylightingData->IllumMap(MapNum).Xnum * state.dataDaylightingData->IllumMap(MapNum).Ynum >
                DataDaylighting::MaxMapRefPoints) {
                ShowSevereError(state, cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", too many map points specified.");
                ShowContinueError(state,
                                  format("...{}[{}] * {}[{}].= [{}] must be <= [{}].",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         state.dataDaylightingData->IllumMap(MapNum).Xnum,
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         state.dataDaylightingData->IllumMap(MapNum).Ynum,
                                         state.dataDaylightingData->IllumMap(MapNum).Xnum * state.dataDaylightingData->IllumMap(MapNum).Ynum,
                                         DataDaylighting::MaxMapRefPoints));
                ErrorsFound = true;
            }
        } // MapNum
        cCurrentModuleObject = "OutputControl:IlluminanceMap:Style";
        MapStyleIn = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (MapStyleIn == 0) {
            state.dataIPShortCut->cAlphaArgs(1) = "COMMA";
            state.dataDaylightingData->MapColSep = CharComma; // comma
        } else if (MapStyleIn == 1) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     1,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlpha,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumber,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            if (state.dataIPShortCut->cAlphaArgs(1) == "COMMA") {
                state.dataDaylightingData->MapColSep = CharComma; // comma
            } else if (state.dataIPShortCut->cAlphaArgs(1) == "TAB") {
                state.dataDaylightingData->MapColSep = CharTab; // tab
            } else if (state.dataIPShortCut->cAlphaArgs(1) == "FIXED" || state.dataIPShortCut->cAlphaArgs(1) == "SPACE") {
                state.dataDaylightingData->MapColSep = CharSpace; // space
            } else {
                state.dataDaylightingData->MapColSep = CharComma; // comma
                ShowWarningError(state,
                                 cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", Commas will be used to separate fields.");
                state.dataIPShortCut->cAlphaArgs(1) = "COMMA";
            }
        }
        print(state.files.eio, "! <Daylighting:Illuminance Maps>,#Maps,Style\n");
        ConvertCaseToLower(state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cAlphaArgs(2));
        state.dataIPShortCut->cAlphaArgs(1).erase(1);
        state.dataIPShortCut->cAlphaArgs(1) += state.dataIPShortCut->cAlphaArgs(2).substr(1);
        print(state.files.eio, "Daylighting:Illuminance Maps,{},{}\n", TotIllumMaps, state.dataIPShortCut->cAlphaArgs(1));
    }

    // Check for illuminance maps associated with this zone
    for (MapNum = 1; MapNum <= TotIllumMaps; ++MapNum) {
        if (state.dataDaylightingData->IllumMap(MapNum).zoneIndex > 0) {
            auto &zone(Zone(state.dataDaylightingData->IllumMap(MapNum).zoneIndex));
            // Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
            CosZoneRelNorth = std::cos(-zone.RelNorth * DataGlobalConstants::DegToRadians);
            SinZoneRelNorth = std::sin(-zone.RelNorth * DataGlobalConstants::DegToRadians);
            if (state.dataDaylightingData->IllumMap(MapNum).Xnum * state.dataDaylightingData->IllumMap(MapNum).Ynum > 0) {
                // Add additional daylighting reference points for map
                AddMapPoints = state.dataDaylightingData->IllumMap(MapNum).Xnum * state.dataDaylightingData->IllumMap(MapNum).Ynum;
                state.dataDaylightingData->IllumMapCalc(MapNum).TotalMapRefPoints = AddMapPoints;
                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord.allocate(3, AddMapPoints);
                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord = 0.0;
                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds.allocate(AddMapPoints);
                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds = true;
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPt.allocate(AddMapPoints);
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPt = 0.0;
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPtHr.allocate(AddMapPoints);
                state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPtHr = 0.0;

                if (AddMapPoints > DataDaylighting::MaxMapRefPoints) {
                    ShowSevereError(state, "GetDaylighting Parameters: Total Map Reference points entered is greater than maximum allowed.");
                    ShowContinueError(state, "Occurs in Zone=" + zone.Name);
                    ShowContinueError(state,
                                      format("Maximum reference points allowed={}, entered amount ( when error first occurred )={}",
                                             DataDaylighting::MaxMapRefPoints,
                                             AddMapPoints));
                    ErrorsFound = true;
                    break;
                }
                RefPt = 1;
                // Calc cos and sin of Zone Relative North values for later use in transforming Map Point coordinates
                // CosZoneRelNorth = std::cos( -zone.RelNorth * DegToRadians ); //Tuned These should not be changing
                // SinZoneRelNorth = std::sin( -zone.RelNorth * DegToRadians );
                if (state.dataDaylightingData->IllumMap(MapNum).Xnum != 1) {
                    state.dataDaylightingData->IllumMap(MapNum).Xinc =
                        (state.dataDaylightingData->IllumMap(MapNum).Xmax - state.dataDaylightingData->IllumMap(MapNum).Xmin) /
                        (state.dataDaylightingData->IllumMap(MapNum).Xnum - 1);
                } else {
                    state.dataDaylightingData->IllumMap(MapNum).Xinc = 0.0;
                }
                if (state.dataDaylightingData->IllumMap(MapNum).Ynum != 1) {
                    state.dataDaylightingData->IllumMap(MapNum).Yinc =
                        (state.dataDaylightingData->IllumMap(MapNum).Ymax - state.dataDaylightingData->IllumMap(MapNum).Ymin) /
                        (state.dataDaylightingData->IllumMap(MapNum).Ynum - 1);
                } else {
                    state.dataDaylightingData->IllumMap(MapNum).Yinc = 0.0;
                }

                // Map points and increments are stored in AbsCoord and then that is operated on if relative coords entered.
                for (Y = 1; Y <= state.dataDaylightingData->IllumMap(MapNum).Ynum; ++Y) {
                    for (X = 1; X <= state.dataDaylightingData->IllumMap(MapNum).Xnum; ++X) {
                        state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) =
                            state.dataDaylightingData->IllumMap(MapNum).Xmin + (X - 1) * state.dataDaylightingData->IllumMap(MapNum).Xinc;
                        state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) =
                            state.dataDaylightingData->IllumMap(MapNum).Ymin + (Y - 1) * state.dataDaylightingData->IllumMap(MapNum).Yinc;
                        state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) = state.dataDaylightingData->IllumMap(MapNum).Z;
                        ++RefPt;
                    }
                }
                RefPt = 1;
                for (Y = 1; Y <= state.dataDaylightingData->IllumMap(MapNum).Ynum; ++Y) {
                    for (X = 1; X <= state.dataDaylightingData->IllumMap(MapNum).Xnum; ++X) {
                        if (!state.dataSurface->DaylRefWorldCoordSystem) {
                            Xb = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) * CosZoneRelNorth -
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) * SinZoneRelNorth + zone.OriginX;
                            Yb = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) * SinZoneRelNorth +
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) * CosZoneRelNorth + zone.OriginY;
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) += zone.OriginZ;
                            if (doTransform) {
                                Xo = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(
                                    1, RefPt); // world coordinates.... shifted by relative north angle...
                                Yo = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt);
                                // next derotate the building
                                XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
                                YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
                                // translate
                                Xtrans = XnoRot * std::sqrt(NewAspectRatio / OldAspectRatio);
                                Ytrans = YnoRot * std::sqrt(OldAspectRatio / NewAspectRatio);
                                // rerotate
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) =
                                    Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) =
                                    Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
                            }
                        } else {
                            Xb = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt);
                            Yb = state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt);
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) =
                                Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) =
                                Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
                        }
                        if (RefPt == 1) {
                            state.dataDaylightingData->IllumMap(MapNum).Xmin =
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt);
                            state.dataDaylightingData->IllumMap(MapNum).Ymin =
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt);
                            state.dataDaylightingData->IllumMap(MapNum).Xmax =
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt);
                            state.dataDaylightingData->IllumMap(MapNum).Ymax =
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt);
                            state.dataDaylightingData->IllumMap(MapNum).Z =
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt);
                        }
                        state.dataDaylightingData->IllumMap(MapNum).Xmin =
                            min(state.dataDaylightingData->IllumMap(MapNum).Xmin,
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt));
                        state.dataDaylightingData->IllumMap(MapNum).Ymin =
                            min(state.dataDaylightingData->IllumMap(MapNum).Ymin,
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt));
                        state.dataDaylightingData->IllumMap(MapNum).Xmax =
                            max(state.dataDaylightingData->IllumMap(MapNum).Xmax,
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt));
                        state.dataDaylightingData->IllumMap(MapNum).Ymax =
                            max(state.dataDaylightingData->IllumMap(MapNum).Ymax,
                                state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt));
                        if ((state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) < zone.MinimumX &&
                             (zone.MinimumX - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt)) > 0.001) ||
                            (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) > zone.MaximumX &&
                             (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) - zone.MaximumX) > 0.001) ||
                            (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) < zone.MinimumY &&
                             (zone.MinimumY - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt)) > 0.001) ||
                            (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) > zone.MaximumY &&
                             (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) - zone.MaximumY) > 0.001) ||
                            (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) < zone.MinimumZ &&
                             (zone.MinimumZ - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt)) > 0.001) ||
                            (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) > zone.MaximumZ &&
                             (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) - zone.MaximumZ) > 0.001)) {
                            state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(RefPt) = false;
                        }
                        // Test extremes of Map Points against Zone Min/Max
                        if (RefPt == 1 || RefPt == state.dataDaylightingData->IllumMapCalc(MapNum).TotalMapRefPoints) {
                            if ((state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) < zone.MinimumX ||
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) > zone.MaximumX) &&
                                !state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(RefPt)) {
                                ShowWarningError(state,
                                                 format("GetInputIlluminanceMap: Reference Map point #[{}], X Value outside Zone Min/Max X, Zone={}",
                                                        RefPt,
                                                        zone.Name));
                                ShowContinueError(state,
                                                  format("...X Reference Point= {:.2R}, Zone Minimum X= {:.2R}, Zone Maximum X= {:.2R}",
                                                         state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt),
                                                         zone.MinimumX,
                                                         zone.MaximumX));
                                if (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) < zone.MinimumX) {
                                    ShowContinueError(
                                        state,
                                        format("...X Reference Distance Outside MinimumX= {:.4R} m.",
                                               zone.MinimumX - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt)));
                                } else {
                                    ShowContinueError(
                                        state,
                                        format("...X Reference Distance Outside MaximumX= {:.4R} m.",
                                               state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt) - zone.MaximumX));
                                }
                            }
                            if ((state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) < zone.MinimumY ||
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) > zone.MaximumY) &&
                                !state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(RefPt)) {
                                ShowWarningError(state,
                                                 format("GetInputIlluminanceMap: Reference Map point #[{}], Y Value outside Zone Min/Max Y, Zone={}",
                                                        RefPt,
                                                        zone.Name));
                                ShowContinueError(state,
                                                  format("...Y Reference Point= {:.2R}, Zone Minimum Y= {:.2R}, Zone Maximum Y= {:.2R}",
                                                         state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt),
                                                         zone.MinimumY,
                                                         zone.MaximumY));
                                if (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) < zone.MinimumY) {
                                    ShowContinueError(
                                        state,
                                        format("...Y Reference Distance Outside MinimumY= {:.4R} m.",
                                               zone.MinimumY - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt)));
                                } else {
                                    ShowContinueError(
                                        state,
                                        format("...Y Reference Distance Outside MaximumY= {:.4R} m.",
                                               state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt) - zone.MaximumY));
                                }
                            }
                            if ((state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) < zone.MinimumZ ||
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) > zone.MaximumZ) &&
                                !state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(RefPt)) {
                                ShowWarningError(state,
                                                 format("GetInputIlluminanceMap: Reference Map point #[{}], Z Value outside Zone Min/Max Z, Zone={}",
                                                        RefPt,
                                                        zone.Name));
                                ShowContinueError(state,
                                                  format("...Z Reference Point= {:.2R}, Zone Minimum Z= {:.2R}, Zone Maximum Z= {:.2R}",
                                                         state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt),
                                                         zone.MinimumZ,
                                                         zone.MaximumZ));
                                if (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) < zone.MinimumZ) {
                                    ShowContinueError(
                                        state,
                                        format("...Z Reference Distance Outside MinimumZ= {:.4R} m.",
                                               zone.MinimumZ - state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt)));
                                } else {
                                    ShowContinueError(
                                        state,
                                        format("...Z Reference Distance Outside MaximumZ= {:.4R} m.",
                                               state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(3, RefPt) - zone.MaximumZ));
                                }
                            }
                        }
                        ++RefPt;
                    } // X
                }     // Y
            }
        }
    } // MapNum
    ZoneMsgDone.dimension(state.dataGlobal->NumOfZones, false);
    for (MapNum = 1; MapNum <= TotIllumMaps; ++MapNum) {
        if (state.dataDaylightingData->IllumMap(MapNum).zoneIndex == 0) continue;
        int enclNum = state.dataDaylightingData->IllumMap(MapNum).enclIndex;
        if (!state.dataDaylightingData->enclDaylight(enclNum).hasSplitFluxDaylighting &&
            !ZoneMsgDone(state.dataDaylightingData->IllumMap(MapNum).zoneIndex)) {
            ShowSevereError(state,
                            "Zone Name in Output:IlluminanceMap is not used for Daylighting:Controls=" +
                                Zone(state.dataDaylightingData->IllumMap(MapNum).zoneIndex).Name);
            ErrorsFound = true;
        }
    }
    ZoneMsgDone.deallocate();

    if (TotIllumMaps > 0) {
        print(state.files.eio,
              "! <Daylighting:Illuminance Maps:Detail>,Name,Zone,XMin {{m}},XMax {{m}},Xinc {{m}},#X Points,YMin "
              "{{m}},YMax {{m}},Yinc {{m}},#Y Points,Z {{m}}\n");
    }
    for (MapNum = 1; MapNum <= TotIllumMaps; ++MapNum) {
        print(state.files.eio,
              "Daylighting:Illuminance Maps:Detail,{},{},{:.2R},{:.2R},{:.2R},{},{:.2R},{:.2R},{:.2R},{},{:.2R}\n",
              state.dataDaylightingData->IllumMap(MapNum).Name,
              Zone(state.dataDaylightingData->IllumMap(MapNum).zoneIndex).Name,
              state.dataDaylightingData->IllumMap(MapNum).Xmin,
              state.dataDaylightingData->IllumMap(MapNum).Xmax,
              state.dataDaylightingData->IllumMap(MapNum).Xinc,
              state.dataDaylightingData->IllumMap(MapNum).Xnum,
              state.dataDaylightingData->IllumMap(MapNum).Ymin,
              state.dataDaylightingData->IllumMap(MapNum).Ymax,
              state.dataDaylightingData->IllumMap(MapNum).Yinc,
              state.dataDaylightingData->IllumMap(MapNum).Ynum,
              state.dataDaylightingData->IllumMap(MapNum).Z);
    }

    if (ErrorsFound) return;
}

void GetDaylightingControls(EnergyPlusData &state, bool &ErrorsFound)
{
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   March 2002
    //       MODIFIED       Glazer - July 2016 - Move geometry transformation portion, rearrange input, allow more than three reference points
    // Obtain the user input data for Daylighting:Controls object in the input file.

    int IOStat;
    int NumAlpha;
    int NumNumber;

    // Smallest deviation from unity for the sum of all fractions
    // Accept approx 4 to 8 ULP error (technically abs(1.0 + sumFracs) should be close to 2)
    //   constexpr Real64 FractionTolerance(4 * std::numeric_limits<Real64>::epsilon());
    // Instead, we use a 0.001 = 0.1% tolerance
    constexpr Real64 FractionTolerance(0.001);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Daylighting:Controls";
    int totDaylightingControls = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataDaylightingData->daylightControl.allocate(totDaylightingControls);
    Array1D<bool> spaceHasDaylightingControl;
    spaceHasDaylightingControl.dimension(state.dataGlobal->numSpaces, false);
    // Reset to zero in case this is called more than once in unit tests
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints = 0;
    }
    for (int controlNum = 1; controlNum <= totDaylightingControls; ++controlNum) {
        state.dataIPShortCut->cAlphaArgs = "";
        state.dataIPShortCut->rNumericArgs = 0.0;
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 controlNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlpha,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        auto &daylightControl(state.dataDaylightingData->daylightControl(controlNum));
        daylightControl.Name = state.dataIPShortCut->cAlphaArgs(1);

        // Is it a space or zone name?
        int const spaceNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->space);
        if (spaceNum > 0) {
            daylightControl.spaceIndex = spaceNum;
            daylightControl.zoneIndex = state.dataHeatBal->space(spaceNum).zoneNum;
            daylightControl.enclIndex = state.dataHeatBal->space(spaceNum).solarEnclosureNum;
            // Check if this is a duplicate
            if (spaceHasDaylightingControl(spaceNum)) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + daylightControl.Name + "\" Space=" + "=\"" + state.dataHeatBal->space(spaceNum).Name +
                                    "\" already has a " + cCurrentModuleObject + " object assigned to it. Only one per Space is allowed.");
                ErrorsFound = true;
                continue;
            }
        } else {
            int const zoneNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
            if (zoneNum == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ErrorsFound = true;
                continue;
            } else {
                daylightControl.zoneIndex = zoneNum;

                // set enclosure index for first space in zone
                int enclNum = state.dataHeatBal->space(state.dataHeatBal->Zone(zoneNum).spaceIndexes(1)).solarEnclosureNum;
                daylightControl.enclIndex = enclNum;
                // check that all spaces in the zone are in the same enclosure
                for (int spaceCounter = 2; spaceCounter <= state.dataHeatBal->Zone(zoneNum).numSpaces; ++spaceCounter) {
                    int zoneSpaceNum = state.dataHeatBal->Zone(zoneNum).spaceIndexes(spaceCounter);
                    if (daylightControl.enclIndex != state.dataHeatBal->space(zoneSpaceNum).solarEnclosureNum) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" +
                                            state.dataIPShortCut->cAlphaArgs(2) +
                                            "\" All spaces in the zone must be in the same enclosure for daylighting.");
                        ErrorsFound = true;
                        break;
                    }
                    // Check if this is a duplicate
                    if (spaceHasDaylightingControl(zoneSpaceNum)) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + "=\"" + daylightControl.Name + "\" Space=" + "=\"" +
                                            state.dataHeatBal->space(zoneSpaceNum).Name + "\" already has a " + cCurrentModuleObject +
                                            " object assigned to it. Only one per Space is allowed.");
                        ErrorsFound = true;
                        continue;
                    }
                }
            }
        }

        state.dataDaylightingData->enclDaylight(daylightControl.enclIndex).daylightControlIndexes.emplace_back(controlNum);
        daylightControl.ZoneName = state.dataHeatBal->Zone(daylightControl.zoneIndex).Name;

        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "SPLITFLUX")) { // Field: Daylighting Method
            daylightControl.DaylightMethod = DataDaylighting::DaylightingMethod::SplitFlux;
            state.dataDaylightingData->enclDaylight(daylightControl.enclIndex).hasSplitFluxDaylighting = true;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3), "DELIGHT")) {
            daylightControl.DaylightMethod = DataDaylighting::DaylightingMethod::DElight;
        } else if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            daylightControl.DaylightMethod = DataDaylighting::DaylightingMethod::SplitFlux;
            state.dataDaylightingData->enclDaylight(daylightControl.enclIndex).hasSplitFluxDaylighting = true;
        } else {
            ShowWarningError(state,
                             "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + " = " + state.dataIPShortCut->cAlphaArgs(3) + ", occurs in " +
                                 cCurrentModuleObject + "object for " + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "SplitFlux assumed, and the simulation continues.");
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) { // Field: Availability Schedule Name
            daylightControl.AvailSchedNum = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (daylightControl.AvailSchedNum == 0) {
                ShowWarningError(state,
                                 "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + " = " + state.dataIPShortCut->cAlphaArgs(4) +
                                     ", occurs in " + cCurrentModuleObject + "object for " + cCurrentModuleObject + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Schedule was not found so controls will always be available, and the simulation continues.");
                daylightControl.AvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
            }
        } else {
            daylightControl.AvailSchedNum = DataGlobalConstants::ScheduleAlwaysOn;
        }

        int typeNum = getEnumerationValue(DataDaylighting::LtgCtrlTypeNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(5)));
        daylightControl.LightControlType = static_cast<DataDaylighting::LtgCtrlType>(typeNum);
        if (daylightControl.LightControlType == DataDaylighting::LtgCtrlType::Invalid) {
            ShowWarningError(state,
                             "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + " = " + state.dataIPShortCut->cAlphaArgs(5) + ", occurs in " +
                                 cCurrentModuleObject + "object for " + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "Continuous assumed, and the simulation continues.");
        }

        daylightControl.MinPowerFraction =
            state.dataIPShortCut->rNumericArgs(1); // Field: Minimum Input Power Fraction for Continuous Dimming Control
        daylightControl.MinLightFraction =
            state.dataIPShortCut->rNumericArgs(2); // Field: Minimum Light Output Fraction for Continuous Dimming Control
        daylightControl.LightControlSteps = state.dataIPShortCut->rNumericArgs(3); // Field: Number of Stepped Control Steps
        daylightControl.LightControlProbability =
            state.dataIPShortCut->rNumericArgs(4); // Field: Probability Lighting will be Reset When Needed in Manual Stepped Control

        if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) { // Field: Glare Calculation Daylighting Reference Point Name
            daylightControl.glareRefPtNumber =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(6),
                                                state.dataDaylightingData->DaylRefPt,
                                                &DataDaylighting::RefPointData::Name); // Field: Glare Calculation Daylighting Reference Point Name
            if (daylightControl.glareRefPtNumber == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(6) + "\" for object named: " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
                continue;
            }
        } else if (daylightControl.DaylightMethod == DataDaylighting::DaylightingMethod::SplitFlux) {
            ShowWarningError(
                state, "No " + state.dataIPShortCut->cAlphaFieldNames(6) + " provided for object named: " + state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(state, "No glare calculation performed, and the simulation continues.");
        }

        if (!state.dataIPShortCut->lNumericFieldBlanks(5)) {
            daylightControl.ViewAzimuthForGlare =
                state.dataIPShortCut->rNumericArgs(5); // Field: Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis
        } else {
            daylightControl.ViewAzimuthForGlare = 0.;
        }

        daylightControl.MaxGlareallowed = state.dataIPShortCut->rNumericArgs(6);           // Field: Maximum Allowable Discomfort Glare Index
        daylightControl.DElightGriddingResolution = state.dataIPShortCut->rNumericArgs(7); // Field: DElight Gridding Resolution

        int curTotalDaylRefPts = NumAlpha - 6; // first six alpha fields are not part of extensible group
        daylightControl.TotalDaylRefPoints = curTotalDaylRefPts;
        state.dataViewFactor->EnclSolInfo(daylightControl.enclIndex).TotalEnclosureDaylRefPoints += curTotalDaylRefPts;
        state.dataDaylightingData->ZoneDaylight(daylightControl.zoneIndex).totRefPts += curTotalDaylRefPts;
        state.dataDaylightingData->maxRefPointsPerControl = max(state.dataDaylightingData->maxRefPointsPerControl, curTotalDaylRefPts);
        if ((NumNumber - 7) / 2 != daylightControl.TotalDaylRefPoints) {
            ShowSevereError(state,
                            cCurrentModuleObject + "The number of extensible numeric fields and alpha fields is inconsistent for: " +
                                state.dataIPShortCut->cAlphaArgs(1));
            ShowContinueError(
                state,
                "For each field: " + state.dataIPShortCut->cAlphaFieldNames(NumAlpha) +
                    " there needs to be the following fields: Fraction Controlled by Reference Point and Illuminance Setpoint at Reference Point");
            ErrorsFound = true;
        }
        daylightControl.DaylRefPtNum.allocate(curTotalDaylRefPts);
        daylightControl.FracZoneDaylit.allocate(curTotalDaylRefPts);
        daylightControl.IllumSetPoint.allocate(curTotalDaylRefPts);
        daylightControl.DaylIllumAtRefPt.allocate(curTotalDaylRefPts);
        daylightControl.GlareIndexAtRefPt.allocate(curTotalDaylRefPts);
        daylightControl.DaylRefPtAbsCoord.allocate(3, curTotalDaylRefPts);
        daylightControl.DaylRefPtInBounds.allocate(curTotalDaylRefPts);
        daylightControl.RefPtPowerReductionFactor.allocate(curTotalDaylRefPts);
        daylightControl.BacLum.allocate(curTotalDaylRefPts);
        daylightControl.TimeExceedingGlareIndexSPAtRefPt.allocate(curTotalDaylRefPts);
        daylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt.allocate(curTotalDaylRefPts);

        for (int refPt = 1; refPt <= curTotalDaylRefPts; ++refPt) {
            daylightControl.DaylRefPtNum(refPt) = 0;
            daylightControl.FracZoneDaylit(refPt) = 0.0;
            daylightControl.IllumSetPoint(refPt) = 0.0;
            daylightControl.DaylIllumAtRefPt(refPt) = 0.0;
            daylightControl.GlareIndexAtRefPt(refPt) = 0.0;
            daylightControl.DaylRefPtInBounds(refPt) = true;
            daylightControl.RefPtPowerReductionFactor(refPt) = 1.0;
            daylightControl.BacLum(refPt) = 0.0;
            daylightControl.TimeExceedingGlareIndexSPAtRefPt(refPt) = 0.0;
            daylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt(refPt) = 0.0;
            for (int coord = 1; coord <= 3; ++coord) {
                daylightControl.DaylRefPtAbsCoord(coord, refPt) = 0.0;
            }
        }

        int countRefPts = 0;
        for (int refPtNum = 1; refPtNum <= curTotalDaylRefPts; ++refPtNum) {
            daylightControl.DaylRefPtNum(refPtNum) =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(6 + refPtNum),
                                                state.dataDaylightingData->DaylRefPt,
                                                &DataDaylighting::RefPointData::Name); // Field: Daylighting Reference Point Name
            if (daylightControl.DaylRefPtNum(refPtNum) == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(6 + refPtNum) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(6 + refPtNum) + "\" for object named: " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
                continue;
            } else {
                ++countRefPts;
            }
            daylightControl.FracZoneDaylit(refPtNum) =
                state.dataIPShortCut->rNumericArgs(6 + refPtNum * 2); // Field: Fraction Controlled by Reference Point
            daylightControl.IllumSetPoint(refPtNum) =
                state.dataIPShortCut->rNumericArgs(7 + refPtNum * 2); // Field: Illuminance Setpoint at Reference Point

            if (daylightControl.DaylightMethod == DataDaylighting::DaylightingMethod::SplitFlux) {
                SetupOutputVariable(state,
                                    format("Daylighting Reference Point {} Illuminance", refPtNum),
                                    OutputProcessor::Unit::lux,
                                    daylightControl.DaylIllumAtRefPt(refPtNum),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    daylightControl.Name);
                SetupOutputVariable(state,
                                    format("Daylighting Reference Point {} Daylight Illuminance Setpoint Exceeded Time", refPtNum),
                                    OutputProcessor::Unit::hr,
                                    daylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt(refPtNum),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    daylightControl.Name);
                SetupOutputVariable(state,
                                    format("Daylighting Reference Point {} Glare Index", refPtNum),
                                    OutputProcessor::Unit::None,
                                    daylightControl.GlareIndexAtRefPt(refPtNum),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Average,
                                    daylightControl.Name);
                SetupOutputVariable(state,
                                    format("Daylighting Reference Point {} Glare Index Setpoint Exceeded Time", refPtNum),
                                    OutputProcessor::Unit::hr,
                                    daylightControl.TimeExceedingGlareIndexSPAtRefPt(refPtNum),
                                    OutputProcessor::SOVTimeStepType::Zone,
                                    OutputProcessor::SOVStoreType::Summed,
                                    daylightControl.Name);
            }
        }
        // Register Error if 0 DElight RefPts have been input for valid DElight object
        if (countRefPts < 1) {
            ShowSevereError(state, "No Reference Points input for " + cCurrentModuleObject + " zone =" + daylightControl.ZoneName);
            ErrorsFound = true;
        }

        Real64 sumFracs = sum(daylightControl.FracZoneDaylit);
        daylightControl.sumFracLights = sumFracs;
        if ((1.0 - sumFracs) > FractionTolerance) {
            ShowWarningError(state, "GetDaylightingControls: Fraction of zone or space controlled by the Daylighting reference points is < 1.0.");
            ShowContinueError(state,
                              format("..discovered in {}=\"{}\", only {:.3R} of the zone or space is controlled.",
                                     cCurrentModuleObject,
                                     daylightControl.Name,
                                     sum(daylightControl.FracZoneDaylit)));
        } else if ((sumFracs - 1.0) > FractionTolerance) {
            ShowSevereError(state, "GetDaylightingControls: Fraction of zone or space controlled by the Daylighting reference points is > 1.0.");
            ShowContinueError(state,
                              format("..discovered in {}=\"{}\", trying to control {:.3R} of the zone or space.",
                                     cCurrentModuleObject,
                                     daylightControl.Name,
                                     sum(daylightControl.FracZoneDaylit)));
            ErrorsFound = true;
        }

        if (daylightControl.LightControlType == DataDaylighting::LtgCtrlType::Stepped && daylightControl.LightControlSteps <= 0) {
            ShowWarningError(state, "GetDaylightingControls: For Stepped Control, the number of steps must be > 0");
            ShowContinueError(
                state, "..discovered in \"" + cCurrentModuleObject + "\" for Zone=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\", will use 1");
            daylightControl.LightControlSteps = 1;
        }
        SetupOutputVariable(state,
                            "Daylighting Lighting Power Multiplier",
                            OutputProcessor::Unit::None,
                            daylightControl.PowerReductionFactor,
                            OutputProcessor::SOVTimeStepType::Zone,
                            OutputProcessor::SOVStoreType::Average,
                            daylightControl.Name);
    }
}

void GeometryTransformForDaylighting(EnergyPlusData &state)
{
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   March 2002
    //       MODIFIED       Glazer - July 2016 - separated this from GetInput function
    // For splitflux daylighting, transform the geometry

    using InternalHeatGains::CheckLightsReplaceableMinMaxForZone;
    using InternalHeatGains::GetDesignLightingLevelForZone;
    using namespace OutputReportPredefined;
    using ScheduleManager::GetScheduleIndex;

    int refPtNum;
    std::string refName;
    Real64 CosBldgRelNorth;         // Cosine of Building rotation
    Real64 SinBldgRelNorth;         // Sine of Building rotation
    Real64 CosZoneRelNorth;         // Cosine of Zone rotation
    Real64 SinZoneRelNorth;         // Sine of Zone rotation
    Real64 CosBldgRotAppGonly(0.0); // Cosine of the building rotation for appendix G only ( relative north )
    Real64 SinBldgRotAppGonly(0.0); // Sine of the building rotation for appendix G only ( relative north )
    Real64 Xb;                      // temp var for transformation calc
    Real64 Yb;                      // temp var for transformation calc
    Real64 Xo;
    Real64 XnoRot;
    Real64 Xtrans;
    Real64 Yo;
    Real64 YnoRot;
    Real64 Ytrans;
    bool doTransform;
    Real64 OldAspectRatio;
    Real64 NewAspectRatio;
    Real64 rLightLevel;

    // Calc cos and sin of Building Relative North values for later use in transforming Reference Point coordinates
    CosBldgRelNorth =
        std::cos(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
    SinBldgRelNorth =
        std::sin(-(state.dataHeatBal->BuildingAzimuth + state.dataHeatBal->BuildingRotationAppendixG) * DataGlobalConstants::DegToRadians);
    // these are only for Building Rotation for Appendix G when using world coordinate system
    CosBldgRotAppGonly = std::cos(-state.dataHeatBal->BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);
    SinBldgRotAppGonly = std::sin(-state.dataHeatBal->BuildingRotationAppendixG * DataGlobalConstants::DegToRadians);

    doTransform = false;
    OldAspectRatio = 1.0;
    NewAspectRatio = 1.0;

    CheckForGeometricTransform(state, doTransform, OldAspectRatio, NewAspectRatio);
    for (int controlNum = 1; controlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++controlNum) {
        auto &daylCntrl = state.dataDaylightingData->daylightControl(controlNum);
        auto &zone(state.dataHeatBal->Zone(daylCntrl.zoneIndex));

        // Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
        CosZoneRelNorth = std::cos(-zone.RelNorth * DataGlobalConstants::DegToRadians);
        SinZoneRelNorth = std::sin(-zone.RelNorth * DataGlobalConstants::DegToRadians);

        rLightLevel = GetDesignLightingLevelForZone(state, daylCntrl.zoneIndex);
        CheckLightsReplaceableMinMaxForZone(state, daylCntrl.zoneIndex);

        for (refPtNum = 1; refPtNum <= daylCntrl.TotalDaylRefPoints; ++refPtNum) {
            auto &curRefPt(state.dataDaylightingData->DaylRefPt(daylCntrl.DaylRefPtNum(refPtNum))); // get the active daylighting:referencepoint
            curRefPt.indexToFracAndIllum = refPtNum; // back reference to the index to the ZoneDaylight structure arrays related to reference points
            if (state.dataSurface->DaylRefWorldCoordSystem) {
                // transform only by appendix G rotation
                daylCntrl.DaylRefPtAbsCoord(1, refPtNum) = curRefPt.x * CosBldgRotAppGonly - curRefPt.y * SinBldgRotAppGonly;
                daylCntrl.DaylRefPtAbsCoord(2, refPtNum) = curRefPt.x * SinBldgRotAppGonly + curRefPt.y * CosBldgRotAppGonly;
                daylCntrl.DaylRefPtAbsCoord(3, refPtNum) = curRefPt.z;
            } else {
                // Transform reference point coordinates into building coordinate system
                Xb = curRefPt.x * CosZoneRelNorth - curRefPt.y * SinZoneRelNorth + zone.OriginX;
                Yb = curRefPt.x * SinZoneRelNorth + curRefPt.y * CosZoneRelNorth + zone.OriginY;
                // Transform into World Coordinate System
                daylCntrl.DaylRefPtAbsCoord(1, refPtNum) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
                daylCntrl.DaylRefPtAbsCoord(2, refPtNum) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
                daylCntrl.DaylRefPtAbsCoord(3, refPtNum) = curRefPt.z + zone.OriginZ;
                if (doTransform) {
                    Xo = daylCntrl.DaylRefPtAbsCoord(1, refPtNum); // world coordinates.... shifted by relative north angle...
                    Yo = daylCntrl.DaylRefPtAbsCoord(2, refPtNum);
                    // next derotate the building
                    XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
                    YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
                    // translate
                    Xtrans = XnoRot * std::sqrt(NewAspectRatio / OldAspectRatio);
                    Ytrans = YnoRot * std::sqrt(OldAspectRatio / NewAspectRatio);
                    // rerotate
                    daylCntrl.DaylRefPtAbsCoord(1, refPtNum) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;
                    daylCntrl.DaylRefPtAbsCoord(2, refPtNum) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
                }
            }
            refName = curRefPt.Name;
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtZone, refName, daylCntrl.ZoneName);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtCtrlName, refName, daylCntrl.Name);
            if (daylCntrl.DaylightMethod == DataDaylighting::DaylightingMethod::SplitFlux) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtKind, refName, "SplitFlux");
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtKind, refName, "DElight");
            }
            // ( 1=continuous, 2=stepped, 3=continuous/off )
            if (daylCntrl.LightControlType == DataDaylighting::LtgCtrlType::Continuous) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtCtrlType, refName, "Continuous");
            } else if (daylCntrl.LightControlType == DataDaylighting::LtgCtrlType::Stepped) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtCtrlType, refName, "Stepped");
            } else if (daylCntrl.LightControlType == DataDaylighting::LtgCtrlType::ContinuousOff) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtCtrlType, refName, "Continuous/Off");
            }
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtFrac, refName, daylCntrl.FracZoneDaylit(refPtNum));
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtWInst, refName, rLightLevel);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchDyLtWCtrl, refName, rLightLevel * daylCntrl.FracZoneDaylit(refPtNum));

            if (daylCntrl.DaylRefPtAbsCoord(1, refPtNum) < zone.MinimumX || daylCntrl.DaylRefPtAbsCoord(1, refPtNum) > zone.MaximumX) {
                daylCntrl.DaylRefPtInBounds(refPtNum) = false;
                ShowWarningError(state, "GeometryTransformForDaylighting: Reference point X Value outside Zone Min/Max X, Zone=" + zone.Name);
                ShowContinueError(state,
                                  format("...X Reference Point= {:.2R}, Zone Minimum X= {:.2R}, Zone Maximum X= {:.2R}",
                                         daylCntrl.DaylRefPtAbsCoord(1, refPtNum),
                                         zone.MinimumX,
                                         zone.MaximumX));
                if (daylCntrl.DaylRefPtAbsCoord(1, refPtNum) < zone.MinimumX) {
                    ShowContinueError(
                        state,
                        format("...X Reference Distance Outside MinimumX= {:.4R} m.", zone.MinimumX - daylCntrl.DaylRefPtAbsCoord(1, refPtNum)));
                } else {
                    ShowContinueError(
                        state,
                        format("...X Reference Distance Outside MaximumX= {:.4R} m.", daylCntrl.DaylRefPtAbsCoord(1, refPtNum) - zone.MaximumX));
                }
            }
            if (daylCntrl.DaylRefPtAbsCoord(2, refPtNum) < zone.MinimumY || daylCntrl.DaylRefPtAbsCoord(2, refPtNum) > zone.MaximumY) {
                daylCntrl.DaylRefPtInBounds(refPtNum) = false;
                ShowWarningError(state, "GeometryTransformForDaylighting: Reference point Y Value outside Zone Min/Max Y, Zone=" + zone.Name);
                ShowContinueError(state,
                                  format("...Y Reference Point= {:.2R}, Zone Minimum Y= {:.2R}, Zone Maximum Y= {:.2R}",
                                         daylCntrl.DaylRefPtAbsCoord(2, refPtNum),
                                         zone.MinimumY,
                                         zone.MaximumY));
                if (daylCntrl.DaylRefPtAbsCoord(2, refPtNum) < zone.MinimumY) {
                    ShowContinueError(
                        state,
                        format("...Y Reference Distance Outside MinimumY= {:.4R} m.", zone.MinimumY - daylCntrl.DaylRefPtAbsCoord(2, refPtNum)));
                } else {
                    ShowContinueError(
                        state,
                        format("...Y Reference Distance Outside MaximumY= {:.4R} m.", daylCntrl.DaylRefPtAbsCoord(2, refPtNum) - zone.MaximumY));
                }
            }
            if (daylCntrl.DaylRefPtAbsCoord(3, refPtNum) < zone.MinimumZ || daylCntrl.DaylRefPtAbsCoord(3, refPtNum) > zone.MaximumZ) {
                daylCntrl.DaylRefPtInBounds(refPtNum) = false;
                ShowWarningError(state, "GeometryTransformForDaylighting: Reference point Z Value outside Zone Min/Max Z, Zone=" + zone.Name);
                ShowContinueError(state,
                                  format("...Z Reference Point= {:.2R}, Zone Minimum Z= {:.2R}, Zone Maximum Z= {:.2R}",
                                         daylCntrl.DaylRefPtAbsCoord(3, refPtNum),
                                         zone.MinimumZ,
                                         zone.MaximumZ));
                if (daylCntrl.DaylRefPtAbsCoord(3, refPtNum) < zone.MinimumZ) {
                    ShowContinueError(
                        state,
                        format("...Z Reference Distance Outside MinimumZ= {:.4R} m.", zone.MinimumZ - daylCntrl.DaylRefPtAbsCoord(3, refPtNum)));
                } else {
                    ShowContinueError(
                        state,
                        format("...Z Reference Distance Outside MaximumZ= {:.4R} m.", daylCntrl.DaylRefPtAbsCoord(3, refPtNum) - zone.MaximumZ));
                }
            }
        } // for (refPtNum) reference point
    }     // for (controlNum) daylighting control
}

void GetInputDayliteRefPt(EnergyPlusData &state, bool &ErrorsFound)
{
    // Perform GetInput function for the Daylighting:ReferencePoint object
    // Glazer - July 2016

    int RefPtNum = 0;
    int IOStat;
    int NumAlpha;
    int NumNumber;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "Daylighting:ReferencePoint";
    int TotRefPoints = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataDaylightingData->DaylRefPt.allocate(TotRefPoints);
    for (auto &pt : state.dataDaylightingData->DaylRefPt) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 ++RefPtNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlpha,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumber,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        pt.Name = state.dataIPShortCut->cAlphaArgs(1);
        pt.ZoneNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->Zone);
        if (pt.ZoneNum == 0) {
            int spaceNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(2), state.dataHeatBal->space);
            if (spaceNum == 0) {
                ShowSevereError(state,
                                cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\".");
                ErrorsFound = true;
            } else {
                pt.ZoneNum = state.dataHeatBal->space(spaceNum).zoneNum;
            }
        }
        pt.x = state.dataIPShortCut->rNumericArgs(1);
        pt.y = state.dataIPShortCut->rNumericArgs(2);
        pt.z = state.dataIPShortCut->rNumericArgs(3);
    }
}

bool doesDayLightingUseDElight(EnergyPlusData &state)
{
    for (auto &znDayl : state.dataDaylightingData->daylightControl) {
        if (znDayl.DaylightMethod == DataDaylighting::DaylightingMethod::DElight) {
            return true;
        }
    }
    return false;
}

void CheckTDDsAndLightShelvesInDaylitZones(EnergyPlusData &state)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Dec 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks daylighting input for TDDs and light shelfs
    //  which need to be checked after daylighting input has been read in (CR 7145)
    //  (eventually this should be changed once/if implementations change to decouple from daylighting calcs so that
    //  these devices can be used in models without daylighting controls
    // CR 7145 was for TDDs, but also implenting check for light shelves, the other "daylighting device"

    // METHODOLOGY EMPLOYED:
    // loop thru daylighting devices and check that their zones have daylight controls

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PipeNum;  // TDD pipe object number
    int ShelfNum; // light shelf object number
    int SurfNum;  // daylight device surface number
    bool ErrorsFound;

    ErrorsFound = false;

    for (PipeNum = 1; PipeNum <= (int)state.dataDaylightingDevicesData->TDDPipe.size(); ++PipeNum) {
        SurfNum = state.dataDaylightingDevicesData->TDDPipe(PipeNum).Diffuser;
        if (SurfNum > 0) {
            int const pipeEnclNum = state.dataSurface->Surface(SurfNum).SolarEnclIndex;
            if (state.dataViewFactor->EnclSolInfo(pipeEnclNum).TotalEnclosureDaylRefPoints == 0) {
                ShowWarningError(state,
                                 "DaylightingDevice:Tubular = " + state.dataDaylightingDevicesData->TDDPipe(PipeNum).Name +
                                     ":  is not connected to a Zone that has Daylighting, no visible transmittance will be modeled through the "
                                     "daylighting device.");
            }

        } else { // SurfNum == 0
            // should not come here (would have already been caught in TDD get input), but is an error
            ShowSevereError(
                state, "DaylightingDevice:Tubular = " + state.dataDaylightingDevicesData->TDDPipe(PipeNum).Name + ":  Diffuser surface not found ");
            ErrorsFound = true;
        }
    } // PipeNum

    for (ShelfNum = 1; ShelfNum <= (int)state.dataDaylightingDevicesData->Shelf.size(); ++ShelfNum) {
        SurfNum = state.dataDaylightingDevicesData->Shelf(ShelfNum).Window;
        if (SurfNum == 0) {
            // should not come here (would have already been caught in shelf get input), but is an error
            ShowSevereError(state, "DaylightingDevice:Shelf = " + state.dataDaylightingDevicesData->Shelf(ShelfNum).Name + ":  window not found ");
            ErrorsFound = true;
        }
    } // ShelfNum

    if (ErrorsFound) ShowFatalError(state, "CheckTDDsAndLightShelvesInDaylitZones: Errors in DAYLIGHTING input.");
}

void AssociateWindowShadingControlWithDaylighting(EnergyPlusData &state)
{
    for (int iShadeCtrl = 1; iShadeCtrl <= state.dataSurface->TotWinShadingControl; ++iShadeCtrl) {
        if (state.dataSurface->WindowShadingControl(iShadeCtrl).DaylightingControlName.empty()) continue;
        int found = -1;
        for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
            if (UtilityRoutines::SameString(state.dataSurface->WindowShadingControl(iShadeCtrl).DaylightingControlName,
                                            state.dataDaylightingData->daylightControl(daylightCtrlNum).Name)) {
                found = daylightCtrlNum;
                break;
            }
        }
        if (found > 0) {
            state.dataSurface->WindowShadingControl(iShadeCtrl).DaylightControlIndex = found;
        } else {
            ShowWarningError(state, "AssociateWindowShadingControlWithDaylighting: Daylighting object name used in WindowShadingControl not found.");
            ShowContinueError(state,
                              "..The WindowShadingControl object=\"" + state.dataSurface->WindowShadingControl(iShadeCtrl).Name +
                                  "\" and referenes an object named: \"" +
                                  state.dataSurface->WindowShadingControl(iShadeCtrl).DaylightingControlName + "\"");
        }
    }
}

void GetLightWellData(EnergyPlusData &state, bool &ErrorsFound) // If errors found in input
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Apr 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Gets data for a light well associated with a rectangular exterior window.
    // Calculates light well efficiency, defined as the ratio of the amount of visible
    // solar radiation leaving a well to the amount entering the well.

    // METHODOLOGY EMPLOYED:
    // Based on fit to Fig. 8-21, "Efficiency factors for various depths of light wells
    // based on well-interreflectance values," Lighting Handbook, 8th Edition, Illuminating
    // Engineering Society of North America, 1993.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int IOStat;            // IO Status when calling get input subroutine
    int NumAlpha;          // Number of alpha names being passed
    int NumProp;           // Number of properties being passed
    int TotLightWells;     // Total Light Well objects
    int loop;              // DO loop index
    int SurfNum;           // Surface number
    bool WrongSurfaceType; // True if associated surface is not an exterior window
    Real64 HeightWell;     // Well height (from window to bottom of well) (m)
    Real64 PerimWell;      // Well perimeter (at bottom of well) (m)
    Real64 AreaWell;       // Well area (at bottom of well) (m2)
    Real64 VisReflWell;    // Area-weighted visible reflectance of well walls
    Real64 WellCavRatio;   // Well cavity ratio
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    // Get the total number of Light Well objects
    cCurrentModuleObject = "DaylightingDevice:LightWell";
    TotLightWells = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    if (TotLightWells == 0) return;

    for (loop = 1; loop <= TotLightWells; ++loop) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlpha,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumProp,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        SurfNum = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(1), state.dataSurface->Surface);
        if (SurfNum == 0) {
            ShowSevereError(state,
                            cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                state.dataIPShortCut->cAlphaArgs(1) + "\" not found.");
        }

        // Check that associated surface is an exterior window
        WrongSurfaceType = false;
        if (SurfNum != 0) {
            if (state.dataSurface->Surface(SurfNum).Class != SurfaceClass::Window &&
                state.dataSurface->Surface(SurfNum).ExtBoundCond != ExternalEnvironment)
                WrongSurfaceType = true;
            if (WrongSurfaceType) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(1) + "\" - not an exterior window.");
                ErrorsFound = true;
            }
        }

        if (!ErrorsFound) {

            // Associated surface is an exterior window; calculate light well efficiency.

            state.dataSurface->SurfWinLightWellEff(SurfNum) = 1.0;
            HeightWell = state.dataIPShortCut->rNumericArgs(1);
            PerimWell = state.dataIPShortCut->rNumericArgs(2);
            AreaWell = state.dataIPShortCut->rNumericArgs(3);
            VisReflWell = state.dataIPShortCut->rNumericArgs(4);

            // Warning if light well area is less than window area
            if (AreaWell < (state.dataSurface->Surface(SurfNum).Area + state.dataSurface->SurfWinDividerArea(SurfNum) - 0.1)) {
                ShowSevereError(state,
                                cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(1) + "\" - Areas.");
                ShowContinueError(state,
                                  format("has Area of Bottom of Well={:.1R} that is less than window area={:.1R}",
                                         state.dataSurface->Surface(SurfNum).Area,
                                         AreaWell));
            }

            if (HeightWell >= 0.0 && PerimWell > 0.0 && AreaWell > 0.0) {
                WellCavRatio = 2.5 * HeightWell * PerimWell / AreaWell;
                state.dataSurface->SurfWinLightWellEff(SurfNum) = std::exp(-WellCavRatio * (0.16368 - 0.14467 * VisReflWell));
            }
        }

    } // End of loop over light well objects
}

inline int findWinShadingStatus(EnergyPlusData &state, int const IWin)
{
    // Return the window shading status, 1=unshaded, 2=shaded
    int WinShadingIndex = 1;
    bool WinShadedNoGlareControl = IS_SHADED_NO_GLARE_CTRL(state.dataSurface->SurfWinShadingFlag(IWin));
    if ((state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel) &&
        (WinShadedNoGlareControl || state.dataSurface->SurfWinSolarDiffusing(IWin))) {
        WinShadingIndex = 2;
    }
    return WinShadingIndex;
}

void DayltgGlare(EnergyPlusData &state,
                 int &IL,                  // Reference point index: 1=first ref pt, 2=second ref pt
                 Real64 &BLUM,             // Window background (surround) luminance (cd/m2)
                 Real64 &GLINDX,           // Glare index
                 int const daylightCtrlNum // Current daylighting control number
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997

    // PURPOSE OF THIS SUBROUTINE:
    // CALCULATE GLARE INDEX.

    // METHODOLOGY EMPLOYED:
    // Called from DayltgInteriorIllum.  Finds glare index at reference
    // point no. IL in a space using the Cornell/BRS large source
    // glare formula. BLUM is the background luminance (cd/m**2).
    // TH comment 1/21/2010: The SurfaceWindow(IWin)%ShadingFlag has to be set
    //  before calling this subroutine. For switchable glazings this is tricky
    //  because the ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop)
    //  may change every time step to represent intermediate switched state.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DGLARE.

    Real64 GTOT = 0.0; // Glare constant

    // Loop over exterior windows associated with zone
    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(thisDaylightControl.enclIndex);
    for (int loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
        int IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);
        int WinShadingIndex = findWinShadingStatus(state, IWin);
        // Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
        // below, which is (0.2936)**0.6
        Real64 GTOT1 = 0.4794 * (std::pow(thisDaylightControl.SourceLumFromWinAtRefPt(loop, WinShadingIndex, IL), 1.6)) *
                       std::pow(thisDaylightControl.SolidAngAtRefPtWtd(loop, IL), 0.8);
        Real64 GTOT2 = BLUM + 0.07 * (std::sqrt(thisDaylightControl.SolidAngAtRefPt(loop, IL))) *
                                  thisDaylightControl.SourceLumFromWinAtRefPt(loop, WinShadingIndex, IL);
        GTOT += GTOT1 / (GTOT2 + 0.000001);
    }

    // Glare index (adding 0.000001 prevents LOG10 (0))
    GLINDX = 10.0 * std::log10(GTOT + 0.000001);
    // Set glare index to zero for GTOT < 1
    GLINDX = max(0.0, GLINDX);
}

void DayltgGlareWithIntWins(EnergyPlusData &state,
                            Array1D<Real64> &GLINDX,  // Glare index
                            int const daylightCtrlNum // Current daylighting control number
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   March 2004

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate daylighting glare index for zones with interior windows.

    // METHODOLOGY EMPLOYED:
    // Finds glare index at reference point IL in a daylit zone using the Cornell/BRS large source
    // glare formula. Takes into account inter-reflected illuminance from light entering
    // the zone through interior windows

    // REFERENCES:
    // Based on subroutine DayltgGlare.

    Real64 GTOT = 0.0; // Glare constant(?)

    // Calculate background luminance including effect of inter-reflected illuminance from light
    // entering zone through its interior windows

    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(thisDaylightControl.enclIndex);
    int RefPoints = thisDaylightControl.TotalDaylRefPoints; // Number of daylighting reference points in zone
    for (int IL = 1; IL <= RefPoints; ++IL) {
        Real64 BackgroundLum =
            thisDaylightControl.BacLum(IL) + thisEnclDaylight.InterReflIllFrIntWins * thisEnclDaylight.aveVisDiffReflect / DataGlobalConstants::Pi;
        BackgroundLum = max(thisDaylightControl.IllumSetPoint(IL) * thisEnclDaylight.aveVisDiffReflect / DataGlobalConstants::Pi, BackgroundLum);

        // Loop over exterior windows associated with zone
        for (int loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
            int IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);
            int WinShadingIndex = findWinShadingStatus(state, IWin);
            // Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
            // below, which is (0.2936)**0.6
            Real64 GTOT1 = 0.4794 * (std::pow(thisDaylightControl.SourceLumFromWinAtRefPt(loop, WinShadingIndex, IL), 1.6)) *
                           std::pow(thisDaylightControl.SolidAngAtRefPtWtd(loop, IL), 0.8);
            Real64 GTOT2 = BackgroundLum + 0.07 * (std::sqrt(thisDaylightControl.SolidAngAtRefPt(loop, IL))) *
                                               thisDaylightControl.SourceLumFromWinAtRefPt(loop, WinShadingIndex, IL);
            GTOT += GTOT1 / (GTOT2 + 0.000001);
        }

        // Glare index
        GLINDX(IL) = 10.0 * std::log10(GTOT + 0.000001);
        // Set glare index to zero for GTOT < 1
        GLINDX(IL) = max(0.0, GLINDX(IL));
    }
}

void DayltgExtHorizIllum(EnergyPlusData &state,
                         Array1A<Real64> HISK, // Horizontal illuminance from sky for different sky types
                         Real64 &HISU          // Horizontal illuminance from sun for unit beam normal
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates exterior daylight illuminance.

    // METHODOLOGY EMPLOYED:
    // Called by CalcDayltgCoefficients. Calculates illuminance
    // on unobstructed horizontal surface by integrating
    // over the luminance distribution of standard CIE skies.
    // Calculates horizontal beam illuminance.
    // REFERENCES:
    // Based on DOE-2.1E subroutine DHILL.

    // Argument array dimensioning
    HISK.dim(4);

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const DTH((2.0 * DataGlobalConstants::Pi) / double(NTH)); // Sky integration azimuth stepsize (radians)
    Real64 const DPH(DataGlobalConstants::PiOvr2 / double(NPH));     // Sky integration altitude stepsize (radians)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int IPH; // Altitude index for sky integration
    int ITH; // Azimuth index for sky integration
    auto &PH = state.dataDaylightingManager->PH;
    auto &TH = state.dataDaylightingManager->TH;
    auto &SPHCPH = state.dataDaylightingManager->SPHCPH;
    int ISky; // Sky type index

    // Integrate to obtain illuminance from sky.
    // The contribution in lumens/m2 from a patch of sky at altitude PH and azimuth TH
    // is L(TH,PH)*SIN(PH)*COS(PH)*DTH*DPH, where L(TH,PH) is the luminance
    // of the patch in cd/m2.
    //  Init
    if (state.dataDaylightingManager->DayltgExtHorizIllum_firstTime) {
        for (IPH = 1; IPH <= NPH; ++IPH) {
            PH(IPH) = (IPH - 0.5) * DPH;
            SPHCPH(IPH) = std::sin(PH(IPH)) * std::cos(PH(IPH)); // DA = COS(PH)*DTH*DPH
        }
        for (ITH = 1; ITH <= NTH; ++ITH) {
            TH(ITH) = (ITH - 0.5) * DTH;
        }
        state.dataDaylightingManager->DayltgExtHorizIllum_firstTime = false;
    }

    HISK = 0.0;

    // Sky integration
    for (IPH = 1; IPH <= NPH; ++IPH) {
        Real64 const PH_IPH(PH(IPH));
        Real64 const SPHCPH_IPH(SPHCPH(IPH));
        for (ITH = 1; ITH <= NTH; ++ITH) {
            Real64 const TH_ITH(TH(ITH));
            for (ISky = 1; ISky <= 4; ++ISky) {
                HISK(ISky) += DayltgSkyLuminance(state, ISky, TH_ITH, PH_IPH) * SPHCPH_IPH;
            }
        }
    }

    for (ISky = 1; ISky <= 4; ++ISky) {
        HISK(ISky) *= DTH * DPH;
    }

    // Direct solar horizontal illum (for unit direct normal illuminance)
    HISU = state.dataDaylightingManager->SPHSUN * 1.0;
}

void DayltgHitObstruction(EnergyPlusData &state,
                          int const IHOUR,           // Hour number
                          int const IWin,            // Window index
                          Vector3<Real64> const &R1, // Origin of ray (m)
                          Vector3<Real64> const &RN, // Unit vector along ray
                          Real64 &ObTrans            // Product of solar transmittances of exterior obstructions
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       FCW, May 2003: update list of surface classes that qualify as obstructions;
    //                        add interior surfaces as possible obstructors;
    //                        return from DO loop over surfaces as soon as any obstruction is hit;
    //                      FCW, July 2003: change from returning whether an obstruction is hit or not
    //                        to product of solar transmittances of hit obstructions.
    //                      FCW, Nov 2003: remove interior surfaces as possible obstructors since there
    //                        is now a separate check for interior obstructions; exclude windows and
    //                        doors as obstructors since if they are obstructors their base surfaces will
    //                        also be obstructors
    //       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

    // PURPOSE OF THIS SUBROUTINE:
    // Determines the product of the solar transmittances of the obstructions hit by a ray
    // from R1 in the direction of vector RN.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DHITSH.

    // Using/Aliasing
    using ScheduleManager::LookUpScheduleValue;

    // Local declarations
    SurfaceClass IType; // Surface type/class:  mirror surfaces of shading surfaces
    auto &DayltgHitObstructionHP = state.dataDaylightingManager->DayltgHitObstructionHP;
    bool hit; // True iff a particular obstruction is hit

    ObTrans = 1.0;

    auto const &window(state.dataSurface->Surface(IWin));
    auto const window_iBaseSurf(window.BaseSurf);

    // Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
    // Building elements are assumed to be opaque
    // A shadowing surface is opaque unless its transmittance schedule value is non-zero
    if (state.dataSurface->TotSurfaces < octreeCrossover) { // Linear search through surfaces

        for (int ISurf = 1; ISurf <= state.dataSurface->TotSurfaces; ++ISurf) {
            auto const &surface(state.dataSurface->Surface(ISurf));
            if (!surface.IsShadowPossibleObstruction) continue;
            IType = surface.Class;
            if ((IType == SurfaceClass::Wall || IType == SurfaceClass::Roof || IType == SurfaceClass::Floor) && (ISurf != window_iBaseSurf)) {
                PierceSurface(state, ISurf, R1, RN, DayltgHitObstructionHP, hit);
                if (hit) { // Building element is hit (assumed opaque)
                    ObTrans = 0.0;
                    break;
                }
            } else if (surface.IsShadowing) {
                PierceSurface(state, ISurf, R1, RN, DayltgHitObstructionHP, hit);
                if (hit) { // Shading surface is hit
                    // Get solar transmittance of the shading surface
                    Real64 const Trans(surface.SchedShadowSurfIndex > 0 ? LookUpScheduleValue(state, surface.SchedShadowSurfIndex, IHOUR, 1) : 0.0);
                    if (Trans < 1.e-6) {
                        ObTrans = 0.0;
                        break;
                    } else {
                        ObTrans *= Trans;
                    }
                }
            }
        }

    } else { // Surface octree search

        auto const &window_base(window_iBaseSurf > 0 ? state.dataSurface->Surface(window_iBaseSurf) : window);
        auto const window_base_p(&window_base);

        // Lambda function for the octree to test for surface hit and update transmittance if hit
        auto solarTransmittance = [=, &state, &R1, &RN, &hit, &ObTrans](SurfaceData const &surface) -> bool {
            if (!surface.IsShadowPossibleObstruction) return false; // Do Consider separate octree without filtered surfaces
            auto const sClass(surface.Class);
            if ((sClass == SurfaceClass::Wall || sClass == SurfaceClass::Roof || sClass == SurfaceClass::Floor) && (&surface != window_base_p)) {
                PierceSurface(surface, R1, RN, state.dataDaylightingManager->DayltgHitObstructionHP, hit);
                if (hit) { // Building element is hit (assumed opaque)
                    ObTrans = 0.0;
                    return true;
                }
            } else if (surface.IsShadowing) {
                PierceSurface(surface, R1, RN, state.dataDaylightingManager->DayltgHitObstructionHP, hit);
                if (hit) { // Shading surface is hit
                    // Get solar transmittance of the shading surface
                    Real64 const Trans(surface.SchedShadowSurfIndex > 0 ? LookUpScheduleValue(state, surface.SchedShadowSurfIndex, IHOUR, 1) : 0.0);
                    if (Trans < 1.e-6) {
                        ObTrans = 0.0;
                        return true;
                    } else {
                        ObTrans *= Trans;
                        return ObTrans == 0.0;
                    }
                }
            }
            return false;
        };

        // Check octree surface candidates for hits: short circuits if zero transmittance reached
        Vector3<Real64> const RN_inv(SurfaceOctreeCube::safe_inverse(RN));
        state.dataHeatBalMgr->surfaceOctree.processSomeSurfaceRayIntersectsCube(state, R1, RN, RN_inv, solarTransmittance);
    }
}

void DayltgHitInteriorObstruction(EnergyPlusData &state,
                                  int const IWin,            // Window index
                                  Vector3<Real64> const &R1, // Origin of ray (m)
                                  Vector3<Real64> const &R2, // Destination of ray (m)
                                  bool &hit                  // True iff ray hits an obstruction
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks for interior obstructions between reference point and window element.

    // Preconditions
    assert(magnitude(R2 - R1) > 0.0); // Protect normalize() from divide by zero

    // Local declarations
    SurfaceClass IType; // Surface type/class
    auto &DayltgHitInteriorObstructionHP =
        state.dataDaylightingManager->DayltgHitInteriorObstructionHP; // Hit coordinates, if ray hits an obstruction
    auto &RN = state.dataDaylightingManager->RN;                      // Unit vector along ray

    hit = false;
    RN = (R2 - R1).normalize();         // Make unit vector
    Real64 const d12(distance(R1, R2)); // Distance between R1 and R2

    auto const &window(state.dataSurface->Surface(IWin));
    auto const window_Enclosure(window.SolarEnclIndex);
    auto const window_iBaseSurf(window.BaseSurf);
    auto const &window_base(window_iBaseSurf > 0 ? state.dataSurface->Surface(window_iBaseSurf) : window);
    auto const window_base_iExtBoundCond(window_base.ExtBoundCond);

    // Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
    if (state.dataSurface->TotSurfaces < octreeCrossover) { // Linear search through surfaces

        for (int ISurf = 1; ISurf <= state.dataSurface->TotSurfaces; ++ISurf) {
            auto const &surface(state.dataSurface->Surface(ISurf));
            IType = surface.Class;
            if ((surface.IsShadowing) ||                         // Shadowing surface
                ((surface.SolarEnclIndex == window_Enclosure) && // Wall/ceiling/floor is in same zone as window
                 (IType == SurfaceClass::Wall || IType == SurfaceClass::Roof || IType == SurfaceClass::Floor) && (ISurf != window_iBaseSurf) &&
                 (ISurf != window_base_iExtBoundCond))) // Exclude window's base or base-adjacent surfaces
            {
                PierceSurface(state, ISurf, R1, RN, d12, DayltgHitInteriorObstructionHP, hit); // Check if R2-R1 segment pierces surface
                if (hit) break;                                                                // Segment pierces surface: Don't check the rest
            }
        }

    } else { // Surface octree search

        auto const window_base_p(&window_base);
        auto const &window_base_adjacent(window_base_iExtBoundCond > 0 ? state.dataSurface->Surface(window_base_iExtBoundCond) : window_base);
        auto const window_base_adjacent_p(&window_base_adjacent);

        // Lambda function for the octree to test for surface hit
        auto surfaceHit = [=, &R1, &hit, &state](SurfaceData const &surface) -> bool {
            auto const sClass(surface.Class);
            if ((surface.IsShadowing) ||                         // Shadowing surface
                ((surface.SolarEnclIndex == window_Enclosure) && // Surface is in same zone as window
                 (sClass == SurfaceClass::Wall || sClass == SurfaceClass::Roof || sClass == SurfaceClass::Floor) && // Wall, ceiling/roof, or floor
                 (&surface != window_base_p) && (&surface != window_base_adjacent_p))) // Exclude window's base or base-adjacent surfaces
            {
                PierceSurface(surface,
                              R1,
                              RN,
                              d12,
                              state.dataDaylightingManager->DayltgHitInteriorObstructionHP,
                              hit); // Check if R2-R1 segment pierces surface
                return hit;
            } else {
                return false;
            }
        };

        // Check octree surface candidates until a hit is found, if any
        state.dataHeatBalMgr->surfaceOctree.hasSurfaceSegmentIntersectsCube(R1, R2, surfaceHit);
    }
}

void DayltgHitBetWinObstruction(EnergyPlusData &state,
                                int const IWin1,           // Surface number of origin window
                                int const IWin2,           // Surface number of destination window
                                Vector3<Real64> const &R1, // Origin of ray (on IWin1) (m)
                                Vector3<Real64> const &R2, // Destination of ray (on IWin2) (m)
                                bool &hit                  // True iff ray hits an obstruction
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Feb 2004
    //       MODIFIED na
    //       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

    // PURPOSE OF THIS SUBROUTINE:
    // Determines if a ray from point R1 on window IWin1 to point R2
    // on window IWin2 hits an obstruction

    // Preconditions
    assert(magnitude(R2 - R1) > 0.0); // Protect normalize() from divide by zero

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    SurfaceClass IType; // Surface type/class
    auto &DayltgHitBetWinObstructionHP = state.dataDaylightingManager->DayltgHitBetWinObstructionHP;
    auto &DayltgHitBetWinObstructionRN = state.dataDaylightingManager->DayltgHitBetWinObstructionRN;

    hit = false;
    DayltgHitBetWinObstructionRN = (R2 - R1).normalize(); // Unit vector
    Real64 const d12(distance(R1, R2));                   // Distance between R1 and R2 (m)

    auto const &window1(state.dataSurface->Surface(IWin1));
    auto const window1_iBaseSurf(window1.BaseSurf);
    auto const &window1_base(window1_iBaseSurf > 0 ? state.dataSurface->Surface(window1_iBaseSurf) : window1);
    auto const window1_base_iExtBoundCond(window1_base.ExtBoundCond);

    auto const &window2(state.dataSurface->Surface(IWin2));
    auto const window2_Enclosure(window2.SolarEnclIndex);
    auto const window2_iBaseSurf(window2.BaseSurf);
    auto const &window2_base(window2_iBaseSurf > 0 ? state.dataSurface->Surface(window2_iBaseSurf) : window2);
    auto const window2_base_iExtBoundCond(window2_base.ExtBoundCond);

    // Preconditions
    //        assert( window1.Zone == window2_Zone ); //? This is violated in PurchAirWithDoubleFacadeDaylighting so then why the asymmetry
    // of  only checking for wall/roof/floor for window2 zone below?

    // Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
    if (state.dataSurface->TotSurfaces < octreeCrossover) { // Linear search through surfaces

        for (int ISurf = 1; ISurf <= state.dataSurface->TotSurfaces; ++ISurf) {
            auto const &surface(state.dataSurface->Surface(ISurf));
            IType = surface.Class;
            if ((surface.IsShadowing) ||                          // Shadowing surface
                ((surface.SolarEnclIndex == window2_Enclosure) && // Wall/ceiling/floor is in same zone as windows
                 (IType == SurfaceClass::Wall || IType == SurfaceClass::Roof || IType == SurfaceClass::Floor) && // Wall, ceiling/roof, or floor
                 (ISurf != window1_iBaseSurf) && (ISurf != window2_iBaseSurf) &&                                 // Exclude windows' base surfaces
                 (ISurf != window1_base_iExtBoundCond) && (ISurf != window2_base_iExtBoundCond))) // Exclude windows' base-adjacent surfaces
            {
                PierceSurface(
                    state, ISurf, R1, DayltgHitBetWinObstructionRN, d12, DayltgHitBetWinObstructionHP, hit); // Check if R2-R1 segment pierces surface
                if (hit) break; // Segment pierces surface: Don't check the rest
            }
        }

    } else { // Surface octree search

        auto const window1_base_p(&window1_base);
        auto const &window1_base_adjacent(window1_base_iExtBoundCond > 0 ? state.dataSurface->Surface(window1_base_iExtBoundCond) : window1_base);
        auto const window1_base_adjacent_p(&window1_base_adjacent);

        auto const window2_base_p(&window2_base);
        auto const &window2_base_adjacent(window2_base_iExtBoundCond > 0 ? state.dataSurface->Surface(window2_base_iExtBoundCond) : window2_base);
        auto const window2_base_adjacent_p(&window2_base_adjacent);

        // Lambda function for the octree to test for surface hit
        auto surfaceHit = [=, &R1, &hit, &state](SurfaceData const &surface) -> bool {
            auto const sClass(surface.Class);
            if ((surface.IsShadowing) ||                          // Shadowing surface
                ((surface.SolarEnclIndex == window2_Enclosure) && // Surface is in same zone as window
                 (sClass == SurfaceClass::Wall || sClass == SurfaceClass::Roof || sClass == SurfaceClass::Floor) && // Wall, ceiling/roof, or floor
                 (&surface != window1_base_p) && (&surface != window2_base_p) &&                                    // Exclude windows' base surfaces
                 (&surface != window1_base_adjacent_p) && (&surface != window2_base_adjacent_p))) // Exclude windows' base-adjacent surfaces
            {
                PierceSurface(surface,
                              R1,
                              state.dataDaylightingManager->DayltgHitBetWinObstructionRN,
                              d12,
                              state.dataDaylightingManager->DayltgHitBetWinObstructionHP,
                              hit); // Check if R2-R1 segment pierces surface
                return hit;
            } else {
                return false;
            }
        };

        // Check octree surface candidates until a hit is found, if any
        state.dataHeatBalMgr->surfaceOctree.hasSurfaceSegmentIntersectsCube(R1, R2, surfaceHit);
    }
}

void initDaylighting(EnergyPlusData &state, bool const initSurfaceHeatBalancefirstTime)
{
    // For daylit zones, calculate interior daylight illuminance at reference points and
    // simulate lighting control system to get overhead electric lighting reduction
    // factor due to daylighting.
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            if (state.dataSurface->Surface(SurfNum).ExtSolar) {
                state.dataSurface->SurfaceWindow(SurfNum).IllumFromWinAtRefPtRep = 0.0;
                state.dataSurface->SurfaceWindow(SurfNum).LumWinFromRefPtRep = 0.0;
            }
        }
    }

    // Reset space power reduction factors
    for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
        state.dataDaylightingData->spacePowerReductionFactor(spaceNum) = 1.0;
    }
    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
        thisDaylightControl.PowerReductionFactor = 1.0;
        if (state.dataEnvrn->PreviousSolRadPositive) {
            // Reset to zero only if there was solar in the previous timestep, otherwise these are already zero
            thisDaylightControl.DaylIllumAtRefPt = 0.0;
            thisDaylightControl.GlareIndexAtRefPt = 0.0;
            state.dataDaylightingData->enclDaylight(thisDaylightControl.enclIndex).InterReflIllFrIntWins =
                0.0; // inter-reflected illuminance from interior windows
            for (int refPtNum = 1; refPtNum <= thisDaylightControl.TotalDaylRefPoints; ++refPtNum) {
                thisDaylightControl.TimeExceedingGlareIndexSPAtRefPt(refPtNum) = 0.0;
                thisDaylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt(refPtNum) = 0.0;
            }
        }

        if (state.dataEnvrn->SunIsUp && thisDaylightControl.TotalDaylRefPoints != 0) {
            if (initSurfaceHeatBalancefirstTime) DisplayString(state, "Computing Interior Daylighting Illumination");
            DayltgInteriorIllum(state, daylightCtrlNum);
        }
    }

    // The following report variables are valid only for daylit zones/enclosures without interior windows
    if (state.dataEnvrn->SunIsUp) {
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            if ((state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints > 0) &&
                (!state.dataViewFactor->EnclSolInfo(enclNum).HasInterZoneWindow)) {
                auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
                for (int extWinNum = 1; extWinNum <= thisEnclDaylight.NumOfDayltgExtWins; ++extWinNum) {
                    int IWin = thisEnclDaylight.DayltgExtWinSurfNums(extWinNum);
                    int IS = 1;
                    if (state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel &&
                        (IS_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) || state.dataSurface->SurfWinSolarDiffusing(IWin))) {
                        IS = 2;
                    }
                    int refPtCount = 0;
                    for (int controlNum : state.dataDaylightingData->enclDaylight(enclNum).daylightControlIndexes) {
                        auto &thisControl = state.dataDaylightingData->daylightControl(controlNum);
                        if (thisControl.DaylightMethod == DataDaylighting::DaylightingMethod::SplitFlux) {
                            for (int refPtNum = 1; refPtNum <= thisControl.TotalDaylRefPoints; ++refPtNum) {
                                ++refPtCount; // Count reference points across each daylighting control in the same enclosure
                                state.dataSurface->SurfaceWindow(IWin).IllumFromWinAtRefPtRep(refPtCount) =
                                    thisControl.IllumFromWinAtRefPt(extWinNum, IS, refPtNum);
                                state.dataSurface->SurfaceWindow(IWin).LumWinFromRefPtRep(refPtCount) =
                                    thisControl.SourceLumFromWinAtRefPt(extWinNum, IS, refPtNum);
                            }
                        }
                    }
                }
            }
        }
    }

    if (state.dataEnvrn->SunIsUp && (int)state.dataDaylightingDevicesData->TDDPipe.size() > 0) {
        if (initSurfaceHeatBalancefirstTime) DisplayString(state, "Computing Interior Daylighting Illumination for TDD pipes");
        DayltgInteriorTDDIllum(state);
    }

    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);

        // RJH DElight Modification Begin - Call to DElight electric lighting control subroutine
        // Check if the sun is up and the current Thermal Zone hosts a Daylighting:DElight object
        if (state.dataEnvrn->SunIsUp && thisDaylightControl.TotalDaylRefPoints != 0 &&
            (thisDaylightControl.DaylightMethod == DataDaylighting::DaylightingMethod::DElight)) {
            int zoneNum = thisDaylightControl.zoneIndex;
            // Call DElight interior illuminance and electric lighting control subroutine
            Real64 dPowerReducFac = 1.0; // Return value Electric Lighting Power Reduction Factor for current Zone and Timestep
            Real64 dHISKFFC = state.dataEnvrn->HISKF * DataDElight::LUX2FC;
            Real64 dHISUNFFC = state.dataEnvrn->HISUNF * DataDElight::LUX2FC;
            Real64 dSOLCOS1 = state.dataEnvrn->SOLCOS(1);
            Real64 dSOLCOS2 = state.dataEnvrn->SOLCOS(2);
            Real64 dSOLCOS3 = state.dataEnvrn->SOLCOS(3);
            Real64 dLatitude = state.dataEnvrn->Latitude;
            Real64 dCloudFraction = state.dataEnvrn->CloudFraction;
            // Init Error Flag to 0 (no Warnings or Errors) (returned from DElight)
            int iErrorFlag = 0;
            bool elOpened;

            int iReadStatus;       // Error File Read Status
            std::string cErrorMsg; // Each DElight Error Message can be up to 200 characters long
            bool bEndofErrFile;    // End of Error File flag

            DElightManagerF::DElightElecLtgCtrl(len(state.dataHeatBal->Zone(zoneNum).Name),
                                                state.dataHeatBal->Zone(zoneNum).Name,
                                                dLatitude,
                                                dHISKFFC,
                                                dHISUNFFC,
                                                dCloudFraction,
                                                dSOLCOS1,
                                                dSOLCOS2,
                                                dSOLCOS3,
                                                dPowerReducFac,
                                                iErrorFlag);
            // Check Error Flag for Warnings or Errors returning from DElight
            // RJH 2008-03-07: If no warnings/errors then read refpt illuminances for standard output reporting
            if (iErrorFlag != 0) {
                // Open DElight Electric Lighting Error File for reading
                auto iDElightErrorFile = state.files.outputDelightDfdmpFilePath.try_open(state.files.outputControl.delightdfdmp);
                elOpened = iDElightErrorFile.good();

                // Sequentially read lines in DElight Electric Lighting Error File
                // and process them using standard EPlus warning/error handling calls
                bEndofErrFile = false;
                iReadStatus = 0;
                while (!bEndofErrFile && elOpened) {
                    auto cErrorLine = iDElightErrorFile.readLine();
                    if (cErrorLine.eof) {
                        bEndofErrFile = true;
                        continue;
                    }

                    // Is the current line a Warning message?
                    if (has_prefix(cErrorLine.data, "WARNING: ")) {
                        cErrorMsg = cErrorLine.data.substr(9);
                        ShowWarningError(state, cErrorMsg);
                    }
                    // Is the current line an Error message?
                    if (has_prefix(cErrorLine.data, "ERROR: ")) {
                        cErrorMsg = cErrorLine.data.substr(7);
                        ShowSevereError(state, cErrorMsg);
                        iErrorFlag = 1;
                    }
                }

                // Close DElight Error File and delete

                if (elOpened) {
                    iDElightErrorFile.close();
                    FileSystem::removeFile(iDElightErrorFile.filePath);
                }
                // If any DElight Error occurred then ShowFatalError to terminate
                if (iErrorFlag > 0) {
                    ShowFatalError(state, "End of DElight Error Messages");
                }
            } else { // RJH 2008-03-07: No errors
                // extract reference point illuminance values from DElight Electric Lighting dump file for reporting
                // Open DElight Electric Lighting Dump File for reading
                auto iDElightErrorFile = state.files.outputDelightEldmpFilePath.try_open(state.files.outputControl.delighteldmp);
                if (iDElightErrorFile.is_open()) {
                    elOpened = true;
                } else {
                    elOpened = false;
                }

                // Sequentially read lines in DElight Electric Lighting Dump File
                // and extract refpt illuminances for standard EPlus output handling
                bEndofErrFile = false;
                int iDElightRefPt = 0; // Reference Point number for reading DElight Dump File (eplusout.delighteldmp)
                iReadStatus = 0;
                while (!bEndofErrFile && elOpened) {
                    auto line = iDElightErrorFile.read<Real64>();
                    Real64 dRefPtIllum = line.data; // tmp var for reading RefPt illuminance
                    if (line.eof) {
                        bEndofErrFile = true;
                        continue;
                    }
                    // Increment refpt counter
                    ++iDElightRefPt;
                    // Assure refpt index does not exceed number of refpts in this zone
                    if (iDElightRefPt <= thisDaylightControl.TotalDaylRefPoints) {
                        thisDaylightControl.DaylIllumAtRefPt(iDElightRefPt) = dRefPtIllum;
                    }
                }

                // Close DElight Electric Lighting Dump File and delete
                if (elOpened) {
                    iDElightErrorFile.close();
                    FileSystem::removeFile(iDElightErrorFile.filePath);
                };
            }
            // Store the calculated total zone Power Reduction Factor due to DElight daylighting
            // in the ZoneDaylight structure for later use
            thisDaylightControl.PowerReductionFactor = dPowerReducFac;
        }
        // RJH DElight Modification End - Call to DElight electric lighting control subroutine
    }

    if (state.dataEnvrn->SunIsUp && !state.dataGlobal->DoingSizing) {
        DayltgInteriorMapIllum(state);
    }
    for (int zoneNum = 1; zoneNum <= state.dataGlobal->NumOfZones; ++zoneNum) {
        int const firstSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceFirst;
        int const lastSurfWin = state.dataHeatBal->Zone(zoneNum).WindowSurfaceLast;
        for (int SurfNum = firstSurfWin; SurfNum <= lastSurfWin; ++SurfNum) {
            state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
            if (IS_SHADED(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 1.0;
            } else {
                state.dataSurface->SurfWinFracTimeShadingDeviceOn(SurfNum) = 0.0;
            }
        }
    }
}

void manageDaylighting(EnergyPlusData &state)
{
    if (state.dataEnvrn->SunIsUp && (state.dataEnvrn->BeamSolarRad + state.dataEnvrn->GndSolarRad + state.dataEnvrn->DifSolarRad > 0.0)) {
        for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
            if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints > 0) {
                if (state.dataViewFactor->EnclSolInfo(enclNum).HasInterZoneWindow) {
                    DayltgInterReflIllFrIntWins(state, enclNum);
                    for (int daylightCtrlNum : state.dataDaylightingData->enclDaylight(enclNum).daylightControlIndexes) {
                        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
                        DayltgGlareWithIntWins(state, thisDaylightControl.GlareIndexAtRefPt, enclNum);
                    }
                }
            }
        }
        DayltgElecLightingControl(state);
    } else if (state.dataDaylightingData->mapResultsToReport && state.dataGlobal->TimeStep == state.dataGlobal->NumOfTimeStepInHour) {
        for (int MapNum = 1; MapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++MapNum) {
            ReportIllumMap(state, MapNum);
        }
        state.dataDaylightingData->mapResultsToReport = false;
    }
}

void DayltgInteriorIllum(EnergyPlusData &state,
                         int const daylightCtrlNum) // Daylighting:Controls number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       March 2000, FCW: interpolate clear-sky daylight factors using
    //                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
    //                      only HourOfDay was used
    //                      Jan 2001, FCW: interpolate in slat angle for windows with blinds
    //                      that have movable slats
    //                      Oct 2002, LKL: changed interpolation steps to HourOfDay/WeightNow
    //                      LastHour/WeightPreviousHour
    //                      Aug 2003, FCW: fix bug that prevented ShadingControlType =
    //                      MEETDAYLIGHTILLUMINANCESETPOINT from working
    //                      Mar 2004, FCW: fix bug in calc of illuminance setpoint contribution
    //                      to background luminance: now it is divided by pi to give cd/m2
    //                      Mar 2004, FCW: modify to handle daylighting through interior windows
    //                      June 2009, TH: modified for thermochromic windows
    //                      Jan 2010, TH (CR 7984): added iterations for switchable windows with shading
    //                       control of MeetDaylightIlluminanceSetpoint and glare control is active
    //                       Also corrected bugs (CR 7988) for switchable glazings not related to CR 7984

    // PURPOSE OF THIS SUBROUTINE:
    // Using daylighting factors and exterior illuminance, determine
    // the current-hour interior daylight illuminance and glare index
    // at each reference point in a space. Deploy window shading window by window
    // if glare control is active for window and if the acceptable glare index
    // is exceeded at both reference points.

    // Called by InitSurfaceHeatBalance.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DINTIL.

    Real64 constexpr tmpSWIterStep(0.05); // step of switching factor, assuming maximum of 20 switching states

    int NREFPT; // Number of daylighting reference points
    int ISky;   // Sky type index
    int ISky1;  // Sky type index values for averaging two sky types
    int ISky2;
    auto &DFSUHR = state.dataDaylightingManager->DFSUHR;       // Sun daylight factor for bare/shaded window
    auto &BFSUHR = state.dataDaylightingManager->BFSUHR;       // Sun background luminance factor for bare/shaded window
    auto &SFSUHR = state.dataDaylightingManager->SFSUHR;       // Sun source luminance factor for bare/shaded window
    auto &HorIllSky = state.dataDaylightingManager->HorIllSky; // Horizontal illuminance for different sky types
    auto &SetPnt = state.dataDaylightingManager->SetPnt;       // Illuminance setpoint at reference points (lux)
    auto &GLRNDX = state.dataDaylightingManager->GLRNDX;       // Glare index at reference point
    auto &GLRNEW = state.dataDaylightingManager->GLRNEW;       // New glare index at reference point
    auto &SFSKHR = state.dataDaylightingManager->SFSKHR; // Sky source luminance factor for sky type (second index), bare/shaded window (first index)
    auto &DFSKHR = state.dataDaylightingManager->DFSKHR; // Sky daylight factor for sky type (second index), bare/shaded window (first index)
    auto &BFSKHR =
        state.dataDaylightingManager->BFSKHR; // Sky background luminance factor for sky type (second index), bare/shaded window (first index)

    auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);
    int enclNum = thisDaylightControl.enclIndex;
    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
    int ISWFLG; // Switchable glazing flag: =1 if one or more windows in a zone
    //  has switchable glazing that adjusts visible transmittance to just meet
    //  daylighting setpoint; =0 otherwise.
    int ICtrl;           // Window shading control pointer
    Real64 VTRAT;        // Ratio between switched and unswitched visible transmittance at normal incidence
    Real64 BACL;         // Window background (surround) luminance for glare calc (cd/m2)
    Real64 SkyWeight;    // Weighting factor used to average two different sky types
    Real64 HorIllSkyFac; // Ratio between horizontal illuminance from sky horizontal irradiance and
    //   luminous efficacy and horizontal illuminance from averaged sky
    bool GlareFlag; // True if maximum glare is exceeded

    Real64 VTRatio;  // VT (visible transmittance) ratio = VTNow / VTMaster
    Real64 VTNow;    // VT of the time step actual TC window
    Real64 VTMaster; // VT of the base/master TC window

    // Added variables for glare iterations for switchable glazings
    auto &tmpSWSL1 = state.dataDaylightingManager->tmpSWSL1;
    auto &tmpSWSL2 = state.dataDaylightingManager->tmpSWSL2;
    auto &tmpSWFactor = state.dataDaylightingManager->tmpSWFactor;
    auto &tmpMult = state.dataDaylightingManager->tmpMult;
    auto &GlareOK = state.dataDaylightingManager->GlareOK;
    auto &tmpIllumFromWinAtRefPt = state.dataDaylightingManager->tmpIllumFromWinAtRefPt;
    auto &tmpBackLumFromWinAtRefPt = state.dataDaylightingManager->tmpBackLumFromWinAtRefPt;
    auto &tmpSourceLumFromWinAtRefPt = state.dataDaylightingManager->tmpSourceLumFromWinAtRefPt;
    auto &blnCycle = state.dataDaylightingManager->blnCycle;

    bool breakOuterLoop(false);
    bool continueOuterLoop(false);

    if (thisDaylightControl.DaylightMethod != DataDaylighting::DaylightingMethod::SplitFlux) return;

    NREFPT = thisDaylightControl.TotalDaylRefPoints;

    // Three arrays to save original clear and dark (fully switched) states'
    //  zone/window daylighting properties.
    if (state.dataDaylightingManager->DayltgInteriorIllum_firstTime) {
        int const d1(max(maxval(state.dataHeatBal->Zone, &ZoneData::NumSubSurfaces),
                         maxval(state.dataDaylightingData->enclDaylight, &DataDaylighting::EnclDaylightCalc::NumOfDayltgExtWins)));
        tmpIllumFromWinAtRefPt.allocate(d1, 2, state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
        tmpBackLumFromWinAtRefPt.allocate(d1, 2, state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
        tmpSourceLumFromWinAtRefPt.allocate(d1, 2, state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);

        SetPnt.allocate(state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
        state.dataDaylightingManager->DaylIllum.allocate(state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
        GLRNDX.allocate(state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);
        GLRNEW.allocate(state.dataDaylightingManager->maxNumRefPtInAnyDaylCtrl);

        state.dataDaylightingManager->DayltgInteriorIllum_firstTime = false;
    }
    tmpIllumFromWinAtRefPt = 0.0;
    tmpBackLumFromWinAtRefPt = 0.0;
    tmpSourceLumFromWinAtRefPt = 0.0;

    // Initialize reference point illuminance and window background luminance
    for (int IL = 1; IL <= NREFPT; ++IL) {
        SetPnt(IL) = thisDaylightControl.IllumSetPoint(IL);
        state.dataDaylightingManager->DaylIllum(IL) = 0.0;
        thisDaylightControl.BacLum(IL) = 0.0;
    }

    if (state.dataEnvrn->SkyClearness > 3.0) { // Sky is average of clear and clear turbid
        SkyWeight = min(1.0, (state.dataEnvrn->SkyClearness - 3.0) / 3.0);
        ISky1 = 1;
        ISky2 = 2;
    } else if (state.dataEnvrn->SkyClearness > 1.2) { // Sky is average of clear turbid and intermediate
        SkyWeight = (state.dataEnvrn->SkyClearness - 1.2) / 1.8;
        ISky1 = 2;
        ISky2 = 3;
    } else { // Sky is average of intermediate and overcast
        SkyWeight = min(1.0, max(0.0, (state.dataEnvrn->SkyClearness - 1.0) / 0.2, (state.dataEnvrn->SkyBrightness - 0.05) / 0.4));
        ISky1 = 3;
        ISky2 = 4;
    }

    // First loop over exterior windows associated with this zone. The window may be an exterior window in
    // the zone or an exterior window in an adjacent zone that shares an interior window with the zone.
    // Find contribution of each window to the daylight illum and to the glare numerator at each reference point.
    // Use shading flags set in WindowShadingManager.
    for (int loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
        int IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);

        // Added TH 6/29/2009 for thermochromic windows
        VTRatio = 1.0;
        if (NREFPT > 0) {
            int const IConst = state.dataSurface->Surface(IWin).Construction;
            if (state.dataConstruction->Construct(IConst).TCFlag == 1) {
                // For thermochromic windows, daylight and glare factors are always calculated
                //  based on the master construction. They need to be adjusted by the VTRatio, including:
                //  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
                //  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
                VTNow = General::POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef);
                VTMaster =
                    General::POLYF(1.0, state.dataConstruction->Construct(state.dataConstruction->Construct(IConst).TCMasterConst).TransVisBeamCoef);
                VTRatio = VTNow / VTMaster;
            }
        }

        bool ShadedOrDiffusingGlassWin = state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel &&
                                         (IS_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) || state.dataSurface->SurfWinSolarDiffusing(IWin));

        // Loop over reference points
        for (int IL = 1; IL <= NREFPT; ++IL) {

            // Daylight factors for current sun position
            for (ISky = 1; ISky <= 4; ++ISky) {

                // ===Bare window===
                DFSKHR(1, ISky) =
                    VTRatio *
                    (state.dataGlobal->WeightNow * thisDaylightControl.DaylIllFacSky(state.dataGlobal->HourOfDay, 1, ISky, IL, loop) +
                     state.dataGlobal->WeightPreviousHour * thisDaylightControl.DaylIllFacSky(state.dataGlobal->PreviousHour, 1, ISky, IL, loop));

                if (ISky == 1)
                    DFSUHR(1) =
                        VTRatio *
                        (state.dataGlobal->WeightNow * (thisDaylightControl.DaylIllFacSun(state.dataGlobal->HourOfDay, 1, IL, loop) +
                                                        thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, 1, IL, loop)) +
                         state.dataGlobal->WeightPreviousHour * (thisDaylightControl.DaylIllFacSun(state.dataGlobal->PreviousHour, 1, IL, loop) +
                                                                 thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, 1, IL, loop)));

                BFSKHR(1, ISky) =
                    VTRatio *
                    (state.dataGlobal->WeightNow * thisDaylightControl.DaylBackFacSky(state.dataGlobal->HourOfDay, 1, ISky, IL, loop) +
                     state.dataGlobal->WeightPreviousHour * thisDaylightControl.DaylBackFacSky(state.dataGlobal->PreviousHour, 1, ISky, IL, loop));

                if (ISky == 1)
                    BFSUHR(1) =
                        VTRatio * (state.dataGlobal->WeightNow * (thisDaylightControl.DaylBackFacSun(state.dataGlobal->HourOfDay, 1, IL, loop) +
                                                                  thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->HourOfDay, 1, IL, loop)) +
                                   state.dataGlobal->WeightPreviousHour *
                                       (thisDaylightControl.DaylBackFacSun(state.dataGlobal->PreviousHour, 1, IL, loop) +
                                        thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->PreviousHour, 1, IL, loop)));

                SFSKHR(1, ISky) =
                    VTRatio *
                    (state.dataGlobal->WeightNow * thisDaylightControl.DaylSourceFacSky(state.dataGlobal->HourOfDay, 1, ISky, IL, loop) +
                     state.dataGlobal->WeightPreviousHour * thisDaylightControl.DaylSourceFacSky(state.dataGlobal->PreviousHour, 1, ISky, IL, loop));

                if (ISky == 1)
                    SFSUHR(1) = VTRatio *
                                (state.dataGlobal->WeightNow * (thisDaylightControl.DaylSourceFacSun(state.dataGlobal->HourOfDay, 1, IL, loop) +
                                                                thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->HourOfDay, 1, IL, loop)) +
                                 state.dataGlobal->WeightPreviousHour *
                                     (thisDaylightControl.DaylSourceFacSun(state.dataGlobal->PreviousHour, 1, IL, loop) +
                                      thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->PreviousHour, 1, IL, loop)));

                if (ShadedOrDiffusingGlassWin) {

                    // ===Shaded window or window with diffusing glass===
                    if (!state.dataSurface->SurfWinMovableSlats(IWin)) {
                        // Shade, screen, blind with fixed slats, or diffusing glass
                        DFSKHR(2, ISky) = VTRatio * (state.dataGlobal->WeightNow *
                                                         thisDaylightControl.DaylIllFacSky(state.dataGlobal->HourOfDay, 2, ISky, IL, loop) +
                                                     state.dataGlobal->WeightPreviousHour *
                                                         thisDaylightControl.DaylIllFacSky(state.dataGlobal->PreviousHour, 2, ISky, IL, loop));

                        if (ISky == 1) {
                            DFSUHR(2) =
                                VTRatio * (state.dataGlobal->WeightNow * thisDaylightControl.DaylIllFacSun(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                           state.dataGlobal->WeightPreviousHour *
                                               thisDaylightControl.DaylIllFacSun(state.dataGlobal->PreviousHour, 2, IL, loop));

                            if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin))
                                DFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow *
                                                            thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                                        state.dataGlobal->WeightPreviousHour *
                                                            thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, 2, IL, loop));
                        }

                        BFSKHR(2, ISky) = VTRatio * (state.dataGlobal->WeightNow *
                                                         thisDaylightControl.DaylBackFacSky(state.dataGlobal->HourOfDay, 2, ISky, IL, loop) +
                                                     state.dataGlobal->WeightPreviousHour *
                                                         thisDaylightControl.DaylBackFacSky(state.dataGlobal->PreviousHour, 2, ISky, IL, loop));

                        if (ISky == 1) {
                            BFSUHR(2) = VTRatio *
                                        (state.dataGlobal->WeightNow * thisDaylightControl.DaylBackFacSun(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                         state.dataGlobal->WeightPreviousHour *
                                             thisDaylightControl.DaylBackFacSun(state.dataGlobal->PreviousHour, 2, IL, loop));
                            if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin))
                                BFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow *
                                                            thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                                        state.dataGlobal->WeightPreviousHour *
                                                            thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->PreviousHour, 2, IL, loop));
                        }

                        SFSKHR(2, ISky) = VTRatio * (state.dataGlobal->WeightNow *
                                                         thisDaylightControl.DaylSourceFacSky(state.dataGlobal->HourOfDay, 2, ISky, IL, loop) +
                                                     state.dataGlobal->WeightPreviousHour *
                                                         thisDaylightControl.DaylSourceFacSky(state.dataGlobal->PreviousHour, 2, ISky, IL, loop));

                        if (ISky == 1) {
                            SFSUHR(2) = VTRatio * (state.dataGlobal->WeightNow *
                                                       thisDaylightControl.DaylSourceFacSun(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                                   state.dataGlobal->WeightPreviousHour *
                                                       thisDaylightControl.DaylSourceFacSun(state.dataGlobal->PreviousHour, 2, IL, loop));
                            if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin))
                                SFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow *
                                                            thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->HourOfDay, 2, IL, loop) +
                                                        state.dataGlobal->WeightPreviousHour *
                                                            thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->PreviousHour, 2, IL, loop));
                        }

                    } else { // Blind with movable slats
                        int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(IWin);
                        Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(IWin);
                        Real64 DaylIllFacSkyNow = General::InterpGeneral(
                            thisDaylightControl.DaylIllFacSky(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylIllFacSky(
                                state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);
                        Real64 DaylBackFacSkyNow = General::InterpGeneral(
                            thisDaylightControl.DaylBackFacSky(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylBackFacSky(
                                state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);
                        Real64 DaylSourceFacSkyNow = General::InterpGeneral(
                            thisDaylightControl.DaylSourceFacSky(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylSourceFacSky(
                                state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);
                        Real64 DaylIllFacSkyPrev = General::InterpGeneral(
                            thisDaylightControl.DaylIllFacSky(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylIllFacSky(
                                state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);
                        Real64 DaylBackFacSkyPrev = General::InterpGeneral(
                            thisDaylightControl.DaylBackFacSky(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylBackFacSky(
                                state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);
                        Real64 DaylSourceFacSkyPrev = General::InterpGeneral(
                            thisDaylightControl.DaylSourceFacSky(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ISky, IL, loop),
                            thisDaylightControl.DaylSourceFacSky(
                                state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, IL, loop),
                            SurfWinSlatsAngInterpFac);

                        DFSKHR(2, ISky) =
                            VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSkyNow + state.dataGlobal->WeightPreviousHour * DaylIllFacSkyPrev);
                        BFSKHR(2, ISky) =
                            VTRatio * (state.dataGlobal->WeightNow * DaylBackFacSkyNow + state.dataGlobal->WeightPreviousHour * DaylBackFacSkyPrev);
                        SFSKHR(2, ISky) = VTRatio * (state.dataGlobal->WeightNow * DaylSourceFacSkyNow +
                                                     state.dataGlobal->WeightPreviousHour * DaylSourceFacSkyPrev);

                        if (ISky == 1) {
                            Real64 DaylIllFacSunNow = General::InterpGeneral(
                                thisDaylightControl.DaylIllFacSun(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylIllFacSun(
                                    state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylBackFacSunNow = General::InterpGeneral(
                                thisDaylightControl.DaylBackFacSun(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylBackFacSun(
                                    state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylSourceFacSunNow = General::InterpGeneral(
                                thisDaylightControl.DaylSourceFacSun(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylSourceFacSun(
                                    state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylIllFacSunPrev = General::InterpGeneral(
                                thisDaylightControl.DaylIllFacSun(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylIllFacSun(
                                    state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylBackFacSunPrev = General::InterpGeneral(
                                thisDaylightControl.DaylBackFacSun(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylBackFacSun(
                                    state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylSourceFacSunPrev = General::InterpGeneral(
                                thisDaylightControl.DaylSourceFacSun(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                thisDaylightControl.DaylSourceFacSun(
                                    state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                SurfWinSlatsAngInterpFac);
                            DFSUHR(2) =
                                VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSunNow + state.dataGlobal->WeightPreviousHour * DaylIllFacSunPrev);
                            BFSUHR(2) = VTRatio *
                                        (state.dataGlobal->WeightNow * DaylBackFacSunNow + state.dataGlobal->WeightPreviousHour * DaylBackFacSunPrev);
                            SFSUHR(2) = VTRatio * (state.dataGlobal->WeightNow * DaylSourceFacSunNow +
                                                   state.dataGlobal->WeightPreviousHour * DaylSourceFacSunPrev);

                            // We add the contribution from the solar disk if slats do not block beam solar
                            // TH CR 8010, DaylIllFacSunDisk needs to be interpolated
                            if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin)) {
                                Real64 DaylIllFacSunDiskNow = General::InterpGeneral(
                                    thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylIllFacSunDisk(
                                        state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylBackFacSunDiskNow = General::InterpGeneral(
                                    thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylBackFacSunDisk(
                                        state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylSourceFacSunDiskNow = General::InterpGeneral(
                                    thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylSourceFacSunDisk(
                                        state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylIllFacSunDiskPrev = General::InterpGeneral(
                                    thisDaylightControl.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylIllFacSunDisk(
                                        state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylBackFacSunDiskPrev = General::InterpGeneral(
                                    thisDaylightControl.DaylBackFacSunDisk(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylBackFacSunDisk(
                                        state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylSourceFacSunDiskPrev = General::InterpGeneral(
                                    thisDaylightControl.DaylSourceFacSunDisk(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, IL, loop),
                                    thisDaylightControl.DaylSourceFacSunDisk(
                                        state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), IL, loop),
                                    SurfWinSlatsAngInterpFac);
                                DFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSunDiskNow +
                                                        state.dataGlobal->WeightPreviousHour * DaylIllFacSunDiskPrev);
                                BFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow * DaylBackFacSunDiskNow +
                                                        state.dataGlobal->WeightPreviousHour * DaylBackFacSunDiskPrev);
                                SFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow * DaylSourceFacSunDiskNow +
                                                        state.dataGlobal->WeightPreviousHour * DaylSourceFacSunDiskPrev);
                            }
                        }
                    } // End of check if window has blind with movable slats
                }     // End of check if window is shaded or has diffusing glass
            }         // End of sky type loop, ISky

            // Get illuminance at ref point from bare and shaded window by
            // multiplying daylight factors by exterior horizontal illuminance

            // Adding 0.001 in the following prevents zero HorIllSky in early morning or late evening when sun
            // is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
            for (ISky = 1; ISky <= 4; ++ISky) {
                // HorIllSky(ISky) = WeightNow * GILSK(ISky,HourOfDay) + WeightNextHour * GILSK(ISky,NextHour) + 0.001
                HorIllSky(ISky) = state.dataGlobal->WeightNow * state.dataDaylightingManager->GILSK(state.dataGlobal->HourOfDay, ISky) +
                                  state.dataGlobal->WeightPreviousHour * state.dataDaylightingManager->GILSK(state.dataGlobal->PreviousHour, ISky) +
                                  0.001;
            }

            // HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
            // which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
            // also calculated in DayltgLuminousEfficacy.

            HorIllSkyFac = state.dataEnvrn->HISKF / ((1 - SkyWeight) * HorIllSky(ISky2) + SkyWeight * HorIllSky(ISky1));

            for (int IS = 1; IS <= 2; ++IS) {
                if (IS == 2 && !ShadedOrDiffusingGlassWin) break;

                thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL) =
                    DFSUHR(IS) * state.dataEnvrn->HISUNF +
                    HorIllSkyFac * (DFSKHR(IS, ISky1) * SkyWeight * HorIllSky(ISky1) + DFSKHR(IS, ISky2) * (1.0 - SkyWeight) * HorIllSky(ISky2));
                thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL) =
                    BFSUHR(IS) * state.dataEnvrn->HISUNF +
                    HorIllSkyFac * (BFSKHR(IS, ISky1) * SkyWeight * HorIllSky(ISky1) + BFSKHR(IS, ISky2) * (1.0 - SkyWeight) * HorIllSky(ISky2));

                thisDaylightControl.SourceLumFromWinAtRefPt(loop, IS, IL) =
                    SFSUHR(IS) * state.dataEnvrn->HISUNF +
                    HorIllSkyFac * (SFSKHR(IS, ISky1) * SkyWeight * HorIllSky(ISky1) + SFSKHR(IS, ISky2) * (1.0 - SkyWeight) * HorIllSky(ISky2));

                thisDaylightControl.SourceLumFromWinAtRefPt(loop, IS, IL) = max(thisDaylightControl.SourceLumFromWinAtRefPt(loop, IS, IL), 0.0);

                // Added TH 1/21/2010 - save the original clear and dark (fully switched) states'
                //  zone daylighting values, needed for switachable glazings
                tmpIllumFromWinAtRefPt(loop, IS, IL) = thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL);
                tmpBackLumFromWinAtRefPt(loop, IS, IL) = thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL);
                tmpSourceLumFromWinAtRefPt(loop, IS, IL) = thisDaylightControl.SourceLumFromWinAtRefPt(loop, IS, IL);
            } // IS

        } // End of reference point loop, IL
    }     // End of first loop over exterior windows associated with this zone

    // Initialize flag that one or more windows has switchable glazing
    // control that adjusts visible transmittance to just meet dayltg setpoint
    // (and the window has not already been switched)
    ISWFLG = 0;

    // Second loop over windows. Find total daylight illuminance and background luminance
    // for each ref pt from all exterior windows associated with the zone.  Use shading flags.
    // This illuminance excludes contribution of inter-reflected illuminance produced by solar
    // entering the zone through interior windows (which is calculated in DayltgInterReflIllFrIntWins.

    for (int loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
        int IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);
        ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
        if (state.dataSurface->Surface(IWin).HasShadeControl && ISWFLG == 0) {
            if (state.dataSurface->WindowShadingControl(ICtrl).ShadingControlType == WindowShadingControlType::MeetDaylIlumSetp &&
                state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::GlassConditionallyLightened)
                ISWFLG = 1;
        }

        // Determine if illuminance contribution is from bare or shaded window
        //  For switchable glazings with shading control type of WSCT_MeetDaylIlumSetp,
        //   the shading flag is initialized at GlassConditionallyLightened (20), and
        //   the window is initialized at clear state: IS = 1
        //  For other windows with glare control, the shading flag is initialized at >10, to be determined
        int IS = findWinShadingStatus(state, IWin);

        for (int IL = 1; IL <= NREFPT; ++IL) {
            state.dataDaylightingManager->DaylIllum(IL) += thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL);
            thisDaylightControl.BacLum(IL) += thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL);
        }
    } // End of second window loop over exterior windows associated with this zone

    // Optical switching control (e.g. electrochromic glass) to adjust
    // window's vis trans downward so daylight level equals or is as
    // close as possible to the illuminance setpoint at first reference point.
    // Assumes vis trans in the fully switched state is less than that in the
    // unswitched state. Assumes some windows in a space may have this control and
    // others not.

    auto &DILLSW = state.dataDaylightingManager->DILLSW;
    auto &DILLUN = state.dataDaylightingManager->DILLUN;
    auto &previously_shaded = state.dataDaylightingManager->previously_shaded;

    // If daylight illuminance is above setpoint, allow switching
    if (ISWFLG != 0 && state.dataDaylightingManager->DaylIllum(1) > SetPnt(1)) {

        // Third loop over windows.  Get illuminance at ref pt 1 from
        // windows that can be switched (DILLSW) with a group and those that can't (DILLUN).
        // Windows that can be switched are initially in the unswitched state. For subsequent
        // groups the windows in previous groups are fully switched.
        DILLSW = 0.0;
        DILLUN = 0.0;
        previously_shaded = false;

        int count = 0;
        for (std::size_t igroup = 1; igroup <= thisDaylightControl.ShadeDeployOrderExtWins.size(); igroup++) {
            std::vector<int> const &listOfExtWin = thisDaylightControl.ShadeDeployOrderExtWins[igroup - 1];
            for (const auto IWin : listOfExtWin) {
                ++count;
                // need to map back to the original order of the "loop" to not change all the other data structures
                int loop = thisDaylightControl.MapShdOrdToLoopNum(count);
                if (loop > 0) {
                    ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
                    int IS = findWinShadingStatus(state, IWin);
                    if (state.dataSurface->Surface(IWin).HasShadeControl) {
                        if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::GlassConditionallyLightened &&
                            state.dataSurface->WindowShadingControl(ICtrl).ShadingControlType == WindowShadingControlType::MeetDaylIlumSetp &&
                            !previously_shaded(loop)) {
                            DILLSW(igroup) += thisDaylightControl.IllumFromWinAtRefPt(loop, IS, 1);
                            previously_shaded(loop) = true;
                        } else {
                            if (!previously_shaded(loop)) {
                                DILLUN(igroup) += thisDaylightControl.IllumFromWinAtRefPt(loop, IS, 1);
                            } else {
                                DILLUN(igroup) += thisDaylightControl.IllumFromWinAtRefPt(loop, 2, 1); // use the shaded state if previously shaded
                            }
                        }
                    }
                }
            }
        } // End of third window loop, IWin

        // Transmittance multiplier
        for (std::size_t igroup = 1; igroup <= thisDaylightControl.ShadeDeployOrderExtWins.size(); igroup++) {
            state.dataDaylightingManager->ASETIL(igroup) = (SetPnt(1) - DILLUN(igroup)) / (DILLSW(igroup) + 0.00001);
        }

        // ASETIL < 1 means there's enough light, so check for switching

        // Fourth loop over windows to determine which to switch
        // iterate in the order that the shades are specified in WindowShadeControl
        count = 0;
        breakOuterLoop = false;
        continueOuterLoop = false;
        for (std::size_t igroup = 1; igroup <= thisDaylightControl.ShadeDeployOrderExtWins.size(); igroup++) {

            std::vector<int> const &listOfExtWin = thisDaylightControl.ShadeDeployOrderExtWins[igroup - 1];
            auto &thisTVIS1 = state.dataDaylightingManager->TVIS1(igroup);
            auto &thisTVIS2 = state.dataDaylightingManager->TVIS2(igroup);
            auto &thisASETIL = state.dataDaylightingManager->ASETIL(igroup);

            for (const auto IWin : listOfExtWin) {
                ++count;
                // need to map back to the original order of the "loop" to not change all the other data structures
                int loop = thisDaylightControl.MapShdOrdToLoopNum(count);
                if (loop > 0) {
                    if (thisASETIL < 1.0) {

                        ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
                        if (!state.dataSurface->Surface(IWin).HasShadeControl) {
                            continueOuterLoop = true;
                            continue;
                        }
                        if (state.dataSurface->SurfWinShadingFlag(IWin) != WinShadingType::GlassConditionallyLightened ||
                            state.dataSurface->WindowShadingControl(ICtrl).ShadingControlType != WindowShadingControlType::MeetDaylIlumSetp) {
                            continueOuterLoop = true;
                            continue;
                        }

                        int const IConst = state.dataSurface->SurfActiveConstruction(IWin);
                        // Vis trans at normal incidence of unswitched glass
                        thisTVIS1 = General::POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef) *
                                    state.dataSurface->SurfWinGlazedFrac(IWin);

                        // Vis trans at normal incidence of fully switched glass
                        int const IConstShaded = state.dataSurface->Surface(IWin).activeShadedConstruction;
                        thisTVIS2 = General::POLYF(1.0, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) *
                                    state.dataSurface->SurfWinGlazedFrac(IWin);

                        // Reset shading flag to indicate that window is shaded by being partially or fully switched
                        state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::SwitchableGlazing;

                        // ASETIL < 0 means illuminance from non-daylight-switchable windows exceeds setpoint,
                        // so completely switch all daylight-switchable windows to minimize solar gain
                        if (thisASETIL <= 0.0) {
                            state.dataSurface->SurfWinSwitchingFactor(IWin) = 1.0;
                            state.dataSurface->SurfWinVisTransSelected(IWin) = thisTVIS2;
                        } else {
                            // Case where 0 < ASETIL < 1: darken glass in all
                            // daylight-switchable windows to just meet illuminance setpoint
                            // From this equation: SETPNT(1) = DILLUN + DILLSW/TVIS1 * VisTransSelected
                            state.dataSurface->SurfWinVisTransSelected(IWin) = max(thisTVIS2, thisASETIL * thisTVIS1) + 0.000001;
                            state.dataSurface->SurfWinSwitchingFactor(IWin) =
                                (thisTVIS1 - state.dataSurface->SurfWinVisTransSelected(IWin)) / (thisTVIS1 - thisTVIS2 + 0.000001);
                            // bound switching factor between 0 and 1
                            state.dataSurface->SurfWinSwitchingFactor(IWin) = min(1.0, state.dataSurface->SurfWinSwitchingFactor(IWin));
                            state.dataSurface->SurfWinSwitchingFactor(IWin) = max(0.0, state.dataSurface->SurfWinSwitchingFactor(IWin));
                        }

                        // Adjust daylight quantities based on ratio between switched and unswitched visible transmittance
                        for (int IL = 1; IL <= NREFPT; ++IL) {
                            // DaylIllum(IL) and BacLum(IL) were calculated at the clear state: IS = 1,
                            //  and need to adjusted for intermediate switched state at VisTransSelected: IS = 2
                            int IS = 1;
                            VTRAT = state.dataSurface->SurfWinVisTransSelected(IWin) / (thisTVIS1 + 0.000001);
                            state.dataDaylightingManager->DaylIllum(IL) += (VTRAT - 1.0) * thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL);
                            thisDaylightControl.BacLum(IL) += (VTRAT - 1.0) * thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL);

                            // Adjust illum, background illum and source luminance for this window in intermediate switched state
                            //  for later use in the DayltgGlare calc because SurfaceWindow(IWin)%ShadingFlag = WinShadingType::SwitchableGlazing = 2
                            IS = 2;
                            VTRAT = state.dataSurface->SurfWinVisTransSelected(IWin) / (thisTVIS2 + 0.000001);
                            thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL) = VTRAT * tmpIllumFromWinAtRefPt(loop, IS, IL);
                            thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL) = VTRAT * tmpBackLumFromWinAtRefPt(loop, IS, IL);
                            thisDaylightControl.SourceLumFromWinAtRefPt(loop, IS, IL) = VTRAT * tmpSourceLumFromWinAtRefPt(loop, IS, IL);
                        } // IL
                    }     // ASETIL < 1
                }
                // If new daylight does not exceed the illuminance setpoint, done, no more checking other groups of switchable glazings
                if (state.dataDaylightingManager->DaylIllum(1) <= SetPnt(1)) {
                    breakOuterLoop = true;
                    break;
                }
            }
            if (breakOuterLoop) break;
            if (continueOuterLoop) continue;
        } // End of fourth window loop

    } // ISWFLG /= 0 .AND. DaylIllum(1) > SETPNT(1)

    // Calculate glare index at each reference point assuming the daylight illuminance setpoint is
    //  met at both reference points, either by daylight or electric lights
    for (int IL = 1; IL <= NREFPT; ++IL) {
        BACL = max(SetPnt(IL) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect / DataGlobalConstants::Pi,
                   thisDaylightControl.BacLum(IL));
        // DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,1,loop) for unshaded windows, and
        //  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded windows
        DayltgGlare(state, IL, BACL, GLRNDX(IL), daylightCtrlNum);
    }

    // Check if glare level is less than maximum allowed at each ref pt.  If maximum
    // is exceeded at either ref pt, attempt to reduce glare to acceptable level by closing
    // shading device on windows that have shades that have not already been closed.
    GlareFlag = false;
    for (int IL = 1; IL <= NREFPT; ++IL) {
        if (GLRNDX(IL) > thisDaylightControl.MaxGlareallowed) {
            GlareFlag = true;
            break;
        }
    }

    auto &WDAYIL = state.dataDaylightingManager->WDAYIL;
    auto &WBACLU = state.dataDaylightingManager->WBACLU;
    auto &RDAYIL = state.dataDaylightingManager->RDAYIL;
    auto &RBACLU = state.dataDaylightingManager->RBACLU;
    if (GlareFlag) {
        // Glare is too high at a ref pt.  Loop through windows.
        int count = 0;

        continueOuterLoop = false;
        for (std::size_t igroup = 1; igroup <= thisDaylightControl.ShadeDeployOrderExtWins.size(); igroup++) {

            std::vector<int> const &listOfExtWin = thisDaylightControl.ShadeDeployOrderExtWins[igroup - 1];
            auto &thisTVIS1 = state.dataDaylightingManager->TVIS1(igroup);
            auto &thisTVIS2 = state.dataDaylightingManager->TVIS1(igroup);

            int countBeforeListOfExtWinLoop = count;
            bool atLeastOneGlareControlIsActive = false;

            for (const auto IWin : listOfExtWin) {
                ++count;
                // need to map back to the original order of the "loop" to not change all the other data structures
                int loop = thisDaylightControl.MapShdOrdToLoopNum(count);
                if (loop > 0) {
                    // Check if window is eligible for glare control
                    // TH 1/21/2010. Switchable glazings already in partially switched state
                    //  should be allowed to further dim to control glare
                    // if (SurfWinShadingFlag(IWin) <= BGBlind && SurfWinShadingFlag(IWin) != SwitchableGlazing) {
                    if (NOT_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) || ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(IWin)) ||
                        ANY_BLIND(state.dataSurface->SurfWinShadingFlag(IWin))) {
                        continueOuterLoop = false;
                        continue;
                    }
                    ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
                    if (!state.dataSurface->Surface(IWin).HasShadeControl) {
                        continueOuterLoop = false;
                        continue;
                    }
                    if (state.dataSurface->WindowShadingControl(ICtrl).GlareControlIsActive) {
                        atLeastOneGlareControlIsActive = true;

                        // Illuminance (WDAYIL) and background luminance (WBACLU) contribution from this
                        // window without shading (IS=1) and with shading (IS=2) for each ref pt
                        //  For switchable windows, this may be partially switched rather than fully dark
                        for (int IL = 1; IL <= NREFPT; ++IL) {
                            for (int IS = 1; IS <= 2; ++IS) {
                                WDAYIL(IS, IL, igroup) = thisDaylightControl.IllumFromWinAtRefPt(loop, IS, IL);
                                WBACLU(IS, IL, igroup) = thisDaylightControl.BackLumFromWinAtRefPt(loop, IS, IL);
                            }
                        }

                        // Recalculate illuminance and glare with shading on this window.
                        //  For switchable glazings, this is the fully switched (dark) state
                        for (int IL = 1; IL <= NREFPT; ++IL) {
                            if (state.dataSurface->SurfWinShadingFlag(IWin) != WinShadingType::SwitchableGlazing) {
                                // for non switchable glazings or switchable glazings not switched yet (still in clear state)
                                //  SurfaceWindow(IWin)%ShadingFlag = WinShadingFlag::GlassConditionallyLightened
                                RDAYIL(IL, igroup) = state.dataDaylightingManager->DaylIllum(IL) - WDAYIL(1, IL, igroup) + WDAYIL(2, IL, igroup);
                                RBACLU(IL, igroup) = thisDaylightControl.BacLum(IL) - WBACLU(1, IL, igroup) + WBACLU(2, IL, igroup);
                            } else {
                                // switchable glazings already in partially switched state when calc the RDAYIL(IL) & RBACLU(IL)
                                RDAYIL(IL, igroup) =
                                    state.dataDaylightingManager->DaylIllum(IL) - WDAYIL(2, IL, igroup) + tmpIllumFromWinAtRefPt(loop, 2, IL);
                                RBACLU(IL, igroup) = thisDaylightControl.BacLum(IL) - WBACLU(2, IL, igroup) + tmpBackLumFromWinAtRefPt(loop, 2, IL);
                            }
                        }

                        if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::GlassConditionallyLightened)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::SwitchableGlazing;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::IntShadeConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::IntShade;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::ExtShadeConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::ExtShade;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::IntBlindConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::IntBlind;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::ExtBlindConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::ExtBlind;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::BGShadeConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::BGShade;
                        else if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::BGBlindConditionallyOff)
                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::BGBlind;

                        // For switchable glazings, it is switched to fully dark state,
                        // update ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for use in DayltgGlare
                        if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::SwitchableGlazing) {
                            for (int IL = 1; IL <= NREFPT; ++IL) {
                                thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, IL) = tmpSourceLumFromWinAtRefPt(loop, 2, IL);
                                thisDaylightControl.IllumFromWinAtRefPt(loop, 2, IL) = tmpIllumFromWinAtRefPt(loop, 2, IL);
                                thisDaylightControl.BackLumFromWinAtRefPt(loop, 2, IL) = tmpBackLumFromWinAtRefPt(loop, 2, IL);
                            }

                            int const IConst = state.dataSurface->SurfActiveConstruction(IWin);
                            // Vis trans at normal incidence of unswitched glass
                            thisTVIS1 = General::POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef) *
                                        state.dataSurface->SurfWinGlazedFrac(IWin);

                            // Vis trans at normal incidence of fully switched glass
                            int const IConstShaded = state.dataSurface->Surface(IWin).activeShadedConstruction;
                            thisTVIS2 = General::POLYF(1.0, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) *
                                        state.dataSurface->SurfWinGlazedFrac(IWin);
                        }
                    }
                }
            }
            if (continueOuterLoop) continue;

            if (atLeastOneGlareControlIsActive) {

                // Re-calc daylight and glare at shaded state. For switchable glazings, it is the fully dark state.
                for (int IL = 1; IL <= NREFPT; ++IL) {
                    BACL = max(SetPnt(IL) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect / DataGlobalConstants::Pi,
                               RBACLU(IL, igroup));
                    // DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded state
                    DayltgGlare(state, IL, BACL, GLRNEW(IL), daylightCtrlNum);
                }

                // Check if the shading did not improve the glare conditions
                //
                // blnCycle when true resets the specific window to its non-shaded condition. A later comment says
                //      Shading this window has not improved the glare situation.
                //      Reset shading flag to no shading condition, go to next window.
                //
                // If the original glare was too high at all reference points and the new glare is lower at all reference points it is good, don't
                // reset it. For each reference point, if the original glare was too high but ok at other reference points and the glare gets
                // lower at the reference and stays ok at the other reference points it is good, don't reset it.
                //
                // The old comments when there were only two reference points were:
                //     One ref pt;  go to next window if glare has increased.
                //     Two ref pts.  There are three cases depending on glare values.
                //         (1) Initial glare too high at both ref pts.  Deploy shading on
                //             this window if this decreases glare at both ref pts.
                //         (2) Initial glare too high only at first ref pt.  Deploy shading
                //             on this window if glare at first ref pt decreases and
                //             glare at second ref pt stays below max.
                //         (3) Initial glare too high at second ref pt.  Deploy shading if glare
                //             at second ref pt decreases and glare at first ref pt stays below max.
                //
                // The approach taken is just to count the number of reference points that fulfill the individual requirements and see if it
                // covers all the reference points.
                int numRefPtOldAboveMaxNewBelowOld = 0;
                int numRefPtOldBelowMaxNewBelowMax = 0;
                for (int IL = 1; IL <= NREFPT; ++IL) {
                    if (GLRNDX(IL) > thisDaylightControl.MaxGlareallowed && GLRNEW(IL) <= GLRNDX(IL)) ++numRefPtOldAboveMaxNewBelowOld;
                    if (GLRNDX(IL) <= thisDaylightControl.MaxGlareallowed && GLRNEW(IL) <= thisDaylightControl.MaxGlareallowed)
                        ++numRefPtOldBelowMaxNewBelowMax;
                }
                blnCycle = true;
                if ((numRefPtOldAboveMaxNewBelowOld + numRefPtOldBelowMaxNewBelowMax) == NREFPT) blnCycle = false;
            }

            // restore the count to the value prior to the last loop through the group of exterior windows
            count = countBeforeListOfExtWinLoop;
            breakOuterLoop = false;

            for (const auto IWin : listOfExtWin) {
                ++count;
                // need to map back to the original order of the "loop" to not change all the other data structures
                int loop = thisDaylightControl.MapShdOrdToLoopNum(count);
                if (loop > 0) {
                    // if (SurfWinShadingFlag(IWin) <= BGBlind && SurfWinShadingFlag(IWin) != SwitchableGlazing) {
                    if (NOT_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) || ANY_SHADE_SCREEN(state.dataSurface->SurfWinShadingFlag(IWin)) ||
                        ANY_BLIND(state.dataSurface->SurfWinShadingFlag(IWin)))
                        continue;

                    ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
                    if (!state.dataSurface->Surface(IWin).HasShadeControl) continue;
                    if (state.dataSurface->WindowShadingControl(ICtrl).GlareControlIsActive) {

                        // Shading this window has not improved the glare situation.
                        // Reset shading flag to no shading condition, go to next window.
                        if (blnCycle) {
                            //  for switchable glazings, reset properties to clear state or partial switched state?
                            if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::SwitchableGlazing) {
                                state.dataSurface->SurfWinSwitchingFactor(IWin) = 0.0;
                                state.dataSurface->SurfWinVisTransSelected(IWin) = thisTVIS1;

                                // RESET properties for fully dark state
                                for (int IL = 1; IL <= NREFPT; ++IL) {
                                    thisDaylightControl.IllumFromWinAtRefPt(loop, 2, IL) = tmpIllumFromWinAtRefPt(loop, 2, IL);
                                    thisDaylightControl.BackLumFromWinAtRefPt(loop, 2, IL) = tmpBackLumFromWinAtRefPt(loop, 2, IL);
                                    thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, IL) = tmpSourceLumFromWinAtRefPt(loop, 2, IL);
                                }
                            }

                            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::ShadeOff;
                            continue;
                        }

                        // Shading this window has improved the glare situation.
                        // Reset background luminance, glare index, and daylight illuminance at each ref pt.
                        // For switchable glazings, this is fully switched, dark state
                        for (int IL = 1; IL <= NREFPT; ++IL) {
                            thisDaylightControl.BacLum(IL) = RBACLU(IL, igroup);
                            GLRNDX(IL) = GLRNEW(IL);
                            state.dataDaylightingManager->DaylIllum(IL) = RDAYIL(IL, igroup);
                        }

                        // TH comments (5/22/2009): seems for EC windows, if the calculated glare exceeds the max setpoint,
                        //  the EC windows will be reset to fully dark state which significantly reduces the available daylight.
                        //  A better way is to dim the EC windows as necessary just to meet the glare index, which will still
                        //  provide more daylight while not exceeding the max glare! The question is then how to set the
                        //  SwitchingFactor to just meet the glare index.
                        //  This was addressed in CR 7984 for E+ 5.0. 1/19/2010

                        // If switchable glazing, set switching factor to 1: fully switched.
                        if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::SwitchableGlazing) {
                            // tmpSWFactor0 = SurfaceWindow( IWin ).SwitchingFactor; // save original
                            // switching  factor
                            ////Unused Set but never used
                            state.dataSurface->SurfWinSwitchingFactor(IWin) = 1.0;
                            state.dataSurface->SurfWinVisTransSelected(IWin) = thisTVIS2;

                            // restore fully dark values
                            for (int IL = 1; IL <= NREFPT; ++IL) {
                                WDAYIL(2, IL, igroup) = tmpIllumFromWinAtRefPt(loop, 2, IL);
                                WBACLU(2, IL, igroup) = tmpBackLumFromWinAtRefPt(loop, 2, IL);
                                thisDaylightControl.IllumFromWinAtRefPt(loop, 2, IL) = tmpIllumFromWinAtRefPt(loop, 2, IL);
                                thisDaylightControl.BackLumFromWinAtRefPt(loop, 2, IL) = tmpBackLumFromWinAtRefPt(loop, 2, IL);
                                thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, IL) = tmpSourceLumFromWinAtRefPt(loop, 2, IL);
                            }
                        }

                        // Check if glare now acceptable at each ref pt.
                        GlareOK = false;
                        if (NREFPT == 1) {
                            if (GLRNDX(1) <= thisDaylightControl.MaxGlareallowed) GlareOK = true;
                        } else if (NREFPT > 1) {
                            if (GLRNDX(1) <= thisDaylightControl.MaxGlareallowed && GLRNDX(2) <= thisDaylightControl.MaxGlareallowed) GlareOK = true;
                        }

                        if (GlareOK) {
                            if (state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::SwitchableGlazing &&
                                state.dataSurface->WindowShadingControl(ICtrl).ShadingControlType == WindowShadingControlType::MeetDaylIlumSetp) {
                                // Added TH 1/14/2010
                                // Only for switchable glazings with MeetDaylightIlluminanceSetpoint control
                                // The glazing is in fully dark state, it might lighten a bit to provide more daylight
                                //  while meeting maximum discomfort glare index
                                // Iteration to find the right switching factor meeting the glare index

                                // get fully dark state values
                                tmpSWSL1 = tmpSourceLumFromWinAtRefPt(loop, 2, 1);
                                if (NREFPT > 1) tmpSWSL2 = tmpSourceLumFromWinAtRefPt(loop, 2, 2);

                                // use simple fixed step search in iteraction, can be improved in future
                                tmpSWFactor = 1.0 - tmpSWIterStep;
                                while (tmpSWFactor > 0) {
                                    // calc new glare at new switching state
                                    for (int IL = 1; IL <= NREFPT; ++IL) {
                                        RDAYIL(IL, igroup) = state.dataDaylightingManager->DaylIllum(IL) +
                                                             (WDAYIL(1, IL, igroup) - WDAYIL(2, IL, igroup)) * (1.0 - tmpSWFactor);
                                        RBACLU(IL, igroup) =
                                            thisDaylightControl.BacLum(IL) + (WBACLU(1, IL, igroup) - WBACLU(2, IL, igroup)) * (1.0 - tmpSWFactor);
                                        BACL = max(SetPnt(IL) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect /
                                                       DataGlobalConstants::Pi,
                                                   RBACLU(IL, igroup));
                                        // needs to update SourceLumFromWinAtRefPt(IL,2,loop) before re-calc DayltgGlare
                                        tmpMult = (thisTVIS1 - (thisTVIS1 - thisTVIS2) * tmpSWFactor) / thisTVIS2;
                                        if (IL == 1) {
                                            thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, IL) = tmpSWSL1 * tmpMult;
                                        } else {
                                            thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, IL) = tmpSWSL2 * tmpMult;
                                        }
                                        // Calc new glare
                                        DayltgGlare(state, IL, BACL, GLRNEW(IL), daylightCtrlNum);
                                    }

                                    // Check whether new glare is OK
                                    GlareOK = false;
                                    if (NREFPT == 1) {
                                        if (GLRNEW(1) <= thisDaylightControl.MaxGlareallowed) GlareOK = true;
                                    } else if (NREFPT > 1) {
                                        if (GLRNEW(1) <= thisDaylightControl.MaxGlareallowed && GLRNEW(2) <= thisDaylightControl.MaxGlareallowed)
                                            GlareOK = true;
                                    }

                                    if (GlareOK) {
                                        if (tmpSWFactor >= tmpSWIterStep) {
                                            // Continue to lighten the glazing
                                            tmpSWFactor -= tmpSWIterStep;
                                            continue;
                                        } else {
                                            // Glare still OK but glazing already in clear state, no more lighten
                                            breakOuterLoop = true;
                                            break;
                                        }
                                    } else {
                                        // Glare too high, exit and use previous switching state
                                        tmpSWFactor += tmpSWIterStep;
                                        breakOuterLoop = true;
                                        break;
                                    }
                                }

                                // Final re-calculation if needed
                                if (!GlareOK) {
                                    // Glare too high, use previous state and re-calc
                                    for (int IL = 1; IL <= NREFPT; ++IL) {
                                        RDAYIL(IL, igroup) = state.dataDaylightingManager->DaylIllum(IL) +
                                                             (WDAYIL(1, IL, igroup) - WDAYIL(2, IL, igroup)) * (1.0 - tmpSWFactor);
                                        RBACLU(IL, igroup) =
                                            thisDaylightControl.BacLum(IL) + (WBACLU(1, IL, igroup) - WBACLU(2, IL, igroup)) * (1.0 - tmpSWFactor);
                                        BACL = max(SetPnt(IL) * state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect /
                                                       DataGlobalConstants::Pi,
                                                   RBACLU(IL, igroup));

                                        // needs to update SourceLumFromWinAtRefPt(IL,2,IWin) before re-calc DayltgGlare
                                        tmpMult = (thisTVIS1 - (thisTVIS1 - thisTVIS2) * tmpSWFactor) / thisTVIS2;
                                        if (IL == 1) {
                                            thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, 1) = tmpSWSL1 * tmpMult;
                                        } else {
                                            thisDaylightControl.SourceLumFromWinAtRefPt(loop, 2, 2) = tmpSWSL2 * tmpMult;
                                        }
                                        DayltgGlare(state, IL, BACL, GLRNEW(IL), daylightCtrlNum);
                                    }
                                }

                                // Update final results
                                for (int IL = 1; IL <= NREFPT; ++IL) {
                                    thisDaylightControl.BacLum(IL) = RBACLU(IL, igroup);
                                    GLRNDX(IL) = GLRNEW(IL);
                                    state.dataDaylightingManager->DaylIllum(IL) = RDAYIL(IL, igroup);

                                    tmpMult = (thisTVIS1 - (thisTVIS1 - thisTVIS2) * tmpSWFactor) / thisTVIS2;
                                    // update report variables
                                    thisDaylightControl.IllumFromWinAtRefPt(loop, 2, IL) = tmpIllumFromWinAtRefPt(loop, 2, IL) * tmpMult;
                                    thisDaylightControl.BackLumFromWinAtRefPt(loop, 2, IL) = tmpBackLumFromWinAtRefPt(loop, 2, IL) * tmpMult;
                                }
                                state.dataSurface->SurfWinSwitchingFactor(IWin) = tmpSWFactor;
                                state.dataSurface->SurfWinVisTransSelected(IWin) = thisTVIS1 - (thisTVIS1 - thisTVIS2) * tmpSWFactor;

                            } else {
                                // For un-switchable glazing or switchable glazing but not MeetDaylightIlluminaceSetpoint control,
                                // it is in shaded state and glare is ok - job is done, exit the window loop - IWin
                                breakOuterLoop = true;
                                break;
                            }
                        }
                    } // End of check if window glare control is active
                }
            } // end of for(auto IWin : listOfExtWin)
            if (breakOuterLoop) break;
        } // for group
    }     // GlareFlag

    // Loop again over windows and reset remaining shading flags that
    // are 10 or higher (i.e., conditionally off) to off
    for (int IWin = state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).WindowSurfaceFirst;
         IWin <= state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).WindowSurfaceLast;
         ++IWin) {
        if (state.dataSurface->Surface(IWin).ExtBoundCond != ExternalEnvironment) continue;
        bool anyGlareControl = BITF_TEST_ANY(BITF(state.dataSurface->SurfWinShadingFlag(IWin)),
                                             BITF(WinShadingType::IntShadeConditionallyOff) | BITF(WinShadingType::GlassConditionallyLightened) |
                                                 BITF(WinShadingType::ExtShadeConditionallyOff) | BITF(WinShadingType::IntBlindConditionallyOff) |
                                                 BITF(WinShadingType::ExtBlindConditionallyOff));
        if (anyGlareControl) {
            state.dataSurface->SurfWinShadingFlag(IWin) = WinShadingType::ShadeOff;
        }
    }

    // Variables for reporting
    for (int IL = 1; IL <= NREFPT; ++IL) {
        thisDaylightControl.DaylIllumAtRefPt(IL) = state.dataDaylightingManager->DaylIllum(IL);
        thisDaylightControl.GlareIndexAtRefPt(IL) = GLRNDX(IL);

        // added TH 12/2/2008
        if (GLRNDX(IL) > thisDaylightControl.MaxGlareallowed) {
            thisDaylightControl.TimeExceedingGlareIndexSPAtRefPt(IL) = state.dataGlobal->TimeStepZone; // fraction of hours
        } else {
            thisDaylightControl.TimeExceedingGlareIndexSPAtRefPt(IL) = 0.0;
        }

        // added TH 7/6/2009
        if (state.dataDaylightingManager->DaylIllum(IL) > thisDaylightControl.IllumSetPoint(IL)) {
            thisDaylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt(IL) = state.dataGlobal->TimeStepZone; // fraction of hours
        } else {
            thisDaylightControl.TimeExceedingDaylightIlluminanceSPAtRefPt(IL) = 0.0;
        }
    }
}

void DayltgInteriorTDDIllum(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate the TDD Pipe illuminance values

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PipeNum;                // TDD pipe object number
    Real64 TDDTransVisDiffNow;  // TDD diffuse visible transmittance at the current hour
    Real64 TDDTransVisDiffPrev; // TDD diffuse visible transmittance at the previous hour
    auto &TDDTransVisDiff = state.dataDaylightingManager->TDDTransVisDiff;
    int ISky;  // Sky type index
    int ISky1; // Sky type index values for averaging two sky types
    int ISky2;
    Real64 SkyWeight; // Weighting factor used to average two different sky types

    if (state.dataEnvrn->SkyClearness > 3.0) { // Sky is average of clear and clear turbid
        SkyWeight = min(1.0, (state.dataEnvrn->SkyClearness - 3.0) / 3.0);
        ISky1 = 1;
        ISky2 = 2;
    } else if (state.dataEnvrn->SkyClearness > 1.2) { // Sky is average of clear turbid and intermediate
        SkyWeight = (state.dataEnvrn->SkyClearness - 1.2) / 1.8;
        ISky1 = 2;
        ISky2 = 3;
    } else { // Sky is average of intermediate and overcast
        SkyWeight = min(1.0, max(0.0, (state.dataEnvrn->SkyClearness - 1.0) / 0.2, (state.dataEnvrn->SkyBrightness - 0.05) / 0.4));
        ISky1 = 3;
        ISky2 = 4;
    }

    // Calculate and report TDD visible transmittances
    for (PipeNum = 1; PipeNum <= (int)state.dataDaylightingDevicesData->TDDPipe.size(); ++PipeNum) {

        state.dataDaylightingDevicesData->TDDPipe(PipeNum).TransVisBeam =
            state.dataGlobal->WeightNow * state.dataDaylightingManager->TDDTransVisBeam(state.dataGlobal->HourOfDay, PipeNum) +
            state.dataGlobal->WeightPreviousHour * state.dataDaylightingManager->TDDTransVisBeam(state.dataGlobal->PreviousHour, PipeNum);

        for (ISky = 1; ISky <= 4; ++ISky) {
            if (state.dataDaylightingManager->TDDFluxInc(state.dataGlobal->HourOfDay, ISky, PipeNum) > 0.0) {
                TDDTransVisDiffNow = state.dataDaylightingManager->TDDFluxTrans(state.dataGlobal->HourOfDay, ISky, PipeNum) /
                                     state.dataDaylightingManager->TDDFluxInc(state.dataGlobal->HourOfDay, ISky, PipeNum);
            } else {
                TDDTransVisDiffNow = 0.0;
            }

            if (state.dataDaylightingManager->TDDFluxInc(state.dataGlobal->PreviousHour, ISky, PipeNum) > 0.0) {
                TDDTransVisDiffPrev = state.dataDaylightingManager->TDDFluxTrans(state.dataGlobal->PreviousHour, ISky, PipeNum) /
                                      state.dataDaylightingManager->TDDFluxInc(state.dataGlobal->PreviousHour, ISky, PipeNum);
            } else {
                TDDTransVisDiffPrev = 0.0;
            }

            TDDTransVisDiff(ISky) = state.dataGlobal->WeightNow * TDDTransVisDiffNow + state.dataGlobal->WeightPreviousHour * TDDTransVisDiffPrev;
        } // ISky

        state.dataDaylightingDevicesData->TDDPipe(PipeNum).TransVisDiff =
            SkyWeight * TDDTransVisDiff(ISky1) + (1.0 - SkyWeight) * TDDTransVisDiff(ISky2);
    } // PipeNum
}

void DayltgElecLightingControl(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       Mar 2004, FCW: add inter-reflected illuminance from interior windows to DaylIllum
    //                      Apr 2004, FCW: move CALL ReportIllumMap from DayltgInteriorIllum2 (DayltgInteriorMapIllum)
    //                      Apr 2010, BG NREL: remove inter-reflected illuminance to stop double counting
    //                      Aug 2012, BG NREL: added availability schedule logic

    // PURPOSE OF THIS SUBROUTINE:
    // For a daylit space, determines lighting power reduction factor due to
    // daylighting for different lighting control systems.

    // Called by InitSurfaceHeatBalance.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DLTSYS.

    using ScheduleManager::GetCurrentScheduleValue;

    Real64 TotReduction; // Electric lighting power reduction factor for a zone
    //  due to daylighting
    int NREFPT;                          // Number of daylighting reference points in a zone
    Real64 ZFTOT = 0.;                   // Fraction of zone's floor area that has daylighting controls
    int IL;                              // Reference point index
    DataDaylighting::LtgCtrlType LSYSTP; // Lighting control type: 1=continuous dimming, 2=stepped,
    //  3=continuous dimming then off
    Real64 ZFRAC; // Fraction of zone controlled by a reference point
    Real64 FL;    // Fraction electric lighting output required to meet setpoint
    Real64 FP;    // Fraction electric lighting power input required to meet setpoint
    Real64 XRAN;  // Random number between 0 and 1
    bool ScheduledAvailable;

    if ((int)state.dataDaylightingData->daylightControl.size() == 0) return;
    // Reset space power reduction factors
    for (int spaceNum = 1; spaceNum <= state.dataGlobal->numSpaces; ++spaceNum) {
        state.dataDaylightingData->spacePowerReductionFactor(spaceNum) = 1.0;
    }

    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);

        if (thisDaylightControl.DaylightMethod != DataDaylighting::DaylightingMethod::SplitFlux) {
            // Set space power reduction factors
            if (thisDaylightControl.PowerReductionFactor < 1.0) {
                if (thisDaylightControl.spaceIndex > 0) {
                    // This is a space-level daylighting control
                    state.dataDaylightingData->spacePowerReductionFactor(thisDaylightControl.spaceIndex) = thisDaylightControl.PowerReductionFactor;
                } else {
                    // This is a zone-level daylighting control
                    for (int spaceNum : state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).spaceIndexes) {
                        state.dataDaylightingData->spacePowerReductionFactor(spaceNum) = thisDaylightControl.PowerReductionFactor;
                    }
                }
            }
            continue;
        }

        TotReduction = 0.0;
        ZFTOT = 0.0;
        //  ScheduledAvailable = .TRUE.

        // check if scheduled to be available
        //  IF (ZoneDaylight(ZoneNum)%AvailSchedNum > 0) THEN
        if (GetCurrentScheduleValue(state, thisDaylightControl.AvailSchedNum) > 0.0) {
            ScheduledAvailable = true;
        } else {
            ScheduledAvailable = false;
        }
        //  ENDIF

        if (ScheduledAvailable) {
            NREFPT = thisDaylightControl.TotalDaylRefPoints;

            // Loop over reference points
            for (IL = 1; IL <= NREFPT; ++IL) {

                // Total fraction of zone that is daylit
                ZFTOT += thisDaylightControl.FracZoneDaylit(IL);

                state.dataDaylightingManager->DaylIllum(IL) = thisDaylightControl.DaylIllumAtRefPt(IL);
                if (state.dataDaylightingManager->DaylIllum(IL) >= thisDaylightControl.IllumSetPoint(IL)) {
                    FL = 0.0;
                } else {
                    FL =
                        (thisDaylightControl.IllumSetPoint(IL) - state.dataDaylightingManager->DaylIllum(IL)) / thisDaylightControl.IllumSetPoint(IL);
                }

                // BRANCH ON LIGHTING SYSTEM TYPE
                LSYSTP = thisDaylightControl.LightControlType;
                if (LSYSTP != DataDaylighting::LtgCtrlType::Stepped) {
                    // Continuously dimmable system with linear power curve
                    // Fractional output power required to meet setpoint
                    FP = 1.0;
                    // LIGHT-CTRL-TYPE = CONTINUOUS (LSYSTP = 1)
                    if (FL <= thisDaylightControl.MinLightFraction) FP = thisDaylightControl.MinPowerFraction;
                    // LIGHT-CTRL-TYPE = CONTINUOUS/OFF (LSYSTP = 3)
                    if (FL <= thisDaylightControl.MinLightFraction && LSYSTP == DataDaylighting::LtgCtrlType::ContinuousOff) FP = 0.0;
                    if (FL > thisDaylightControl.MinLightFraction && FL < 1.0)
                        FP = (FL + (1.0 - FL) * thisDaylightControl.MinPowerFraction - thisDaylightControl.MinLightFraction) /
                             (1.0 - thisDaylightControl.MinLightFraction);

                } else { // LSYSTP = 2
                    // Stepped system
                    FP = 0.0;
                    if (state.dataDaylightingManager->DaylIllum(IL) > 0.0 &&
                        state.dataDaylightingManager->DaylIllum(IL) < thisDaylightControl.IllumSetPoint(IL))
                        FP = double(int(thisDaylightControl.LightControlSteps * FL) + 1) / double(thisDaylightControl.LightControlSteps);

                    if (state.dataDaylightingManager->DaylIllum(IL) == 0.0) FP = 1.0;

                    if (thisDaylightControl.LightControlProbability < 1.0) {
                        // Manual operation.  Occupant sets lights one level too high a fraction of the time equal to
                        // 1. - ZoneDaylight(ZoneNum)%LightControlProbability.  RANDOM_NUMBER returns a random number
                        // between 0 and 1.
                        RANDOM_NUMBER(XRAN);
                        if (XRAN >= thisDaylightControl.LightControlProbability) {
                            // Set level one higher
                            if (FP < 1.0) FP += (1.0 / double(thisDaylightControl.LightControlSteps));
                        } // XRAN
                    }     // Light Control Probability < 1
                }         // Lighting System Type

                thisDaylightControl.RefPtPowerReductionFactor(IL) = FP;

                // Accumulate net ltg power reduction factor for entire zone
                ZFRAC = thisDaylightControl.FracZoneDaylit(IL);
                TotReduction += thisDaylightControl.RefPtPowerReductionFactor(IL) * ZFRAC;

            } // End of loop over reference points, IL

            // Correct for fraction of zone (1-ZFTOT) not controlled by
            // the reference points.  For this fraction (which is usually zero),
            // the electric lighting is unaffected and the power reduction
            // factor is therefore 1.0.
            TotReduction += (1.0 - ZFTOT);
        } else { // controls not currently available
            TotReduction = 1.0;
        }
        thisDaylightControl.PowerReductionFactor = TotReduction;

        // Set space power reduction factors
        if (thisDaylightControl.spaceIndex > 0) {
            // This is a space-level daylighting control
            state.dataDaylightingData->spacePowerReductionFactor(thisDaylightControl.spaceIndex) = TotReduction;
        } else {
            // This is a zone-level daylighting control
            for (int spaceNum : state.dataHeatBal->Zone(thisDaylightControl.zoneIndex).spaceIndexes) {
                state.dataDaylightingData->spacePowerReductionFactor(spaceNum) = TotReduction;
            }
        }
    } // end daylighting control loop

    //  IF(DataDaylighting::TotIllumMaps > 0 .and. .not. DoingSizing .and. .not. WarmupFlag .and. .not. KickoffSimulation) THEN
    if ((int)state.dataDaylightingData->IllumMap.size() > 0 && !state.dataGlobal->DoingSizing && !state.dataGlobal->WarmupFlag) {
        for (int mapNum = 1; mapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++mapNum) {
            if (state.dataGlobal->TimeStep == 1) state.dataDaylightingData->mapResultsToReport = false;
            for (IL = 1; IL <= state.dataDaylightingData->IllumMapCalc(mapNum).TotalMapRefPoints; ++IL) {
                state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPtHr(IL) +=
                    state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPt(IL) / double(state.dataGlobal->NumOfTimeStepInHour);
                if (state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPtHr(IL) > 0.0) {
                    state.dataDaylightingData->mapResultsToReport = true;
                    state.dataDaylightingData->mapResultsReported = true;
                }
            }
            ReportIllumMap(state, mapNum);
            if (state.dataGlobal->TimeStep == state.dataGlobal->NumOfTimeStepInHour) {
                state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPtHr = 0.0;
                state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllumAtMapPt = 0.0;
            }
        }
    }
}

Real64 DayltgGlarePositionFactor(Real64 &X, // Lateral and vertical distance of luminous window element from
                                 Real64 &Y)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // by table interpolation, evaluates the
    // Hopkinson position factor used in glare calculation
    // (Hopkinson, Petherbridge, AND Longmore -- Daylighting,
    // London, 1966, PP 307, 323).  X (Y) is the lateral
    // (vertical) distance of luminous window element from
    // horizontal line of vision, divided by horizontal distance
    // from eye of observer. The array PF contains values of
    // the position factor for X = 0, 0.5, 1.0, 1.5, 2.0, 2.5,
    // and 3.0 and Y = 0, 0.5, 1.0, 1.5, 2.0. Called by CalcDayltgCoefficients.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DPFAC.

    // Return value
    Real64 DayltgGlarePositionFactor; // Position factor

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int IX; // Lateral and vertical displacement indices
    int IY;
    Real64 X1; // Lateral and vertical displacement ratios
    Real64 Y1;
    Real64 FA; // Intermediate variables
    Real64 FB;

    // Position factor array
    static constexpr std::array<std::array<Real64, 7>, 5> PF = {{
        {1.00, 0.492, 0.226, 0.128, 0.081, 0.061, 0.057},
        {0.123, 0.119, 0.065, 0.043, 0.029, 0.026, 0.023},
        {0.019, 0.026, 0.019, 0.016, 0.014, 0.011, 0.011},
        {0.008, 0.008, 0.008, 0.008, 0.008, 0.006, 0.006},
        {0.0, 0.0, 0.003, 0.003, 0.003, 0.003, 0.003},
    }};

    DayltgGlarePositionFactor = 0.0;
    if (X < 0.0 || X >= 3.0) return DayltgGlarePositionFactor;
    if (Y < 0.0 || Y >= 2.0) return DayltgGlarePositionFactor;

    IX = 1 + int(2.0 * X);
    IY = 1 + int(2.0 * Y);
    X1 = 0.5 * double(IX - 1);
    Y1 = 0.5 * double(IY - 1);
    FA = PF[IY - 1][IX - 1] + 2.0 * (X - X1) * (PF[IY - 1][IX] - PF[IY - 1][IX - 1]);
    FB = PF[IY][IX - 1] + 2.0 * (X - X1) * (PF[IY][IX] - PF[IY][IX - 1]);
    DayltgGlarePositionFactor = FA + 2.0 * (Y - Y1) * (FB - FA);

    return DayltgGlarePositionFactor;
}

void DayltgInterReflectedIllum(EnergyPlusData &state,
                               int const ISunPos, // Sun position counter; used to avoid calculating various
                               int const IHR,     // Hour of day
                               int const enclNum, // Daylighting enclosure index
                               int const IWin     // Window index
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       FCW December 1998
    //                      FCW June 2001: Add blind calculations
    //                      FCW Jan 2001: Add blinds with movable slats
    //                      FCW Jan 2003: Add between-glass blinds
    //                      FCW Jul 2003: account for transmittance of shading surfaces
    //                       (previously these were assumed opaque even if transmittance schedule
    //                        value was non-zero)
    //                      FCW Aug 2003: modify initialization of WLUMSK from WLUMSK = 0. TO
    //                        WLUMSK(:,:,IHR) = 0. Otherwise values calculated in previous
    //                        call are incorrectly zeroed. Result was that window luminance with
    //                        shade or blind included only contribution from first window element
    //                        in window element loop in CalcDayltgCoefficients, thus seriously
    //                        undercalculating window luminance for windows with more than one
    //                        window element. Similarly, modified initialization of WLUMSU from
    //                        WLUMSU = 0. to WLUMSU(:,IHR) = 0., and of WLUMSUdisk from
    //                        WLUMSUdisk = 0. to WLUMSUdisk(:,IHR) = 0.
    //                      PGE Aug 2003: Add daylighting shelves.
    //                      FCW Nov 2003: Add beam solar and sky solar reflected from obstructions;
    //                                    add beam solar reflected from ground accounting for obstructions.
    //                      FCW Nov 2003: increase NPHMAX from 9 to 10 to avoid rays with altitude angle = 0
    //                                    for vertical surfaces.
    //                      FCW Nov 2003: fix the expression for min and max limits of azimuth; old expression
    //                                    broke down for window normals with negative altitude angle
    //                      FCW Nov 2003: add specular reflection from exterior obstructions
    //                      FCW Apr 2004: add light well efficiency multiplying window transmittance
    //                      FCW Apr 2004: add diffusing glazing
    //                      RAR (FSEC)  May 2006: add exterior window screen
    //                      B. Griffith NREL April 2010: CR7869 add adjacent zone area if window is not on this zone
    //                                    apply interior window transmission and blocking to beam transmission from ext win
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Called from CalcDayltgCoefficients for each window and reference point in a daylit
    // space, for each sun position. Calculates illuminance (EINTSK and EINTSU) at reference point due
    // to internally reflected light by integrating to determine the amount of flux from
    // sky and ground (and beam reflected from obstructions) transmitted through
    // the center of the window and then reflecting this
    // light from the inside surfaces of the space.  The "split-flux" method is used
    // (Lynes, Principles of Natural Lighting, 1968).  EINT is determined for
    // different sky types and for window with and without shades, screens or blinds.
    // Also finds luminance (WLUMSK and WLUMSU) of window with shade or blind, &
    // or with diffusing glass, for different sky types.

    // METHODOLOGY EMPLOYED:na

    // REFERENCES:
    // Based on DOE-2.1E subroutine DREFLT.

    // Using/Aliasing
    using DaylightingDevices::TransTDD;
    using General::InterpProfAng;
    using General::POLYF;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // In the following I,J arrays:
    // I = sky type;
    // J = 1 for bare window, 2 and above for window with shade or blind.
    auto &ZSK = state.dataDaylightingManager->ZSK;
    auto &U = state.dataDaylightingManager->U;
    auto &DayltgInterReflectedIllumNearestHitPt = state.dataDaylightingManager->DayltgInterReflectedIllumNearestHitPt;
    auto &DayltgInterReflectedIllumObsHitPt = state.dataDaylightingManager->DayltgInterReflectedIllumObsHitPt;
    auto &DayltgInterReflectedIllumGroundHitPt = state.dataDaylightingManager->DayltgInterReflectedIllumGroundHitPt;
    auto &SkyObstructionMult = state.dataDaylightingManager->SkyObstructionMult;
    auto &FLFWSK = state.dataDaylightingManager->FLFWSK;
    auto &FLFWSU = state.dataDaylightingManager->FLFWSU;
    auto &FLFWSUdisk = state.dataDaylightingManager->FLFWSUdisk;
    auto &FLCWSK = state.dataDaylightingManager->FLCWSK;
    auto &FLCWSU = state.dataDaylightingManager->FLCWSU;
    auto &TransMult = state.dataDaylightingManager->TransMult;
    auto &DayltgInterReflectedIllumTransBmBmMult = state.dataDaylightingManager->DayltgInterReflectedIllumTransBmBmMult;
    auto &ObTransM = state.dataDaylightingManager->ObTransM;

    int ISky; // Sky type index: 1=clear, 2=clear turbid,
    //  3=intermediate, 4=overcast
    Real64 DPH; // Sky/ground element altitude and azimuth increments (radians)
    Real64 DTH;
    int IPH; // Sky/ground element altitude and azimuth indices
    int ITH;
    Real64 PH; // Sky/ground element altitude and azimuth (radians)
    Real64 TH;
    Real64 SPH; // Sine and cosine of PH
    Real64 CPH;
    Real64 PHMIN; // Limits of altitude integration (radians)
    Real64 PHMAX;
    Real64 ThMin; // Limits of azimuth integration (radians)
    Real64 ThMax;
    Real64 PhWin; // Altitude, azimuth angle of window normal (radians)
    Real64 ThWin;
    Real64 ACosTanTan; // ACOS(-TAN(Ph)*TAN(PhWin))
    Real64 DA;         // CPH*DTH*DPH
    Real64 COSB;       // Cosine of angle of incidence of light from sky or ground
    Real64 TVISBR;     // Transmittance of window without shading at COSB
    //  (times light well efficiency, if appropriate)
    Real64 ZSU;
    //  element for clear and overcast sky
    Real64 ObTrans; // Product of solar transmittances of obstructions seen by a light ray
    // unused  REAL(r64)         :: HitPointLumFrClearSky     ! Luminance of obstruction from clear sky (cd/m2)
    // unused  REAL(r64)         :: HitPointLumFrOvercSky     ! Luminance of obstruction from overcast sky (cd/m2)
    // unused  REAL(r64)         :: HitPointLumFrSun          ! Luminance of obstruction from sun (cd/m2)
    int ICtrl;       // Window control pointer
    int JSH;         // Shading index: JSH=1 is bare window, JSH=2 is shaded window
    Real64 COSBSun;  // Cosine of angle of incidence of direct sun on window
    Real64 TVISBSun; // Window's visible transmittance at COSBSun
    //  (times light well efficiency, if appropriate)
    Real64 ZSU1; // Transmitted direct normal illuminance (lux)
    //  CHARACTER(len=32) :: ShType                    ! Window shading device type
    bool ShadeOn;                // True if exterior or interior window shade present
    bool BlindOn;                // True if exterior or interior window blind present
    bool ScreenOn;               // True if exterior window screen present
    int BlNum;                   // Blind number
                                 //        int ScNum; // Screen number //Unused Set but never used
    int PipeNum;                 // TDD pipe object number
    int ShelfNum;                // Daylighting shelf object number
    int InShelfSurf;             // Inside daylighting shelf surface number
    int OutShelfSurf;            // Outside daylighting shelf surface number
    Real64 TransBlBmDiffFront;   // Isolated blind vis beam-diffuse front transmittance
    Real64 TransScBmDiffFront;   // Isolated screen vis beam-diffuse front transmittance
    Real64 TransScDiffDiffFront; // Isolated screen vis diffuse-diffuse front transmittance
    Real64 ReflGlDiffDiffBack;   // Bare glazing system vis diffuse back reflectance
    Real64 ReflGlDiffDiffFront;  // Bare glazing system vis diffuse front reflectance
    Real64 ReflBlBmDiffFront;    // Isolated blind vis beam-diffuse front reflectance
    Real64 TransBlDiffDiffFront; // Isolated blind vis diffuse-diffuse front transmittance
    Real64 ReflBlDiffDiffFront;  // Isolated blind vis diffuse-diffuse front reflectance
    Real64 ReflBlDiffDiffBack;   // Isolated blind vis diffuse-diffuse back reflectance
    Real64 ReflScDiffDiffBack;   // Isolated screen vis diffuse-diffuse back reflectance
    Real64 ProfAng;              // Solar profile angle (radians)
    Real64 SlatAng;              // Blind slat angle
    int JB;                      // Blind slat angle index
    Real64 t1;                   // Beam-beam vis trans of bare glass layers 1 and 2
    Real64 t2;
    Real64 td2; // Diffuse-diffuse vis trans of bare glass layers 2 and 3
    Real64 td3;
    Real64 rbd1; // Beam-diffuse back vis reflectance of bare glass layers 1 and 2
    Real64 rbd2;
    Real64 rfd2; // Beam-diffuse front vis reflectance of bare glass layers 2 and 3
    Real64 rfd3;
    Real64 tfshBd;     // Beam-diffuse front vis trans of bare blind
    Real64 rfshB;      // Beam-diffuse front vis reflectance of bare blind
    Real64 tfshd;      // Diffuse-diffuse front vis trans of bare blind
    Real64 rbshd;      // Diffuse-diffuse back vis reflectance of bare blind
    Real64 ZSUObsRefl; // Illuminance on window from beam solar reflected by an
    //  obstruction (for unit beam normal illuminance)
    int NearestHitSurfNum;  // Surface number of nearest obstruction
    int NearestHitSurfNumX; // Surface number to use when obstruction is a shadowing surface
    Real64 LumAtHitPtFrSun; // Luminance at hit point on obstruction from solar reflection
    //  for unit beam normal illuminance (cd/m2)
    Real64 SunObstructionMult; // = 1 if sun hits a ground point; otherwise = 0
    Real64 Alfa;               // Direction angles for ray heading towards the ground (radians)
    Real64 Beta;
    Real64 HorDis;        // Distance between ground hit point and proj'n of window center onto ground (m)
    int ObsSurfNum;       // Obstruction surface number
    bool hitObs;          // True iff obstruction is hit
    Real64 ObsVisRefl;    // Visible reflectance of obstruction
    Real64 SkyReflVisLum; // Reflected sky luminance at hit point divided by unobstructed sky
    //  diffuse horizontal illuminance [(cd/m2)/lux]
    Real64 dReflObsSky; // Contribution to sky-related illuminance on window due to sky diffuse
    //  reflection from an obstruction
    Real64 TVisSunRefl; // Diffuse vis trans of bare window for beam reflection calc
    //  (times light well efficiency, if appropriate)
    Real64 ZSU1refl; // Beam normal illuminance times ZSU1refl = illuminance on window
    //  due to specular reflection from exterior surfaces

    DataDaylighting::ExtWinType ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
    Real64 EnclInsideSurfArea;              // temporary for calculations, total surface area of enclosure surfaces m2
    int IntWinAdjZoneExtWinNum;             // the index of the exterior window in IntWinAdjZoneExtWin nested struct
    int AdjExtWinLoop;                      // loop index for searching IntWinAdjZoneExtWin
    int IntWinLoop;                         // loop index for searching interior windows
    int IntWinNum;                          // window index for interior windows associated with exterior windows
    Real64 COSBintWin;

    WinShadingType ShType;

    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
    int const enclNumThisWin = state.dataSurface->Surface(state.dataSurface->Surface(IWin).BaseSurf).SolarEnclIndex;
    // The inside surface area, ZoneDaylight(ZoneNum)%totInsSurfArea was calculated in subr DayltgAveInteriorReflectance

    if (enclNumThisWin == enclNum) {
        ExtWinType = DataDaylighting::ExtWinType::InZoneExtWin;
        EnclInsideSurfArea = state.dataDaylightingData->enclDaylight(enclNumThisWin).totInsSurfArea;
        IntWinAdjZoneExtWinNum = 0;
    } else {
        ExtWinType = DataDaylighting::ExtWinType::AdjZoneExtWin;
        // If window is exterior window in adjacent zone, then use areas of both enclosures
        EnclInsideSurfArea =
            state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea + state.dataDaylightingData->enclDaylight(enclNumThisWin).totInsSurfArea;
        // find index in IntWinAdjZoneExtWin
        for (AdjExtWinLoop = 1; AdjExtWinLoop <= thisEnclDaylight.NumOfIntWinAdjEnclExtWins; ++AdjExtWinLoop) {
            if (IWin == thisEnclDaylight.IntWinAdjEnclExtWin(AdjExtWinLoop).SurfNum) { // found it
                IntWinAdjZoneExtWinNum = AdjExtWinLoop;
                break; // added TH 4/13/2010
            }
        }
    }

    // Initialize window luminance and fluxes for split-flux calculation
    state.dataDaylightingManager->WLUMSK(IHR, _, _) = 0.0;
    state.dataDaylightingManager->WLUMSU(IHR, _) = 0.0;
    state.dataDaylightingManager->WLUMSUdisk(IHR, _) = 0.0;
    FLFWSK = 0.0;
    FLFWSU = 0.0;
    FLFWSUdisk = 0.0;
    FLCWSK = 0.0;
    FLCWSU = 0.0;

    int const IConst = state.dataSurface->SurfActiveConstruction(IWin);
    BlindOn = false;
    ShadeOn = false;
    ScreenOn = false;

    if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
        PipeNum = state.dataSurface->SurfWinTDDPipeNum(IWin);
    }

    ShelfNum = state.dataSurface->SurfDaylightingShelfInd(IWin);
    if (ShelfNum > 0) {
        InShelfSurf = state.dataDaylightingDevicesData->Shelf(ShelfNum).InSurf;   // Inside daylighting shelf present if > 0
        OutShelfSurf = state.dataDaylightingDevicesData->Shelf(ShelfNum).OutSurf; // Outside daylighting shelf present if > 0
    } else {
        InShelfSurf = 0;
        OutShelfSurf = 0;
    }

    // Divide sky and ground into elements of altitude PH and
    // azimuth TH, and add the contribution of light coming from each
    // element to the transmitted flux at the center of the window
    // Azimuth ranges over a maximum of 2 Pi radians.
    // Altitude ranges over a maximum of Pi/2 radians between -Pi/2 < PH < +Pi/2, so that elements are not counted twice
    // PH = 0 at the horizon; PH = Pi/2 at the zenith
    PHMIN = max(-DataGlobalConstants::PiOvr2, state.dataSurface->SurfWinPhi(IWin) - DataGlobalConstants::PiOvr2);
    PHMAX = min(DataGlobalConstants::PiOvr2, state.dataSurface->SurfWinPhi(IWin) + DataGlobalConstants::PiOvr2);
    DPH = (PHMAX - PHMIN) / double(NPHMAX);

    // Sky/ground element altitude integration
    Vector3<Real64> const SUNCOS_IHR(state.dataSurface->SurfSunCosHourly(IHR));
    for (IPH = 1; IPH <= NPHMAX; ++IPH) {
        PH = PHMIN + (double(IPH) - 0.5) * DPH;

        SPH = std::sin(PH);
        CPH = std::cos(PH);
        // Third component of unit vector in (TH,PH) direction
        U(3) = SPH;

        // Limits of azimuth integration
        PhWin = state.dataSurface->SurfWinPhi(IWin);
        ThWin = state.dataSurface->SurfWinTheta(IWin);
        if (PhWin >= 0.0) {
            if (PH >= DataGlobalConstants::PiOvr2 - PhWin) {
                ThMin = -DataGlobalConstants::Pi;
                ThMax = DataGlobalConstants::Pi;
            } else {
                ACosTanTan = std::acos(-std::tan(PH) * std::tan(PhWin));
                ThMin = ThWin - std::abs(ACosTanTan);
                ThMax = ThWin + std::abs(ACosTanTan);
            }

        } else { // PhiSurf < 0.0
            if (PH <= -PhWin - DataGlobalConstants::PiOvr2) {
                ThMin = -DataGlobalConstants::Pi;
                ThMax = DataGlobalConstants::Pi;
            } else {
                ACosTanTan = std::acos(-std::tan(PH) * std::tan(PhWin));
                ThMin = ThWin - std::abs(ACosTanTan);
                ThMax = ThWin + std::abs(ACosTanTan);
            }
        }

        DTH = (ThMax - ThMin) / double(NTHMAX);
        DA = CPH * DTH * DPH;

        // Sky/ground element azimuth integration
        Real64 const sin_window_phi(std::sin(state.dataSurface->SurfWinPhi(IWin)));
        Real64 const cos_window_phi(std::cos(state.dataSurface->SurfWinPhi(IWin)));
        for (ITH = 1; ITH <= NTHMAX; ++ITH) {
            TH = ThMin + (double(ITH) - 0.5) * DTH;
            U(1) = CPH * std::cos(TH);
            U(2) = CPH * std::sin(TH);
            // Cosine of angle of incidence of light from sky or ground element
            COSB = SPH * sin_window_phi + CPH * cos_window_phi * std::cos(TH - state.dataSurface->SurfWinTheta(IWin));
            if (COSB < 0.0) continue; // Sky/ground elements behind window (although there shouldn't be any)

            // Initialize illuminance on window for this sky/ground element
            ZSK = 0.0;
            ZSU = 0.0;
            // Initialize illuminance on window from beam solar reflection if ray hits an obstruction
            ZSUObsRefl = 0.0;

            if (ISunPos == 1) { // Intersection calculation has to be done only for first sun position
                // Determine net transmittance of obstructions that the ray hits. ObTrans will be 1.0
                // if no obstructions are hit.
                DayltgHitObstruction(state, IHR, IWin, state.dataSurface->SurfaceWindow(IWin).WinCenter, U, ObTrans);
                ObTransM(IPH, ITH) = ObTrans;
            }

            // SKY AND GROUND RADIATION ON WINDOW

            // Contribution is from sky if PH > 0 (ray goes upward), and from ground if PH < 0 (ray goes downward)
            // (There may also be contributions from reflection from obstructions; see 'BEAM SOLAR AND SKY SOLAR
            // REFLECTED FROM NEAREST OBSTRUCTION,' below.)

            if (ISunPos == 1) SkyObstructionMult(IPH, ITH) = 1.0;
            if (PH > 0.0) { // Contribution is from sky
                for (ISky = 1; ISky <= 4; ++ISky) {
                    ZSK(ISky) = DayltgSkyLuminance(state, ISky, TH, PH) * COSB * DA * ObTransM(IPH, ITH);
                }
            } else { // PH <= 0.0; contribution is from ground
                if (state.dataSurface->CalcSolRefl && ObTransM(IPH, ITH) > 1.e-6 && ISunPos == 1) {
                    // Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
                    // by the ray. This effect is given by the ratio SkyObstructionMult =
                    // (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
                    // This ratio is calculated for an isotropic sky.
                    // Ground point hit by the ray:
                    Alfa = std::acos(-U(3));
                    Beta = std::atan2(U(2), U(1));
                    HorDis = (state.dataSurface->SurfaceWindow(IWin).WinCenter(3) - state.dataSurface->GroundLevelZ) * std::tan(Alfa);
                    DayltgInterReflectedIllumGroundHitPt(3) = state.dataSurface->GroundLevelZ;
                    DayltgInterReflectedIllumGroundHitPt(1) = state.dataSurface->SurfaceWindow(IWin).WinCenter(1) + HorDis * std::cos(Beta);
                    DayltgInterReflectedIllumGroundHitPt(2) = state.dataSurface->SurfaceWindow(IWin).WinCenter(2) + HorDis * std::sin(Beta);

                    SkyObstructionMult(IPH, ITH) =
                        CalcObstrMultiplier(state, DayltgInterReflectedIllumGroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc);
                } // End of check if solar reflection calc is in effect
                for (ISky = 1; ISky <= 4; ++ISky) {
                    // Below, luminance of ground in cd/m2 is illuminance on ground in lumens/m2
                    // times ground reflectance, divided by pi, times obstruction multiplier.
                    ZSK(ISky) =
                        (state.dataDaylightingManager->GILSK(IHR, ISky) * state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi) * COSB *
                        DA * ObTransM(IPH, ITH) * SkyObstructionMult(IPH, ITH);
                }
                // Determine if sun illuminates the point that ray hits the ground. If the solar reflection
                // calculation has been requested (CalcSolRefl = .TRUE.) shading by obstructions, including
                // the building itself, is considered in determining whether sun hits the ground point.
                // Otherwise this shading is ignored and the sun always hits the ground point.
                SunObstructionMult = 1.0;
                if (state.dataSurface->CalcSolRefl && ObTransM(IPH, ITH) > 1.e-6) {
                    // Sun reaches ground point if vector from this point to the sun is unobstructed
                    hitObs = false;
                    for (ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
                        if (!state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) continue;
                        PierceSurface(state, ObsSurfNum, DayltgInterReflectedIllumGroundHitPt, SUNCOS_IHR, DayltgInterReflectedIllumObsHitPt, hitObs);
                        if (hitObs) break;
                    }
                    if (hitObs) SunObstructionMult = 0.0;
                }
                ZSU = (state.dataDaylightingManager->GILSU(IHR) * state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi) * COSB * DA *
                      ObTransM(IPH, ITH) * SunObstructionMult;
            }
            // BEAM SOLAR AND SKY SOLAR REFLECTED FROM NEAREST OBSTRUCTION

            if (state.dataSurface->CalcSolRefl && ObTransM(IPH, ITH) < 1.0) {
                // Find obstruction whose hit point is closest to the center of the window
                DayltgClosestObstruction(
                    state, state.dataSurface->SurfaceWindow(IWin).WinCenter, U, NearestHitSurfNum, DayltgInterReflectedIllumNearestHitPt);
                if (NearestHitSurfNum > 0) {

                    // Beam solar reflected from nearest obstruction.
                    DayltgSurfaceLumFromSun(state, IHR, U, NearestHitSurfNum, DayltgInterReflectedIllumNearestHitPt, LumAtHitPtFrSun);
                    ZSUObsRefl = LumAtHitPtFrSun * COSB * DA;
                    ZSU += ZSUObsRefl;

                    // Sky solar reflected from nearest obstruction.
                    int const ObsConstrNum = state.dataSurface->Surface(NearestHitSurfNum).Construction;
                    if (ObsConstrNum > 0) {
                        // Exterior building surface is nearest hit
                        if (!state.dataConstruction->Construct(ObsConstrNum).TypeIsWindow) {
                            // Obstruction is not a window, i.e., is an opaque surface
                            ObsVisRefl =
                                1.0 - state.dataMaterial->Material(state.dataConstruction->Construct(ObsConstrNum).LayerPoint(1)).AbsorpVisible;
                        } else {
                            // Obstruction is a window; assume it is bare
                            ObsVisRefl = state.dataConstruction->Construct(ObsConstrNum).ReflectVisDiffFront;
                        }
                    } else {
                        // Shadowing surface is nearest hit
                        if (state.dataSurface->SurfDaylightingShelfInd(NearestHitSurfNum) > 0) {
                            // Skip daylighting shelves, whose reflection is separately calculated
                            ObsVisRefl = 0.0;
                        } else {
                            ObsVisRefl = state.dataSurface->SurfShadowDiffuseVisRefl(NearestHitSurfNum);
                            if (state.dataSurface->SurfShadowGlazingConstruct(NearestHitSurfNum) > 0)
                                ObsVisRefl += state.dataSurface->SurfShadowGlazingFrac(NearestHitSurfNum) *
                                              state.dataConstruction->Construct(state.dataSurface->SurfShadowGlazingConstruct(NearestHitSurfNum))
                                                  .ReflectVisDiffFront;
                            // Note in the above that ShadowSurfDiffuseVisRefl is the reflectance of opaque part of
                            // shadowing surface times (1 - ShadowSurfGlazingFrac)
                        }
                    }
                    NearestHitSurfNumX = NearestHitSurfNum;
                    // Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
                    // The following gets the correct side of a shadowing surface for reflection.
                    if (state.dataSurface->Surface(NearestHitSurfNum).IsShadowing) {
                        if (dot(U, state.dataSurface->Surface(NearestHitSurfNum).OutNormVec) > 0.0) NearestHitSurfNumX = NearestHitSurfNum + 1;
                    }
                    if (!state.dataSysVars->DetailedSkyDiffuseAlgorithm || !state.dataSurface->ShadingTransmittanceVaries ||
                        state.dataHeatBal->SolarDistribution == DataHeatBalance::Shadowing::Minimal) {
                        SkyReflVisLum = ObsVisRefl * state.dataSurface->Surface(NearestHitSurfNumX).ViewFactorSky *
                                        state.dataSolarShading->SurfDifShdgRatioIsoSky(NearestHitSurfNumX) / DataGlobalConstants::Pi;
                    } else {
                        SkyReflVisLum = ObsVisRefl * state.dataSurface->Surface(NearestHitSurfNumX).ViewFactorSky *
                                        state.dataSolarShading->SurfDifShdgRatioIsoSkyHRTS(1, IHR, NearestHitSurfNumX) / DataGlobalConstants::Pi;
                    }
                    dReflObsSky = SkyReflVisLum * COSB * DA;
                    for (ISky = 1; ISky <= 4; ++ISky) {
                        ZSK(ISky) += state.dataDaylightingManager->GILSK(IHR, ISky) * dReflObsSky;
                    }
                }
            } // End of check if exterior solar reflection calculation is active

            //  ===Bare window (no shade or blind; non-diffusing glass)===

            // Increment flux entering space and window luminance (cd/m2).
            // FLCW--(I,J) = part of incoming flux (in lumens) that goes up to ceiling and upper part of walls.
            // FLFW--(I,J) = part that goes down to floor and lower part of walls

            if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                // Unshaded visible transmittance of TDD for a single ray from sky/ground element
                TVISBR = TransTDD(state, PipeNum, COSB, DataDaylightingDevices::RadType::VisibleBeam) * state.dataSurface->SurfWinGlazedFrac(IWin);

                // Make all transmitted light diffuse for a TDD with a bare diffuser
                for (ISky = 1; ISky <= 4; ++ISky) {
                    state.dataDaylightingManager->WLUMSK(IHR, 1, ISky) += ZSK(ISky) * TVISBR / DataGlobalConstants::Pi;
                    FLFWSK(1, ISky) += ZSK(ISky) * TVISBR * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                    FLCWSK(1, ISky) += ZSK(ISky) * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);

                    // For later calculation of diffuse visible transmittance
                    state.dataDaylightingManager->TDDFluxInc(IHR, ISky, PipeNum) += ZSK(ISky);
                    state.dataDaylightingManager->TDDFluxTrans(IHR, ISky, PipeNum) += ZSK(ISky) * TVISBR;

                    if (ISky == 1) {
                        state.dataDaylightingManager->WLUMSU(IHR, 1) += ZSU * TVISBR / DataGlobalConstants::Pi;
                        FLFWSU(1) += ZSU * TVISBR * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                        FLCWSU(1) += ZSU * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);

                        // For later calculation of diffuse visible transmittance
                        state.dataDaylightingManager->TDDFluxInc(IHR, ISky, PipeNum) += ZSU;
                        state.dataDaylightingManager->TDDFluxTrans(IHR, ISky, PipeNum) += ZSU * TVISBR;
                    }
                }

            } else { // Bare window
                // Transmittance of bare window for this sky/ground element
                TVISBR = POLYF(COSB, state.dataConstruction->Construct(IConst).TransVisBeamCoef) * state.dataSurface->SurfWinGlazedFrac(IWin) *
                         state.dataSurface->SurfWinLightWellEff(IWin);

                if (InShelfSurf > 0) { // Inside daylighting shelf
                    // Daylighting shelf simplification:  All light is diffuse
                    // SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
                    for (ISky = 1; ISky <= 4; ++ISky) {
                        FLCWSK(1, ISky) += ZSK(ISky) * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);

                        if (ISky == 1) {
                            FLCWSU(1) += ZSU * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);
                        }
                    }

                } else { // Normal window

                    // CR 7869  correct TVISBR if disk beam passes thru interior window
                    if (ExtWinType == DataDaylighting::ExtWinType::AdjZoneExtWin) {
                        // modify TVISBR by second window transmission
                        // first determine if ray from point passes thru any interior window
                        hitObs = false;
                        for (IntWinLoop = 1; IntWinLoop <= thisEnclDaylight.IntWinAdjEnclExtWin(IntWinAdjZoneExtWinNum).NumOfIntWindows;
                             ++IntWinLoop) {
                            IntWinNum = thisEnclDaylight.IntWinAdjEnclExtWin(IntWinAdjZoneExtWinNum).IntWinNum(IntWinLoop);
                            PierceSurface(state,
                                          IntWinNum,
                                          state.dataSurface->SurfaceWindow(IntWinNum).WinCenter,
                                          SUNCOS_IHR,
                                          DayltgInterReflectedIllumObsHitPt,
                                          hitObs);
                            if (hitObs) { // disk passes thru
                                // cosine of incidence angle of light from sky or ground element for
                                COSBintWin = SPH * std::sin(state.dataSurface->SurfWinPhi(IntWinNum)) +
                                             CPH * std::cos(state.dataSurface->SurfWinPhi(IntWinNum)) *
                                                 std::cos(TH - state.dataSurface->SurfWinTheta(IntWinNum));
                                TVISBR *=
                                    POLYF(COSBintWin,
                                          state.dataConstruction->Construct(state.dataSurface->Surface(IntWinNum).Construction).TransVisBeamCoef);
                                break;
                            }
                        }
                        if (!hitObs) { // blocked by opaque parts, beam does not actually pass thru interior window to reach zone
                            TVISBR = 0.0;
                        }
                    }

                    for (ISky = 1; ISky <= 4; ++ISky) {
                        // IF (PH < 0.0d0) THEN
                        // Fixed by FCW, Nov. 2003:
                        if (PH > 0.0) {
                            FLFWSK(1, ISky) += ZSK(ISky) * TVISBR;
                            if (ISky == 1) FLFWSU(1) += ZSU * TVISBR;
                        } else {
                            FLCWSK(1, ISky) += ZSK(ISky) * TVISBR;
                            if (ISky == 1) FLCWSU(1) += ZSU * TVISBR;
                        }
                    }
                } // End of check if window with daylighting shelf or normal window
            }     // End of check if TDD:DOME or bare window

            // Check if window has shade or blind
            ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
            if (state.dataSurface->Surface(IWin).HasShadeControl) {
                ShType = state.dataSurface->WindowShadingControl(ICtrl).ShadingType;
                BlNum = state.dataSurface->SurfWinBlindNumber(IWin);
                //                    ScNum = SurfaceWindow( IWin ).ScreenNumber; //Unused Set but never used

                ShadeOn = ANY_SHADE(ShType);
                BlindOn = ANY_BLIND(ShType);
                ScreenOn = (ShType == WinShadingType::ExtScreen);
            }

            if (ShadeOn || BlindOn || ScreenOn || state.dataSurface->SurfWinSolarDiffusing(IWin)) {

                // ===Window with interior or exterior shade or blind, exterior screen, or with diffusing glass===

                // Increment flux entering space and window luminance. Shades and diffusing glass are
                // assumed to be perfect diffusers, i.e., the transmittance is independent of angle of
                // incidence and the transmitted light is isotropic. The transmittance of a blind is
                // assumed to depend on profile angle and slat angle; the diffuse light entering the room from
                // the slats of the blind is assumed to be isotropic. With blinds, light can also enter
                // the room by passing between the slats without reflection. The beam transmittance of a screen
                // is assumed to depend on sun azimuth and azimuth angle.

                // For light from a shade, or from diffusing glass, or from the slats of a blind, a flux fraction,
                // SurfaceWindow(IWin)%FractionUpgoing (determined by window tilt), goes up toward
                // ceiling and upper part of walls, and 1-Surfacewindow(iwin)%FractionUpgoing
                // goes down toward floor and lower part of walls. For a blind, the light passing
                // between the slats goes either up or down depending on the altitude angle of the
                // element from which the light came. For a screen, the light passing
                // between the screen's cylinders goes either up or down depending on the altitude angle of the
                // element from which the light came.

                int IConstShaded = state.dataSurface->SurfWinActiveShadedConstruction(IWin);
                if (state.dataSurface->SurfWinSolarDiffusing(IWin)) IConstShaded = state.dataSurface->Surface(IWin).Construction;

                // Transmittance of window including shade, screen or blind
                DayltgInterReflectedIllumTransBmBmMult = 0.0;
                TransMult = 0.0;

                if (ShadeOn) { // Shade
                    if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                        // Shaded visible transmittance of TDD for a single ray from sky/ground element
                        TransMult(1) =
                            TransTDD(state, PipeNum, COSB, DataDaylightingDevices::RadType::VisibleBeam) * state.dataSurface->SurfWinGlazedFrac(IWin);
                    } else { // Shade only, no TDD
                        // Calculate transmittance of the combined window and shading device for this sky/ground element
                        TransMult(1) = POLYF(COSB, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) *
                                       state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
                    }

                } else if (ScreenOn) { // Screen: get beam-beam, beam-diffuse and diffuse-diffuse vis trans/ref of screen and glazing system
                    CalcScreenTransmittance(state, IWin, (PH - state.dataSurface->SurfWinPhi(IWin)), (TH - state.dataSurface->SurfWinTheta(IWin)));
                    ReflGlDiffDiffFront = state.dataConstruction->Construct(IConst).ReflectVisDiffFront;
                    ReflScDiffDiffBack = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).DifReflectVis;
                    TransScBmDiffFront = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).BmDifTransVis;
                    TransMult(1) = TransScBmDiffFront * state.dataSurface->SurfWinGlazedFrac(IWin) *
                                   state.dataConstruction->Construct(IConst).TransDiffVis / (1 - ReflGlDiffDiffFront * ReflScDiffDiffBack) *
                                   state.dataSurface->SurfWinLightWellEff(IWin);
                    DayltgInterReflectedIllumTransBmBmMult(1) =
                        state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).BmBmTransVis;

                } else if (BlindOn) { // Blind: get beam-diffuse and beam-beam vis trans of blind+glazing system
                    // PETER:  As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
                    //         for TDDs because it is based on TVISBR which is correctly calculated for TDDs above.

                    ProfileAngle(state, IWin, U, state.dataHeatBal->Blind(BlNum).SlatOrientation, ProfAng);

                    for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                        if (!state.dataSurface->SurfWinMovableSlats(IWin) && JB > 1) break;

                        TransBlBmDiffFront = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffTrans(JB, {1, 37}));

                        if (ShType == WinShadingType::IntBlind) { // Interior blind
                            ReflGlDiffDiffBack = state.dataConstruction->Construct(IConst).ReflectVisDiffBack;
                            ReflBlBmDiffFront = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffRefl(JB, {1, 37}));
                            ReflBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(JB);
                            TransBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(JB);
                            TransMult(JB) = TVISBR * (TransBlBmDiffFront + ReflBlBmDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront /
                                                                               (1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack));

                        } else if (ShType == WinShadingType::ExtBlind) { // Exterior blind
                            ReflGlDiffDiffFront = state.dataConstruction->Construct(IConst).ReflectVisDiffFront;
                            ReflBlDiffDiffBack = state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(JB);
                            TransMult(JB) = TransBlBmDiffFront * state.dataSurface->SurfWinGlazedFrac(IWin) *
                                            state.dataConstruction->Construct(IConst).TransDiffVis /
                                            (1.0 - ReflGlDiffDiffFront * ReflBlDiffDiffBack) * state.dataSurface->SurfWinLightWellEff(IWin);

                        } else { // Between-glass blind
                            t1 = POLYF(COSB, state.dataConstruction->Construct(IConst).tBareVisCoef(1));
                            td2 = state.dataConstruction->Construct(IConst).tBareVisDiff(2);
                            rbd1 = state.dataConstruction->Construct(IConst).rbBareVisDiff(1);
                            rfd2 = state.dataConstruction->Construct(IConst).rfBareVisDiff(2);
                            tfshBd = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffTrans(JB, {1, 37}));
                            tfshd = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(JB);
                            rfshB = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffRefl(JB, {1, 37}));
                            rbshd = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(JB);
                            if (state.dataConstruction->Construct(IConst).TotGlassLayers == 2) { // 2 glass layers
                                TransMult(JB) =
                                    t1 * (tfshBd * (1.0 + rfd2 * rbshd) + rfshB * rbd1 * tfshd) * td2 * state.dataSurface->SurfWinLightWellEff(IWin);
                            } else { // 3 glass layers; blind between layers 2 and 3
                                t2 = POLYF(COSB, state.dataConstruction->Construct(IConst).tBareVisCoef(2));
                                td3 = state.dataConstruction->Construct(IConst).tBareVisDiff(3);
                                rfd3 = state.dataConstruction->Construct(IConst).rfBareVisDiff(3);
                                rbd2 = state.dataConstruction->Construct(IConst).rbBareVisDiff(2);
                                TransMult(JB) = t1 * t2 * (tfshBd * (1.0 + rfd3 * rbshd) + rfshB * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) * td3 *
                                                state.dataSurface->SurfWinLightWellEff(IWin);
                            }
                        }

                        if (state.dataSurface->SurfWinMovableSlats(IWin)) {
                            SlatAng = (JB - 1) * DataGlobalConstants::Pi / (MaxSlatAngs - 1);
                        } else {
                            SlatAng = state.dataHeatBal->Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians;
                        }
                        DayltgInterReflectedIllumTransBmBmMult(JB) =
                            TVISBR * General::BlindBeamBeamTrans(ProfAng,
                                                                 SlatAng,
                                                                 state.dataHeatBal->Blind(BlNum).SlatWidth,
                                                                 state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                                                 state.dataHeatBal->Blind(BlNum).SlatThickness);
                    } // End of loop over slat angles

                } else { // Diffusing glass
                    TransMult(1) = POLYF(COSB, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) *
                                   state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
                } // End of check if shade, blind or diffusing glass

                if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                    // No beam is transmitted.  This takes care of all types of screens and blinds.
                    DayltgInterReflectedIllumTransBmBmMult = 0.0;
                }

                // Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
                if (InShelfSurf > 0) {                            // Inside daylighting shelf
                    DayltgInterReflectedIllumTransBmBmMult = 0.0; // No beam, diffuse only
                }

                // DayltgInterReflectedIllumTransBmBmMult is used in the following for windows with blinds or screens to get contribution from light
                // passing directly between slats or between screen material without reflection.

                for (ISky = 1; ISky <= 4; ++ISky) {
                    for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                        // EXIT after first pass if not movable slats or exterior window screen
                        if (!state.dataSurface->SurfWinMovableSlats(IWin) && JB > 1) break;

                        state.dataDaylightingManager->WLUMSK(IHR, JB + 1, ISky) += ZSK(ISky) * TransMult(JB) / DataGlobalConstants::Pi;
                        FLFWSK(JB + 1, ISky) += ZSK(ISky) * TransMult(JB) * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                        if (PH > 0.0 && (BlindOn || ScreenOn)) FLFWSK(JB + 1, ISky) += ZSK(ISky) * DayltgInterReflectedIllumTransBmBmMult(JB);
                        FLCWSK(JB + 1, ISky) += ZSK(ISky) * TransMult(JB) * state.dataSurface->SurfWinFractionUpgoing(IWin);
                        if (PH <= 0.0 && (BlindOn || ScreenOn)) FLCWSK(JB + 1, ISky) += ZSK(ISky) * DayltgInterReflectedIllumTransBmBmMult(JB);
                        if (ISky == 1) {
                            state.dataDaylightingManager->WLUMSU(IHR, JB + 1) += ZSU * TransMult(JB) / DataGlobalConstants::Pi;
                            FLFWSU(JB + 1) += ZSU * TransMult(JB) * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                            if (PH > 0.0 && (BlindOn || ScreenOn)) FLFWSU(JB + 1) += ZSU * DayltgInterReflectedIllumTransBmBmMult(JB);
                            FLCWSU(JB + 1) += ZSU * TransMult(JB) * state.dataSurface->SurfWinFractionUpgoing(IWin);
                            if (PH <= 0.0 && (BlindOn || ScreenOn)) FLCWSU(JB + 1) += ZSU * DayltgInterReflectedIllumTransBmBmMult(JB);
                        }
                    }
                }
            } // End of window with shade, screen, blind or diffusing glass

        } // End of azimuth integration loop, ITH
    }     // End of altitude integration loop, IPH

    if (OutShelfSurf > 0) { // Outside daylighting shelf
        // Add exterior diffuse illuminance due to outside shelf
        // Since all of the illuminance is added to the zone as upgoing diffuse, it can be added as a lump sum here

        TVISBR = state.dataConstruction->Construct(IConst).TransDiffVis; // Assume diffuse transmittance for shelf illuminance

        for (ISky = 1; ISky <= 4; ++ISky) {
            // This is only an estimate because the anisotropic sky view of the shelf is not yet taken into account.
            // SurfAnisoSkyMult would be great to use but it is not available until the heat balance starts up.
            ZSK(ISky) = state.dataDaylightingManager->GILSK(IHR, ISky) * 1.0 * state.dataDaylightingDevicesData->Shelf(ShelfNum).OutReflectVis *
                        state.dataDaylightingDevicesData->Shelf(ShelfNum).ViewFactor;

            // SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
            FLCWSK(1, ISky) += ZSK(ISky) * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);

            if (ISky == 1) {
                ZSU = state.dataDaylightingManager->GILSU(IHR) * state.dataHeatBal->SurfSunlitFracHR(IHR, OutShelfSurf) *
                      state.dataDaylightingDevicesData->Shelf(ShelfNum).OutReflectVis * state.dataDaylightingDevicesData->Shelf(ShelfNum).ViewFactor;
                FLCWSU(1) += ZSU * TVISBR * state.dataSurface->SurfWinFractionUpgoing(IWin);
            }
        } // ISKY
    }

    // Sky-related portion of internally reflected illuminance.
    // The inside surface area, ZoneDaylight(ZoneNum)%totInsSurfArea, and ZoneDaylight(ZoneNum)%aveVisDiffReflect,
    // were calculated in subr DayltgAveInteriorReflectance.

    for (ISky = 1; ISky <= 4; ++ISky) {
        for (JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH) {
            if (!state.dataSurface->SurfWinMovableSlats(IWin) && JSH > 2) break;
            // Full area of window is used in following since effect of dividers on reducing
            // effective window transmittance has already been accounted for in calc of FLFWSK and FLCWSK.
            state.dataDaylightingManager->EINTSK(IHR, JSH, ISky) =
                (FLFWSK(JSH, ISky) * state.dataSurface->SurfWinRhoFloorWall(IWin) +
                 FLCWSK(JSH, ISky) * state.dataSurface->SurfWinRhoCeilingWall(IWin)) *
                (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
                (EnclInsideSurfArea * (1.0 - state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect));
        } // JSH
    }     // ISKY

    // BEAM SOLAR RADIATION ON WINDOW

    // Beam reaching window directly (without specular reflection from exterior obstructions)

    if (state.dataHeatBal->SurfSunlitFracHR(IHR, IWin) > 0.0) {
        // Cos of angle of incidence
        COSBSun = state.dataDaylightingManager->SPHSUN * std::sin(state.dataSurface->SurfWinPhi(IWin)) +
                  state.dataDaylightingManager->CPHSUN * std::cos(state.dataSurface->SurfWinPhi(IWin)) *
                      std::cos(state.dataDaylightingManager->THSUN - state.dataSurface->SurfWinTheta(IWin));

        if (COSBSun > 0.0) {
            // Multiply direct normal illuminance (normalized to 1.0 lux)
            // by incident angle factor and by fraction of window that is sunlit.
            // Note that in the following SurfSunlitFracHR accounts for possibly non-zero transmittance of
            // shading surfaces.

            ZSU1 = COSBSun * state.dataHeatBal->SurfSunlitFracHR(IHR, IWin);

            // Contribution to window luminance and downgoing flux

            // -- Bare window

            if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                // Unshaded visible transmittance of TDD for collimated beam from the sun
                TVISBSun =
                    TransTDD(state, PipeNum, COSBSun, DataDaylightingDevices::RadType::VisibleBeam) * state.dataSurface->SurfWinGlazedFrac(IWin);
                state.dataDaylightingManager->TDDTransVisBeam(IHR, PipeNum) = TVISBSun;

                FLFWSUdisk(1) = 0.0; // Diffuse light only

                state.dataDaylightingManager->WLUMSU(IHR, 1) += ZSU1 * TVISBSun / DataGlobalConstants::Pi;
                FLFWSU(1) += ZSU1 * TVISBSun * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                FLCWSU(1) += ZSU1 * TVISBSun * state.dataSurface->SurfWinFractionUpgoing(IWin);

            } else { // Bare window
                TVISBSun = POLYF(COSBSun, state.dataConstruction->Construct(IConst).TransVisBeamCoef) * state.dataSurface->SurfWinGlazedFrac(IWin) *
                           state.dataSurface->SurfWinLightWellEff(IWin);

                // Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
                if (InShelfSurf > 0) {   // Inside daylighting shelf
                    FLFWSUdisk(1) = 0.0; // Diffuse light only

                    // SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
                    // WLUMSU(1,IHR) = WLUMSU(1,IHR) + ZSU1 * TVISBSun / PI
                    // FLFWSU(1) = FLFWSU(1) + ZSU1 * TVISBSun * (1.0 - SurfaceWindow(IWin)%FractionUpgoing)
                    FLCWSU(1) += ZSU1 * TVISBSun * state.dataSurface->SurfWinFractionUpgoing(IWin);
                } else { // Normal window
                    FLFWSUdisk(1) = ZSU1 * TVISBSun;
                }
            }

            // -- Window with shade, screen, blind or diffusing glass
            if (ShadeOn || BlindOn || ScreenOn || state.dataSurface->SurfWinSolarDiffusing(IWin)) {
                DayltgInterReflectedIllumTransBmBmMult = 0.0;
                TransMult = 0.0;

                // TH 7/7/2010 moved from inside the loop: DO JB = 1,MaxSlatAngs
                if (BlindOn)
                    ProfileAngle(state, IWin, state.dataSurface->SurfSunCosHourly(IHR), state.dataHeatBal->Blind(BlNum).SlatOrientation, ProfAng);

                for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                    if (!state.dataSurface->SurfWinMovableSlats(IWin) && JB > 1) break;

                    if (ShadeOn || ScreenOn || state.dataSurface->SurfWinSolarDiffusing(IWin)) { // Shade or screen on or diffusing glass
                        if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                            // Shaded visible transmittance of TDD for collimated beam from the sun
                            TransMult(1) = TransTDD(state, PipeNum, COSBSun, DataDaylightingDevices::RadType::VisibleBeam) *
                                           state.dataSurface->SurfWinGlazedFrac(IWin);
                        } else {
                            if (ScreenOn) {
                                TransMult(1) = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).BmBmTransVis *
                                               state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
                            } else {
                                int IConstShaded = state.dataSurface->SurfWinActiveShadedConstruction(IWin);
                                if (state.dataSurface->SurfWinSolarDiffusing(IWin)) IConstShaded = state.dataSurface->Surface(IWin).Construction;
                                TransMult(1) = POLYF(COSBSun, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) *
                                               state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);
                            }
                        }

                    } else { // Blind on

                        // As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
                        // for TDDs because it is based on TVISBSun which is correctly calculated for TDDs above.

                        TransBlBmDiffFront = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffTrans(JB, {1, 37}));

                        if (ShType == WinShadingType::IntBlind) { // Interior blind
                            // TH CR 8121, 7/7/2010
                            // ReflBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl)
                            ReflBlBmDiffFront = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffRefl(JB, {1, 37}));

                            // TH added 7/12/2010 for CR 8121
                            ReflBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(JB);
                            TransBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(JB);

                            TransMult(JB) = TVISBSun * (TransBlBmDiffFront + ReflBlBmDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront /
                                                                                 (1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack));

                        } else if (ShType == WinShadingType::ExtBlind) { // Exterior blind
                            TransMult(JB) = TransBlBmDiffFront *
                                            (state.dataConstruction->Construct(IConst).TransDiffVis /
                                             (1.0 - ReflGlDiffDiffFront * state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(JB))) *
                                            state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);

                        } else { // Between-glass blind
                            t1 = POLYF(COSBSun, state.dataConstruction->Construct(IConst).tBareVisCoef(1));
                            tfshBd = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffTrans(JB, {1, 37}));
                            rfshB = InterpProfAng(ProfAng, state.dataHeatBal->Blind(BlNum).VisFrontBeamDiffRefl(JB, {1, 37}));
                            if (state.dataConstruction->Construct(IConst).TotGlassLayers == 2) { // 2 glass layers
                                TransMult(JB) =
                                    t1 * (tfshBd * (1.0 + rfd2 * rbshd) + rfshB * rbd1 * tfshd) * td2 * state.dataSurface->SurfWinLightWellEff(IWin);
                            } else { // 3 glass layers; blind between layers 2 and 3
                                t2 = POLYF(COSBSun, state.dataConstruction->Construct(IConst).tBareVisCoef(2));
                                TransMult(JB) = t1 * t2 * (tfshBd * (1.0 + rfd3 * rbshd) + rfshB * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) * td3 *
                                                state.dataSurface->SurfWinLightWellEff(IWin);
                            }
                        }
                        if (state.dataSurface->SurfWinMovableSlats(IWin)) {
                            SlatAng = (JB - 1) * DataGlobalConstants::Pi / (MaxSlatAngs - 1);
                        } else {
                            SlatAng = state.dataHeatBal->Blind(BlNum).SlatAngle * DataGlobalConstants::DegToRadians;
                        }
                        DayltgInterReflectedIllumTransBmBmMult(JB) =
                            TVISBSun * General::BlindBeamBeamTrans(ProfAng,
                                                                   SlatAng,
                                                                   state.dataHeatBal->Blind(BlNum).SlatWidth,
                                                                   state.dataHeatBal->Blind(BlNum).SlatSeparation,
                                                                   state.dataHeatBal->Blind(BlNum).SlatThickness);
                    } // ShadeOn/ScreenOn/BlindOn/Diffusing glass

                    if (state.dataSurface->SurfWinOriginalClass(IWin) == SurfaceClass::TDD_Dome) {
                        DayltgInterReflectedIllumTransBmBmMult = 0.0; // No beam, diffuse only
                    }

                    // Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
                    if (InShelfSurf > 0) {                            // Inside daylighting shelf
                        DayltgInterReflectedIllumTransBmBmMult = 0.0; // No beam, diffuse only (Not sure if this really works)
                                                                      // SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
                    }

                    state.dataDaylightingManager->WLUMSU(IHR, JB + 1) += ZSU1 * TransMult(JB) / DataGlobalConstants::Pi;
                    state.dataDaylightingManager->WLUMSUdisk(IHR, JB + 1) =
                        ZSU1 * DayltgInterReflectedIllumTransBmBmMult(JB) / DataGlobalConstants::Pi;
                    FLFWSU(JB + 1) += ZSU1 * TransMult(JB) * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                    FLFWSUdisk(JB + 1) = ZSU1 * DayltgInterReflectedIllumTransBmBmMult(JB);
                    FLCWSU(JB + 1) += ZSU1 * TransMult(JB) * state.dataSurface->SurfWinFractionUpgoing(IWin);
                } // End of loop over slat angles
            }     // End of window with shade or blind
        }         // COSBSun > 0
    }             // SurfSunlitFracHR > 0

    // Beam reaching window after specular reflection from exterior obstruction

    // In the following, Beam normal illuminance times ZSU1refl = illuminance on window due to
    // specular reflection from exterior surfaces

    if (state.dataSurface->CalcSolRefl && state.dataSurface->SurfWinOriginalClass(IWin) != SurfaceClass::TDD_Dome) {
        ZSU1refl = state.dataSurface->SurfReflFacBmToBmSolObs(IHR, IWin);

        if (ZSU1refl > 0.0) {
            // Contribution to window luminance and downgoing flux

            // -- Bare window. We use diffuse-diffuse transmittance here rather than beam-beam to avoid
            //    complications due to specular reflection from multiple exterior surfaces

            TVisSunRefl = state.dataConstruction->Construct(IConst).TransDiffVis * state.dataSurface->SurfWinGlazedFrac(IWin) *
                          state.dataSurface->SurfWinLightWellEff(IWin);
            // In the following it is assumed that all reflected beam is going downward, as it would be in the
            // important case of reflection from a highly glazed facade of a neighboring building. However, in
            // rare cases (such as upward specular reflection from a flat horizontal skylight) it may
            // actually be going upward.
            FLFWSUdisk(1) += ZSU1refl * TVisSunRefl;

            // -- Window with shade, blind or diffusing glass

            if (ShadeOn || BlindOn || ScreenOn || state.dataSurface->SurfWinSolarDiffusing(IWin)) {
                DayltgInterReflectedIllumTransBmBmMult = 0.0;
                TransMult = 0.0;

                for (JB = 1; JB <= MaxSlatAngs; ++JB) {
                    if (!state.dataSurface->SurfWinMovableSlats(IWin) && JB > 1) break;

                    if (ShadeOn || state.dataSurface->SurfWinSolarDiffusing(IWin)) { // Shade on or diffusing glass
                        int IConstShaded = state.dataSurface->SurfWinActiveShadedConstruction(IWin);
                        if (state.dataSurface->SurfWinSolarDiffusing(IWin)) IConstShaded = state.dataSurface->Surface(IWin).Construction;
                        TransMult(1) = state.dataConstruction->Construct(IConstShaded).TransDiffVis * state.dataSurface->SurfWinGlazedFrac(IWin) *
                                       state.dataSurface->SurfWinLightWellEff(IWin);

                    } else if (ScreenOn) { // Exterior screen on
                        TransScDiffDiffFront = state.dataHeatBal->SurfaceScreens(state.dataSurface->SurfWinScreenNumber(IWin)).DifDifTransVis;
                        TransMult(1) = TransScDiffDiffFront *
                                       (state.dataConstruction->Construct(IConst).TransDiffVis / (1.0 - ReflGlDiffDiffFront * ReflScDiffDiffBack)) *
                                       state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);

                    } else { // Blind on
                        TransBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(JB);
                        if (ShType == WinShadingType::IntBlind) { // Interior blind
                            ReflBlDiffDiffFront = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(JB);
                            TransMult(JB) = TVisSunRefl * (TransBlDiffDiffFront + ReflBlDiffDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront /
                                                                                      (1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack));

                        } else if (ShType == WinShadingType::ExtBlind) { // Exterior blind
                            TransMult(JB) = TransBlDiffDiffFront *
                                            (state.dataConstruction->Construct(IConst).TransDiffVis /
                                             (1.0 - ReflGlDiffDiffFront * state.dataHeatBal->Blind(BlNum).VisBackDiffDiffRefl(JB))) *
                                            state.dataSurface->SurfWinGlazedFrac(IWin) * state.dataSurface->SurfWinLightWellEff(IWin);

                        } else { // Between-glass blind
                            t1 = state.dataConstruction->Construct(IConst).tBareVisDiff(1);
                            tfshBd = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffTrans(JB);
                            rfshB = state.dataHeatBal->Blind(BlNum).VisFrontDiffDiffRefl(JB);
                            if (state.dataConstruction->Construct(IConst).TotGlassLayers == 2) { // 2 glass layers
                                TransMult(JB) =
                                    t1 * (tfshBd * (1.0 + rfd2 * rbshd) + rfshB * rbd1 * tfshd) * td2 * state.dataSurface->SurfWinLightWellEff(IWin);
                            } else { // 3 glass layers; blind between layers 2 and 3
                                t2 = state.dataConstruction->Construct(IConst).tBareVisDiff(2);
                                TransMult(JB) = t1 * t2 * (tfshBd * (1.0 + rfd3 * rbshd) + rfshB * (rbd2 * tfshd + td2 * rbd1 * td2 * tfshd)) * td3 *
                                                state.dataSurface->SurfWinLightWellEff(IWin);
                            }
                        } // End of check of interior/exterior/between-glass blind
                    }     // ShadeOn/BlindOn

                    state.dataDaylightingManager->WLUMSU(IHR, JB + 1) += ZSU1refl * TransMult(JB) / DataGlobalConstants::Pi;
                    FLFWSU(JB + 1) += ZSU1refl * TransMult(JB) * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
                    FLCWSU(JB + 1) += ZSU1refl * TransMult(JB) * state.dataSurface->SurfWinFractionUpgoing(IWin);
                } // End of loop over slat angles
            }     // End of check if window has shade, blind or diffusing glass
        }         // End of check if ZSU1refl > 0.0
    }             // End of check if solar reflections are in effect

    // Sun-related portion of internally reflected illuminance

    for (JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH) {
        if (!state.dataSurface->SurfWinMovableSlats(IWin) && JSH > 2) break;

        // Full area of window is used in following since effect of dividers on reducing
        // effective window transmittance already accounted for in calc of FLFWSU and FLCWSU
        // CR 7869 added effect of intervening interior windows on transmittance and
        // added inside surface area of adjacent zone
        state.dataDaylightingManager->EINTSU(IHR, JSH) =
            (FLFWSU(JSH) * state.dataSurface->SurfWinRhoFloorWall(IWin) + FLCWSU(JSH) * state.dataSurface->SurfWinRhoCeilingWall(IWin)) *
            (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
            (EnclInsideSurfArea * (1.0 - thisEnclDaylight.aveVisDiffReflect));

        state.dataDaylightingManager->EINTSUdisk(IHR, JSH) = FLFWSUdisk(JSH) * state.dataSurface->SurfWinRhoFloorWall(IWin) *
                                                             (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
                                                             (EnclInsideSurfArea * (1.0 - thisEnclDaylight.aveVisDiffReflect));
    }
}

void ComplexFenestrationLuminances(EnergyPlusData &state,
                                   int const IWin,
                                   int const WinEl,
                                   int const NBasis,
                                   int const IHR,
                                   int const iRefPoint,
                                   Array2<Real64> &ElementLuminanceSky,      // sky related luminance at window element (exterior side)
                                   Array1D<Real64> &ElementLuminanceSun,     // sun related luminance at window element (exterior side),
                                   Array1D<Real64> &ElementLuminanceSunDisk, // sun related luminance at window element (exterior side),
                                   DataDaylighting::CalledFor const CalledFrom,
                                   int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    int iIncElem;
    int iSky;
    int SolBmIndex;
    Real64 LambdaInc;
    Real64 Altitude;
    Real64 Azimuth;
    int CurCplxFenState;
    Real64 SunObstrMultiplier; // sun obstruction multiplier used to determine if sun hit the ground point
    Real64 ObstrTrans;         // product of all surface transmittances intersecting incoming beam

    Real64 BeamObstrMultiplier; // beam obstruction multiplier in case incoming beam is from the ground
    int ObsSurfNum;             // Obstruction surface number
    bool hitObs;                // True iff obstruction is hit
    auto &ComplexFenestrationLuminancesObsHitPt =
        state.dataDaylightingManager->ComplexFenestrationLuminancesObsHitPt; // Coordinates of hit point on an obstruction (m)
    auto &ComplexFenestrationLuminancesGroundHitPt =
        state.dataDaylightingManager
            ->ComplexFenestrationLuminancesGroundHitPt; // Coordinates of point that ray from window center hits the ground (m)

    int NRefl;          // number of exterior obstructions
    int iReflElem;      // incoming direction blocking surfaces element counter
    int iReflElemIndex; // reflection element index

    int NGnd;          // number of ground elements
    int iGndElem;      // ground elements counter
    int iGndElemIndex; // ground element index

    CurCplxFenState = state.dataSurface->SurfaceWindow(IWin).ComplexFen.CurrentState;

    // Calculate luminance from sky and sun excluding exterior obstruction transmittances and obstruction multipliers
    SolBmIndex = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).SolBmIndex(IHR, state.dataGlobal->TimeStep);
    for (iIncElem = 1; iIncElem <= NBasis; ++iIncElem) {
        LambdaInc = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).Inc.Lamda(iIncElem);
        // COSB = ComplexWind(IWin)%Geom(CurCplxFenState)%CosInc(iIncElem)
        // DA = ComplexWind(IWin)%Geom(CurCplxFenState)%DAInc(iIncElem)
        Altitude = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).pInc(iIncElem).Altitude;
        Azimuth = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).pInc(iIncElem).Azimuth;
        if (Altitude > 0.0) {
            // Ray from sky element
            for (iSky = 1; iSky <= 4; ++iSky) {
                ElementLuminanceSky(iSky, iIncElem) = DayltgSkyLuminance(state, iSky, Azimuth, Altitude) * LambdaInc;
            }
        } else if (Altitude < 0.0) {
            // Ray from ground element
            // BeamObstrMultiplier = ComplexWind(IWin)%DaylghtGeom(CurCplxFenState)%GndObstrMultiplier(WinEl, iIncElem)
            for (iSky = 1; iSky <= 4; ++iSky) {
                ElementLuminanceSky(iSky, iIncElem) =
                    state.dataDaylightingManager->GILSK(IHR, iSky) * state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi * LambdaInc;
            }
            ElementLuminanceSun(iIncElem) =
                state.dataDaylightingManager->GILSU(IHR) * state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi * LambdaInc;
        } else {
            // Ray from the element which is half sky and half ground
            for (iSky = 1; iSky <= 4; ++iSky) {
                // in this case half of the pach is coming from the sky and half from the ground
                ElementLuminanceSky(iSky, iIncElem) = 0.5 * DayltgSkyLuminance(state, iSky, Azimuth, Altitude) * LambdaInc;
                ElementLuminanceSky(iSky, iIncElem) += 0.5 * state.dataDaylightingManager->GILSK(IHR, iSky) *
                                                       state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi * LambdaInc;
            }
            ElementLuminanceSun(iIncElem) =
                0.5 * state.dataDaylightingManager->GILSU(IHR) * state.dataEnvrn->GndReflectanceForDayltg / DataGlobalConstants::Pi * LambdaInc;
        }
        // Sun beam calculations
        if ((SolBmIndex == iIncElem) && (state.dataHeatBal->SurfSunlitFracHR(IHR, IWin) > 0.0)) {
            ElementLuminanceSunDisk(iIncElem) = 1.0;
        }
    }

    // add exterior obstructions transmittances to calculated luminances
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        NRefl = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).NReflSurf(WinEl);
    } else {
        assert(MapNum > 0);
        NRefl = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).NReflSurf(WinEl);
    }
    for (iReflElem = 1; iReflElem <= NRefl; ++iReflElem) {
        if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            ObstrTrans = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).TransOutSurf(iReflElem, WinEl);
            iReflElemIndex = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).RefSurfIndex(iReflElem, WinEl);
        } else {
            ObstrTrans =
                state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).TransOutSurf(iReflElem, WinEl);
            iReflElemIndex =
                state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).RefSurfIndex(iReflElem, WinEl);
        }

        for (iSky = 1; iSky <= 4; ++iSky) {
            ElementLuminanceSky(iSky, iReflElemIndex) *= ObstrTrans;
        }
        ElementLuminanceSun(iReflElemIndex) *= ObstrTrans;
        ElementLuminanceSunDisk(iReflElemIndex) *= ObstrTrans;
    }

    // add exterior ground element obstruction multipliers to calculated luminances. For sun reflection, calculate if
    // sun reaches the ground for that point
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        NGnd = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).NGnd(WinEl);
    } else {
        NGnd = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).NGnd(WinEl);
    }
    Vector3<Real64> const SUNCOS_IHR(state.dataSurface->SurfSunCosHourly(IHR));
    for (iGndElem = 1; iGndElem <= NGnd; ++iGndElem) {
        // case for sky elements. Integration is done over upper ground hemisphere to determine how many obstructions
        // were hit in the process
        if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            BeamObstrMultiplier =
                state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).GndObstrMultiplier(iGndElem, WinEl);
            iGndElemIndex = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).GndIndex(iGndElem, WinEl);
        } else {
            BeamObstrMultiplier = state.dataBSDFWindow->ComplexWind(IWin)
                                      .DaylghtGeom(CurCplxFenState)
                                      .IlluminanceMap(iRefPoint, MapNum)
                                      .GndObstrMultiplier(iGndElem, WinEl);
            iGndElemIndex =
                state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).GndIndex(iGndElem, WinEl);
        }
        for (iSky = 1; iSky <= 4; ++iSky) {
            ElementLuminanceSky(iSky, iGndElemIndex) *= BeamObstrMultiplier;
        }

        // direct sun disk reflect off the ground
        SunObstrMultiplier = 1.0;
        if (state.dataSurface->CalcSolRefl) {
            // Sun reaches ground point if vector from this point to the sun is unobstructed
            hitObs = false;
            for (ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
                if (!state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) continue;
                if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                    ComplexFenestrationLuminancesGroundHitPt(1) =
                        state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).GndPt(iGndElem, WinEl).x;
                    ComplexFenestrationLuminancesGroundHitPt(2) =
                        state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).GndPt(iGndElem, WinEl).y;
                    ComplexFenestrationLuminancesGroundHitPt(3) =
                        state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).GndPt(iGndElem, WinEl).z;
                } else {
                    ComplexFenestrationLuminancesGroundHitPt(1) = state.dataBSDFWindow->ComplexWind(IWin)
                                                                      .DaylghtGeom(CurCplxFenState)
                                                                      .IlluminanceMap(iRefPoint, MapNum)
                                                                      .GndPt(iGndElem, WinEl)
                                                                      .x;
                    ComplexFenestrationLuminancesGroundHitPt(2) = state.dataBSDFWindow->ComplexWind(IWin)
                                                                      .DaylghtGeom(CurCplxFenState)
                                                                      .IlluminanceMap(iRefPoint, MapNum)
                                                                      .GndPt(iGndElem, WinEl)
                                                                      .y;
                    ComplexFenestrationLuminancesGroundHitPt(3) = state.dataBSDFWindow->ComplexWind(IWin)
                                                                      .DaylghtGeom(CurCplxFenState)
                                                                      .IlluminanceMap(iRefPoint, MapNum)
                                                                      .GndPt(iGndElem, WinEl)
                                                                      .z;
                }

                PierceSurface(state, ObsSurfNum, ComplexFenestrationLuminancesGroundHitPt, SUNCOS_IHR, ComplexFenestrationLuminancesObsHitPt, hitObs);
                if (hitObs) break;
            }
            if (hitObs) SunObstrMultiplier = 0.0;
        }
        ElementLuminanceSun(iGndElemIndex) *= SunObstrMultiplier;
    }
}

void DayltgInterReflectedIllumComplexFenestration(EnergyPlusData &state,
                                                  int const IWin,            // Window index
                                                  int const WinEl,           // Current window element counter
                                                  int const IHR,             // Hour of day
                                                  int const daylightCtrlNum, // Daylighting control number
                                                  int const iRefPoint,       // reference point counter
                                                  DataDaylighting::CalledFor const CalledFrom,
                                                  int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   April 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Called from CalcDayltgCoefficients for each complex (bsdf) fenestration and reference point in a daylit
    // space, for each sun position. Calculates illuminance (EINTSK and EINTSU) at reference point due
    // to internally reflected light by integrating to determine the amount of flux from
    // sky and ground (and beam reflected from obstructions) transmitted through
    // the center of the window and then reflecting this
    // light from the inside surfaces of the space.

    Array2D<Real64> FLSK;     // Sky related luminous flux
    Array1D<Real64> FLSU;     // Sun related luminous flux, excluding entering beam
    Array1D<Real64> FLSUdisk; // Sun related luminous flux, due to entering beam

    Array2D<Real64> FirstFluxSK;     // Sky related first reflected flux
    Array1D<Real64> FirstFluxSU;     // Sun related first reflected flux, excluding entering beam
    Array1D<Real64> FirstFluxSUdisk; // Sun related first reflected flux, due to entering beam

    Array2D<Real64> ElementLuminanceSky;     // sky related luminance at window element (exterior side)
    Array1D<Real64> ElementLuminanceSun;     // sun related luminance at window element (exterior side), exluding beam
    Array1D<Real64> ElementLuminanceSunDisk; // sun related luminance at window element (exterior side), due to sun beam
    Real64 FLSUTot;
    Real64 FLSUdiskTot;

    // Total for first relflected fluxes
    auto &FFSKTot = state.dataDaylightingManager->FFSKTot;
    Real64 FFSUTot;
    Real64 FFSUdiskTot;

    Real64 COSIncSun; // cosine of sun incidence angle (from basis elements)

    int iSky;   // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
    int iConst; // Construction number

    int CurCplxFenState;
    int NIncBasis;
    int NTrnBasis;
    int SolBmIndex; // index of current sun position

    int iIncElem;  // incoming direction counter
    int iBackElem; // outgoing direction counter

    Real64 LambdaInc; // current lambda value for incoming direction
    // REAL(r64) :: LambdaTrn  ! current lambda value for incoming direction
    Real64 dirTrans; // directional bsdf transmittance

    CurCplxFenState = state.dataSurface->SurfaceWindow(IWin).ComplexFen.CurrentState;
    iConst = state.dataSurface->SurfaceWindow(IWin).ComplexFen.State(CurCplxFenState).Konst;
    NTrnBasis = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).Trn.NBasis;

    if (!allocated(FLSK)) FLSK.allocate(4, NTrnBasis);
    FLSK = 0.0;
    if (!allocated(FLSU)) FLSU.dimension(NTrnBasis, 0.0);
    if (!allocated(FLSUdisk)) FLSUdisk.dimension(NTrnBasis, 0.0);

    if (!allocated(FirstFluxSK)) FirstFluxSK.allocate(4, NTrnBasis);
    FirstFluxSK = 0.0;
    if (!allocated(FirstFluxSU)) FirstFluxSU.dimension(NTrnBasis, 0.0);
    if (!allocated(FirstFluxSUdisk)) FirstFluxSUdisk.dimension(NTrnBasis, 0.0);

    NIncBasis = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).Inc.NBasis;
    if (!allocated(ElementLuminanceSky)) ElementLuminanceSky.allocate(4, NIncBasis);
    ElementLuminanceSky = 0.0;
    if (!allocated(ElementLuminanceSun)) ElementLuminanceSun.dimension(NIncBasis, 0.0);
    if (!allocated(ElementLuminanceSunDisk)) ElementLuminanceSunDisk.dimension(NIncBasis, 0.0);

    // Integration over sky/ground/sun elements is done over window incoming basis element and flux is calculated for each
    // outgoing direction. This is used to calculate first reflected flux

    ComplexFenestrationLuminances(
        state, IWin, WinEl, NIncBasis, IHR, iRefPoint, ElementLuminanceSky, ElementLuminanceSun, ElementLuminanceSunDisk, CalledFrom, MapNum);

    // luminance from sun disk needs to include fraction of sunlit area
    SolBmIndex = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).SolBmIndex(IHR, state.dataGlobal->TimeStep);
    if (SolBmIndex > 0) {
        COSIncSun = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).CosInc(SolBmIndex);
    } else {
        COSIncSun = 0.0;
    }
    ElementLuminanceSunDisk *= state.dataHeatBal->SurfSunlitFracHR(IHR, IWin) * COSIncSun;

    //        FLSKTot = 0.0;
    FLSUTot = 0.0;
    FLSUdiskTot = 0.0;
    FFSKTot = 0.0;
    FFSUTot = 0.0;
    FFSUdiskTot = 0.0;
    // now calculate flux into each outgoing direction by integrating over all incoming directions
    for (iBackElem = 1; iBackElem <= NTrnBasis; ++iBackElem) {
        for (iIncElem = 1; iIncElem <= NIncBasis; ++iIncElem) {
            LambdaInc = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).Inc.Lamda(iIncElem);
            dirTrans = state.dataConstruction->Construct(iConst).BSDFInput.VisFrtTrans(iBackElem, iIncElem);

            for (iSky = 1; iSky <= 4; ++iSky) {
                FLSK(iSky, iBackElem) += dirTrans * LambdaInc * ElementLuminanceSky(iSky, iIncElem);
            }

            FLSU(iBackElem) += dirTrans * LambdaInc * ElementLuminanceSun(iIncElem);
            FLSUdisk(iBackElem) += dirTrans * LambdaInc * ElementLuminanceSunDisk(iIncElem);
        }

        for (iSky = 1; iSky <= 4; ++iSky) {
            FirstFluxSK(iSky, iBackElem) =
                FLSK(iSky, iBackElem) * state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).AveRhoVisOverlap(iBackElem);
            FFSKTot(iSky) += FirstFluxSK(iSky, iBackElem);
            //                FLSKTot( iSky ) += FLSK( iSky, iBackElem );
        }
        FirstFluxSU(iBackElem) = FLSU(iBackElem) * state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).AveRhoVisOverlap(iBackElem);
        FFSUTot += FirstFluxSU(iBackElem);
        FLSUTot += FLSU(iBackElem);

        FirstFluxSUdisk(iBackElem) = FLSUdisk(iBackElem) * state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).AveRhoVisOverlap(iBackElem);
        FFSUdiskTot += FirstFluxSUdisk(iBackElem);
        FLSUdiskTot += FLSUdisk(iBackElem);
    }

    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(state.dataDaylightingData->daylightControl(daylightCtrlNum).enclIndex);
    Real64 EnclInsideSurfArea = thisEnclDaylight.totInsSurfArea;
    for (iSky = 1; iSky <= 4; ++iSky) {
        state.dataDaylightingManager->EINTSK(IHR, 1, iSky) = FFSKTot(iSky) *
                                                             (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
                                                             (EnclInsideSurfArea * (1.0 - thisEnclDaylight.aveVisDiffReflect));
    }
    state.dataDaylightingManager->EINTSU(IHR, 1) = FFSUTot * (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
                                                   (EnclInsideSurfArea * (1.0 - thisEnclDaylight.aveVisDiffReflect));
    state.dataDaylightingManager->EINTSUdisk(IHR, 1) = FFSUdiskTot *
                                                       (state.dataSurface->Surface(IWin).Area / state.dataSurface->SurfWinGlazedFrac(IWin)) /
                                                       (EnclInsideSurfArea * (1.0 - thisEnclDaylight.aveVisDiffReflect));

    if (allocated(FLSK)) FLSK.deallocate();
    if (allocated(FLSU)) FLSU.deallocate();
    if (allocated(FLSUdisk)) FLSUdisk.deallocate();

    if (allocated(FirstFluxSK)) FirstFluxSK.deallocate();
    if (allocated(FirstFluxSU)) FirstFluxSU.deallocate();
    if (allocated(FirstFluxSUdisk)) FirstFluxSUdisk.deallocate();

    if (allocated(ElementLuminanceSky)) ElementLuminanceSky.deallocate();
    if (allocated(ElementLuminanceSun)) ElementLuminanceSun.deallocate();
    if (allocated(ElementLuminanceSunDisk)) ElementLuminanceSunDisk.deallocate();
}

void DayltgDirectIllumComplexFenestration(EnergyPlusData &state,
                                          int const IWin,      // Window index
                                          int const WinEl,     // Current window element counter
                                          int const IHR,       // Hour of day
                                          int const iRefPoint, // reference point index
                                          DataDaylighting::CalledFor const CalledFrom,
                                          int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Luminances from different sources to the window
    Array2D<Real64> ElementLuminanceSky; // sky related luminance at window element (exterior side)
    Array1D<Real64> ElementLuminanceSun; // sun related luminance at window element (exterior side),
    // exluding beam
    Array1D<Real64> ElementLuminanceSunDisk; // sun related luminance at window element (exterior side),
    // due to sun beam

    auto &WinLumSK = state.dataDaylightingManager->WinLumSK; // Sky related window luminance
    auto &EDirSky = state.dataDaylightingManager->EDirSky;   // Sky related direct illuminance
    Real64 WinLumSU;                                         // Sun related window luminance, excluding entering beam
    Real64 EDirSun;                                          // Sun related direct illuminance, excluding entering beam
    int CurCplxFenState;
    int NIncBasis;
    int RefPointIndex; // reference point patch number
    int iIncElem;
    int iConst;
    int iSky;

    Real64 dirTrans;    // directional BSDF transmittance
    Real64 dOmega;      // solid view angle of current element
    Real64 zProjection; // z-axe projection of solid view angle (used to calculate amount of light at horizontal surface
    // laying at reference point)

    CurCplxFenState = state.dataSurface->SurfaceWindow(IWin).ComplexFen.CurrentState;
    iConst = state.dataSurface->SurfaceWindow(IWin).ComplexFen.State(CurCplxFenState).Konst;
    NIncBasis = state.dataBSDFWindow->ComplexWind(IWin).Geom(CurCplxFenState).Inc.NBasis;

    if (!allocated(ElementLuminanceSky)) ElementLuminanceSky.allocate(4, NIncBasis);
    ElementLuminanceSky = 0.0;
    if (!allocated(ElementLuminanceSun)) ElementLuminanceSun.dimension(NIncBasis, 0.0);
    if (!allocated(ElementLuminanceSunDisk)) ElementLuminanceSunDisk.dimension(NIncBasis, 0.0);

    ComplexFenestrationLuminances(
        state, IWin, WinEl, NIncBasis, IHR, iRefPoint, ElementLuminanceSky, ElementLuminanceSun, ElementLuminanceSunDisk, CalledFrom, MapNum);

    // find number of outgoing basis towards current reference point
    if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
        RefPointIndex = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).RefPointIndex(WinEl);
        dOmega = state.dataBSDFWindow->ComplexWind(IWin).RefPoint(iRefPoint).SolidAngle(WinEl);
        zProjection = state.dataBSDFWindow->ComplexWind(IWin).RefPoint(iRefPoint).SolidAngleVec(WinEl).z;
    } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
        assert(MapNum > 0);
        RefPointIndex = state.dataBSDFWindow->ComplexWind(IWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).RefPointIndex(WinEl);
        dOmega = state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap(iRefPoint, MapNum).SolidAngle(WinEl);
        zProjection = state.dataBSDFWindow->ComplexWind(IWin).IlluminanceMap(iRefPoint, MapNum).SolidAngleVec(WinEl).z;
    }

    WinLumSK = 0.0;
    WinLumSU = 0.0;
    // WinLumSUdisk = 0.0d0
    EDirSky = 0.0;
    EDirSun = 0.0;
    //        EDirSunDisk = 0.0; //Unused Set but never used

    for (iIncElem = 1; iIncElem <= NIncBasis; ++iIncElem) {
        // LambdaInc = ComplexWind(IWin)%Geom(CurCplxFenState)%Inc%Lamda(iIncElem)
        dirTrans = state.dataConstruction->Construct(iConst).BSDFInput.VisFrtTrans(RefPointIndex, iIncElem);

        for (iSky = 1; iSky <= 4; ++iSky) {
            WinLumSK(iSky) += dirTrans * ElementLuminanceSky(iSky, iIncElem);
        }

        WinLumSU += dirTrans * ElementLuminanceSun(iIncElem);

        // For sun disk need to go throug outgoing directions and see which directions actually contain reference point
    }

    if (zProjection > 0.0) {
        for (iSky = 1; iSky <= 4; ++iSky) {
            EDirSky(iSky) = WinLumSK(iSky) * dOmega * zProjection;
        }
        EDirSun = WinLumSU * dOmega * zProjection;
    }

    // Store solution in global variables
    for (iSky = 1; iSky <= 4; ++iSky) {
        state.dataDaylightingManager->AVWLSK(IHR, 1, iSky) += WinLumSK(iSky);
        state.dataDaylightingManager->EDIRSK(IHR, 1, iSky) += EDirSky(iSky);
    }

    state.dataDaylightingManager->AVWLSU(IHR, 1) += WinLumSU;
    state.dataDaylightingManager->EDIRSU(IHR, 1) += EDirSun;
    // AVWLSUdisk(1,IHR) = AVWLSUdisk(1,IHR) + WinLumSUdisk
}

void DayltgDirectSunDiskComplexFenestration(EnergyPlusData &state,
                                            int const iWin,  // Window index
                                            int const iHour, // Hour of day
                                            int const iRefPoint,
                                            int const NumEl,                             // Total number of window elements
                                            Real64 const AZVIEW,                         // Azimuth of view vector in absolute coord system for
                                            DataDaylighting::CalledFor const CalledFrom, // indicate  which type of routine called this routine
                                            int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   June 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate illuminance from sun disk for complex fenestration systems

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CurCplxFenState;
    int iConst;
    int SolBmIndex;
    int NTrnBasis;
    int iTrnElem;
    Real64 WindowSolidAngleDaylightPoint(0.0);
    Real64 XR;
    Real64 YR;
    Real64 PosFac;
    Real64 dirTrans;
    Real64 LambdaTrn;
    Real64 WinLumSunDisk; // window luminance from sun disk
    Real64 ELumSunDisk;   // window illuminance from sun disk
    Real64 TransBeam;     // transmittance of the beam for given direction
    auto &DayltgDirectSunDiskComplexFenestrationV = state.dataDaylightingManager->DayltgDirectSunDiskComplexFenestrationV;       // temporary vector
    auto &DayltgDirectSunDiskComplexFenestrationRWin = state.dataDaylightingManager->DayltgDirectSunDiskComplexFenestrationRWin; // Window center
    Real64 RayZ; // z component of unit vector for outgoing direction
    bool refPointIntersect;

    CurCplxFenState = state.dataSurface->SurfaceWindow(iWin).ComplexFen.CurrentState;
    iConst = state.dataSurface->SurfaceWindow(iWin).ComplexFen.State(CurCplxFenState).Konst;
    SolBmIndex = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).SolBmIndex(iHour, state.dataGlobal->TimeStep);

    switch (CalledFrom) {
    case DataDaylighting::CalledFor::RefPoint: {
        WindowSolidAngleDaylightPoint = state.dataSurface->SurfaceWindow(iWin).SolidAngAtRefPtWtd(iRefPoint);
    } break;
    case DataDaylighting::CalledFor::MapPoint: {
        WindowSolidAngleDaylightPoint = 0.0;
    } break;
    default: {
        assert(false); // Bad CalledFrom argument
    } break;
    }

    if (WindowSolidAngleDaylightPoint < 1e-6) return;

    WinLumSunDisk = 0.0;
    ELumSunDisk = 0.0;
    NTrnBasis = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).Trn.NBasis;
    for (iTrnElem = 1; iTrnElem <= NTrnBasis; ++iTrnElem) {
        // if ray from any part of the window can reach reference point
        if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
            refPointIntersect =
                state.dataBSDFWindow->ComplexWind(iWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).RefPointIntersection(iTrnElem);
        } else if (CalledFrom == DataDaylighting::CalledFor::MapPoint) {
            assert(MapNum > 0);
            refPointIntersect =
                state.dataBSDFWindow->ComplexWind(iWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).RefPointIntersection(iTrnElem);
        }
        if (refPointIntersect) {
            if (CalledFrom == DataDaylighting::CalledFor::RefPoint) {
                PosFac = state.dataBSDFWindow->ComplexWind(iWin).DaylghtGeom(CurCplxFenState).RefPoint(iRefPoint).RefPtIntPosFac(iTrnElem);
            } else {
                PosFac =
                    state.dataBSDFWindow->ComplexWind(iWin).DaylghtGeom(CurCplxFenState).IlluminanceMap(iRefPoint, MapNum).RefPtIntPosFac(iTrnElem);
            }
            RayZ = -state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).sTrn(iTrnElem).z;

            // Need to recalculate position factor for dominant direction in case of specular bsdf.  Otherwise this will produce
            // very inaccurate results because of position factor of the sun and bsdf pach can vary by lot
            if (iTrnElem == SolBmIndex) {
                XR = std::tan(std::abs(DataGlobalConstants::PiOvr2 - AZVIEW - state.dataDaylightingManager->THSUN) + 0.001);
                YR = std::tan(state.dataDaylightingManager->PHSUN + 0.001);
                PosFac = DayltgGlarePositionFactor(XR, YR);
                RayZ = state.dataDaylightingManager->SPHSUN;
            }

            if (PosFac != 0.0) {
                if (SolBmIndex > 0) {
                    dirTrans = state.dataConstruction->Construct(iConst).BSDFInput.VisFrtTrans(iTrnElem, SolBmIndex);
                } else {
                    dirTrans = 0.0;
                }
                LambdaTrn = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).Trn.Lamda(iTrnElem);

                DayltgDirectSunDiskComplexFenestrationV(1) = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).sTrn(iTrnElem).x;
                DayltgDirectSunDiskComplexFenestrationV(2) = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).sTrn(iTrnElem).y;
                DayltgDirectSunDiskComplexFenestrationV(3) = state.dataBSDFWindow->ComplexWind(iWin).Geom(CurCplxFenState).sTrn(iTrnElem).z;
                DayltgDirectSunDiskComplexFenestrationV = -DayltgDirectSunDiskComplexFenestrationV;

                DayltgDirectSunDiskComplexFenestrationRWin(1) = state.dataSurface->Surface(iWin).Centroid.x;
                DayltgDirectSunDiskComplexFenestrationRWin(2) = state.dataSurface->Surface(iWin).Centroid.y;
                DayltgDirectSunDiskComplexFenestrationRWin(3) = state.dataSurface->Surface(iWin).Centroid.z;

                DayltgHitObstruction(
                    state, iHour, iWin, DayltgDirectSunDiskComplexFenestrationRWin, DayltgDirectSunDiskComplexFenestrationV, TransBeam);

                WinLumSunDisk += (14700.0 * std::sqrt(0.000068 * PosFac) * double(NumEl) / std::pow(WindowSolidAngleDaylightPoint, 0.8)) * dirTrans *
                                 LambdaTrn * TransBeam;

                ELumSunDisk += RayZ * dirTrans * LambdaTrn * TransBeam;
            }
        }
    }

    state.dataDaylightingManager->AVWLSUdisk(iHour, 1) = WinLumSunDisk;
    state.dataDaylightingManager->EDIRSUdisk(iHour, 1) = ELumSunDisk;
}

Real64 DayltgSkyLuminance(EnergyPlusData &state,
                          int const ISky,     // Sky type: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
                          Real64 const THSKY, // Azimuth and altitude of sky element (radians)
                          Real64 const PHSKY)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Called by CalcDayltgCoefficients, DayltgExtHorizIllum AND DayltgInterReflectedIllum.  gives
    // luminance in cd/m2 for four different sky types, as described in R.Perez, P.Ineichen,
    // R.Seals, J.Michalsky and R.Stewart, "Modeling daylight availability and irradiance
    // components from direct and global irradiance," Solar Energy 44, 1990, 271-289.
    // The luminance distributions in this routine are normalized such that
    // the zenith luminance is 1.0, i.e., DayltgSkyLuminance =
    // (sky luminance at THSKY, PHSKY)/(zenith luminance), which is dimensionless.
    // The sky types are:
    // 1. Standard CIE clear sky
    // 2. Standard CIE high-turbidity clear sky
    // 3. CIE intermediate sky
    // 4. CIE overcast sky

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // Based on DOE-2.1E subroutine DSKYLU, which did only clear and overcast skies.

    // OTHER NOTES:
    // THSKY ranges from 0 to 2Pi starting with 0 directly East and rotating clockwise.
    // PHSKY ranges from 0 to Pi starting with 0 at the horizon and Pi/2 at the zenith.

    // USE STATEMENTS: na

    // Return value
    Real64 DayltgSkyLuminance(0.0); // Luminance of sky element divided by zenith luminance

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 SPHSKY;    // Sine of PHSKY
    Real64 G(0.0);    // Angle between sun and element of sky (radians)
    Real64 COSG(0.0); // Cosine of G
    Real64 Z;         // Solar zenith angle (radians)
    Real64 Z1;        // Luminance factors (intermediate variables)
    Real64 Z2;
    Real64 Z3;
    Real64 Z4;

    SPHSKY = max(std::sin(PHSKY), 0.01); // Prevent floating point underflows
    Z = DataGlobalConstants::PiOvr2 - state.dataDaylightingManager->PHSUN;
    if (ISky >= 1 && ISky <= 3) { // Following not needed for overcast sky
        COSG = SPHSKY * state.dataDaylightingManager->SPHSUN +
               std::cos(PHSKY) * state.dataDaylightingManager->CPHSUN * std::cos(THSKY - state.dataDaylightingManager->THSUN);
        COSG = max(DataPrecisionGlobals::constant_minusone, min(COSG, 1.0)); // Prevent out of range due to roundoff
        G = std::acos(COSG);
    }

    if (ISky == 1) { // Clear Sky
        Z1 = 0.910 + 10.0 * std::exp(-3.0 * G) + 0.45 * COSG * COSG;
        Z2 = 1.0 - std::exp(-0.32 / SPHSKY);
        Z3 = 0.27385 * (0.91 + 10.0 * std::exp(-3.0 * Z) + 0.45 * state.dataDaylightingManager->SPHSUN * state.dataDaylightingManager->SPHSUN);
        DayltgSkyLuminance = Z1 * Z2 / Z3;

    } else if (ISky == 2) { // Clear turbid sky
        Z1 = 0.856 + 16.0 * std::exp(-3.0 * G) + 0.3 * COSG * COSG;
        Z2 = 1.0 - std::exp(-0.32 / SPHSKY);
        Z3 = 0.27385 * (0.856 + 16.0 * std::exp(-3.0 * Z) + 0.3 * state.dataDaylightingManager->SPHSUN * state.dataDaylightingManager->SPHSUN);
        DayltgSkyLuminance = Z1 * Z2 / Z3;

    } else if (ISky == 3) { // Intermediate sky
        Z1 = (1.35 * (std::sin(3.59 * PHSKY - 0.009) + 2.31) * std::sin(2.6 * state.dataDaylightingManager->PHSUN + 0.316) + PHSKY + 4.799) / 2.326;
        Z2 = std::exp(-G * 0.563 * ((state.dataDaylightingManager->PHSUN - 0.008) * (PHSKY + 1.059) + 0.812));
        Z3 = 0.99224 * std::sin(2.6 * state.dataDaylightingManager->PHSUN + 0.316) + 2.73852;
        Z4 = std::exp(-Z * 0.563 * ((state.dataDaylightingManager->PHSUN - 0.008) * 2.6298 + 0.812));
        DayltgSkyLuminance = Z1 * Z2 / (Z3 * Z4);

    } else if (ISky == 4) { // Overcast sky
        DayltgSkyLuminance = (1.0 + 2.0 * SPHSKY) / 3.0;
    }

    return DayltgSkyLuminance;
}

void ProfileAngle(EnergyPlusData &state,
                  int const SurfNum,                                      // Surface number
                  Vector3<Real64> const &CosDirSun,                       // Solar direction cosines
                  DataWindowEquivalentLayer::Orientation const HorOrVert, // If HORIZONTAL, calculates ProfileAngHor
                  Real64 &ProfileAng                                      // Solar profile angle (radians).
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   May 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates profile angle for a surface.

    // Using/Aliasing
    using namespace DataSurfaces;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // For HorOrVert = HORIZONTAL,
    //  this is the incidence angle in a plane that is normal to the window
    //  and parallel to the Y-axis of the window (the axis along
    //  which the height of the window is measured).
    //  For HorOrVert = VERTICAL,
    //  this is the incidence angle in a plane that is normal to the window
    //  and parallel to the X-axis of the window (the axis along
    //  which the width of the window is measured).
    // If VERTICAL, calculates ProfileAngVert

    // SUBROUTINE PARAMETER DEFINITIONS: na
    // INTERFACE BLOCK SPECIFICATIONS: na
    // DERIVED TYPE DEFINITIONS: na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ElevSun;                                          // Sun elevation; angle between sun and horizontal (radians)
    Real64 ElevWin;                                          // Window elevation: angle between window outward normal and horizontal (radians)
    Real64 AzimWin;                                          // Window azimuth (radians)
    Real64 AzimSun;                                          // Sun azimuth (radians)
    Real64 ThWin;                                            // Azimuth angle of WinNorm
    auto &WinNorm = state.dataDaylightingManager->WinNorm;   // Window outward normal unit vector
    auto &SunPrime = state.dataDaylightingManager->SunPrime; // Projection of sun vector onto plane (perpendicular to window plane) determined by
                                                             // WinNorm and vector along baseline of window
    auto &WinNormCrossBase = state.dataDaylightingManager->WinNormCrossBase; // Cross product of WinNorm and vector along window baseline

    if (HorOrVert == DataWindowEquivalentLayer::Orientation::Horizontal) { // Profile angle for horizontal structures
        ElevWin = DataGlobalConstants::PiOvr2 - state.dataSurface->Surface(SurfNum).Tilt * DataGlobalConstants::DegToRadians;
        AzimWin = (90.0 - state.dataSurface->Surface(SurfNum).Azimuth) * DataGlobalConstants::DegToRadians;
        ElevSun = std::asin(CosDirSun(3));
        AzimSun = std::atan2(CosDirSun(2), CosDirSun(1));
        ProfileAng = std::atan(std::sin(ElevSun) / std::abs(std::cos(ElevSun) * std::cos(AzimWin - AzimSun))) - ElevWin;
    } else { // Profile angle for vertical structures
        ElevWin = DataGlobalConstants::PiOvr2 - state.dataSurface->Surface(SurfNum).Tilt * DataGlobalConstants::DegToRadians;
        AzimWin = state.dataSurface->Surface(SurfNum).Azimuth * DataGlobalConstants::DegToRadians; // 7952
        AzimSun = std::atan2(CosDirSun(1), CosDirSun(2));                                          // 7952
        if (std::abs(ElevWin) < 0.1) {                                                             // Near-vertical window
            ProfileAng = AzimWin - AzimSun;                                                        // CR7952 allow sign changes.
        } else {
            WinNorm = state.dataSurface->Surface(SurfNum).OutNormVec;
            ThWin = AzimWin - DataGlobalConstants::PiOvr2;
            Real64 const sin_ElevWin(std::sin(ElevWin));
            WinNormCrossBase(1) = -sin_ElevWin * std::cos(ThWin);
            WinNormCrossBase(2) = sin_ElevWin * std::sin(ThWin);
            WinNormCrossBase(3) = std::cos(ElevWin);
            SunPrime = CosDirSun - WinNormCrossBase * dot(CosDirSun, WinNormCrossBase);
            ProfileAng = std::abs(std::acos(dot(WinNorm, SunPrime) / SunPrime.magnitude()));
            // CR7952 correct sign of result for vertical slats
            if ((AzimWin - AzimSun) < 0.0) ProfileAng = -1.0 * ProfileAng;
        }
        // Constrain to 0 to pi
        if (ProfileAng > DataGlobalConstants::Pi) ProfileAng = 2.0 * DataGlobalConstants::Pi - ProfileAng;
    }
}

void DayltgClosestObstruction(EnergyPlusData &state,
                              Vector3<Real64> const &RecPt,  // Point on window from which ray emanates (m)
                              Vector3<Real64> const &RayVec, // Unit vector along ray pointing away from window (m)
                              int &NearestHitSurfNum,        // Surface number of nearest obstruction that is hit by ray;
                              Vector3<Real64> &NearestHitPt  // Ray's hit point on nearest obstruction (m)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   November 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Determines surface number and hit point of closest exterior obstruction hit
    // by a ray from a window. If no obstruction is hit, NearestHitSurfNum = 0.

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    //  = 0 if no obstruction is hit.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &HitPt = state.dataDaylightingManager->HitPt; // Hit point on an obstruction (m)
    bool hit;                                          // True iff obstruction is hit

    NearestHitSurfNum = 0;
    Real64 NearestHitDistance_sq(std::numeric_limits<Real64>::max()); // Distance squared from receiving point to nearest hit point for a ray (m^2)
    NearestHitPt = 0.0;
    if (state.dataSurface->TotSurfaces < octreeCrossover) { // Linear search through surfaces

        for (int ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
            if (state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) {
                // Determine if this ray hits the surface and, if so, get the distance from the receiving point to the hit
                PierceSurface(state, ObsSurfNum, RecPt, RayVec, HitPt, hit);
                if (hit) { // Ray pierces surface
                    // If obstruction is a window and its base surface is the nearest obstruction hit so far set nearestHitSurface to this window
                    // Note that in this case NearestHitDistance_sq has already been calculated, so does not have to be recalculated
                    if ((state.dataSurface->Surface(ObsSurfNum).Class == SurfaceClass::Window) &&
                        (state.dataSurface->Surface(ObsSurfNum).BaseSurf == NearestHitSurfNum)) {
                        NearestHitSurfNum = ObsSurfNum;
                    } else {
                        // Distance squared from receiving point to hit point
                        Real64 const HitDistance_sq(distance_squared(HitPt, RecPt));
                        // Reset NearestHitSurfNum and NearestHitDistance_sq if this hit point is closer than previous closest
                        if (HitDistance_sq < NearestHitDistance_sq) {
                            NearestHitDistance_sq = HitDistance_sq;
                            NearestHitSurfNum = ObsSurfNum;
                            NearestHitPt = HitPt;
                        }
                    }
                } // End of check if obstruction was hit
            }
        } // End of loop over possible obstructions for this ray

    } else { // Surface octree search

        SurfaceData const *nearestHitSurface(nullptr);

        // Lambda function for the octree to test for surface hit
        auto surfaceHit = [=, &state, &RecPt, &RayVec, &hit, &NearestHitDistance_sq, &nearestHitSurface, &NearestHitPt](SurfaceData const &surface) {
            if (surface.IsShadowPossibleObstruction) {
                // Determine if this ray hits the surface and, if so, get the distance from the receiving point to the hit
                PierceSurface(surface, RecPt, RayVec, state.dataDaylightingManager->HitPt, hit); // Check if ray pierces surface
                if (hit) {                                                                       // Ray pierces surface
                    // If obstruction is a window and its base surface is the nearest obstruction hit so far set nearestHitSurface to this window
                    // Note that in this case NearestHitDistance_sq has already been calculated, so does not have to be recalculated
                    if ((surface.Class == SurfaceClass::Window) && (surface.BaseSurf > 0) &&
                        (&state.dataSurface->Surface(surface.BaseSurf) == nearestHitSurface)) {
                        nearestHitSurface = &surface;
                    } else {
                        // Distance squared from receiving point to hit point
                        Real64 const HitDistance_sq(distance_squared(HitPt, RecPt));
                        // Reset nearestHitSurface and NearestHitDistance_sq if this hit point is closer than previous closest
                        if (HitDistance_sq < NearestHitDistance_sq) {
                            NearestHitDistance_sq = HitDistance_sq;
                            nearestHitSurface = &surface;
                            NearestHitPt = HitPt;
                        }
                    }
                } // End of check if obstruction was hit
            }
        };

        // Process octree surface candidates
        Vector3<Real64> const RayVec_inv(SurfaceOctreeCube::safe_inverse(RayVec));
        state.dataHeatBalMgr->surfaceOctree.processSurfaceRayIntersectsCube(RecPt, RayVec, RayVec_inv, surfaceHit);
        if (nearestHitSurface != nullptr) { // Find surface number: This is inefficient: Improve when surfaces know their own number
            for (int i = 1; i <= state.dataSurface->TotSurfaces; ++i) {
                if (&state.dataSurface->Surface(i) == nearestHitSurface) {
                    NearestHitSurfNum = i;
                    break;
                }
            }
            assert(NearestHitSurfNum != 0);
        }
    }
}

void DayltgSurfaceLumFromSun(EnergyPlusData &state,
                             int const IHR,                    // Hour number
                             Vector3<Real64> const &Ray,       // Ray from window to reflecting surface (m)
                             int const ReflSurfNum,            // Number of surface for which luminance is being calculated
                             Vector3<Real64> const &ReflHitPt, // Point on ReflSurfNum for luminance calculation (m)
                             Real64 &LumAtReflHitPtFrSun       // Luminance at ReflHitPt from beam solar reflection for unit
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   November 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates exterior surface luminance due to beam solar diffuse reflection.

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    //  beam normal illuminance (cd/m2)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    auto &DayltgSurfaceLumFromSunReflNorm = state.dataDaylightingManager->DayltgSurfaceLumFromSunReflNorm; // Unit normal to reflecting surface (m)
    auto &DayltgSurfaceLumFromSunObsHitPt = state.dataDaylightingManager->DayltgSurfaceLumFromSunObsHitPt; // Hit point on obstruction (m)
    int ObsSurfNum;                                                                                        // Obstruction surface number
    bool hitObs;                                                                                           // True iff obstruction is hit
    Real64 CosIncAngAtHitPt; // Cosine of angle of incidence of sun at HitPt
    Real64 DiffVisRefl;      // Diffuse visible reflectance of ReflSurfNum

    LumAtReflHitPtFrSun = 0.0;
    // Skip daylighting shelves since reflection from these is separately calculated
    if (state.dataSurface->SurfDaylightingShelfInd(ReflSurfNum) > 0) return;
    // Normal to reflecting surface in hemisphere containing window element
    DayltgSurfaceLumFromSunReflNorm = state.dataSurface->Surface(ReflSurfNum).OutNormVec;
    if (state.dataSurface->Surface(ReflSurfNum).IsShadowing) {
        if (dot(DayltgSurfaceLumFromSunReflNorm, Ray) > 0.0) {
            DayltgSurfaceLumFromSunReflNorm *= -1.0;
        }
    }
    // Cosine of angle of incidence of sun at HitPt if sun were to reach HitPt
    Vector3<Real64> const SUNCOS_IHR(state.dataSurface->SurfSunCosHourly(IHR));
    CosIncAngAtHitPt = dot(DayltgSurfaceLumFromSunReflNorm, SUNCOS_IHR);
    // Require that the sun be in front of this surface relative to window element
    if (CosIncAngAtHitPt <= 0.0) return; // Sun is in back of reflecting surface
    // Sun reaches ReflHitPt if vector from ReflHitPt to sun is unobstructed
    hitObs = false;
    for (ObsSurfNum = 1; ObsSurfNum <= state.dataSurface->TotSurfaces; ++ObsSurfNum) {
        if (!state.dataSurface->Surface(ObsSurfNum).IsShadowPossibleObstruction) continue;
        // Exclude as a possible obstructor ReflSurfNum and its base surface (if it has one)
        if (ObsSurfNum == ReflSurfNum || ObsSurfNum == state.dataSurface->Surface(ReflSurfNum).BaseSurf) continue;
        PierceSurface(state, ObsSurfNum, ReflHitPt, SUNCOS_IHR, DayltgSurfaceLumFromSunObsHitPt, hitObs);
        if (hitObs) break;
    }
    if (hitObs) return; // Obstruction was hit, blocking s auto surfaceHit = [&state, &GroundHitPtun
    // Obstruction was not hit; sun reaches ReflHitPt.
    // Calculate luminance at ReflHitPt due to beam solar reflection (for unit beam normal illuminance)
    if (state.dataSurface->Surface(ReflSurfNum).IsShadowing) {
        DiffVisRefl = state.dataSurface->SurfShadowDiffuseVisRefl(ReflSurfNum);
        // Note that if the shadowing surface has a non-zero glazing fraction (e.g., neighboring bldg) that the above is
        // (1 - glazing fraction) * (vis refl of opaque part of shadowing surface); specular reflection is
        // excluded in this value of DiffVisRefl.
    } else { // Exterior building surface
        if (!state.dataConstruction->Construct(state.dataSurface->Surface(ReflSurfNum).Construction).TypeIsWindow) {
            DiffVisRefl = 1.0 - state.dataConstruction->Construct(state.dataSurface->Surface(ReflSurfNum).Construction).OutsideAbsorpSolar;
        } else {
            // Window; assume bare so no beam-to-diffuse reflection
            DiffVisRefl = 0.0;
        }
    }
    LumAtReflHitPtFrSun = CosIncAngAtHitPt * DiffVisRefl / DataGlobalConstants::Pi;
}

void DayltgInteriorMapIllum(EnergyPlusData &state)
{

    // *****super modified version of DayltgInteriorIllum by Peter Graham Ellis
    // *****removes all control code, just calculates illum with previously determined control settings
    // *****this should be packaged into a subroutine called from 2 places

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   July 1997
    //       MODIFIED       March 2000, FW: interpolate clear-sky daylight factors using
    //                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
    //                      only HourOfDay was used
    //                      Jan 2001, FW: interpolate in slat angle for windows with blinds
    //                      that have movable slats
    //                      Dec 2003, FW: fix bug--even though between-glass shade/blind is on
    //                        daylight illum at ref pt was calculated as though it was off
    //                      June 2009, TH: modified for thermochromic windows
    //                      March 2010, TH: fix bug (CR 8057) for electrochromic windows
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Using daylighting factors and exterior illuminance, determine
    // the current-hour interior daylight illuminance and glare index
    // at each reference point in a space.

    // Called by InitSurfaceHeatBalance.

    // REFERENCES:
    // Based on DOE-2.1E subroutine DINTIL.

    // Using/Aliasing
    using General::POLYF;

    // Locals
    auto &daylight_illum = state.dataDaylightingManager->daylight_illum;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NREFPT; // Number of daylighting map reference points
    // INTEGER   :: REFPT1                ! 1st reference point
    int ISky;  // Sky type index
    int ISky1; // Sky type index values for averaging two sky types
    int ISky2;
    auto &DFSUHR = state.dataDaylightingManager->DFSUHR; // Sun daylight factor for bare/shaded window
    auto &IConstShaded = state.dataDaylightingManager->IConstShaded;
    auto &VTDark = state.dataDaylightingManager->VTDark;
    auto &VTMULT = state.dataDaylightingManager->VTMULT;
    auto &DayltgInteriorMapIllumDFSUHR = state.dataDaylightingManager->DayltgInteriorMapIllumDFSUHR;
    auto &DayltgInteriorMapIllumHorIllSky = state.dataDaylightingManager->DayltgInteriorMapIllumHorIllSky;
    auto &DFSKHR = state.dataDaylightingManager->DayltgInteriorMapIllumDFSKHR;
    int IL;              // Reference point index
    int IWin;            // Window index
    int IS;              // IS=1 for unshaded window, =2 for shaded window
    int ICtrl;           // Window shading control pointer
    Real64 SkyWeight;    // Weighting factor used to average two different sky types
    Real64 HorIllSkyFac; // Ratio between horizontal illuminance from sky horizontal irradiance and
    //   luminous efficacy and horizontal illuminance from averaged sky
    int loop; // Window loop index
    int ILB;
    int IConst;
    Real64 VTRatio;
    Real64 VTNow;
    Real64 VTMaster;

    if (state.dataDaylightingManager->DayltgInteriorMapIllum_FirstTimeFlag) {
        daylight_illum.allocate(DataDaylighting::MaxMapRefPoints);
        state.dataDaylightingManager->DayltgInteriorMapIllum_FirstTimeFlag = false;
    }

    if (state.dataGlobal->WarmupFlag) return;

    //              Initialize reference point illuminance and window background luminance

    for (int mapNum = 1; mapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++mapNum) {
        auto &thisMap = state.dataDaylightingData->IllumMapCalc(mapNum);
        int enclNum = thisMap.enclIndex;
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);

        NREFPT = thisMap.TotalMapRefPoints;

        daylight_illum = 0.0;

        if (state.dataEnvrn->SkyClearness > 3.0) { // Sky is average of clear and clear turbid
            SkyWeight = min(1.0, (state.dataEnvrn->SkyClearness - 3.0) / 3.0);
            ISky1 = 1;
            ISky2 = 2;
        } else if (state.dataEnvrn->SkyClearness > 1.2) { // Sky is average of clear turbid and intermediate
            SkyWeight = (state.dataEnvrn->SkyClearness - 1.2) / 1.8;
            ISky1 = 2;
            ISky2 = 3;
        } else { // Sky is average of intermediate and overcast
            SkyWeight = min(1.0, max(0.0, (state.dataEnvrn->SkyClearness - 1.0) / 0.2, (state.dataEnvrn->SkyBrightness - 0.05) / 0.4));
            ISky1 = 3;
            ISky2 = 4;
        }

        //              First loop over windows in this space.
        //              Find contribution of each window to the daylight illum
        //              and to the glare numerator at each reference point.
        //              Use shading flags set in WindowShadingManager.

        for (loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
            IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);

            // Added TH 6/29/2009 for thermochromic windows
            VTRatio = 1.0;
            if (NREFPT > 0) {
                IConst = state.dataSurface->Surface(IWin).Construction;
                if (state.dataConstruction->Construct(IConst).TCFlag == 1) {
                    // For thermochromic windows, daylight and glare factors are always calculated
                    //  based on the master construction. They need to be adjusted by the VTRatio, including:
                    //  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
                    //  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
                    VTNow = POLYF(1.0, state.dataConstruction->Construct(IConst).TransVisBeamCoef);
                    VTMaster =
                        POLYF(1.0, state.dataConstruction->Construct(state.dataConstruction->Construct(IConst).TCMasterConst).TransVisBeamCoef);
                    VTRatio = VTNow / VTMaster;
                }
            }

            //              Loop over reference points
            for (ILB = 1; ILB <= NREFPT; ++ILB) {

                //          Daylight factors for current sun position
                for (ISky = 1; ISky <= 4; ++ISky) {
                    //                                ===Bare window===
                    DFSKHR(1, ISky) =
                        VTRatio * (state.dataGlobal->WeightNow * thisMap.DaylIllFacSky(state.dataGlobal->HourOfDay, 1, ISky, ILB, loop) +
                                   state.dataGlobal->WeightPreviousHour * thisMap.DaylIllFacSky(state.dataGlobal->PreviousHour, 1, ISky, ILB, loop));

                    if (ISky == 1) {
                        DayltgInteriorMapIllumDFSUHR(1) =
                            VTRatio *
                            (state.dataGlobal->WeightNow * (thisMap.DaylIllFacSun(state.dataGlobal->HourOfDay, 1, ILB, loop) +
                                                            thisMap.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, 1, ILB, loop)) +
                             state.dataGlobal->WeightPreviousHour * (thisMap.DaylIllFacSun(state.dataGlobal->PreviousHour, 1, ILB, loop) +
                                                                     thisMap.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, 1, ILB, loop)));
                    }

                    if ((state.dataSurface->SurfWinWindowModelType(IWin) != WindowBSDFModel) &&
                        (IS_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) || state.dataSurface->SurfWinSolarDiffusing(IWin))) {

                        //                                 ===Shaded window===
                        if (!state.dataSurface->SurfWinMovableSlats(IWin)) {
                            // Shade, screen, blind with fixed slats, or diffusing glass
                            DFSKHR(2, ISky) =
                                VTRatio *
                                (state.dataGlobal->WeightNow * thisMap.DaylIllFacSky(state.dataGlobal->HourOfDay, 2, ISky, ILB, loop) +
                                 state.dataGlobal->WeightPreviousHour * thisMap.DaylIllFacSky(state.dataGlobal->PreviousHour, 2, ISky, ILB, loop));

                            if (ISky == 1) {
                                DayltgInteriorMapIllumDFSUHR(2) =
                                    VTRatio *
                                    (state.dataGlobal->WeightNow * thisMap.DaylIllFacSun(state.dataGlobal->HourOfDay, 2, ILB, loop) +
                                     state.dataGlobal->WeightPreviousHour * thisMap.DaylIllFacSun(state.dataGlobal->PreviousHour, 2, ILB, loop));

                                if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin)) {
                                    DayltgInteriorMapIllumDFSUHR(2) +=
                                        VTRatio *
                                        (state.dataGlobal->WeightNow * thisMap.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, 2, ILB, loop) +
                                         state.dataGlobal->WeightPreviousHour *
                                             thisMap.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, 2, ILB, loop));
                                }
                            }
                        } else { // Blind with movable slats
                            int SurfWinSlatsAngIndex = state.dataSurface->SurfWinSlatsAngIndex(IWin);
                            Real64 SurfWinSlatsAngInterpFac = state.dataSurface->SurfWinSlatsAngInterpFac(IWin);
                            Real64 DaylIllFacSkyNow = General::InterpGeneral(
                                thisMap.DaylIllFacSky(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ISky, ILB, loop),
                                thisMap.DaylIllFacSky(
                                    state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, ILB, loop),
                                SurfWinSlatsAngInterpFac);
                            Real64 DaylIllFacSkyPrev = General::InterpGeneral(
                                thisMap.DaylIllFacSky(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ISky, ILB, loop),
                                thisMap.DaylIllFacSky(
                                    state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ISky, ILB, loop),
                                SurfWinSlatsAngInterpFac);

                            DFSKHR(2, ISky) =
                                VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSkyNow + state.dataGlobal->WeightPreviousHour * DaylIllFacSkyPrev);

                            if (ISky == 1) {
                                Real64 DaylIllFacSunNow = General::InterpGeneral(
                                    thisMap.DaylIllFacSun(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ILB, loop),
                                    thisMap.DaylIllFacSun(
                                        state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ILB, loop),
                                    SurfWinSlatsAngInterpFac);
                                Real64 DaylIllFacSunPrev = General::InterpGeneral(
                                    thisMap.DaylIllFacSun(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ILB, loop),
                                    thisMap.DaylIllFacSun(
                                        state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ILB, loop),
                                    SurfWinSlatsAngInterpFac);
                                DFSUHR(2) = VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSunNow +
                                                       state.dataGlobal->WeightPreviousHour * DaylIllFacSunPrev);

                                // We add the contribution from the solar disk if slats do not block beam solar
                                // TH CR 8010, DaylIllFacSunDisk needs to be interpolated
                                if (!state.dataSurface->SurfWinSlatsBlockBeam(IWin)) {
                                    Real64 DaylIllFacSunDiskNow = General::InterpGeneral(
                                        thisMap.DaylIllFacSunDisk(state.dataGlobal->HourOfDay, SurfWinSlatsAngIndex + 1, ILB, loop),
                                        thisMap.DaylIllFacSunDisk(
                                            state.dataGlobal->HourOfDay, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ILB, loop),
                                        SurfWinSlatsAngInterpFac);
                                    Real64 DaylIllFacSunDiskPrev = General::InterpGeneral(
                                        thisMap.DaylIllFacSunDisk(state.dataGlobal->PreviousHour, SurfWinSlatsAngIndex + 1, ILB, loop),
                                        thisMap.DaylIllFacSunDisk(
                                            state.dataGlobal->PreviousHour, std::min(MaxSlatAngs + 1, SurfWinSlatsAngIndex + 2), ILB, loop),
                                        SurfWinSlatsAngInterpFac);
                                    DFSUHR(2) += VTRatio * (state.dataGlobal->WeightNow * DaylIllFacSunDiskNow +
                                                            state.dataGlobal->WeightPreviousHour * DaylIllFacSunDiskPrev);
                                }
                            }

                        } // End of check if window has blind with movable slats

                    } // End of check if window is shaded or has diffusing glass
                }

                //              Get illuminance at ref point from bare and shaded window by
                //              multiplying daylight factors by exterior horizontal illuminance

                // Adding 0.001 in the following prevents zero DayltgInteriorMapIllumHorIllSky in early morning or late evening when sun
                // is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
                for (ISky = 1; ISky <= 4; ++ISky) {
                    DayltgInteriorMapIllumHorIllSky(ISky) =
                        state.dataGlobal->WeightNow * state.dataDaylightingManager->GILSK(state.dataGlobal->HourOfDay, ISky) +
                        state.dataGlobal->WeightPreviousHour * state.dataDaylightingManager->GILSK(state.dataGlobal->PreviousHour, ISky) + 0.001;
                }

                // HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
                // which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
                // also calculated in DayltgLuminousEfficacy.
                HorIllSkyFac = state.dataEnvrn->HISKF /
                               ((1.0 - SkyWeight) * DayltgInteriorMapIllumHorIllSky(ISky2) + SkyWeight * DayltgInteriorMapIllumHorIllSky(ISky1));

                for (IS = 1; IS <= 2; ++IS) {
                    if (IS == 2 && state.dataSurface->SurfWinWindowModelType(IWin) == WindowBSDFModel) break;
                    if (IS == 2 && NOT_SHADED(state.dataSurface->SurfWinShadingFlag(IWin)) && !state.dataSurface->SurfWinSolarDiffusing(IWin)) break;

                    thisMap.IllumFromWinAtMapPt(loop, IS, ILB) =
                        DayltgInteriorMapIllumDFSUHR(IS) * state.dataEnvrn->HISUNF +
                        HorIllSkyFac * (DFSKHR(IS, ISky1) * SkyWeight * DayltgInteriorMapIllumHorIllSky(ISky1) +
                                        DFSKHR(IS, ISky2) * (1.0 - SkyWeight) * DayltgInteriorMapIllumHorIllSky(ISky2));
                }

            } // End of reference point loop
        }     // End of first loop over windows

        //              Second loop over windows. Find total daylight illuminance
        //              and background luminance for each ref pt from all windows in
        //              the space.  Use shading flags.

        for (loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
            IWin = thisEnclDaylight.DayltgExtWinSurfNums(loop);

            IS = findWinShadingStatus(state, IWin);

            // CR 8057. 3/17/2010.
            // Switchable windows may be in partially switched state rather than fully dark state
            VTMULT = 1.0;

            ICtrl = state.dataSurface->Surface(IWin).activeWindowShadingControl;
            if (state.dataSurface->Surface(IWin).HasShadeControl) {
                if (state.dataSurface->WindowShadingControl(ICtrl).ShadingControlType == WindowShadingControlType::MeetDaylIlumSetp &&
                    state.dataSurface->SurfWinShadingFlag(IWin) == WinShadingType::SwitchableGlazing) {
                    // switchable windows in partial or fully switched state,
                    //  get its intermediate VT calculated in DayltgInteriorIllum
                    IConstShaded = state.dataSurface->Surface(IWin).activeShadedConstruction;
                    if (IConstShaded > 0)
                        VTDark =
                            POLYF(1.0, state.dataConstruction->Construct(IConstShaded).TransVisBeamCoef) * state.dataSurface->SurfWinGlazedFrac(IWin);
                    if (VTDark > 0) VTMULT = state.dataSurface->SurfWinVisTransSelected(IWin) / VTDark;
                }
            }

            for (IL = 1; IL <= NREFPT; ++IL) {
                //              Determine if illuminance contribution is from bare or shaded window
                daylight_illum(IL) += VTMULT * thisMap.IllumFromWinAtMapPt(loop, IS, IL);
            }
        } // End of second window loop

        //              Variables for reporting
        for (IL = 1; IL <= NREFPT; ++IL) {
            thisMap.DaylIllumAtMapPt(IL) = max(daylight_illum(IL), 0.0);
        }
    } // End loop over maps
}

void ReportIllumMap(EnergyPlusData &state, int const MapNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Ellis
    //       DATE WRITTEN   May 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine produces the Daylighting Illuminance Map output.  Each separate map (by zone)
    // is placed on a temporary file and later (see CloseReportIllumMaps) coallesced into a single
    // output file.

    // Using/Aliasing
    using DataStringGlobals::CharComma;
    using DataStringGlobals::CharSpace;
    using DataStringGlobals::CharTab;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string String;
    int RefPt;
    int X;
    int Y;
    int R;
    int IllumOut;

    auto &FirstTimeMaps = state.dataDaylightingManager->FirstTimeMaps;
    auto &EnvrnPrint = state.dataDaylightingManager->EnvrnPrint;
    auto &SavedMnDy = state.dataDaylightingManager->SavedMnDy;
    auto &XValue = state.dataDaylightingManager->XValue;
    auto &YValue = state.dataDaylightingManager->YValue;
    auto &IllumValue = state.dataDaylightingManager->IllumValue;
    std::string MapNoString;
    int linelen;
    // BSLLC Start
    int SQYear;
    int SQMonth;
    int SQDayOfMonth;
    int IllumIndex;
    //        static bool CommaDelimited( true ); //Unused Set but never used
    // BSLLC Finish

    if (state.dataDaylightingManager->ReportIllumMap_firstTime) {
        state.dataDaylightingManager->ReportIllumMap_firstTime = false;
        FirstTimeMaps.dimension((int)state.dataDaylightingData->IllumMap.size(), true);
        EnvrnPrint.dimension((int)state.dataDaylightingData->IllumMap.size(), true);
        SavedMnDy.allocate((int)state.dataDaylightingData->IllumMap.size());
    }

    if (FirstTimeMaps(MapNum)) {

        FirstTimeMaps(MapNum) = false;

        auto openMapFile = [&](const fs::path &filePath) -> InputOutputFile & {
            auto &outputFile = *state.dataDaylightingData->IllumMap(MapNum).mapFile;
            outputFile.filePath = fs::path(filePath.string() + fmt::to_string(MapNum));
            outputFile.ensure_open(state, "ReportIllumMap");
            return outputFile;
        };
        if (state.dataDaylightingData->MapColSep == CharTab) {
            if (!openMapFile(state.files.outputMapTabFilePath).good()) return;
            //                CommaDelimited = false; //Unused Set but never used
        } else if (state.dataDaylightingData->MapColSep == CharComma) {
            if (!openMapFile(state.files.outputMapCsvFilePath).good()) return;
            //                CommaDelimited = true; //Unused Set but never used
        } else {
            if (!openMapFile(state.files.outputMapTxtFilePath).good()) return;
            //                CommaDelimited = false; //Unused Set but never used
        }

        SavedMnDy(MapNum) = state.dataEnvrn->CurMnDyHr.substr(0, 5);

        state.dataDaylightingData->IllumMap(MapNum).Name =
            format("{} at {:.2R}m", state.dataDaylightingData->IllumMap(MapNum).Name, state.dataDaylightingData->IllumMap(MapNum).Z);
    }
    if (SavedMnDy(MapNum) != state.dataEnvrn->CurMnDyHr.substr(0, 5)) {
        EnvrnPrint(MapNum) = true;
        SavedMnDy(MapNum) = state.dataEnvrn->CurMnDyHr.substr(0, 5);
    }

    state.dataDaylightingData->IllumMap(MapNum).pointsHeader = "";
    int rCount = 0;
    for (int daylightCtrlNum = 1; daylightCtrlNum <= (int)state.dataDaylightingData->daylightControl.size(); ++daylightCtrlNum) {
        if (state.dataDaylightingData->daylightControl(daylightCtrlNum).zoneIndex == state.dataDaylightingData->IllumMap(MapNum).zoneIndex) {
            auto &thisDaylightControl = state.dataDaylightingData->daylightControl(daylightCtrlNum);

            for (R = 1; R <= thisDaylightControl.TotalDaylRefPoints; ++R) {
                ++rCount;
                state.dataDaylightingData->IllumMap(MapNum).pointsHeader += format(" RefPt{}=({:.2R}:{:.2R}:{:.2R}),",
                                                                                   rCount,
                                                                                   thisDaylightControl.DaylRefPtAbsCoord(1, R),
                                                                                   thisDaylightControl.DaylRefPtAbsCoord(2, R),
                                                                                   thisDaylightControl.DaylRefPtAbsCoord(3, R));
            }
        }
    }

    if (rCount > 0) {
        // Remove trailing comma
        state.dataDaylightingData->IllumMap(MapNum).pointsHeader.pop_back();
    }
    if (EnvrnPrint(MapNum)) {
        WriteDaylightMapTitle(state,
                              MapNum,
                              *state.dataDaylightingData->IllumMap(MapNum).mapFile,
                              state.dataDaylightingData->IllumMap(MapNum).Name,
                              state.dataEnvrn->EnvironmentName,
                              state.dataDaylightingData->IllumMap(MapNum).zoneIndex,
                              state.dataDaylightingData->IllumMap(MapNum).pointsHeader,
                              state.dataDaylightingData->IllumMap(MapNum).Z);
        EnvrnPrint(MapNum) = false;
    }

    if (!state.dataGlobal->WarmupFlag) {
        if (state.dataGlobal->TimeStep == state.dataGlobal->NumOfTimeStepInHour) { // Report only hourly

            // Write X scale column header
            auto mapLine = format(" {} {:02}:00", SavedMnDy(MapNum), state.dataGlobal->HourOfDay);
            if (state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLengthNeeded) linelen = int(len(mapLine));
            RefPt = 1;
            for (X = 1; X <= state.dataDaylightingData->IllumMap(MapNum).Xnum; ++X) {
                const auto AddXorYString = format("{}({:.2R};{:.2R})=",
                                                  state.dataDaylightingData->MapColSep,
                                                  state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt),
                                                  state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt));
                if (state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLengthNeeded) linelen += int(len(AddXorYString));
                mapLine += AddXorYString;
                ++RefPt;
            } // X

            if (state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLengthNeeded) {
                state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLength = linelen;
                if (static_cast<std::string::size_type>(state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLength) > len(mapLine)) {
                    ShowWarningError(state,
                                     format("ReportIllumMap: Map=\"{}\" -- the X Header overflows buffer -- will be truncated at {} characters.",
                                            state.dataDaylightingData->IllumMap(MapNum).Name,
                                            int(len(mapLine))));
                    ShowContinueError(state,
                                      format("...needed {} characters. Please contact EnergyPlus support.",
                                             state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLength));
                }
                state.dataDaylightingData->IllumMap(MapNum).HeaderXLineLengthNeeded = false;
            }

            print(*state.dataDaylightingData->IllumMap(MapNum).mapFile, "{}\n", mapLine);

            // Write Y scale prefix and illuminance values
            RefPt = 1;
            for (Y = 1; Y <= state.dataDaylightingData->IllumMap(MapNum).Ynum; ++Y) {
                mapLine = format("({:.2R};{:.2R})=",
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(1, RefPt),
                                 state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtAbsCoord(2, RefPt));
                for (R = RefPt; R <= RefPt + state.dataDaylightingData->IllumMap(MapNum).Xnum - 1; ++R) {
                    IllumOut = nint(state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPtHr(R));
                    if (state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(R)) {
                        String = fmt::to_string(IllumOut);
                    } else {
                        String = fmt::to_string(IllumOut);
                        String = "*" + String;
                    }
                    mapLine += state.dataDaylightingData->MapColSep + String;
                }

                print(*state.dataDaylightingData->IllumMap(MapNum).mapFile, "{}\n", mapLine);

                RefPt += state.dataDaylightingData->IllumMap(MapNum).Xnum;
            } // X

            if (state.dataSQLiteProcedures->sqlite) {
                if (state.dataDaylightingManager->SQFirstTime) {
                    int const nX(maxval(state.dataDaylightingData->IllumMap, &DataDaylighting::IllumMapData::Xnum));
                    int const nY(maxval(state.dataDaylightingData->IllumMap, &DataDaylighting::IllumMapData::Ynum));
                    XValue.allocate(nX);
                    YValue.allocate(nY);
                    IllumValue.allocate(nX, nY);
                    state.dataDaylightingManager->SQFirstTime = false;
                }

                // We need DataGlobals::CalendarYear, and not DataEnvironment::Year because
                // otherwise if you run a TMY file, you'll get for eg 1977, 1981, etc
                SQYear = state.dataGlobal->CalendarYear;
                SQMonth = state.dataEnvrn->Month;
                SQDayOfMonth = state.dataEnvrn->DayOfMonth;

                for (Y = 1; Y <= state.dataDaylightingData->IllumMap(MapNum).Ynum; ++Y) {
                    YValue(Y) = state.dataDaylightingData->IllumMap(MapNum).Ymin + (Y - 1) * state.dataDaylightingData->IllumMap(MapNum).Yinc;
                    for (X = 1; X <= state.dataDaylightingData->IllumMap(MapNum).Xnum; ++X) {
                        XValue(X) = state.dataDaylightingData->IllumMap(MapNum).Xmin + (X - 1) * state.dataDaylightingData->IllumMap(MapNum).Xinc;
                        IllumIndex = X + (Y - 1) * state.dataDaylightingData->IllumMap(MapNum).Xnum;
                        IllumValue(X, Y) = nint(state.dataDaylightingData->IllumMapCalc(MapNum).DaylIllumAtMapPtHr(IllumIndex));
                        if (!state.dataDaylightingData->IllumMapCalc(MapNum).MapRefPtInBounds(IllumIndex)) {
                            IllumValue(X, Y) = -IllumValue(X, Y);
                        }
                    } // X Loop
                }     // Y Loop

                state.dataSQLiteProcedures->sqlite->createSQLiteDaylightMap(MapNum,
                                                                            SQYear,
                                                                            SQMonth,
                                                                            SQDayOfMonth,
                                                                            state.dataGlobal->HourOfDay,
                                                                            state.dataDaylightingData->IllumMap(MapNum).Xnum,
                                                                            XValue,
                                                                            state.dataDaylightingData->IllumMap(MapNum).Ynum,
                                                                            YValue,
                                                                            IllumValue);

            } // WriteOutputToSQLite
        }     // end time step
    }         // not Warmup
}

void CloseReportIllumMaps(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   June 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine "closes" out the created daylight illuminance maps by merging them
    // into the "eplusout.map" file.

    // Using/Aliasing
    using DataStringGlobals::CharComma;
    using DataStringGlobals::CharSpace;
    using DataStringGlobals::CharTab;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    if ((int)state.dataDaylightingData->IllumMap.size() > 0) {
        // Write map header
        if (state.dataDaylightingData->MapColSep == CharTab) {
            state.files.map.filePath = state.files.outputMapTabFilePath;
        } else if (state.dataDaylightingData->MapColSep == CharComma) {
            state.files.map.filePath = state.files.outputMapCsvFilePath;
        } else {
            state.files.map.filePath = state.files.outputMapTxtFilePath;
        }

        state.files.map.ensure_open(state, "CloseReportIllumMaps");

        for (int MapNum = 1; MapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++MapNum) {
            if (!state.dataDaylightingData->IllumMap(MapNum).mapFile->good()) continue; // fatal error processing

            const auto mapLines = state.dataDaylightingData->IllumMap(MapNum).mapFile->getLines();
            if (mapLines.empty()) {
                ShowSevereError(state, "CloseReportIllumMaps: IllumMap=\"" + state.dataDaylightingData->IllumMap(MapNum).Name + "\" is empty.");
                break;
            }
            for (const auto &mapLine : mapLines) {
                print(state.files.map, "{}\n", mapLine);
            }
            state.dataDaylightingData->IllumMap(MapNum).mapFile->del();
        }

        if (!state.dataDaylightingData->mapResultsReported && !state.dataErrTracking->AbortProcessing) {
            const auto message = "CloseReportIllumMaps: Illuminance maps requested but no data ever reported. Likely cause is no solar.";
            ShowSevereError(state, message);
            print(state.files.map, "{}\n", message);
        }
    }
}

void CloseDFSFile(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Make sure DFSFile is closed at exit time.  Do not rely on operating system to
    // take care of it.

    state.files.dfs.close();
}

void DayltgSetupAdjZoneListsAndPointers(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Feb. 2004
    //       MODIFIED:      June 2010;LKL - Merged two routines.

    // PURPOSE OF THIS SUBROUTINE:
    // For each Daylighting:Detailed enclosure, creates a list of adjacent enclosures,
    // that have one or more exterior windows and that share one or more interior
    // windows with Z. Used in calculation of daylighting through interior windows.

    // Sets the daylighting factor pointers for each Daylighting:Detailed control. The pointer
    // may be associated with an exterior window in a daylit target zone's enclosure or an exterior window in
    // an adjacent enclosure, daylit or not, that shares interior windows with the target zone's enclosure.

    // Count number of exterior Windows (use to allocate arrays)
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        thisEnclDaylight.TotalExtWindows = 0;

        // Count exterior windows in this solar enclosure
        for (int const surfNum : state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr) {
            if ((state.dataSurface->Surface(surfNum).Class == SurfaceClass::Window &&
                 state.dataSurface->Surface(surfNum).ExtBoundCond == ExternalEnvironment) ||
                state.dataSurface->SurfWinOriginalClass(surfNum) == SurfaceClass::TDD_Diffuser) {
                ++thisEnclDaylight.TotalExtWindows;
            }
        }
    } // End of primary enclosure loop

    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        int NumList = 0;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        // This is a Daylighting:Detailed enclosure
        // Find adjacent zones/enclosures
        for (int adjEnclNum = 1; adjEnclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++adjEnclNum) {
            if (adjEnclNum == enclNum) continue;
            // Require that adjEnclNum have a least one exterior window
            bool AdjEnclHasExtWins = false;
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment)) {
                    AdjEnclHasExtWins = true;
                    break;
                }
            }
            if (!AdjEnclHasExtWins) continue;
            // Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to ZoneNum
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond >= 1)) {
                    // This is an interior window in ZoneNumAdj
                    if (state.dataSurface->Surface(state.dataSurface->Surface(SurfNumAdj).ExtBoundCond).SolarEnclIndex == enclNum) {
                        // This interior window is adjacent to ZoneNum
                        ++NumList;
                        break;
                    }
                }
            }
        }
        thisEnclDaylight.AdjIntWinEnclNums.allocate(NumList);
        thisEnclDaylight.AdjIntWinEnclNums = 0;
    } // End of primary enclosure loop

    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        int NumList = 0;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        // This is a Daylighting:Detailed enclosure
        // Find adjacent zones/enclosures
        for (int adjEnclNum = 1; adjEnclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++adjEnclNum) {
            if (adjEnclNum == enclNum) continue;
            // Require that adjEnclNum have a least one exterior window
            bool AdjEnclHasExtWins = false;
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment)) {
                    AdjEnclHasExtWins = true;
                    break;
                }
            }
            if (!AdjEnclHasExtWins) continue;
            // Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to enclNum
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond >= 1)) {
                    // This is an interior window in adjEnclNum
                    if (state.dataSurface->Surface(state.dataSurface->Surface(SurfNumAdj).ExtBoundCond).SolarEnclIndex == enclNum) {
                        // This interior window is adjacent to ZoneNum
                        ++NumList;
                        int enclNumAdj = state.dataSurface->Surface(SurfNumAdj).SolarEnclIndex;
                        thisEnclDaylight.AdjIntWinEnclNums(NumList) = enclNumAdj;
                        state.dataDaylightingData->enclDaylight(enclNumAdj).adjEnclHasDayltgCtrl = true;
                        break;
                    }
                }
            }
        }
        thisEnclDaylight.NumOfIntWinAdjEncls = NumList;
    } // End of primary enclosure loop

    // now fill out information on relationship between adjacent exterior windows and associated interior windows
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        // first find count of exterior windows
        if (thisEnclDaylight.NumOfIntWinAdjEncls <= 0) {
            thisEnclDaylight.NumOfIntWinAdjEnclExtWins = 0;
            continue;
        }
        for (int adjEnclNum : thisEnclDaylight.AdjIntWinEnclNums) {
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment)) {
                    ++thisEnclDaylight.NumOfIntWinAdjEnclExtWins;
                }
            }
        }
        // now allocate nested struct based on exterior window count
        thisEnclDaylight.IntWinAdjEnclExtWin.allocate(thisEnclDaylight.NumOfIntWinAdjEnclExtWins);

        // now fill nested structure
        int ExtWinIndex = 0;
        for (int adjEnclNum : thisEnclDaylight.AdjIntWinEnclNums) {
            for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window) &&
                    (state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment)) {
                    ++ExtWinIndex;
                    thisEnclDaylight.IntWinAdjEnclExtWin(ExtWinIndex).SurfNum = SurfNumAdj;

                    // now count interior windows shared by both zones
                    int NumOfIntWindowsCount = 0;
                    for (int SurfNumAdj2 : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                        if ((state.dataSurface->Surface(SurfNumAdj2).Class == SurfaceClass::Window) &&
                            (state.dataSurface->Surface(SurfNumAdj2).ExtBoundCond >= 1)) {
                            // This is an interior window in ZoneNumAdj
                            if (state.dataSurface->Surface(state.dataSurface->Surface(SurfNumAdj2).ExtBoundCond).SolarEnclIndex == enclNum) {
                                // This interior window is adjacent to ZoneNum and associated with this
                                ++NumOfIntWindowsCount;
                            }
                        }
                    }
                    // allocate nested array
                    thisEnclDaylight.IntWinAdjEnclExtWin(ExtWinIndex).IntWinNum.allocate(NumOfIntWindowsCount);
                    thisEnclDaylight.IntWinAdjEnclExtWin(ExtWinIndex).IntWinNum = 0;
                    int IntWinIndex = 0;
                    for (int SurfNumAdj2 : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                        if ((state.dataSurface->Surface(SurfNumAdj2).Class == SurfaceClass::Window) &&
                            (state.dataSurface->Surface(SurfNumAdj2).ExtBoundCond >= 1)) {
                            // This is an interior window in ZoneNumAdj
                            if (state.dataSurface->Surface(state.dataSurface->Surface(SurfNumAdj2).ExtBoundCond).SolarEnclIndex == enclNum) {
                                // This interior window is adjacent to ZoneNum and associated with this
                                ++IntWinIndex;
                                thisEnclDaylight.IntWinAdjEnclExtWin(ExtWinIndex).IntWinNum(IntWinIndex) = SurfNumAdj2;
                            }
                        }
                    }
                }
            }
        }
    } // End of primary enclosure loop

    Array1D_int enclExtWin;
    enclExtWin.dimension(state.dataViewFactor->NumOfSolarEnclosures, 0);

    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        enclExtWin(enclNum) = 0;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        // This is a Daylighting:Detailed zone

        // Get exterior windows in this solar enclosure
        for (int const surfNum : state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr) {
            if ((state.dataSurface->Surface(surfNum).Class == SurfaceClass::Window &&
                 state.dataSurface->Surface(surfNum).ExtBoundCond == ExternalEnvironment) ||
                state.dataSurface->SurfWinOriginalClass(surfNum) == SurfaceClass::TDD_Diffuser) {
                ++enclExtWin(enclNum);
            }
        }

        // Get exterior windows in adjacent enclosures that share interior windows with enclNum
        if (thisEnclDaylight.NumOfIntWinAdjEncls > 0) {
            for (int adjEnclNum : thisEnclDaylight.AdjIntWinEnclNums) {
                // Get exterior windows in EnclNumAdj -- there must be at least one, otherwise
                // it would not be an "AdjIntWinEncl"
                for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                    if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window &&
                         state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment) ||
                        state.dataSurface->SurfWinOriginalClass(SurfNumAdj) == SurfaceClass::TDD_Diffuser) {
                        ++enclExtWin(enclNum);
                    }
                }
            }
        }
    } // End of primary enclosure loop

    std::size_t maxShadeDeployOrderExtWinsSize(0);
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        thisEnclDaylight.NumOfDayltgExtWins = 0;
        int thisEnclNumRefPoints = state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints;
        if (thisEnclNumRefPoints > 0) {
            // This is a Daylighting:Detailed enclosure

            // Get exterior windows in this enclosure
            if (enclExtWin(enclNum) == 0) continue;
            thisEnclDaylight.DayltgExtWinSurfNums.allocate(enclExtWin(enclNum));
            thisEnclDaylight.DayltgExtWinSurfNums = 0;
            for (int controlNum : thisEnclDaylight.daylightControlIndexes) {
                auto &thisDaylightControl = state.dataDaylightingData->daylightControl(controlNum);
                thisDaylightControl.MapShdOrdToLoopNum.allocate(enclExtWin(enclNum));
                thisDaylightControl.MapShdOrdToLoopNum = 0;

                thisDaylightControl.SolidAngAtRefPt.allocate(enclExtWin(enclNum), thisDaylightControl.TotalDaylRefPoints);
                thisDaylightControl.SolidAngAtRefPt = 0.0;
                thisDaylightControl.SolidAngAtRefPtWtd.allocate(enclExtWin(enclNum), thisDaylightControl.TotalDaylRefPoints);
                thisDaylightControl.SolidAngAtRefPtWtd = 0.0;
                thisDaylightControl.IllumFromWinAtRefPt.allocate(enclExtWin(enclNum), 2, thisDaylightControl.TotalDaylRefPoints);
                thisDaylightControl.IllumFromWinAtRefPt = 0.0;
                thisDaylightControl.BackLumFromWinAtRefPt.allocate(enclExtWin(enclNum), 2, thisDaylightControl.TotalDaylRefPoints);
                thisDaylightControl.BackLumFromWinAtRefPt = 0.0;
                thisDaylightControl.SourceLumFromWinAtRefPt.allocate(enclExtWin(enclNum), 2, thisDaylightControl.TotalDaylRefPoints);
                thisDaylightControl.SourceLumFromWinAtRefPt = 0.0;
            }

            int enclExtWinCtr = 0;

            for (int const surfNum : state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr) {
                if ((state.dataSurface->Surface(surfNum).Class == SurfaceClass::Window &&
                     state.dataSurface->Surface(surfNum).ExtBoundCond == ExternalEnvironment) ||
                    state.dataSurface->SurfWinOriginalClass(surfNum) == SurfaceClass::TDD_Diffuser) {
                    ++enclExtWinCtr;
                    thisEnclDaylight.DayltgExtWinSurfNums(enclExtWinCtr) = surfNum;
                }
            }

            // Get exterior windows in adjacent enclosures that share interior windows with enclNum
            if (thisEnclDaylight.NumOfIntWinAdjEncls > 0) {
                for (int adjEnclNum : thisEnclDaylight.AdjIntWinEnclNums) {
                    // Get exterior windows in EnclNumAdj -- there must be at least one, otherwise
                    // it would not be an "AdjIntWinEncl"
                    for (int SurfNumAdj : state.dataViewFactor->EnclSolInfo(adjEnclNum).SurfacePtr) {
                        if ((state.dataSurface->Surface(SurfNumAdj).Class == SurfaceClass::Window &&
                             state.dataSurface->Surface(SurfNumAdj).ExtBoundCond == ExternalEnvironment) ||
                            state.dataSurface->SurfWinOriginalClass(SurfNumAdj) == SurfaceClass::TDD_Diffuser) {
                            ++enclExtWinCtr;
                            thisEnclDaylight.DayltgExtWinSurfNums(enclExtWinCtr) = SurfNumAdj;

                            // If no daylighting in the adjacent enclosure, set up variables anyway:
                            if (state.dataViewFactor->EnclSolInfo(adjEnclNum).TotalEnclosureDaylRefPoints == 0) {
                                if (!state.dataSurface->SurfWinSurfDayLightInit(SurfNumAdj)) {
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SolidAngAtRefPt.allocate(thisEnclNumRefPoints);
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SolidAngAtRefPt = 0.0;
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SolidAngAtRefPtWtd.allocate(thisEnclNumRefPoints);
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SolidAngAtRefPtWtd = 0.0;
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).IllumFromWinAtRefPt.allocate(2, thisEnclNumRefPoints);
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).IllumFromWinAtRefPt = 0.0;
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).BackLumFromWinAtRefPt.allocate(2, thisEnclNumRefPoints);
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).BackLumFromWinAtRefPt = 0.0;
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SourceLumFromWinAtRefPt.allocate(2, thisEnclNumRefPoints);
                                    state.dataSurface->SurfaceWindow(SurfNumAdj).SourceLumFromWinAtRefPt = 0.0;
                                    state.dataSurface->SurfWinSurfDayLightInit(SurfNumAdj) = true;
                                }
                            }
                        }
                    }
                }
            }

            thisEnclDaylight.NumOfDayltgExtWins = enclExtWin(enclNum);
            int winSize = enclExtWin(enclNum);

            for (int controlNum : thisEnclDaylight.daylightControlIndexes) {
                auto &thisDaylightControl = state.dataDaylightingData->daylightControl(controlNum);
                int refSize = thisDaylightControl.TotalDaylRefPoints;
                thisDaylightControl.DaylIllFacSky.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, 4, refSize, winSize);
                thisDaylightControl.DaylSourceFacSky.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, 4, refSize, winSize);
                thisDaylightControl.DaylBackFacSky.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, 4, refSize, winSize);
                thisDaylightControl.DaylIllFacSun.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
                thisDaylightControl.DaylIllFacSunDisk.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
                thisDaylightControl.DaylSourceFacSun.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
                thisDaylightControl.DaylSourceFacSunDisk.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
                thisDaylightControl.DaylBackFacSun.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
                thisDaylightControl.DaylBackFacSunDisk.allocate(24, state.dataSurface->actualMaxSlatAngs + 1, refSize, winSize);
            }
        } // End of check if thisEnclNumRefPoints > 0

        if (state.dataSurface->TotWinShadingControl > 0) {
            std::size_t maxSize = CreateShadeDeploymentOrder(state, enclNum);
            if (maxSize > maxShadeDeployOrderExtWinsSize) maxShadeDeployOrderExtWinsSize = maxSize;
        }
    } // End of primary enclosure loop

    // size these for the maximum of the shade deployment order
    state.dataDaylightingManager->DILLSW.allocate(maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->DILLUN.allocate(maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->WDAYIL.allocate(2, state.dataDaylightingData->maxRefPointsPerControl, maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->WBACLU.allocate(2, state.dataDaylightingData->maxRefPointsPerControl, maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->RDAYIL.allocate(state.dataDaylightingData->maxRefPointsPerControl, maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->RBACLU.allocate(state.dataDaylightingData->maxRefPointsPerControl, maxShadeDeployOrderExtWinsSize);

    state.dataDaylightingManager->TVIS1.allocate(maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->TVIS2.allocate(maxShadeDeployOrderExtWinsSize);
    state.dataDaylightingManager->ASETIL.allocate(maxShadeDeployOrderExtWinsSize);

    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        int thisEnclNumRefPoints = state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints;
        if (thisEnclNumRefPoints > 0) {
            if (state.dataSurface->TotWinShadingControl > 0) {
                MapShadeDeploymentOrderToLoopNumber(state, enclNum);
            }
        }
    }

    for (int mapNum = 1; mapNum <= (int)state.dataDaylightingData->IllumMap.size(); ++mapNum) {
        int numExtWin = enclExtWin(state.dataDaylightingData->IllumMapCalc(mapNum).enclIndex);
        int numMapRefPts = state.dataDaylightingData->IllumMapCalc(mapNum).TotalMapRefPoints;

        if (numMapRefPts > 0) {
            state.dataDaylightingData->IllumMapCalc(mapNum).IllumFromWinAtMapPt.allocate(numExtWin, 2, numMapRefPts);
            state.dataDaylightingData->IllumMapCalc(mapNum).IllumFromWinAtMapPt = 0.0;

            state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSky.allocate(
                24, state.dataSurface->actualMaxSlatAngs + 1, 4, numMapRefPts, numExtWin);
            state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSun.allocate(
                24, state.dataSurface->actualMaxSlatAngs + 1, numMapRefPts, numExtWin);
            state.dataDaylightingData->IllumMapCalc(mapNum).DaylIllFacSunDisk.allocate(
                24, state.dataSurface->actualMaxSlatAngs + 1, numMapRefPts, numExtWin);
        }
    } // End of map loop

    state.dataDaylightingManager->EINTSK.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 4, 0.0);
    state.dataDaylightingManager->EINTSU.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 0.0);
    state.dataDaylightingManager->EINTSUdisk.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 0.0);
    state.dataDaylightingManager->WLUMSK.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 4, 0.0);
    state.dataDaylightingManager->WLUMSU.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 0.0);
    state.dataDaylightingManager->WLUMSUdisk.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 0.0);
    state.dataDaylightingManager->EDIRSK.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 4);
    state.dataDaylightingManager->EDIRSU.dimension(24, state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->EDIRSUdisk.dimension(24, state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->AVWLSK.dimension(24, state.dataSurface->actualMaxSlatAngs + 1, 4);
    state.dataDaylightingManager->AVWLSU.dimension(24, state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->AVWLSUdisk.dimension(24, state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->FLFWSU.dimension(state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->FLFWSUdisk.dimension(state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->FLCWSU.dimension(state.dataSurface->actualMaxSlatAngs + 1);
    state.dataDaylightingManager->TransMult.dimension(state.dataSurface->actualMaxSlatAngs);
    state.dataDaylightingManager->DayltgInterReflectedIllumTransBmBmMult.dimension(state.dataSurface->actualMaxSlatAngs);
    state.dataDaylightingManager->TransBmBmMult.dimension(state.dataSurface->actualMaxSlatAngs);
    state.dataDaylightingManager->TransBmBmMultRefl.dimension(state.dataSurface->actualMaxSlatAngs);
    state.dataDaylightingManager->FLCWSK.dimension(state.dataSurface->actualMaxSlatAngs + 1, 4);
    state.dataDaylightingManager->FLFWSK.dimension(state.dataSurface->actualMaxSlatAngs + 1, 4);

    static constexpr std::string_view Format_700("! <Enclosure/Window Adjacency Daylighting Counts>, Enclosure Name, Number of Exterior Windows, "
                                                 "Number of Exterior Windows in Adjacent Enclosures\n");
    print(state.files.eio, Format_700);
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        static constexpr std::string_view Format_701("Enclosure/Window Adjacency Daylighting Counts, {},{},{}\n");
        print(state.files.eio,
              Format_701,
              state.dataViewFactor->EnclSolInfo(enclNum).Name,
              thisEnclDaylight.TotalExtWindows,
              (thisEnclDaylight.NumOfDayltgExtWins - thisEnclDaylight.TotalExtWindows));
    }
    static constexpr std::string_view Format_702(
        "! <Enclosure/Window Adjacency Daylighting Matrix>, Enclosure Name, Number of Adjacent Enclosures with Windows,Adjacent "
        "Enclosure Names - 1st 100 (max)\n");
    print(state.files.eio, Format_702);
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        if (!thisEnclDaylight.hasSplitFluxDaylighting) continue;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        static constexpr std::string_view Format_703("Enclosure/Window Adjacency Daylighting Matrix, {},{}");
        print(state.files.eio, Format_703, state.dataViewFactor->EnclSolInfo(enclNum).Name, thisEnclDaylight.NumOfIntWinAdjEncls);
        for (int loop = 1, loop_end = min(thisEnclDaylight.NumOfIntWinAdjEncls, 100); loop <= loop_end; ++loop) {
            print(state.files.eio, ",{}", state.dataViewFactor->EnclSolInfo(thisEnclDaylight.AdjIntWinEnclNums(loop)).Name);
        }
        print(state.files.eio, "\n");
    }

    enclExtWin.deallocate();
}

std::size_t CreateShadeDeploymentOrder(EnergyPlusData &state, int const enclNum)
{
    // J. Glazer - 2018
    // create sorted list for shade deployment order
    // first step is to create a sortable list of WindowShadingControl objects by sequence
    std::vector<std::pair<int, int>> shadeControlSequence; // sequence, WindowShadingControl
    for (int iShadeCtrl = 1; iShadeCtrl <= state.dataSurface->TotWinShadingControl; ++iShadeCtrl) {
        for (int spaceNum : state.dataHeatBal->Zone(state.dataSurface->WindowShadingControl(iShadeCtrl).ZoneIndex).spaceIndexes) {
            int shadeCtrlEnclNum = state.dataHeatBal->space(spaceNum).solarEnclosureNum;
            if (shadeCtrlEnclNum == enclNum) {
                shadeControlSequence.push_back(std::make_pair(state.dataSurface->WindowShadingControl(iShadeCtrl).SequenceNumber, iShadeCtrl));
                break;
            }
        }
    }
    // sort the WindowShadingControl objects based on sequence number
    sort(shadeControlSequence.begin(), shadeControlSequence.end());
    // now make the deployment list of lists.
    // each sublist is a group of surfaces that should be deployed together
    // often the sublist is just a single item.
    std::size_t maxShadeDeployOrderExtWinsSize = 0;
    for (int controlNum : state.dataDaylightingData->enclDaylight(enclNum).daylightControlIndexes) {
        auto &thisDaylightCtrl = state.dataDaylightingData->daylightControl(controlNum);
        for (auto sequence : shadeControlSequence) {
            int curShadeControl = sequence.second;
            if (state.dataSurface->WindowShadingControl(curShadeControl).MultiSurfaceCtrlIsGroup) {
                // add a group of surfaces since they should be deployed as a group
                std::vector<int> group;
                for (int i = 1; i <= state.dataSurface->WindowShadingControl(curShadeControl).FenestrationCount; i++) {
                    group.push_back(state.dataSurface->WindowShadingControl(curShadeControl).FenestrationIndex(i));
                }
                thisDaylightCtrl.ShadeDeployOrderExtWins.push_back(group);
            } else {
                // add each individial surface as a separate list so they are deployed individually
                for (int i = 1; i <= state.dataSurface->WindowShadingControl(curShadeControl).FenestrationCount; i++) {
                    std::vector<int> singleMemberVector;
                    singleMemberVector.push_back(state.dataSurface->WindowShadingControl(curShadeControl).FenestrationIndex(i));
                    thisDaylightCtrl.ShadeDeployOrderExtWins.push_back(singleMemberVector);
                }
            }
        }
        maxShadeDeployOrderExtWinsSize = max(maxShadeDeployOrderExtWinsSize, thisDaylightCtrl.ShadeDeployOrderExtWins.size());
    }
    int maxNumOfDayltgExtWins = 0;
    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        maxNumOfDayltgExtWins = max(maxNumOfDayltgExtWins, state.dataDaylightingData->enclDaylight(enclNum).NumOfDayltgExtWins);
    }
    state.dataDaylightingManager->previously_shaded.allocate(maxNumOfDayltgExtWins);

    return maxShadeDeployOrderExtWinsSize;
}

void MapShadeDeploymentOrderToLoopNumber(EnergyPlusData &state, int const enclNum)
{
    // J. Glazer - 2018
    // Allow a way to map back to the original "loop" index that is used in many other places in the
    // ZoneDayLight data structure when traversing the list in the order of the window shaded deployment

    auto const &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
    auto const &thisSolEnclosureName = state.dataViewFactor->EnclSolInfo(enclNum).Name;
    if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints > 0 && thisEnclDaylight.NumOfDayltgExtWins > 0) {
        for (int controlNum : state.dataDaylightingData->enclDaylight(enclNum).daylightControlIndexes) {
            auto &thisDaylightCtrl = state.dataDaylightingData->daylightControl(controlNum);
            if (thisDaylightCtrl.ShadeDeployOrderExtWins.size() > 0) {
                int count = 0;
                bool showOnce = true;
                for (auto listOfExtWin : thisDaylightCtrl.ShadeDeployOrderExtWins) {
                    for (auto IWinShdOrd : listOfExtWin) {
                        ++count;
                        if (count > thisEnclDaylight.NumOfDayltgExtWins) {
                            if (showOnce) {
                                ShowWarningError(state,
                                                 "MapShadeDeploymentOrderToLoopNumber: too many controlled shaded windows in enclosure " +
                                                     thisSolEnclosureName);
                                ShowContinueError(
                                    state, "Check the Zone Name in the WindowShadingControl that references the following fenestration surfaces:");
                                showOnce = false;
                            }
                            ShowContinueError(state, "  -  " + state.dataSurface->Surface(IWinShdOrd).Name);
                        }
                        bool found = false;
                        for (int loop = 1; loop <= thisEnclDaylight.NumOfDayltgExtWins; ++loop) {
                            int IWinLoop = thisEnclDaylight.DayltgExtWinSurfNums(loop);
                            if (IWinShdOrd == IWinLoop) {
                                thisDaylightCtrl.MapShdOrdToLoopNum(count) = loop;
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }
        } // controlNum loop
    }
}

void DayltgInterReflIllFrIntWins(EnergyPlusData &state, int const enclNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Mar. 2004

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the inter-reflected illuminance in a daylit zone from beam
    // and diffuse daylight entering the zone through interior windows. This illuminance
    // is determined by the split-flux method and is assumed to be uniform, i.e., the same
    // at all reference points.

    Real64 QDifTrans;              // Luminous flux transmitted through an int win from adjacent zone's enclosure (lumens)
    Real64 QDifTransUp;            // Upgoing part of QDifTrans (lumens)
    Real64 QDifTransDn;            // Downgoing part of QDifTrans (lumens)
    Real64 DifInterReflIllThisWin; // Inter-reflected illuminance due to QDifTrans (lux)
    Real64 BmInterReflIll;         // Inter-reflected illuminance due to beam solar entering ZoneNum's enclosure
                                   //  through its interior windows (lux)

    auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
    thisEnclDaylight.InterReflIllFrIntWins = 0.0;

    auto &thisEnclSurfaces(state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr);
    for (int const IWin : thisEnclSurfaces) {
        if (state.dataSurface->Surface(IWin).Class == SurfaceClass::Window && state.dataSurface->Surface(IWin).ExtBoundCond >= 1) {
            // This is an interior window in ZoneNum
            int const ConstrNum = state.dataSurface->Surface(IWin).Construction;
            int const adjEnclNum = state.dataSurface->Surface(state.dataSurface->Surface(IWin).ExtBoundCond).SolarEnclIndex;
            QDifTrans = state.dataHeatBal->EnclSolQSDifSol(adjEnclNum) * state.dataConstruction->Construct(ConstrNum).TransDiffVis *
                        state.dataSurface->Surface(IWin).Area * state.dataEnvrn->PDIFLW;
            QDifTransUp = QDifTrans * state.dataSurface->SurfWinFractionUpgoing(IWin);
            QDifTransDn = QDifTrans * (1.0 - state.dataSurface->SurfWinFractionUpgoing(IWin));
            if (state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea *
                    (1.0 - state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect) !=
                0.0) {
                DifInterReflIllThisWin =
                    (QDifTransDn * state.dataSurface->SurfWinRhoFloorWall(IWin) + QDifTransUp * state.dataSurface->SurfWinRhoCeilingWall(IWin)) /
                    (state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea *
                     (1.0 - state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect));
            } else {
                DifInterReflIllThisWin = 0.0;
            }
            thisEnclDaylight.InterReflIllFrIntWins += DifInterReflIllThisWin;
        }
    }

    // Add inter-reflected illuminance from beam solar entering enclosure through interior windows
    // TH, CR 7873, 9/17/2009
    BmInterReflIll = 0.0;
    if (state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea > 0) {
        BmInterReflIll = (state.dataHeatBal->EnclSolDBIntWin(enclNum) * state.dataEnvrn->BeamSolarRad * state.dataEnvrn->PDIRLW *
                          state.dataDaylightingData->enclDaylight(enclNum).floorVisRefl) /
                         (state.dataDaylightingData->enclDaylight(enclNum).totInsSurfArea *
                          (1.0 - state.dataDaylightingData->enclDaylight(enclNum).aveVisDiffReflect));
    }

    thisEnclDaylight.InterReflIllFrIntWins += BmInterReflIll;
}

void CalcMinIntWinSolidAngs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Winkelmann
    //       DATE WRITTEN   Feb. 2004
    //       MODIFIED:na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // For each Daylighting:Detailed zone finds the minimum solid angle subtended
    // by interior windows through which daylight can pass from adjacent zones with
    // exterior windows.

    // METHODOLOGY EMPLOYED:na
    // REFERENCES:na
    // Using/Aliasing

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS: na
    // SUBROUTINE PARAMETER DEFINITIONS: na
    // INTERFACE BLOCK SPECIFICATIONS: na
    // DERIVED TYPE DEFINITIONS: na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    bool is_Triangle;      // True if window is a triangle
    bool is_Rectangle;     // True if window is a rectangle
    Real64 IntWinSolidAng; // Approximation to solid angle subtended by an interior window
    // from a point a distance SQRT(zone floor area) away.
    auto &W1 = state.dataDaylightingManager->CalcMinIntWinSolidAngsW1; // Window vertices
    auto &W2 = state.dataDaylightingManager->CalcMinIntWinSolidAngsW2;
    auto &W3 = state.dataDaylightingManager->CalcMinIntWinSolidAngsW3;
    auto &WC = state.dataDaylightingManager->CalcMinIntWinSolidAngsWC;   // Center point of window
    auto &W21 = state.dataDaylightingManager->CalcMinIntWinSolidAngsW21; // Unit vectors from window vertex 2 to 1 and 2 to 3
    auto &W23 = state.dataDaylightingManager->CalcMinIntWinSolidAngsW23;
    auto &RREF = state.dataDaylightingManager->CalcMinIntWinSolidAngsRREF;   // Location of a reference point in absolute coordinate system
    auto &Ray = state.dataDaylightingManager->CalcMinIntWinSolidAngsRay;     // Unit vector along ray from reference point to window center
    auto &REFWC = state.dataDaylightingManager->CalcMinIntWinSolidAngsREFWC; // Vector from reference point to center of window
    auto &WNORM = state.dataDaylightingManager->CalcMinIntWinSolidAngsWNORM; // Unit vector normal to window (pointing away from room)
    Real64 HW;                                                               // Window height and width (m)
    Real64 WW;
    Real64 DIS;  // Distance from ref point to window center (m)
    Real64 COSB; // Cosine of angle between ray from ref pt to center of window
                 //  and window outward normal

    for (int enclNum = 1; enclNum <= state.dataViewFactor->NumOfSolarEnclosures; ++enclNum) {
        auto &thisEnclDaylight = state.dataDaylightingData->enclDaylight(enclNum);
        thisEnclDaylight.MinIntWinSolidAng = 2.0 * DataGlobalConstants::Pi;
        if (state.dataViewFactor->EnclSolInfo(enclNum).TotalEnclosureDaylRefPoints == 0) continue;
        if (thisEnclDaylight.NumOfIntWinAdjEncls == 0) continue;
        for (int IWin : state.dataViewFactor->EnclSolInfo(enclNum).SurfacePtr) {
            if ((state.dataSurface->Surface(IWin).Class == SurfaceClass::Window) && (state.dataSurface->Surface(IWin).ExtBoundCond >= 1)) {
                // This is an interior window in enclNum
                int const winAdjEnclNum = state.dataSurface->Surface(state.dataSurface->Surface(IWin).ExtBoundCond).SolarEnclIndex;
                bool IntWinNextToIntWinAdjZone = false; // True if an interior window is next to a zone with one or more exterior windows
                for (int adjEnclNum : thisEnclDaylight.AdjIntWinEnclNums) {
                    if (winAdjEnclNum == adjEnclNum) {
                        IntWinNextToIntWinAdjZone = true;
                        break;
                    }
                }
                if (IntWinNextToIntWinAdjZone) {
                    for (int controlNum : thisEnclDaylight.daylightControlIndexes) {
                        auto &thisDaylightControl = state.dataDaylightingData->daylightControl(controlNum);
                        for (int IL = 1; IL <= thisDaylightControl.TotalDaylRefPoints; ++IL) {
                            // Reference point in absolute coordinate system
                            RREF = thisDaylightControl.DaylRefPtAbsCoord({1, 3}, IL);
                            is_Triangle = (state.dataSurface->Surface(IWin).Sides == 3);
                            is_Rectangle = (state.dataSurface->Surface(IWin).Sides == 4);
                            if (is_Rectangle) {
                                // Vertices of window numbered counter-clockwise starting at upper left as viewed
                                // from inside of room. Assumes original vertices are numbered counter-clockwise from
                                // upper left as viewed from outside.
                                W3 = state.dataSurface->Surface(IWin).Vertex(2);
                                W2 = state.dataSurface->Surface(IWin).Vertex(3);
                                W1 = state.dataSurface->Surface(IWin).Vertex(4);
                            } else if (is_Triangle) {
                                W3 = state.dataSurface->Surface(IWin).Vertex(2);
                                W2 = state.dataSurface->Surface(IWin).Vertex(3);
                                W1 = state.dataSurface->Surface(IWin).Vertex(1);
                            }
                            // Unit vectors from window vertex 2 to 1 and 2 to 3, center point of window,
                            // and vector from ref pt to center of window
                            W21 = W1 - W2;
                            W23 = W3 - W2;
                            HW = W21.magnitude();
                            WW = W23.magnitude();
                            if (is_Rectangle) {
                                WC = W2 + (W23 + W21) / 2.0;
                            } else if (is_Triangle) {
                                WC = W2 + (W23 + W21) / 3.0;
                            }
                            // Vector from ref point to center of window
                            REFWC = WC - RREF;
                            W21 /= HW;
                            W23 /= WW;
                            // Unit vector normal to window (pointing away from room)
                            WNORM = state.dataSurface->Surface(IWin).OutNormVec;
                            // Distance from ref point to center of window
                            DIS = REFWC.magnitude();
                            // Unit vector from ref point to center of window
                            Ray = REFWC / DIS;
                            // Cosine of angle between ray from ref pt to center of window and window outward normal
                            COSB = dot(WNORM, Ray);
                            if (COSB > 0.01765) { // 0 <= B < 89 deg
                                // Above test avoids case where ref point cannot receive daylight directly from the
                                // interior window
                                IntWinSolidAng = COSB * state.dataSurface->Surface(IWin).Area / (pow_2(DIS) + 0.001);
                                thisEnclDaylight.MinIntWinSolidAng = min(thisEnclDaylight.MinIntWinSolidAng, IntWinSolidAng);
                            }
                        } // End of loop over reference points
                    }     // End of loop over daylighting controls
                }
            }
        } // End of loop over surfaces in zone
    }     // End of loop over zones
}

void CheckForGeometricTransform(EnergyPlusData &state, bool &doTransform, Real64 &OldAspectRatio, Real64 &NewAspectRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   February 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // check for geometrytransform in the daylighting access for reference and map points

    // METHODOLOGY EMPLOYED:
    // once reference points  have been converted to WCS,
    //  change them to reflect a different aspect
    // ratio for the entire building based on user input.

    // REFERENCES:
    // na

    // Using/Aliasing
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
    Array1D<Real64> rNumerics;
    int NAlphas;
    int NNum;
    int IOStat;
    std::string transformPlane;

    // begin execution
    // get user input...
    doTransform = false;
    OldAspectRatio = 1.0;
    NewAspectRatio = 1.0;

    if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject) == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 1,
                                                                 cAlphas,
                                                                 NAlphas,
                                                                 rNumerics,
                                                                 NNum,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        OldAspectRatio = rNumerics(1);
        NewAspectRatio = rNumerics(2);
        transformPlane = cAlphas(1);
        if (transformPlane != "XY") {
            ShowWarningError(state,
                             CurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(1) + "=\"" + cAlphas(1) + "...ignored.");
        }
        doTransform = true;
        state.dataSurface->AspectTransform = true;
    }
    if (state.dataSurface->WorldCoordSystem) {
        doTransform = false;
        state.dataSurface->AspectTransform = false;
    }
}

void WriteDaylightMapTitle(EnergyPlusData &state,
                           int const mapNum,
                           InputOutputFile &mapFile,
                           std::string const &mapName,
                           std::string const &environmentName,
                           int const ZoneNum,
                           std::string const &refPts,
                           Real64 const zcoord)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Greg Stark
    //       DATE WRITTEN   Sept 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // The purpose of the routine is to allow the daylighting map data to be written in various formats

    // must add correct number of commas at end
    const auto fullmapName = fmt::format("{}:{}:{} Illuminance [lux] (Hourly)", state.dataHeatBal->Zone(ZoneNum).Name, environmentName, mapName);
    print(mapFile,
          "Date/Time{}{}{}{}{}{}\n",
          state.dataDaylightingData->MapColSep,
          fullmapName,
          state.dataDaylightingData->MapColSep,
          refPts,
          state.dataDaylightingData->MapColSep,
          state.dataDaylightingData->MapColSep);

    if (state.dataSQLiteProcedures->sqlite) {
        state.dataSQLiteProcedures->sqlite->createSQLiteDaylightMapTitle(mapNum, fullmapName, environmentName, ZoneNum, refPts, zcoord);
    }
}

} // namespace EnergyPlus::DaylightingManager
