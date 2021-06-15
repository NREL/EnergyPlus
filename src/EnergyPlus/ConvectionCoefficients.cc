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
#include <array>
#include <cassert>
#include <cmath>
#include <string>
#include <unordered_map>
#include <unordered_set>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/ConvectionCoefficients.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataRoomAirModel.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/Material.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/Vectors.hh>

namespace EnergyPlus::ConvectionCoefficients {

// Module containing the routines dealing with the convection coefficients

// MODULE INFORMATION:
//       AUTHOR         Rick Strand
//       DATE WRITTEN   August 2000
//       MODIFIED       Brent Griffith, August 2010 expanded model choices
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// This module contain the routines dealing with convection coefficients.
// This module collects correlations/calculations for both the interior and exterior
// Manages a portion of the input and calculations for Hc values for use in surface heat balances.

// METHODOLOGY EMPLOYED:
// Subroutines are called to fill the variable HConvIn with the convection coefficient at
// the inside face.  or outside face for the current surface.

using namespace DataLoopNode;
using namespace DataHeatBalance;
using namespace DataSurfaces;
using namespace DataVectorTypes;

// Coefficients that modify the convection coeff based on surface roughness
Array1D<Real64> const RoughnessMultiplier(6, {2.17, 1.67, 1.52, 1.13, 1.11, 1.0});

void InitInteriorConvectionCoeffs(EnergyPlusData &state,
                                  const Array1D<Real64> &SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
                                  Optional_int_const ZoneToResimulate         // if passed in, then only calculate surfaces that have this zone
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   March 1998
    //       MODIFIED       Dan Fisher, Nov 2000
    //                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine initializes the arrays associated with interior
    // surface convection.  The main parameter which is initialized
    // in this routine is HConvIn, the convection coefficient on the
    // inside surface.

    // METHODOLOGY EMPLOYED:
    // Determine the temperature difference between the surface and the
    // zone air for the last time step and then base the calculation
    // of the convection coefficient on that value and the surface tilt.

    // REFERENCES:
    // (I)BLAST legacy routine VARTMP
    // 1.  Passive Solar Extension of the BLAST Program
    //       Appendix E. p. 17,18
    // 2.  ASHRAE
    //       Simple Algorithm:    ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1
    //       Detailed Algorithm:  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5
    // 3.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
    //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80
    // 4.  Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
    //       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137
    // 5.  ISO Standard 15099:2003e

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum; // DO loop counter for zones
    int SurfNum; // DO loop counter for surfaces in zone

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    if (state.dataConvectionCoefficient->GetUserSuppliedConvectionCoeffs) {
        GetUserConvectionCoefficients(state);
        state.dataConvectionCoefficient->GetUserSuppliedConvectionCoeffs = false;
    }

    if (state.dataConvectionCoefficient->NodeCheck) { // done once when conditions are ready...
        if (!state.dataGlobal->SysSizingCalc && !state.dataGlobal->ZoneSizingCalc && state.dataZoneEquip->ZoneEquipInputsFilled &&
            allocated(state.dataLoopNodes->Node)) {
            state.dataConvectionCoefficient->NodeCheck = false;
            for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
                if (Zone(ZoneNum).InsideConvectionAlgo != CeilingDiffuser) continue;
                if (Zone(ZoneNum).SystemZoneNodeNumber != 0) continue;
                ShowSevereError(state,
                                "InitInteriorConvectionCoeffs: Inside Convection=CeilingDiffuser, but no system inlet node defined, Zone=" +
                                    Zone(ZoneNum).Name);
                ShowContinueError(state, "Defaulting inside convection to TARP. Check ZoneHVAC:EquipmentConnections for Zone=" + Zone(ZoneNum).Name);
                Zone(ZoneNum).InsideConvectionAlgo = ASHRAETARP;
            }
            // insert one-time setup for adpative inside face
        }
    }

    if (state.dataConvectionCoefficient->ActiveSurfaceCheck && !state.dataGlobal->SysSizingCalc && !state.dataGlobal->ZoneSizingCalc &&
        state.dataZoneEquip->ZoneEquipSimulatedOnce) {
        SetupAdaptiveConvectionRadiantSurfaceData(state);
        state.dataConvectionCoefficient->ActiveSurfaceCheck = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && state.dataConvectionCoefficient->MyEnvirnFlag) {
        bool anyAdaptiveConvectionAlgorithm = false;
        for (int SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
            if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) == DataHeatBalance::AdaptiveConvectionAlgorithm) {
                anyAdaptiveConvectionAlgorithm = true;
                break;
            }
        }
        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataHeatBal->Zone(ZoneNum).InsideConvectionAlgo == DataHeatBalance::AdaptiveConvectionAlgorithm) {
                anyAdaptiveConvectionAlgorithm = true;
                break;
            }
        }
        if (anyAdaptiveConvectionAlgorithm) {
            // need to clear out node conditions because dynamic assignments will be affected
            if (state.dataLoopNodes->NumOfNodes > 0 && allocated(state.dataLoopNodes->Node)) {
                for (auto &e : state.dataLoopNodes->Node) {
                    e.Temp = state.dataLoopNodes->DefaultNodeValues.Temp;
                    e.TempMin = state.dataLoopNodes->DefaultNodeValues.TempMin;
                    e.TempMax = state.dataLoopNodes->DefaultNodeValues.TempMax;
                    e.TempSetPoint = state.dataLoopNodes->DefaultNodeValues.TempSetPoint;
                    e.MassFlowRate = state.dataLoopNodes->DefaultNodeValues.MassFlowRate;
                    e.MassFlowRateMin = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMin;
                    e.MassFlowRateMax = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMax;
                    e.MassFlowRateMinAvail = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMinAvail;
                    e.MassFlowRateMaxAvail = state.dataLoopNodes->DefaultNodeValues.MassFlowRateMaxAvail;
                    e.MassFlowRateSetPoint = state.dataLoopNodes->DefaultNodeValues.MassFlowRateSetPoint;
                    e.Quality = state.dataLoopNodes->DefaultNodeValues.Quality;
                    e.Press = state.dataLoopNodes->DefaultNodeValues.Press;
                    e.Enthalpy = state.dataLoopNodes->DefaultNodeValues.Enthalpy;
                    e.HumRat = state.dataLoopNodes->DefaultNodeValues.HumRat;
                    e.HumRatMin = state.dataLoopNodes->DefaultNodeValues.HumRatMin;
                    e.HumRatMax = state.dataLoopNodes->DefaultNodeValues.HumRatMax;
                    e.HumRatSetPoint = state.dataLoopNodes->DefaultNodeValues.HumRatSetPoint;
                    e.TempSetPointHi = state.dataLoopNodes->DefaultNodeValues.TempSetPointHi;
                    e.TempSetPointLo = state.dataLoopNodes->DefaultNodeValues.TempSetPointLo;
                }
                if (allocated(state.dataLoopNodes->MoreNodeInfo)) {
                    for (auto &e : state.dataLoopNodes->MoreNodeInfo) {
                        e.WetBulbTemp = state.dataLoopNodes->DefaultNodeValues.Temp;
                        e.RelHumidity = 0.0;
                        e.ReportEnthalpy = state.dataLoopNodes->DefaultNodeValues.Enthalpy;
                        e.VolFlowRateStdRho = 0.0;
                        e.VolFlowRateCrntRho = 0.0;
                        e.Density = 0.0;
                    }
                }
            }
        }
        state.dataConvectionCoefficient->MyEnvirnFlag = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) state.dataConvectionCoefficient->MyEnvirnFlag = true;
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        {
            auto const SELECT_CASE_var(Zone(ZoneNum).InsideConvectionAlgo);
            // Ceiling Diffuser and Trombe Wall only make sense at Zone Level
            // Interior convection coeffs are first calculated here and then at surface level
            if (SELECT_CASE_var == CeilingDiffuser) {
                CalcCeilingDiffuserIntConvCoeff(state, ZoneNum, SurfaceTemperatures);

            } else if (SELECT_CASE_var == TrombeWall) {
                CalcTrombeWallIntConvCoeff(state, ZoneNum, SurfaceTemperatures);

            } else {
            }
        }
    }
    for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {

        for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {

            if (present(ZoneToResimulate)) {
                if ((ZoneNum != ZoneToResimulate) && (state.dataSurface->AdjacentZoneToSurface(SurfNum) != ZoneToResimulate)) {
                    continue; // skip surfaces that are not associated with this zone
                }
            }

            int algoNum;
            bool standardAlgo;
            if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) <= -1) { // Set by user using one of the standard algorithms...
                algoNum = std::abs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum));
                standardAlgo = true;
            } else if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) == 0) { // Not set by user, uses Zone Setting
                algoNum = Zone(ZoneNum).InsideConvectionAlgo;
                standardAlgo = true;
            } else {
                algoNum = Zone(ZoneNum).InsideConvectionAlgo;
                standardAlgo = false;
            }

            if (standardAlgo) {
                auto const SELECT_CASE_var1(algoNum);

                if (SELECT_CASE_var1 == ASHRAESimple) {
                    CalcASHRAESimpleIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));
                    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
                    if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
                        state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;

                } else if (SELECT_CASE_var1 == ASHRAETARP) {
                    if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) {
                        CalcASHRAEDetailedIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));
                    } else {
                        CalcISO15099WindowIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));
                    }

                    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
                    if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
                        state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;

                } else if (SELECT_CASE_var1 == AdaptiveConvectionAlgorithm) {

                    ManageInsideAdaptiveConvectionAlgo(state, SurfNum);

                } else if ((SELECT_CASE_var1 == CeilingDiffuser) || (SELECT_CASE_var1 == TrombeWall)) {
                    // Already done above and can't be at individual surface

                } else if (SELECT_CASE_var1 == ASTMC1340) {
                    CalcASTMC1340ConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));

                } else {

                    ShowFatalError(state, "Unhandled convection coefficient algorithm.");
                }
            } else { // Interior convection has been set by the user with "value" or "schedule"
                state.dataHeatBal->HConvIn(SurfNum) = SetIntConvectionCoeff(state, SurfNum);
                // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
                if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
                    state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
            }

            if (state.dataSurface->SurfEMSOverrideIntConvCoef(SurfNum)) {
                state.dataHeatBal->HConvIn(SurfNum) = state.dataSurface->SurfEMSValueForIntConvCoef(SurfNum);
                if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                    Real64 hConst = state.dataSurface->SurfEMSValueForIntConvCoef(SurfNum);
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = KIVA_CONST_CONV(hConst);
                }
            }
        }
    }
}

void InitExteriorConvectionCoeff(EnergyPlusData &state,
                                 int const SurfNum,      // Surface number (in Surface derived type)
                                 Real64 const HMovInsul, // Equivalent convection coefficient of movable insulation
                                 int const Roughness,    // Roughness index (1-6), see DataHeatBalance parameters
                                 Real64 const AbsExt,    // Exterior thermal absorptance
                                 Real64 const TempExt,   // Exterior surface temperature (C)
                                 Real64 &HExt,           // Convection coefficient to exterior air
                                 Real64 &HSky,           // "Convection" coefficient to sky temperature
                                 Real64 &HGround,        // "Convection" coefficient to ground temperature
                                 Real64 &HAir            // Radiation to Air Component
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         George Walton
    //       DATE WRITTEN   January 1990
    //       MODIFIED       na
    //       RE-ENGINEERED  Mar98 (RKS); Sep03 (LKL): Add additional flavors of Ext Convection Coeff.
    //                      Dec03 (PGE): Re-eng'd ASHRAEDetailed to match BLAST and TARP.
    //                      Aug04 (PGE): Corrected error for calculating local wind speeds for different terrains.
    //                      Aug 2010 B. Griffith.  for outside air convection, added new adaptive convection algorithm etc.

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine determines the outside convection coefficient for
    // a particular surface.

    // METHODOLOGY EMPLOYED:
    // Based on the properties of a particular surface, determine what the
    // outside convection coefficients are for outside air, the sky, and
    // the ground.  Convection coefficients for the sky and ground are
    // actually linearized radiation coefficients.  The ground surface is
    // assumed to be the same temperature as the outside air.

    // REFERENCES:
    // (I)BLAST legacy routine OCNVCO
    // TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TAir;          // Absolute dry bulb temperature of outdoor air (K)
    Real64 TSurf;         // Absolute temperature of the exterior surface (K)
    Real64 SurfWindSpeed; // Local wind speed at height of the heat transfer surface (m/s)
    Real64 SurfWindDir;
    Real64 TSky;
    Real64 TGround;
    Real64 Hn;             // Natural part of exterior convection
    Real64 Hf;             // Forced part of exterior convection
    Real64 rCalcPerimeter; // approximation for Perimeter
    int BaseSurf;
    int SrdSurfsNum; // Srd surface counter

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    if (state.dataConvectionCoefficient->GetUserSuppliedConvectionCoeffs) {
        GetUserConvectionCoefficients(state);
        state.dataConvectionCoefficient->GetUserSuppliedConvectionCoeffs = false;
    }

    TAir = state.dataSurface->SurfOutDryBulbTemp(SurfNum) + DataGlobalConstants::KelvinConv;
    TSurf = TempExt + DataGlobalConstants::KelvinConv;
    TSky = state.dataEnvrn->SkyTempKelvin;
    TGround = TAir;

    if (state.dataSurface->SurfHasSurroundingSurfProperties(SurfNum)) {
        SrdSurfsNum = state.dataSurface->SurfSurroundingSurfacesNum(SurfNum);
        if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum != 0) {
            TSky = GetCurrentScheduleValue(state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).SkyTempSchNum) +
                   DataGlobalConstants::KelvinConv;
        }
        if (state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum != 0) {
            TGround = GetCurrentScheduleValue(state, state.dataSurface->SurroundingSurfsProperty(SrdSurfsNum).GroundTempSchNum) +
                      DataGlobalConstants::KelvinConv;
        }
    }

    BaseSurf = Surface(SurfNum).BaseSurf; // If this is a base surface, BaseSurf = SurfNum

    SurfWindDir = state.dataSurface->SurfOutWindDir(SurfNum);

    if (!Surface(SurfNum).ExtWind) {
        SurfWindSpeed = 0.0; // No wind exposure
    } else if (Surface(SurfNum).Class == SurfaceClass::Window && state.dataSurface->SurfWinShadingFlag(SurfNum) == WinShadingType::ExtShade) {
        SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
    } else {
        SurfWindSpeed = state.dataSurface->SurfOutWindSpeed(SurfNum);
    }

    // Check if exterior is to be set by user

    int algoNum;
    bool standardAlgo;
    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) <= -1) { // Set by user using one of the standard algorithms...
        algoNum = std::abs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum));
        standardAlgo = true;
    } else if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) == 0) { // Not set by user, uses Zone Setting
        algoNum = Zone(Surface(SurfNum).Zone).OutsideConvectionAlgo;
        standardAlgo = true;
    } else {
        algoNum = Zone(Surface(SurfNum).Zone).OutsideConvectionAlgo;
        standardAlgo = false;
    }

    if (standardAlgo) {

        auto const SELECT_CASE_var1(algoNum);

        if (SELECT_CASE_var1 == ASHRAESimple) {

            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [](double, double, double, double windSpeed) -> double {
                    return windSpeed;
                };
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out = [=](double, double, double hfTerm, double, double) -> double {
                    return CalcASHRAESimpExtConvectCoeff(Roughness, hfTerm);
                };
            } else {
                HExt = CalcASHRAESimpExtConvectCoeff(Roughness, SurfWindSpeed); // includes radiation to sky, ground, and air
            }

        } else if ((SELECT_CASE_var1 == ASHRAETARP) || (SELECT_CASE_var1 == BLASTHcOutside) || (SELECT_CASE_var1 == TarpHcOutside)) {
            //   Convection is split into forced and natural components. The total
            //   convective heat transfer coefficient is the sum of these components.
            //   Coefficients for subsurfaces are handled in a special way.  The values for perimeter and gross area
            //   are actually referencing the base surface because a subsurface does not initiate a completely new
            //   thermal boundary layer (although it may add some additional complexity that cannot be accounted for
            //   here).  The values for height (Z) and roughness do, however, come from the subsurface.
            //   BLAST algorithm has been replaced by this one since it was identical except for the standard wind
            //   speed measurement height which was only different because of unit conversions:  10 m vs. 30 ft (= 9.14 m).
            //   ASHRAE/BLAST REFERENCES:
            //   ?
            //   TARP REFERENCES:
            //   Walton, G. N.  1983.  Thermal Analysis Research Program Reference Manual.
            //   National Bureau of Standards.  NBSSIR 83-2655.

            // due to outlying calculations when perimeter is very small compared to area, use Perimeter
            // approximation calculation

            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                    auto &fnd = state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].get_instance(0).first->foundation;
                    const double length = fnd.netPerimeter;
                    const double height = fnd.wall.heightAboveGrade;
                    const double area = length * height;
                    const double perim = 2.0 * (length + height);
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        // Average windward and leeward since all walls use same algorithm
                        double windwardHf = CalcSparrowWindward(Roughness, perim, area, windSpeed);
                        double leewardHf = CalcSparrowLeeward(Roughness, perim, area, windSpeed);
                        return (windwardHf + leewardHf) / 2.0;
                    };
                } else { // Slab (used for exterior grade convection)
                    // Assume very large area for grade (relative to perimeter).
                    const double area = 9999999.;
                    const double perim = 1.;
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        return CalcSparrowWindward(Roughness, perim, area, windSpeed);
                    };
                }
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out =
                    [=](double Tsurf, double Tamb, double hfTerm, double, double cosTilt) -> double {
                    Real64 Ts = Tsurf;
                    if (HMovInsul > 0.0) Ts = (HMovInsul * Tsurf + hfTerm * Tamb) / (HMovInsul + hfTerm);
                    return CalcASHRAETARPNatural(Ts, Tamb, cosTilt) + hfTerm;
                };
            } else {
                if (Surface(BaseSurf).GrossArea != 0.0 && Surface(BaseSurf).Height != 0.0) {
                    rCalcPerimeter = 2.0 * (Surface(BaseSurf).GrossArea / Surface(BaseSurf).Height + Surface(BaseSurf).Height);
                    Hf = CalcHfExteriorSparrow(SurfWindSpeed,
                                               Surface(BaseSurf).GrossArea,
                                               rCalcPerimeter,
                                               Surface(SurfNum).CosTilt,
                                               Surface(SurfNum).Azimuth,
                                               Roughness,
                                               SurfWindDir);
                } else {
                    Hf = 0.0;
                }

                if (HMovInsul > 0.0) TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf);
                Hn = CalcASHRAETARPNatural(TSurf, TAir, Surface(SurfNum).CosTilt);
                HExt = Hn + Hf;
            }

        } else if (SELECT_CASE_var1 == MoWiTTHcOutside) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {

                if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        // Average windward and leeward since all walls use same algorithm
                        double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                        double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                        return (windwardHf + leewardHf) / 2.0;
                    };
                } else {
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        return CalcMoWITTForcedWindward(windSpeed);
                    };
                }
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out =
                    [=](double Tsurf, double Tamb, double hfTerm, double, double) -> double {
                    Real64 Hn = CalcMoWITTNatural(Tsurf - Tamb);
                    return std::sqrt(pow_2(Hn) + pow_2(hfTerm));
                };
            } else {
                // NOTE: Movable insulation is not taken into account here
                if (Windward(Surface(SurfNum).CosTilt, Surface(SurfNum).Azimuth, SurfWindDir)) {
                    HExt = CalcMoWITTWindward(TAir - TSurf, SurfWindSpeed);
                } else { // leeward
                    HExt = CalcMoWITTLeeward(TAir - TSurf, SurfWindSpeed);
                }
            }

        } else if (SELECT_CASE_var1 == DOE2HcOutside) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                if (Surface(SurfNum).Class == SurfaceClass::Wall) {
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        // Average windward and leeward since all walls use same algorithm
                        double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                        double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                        return (windwardHf + leewardHf) / 2.0;
                    };
                } else {
                    state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = [=](double, double, double, double windSpeed) -> double {
                        return CalcMoWITTForcedWindward(windSpeed);
                    };
                }
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out =
                    [=](double Tsurf, double Tamb, double hfTerm, double, double cosTilt) -> double {
                    Real64 Hf = CalcDOE2Forced(Tsurf, Tamb, cosTilt, hfTerm, Roughness);

                    Real64 Ts = Tsurf;
                    if (HMovInsul > 0.0) {
                        Ts = (HMovInsul * TSurf + Hf * Tamb) / (HMovInsul + Hf);
                    }

                    Real64 Hn = CalcASHRAETARPNatural(Ts, Tamb, cosTilt);
                    return Hn + Hf;
                };
            } else {
                if (Windward(Surface(SurfNum).CosTilt, Surface(SurfNum).Azimuth, SurfWindDir)) {
                    Hf = CalcDOE2Windward(TSurf, TAir, Surface(SurfNum).CosTilt, SurfWindSpeed, Roughness);
                } else { // leeward
                    Hf = CalcDOE2Leeward(TSurf, TAir, Surface(SurfNum).CosTilt, SurfWindSpeed, Roughness);
                }
                if (HMovInsul > 0.0) {
                    TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf);
                }

                Hn = CalcASHRAETARPNatural(TSurf, TAir, Surface(SurfNum).CosTilt);
                // Better if there was iteration for movable insulation?

                HExt = Hn + Hf;
            }

        } else if (SELECT_CASE_var1 == AdaptiveConvectionAlgorithm) {

            ManageOutsideAdaptiveConvectionAlgo(state, SurfNum, HExt);

        } else {
            ShowFatalError(state, "InitExtConvection Coefficients: invalid parameter -- outside convection type, Surface=" + Surface(SurfNum).Name);
        }

    } else { // Exterior convection scheme for this surface has been set by user

        HExt = SetExtConvectionCoeff(state, SurfNum);
    }

    if (state.dataSurface->SurfEMSOverrideExtConvCoef(SurfNum)) {
        HExt = state.dataSurface->SurfEMSValueForExtConvCoef(SurfNum);
        if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = KIVA_HF_ZERO;
            Real64 hConst = state.dataSurface->SurfEMSValueForExtConvCoef(SurfNum);
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out = KIVA_CONST_CONV(hConst);
        }
    }

    if (TSurf == TSky || algoNum == ASHRAESimple) {
        HSky = 0.0;
    } else {
        // Compute sky radiation coefficient
        HSky = DataGlobalConstants::StefanBoltzmann * AbsExt * Surface(SurfNum).ViewFactorSkyIR * state.dataSurface->SurfAirSkyRadSplit(SurfNum) *
               (pow_4(TSurf) - pow_4(TSky)) / (TSurf - TSky);
    }

    if (TSurf == TAir || algoNum == ASHRAESimple) {
        HGround = 0.0;
        HAir = 0.0;
    } else {
        // Compute ground radiation coefficient
        HGround =
            DataGlobalConstants::StefanBoltzmann * AbsExt * Surface(SurfNum).ViewFactorGroundIR * (pow_4(TSurf) - pow_4(TGround)) / (TSurf - TGround);

        // Compute air radiation coefficient
        HAir = DataGlobalConstants::StefanBoltzmann * AbsExt * Surface(SurfNum).ViewFactorSkyIR *
               (1.0 - state.dataSurface->SurfAirSkyRadSplit(SurfNum)) * (pow_4(TSurf) - pow_4(TAir)) / (TSurf - TAir);
    }
}

Real64 CalcHfExteriorSparrow(Real64 const SurfWindSpeed, // Local wind speed at height of the heat transfer surface (m/s)
                             Real64 const GrossArea,     // Gross surface area {m2}
                             Real64 const Perimeter,     // Surface perimeter length {m}
                             Real64 const CosTilt,       // Cosine of the Surface Tilt Angle
                             Real64 const Azimuth,       // Facing angle (degrees) of the surface outward normal
                             int const Roughness,        // Surface roughness index (6=very smooth, 5=smooth, 4=medium smooth,
                             Real64 const WindDirection  // Wind (compass) direction (degrees)
)
{
    if (Windward(CosTilt, Azimuth, WindDirection)) {
        return CalcSparrowWindward(Roughness, Perimeter, GrossArea, SurfWindSpeed);
    } else {
        return CalcSparrowLeeward(Roughness, Perimeter, GrossArea, SurfWindSpeed);
    }
}

bool Windward(Real64 const CosTilt,      // Cosine of the surface tilt angle
              Real64 const Azimuth,      // or Facing, Direction the surface outward normal faces (degrees)
              Real64 const WindDirection // Wind direction measured clockwise from geographhic North
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function determines if a surface is "windward" or "leeward" (that is,
    // into / against the wind (true) or in shelter from wind (false).

    // METHODOLOGY EMPLOYED:
    // Leeward is defined as greater than 100 degrees from normal incidence.
    // Note that a sufficiently horizontal surface is always considered windward.

    // REFERENCES:
    //   Walton, G. N.  1981.  Passive solar extension of the Building Loads
    //   Analysis and System Thermodynamics (BLAST) program.  Technical Report,
    //   United States Army Construction Engineering Research Laboratory,
    //   Champaign, IL.

    // Return value
    bool AgainstWind; // True for windward, false for leeward.

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 Diff; // Difference between the wind direction and the surface azimuth

    AgainstWind = true;
    if (std::abs(CosTilt) < 0.98) { // Surface is not horizontal
        Diff = std::abs(WindDirection - Azimuth);
        if ((Diff - 180.0) > 0.001) Diff -= 360.0;
        if ((std::abs(Diff) - 90.0) > 0.001) AgainstWind = false; // Surface is leeward
    }

    return AgainstWind;
}

bool SetAdaptiveConvectionAlgoCoefficient(EnergyPlusData &state,
                                          const std::unordered_map<std::string, int> &HcInt_ConvectionTypesMap,
                                          int *const InsideFaceAdaptiveConvectionAlgoParam,
                                          const std::string &equationName,
                                          const std::string &curveName,
                                          const std::string &sourceFieldName,
                                          const std::string &curveFieldName,
                                          const std::string_view RoutineName,
                                          const std::string_view CurrentModuleObject)
{

    bool ErrorsFound = false;

    if (HcInt_ConvectionTypesMap.find(equationName) != HcInt_ConvectionTypesMap.end()) {
        *InsideFaceAdaptiveConvectionAlgoParam = HcInt_ConvectionTypesMap.at(equationName);
        if (HcInt_ConvectionTypesMap.at(equationName) == HcInt_UserCurve) {
            *InsideFaceAdaptiveConvectionAlgoParam = UtilityRoutines::FindItemInList(curveName, state.dataConvectionCoefficient->HcInsideUserCurve);
            if (*InsideFaceAdaptiveConvectionAlgoParam == 0) {
                ShowSevereError(state, std::string{RoutineName} + std::string{CurrentModuleObject} + "=\"" + equationName + ", invalid value");
                ShowContinueError(state, "Invalid Name choice Entered, for " + curveFieldName + '=' + curveName);
                ErrorsFound = true;
            }
        }
    } else {
        ShowSevereError(state, std::string{RoutineName} + std::string{CurrentModuleObject} + "=\"" + equationName + ", invalid value");
        ShowContinueError(state, "Invalid Key choice Entered, for " + sourceFieldName + '=' + equationName);
        ErrorsFound = true;
    }
    return ErrorsFound;
}

void GetUserConvectionCoefficients(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   February 2003
    //       MODIFIED       November 2004; add more "user supplied convection coefficients"
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the input for the object "Convection Coefficients" which
    // can be specified by a user to override the "normally" calculated convection coefficients.  The
    // change (November 2004) allows the user to specify down to the "surface level" the
    // exterior or interior algorithm to be used.

    // REFERENCES:
    // This routine gets the objects:
    // SurfaceProperty:ConvectionCoefficients,
    //      \memo Allow user settable interior and/or exterior convection coefficients.
    //      \memo Note that some other factors may limit the lower bounds for these values, such as
    //      \memo for windows, the interior convection coefficient must be >.28,
    //      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
    //      \memo for detailed interior convection, the lower limit is also .1
    //  A1, \field Surface Name
    //      \required-field
    //      \type object-list
    //      \object-list SurfaceNames
    //  A2, \field Convection Coefficient 1 Location
    //      \required-field
    //      \type choice
    //      \key Outside
    //      \key Inside
    //  A3, \field Convection Coefficient 1 Type
    //      \required-field
    //      \type choice
    //      \key Value
    //      \key Schedule
    //      \key Simple
    //      \key Detailed
    //      \key BLAST
    //      \key TARP
    //      \key DOE-2
    //      \key MoWitt
    //  N1, \field Convection Coefficient 1
    //      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
    //      \units W/m2-K
    //  A4, \field Convection Coefficient 1 Schedule Name
    //       \note used if Convection Value Type=Schedule Name
    //       \type object-list
    //       \object-list ScheduleNames
    //   < Remainder fields are a repeat of A2, A3, N1, A4>
    // SurfaceProperty:ConvectionCoefficients:MultipleSurface,
    //      \memo Allow user settable interior and/or exterior convection coefficients.
    //      \memo Note that some other factors may limit the lower bounds for these values, such as
    //      \memo for windows, the interior convection coefficient must be >.28,
    //      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
    //      \memo for detailed interior convection, the lower limit is also .1
    //  A1, \field Surface Type
    //      \required-field
    //      \type choice
    //      \key AllExteriorSurfaces
    //      \key AllExteriorWindows
    //      \key AllExteriorWalls
    //      \key AllExteriorRoofs
    //      \key AllExteriorFloors
    //      \key AllInteriorSurfaces
    //      \key AllInteriorWalls
    //      \key AllInteriorWindows
    //      \key AllInteriorCeilings
    //      \key AllInteriorFloors
    //  A2, \field Convection Coefficient 1 Location
    //      \required-field
    //      \type choice
    //      \key Outside
    //      \key Inside
    //  A3, \field Convection Coefficient 1 Type
    //      \required-field
    //      \type choice
    //      \key Value
    //      \key Schedule
    //      \key Simple
    //      \key Detailed
    //      \key BLAST
    //      \key TARP
    //      \key DOE-2
    //      \key MoWitt
    //  N1, \field Convection Coefficient 1
    //      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
    //      \units W/m2-K
    //  A4, \field Convection Coefficient 1 Schedule Name
    //       \note used if Convection Coefficient Type=Schedule
    //       \type object-list
    //       \object-list ScheduleNames
    //   < Remainder fields are a repeat of A2, A3, N1, A4>

    // Using/Aliasing
    using CurveManager::GetCurveIndex;
    using ScheduleManager::CheckScheduleValueMinMax;
    using ScheduleManager::GetScheduleIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetUserConvectionCoefficients");
    const std::unordered_set<std::string> ValidSurfaceTypes = {"ALLEXTERIORSURFACES",
                                                               "ALLEXTERIORWINDOWS",
                                                               "ALLEXTERIORWALLS",
                                                               "ALLEXTERIORROOFS",
                                                               "ALLEXTERIORFLOORS",
                                                               "ALLINTERIORSURFACES",
                                                               "ALLINTERIORWINDOWS",
                                                               "ALLINTERIORWALLS",
                                                               "ALLINTERIORROOFS",
                                                               "ALLINTERIORCEILINGS",
                                                               "ALLINTERIORFLOORS"};

    const std::unordered_map<std::string, int> HcInt_ConvectionTypesMap = {
        {"VALUE", -999},
        {"SCHEDULE", -999},
        {"USERCURVE", HcInt_UserCurve},
        {"ASHRAEVERTICALWALL", HcInt_ASHRAEVerticalWall},
        {"WALTONUNSTABLEHORIZONTALORTILT", HcInt_WaltonUnstableHorizontalOrTilt},
        {"WALTONSTABLEHORIZONTALORTILT", HcInt_WaltonStableHorizontalOrTilt},
        {"FISHERPEDERSENCEILINGDIFFUSERWALLS", HcInt_FisherPedersenCeilDiffuserWalls},
        {"FISHERPEDERSENCEILINGDIFFUSERCEILING", HcInt_FisherPedersenCeilDiffuserCeiling},
        {"FISHERPEDERSENCEILINGDIFFUSERFLOOR", HcInt_FisherPedersenCeilDiffuserFloor},
        {"ALAMDARIHAMMONDSTABLEHORIZONTAL", HcInt_AlamdariHammondStableHorizontal},
        {"ALAMDARIHAMMONDUNSTABLEHORIZONTAL", HcInt_AlamdariHammondUnstableHorizontal},
        {"ALAMDARIHAMMONDVERTICALWALL", HcInt_AlamdariHammondVerticalWall},
        {"KHALIFAEQ3WALLAWAYFROMHEAT", HcInt_KhalifaEq3WallAwayFromHeat},
        {"KHALIFAEQ4CEILINGAWAYFROMHEAT", HcInt_KhalifaEq4CeilingAwayFromHeat},
        {"KHALIFAEQ5WALLNEARHEAT", HcInt_KhalifaEq5WallNearHeat},
        {"KHALIFAEQ6NONHEATEDWALLS", HcInt_KhalifaEq6NonHeatedWalls},
        {"KHALIFAEQ7CEILING", HcInt_KhalifaEq7Ceiling},
        {"AWBIHATTONHEATEDFLOOR", HcInt_AwbiHattonHeatedFloor},
        {"AWBIHATTONHEATEDWALL", HcInt_AwbiHattonHeatedWall},
        {"BEAUSOLEILMORRISONMIXEDASSISTEDWALL", HcInt_BeausoleilMorrisonMixedAssistingWall},
        {"BEAUSOLEILMORRISONMIXEDOPPOSINGWALL", HcInt_BeausoleilMorrisonMixedOppossingWall},
        {"BEAUSOLEILMORRISONMIXEDSTABLEFLOOR", HcInt_BeausoleilMorrisonMixedStableFloor},
        {"BEAUSOLEILMORRISONMIXEDUNSTABLEFLOOR", HcInt_BeausoleilMorrisonMixedUnstableFloor},
        {"BEAUSOLEILMORRISONMIXEDSTABLECEILING", HcInt_BeausoleilMorrisonMixedStableCeiling},
        {"BEAUSOLEILMORRISONMIXEDUNSTABLECEILING", HcInt_BeausoleilMorrisonMixedUnstableCeiling},
        {"FOHANNOPOLIDORIVERTICALWALL", HcInt_FohannoPolidoriVerticalWall},
        {"KARADAGCHILLEDCEILING", HcInt_KaradagChilledCeiling},
        {"ISO15099WINDOWS", HcInt_ISO15099Windows},
        {"GOLDSTEINNOVOSELACCEILINGDIFFUSERWINDOW", HcInt_GoldsteinNovoselacCeilingDiffuserWindow},
        {"GOLDSTEINNOVOSELACCEILINGDIFFUSERWALLS", HcInt_GoldsteinNovoselacCeilingDiffuserWalls},
        {"GOLDSTEINNOVOSELACCEILINGDIFFUSERFLOOR", HcInt_GoldsteinNovoselacCeilingDiffuserFloor},
    };

    std::unordered_map<std::string, int> HcExt_ConvectionTypesMap = {
        {"VALUE", -999},
        {"SCHEDULE", -999},
        {"TARP", TarpHcOutside},
        {"MOWITT", MoWiTTHcOutside},
        {"DOE-2", DOE2HcOutside},
        {"ADAPTIVECONVECTIONALGORITHM", AdaptiveConvectionAlgorithm},
        {"USERCURVE", HcExt_UserCurve},
        {"ASHRAEVERTICALWALL", HcExt_NaturalASHRAEVerticalWall},
        {"WALTONUNSTABLEHORIZONTALORTILT", HcExt_NaturalWaltonUnstableHorizontalOrTilt},
        {"WALTONSTABLEHORIZONTALORTILT", HcExt_NaturalWaltonStableHorizontalOrTilt},
        {"NUSSELTJURGES", HcExt_NusseltJurges},
        {"MCADAMS", HcExt_McAdams},
        {"MITCHELL", HcExt_Mitchell},
        {"CLEARROOF", HcExt_ClearRoof},
        {"EMMELVERTICAL", HcExt_EmmelVertical},
        {"EMMELROOF", HcExt_EmmelRoof},
        {"ALAMDARIHAMMONDVERTICALWALL", HcExt_AlamdariHammondVerticalWall},
        {"FOHANNOPOLIDORIVERTICALWALL", HcExt_FohannoPolidoriVerticalWall},
        {"ISO15099WINDOWS", HcExt_ISO15099Windows},
        {"ALAMDARIHAMMONDSTABLEHORIZONTAL", HcExt_AlamdariHammondStableHorizontal},
        {"ALAMDARIHAMMONDUNSTABLEHORIZONTAL", HcExt_AlamdariHammondUnstableHorizontal},
        {"SIMPLECOMBINED", HcExt_ASHRAESimpleCombined},
        {"TARPWINDWARD", HcExt_SparrowWindward},
        {"TARPLEEWARD", HcExt_SparrowLeeward},
        {"MOWITTWINDWARD", HcExt_MoWiTTWindward},
        {"MOWITTLEEWARD", HcExt_MoWiTTLeeward},
        {"DOE2WINDWARD", HcExt_DOE2Windward},
        {"DOE2LEEWARD", HcExt_DOE2Leeward},
        {"BLOCKENWINDWARD", HcExt_BlockenWindward},
    };

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_string Alphas(9);
    Array1D<Real64> Numbers(2);
    int NumAlphas;
    int NumNumbers;
    int Count;
    int Status;
    int Found;
    bool ErrorsFound(false);
    int ExtValue;
    int IntValue;
    int Ptr;
    int Pass;
    int FieldNo;
    int NumField;
    std::string CurrentModuleObject;
    int PotentialAssignedValue;
    int SurfNum;

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    // first get user-defined H models so they can be processed for later objects
    CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside:UserCurve";
    int TotInsideHcUserCurves = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataConvectionCoefficient->HcInsideUserCurve.allocate(TotInsideHcUserCurves);
    for (int Loop = 1; Loop <= TotInsideHcUserCurves; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        state.dataConvectionCoefficient->HcInsideUserCurve(Loop).Name = state.dataIPShortCut->cAlphaArgs(1);
        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));
            if (SELECT_CASE_var == "MEANAIRTEMPERATURE") {
                state.dataConvectionCoefficient->HcInsideUserCurve(Loop).ReferenceTempType = RefTempMeanAirTemp;
            } else if (SELECT_CASE_var == "ADJACENTAIRTEMPERATURE") {
                state.dataConvectionCoefficient->HcInsideUserCurve(Loop).ReferenceTempType = RefTempAdjacentAirTemp;
            } else if (SELECT_CASE_var == "SUPPLYAIRTEMPERATURE") {
                state.dataConvectionCoefficient->HcInsideUserCurve(Loop).ReferenceTempType = RefTempSupplyAirTemp;
            } else {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Key choice Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ErrorsFound = true;
            }
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffCurveNum = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if (state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + '=' + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffCurveNum, // Curve index
                                                 {1},                                                                           // Valid dimensions
                                                 RoutineName,                                                                   // Routine name
                                                 CurrentModuleObject,                                                           // Object Type
                                                 state.dataConvectionCoefficient->HcInsideUserCurve(Loop).Name,                 // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(3));                                    // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffCurveNum = 0;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffDivHeightCurveNum =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffDivHeightCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + '=' + state.dataIPShortCut->cAlphaArgs(4));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffDivHeightCurveNum, // Curve index
                    {1},                                                                                    // Valid dimensions
                    RoutineName,                                                                            // Routine name
                    CurrentModuleObject,                                                                    // Object Type
                    state.dataConvectionCoefficient->HcInsideUserCurve(Loop).Name,                          // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(4));                                             // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnTempDiffDivHeightCurveNum = 0;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHCurveNum = GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
            if (state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + '=' + state.dataIPShortCut->cAlphaArgs(5));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHCurveNum, // Curve index
                                                            {1},                                                           // Valid dimensions
                                                            RoutineName,                                                   // Routine name
                                                            CurrentModuleObject,                                           // Object Type
                                                            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).Name, // Object Name
                                                            state.dataIPShortCut->cAlphaFieldNames(5));                    // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHCurveNum = 0;
        }

        if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHDivPerimLengthCurveNum =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(6));
            if (state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHDivPerimLengthCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHDivPerimLengthCurveNum, // Curve index
                    {1},                                                                                    // Valid dimensions
                    RoutineName,                                                                            // Routine name
                    CurrentModuleObject,                                                                    // Object Type
                    state.dataConvectionCoefficient->HcInsideUserCurve(Loop).Name,                          // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(6));                                             // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcInsideUserCurve(Loop).HcFnACHDivPerimLengthCurveNum = 0;
        }

    } // end of 'SurfaceConvectionAlgorithm:Inside:UserCurve'

    CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside:UserCurve";
    int TotOutsideHcUserCurves = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataConvectionCoefficient->HcOutsideUserCurve.allocate(TotOutsideHcUserCurves);
    for (int Loop = 1; Loop <= TotOutsideHcUserCurves; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).Name = state.dataIPShortCut->cAlphaArgs(1);

        {
            auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(2));

            if (SELECT_CASE_var == "WEATHERFILE") {
                state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).WindSpeedType = RefWindWeatherFile;
            } else if (SELECT_CASE_var == "HEIGHTADJUST") {
                state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).WindSpeedType = RefWindAtZ;
            } else if (SELECT_CASE_var == "PARALLELCOMPONENT") {
                state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).WindSpeedType = RefWindParallComp;
            } else if (SELECT_CASE_var == "PARALLELCOMPONENTHEIGHTADJUST") {
                state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).WindSpeedType = RefWindParallCompAtZ;
            } else {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Key choice Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ErrorsFound = true;
            }
        }

        // A3 , \field Hf Function of Wind Speed Curve Name
        if (!state.dataIPShortCut->lAlphaFieldBlanks(3)) {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HfFnWindSpeedCurveNum =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(3));
            if (state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HfFnWindSpeedCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(3) + '=' + state.dataIPShortCut->cAlphaArgs(3));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HfFnWindSpeedCurveNum, // Curve index
                                                 {1},                                                                             // Valid dimensions
                                                 RoutineName,                                                                     // Routine name
                                                 CurrentModuleObject,                                                             // Object Type
                                                 state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).Name,                  // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(3));                                      // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HfFnWindSpeedCurveNum = 0;
        }

        //  A4 , \field Hn Function of Temperature Difference Curve Name
        if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffCurveNum =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(4) + '=' + state.dataIPShortCut->cAlphaArgs(4));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |=
                    CurveManager::CheckCurveDims(state,
                                                 state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffCurveNum, // Curve index
                                                 {1},                                                                            // Valid dimensions
                                                 RoutineName,                                                                    // Routine name
                                                 CurrentModuleObject,                                                            // Object Type
                                                 state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).Name,                 // Object Name
                                                 state.dataIPShortCut->cAlphaFieldNames(4));                                     // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffCurveNum = 0;
        }

        //  A5 , \field Hn Function of Temperature Difference Divided by Height Curve Name
        if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffDivHeightCurveNum =
                GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
            if (state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffDivHeightCurveNum == 0) {
                ShowSevereError(state,
                                "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ": Invalid Name Entered, for " +
                                    state.dataIPShortCut->cAlphaFieldNames(5) + '=' + state.dataIPShortCut->cAlphaArgs(5));
                ErrorsFound = true;
            } else { // check type
                ErrorsFound |= CurveManager::CheckCurveDims(
                    state,
                    state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffDivHeightCurveNum, // Curve index
                    {1},                                                                                     // Valid dimensions
                    RoutineName,                                                                             // Routine name
                    CurrentModuleObject,                                                                     // Object Type
                    state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).Name,                          // Object Name
                    state.dataIPShortCut->cAlphaFieldNames(5));                                              // Field Name
            }
        } else {
            state.dataConvectionCoefficient->HcOutsideUserCurve(Loop).HnFnTempDiffDivHeightCurveNum = 0;
        }

    } // 'SurfaceConvectionAlgorithm:Outside:UserCurve'

    // now get user directed overrides at the surface level.
    state.dataSurface->TotIntConvCoeff = 0;
    state.dataSurface->TotExtConvCoeff = 0;
    CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients:MultipleSurface";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    for (int Loop = 1; Loop <= Count; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (Alphas(2) == "INSIDE") {
            ++state.dataSurface->TotIntConvCoeff;
        }
        if (Alphas(6) == "INSIDE") {
            ++state.dataSurface->TotIntConvCoeff;
        }
        if (Alphas(2) == "OUTSIDE") {
            ++state.dataSurface->TotExtConvCoeff;
        }
        if (Alphas(6) == "OUTSIDE") {
            ++state.dataSurface->TotExtConvCoeff;
        }
        if (NumAlphas >= 2 && state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            ShowWarningError(state,
                             "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                 Alphas(1));
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(2) + " is blank and rest of fields will not be processed.");
        }
        if (NumAlphas >= 6 && state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            ShowWarningError(state,
                             "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                 Alphas(1));
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(6) + " is blank and rest of fields will not be processed.");
        }
    }
    CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    for (int Loop = 1; Loop <= Count; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        if (Alphas(2) == "INSIDE") {
            ++state.dataSurface->TotIntConvCoeff;
        }
        if (Alphas(6) == "INSIDE") {
            ++state.dataSurface->TotIntConvCoeff;
        }
        if (Alphas(2) == "OUTSIDE") {
            ++state.dataSurface->TotExtConvCoeff;
        }
        if (Alphas(6) == "OUTSIDE") {
            ++state.dataSurface->TotExtConvCoeff;
        }
        if (NumAlphas >= 2 && state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            ShowWarningError(state,
                             "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                 Alphas(1));
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(2) + " is blank and rest of fields will not be processed.");
        }
        if (NumAlphas >= 6 && state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            ShowWarningError(state,
                             "GetUserConvectionCoefficients: " + CurrentModuleObject + ", for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                 Alphas(1));
            ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(6) + " is blank and rest of fields will not be processed.");
        }
    }

    state.dataSurface->UserIntConvectionCoeffs.allocate(state.dataSurface->TotIntConvCoeff);
    state.dataSurface->UserExtConvectionCoeffs.allocate(state.dataSurface->TotExtConvCoeff);

    state.dataSurface->TotIntConvCoeff = 0;
    state.dataSurface->TotExtConvCoeff = 0;

    //   Now, get for real and check for consistency
    CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    for (int Loop = 1; Loop <= Count; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        Found = UtilityRoutines::FindItemInList(Alphas(1), Surface);
        if (Found == 0) {
            ShowSevereError(state,
                            "GetUserConvectionCoefficients: " + CurrentModuleObject + ", illegal value for " +
                                state.dataIPShortCut->cAlphaFieldNames(1) + '=' + Alphas(1));
            ErrorsFound = true;
            continue;
        }

        Ptr = 2;
        FieldNo = 2;
        NumField = 1;
        for (Pass = 1; Pass <= 2; ++Pass) {

            {
                auto const SELECT_CASE_var(Alphas(Ptr));
                if (SELECT_CASE_var == "OUTSIDE") {
                    if (Surface(Found).OSCPtr > 0) {
                        ShowSevereError(state,
                                        "GetUserSuppliedConvectionCoefficients: " + CurrentModuleObject + ", OUTSIDE " + CurrentModuleObject +
                                            " cannot be specified for OtherSideCoefficient Surface=" + Alphas(1));
                        ErrorsFound = true;
                    }
                    ExtValue = 0;
                    PotentialAssignedValue = 0;
                    std::string equationName = Alphas(Ptr + 1);
                    if (HcExt_ConvectionTypesMap.find(equationName) != HcExt_ConvectionTypesMap.end()) {
                        ExtValue = HcExt_ConvectionTypesMap.at(equationName);

                        if ((equationName == "SIMPLECOMBINED") || (equationName == "TARP") || (equationName == "MOWITT") ||
                            (equationName == "DOE-2") || (equationName == "ADAPTIVECONVECTIONALGORITHM")) {
                            PotentialAssignedValue = -ExtValue;
                        } else if (equationName == "VALUE") {
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = Found;
                            if (Numbers(NumField) < state.dataHeatBal->LowHConvLimit || Numbers(NumField) > state.dataHeatBal->HighHConvLimit) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", out of range value");
                                ShowContinueError(state,
                                                  format("{}={}, {}=[{:.5R}].",
                                                         state.dataIPShortCut->cAlphaFieldNames(Ptr),
                                                         Alphas(Ptr),
                                                         state.dataIPShortCut->cNumericFieldNames(NumField),
                                                         Numbers(NumField)));
                                ShowContinueError(state,
                                                  format("Out-of-range from low/high limits=[>={:.9R}, <={:.1R}].",
                                                         state.dataHeatBal->LowHConvLimit,
                                                         state.dataHeatBal->HighHConvLimit));
                                ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
                                ErrorsFound = true;
                            }
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefValue;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideValue = Numbers(NumField);
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(Ptr + 2)) {
                                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", duplicate value");
                                ShowContinueError(state,
                                                  "Since VALUE is used for \"" + state.dataIPShortCut->cAlphaFieldNames(FieldNo + 2) + "\", " +
                                                      state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + '=' + Alphas(Ptr + 2) + " is ignored.");
                            }
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                        } else if (equationName == "SCHEDULE") { // Schedule
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefSchedule;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleIndex =
                                GetScheduleIndex(state, Alphas(Ptr + 2));
                            if (state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + " entered=" + Alphas(Ptr + 2));
                                ErrorsFound = true;
                            } else {
                                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleName = Alphas(Ptr + 2);
                            }
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                        } else if (ExtValue == HcExt_UserCurve) { // User curve
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefUserCurve;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).UserCurveIndex =
                                UtilityRoutines::FindItemInList(Alphas(Ptr + 3), state.dataConvectionCoefficient->HcOutsideUserCurve);
                            if (state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).UserCurveIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 3) + " entered=" + Alphas(Ptr + 3));
                                ErrorsFound = true;
                            }
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                        } else if (ExtValue > HcExt_UserCurve) {
                            // specificmodel
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefSpecifiedModel;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).HcModelEq = ExtValue;
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;

                        } else {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", check input");
                            ShowContinueError(state, "Check Input Entered :" + Alphas(Ptr + 1));
                            ErrorsFound = true;
                        }
                        if (state.dataSurface->SurfExtConvCoeffIndex(Found) != 0) {
                            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                            ShowContinueError(state, "Duplicate (Outside) assignment attempt");
                            ErrorsFound = true;
                        } else {
                            state.dataSurface->SurfExtConvCoeffIndex(Found) = PotentialAssignedValue;
                        }
                    }

                } else if (SELECT_CASE_var == "INSIDE") {
                    IntValue = 0;
                    PotentialAssignedValue = 0;
                    std::string equationName = Alphas(Ptr + 1);
                    if (HcInt_ConvectionTypesMap.find(equationName) != HcInt_ConvectionTypesMap.end()) {
                        IntValue = HcInt_ConvectionTypesMap.at(equationName);
                        if ((equationName == "SIMPLE") || (equationName == "TARP") || (equationName == "ADAPTIVECONVECTIONALGORITHM") ||
                            (equationName == "ASTMC1340")) {
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", -IntValue);
                        } else if (equationName == "VALUE") {
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = Found;
                            if (Numbers(NumField) < state.dataHeatBal->LowHConvLimit || Numbers(NumField) > state.dataHeatBal->HighHConvLimit) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", out of range value");
                                ShowContinueError(state,
                                                  format("{}={}, {}=[{:.5R}].",
                                                         state.dataIPShortCut->cAlphaFieldNames(Ptr),
                                                         Alphas(Ptr),
                                                         state.dataIPShortCut->cNumericFieldNames(NumField),
                                                         Numbers(NumField)));
                                ShowContinueError(state,
                                                  format("Out-of-range from low/high limits=[>={:.9R}, <={:.1R}].",
                                                         state.dataHeatBal->LowHConvLimit,
                                                         state.dataHeatBal->HighHConvLimit));
                                ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
                                ErrorsFound = true;
                            }
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefValue;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideValue = Numbers(NumField);
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(Ptr + 2)) {
                                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", duplicate value");
                                ShowContinueError(state,
                                                  "Since VALUE is used for \"" + state.dataIPShortCut->cAlphaFieldNames(FieldNo + 1) + "\", " +
                                                      state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + '=' + Alphas(Ptr + 2) + " is ignored.");
                            }
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;
                        } else if (equationName == "SCHEDULE") {
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefSchedule;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleIndex =
                                GetScheduleIndex(state, Alphas(Ptr + 2));
                            if (state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + " entered=" + Alphas(Ptr + 2));
                                ErrorsFound = true;
                            } else {
                                state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleName = Alphas(Ptr + 2);
                            }
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;
                        } else if (IntValue == HcInt_UserCurve) {
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefUserCurve;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).UserCurveIndex =
                                UtilityRoutines::FindItemInList(Alphas(Ptr + 3), state.dataConvectionCoefficient->HcInsideUserCurve);
                            if (state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).UserCurveIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 3) + " entered=" + Alphas(Ptr + 3));
                                ErrorsFound = true;
                            }
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;
                        } else if (IntValue > HcInt_UserCurve) {
                            // specificmodel
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(1);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = Found;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefSpecifiedModel;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).HcModelEq = IntValue;
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;

                        } else {
                            // treat CeilingDiffuser and TrombeWall special
                            if (UtilityRoutines::SameString(Alphas(Ptr + 1), "CEILINGDIFFUSER") ||
                                UtilityRoutines::SameString(Alphas(Ptr + 1), "TROMBEWALL")) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  "Invalid Value Entered, for " + state.dataIPShortCut->cAlphaFieldNames(Ptr) + '=' + Alphas(Ptr));
                                ShowContinueError(state,
                                                  "invalid value in " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 1) + '=' + Alphas(Ptr + 1) +
                                                      "\". This type is only applicable at a Zone level.");
                                ErrorsFound = true;
                            } else { // really invalid
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  "Invalid Value Entered, for " + state.dataIPShortCut->cAlphaFieldNames(Ptr) + '=' + Alphas(Ptr));
                                ShowContinueError(state,
                                                  "invalid value in " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 1) + '=' + Alphas(Ptr + 1));
                                ErrorsFound = true;
                            }
                        }
                    }
                    if (state.dataSurface->SurfIntConvCoeffIndex(Found) != 0) {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", duplicate (inside)");
                        ShowContinueError(state, "Duplicate (Inside) assignment attempt.");
                        ErrorsFound = true;
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(Found) = PotentialAssignedValue;
                    }

                } else if (SELECT_CASE_var == std::string()) { // Blank

                } else {
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                    ShowContinueError(state, "Invalid Value Entered, for " + state.dataIPShortCut->cAlphaFieldNames(Ptr) + '=' + Alphas(Ptr));
                    ErrorsFound = true;
                }
            }

            Ptr += 4;
            FieldNo += 4;
            ++NumField;
        }
    }

    CurrentModuleObject = "SurfaceProperty:ConvectionCoefficients:MultipleSurface";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    for (int Loop = 1; Loop <= Count; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Loop,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        // Check Field 1 for validity
        if (ValidSurfaceTypes.find(Alphas(1)) == ValidSurfaceTypes.end()) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
            ShowContinueError(state, "illegal value for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' + Alphas(1));
            ErrorsFound = true;
        }
        Ptr = 2;
        FieldNo = 2;
        NumField = 1;
        for (Pass = 1; Pass <= 2; ++Pass) {

            {
                auto const SELECT_CASE_var(Alphas(Ptr));
                if (SELECT_CASE_var == "OUTSIDE") {
                    std::string equationName = Alphas(Ptr + 1);
                    if (HcExt_ConvectionTypesMap.find(equationName) != HcExt_ConvectionTypesMap.end()) {
                        ExtValue = HcExt_ConvectionTypesMap.at(equationName);
                        if (equationName == "SIMPLE" || equationName == "TARP" || equationName == "MOWITT" || equationName == "DOE-2" ||
                            equationName == "ADAPTIVECONVECTIONALGORITHM") {
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", -ExtValue);
                        } else if (equationName == "VALUE") {
                            // SimpleValueAssignment via UserExtConvectionCoeffs array
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = -999;
                            if (Numbers(NumField) < state.dataHeatBal->LowHConvLimit || Numbers(NumField) > state.dataHeatBal->HighHConvLimit) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", out of range value");
                                ShowContinueError(state,
                                                  format("{}={}, {}=[{:.5R}].",
                                                         state.dataIPShortCut->cAlphaFieldNames(Ptr),
                                                         Alphas(Ptr),
                                                         state.dataIPShortCut->cNumericFieldNames(NumField),
                                                         Numbers(NumField)));
                                ShowContinueError(state,
                                                  format("Out-of-range from low/high limits=[>={:.9R}, <={:.1R}].",
                                                         state.dataHeatBal->LowHConvLimit,
                                                         state.dataHeatBal->HighHConvLimit));
                                ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
                                ErrorsFound = true;
                            }
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefValue;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideValue = Numbers(NumField);
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(Ptr + 2)) {
                                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", duplicate value");
                                ShowContinueError(state,
                                                  "Since VALUE is used for \"" + state.dataIPShortCut->cAlphaFieldNames(FieldNo + 2) + "\", " +
                                                      state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + '=' + Alphas(Ptr + 2) + " is ignored.");
                            }
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", state.dataSurface->TotExtConvCoeff);
                        } else if (equationName == "SCHEDULE") {
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefSchedule;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleIndex =
                                GetScheduleIndex(state, Alphas(Ptr + 2));
                            if (state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + " entered=" + Alphas(Ptr + 2));
                                ErrorsFound = true;
                            } else {
                                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).ScheduleName = Alphas(Ptr + 2);
                            }
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", state.dataSurface->TotExtConvCoeff);
                        } else if (ExtValue == HcExt_UserCurve) { // User curve
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefUserCurve;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).UserCurveIndex =
                                UtilityRoutines::FindItemInList(Alphas(Ptr + 3), state.dataConvectionCoefficient->HcOutsideUserCurve);
                            if (state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).UserCurveIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 3) + " entered=" + Alphas(Ptr + 3));
                                ErrorsFound = true;
                            }
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", state.dataSurface->TotExtConvCoeff);

                        } else if (ExtValue > HcExt_UserCurve) {
                            // specificmodel
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefSpecifiedModel;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).HcModelEq = ExtValue;
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", state.dataSurface->TotExtConvCoeff);
                        } else {
                            ++state.dataSurface->TotExtConvCoeff;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).OverrideType = ConvCoefSpecifiedModel;
                            state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->TotExtConvCoeff).HcModelEq = ExtValue;
                            PotentialAssignedValue = state.dataSurface->TotExtConvCoeff;
                            ApplyConvectionValue(state, Alphas(1), "OUTSIDE", state.dataSurface->TotExtConvCoeff);
                        }
                    } else {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", check input");
                        ShowContinueError(state, "Check Input Entered :" + Alphas(Ptr + 1));
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "INSIDE") {
                    std::string equationName = Alphas(Ptr + 1);
                    if (HcInt_ConvectionTypesMap.find(equationName) != HcInt_ConvectionTypesMap.end()) {
                        IntValue = HcInt_ConvectionTypesMap.at(equationName);
                        if ((equationName == "SIMPLE") || (equationName == "TARP") ||
                            (equationName == "ADAPTIVECONVECTIONALGORITHM" || (equationName == "ASTMC1340"))) {
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", -IntValue);
                        } else if (equationName == "VALUE") {
                            // SimpleValueAssignment via UserExtConvectionCoeffs array
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = -999;
                            if (Numbers(NumField) < state.dataHeatBal->LowHConvLimit || Numbers(NumField) > state.dataHeatBal->HighHConvLimit) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", out of range value");
                                ShowContinueError(state,
                                                  format("{}={}, {}=[{:.5R}].",
                                                         state.dataIPShortCut->cAlphaFieldNames(Ptr),
                                                         Alphas(Ptr),
                                                         state.dataIPShortCut->cNumericFieldNames(NumField),
                                                         Numbers(NumField)));
                                ShowContinueError(state,
                                                  format("Out-of-range from low/high limits=[>={:.9R}, <={:.1R}].",
                                                         state.dataHeatBal->LowHConvLimit,
                                                         state.dataHeatBal->HighHConvLimit));
                                ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
                                ErrorsFound = true;
                            }
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefValue;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideValue = Numbers(NumField);
                            if (!state.dataIPShortCut->lAlphaFieldBlanks(Ptr + 2)) {
                                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", duplicate value");
                                ShowContinueError(state,
                                                  "Since VALUE is used for \"" + state.dataIPShortCut->cAlphaFieldNames(FieldNo + 2) + "\", " +
                                                      state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + '=' + Alphas(Ptr + 2) + " is ignored.");
                            }
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", state.dataSurface->TotIntConvCoeff);
                        } else if (equationName == "SCHEDULE") {
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefSchedule;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleIndex =
                                GetScheduleIndex(state, Alphas(Ptr + 2));
                            if (state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleIndex == 0) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 2) + " entered=" + Alphas(Ptr + 2));
                                ErrorsFound = true;
                            } else {
                                state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).ScheduleName = Alphas(Ptr + 2);
                            }
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", state.dataSurface->TotIntConvCoeff);
                        } else if (IntValue == HcInt_UserCurve) {
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefUserCurve;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).UserCurveIndex =
                                UtilityRoutines::FindItemInList(Alphas(Ptr + 3), state.dataConvectionCoefficient->HcInsideUserCurve);
                            if (state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).UserCurveIndex == 0) {

                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 3) + " entered=" + Alphas(Ptr + 3));
                                ErrorsFound = true;
                            }
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", state.dataSurface->TotIntConvCoeff);
                        } else if (IntValue > HcInt_UserCurve) {
                            // specificmodel
                            ++state.dataSurface->TotIntConvCoeff;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).SurfaceName = Alphas(Ptr);
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).WhichSurface = -999;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).OverrideType = ConvCoefSpecifiedModel;
                            state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->TotIntConvCoeff).HcModelEq = IntValue;
                            PotentialAssignedValue = state.dataSurface->TotIntConvCoeff;
                            ApplyConvectionValue(state, Alphas(1), "INSIDE", state.dataSurface->TotIntConvCoeff);

                        } else {
                            // treat CeilingDiffuser and TrombeWall special
                            if (UtilityRoutines::SameString(Alphas(Ptr + 1), "CEILINGDIFFUSER") ||
                                UtilityRoutines::SameString(Alphas(Ptr + 1), "TROMBEWALL")) {
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state, " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr) + " entered=" + Alphas(Ptr));
                                ShowContinueError(state,
                                                  "invalid value in " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 1) + '=' + Alphas(Ptr + 1) +
                                                      "\". This type is only applicable at a Zone level.");
                                ErrorsFound = true;
                            } else { // really invalid
                                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                                ShowContinueError(state,
                                                  " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr + 1) + " entered=" + Alphas(Ptr + 1));
                                ErrorsFound = true;
                            }
                        }
                    }
                } else if (SELECT_CASE_var == std::string()) { // Blank

                } else { // Error Case
                    ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + ", invalid value");
                    ShowContinueError(state, " Invalid " + state.dataIPShortCut->cAlphaFieldNames(Ptr) + " entered=" + Alphas(Ptr));
                    ErrorsFound = true;
                }
            }

            Ptr += 4;
            FieldNo += 4;
            ++NumField;
        }
    }

    for (int Loop = 1; Loop <= state.dataSurface->TotIntConvCoeff; ++Loop) {
        if (state.dataSurface->UserIntConvectionCoeffs(Loop).OverrideType != ConvCoefSchedule) continue;
        if (state.dataSurface->UserIntConvectionCoeffs(Loop).ScheduleIndex == 0) continue;
        if (CheckScheduleValueMinMax(state,
                                     state.dataSurface->UserIntConvectionCoeffs(Loop).ScheduleIndex,
                                     ">=",
                                     state.dataHeatBal->LowHConvLimit,
                                     "<=",
                                     state.dataHeatBal->HighHConvLimit))
            continue;
        ShowSevereError(state,
                        std::string{RoutineName} + "Surface=\"" + state.dataSurface->UserIntConvectionCoeffs(Loop).SurfaceName +
                            "\", out-of-range convection coefficient:");
        ShowContinueError(state, "Out-of-range value found in schedule=" + state.dataSurface->UserIntConvectionCoeffs(Loop).ScheduleName);
        ShowContinueError(state,
                          format("User supplied convection coefficients must be in range [>={:.9R}, <={:.1R}]",
                                 state.dataHeatBal->LowHConvLimit,
                                 state.dataHeatBal->HighHConvLimit));
        ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
        ErrorsFound = true;
    }

    for (int Loop = 1; Loop <= state.dataSurface->TotExtConvCoeff; ++Loop) {
        if (state.dataSurface->UserExtConvectionCoeffs(Loop).OverrideType != ConvCoefSchedule) continue;
        if (state.dataSurface->UserExtConvectionCoeffs(Loop).ScheduleIndex == 0) continue;
        if (CheckScheduleValueMinMax(state,
                                     state.dataSurface->UserExtConvectionCoeffs(Loop).ScheduleIndex,
                                     ">=",
                                     state.dataHeatBal->LowHConvLimit,
                                     "<=",
                                     state.dataHeatBal->HighHConvLimit))
            continue;
        ShowSevereError(state,
                        std::string{RoutineName} + "Surface=\"" + state.dataSurface->UserExtConvectionCoeffs(Loop).SurfaceName +
                            "\", out-of-range convection coefficient:");
        ShowContinueError(state, "Out-of-range value found in schedule=" + state.dataSurface->UserExtConvectionCoeffs(Loop).ScheduleName);
        ShowContinueError(state,
                          format("User supplied convection coefficients must be in range [>={:.9R}, <={:.1R}]",
                                 state.dataHeatBal->LowHConvLimit,
                                 state.dataHeatBal->HighHConvLimit));
        ShowContinueError(state, "Limits are set (or default) in HeatBalanceAlgorithm object.");
        ErrorsFound = true;
    }

    if (state.dataHeatBal->DefaultOutsideConvectionAlgo == ASHRAESimple ||
        std::any_of(
            Zone.begin(), Zone.end(), [](DataHeatBalance::ZoneData const &e) { return e.OutsideConvectionAlgo == DataHeatBalance::ASHRAESimple; })) {
        Count = 0;
        for (int Loop = 1; Loop <= state.dataSurface->TotExtConvCoeff; ++Loop) {
            SurfNum = state.dataSurface->UserExtConvectionCoeffs(Loop).WhichSurface;
            // Tests show that Zone will override the simple convection specification of global.
            if (SurfNum <= 0) continue;               // ignore this error condition
            if (Surface(SurfNum).Zone == 0) continue; // ignore this error condition
            if (Zone(Surface(SurfNum).Zone).OutsideConvectionAlgo == ASHRAESimple &&
                ((state.dataSurface->UserExtConvectionCoeffs(Loop).OverrideType == ConvCoefSpecifiedModel &&
                  state.dataSurface->UserExtConvectionCoeffs(Loop).HcModelEq != ASHRAESimple) ||
                 state.dataSurface->UserExtConvectionCoeffs(Loop).OverrideType != ConvCoefSpecifiedModel)) {
                ++Count;
                if (state.dataGlobal->DisplayExtraWarnings) {
                    ShowSevereError(
                        state, std::string{RoutineName} + "Surface=\"" + state.dataSurface->UserExtConvectionCoeffs(Loop).SurfaceName + "\", mixed algorithms.");
                    ShowContinueError(
                        state, "Zone Outside Convection Algorithm specifies \"SimpleCombined\". SimpleCombined will be used for this surface.");
                }
            }
        }
        if (Count > 0) {
            ShowSevereMessage(state, std::string{RoutineName} + format("{} surfaces had different outside convection algorithms specified when", Count));
            ShowContinueError(state,
                              "the Zone Outside Convection Algorithm specifies \"SimpleCombined\". SimpleCombined will be used for these surfaces.");
            if (!state.dataGlobal->DisplayExtraWarnings) {
                ShowContinueError(state, "Use OutputDiagnostics,DisplayExtraWarnings; to see specific instances.");
                state.dataErrTracking->TotalSevereErrors += Count;
            }
        }
    }

    // get SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections

    CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    // IF (Count > 1) ! throw  error ... TODO or IP handles it
    if (Count == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.Name = state.dataIPShortCut->cAlphaArgs(1); // not used by E+, unique object
        state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.EnteredByUser = true;

        // The following array maps the inputs for the SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections algorithm input fields
        // to the corresponding defaults by making a pair with a pointer to the InsideFaceAdaptiveConvectionAlgo algorithm int parameter
        // to the default int value
        std::array<int *const, 45> AdaptiveConvectionAlgoInsideDefaults = {
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum,
            &state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum};
        for (int i = 2; i <= NumAlphas - 1; i += 2) { // up to 45
            static constexpr std::string_view RoutineName = "GetUserConvectionCoefficients";
            static constexpr std::string_view CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections";
            ErrorsFound = SetAdaptiveConvectionAlgoCoefficient(state,
                                                               HcInt_ConvectionTypesMap,
                                                               AdaptiveConvectionAlgoInsideDefaults[(i / 2) - 1],
                                                               state.dataIPShortCut->cAlphaArgs(i),
                                                               state.dataIPShortCut->cAlphaArgs(i + 1),
                                                               state.dataIPShortCut->cAlphaFieldNames(i),
                                                               state.dataIPShortCut->cAlphaFieldNames(i + 1),
                                                               RoutineName,
                                                               CurrentModuleObject);
        }
    } // end of 'SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections'

    CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections";
    Count = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    // IF (Count > 1) ! throw  error ... TODO or IP handles it
    if (Count == 1) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 1,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 Status,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.Name =
            state.dataIPShortCut->cAlphaArgs(1); // not used by E+, unique object
        state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.EnteredByUser = true;
        std::array<int *const, 6> AdaptiveConvectionAlgoOutsideDefaults = {
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum,
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum,
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum,
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum,
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum,
            &state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum,
        };

        for (int i = 2; i <= NumAlphas - 1; i += 2) {
            static constexpr std::string_view RoutineName = "GetUserConvectionCoefficients";
            static constexpr std::string_view CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections";
            ErrorsFound = SetAdaptiveConvectionAlgoCoefficient(state,
                                                               HcExt_ConvectionTypesMap,
                                                               AdaptiveConvectionAlgoOutsideDefaults[(i / 2) - 1],
                                                               state.dataIPShortCut->cAlphaArgs(i),
                                                               state.dataIPShortCut->cAlphaArgs(i + 1),
                                                               state.dataIPShortCut->cAlphaFieldNames(i),
                                                               state.dataIPShortCut->cAlphaFieldNames(i + 1),
                                                               RoutineName,
                                                               CurrentModuleObject);
        }
    } // end of 'SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections'

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found getting input.  Program termination.");
    }

    SetupAdaptiveConvectionStaticMetaData(state);
}

void ApplyConvectionValue(EnergyPlusData &state, std::string const &SurfaceTypes, std::string const &ConvectionType, int const Value)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine applies a convection type to a set of surfaces.  This is
    // one of the "regular" convection types and becomes a "negative" convection
    // type to that surface.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SurfNum;
    bool SurfacesOfType;
    int SurfaceCountOutside;
    int SurfaceCountInside;
    std::string OverwriteMessage;

    auto &Surface(state.dataSurface->Surface);

    {
        auto const SELECT_CASE_var(SurfaceTypes);

        if (SELECT_CASE_var == "ALLEXTERIORSURFACES") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLEXTERIORWINDOWS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLEXTERIORWALLS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLEXTERIORROOFS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Roof) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLEXTERIORFLOORS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond > 0) continue; // Interior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Floor) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLINTERIORSURFACES") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLINTERIORWINDOWS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLINTERIORWALLS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if ((SELECT_CASE_var == "ALLINTERIORROOFS") || (SELECT_CASE_var == "ALLINTERIORCEILINGS")) {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Roof) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else if (SELECT_CASE_var == "ALLINTERIORFLOORS") {
            SurfacesOfType = false;
            SurfaceCountOutside = 0;
            SurfaceCountInside = 0;
            for (SurfNum = 1; SurfNum <= state.dataSurface->TotSurfaces; ++SurfNum) {
                if (!Surface(SurfNum).HeatTransSurf) continue;
                if (Surface(SurfNum).ExtBoundCond <= 0) continue; // Exterior surfaces
                if (Surface(SurfNum).Class != SurfaceClass::Floor) continue;
                SurfacesOfType = true;
                if (ConvectionType == "OUTSIDE") {
                    if (Surface(SurfNum).OSCPtr > 0) continue;
                    if (state.dataSurface->SurfExtConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Outside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountOutside;
                        }
                    } else {
                        state.dataSurface->SurfExtConvCoeffIndex(SurfNum) = Value;
                    }
                } else {
                    if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            ShowWarningError(state,
                                             "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                                 "\", not overwriting already assigned value for (Inside) in Surface=" + Surface(SurfNum).Name);
                        } else {
                            ++SurfaceCountInside;
                        }
                    } else {
                        state.dataSurface->SurfIntConvCoeffIndex(SurfNum) = Value;
                    }
                }
            }
            if (!state.dataGlobal->DisplayExtraWarnings && (SurfaceCountOutside > 0 || SurfaceCountInside > 0)) {
                if (SurfaceCountOutside > 0) {
                    OverwriteMessage = format("{} Outside", SurfaceCountOutside);
                }
                if (SurfaceCountInside > 0) {
                    OverwriteMessage = format("{} Inside", SurfaceCountInside);
                }
                ShowWarningError(state,
                                 "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                                     "\", not overwriting already assigned values for " + OverwriteMessage + " assignments.");
            }

        } else {
            SurfacesOfType = false;
        }
    }

    if (!SurfacesOfType) {
        ShowWarningError(state,
                         "User Supplied Convection Coefficients, Multiple Surface Assignments=\"" + SurfaceTypes +
                             "\", there were no surfaces of that type found for " + ConvectionType + " assignment.");
    }
}

Real64 CalcASHRAESimpExtConvectCoeff(int const Roughness,       // Integer index for roughness, relates to parameter array indices
                                     Real64 const SurfWindSpeed // Current wind speed, m/s
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the exterior convection coefficient
    // using the ASHRAE Simple Method from a correlation from Figure 1
    // on p. 22.4 of the 1989 ASHRAE Handbook of Fundamentals.
    // This is a combined coefficient that includes radiation to sky, ground, and air.

    // METHODOLOGY EMPLOYED:
    // Apply the correlation based on the input data.

    // REFERENCES:
    // ASHRAE Handbook of Fundamentals 1989, p.22.4

    // Return value
    Real64 CalcASHRAESimpExtConvectCoeff;

    // FUNCTION PARAMETER DEFINITIONS:
    static Array1D<Real64> const D(6, {11.58, 12.49, 10.79, 8.23, 10.22, 8.23});
    static Array1D<Real64> const E(6, {5.894, 4.065, 4.192, 4.00, 3.100, 3.33});
    static Array1D<Real64> const F(6, {0.0, 0.028, 0.0, -0.057, 0.0, -0.036});

    CalcASHRAESimpExtConvectCoeff = D(Roughness) + E(Roughness) * SurfWindSpeed + F(Roughness) * pow_2(SurfWindSpeed);

    return CalcASHRAESimpExtConvectCoeff;
}

Real64 CalcASHRAESimpleIntConvCoeff(Real64 const Tsurf, Real64 const Tamb, Real64 const cosTilt)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the interior convection coefficient for a surface.

    // METHODOLOGY EMPLOYED:
    // The convection coefficients are taken directly from the TARP Reference Manual.  TARP calculated
    // its coefficients using the surface conductances for e=0.9 found in ASHRAE Handbook of Fundamentals
    // 1985 in Table 1 on p. 23.2, but subtracted off the radiative component which was estimated at
    // 1.02 * 0.9 = 0.918 BTU/h-ft2-F.  Coefficients were then converted to SI units to yield the values
    // in this subroutine.

    // REFERENCES:
    // 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
    //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79.
    // 2.  ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1.

    //      +---------------------+-----------+---------------------------------------------+------------------+-----------------+-------------+
    //      |      Situation      | DeltaTemp |                   CosTilt                   | cos(tilt)*deltaT | Convection Type | Coefficient |
    //      +---------------------+-----------+---------------------------------------------+------------------+-----------------+-------------+
    //      | Vertical Surface    | N/A       | -0.3827 to 0.3827 (67.5 to 112.5 degrees)   | N/A              | Normal          |       3.076 |
    //      | Horizontal Surface  | Positive  | 0.9238 to 1.0 (0 to 22.5 degrees)           | Positive         | Enhanced        |       4.043 |
    //      | Horizontal Surface  | Positive  | -0.9238 to -1.0 (157.5 to 180 degrees)      | Negative         | Reduced         |       0.948 |
    //      | Horizontal Surface  | Negative  | 0.9239 to 1.0 (0 to 22.5 degrees)           | Negative         | Reduced         |       0.948 |
    //      | Horizontal Surface  | Negative  | -0.9239 to -1.0 (157.5 to 180 degrees)      | Positive         | Enhanced        |       4.040 |
    //      | Tilted Surface      | Positive  | 0.3827 to 0.9239 (22.5 to 67.5 degrees)     | Positive         | Enhanced        |       3.870 |
    //      | Tilted Surface      | Negative  | -0.3827 to -0.9239 (157.5 to 157.5 degrees) | Positive         | Enhanced        |       3.870 |
    //      | Tilted Surface      | Negative  | 0.3827 to 0.9239 (22.5 to 67.5 degrees)     | Negative         | Reduced         |       2.281 |
    //      | Tilted Surface      | Positive  | -0.3827 to -0.9239 (157.5 to 157.5 degrees) | Negative         | Reduced         |       2.281 |
    //      +---------------------+-----------+---------------------------------------------+------------------+-----------------+-------------+

    // Set HConvIn using the proper correlation based on DeltaTemp and Cosine of the Tilt of the Surface
    if (std::abs(cosTilt) < 0.3827) { // Vertical Surface
        return 3.076;
    } else {
        Real64 DeltaTempCosTilt = (Tamb - Tsurf) * cosTilt;
        if (std::abs(cosTilt) >= 0.9239) { // Horizontal Surface
            if (DeltaTempCosTilt > 0.0) {  // Enhanced Convection
                return 4.040;
            } else if (DeltaTempCosTilt < 0.0) { // Reduced Convection
                return 0.948;
            } else { // Zero DeltaTemp
                return 3.076;
            }
        } else {                          // tilted surface
            if (DeltaTempCosTilt > 0.0) { // Enhanced Convection
                return 3.870;
            } else if (DeltaTempCosTilt < 0.0) { // Reduced Convection
                return 2.281;
            } else { // Zero DeltaTemp
                return 3.076;
            }
        }
    }
}

void CalcASHRAESimpleIntConvCoeff(EnergyPlusData &state,
                                  int const SurfNum,                  // surface number for which coefficients are being calculated
                                  Real64 const SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                                  Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
)
{
    auto &Surface(state.dataSurface->Surface);
    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = [](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
            return CalcASHRAESimpleIntConvCoeff(Tsurf, Tamb, cosTilt);
        };
    } else {
        state.dataHeatBal->HConvIn(SurfNum) = CalcASHRAESimpleIntConvCoeff(SurfaceTemperature, ZoneMeanAirTemperature, Surface(SurfNum).CosTilt);
    }

    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    state.dataHeatBal->HConvIn(SurfNum) = max(state.dataHeatBal->HConvIn(SurfNum), state.dataHeatBal->LowHConvLimit);
}

Real64 CalcASHRAETARPNatural(Real64 const Tsurf, Real64 const Tamb, Real64 const cosTilt)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the convection coefficient for a surface.

    // NOTE:
    // Because surface tilts are given with respect to the outward normal, applications for interior
    // surfaces should use a negative cos(Tilt).

    // METHODOLOGY EMPLOYED:
    // The algorithm for convection coefficients is taken directly from the TARP Reference Manual.
    // ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5 gives equations for natural convection
    // heat transfer coefficients in the turbulent range for large, vertical plates and for large,
    // horizontal plates facing upward when heated (or downward when cooled).  A note in the text
    // also gives an approximation for large, horizontal places facing downward when heated (or
    // upward when cooled) recommending that it should be half of the facing upward value.
    // TARP then adds a curve fit as a function of the cosine of the tilt angle to provide intermediate
    // values between vertical and horizontal.  The curve fit values at the extremes match the ASHRAE
    // values very well.

    // REFERENCES:
    // 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
    //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.
    // 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

    Real64 DeltaTemp = Tsurf - Tamb;

    // Set HConvIn using the proper correlation based on DeltaTemp and Surface (Cosine Tilt)

    if ((DeltaTemp == 0.0) || (cosTilt == 0.0)) { // Vertical Surface

        return CalcASHRAEVerticalWall(DeltaTemp);

    } else if (((DeltaTemp < 0.0) && (cosTilt < 0.0)) || ((DeltaTemp > 0.0) && (cosTilt > 0.0))) { // Enhanced Convection

        return CalcWaltonUnstableHorizontalOrTilt(DeltaTemp, cosTilt);

    } else /*if (((DeltaTemp > 0.0) && (cosTilt < 0.0)) ||
               ((DeltaTemp < 0.0) && (cosTilt > 0.0)))*/
    {      // Reduced Convection

        return CalcWaltonStableHorizontalOrTilt(DeltaTemp, cosTilt);

    } // ...end of IF-THEN block to set HConvIn
}

void CalcASHRAEDetailedIntConvCoeff(EnergyPlusData &state,
                                    int const SurfNum,                  // surface number for which coefficients are being calculated
                                    Real64 const SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                                    Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
)
{
    auto &Surface(state.dataSurface->Surface);
    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = [](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
            return CalcASHRAETARPNatural(Tsurf, Tamb, cosTilt);
        };
    } else {
        state.dataHeatBal->HConvIn(SurfNum) = CalcASHRAETARPNatural(
            SurfaceTemperature, ZoneMeanAirTemperature, -Surface(SurfNum).CosTilt); // negative CosTilt because CosTilt is relative to exterior
    }

    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
        state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
}

void CalcDetailedHcInForDVModel(EnergyPlusData &state,
                                int const SurfNum,                          // surface number for which coefficients are being calculated
                                const Array1D<Real64> &SurfaceTemperatures, // Temperature of surfaces for evaluation of HcIn
                                Array1D<Real64> &HcIn,                      // Interior Convection Coeff Array
                                Optional<Array1S<Real64> const> Vhc         // Velocity array for forced convection coeff calculation
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       Used for DV model; Feb 2004, LKL
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the interior convection coefficient for a surface.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TAirConv;
    Real64 Hf;
    auto &Surface(state.dataSurface->Surface);

    if (Surface(SurfNum).HeatTransSurf) { // Only treat heat transfer surfaces

        // UCSD
        {
            auto const SELECT_CASE_var(state.dataSurface->SurfTAirRef(SurfNum));
            if (SELECT_CASE_var == AdjacentAirTemp) {
                TAirConv = state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
            } else {
                // currently set to mean air temp but should add error warning here
                TAirConv = state.dataHeatBalFanSys->MAT(Surface(SurfNum).Zone);
            }
        }

        if (state.dataRoomAirMod->AirModel(Surface(SurfNum).Zone).AirModelType == DataRoomAirModel::RoomAirModel::UCSDDV ||
            state.dataRoomAirMod->AirModel(Surface(SurfNum).Zone).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFI ||
            state.dataRoomAirMod->AirModel(Surface(SurfNum).Zone).AirModelType == DataRoomAirModel::RoomAirModel::UCSDUFE) {

            // Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
            if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {

                HcIn(SurfNum) = SetIntConvectionCoeff(state, SurfNum);

            } else {
                HcIn(SurfNum) = CalcASHRAETARPNatural(SurfaceTemperatures(SurfNum),
                                                      TAirConv,
                                                      -Surface(SurfNum).CosTilt); // negative CosTilt because CosTilt is relative to exterior
            }

        } else if (state.dataRoomAirMod->AirModel(Surface(SurfNum).Zone).AirModelType == DataRoomAirModel::RoomAirModel::UCSDCV) {

            Hf = 4.3 * Vhc()(Surface(SurfNum).Zone);

            // Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
            if (state.dataSurface->SurfIntConvCoeffIndex(SurfNum) != 0) {

                HcIn(SurfNum) = SetIntConvectionCoeff(state, SurfNum);

            } else {
                HcIn(SurfNum) = CalcASHRAETARPNatural(SurfaceTemperatures(SurfNum),
                                                      TAirConv,
                                                      -Surface(SurfNum).CosTilt); // negative CosTilt because CosTilt is relative to exterior
                HcIn(SurfNum) = std::pow(std::pow(HcIn(SurfNum), 3.2) + std::pow(Hf, 3.2), 1.0 / 3.2);
            }
        }
    }

    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    if (HcIn(SurfNum) < state.dataHeatBal->LowHConvLimit) HcIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
}

Real64 CalcZoneSystemACH(EnergyPlusData &state, int const ZoneNum)
{

    if (!allocated(state.dataLoopNodes->Node)) {
        return 0.0;
    } else {
        // Set local variables
        Real64 ZoneVolume = state.dataHeatBal->Zone(ZoneNum).Volume;
        Real64 ZoneVolFlowRate = CalcZoneSystemVolFlowRate(state, ZoneNum);

        // Calculate ACH
        return ZoneVolFlowRate / ZoneVolume * DataGlobalConstants::SecInHour;
    }
}

Real64 CalcZoneSupplyAirTemp(EnergyPlusData &state, int const ZoneNum)
{
    using namespace DataZoneEquipment;

    int ZoneNode = state.dataHeatBal->Zone(ZoneNum).SystemZoneNodeNumber;
    int thisZoneInletNode = 0;
    if (ZoneNode > 0) {
        Real64 SumMdotTemp = 0.0;
        Real64 SumMdot = 0.0;
        for (int EquipNum = 1;
             EquipNum <= state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).NumOfEquipTypes;
             ++EquipNum) {
            if (state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).EquipData(EquipNum).NumOutlets > 0) {
                thisZoneInletNode = state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .EquipData(EquipNum)
                                        .OutletNodeNums(1);
                if ((thisZoneInletNode > 0) && (state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate > 0.0)) {
                    SumMdotTemp += state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate * state.dataLoopNodes->Node(thisZoneInletNode).Temp;
                    SumMdot += state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate;
                }
            }
        }
        if (SumMdot > 0.0) {
            return SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
        } else {
            if (thisZoneInletNode > 0) {
                return state.dataLoopNodes->Node(thisZoneInletNode).Temp;
            } else {
                return state.dataLoopNodes->Node(ZoneNode).Temp;
            }
        }
    } else {
        return state.dataLoopNodes->Node(ZoneNode).Temp;
    }
}

Real64 CalcZoneSystemVolFlowRate(EnergyPlusData &state, int const ZoneNum)
{
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdpPb;

    auto &Zone(state.dataHeatBal->Zone);

    int ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
    if (!state.dataGlobal->BeginEnvrnFlag && ZoneNode > 0) {
        int ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
        Real64 AirDensity = PsyRhoAirFnPbTdbW(state,
                                              state.dataEnvrn->OutBaroPress,
                                              state.dataLoopNodes->Node(ZoneNode).Temp,
                                              PsyWFnTdpPb(state, state.dataLoopNodes->Node(ZoneNode).Temp, state.dataEnvrn->OutBaroPress));
        return state.dataLoopNodes->Node(ZoneNode).MassFlowRate / (AirDensity * ZoneMult);
    } else {
        return 0.0;
    }
}

Real64 CalcCeilingDiffuserACH(EnergyPlusData &state, int const ZoneNum)
{
    constexpr Real64 MinFlow(0.01); // Minimum mass flow rate
    constexpr Real64 MaxACH(100.0); // Maximum ceiling diffuser correlation limit

    auto &Zone(state.dataHeatBal->Zone);

    Real64 ACH = CalcZoneSystemACH(state, ZoneNum); // Air changes per hour

    Real64 ZoneMassFlowRate;
    Real64 ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
    int ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber; // Zone node as defined in system simulation
    if (!state.dataGlobal->BeginEnvrnFlag && ZoneNode > 0) {
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneNode).MassFlowRate / ZoneMult;
    } else { // because these are not updated yet for new environment
        ZoneMassFlowRate = 0.0;
    }

    if (ZoneMassFlowRate < MinFlow) {
        ACH = 0.0;
    } else {
        // Calculate ACH
        ACH = min(ACH, MaxACH);
        ACH = max(ACH, 0.0);
    }

    return ACH;
}

Real64 CalcCeilingDiffuserIntConvCoeff(EnergyPlusData &state,
                                       Real64 const ACH, // [1/hr] air system air change rate
                                       Real64 const Tsurf,
                                       Real64 const Tair,
                                       Real64 const cosTilt,
                                       Real64 const humRat,
                                       Real64 const height,
                                       bool const isWindow)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the interior convection coefficients
    // for ceiling diffusers correlated to the outlet air temperature.

    // METHODOLOGY EMPLOYED:
    // call functions with the actual model equations

    // REFERENCES:
    // Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
    //       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

    // OTHER NOTES:
    // The correlations shown below differ from (and are less accurate than) those shown
    // in the reference above (Fisher 1997).  They have been reformulated with an outlet
    // temperature reference in order to accomodate the structure of the EnergyPlus code.

    // If the Ceiling Diffuser option is selected the following correlations are used.
    // The development of the ceiling diffuser convection correlations is shown in reference 4.
    // The correlations shown below differ from (and are less accurate than) those shown in reference 4 because they have been
    // reformulated with an outlet temperature reference in order to accomodate the structure of the
    // EnergyPlus code.

    // Set HConvIn using the proper correlation based on Surface Tilt
    static const Real64 cos45(sqrt(2.) / 2.0);

    if (cosTilt < -cos45) {
        return CalcFisherPedersenCeilDiffuserFloor(state, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow); // Floor correlation
    } else if (cosTilt > cos45) {
        return CalcFisherPedersenCeilDiffuserCeiling(state, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow); // Ceiling correlation
    } else {
        return CalcFisherPedersenCeilDiffuserWalls(state, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow); // Wall correlation
    }
}

void CalcCeilingDiffuserIntConvCoeff(EnergyPlusData &state,
                                     int const ZoneNum,
                                     const Array1D<Real64> &SurfaceTemperatures) // zone number for which coefficients are being calculated
{

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    Real64 ACH = CalcCeilingDiffuserACH(state, ZoneNum);

    Real64 AirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);

    for (auto SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
            state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in =
                [=, &state](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                return CalcCeilingDiffuserIntConvCoeff(state,
                                                       ACH,
                                                       Tsurf,
                                                       Tamb,
                                                       cosTilt,
                                                       AirHumRat,
                                                       Surface(SurfNum).Height,
                                                       state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow);
            };
        } else {
            state.dataHeatBal->HConvIn(SurfNum) =
                CalcCeilingDiffuserIntConvCoeff(state,
                                                ACH,
                                                SurfaceTemperatures(SurfNum),
                                                state.dataHeatBalFanSys->MAT(ZoneNum),
                                                Surface(SurfNum).CosTilt,
                                                AirHumRat,
                                                Surface(SurfNum).Height,
                                                state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow);
            // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
            if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
                state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
        }
    } // SurfNum
}

// CalcCeilingDiffuserInletCorr should replace CalcCeilingDiffuser (above), if ZoneTempPredictorCorrector can
// ever be made to work correctly with the inlet air temperature.

void CalcCeilingDiffuserInletCorr(EnergyPlusData &state,
                                  int const ZoneNum,                         // Zone number
                                  const Array1S<Real64> &SurfaceTemperatures // For CalcASHRAEDetailed, if called
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   August 2000
    //       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
    //       MODIFIED       July 2003, (CC) set a flag for reference temperature so that supply air temperature
    //                                      is used as the reference in the inside heat balance calculations

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the interior convection coefficients
    // for ceiling diffusers correlated to the inlet air temperature.

    // REFERENCES:
    // Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
    //   Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

    // Using/Aliasing
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdpPb;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 const MinFlow(0.01); // Minimum mass flow rate
    Real64 const MaxACH(100.0); // Maximum ceiling diffuser correlation limit
    Real64 ACH;                 // Air changes per hour
    int ZoneNode;               // Zone node as defined in system simulation
    Real64 ZoneVolume;          // Zone node as defined in system simulation
    Real64 ZoneMassFlowRate;    // Zone node as defined in system simulation
    Real64 AirDensity;          // zone air density
    int SurfNum;                // DO loop counter for surfaces
    Real64 Tilt;                // Surface tilt
    Real64 ZoneMult;

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    if (state.dataGlobal->SysSizingCalc || state.dataGlobal->ZoneSizingCalc || !allocated(state.dataLoopNodes->Node)) {
        ACH = 0.0;
    } else {
        // Set local variables
        ZoneVolume = Zone(ZoneNum).Volume;
        ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
        ZoneMult = Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier;
        AirDensity = PsyRhoAirFnPbTdbW(state,
                                       state.dataEnvrn->OutBaroPress,
                                       state.dataLoopNodes->Node(ZoneNode).Temp,
                                       PsyWFnTdpPb(state, state.dataLoopNodes->Node(ZoneNode).Temp, state.dataEnvrn->OutBaroPress));
        ZoneMassFlowRate = state.dataLoopNodes->Node(ZoneNode).MassFlowRate / ZoneMult;

        if (ZoneMassFlowRate < MinFlow) {
            ACH = 0.0;
        } else {
            // Calculate ACH
            ACH = ZoneMassFlowRate / AirDensity / ZoneVolume * DataGlobalConstants::SecInHour;
            // Limit ACH to range of correlation
            ACH = min(ACH, MaxACH);
            ACH = max(ACH, 0.0);
        }
    }

    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        if (ACH <= 3.0) { // Use the other convection algorithm
            if (!state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow) {
                CalcASHRAEDetailedIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));
            } else {
                CalcISO15099WindowIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));
            }
        } else { // Use forced convection correlations
            Tilt = Surface(SurfNum).Tilt;

            // assume that reference air temp for user defined convection coefficient is the mean air temperature (=MAT)
            // Calculate the convection coefficient based on inlet (supply) air conditions
            if (Tilt < 45.0) {
                state.dataHeatBal->HConvIn(SurfNum) = 0.49 * std::pow(ACH, 0.8); // Ceiling correlation
            } else if (Tilt > 135.0) {
                state.dataHeatBal->HConvIn(SurfNum) = 0.13 * std::pow(ACH, 0.8); // Floor correlation
            } else {
                state.dataHeatBal->HConvIn(SurfNum) = 0.19 * std::pow(ACH, 0.8); // Wall correlation
            }
            // set flag for reference air temperature
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneSupplyAirTemp;
        }

        // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
        if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
            state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;

    } // SurfNum

    if (ACH > 100.0) ShowWarningError(state, "CeilingDiffuser convection correlation is out of range: ACH > 100");
}

void CalcTrombeWallIntConvCoeff(EnergyPlusData &state,
                                int const ZoneNum,                         // Zone number for which coefficients are being calculated
                                const Array1D<Real64> &SurfaceTemperatures // Temperature of surfaces for evaluation of HcIn
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   ?????
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This subroutine calculates the interior convection coefficient
    // using the Trombe Wall correlation ?????

    // Using/Aliasing

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr Real64 g(9.81);     // gravity constant (m/s**2)
    constexpr Real64 v(15.89e-6); // kinematic viscosity (m**2/s) for air at 300 K
    constexpr Real64 k(0.0263);   // thermal conductivity (W/m K) for air at 300 K
    constexpr Real64 Pr(0.71);    // Prandtl number for air at ?

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SurfNum; // DO loop counter for surfaces
    int Surf1;   // first major wall surface
    int Surf2;   // second major wall surface

    Real64 H;        // height of enclosure
    Real64 minorW;   // width of enclosure (narrow dimension)
    Real64 majorW;   // width of major surface
    Real64 gapW;     // width of air gap
    Real64 asp;      // aspect ratio H/gapW
    Real64 beta;     // volumetric thermal expansion coefficient
    Real64 Gr;       // Grashof number
    Real64 Nu;       // Nusselt number
    Real64 HConvNet; // net heat transfer coefficient from wall to wall
    Real64 Tso;      // outside surface temperature [K]
    Real64 Tsi;      // inside surface temperature [K]

    // If the Trombe Wall option is selected the following correlations
    // will be used based on references by .....
    // tall enclosed rectangular cavity

    // This routine assumes that the major Trombe wall surfaces are of the
    // "WALL" class and are vertical.  The important heat transfer surfaces
    // are assumed to have exactly equal widths AND must have a greater
    // width than the side surfaces.

    Surf1 = 0;
    Surf2 = 0;

    H = Zone(ZoneNum).CeilingHeight;
    minorW = 100000.0; // An impossibly big width
    majorW = 0.0;
    gapW = 0.0;

    Tso = 0.0;
    Tsi = 0.0;
    HConvNet = 0.0;

    // determine major width and minor width
    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;

        if (Surface(SurfNum).Width > majorW) {
            majorW = Surface(SurfNum).Width;
        }

        if (Surface(SurfNum).Width < minorW) {
            minorW = Surface(SurfNum).Width;
        }
    }

    // assign major surfaces
    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        if (Surface(SurfNum).Class != SurfaceClass::Wall) continue;

        if (Surface(SurfNum).Width == majorW) {
            if (Surf1 == 0) {
                Surf1 = SurfNum;
            } else {
                Surf2 = SurfNum;

                break; // both major surfaces are now assigned
            }
        }
    }

    // check to make sure major surfaces were found
    if (Surf1 > 0 && Surf2 > 0) {
        gapW = minorW;
        asp = H / gapW; // This calc should only be done once for the zone

        // make sure inside surface is hot, outside is cold
        // NOTE: this is not ideal.  could have circumstances that reverse this?
        if (SurfaceTemperatures(Surf1) > SurfaceTemperatures(Surf2)) {
            Tsi = SurfaceTemperatures(Surf1) + DataGlobalConstants::KelvinConv;
            Tso = SurfaceTemperatures(Surf2) + DataGlobalConstants::KelvinConv;
        } else {
            Tso = SurfaceTemperatures(Surf1) + DataGlobalConstants::KelvinConv;
            Tsi = SurfaceTemperatures(Surf2) + DataGlobalConstants::KelvinConv;
        }

        beta = 2.0 / (Tso + Tsi);

        Gr = (g * beta * std::abs(Tsi - Tso) * pow_3(gapW)) / pow_2(v); // curve fit for v = v(T)?

        CalcNusselt(state, SurfNum, asp, Tso, Tsi, Gr, Pr, Nu); // curve fit for Pr = Pr(T)?

        HConvNet = (k / gapW) * Nu; // curve fit for k = k(T)?

    } else {
        // fatal Error msg "heat transfer surfaces not found"
    }

    // Assign convection coefficients
    for (SurfNum = Zone(ZoneNum).HTSurfaceFirst; SurfNum <= Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
        // Use ASHRAESimple correlation to give values for all the minor surfaces
        CalcASHRAESimpleIntConvCoeff(state, SurfNum, SurfaceTemperatures(SurfNum), state.dataHeatBalFanSys->MAT(ZoneNum));

        // assign the convection coefficent to the major surfaces and any subsurfaces on them
        if ((Surface(SurfNum).BaseSurf == Surf1) || (Surface(SurfNum).BaseSurf == Surf2)) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                ShowFatalError(state, "Trombe wall convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
            }
            state.dataHeatBal->HConvIn(SurfNum) = 2.0 * HConvNet;
        }

        // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
        if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
            state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
    }
}

void CalcNusselt(EnergyPlusData &state,
                 int const SurfNum, // Surface number
                 Real64 const asp,  // Aspect ratio: window height to gap width
                 Real64 const tso,  // Temperature of gap surface closest to outside (K)
                 Real64 const tsi,  // Temperature of gap surface closest to zone (K)
                 Real64 const gr,   // Gap gas Grashof number
                 Real64 const pr,   // Gap gas Prandtl number
                 Real64 &gnu        // Gap gas Nusselt number
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Peter Graham Ellis, based on code adapted by Fred Winkelmann
    //                      from Window5 subroutine NusseltNumber
    //       DATE WRITTEN   September 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Finds the Nusselt number for gas-filled gaps between isothermal solid layers.
    // The gap may be filled with a single gas or a gas mixture.

    // METHODOLOGY EMPLOYED:
    // Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
    // "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
    // The equation numbers below correspond to those in the standard.

    // REFERENCES:
    // Window5 source code; ISO 15099

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS
    Real64 ra;     // Rayleigh number
    Real64 gnu901; // Nusselt number temporary variables for
    Real64 gnu902;
    Real64 gnu90;
    Real64 gnu601;
    Real64 gnu602; // different tilt and Ra ranges
    Real64 gnu60;
    Real64 gnu601a;
    Real64 gnua;
    Real64 gnub;
    Real64 cra; // Temporary variables
    Real64 a;
    Real64 b;
    Real64 g;
    Real64 ang;
    Real64 tilt;
    Real64 tiltr;
    Real64 costilt;
    Real64 sintilt;

    auto &Surface(state.dataSurface->Surface);

    tilt = Surface(SurfNum).Tilt;
    tiltr = tilt * DataGlobalConstants::DegToRadians;
    costilt = Surface(SurfNum).CosTilt;
    sintilt = Surface(SurfNum).SinTilt;
    ra = gr * pr;
    //! fw if (ra > 2.0e6): error that outside range of Rayleigh number?

    if (ra <= 1.0e4) gnu901 = 1.0 + 1.7596678e-10 * std::pow(ra, 2.2984755); // eq. 51
    if (ra > 1.0e4 && ra <= 5.0e4) gnu901 = 0.028154 * std::pow(ra, 0.4134); // eq. 50
    if (ra > 5.0e4) gnu901 = 0.0673838 * std::pow(ra, 1.0 / 3.0);            // eq. 49

    gnu902 = 0.242 * std::pow(ra / asp, 0.272); // eq. 52
    gnu90 = max(gnu901, gnu902);

    if (tso > tsi) {                         // window heated from above
        gnu = 1.0 + (gnu90 - 1.0) * sintilt; // eq. 53
    } else {                                 // window heated from below
        if (tilt >= 60.0) {
            g = 0.5 * std::pow(1.0 + std::pow(ra / 3160.0, 20.6), -0.1);     // eq. 47
            gnu601a = 1.0 + pow_7(0.0936 * std::pow(ra, 0.314) / (1.0 + g)); // eq. 45
            gnu601 = std::pow(gnu601a, 0.142857);

            // For any aspect ratio
            gnu602 = (0.104 + 0.175 / asp) * std::pow(ra, 0.283); // eq. 46
            gnu60 = max(gnu601, gnu602);

            // linear interpolation for layers inclined at angles between 60 and 90 deg
            gnu = ((90.0 - tilt) * gnu60 + (tilt - 60.0) * gnu90) / 30.0;
        }
        if (tilt < 60.0) { // eq. 42
            cra = ra * costilt;
            a = 1.0 - 1708.0 / cra;
            b = std::pow(cra / 5830.0, 0.33333) - 1.0; // LKL- replace .333 with OneThird?
            gnua = (std::abs(a) + a) / 2.0;
            gnub = (std::abs(b) + b) / 2.0;
            ang = 1708.0 * std::pow(std::sin(1.8 * tiltr), 1.6);
            gnu = 1.0 + 1.44 * gnua * (1.0 - ang / cra) + gnub;
        }
    }
}

Real64 SetExtConvectionCoeff(EnergyPlusData &state, int const SurfNum) // Surface Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   May 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function accesses the data structure for the User
    // Supplied Exterior Convection Coefficients and returns that
    // as the result of this function.  The surface has already
    // been verified to have user supplied exterior convection values.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 SetExtConvectionCoeff;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 HExt(0.0); // Will become the returned value

    auto &Surface(state.dataSurface->Surface);

    {
        auto const SELECT_CASE_var(state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).OverrideType);

        if (SELECT_CASE_var == ConvCoefValue) {
            HExt = state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).OverrideValue;
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = KIVA_HF_ZERO;
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out = KIVA_CONST_CONV(HExt);
            }
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) = HcExt_UserValue; // reporting
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) = HcExt_None;      // reporting
        } else if (SELECT_CASE_var == ConvCoefSchedule) {
            HExt = GetCurrentScheduleValue(
                state, state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).ScheduleIndex);
            // Need to check for validity
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = KIVA_HF_ZERO;
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out = KIVA_CONST_CONV(HExt);
            }
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) = HcExt_UserSchedule; // reporting
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) = HcExt_None;         // reporting
        } else if (SELECT_CASE_var == ConvCoefUserCurve) {
            CalcUserDefinedOutsideHcModel(
                state, SurfNum, state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).UserCurveIndex, HExt);
            // Kiva convection handled in function above
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) = HcExt_UserCurve; // reporting
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) = HcExt_None;      // reporting
        } else if (SELECT_CASE_var == ConvCoefSpecifiedModel) {
            EvaluateExtHcModels(state,
                                SurfNum,
                                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).HcModelEq,
                                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).HcModelEq,
                                HExt);
            // Kiva convection handled in function above
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) =
                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).HcModelEq; // reporting
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) =
                state.dataSurface->UserExtConvectionCoeffs(state.dataSurface->SurfExtConvCoeffIndex(SurfNum)).HcModelEq; // reporting
        } else {
            assert(false);
        }
    }

    SetExtConvectionCoeff = HExt;

    return SetExtConvectionCoeff;
}

Real64 SetIntConvectionCoeff(EnergyPlusData &state, int const SurfNum) // Surface Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   May 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function accesses the data structure for the User
    // Supplied Interior Convection Coefficients and returns that
    // as the result of this function.  The surface has already
    // been verified to have user supplied interior convection values.

    // Using/Aliasing
    using ScheduleManager::GetCurrentScheduleValue;

    // Return value
    Real64 SetIntConvectionCoeff;

    Real64 HInt(0.0); // Will become the returned value

    auto &Surface(state.dataSurface->Surface);

    {
        auto const SELECT_CASE_var(state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).OverrideType);

        if (SELECT_CASE_var == ConvCoefValue) {
            HInt = state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).OverrideValue;
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = KIVA_CONST_CONV(HInt);
            }
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_UserValue; // reporting
        } else if (SELECT_CASE_var == ConvCoefSchedule) {
            HInt = GetCurrentScheduleValue(
                state, state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).ScheduleIndex);
            // Need to check for validity
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in = KIVA_CONST_CONV(HInt);
            }
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_UserSchedule; // reporting
        } else if (SELECT_CASE_var == ConvCoefUserCurve) {
            CalcUserDefinedInsideHcModel(
                state, SurfNum, state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).UserCurveIndex, HInt);
            // Kiva convection handled in function above
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_UserCurve; // reporting
        } else if (SELECT_CASE_var == ConvCoefSpecifiedModel) {
            EvaluateIntHcModels(
                state, SurfNum, state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).HcModelEq, HInt);
            // Kiva convection handled in function above
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataSurface->UserIntConvectionCoeffs(state.dataSurface->SurfIntConvCoeffIndex(SurfNum)).HcModelEq;
        } else {
            assert(false);
        }
    }

    SetIntConvectionCoeff = HInt;

    return SetIntConvectionCoeff;
}

Real64 CalcISO15099WindowIntConvCoeff(EnergyPlusData &state,
                                      Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
                                      Real64 const AirTemperature,     // Mean Air Temperature of Zone (or adjacent air temperature)
                                      Real64 const AirHumRat,          // air humidity ratio
                                      Real64 const Height,             // window cavity height [m]
                                      Real64 TiltDeg,                  // glazing tilt in degrees
                                      Real64 const sineTilt            // sine of glazing tilt
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   January 2009
    //       MODIFIED       BG May 2009, added EMS override for window coeffs.
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate interior surface convection coefficients for windows

    // METHODOLOGY EMPLOYED:
    // correlation documented in ISO 15099, Section 8.3.2.2

    // REFERENCES:
    // Internation Standard ISO 15099. Thermal performance of windows, doors and shading devices -- Detailed Calculations
    // First Edition 2003-11-15. ISO 15099:2003(E)

    // Using/Aliasing
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // Locals
    static constexpr Real64 OneThird((1.0 / 3.0)); // 1/3 in highest precision
    static Real64 const pow_5_25(0.56 * root_4(1.0E+5));
    static Real64 const pow_11_25(0.56 * root_4(1.0E+11));
    static Real64 const pow_11_2(0.58 * std::pow(1.0E+11, 0.2));
    static constexpr std::string_view RoutineName("WindowTempsForNominalCond");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 DeltaTemp;       // Temperature difference between the zone air and the surface
    Real64 TmeanFilm;       // mean film temperature
    Real64 TmeanFilmKelvin; // mean film temperature for property evaluation
    Real64 rho;             // density of air [kg/m3]
    Real64 g;               // acceleration due to gravity [m/s2]
    Real64 Cp;              // specific heat of air [J/kg-K]
    Real64 lambda;          // thermal conductivity of air [W/m-K]
    Real64 mu;              // dynamic viscosity of air [kg/m-s]
    Real64 RaH;             // Rayleigh number for cavity height [ Non dim]
    Real64 RaCV;            // Rayleigh number for slanted cavity
    Real64 Nuint(0.0);      // Nusselt number for interior surface convection
    Real64 SurfTempKelvin;  // surface temperature in Kelvin
    Real64 AirTempKelvin;   // air temperature in Kelvin

    SurfTempKelvin = SurfaceTemperature + 273.15;
    AirTempKelvin = AirTemperature + 273.15;
    DeltaTemp = SurfaceTemperature - AirTemperature;

    // protect against wildly out of range temperatures
    if ((AirTempKelvin < 200.0) || (AirTempKelvin > 400.0)) { // out of range
        return state.dataHeatBal->LowHConvLimit;
    }
    if ((SurfTempKelvin < 180.0) || (SurfTempKelvin > 450.0)) { // out of range
        return state.dataHeatBal->LowHConvLimit;
    }

    // mean film temperature
    TmeanFilmKelvin = AirTempKelvin + 0.25 * (SurfTempKelvin - AirTempKelvin); // eq. 133 in ISO 15099
    TmeanFilm = TmeanFilmKelvin - 273.15;

    rho = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TmeanFilm, AirHumRat, RoutineName);
    g = 9.81;

    // the following properties are probably for dry air, should maybe be remade for moist-air
    lambda = 2.873E-3 + 7.76E-5 * TmeanFilmKelvin; // Table B.1 in ISO 15099,
    mu = 3.723E-6 + 4.94E-8 * TmeanFilmKelvin;     // Table B.2 in ISO 15099

    Cp = PsyCpAirFnW(AirHumRat);

    // four cases depending on tilt and DeltaTemp (heat flow direction )
    if (DeltaTemp > 0.0) TiltDeg = 180.0 - TiltDeg; // complement angle if cooling situation

    RaH = (pow_2(rho) * pow_3(Height) * g * Cp * (std::abs(SurfTempKelvin - AirTempKelvin))) / (TmeanFilmKelvin * mu * lambda); // eq 132 in ISO 15099

    // case a)
    if ((0.0 <= TiltDeg) && (TiltDeg < 15.0)) {

        Nuint = 0.13 * std::pow(RaH, OneThird);

        // case b)
    } else if ((15.0 <= TiltDeg) && (TiltDeg <= 90.0)) {

        RaCV = 2.5E+5 * std::pow(std::exp(0.72 * TiltDeg) / sineTilt, 0.2); // eq. 137

        if (RaH <= RaCV) {
            Nuint = 0.56 * root_4(RaH * sineTilt); // eq. 135 in ISO 15099
        } else {
            Nuint = 0.13 * (std::pow(RaH, OneThird) - std::pow(RaCV, OneThird)) + 0.56 * root_4(RaCV * sineTilt); // eq. 136 in ISO 15099
        }

        // case c)
    } else if ((90.0 < TiltDeg) && (TiltDeg <= 179.0)) {
        // bound by applicability
        if (RaH * sineTilt < 1.0E+5) {
            Nuint = pow_5_25; // bounded
        } else if (RaH * sineTilt >= 1.0E+11) {
            Nuint = pow_11_25; // bounded
        } else {
            Nuint = 0.56 * root_4(RaH * sineTilt); // eq.. 138
        }

        // case d)
    } else if ((179.0 < TiltDeg) && (TiltDeg <= 180.0)) {

        if (RaH > 1.0E+11) {
            Nuint = pow_11_2; // bounded
        } else {
            Nuint = 0.58 * std::pow(RaH, 0.2);
        }

    } else {
        assert(false);
    }

    return Nuint * lambda / Height;
}

void CalcISO15099WindowIntConvCoeff(EnergyPlusData &state,
                                    int const SurfNum,               // surface number for which coefficients are being calculated
                                    Real64 const SurfaceTemperature, // Temperature of surface for evaluation of HcIn
                                    Real64 const AirTemperature      // Mean Air Temperature of Zone (or adjacent air temperature)
)
{
    auto &Surface(state.dataSurface->Surface);

    // Get humidity ratio
    Real64 AirHumRat;
    if (Surface(SurfNum).Zone > 0) {
        AirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(Surface(SurfNum).Zone);
    } else {
        AirHumRat = state.dataEnvrn->OutHumRat;
    }

    Real64 Height = Surface(SurfNum).Height;
    Real64 TiltDeg = Surface(SurfNum).Tilt;
    Real64 sineTilt = Surface(SurfNum).SinTilt;

    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        ShowFatalError(state, "ISO15099 convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
    }

    state.dataHeatBal->HConvIn(SurfNum) =
        CalcISO15099WindowIntConvCoeff(state, SurfaceTemperature, AirTemperature, AirHumRat, Height, TiltDeg, sineTilt);

    // EMS override point (Violates Standard 15099?  throw warning? scary.
    if (state.dataSurface->SurfEMSOverrideIntConvCoef(SurfNum))
        state.dataHeatBal->HConvIn(SurfNum) = state.dataSurface->SurfEMSValueForIntConvCoef(SurfNum);

    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
        state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
}

void SetupAdaptiveConvectionStaticMetaData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // do one-time setup needed to store static data
    // for adaptive convection algorithm

    // Using/Aliasing

    using General::ScanForReports;
    using Vectors::CreateNewellAreaVector;
    using Vectors::CreateNewellSurfaceNormalVector;
    using Vectors::DetermineAzimuthAndTilt;
    using Vectors::VecLength;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;
    int VertLoop;
    Real64 BldgVolumeSum;
    Real64 PerimExtLengthSum;

    Real64 thisWWR;
    Real64 thisZoneSimplePerim;
    Real64 thisZoneHorizHydralicDiameter;
    int ExtWallCount;
    int ExtWindowCount;
    Real64 thisAzimuth;
    Real64 thisArea;
    int thisZone;
    Array1D<Real64> RoofBoundZvals(8);
    Array1D<Real64> TestDist(4);
    Real64 surfacearea;
    Real64 BoundTilt;
    Real64 BoundAzimuth;
    bool DoReport;
    Real64 SideALength;
    Real64 SideBLength;
    Real64 SideCLength;
    Real64 SideDLength;
    std::string YesNo1;
    std::string YesNo2;

    // Object Data
    Vector BoundNewellVec;
    Vector BoundNewellAreaVec;
    Vector dummy1;
    Vector dummy2;
    Vector dummy3;

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    BldgVolumeSum = 0.0;
    RoofBoundZvals = 0.0;
    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {

        BldgVolumeSum += Zone(ZoneLoop).Volume * Zone(ZoneLoop).Multiplier * Zone(ZoneLoop).ListMultiplier;
        PerimExtLengthSum = 0.0; // init
        ExtWallCount = 0;        // init
        ExtWindowCount = 0;      // init
        // model perimeter of bounding horizontal rectangle from max and min x and y values
        thisZoneSimplePerim = 2.0 * (Zone(ZoneLoop).MaximumY - Zone(ZoneLoop).MinimumY) + 2.0 * (Zone(ZoneLoop).MaximumX - Zone(ZoneLoop).MinimumX);
        if (thisZoneSimplePerim > 0.0) {
            thisZoneHorizHydralicDiameter = 4.0 * Zone(ZoneLoop).FloorArea / thisZoneSimplePerim;
        } else {
            if (Zone(ZoneLoop).FloorArea > 0.0) {
                thisZoneHorizHydralicDiameter = std::sqrt(Zone(ZoneLoop).FloorArea);
            }
        }

        if (Zone(ZoneLoop).ExtGrossWallArea > 0.0) {
            thisWWR = Zone(ZoneLoop).ExtWindowArea / Zone(ZoneLoop).ExtGrossWallArea;
        } else {
            thisWWR = -999.0; // throw error?
        }
        // first pass thru this zones surfaces to gather data
        for (int SurfLoop = Zone(ZoneLoop).HTSurfaceFirst; SurfLoop <= Zone(ZoneLoop).HTSurfaceLast; ++SurfLoop) {
            // first catch exterior walls and do summations
            if ((Surface(SurfLoop).ExtBoundCond == ExternalEnvironment) && (Surface(SurfLoop).Class == SurfaceClass::Wall)) {
                PerimExtLengthSum += Surface(SurfLoop).Width;
                ++ExtWallCount;
            }
            if ((Surface(SurfLoop).ExtBoundCond == ExternalEnvironment) &&
                ((Surface(SurfLoop).Class == SurfaceClass::Window) || (Surface(SurfLoop).Class == SurfaceClass::GlassDoor))) {
                ++ExtWindowCount;
            }
        }

        // second pass thru zone surfs to fill data
        for (int SurfLoop = Zone(ZoneLoop).HTSurfaceFirst; SurfLoop <= Zone(ZoneLoop).HTSurfaceLast; ++SurfLoop) {
            // now fill values
            state.dataSurface->SurfIntConvZoneWallHeight(SurfLoop) = Zone(ZoneLoop).CeilingHeight;
            state.dataSurface->SurfIntConvZonePerimLength(SurfLoop) = PerimExtLengthSum;
            state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfLoop) = thisZoneHorizHydralicDiameter;
            state.dataSurface->SurfIntConvWindowWallRatio(SurfLoop) = thisWWR;
        } // 2nd pass over surfaces.

        // third pass for window locations
        if ((ExtWindowCount > 0) && (ExtWallCount > 0)) {
            for (int SurfLoop = Zone(ZoneLoop).HTSurfaceFirst; SurfLoop <= Zone(ZoneLoop).HTSurfaceLast; ++SurfLoop) {
                if ((Surface(SurfLoop).ExtBoundCond == ExternalEnvironment) &&
                    ((Surface(SurfLoop).Class == SurfaceClass::Window) || (Surface(SurfLoop).Class == SurfaceClass::GlassDoor))) {
                    if (state.dataSurface->SurfIntConvWindowWallRatio(SurfLoop) < 0.5) {
                        if (Surface(SurfLoop).Centroid.z < Zone(ZoneLoop).Centroid.z) {
                            state.dataSurface->SurfIntConvWindowLocation(SurfLoop) = InConvWinLoc_LowerPartOfExteriorWall;
                        } else {
                            state.dataSurface->SurfIntConvWindowLocation(SurfLoop) = InConvWinLoc_UpperPartOfExteriorWall;
                        }
                    } else {
                        state.dataSurface->SurfIntConvWindowLocation(SurfLoop) = InConvWinLoc_LargePartOfExteriorWall;
                    }
                    if ((Surface(Surface(SurfLoop).BaseSurf).ExtBoundCond == ExternalEnvironment) &&
                        (Surface(Surface(SurfLoop).BaseSurf).Class == SurfaceClass::Wall)) {
                        if (Surface(Surface(SurfLoop).BaseSurf).Centroid.z < Surface(SurfLoop).Centroid.z) {
                            state.dataSurface->SurfIntConvWindowLocation(Surface(SurfLoop).BaseSurf) = InConvWinLoc_WindowAboveThis;
                        } else {
                            state.dataSurface->SurfIntConvWindowLocation(Surface(SurfLoop).BaseSurf) = InConvWinLoc_WindowBelowThis;
                        }
                    }
                }
                if ((Surface(SurfLoop).ExtBoundCond == ExternalEnvironment) && (Surface(SurfLoop).Class == SurfaceClass::Wall) &&
                    (state.dataSurface->SurfIntConvWindowLocation(SurfLoop) == InConvWinLoc_NotSet)) {
                    if (Surface(SurfLoop).Centroid.z < Zone(ZoneLoop).Centroid.z) {
                        state.dataSurface->SurfIntConvWindowLocation(SurfLoop) = InConvWinLoc_WindowAboveThis;
                    } else {
                        state.dataSurface->SurfIntConvWindowLocation(SurfLoop) = InConvWinLoc_WindowBelowThis;
                    }
                }
            } // third pass over surfaces
        }
    } // loop over zones for inside face parameters

    state.dataConvectionCoefficient->CubeRootOfOverallBuildingVolume = std::pow(BldgVolumeSum, OneThird);

    auto &NorthFacade = state.dataConvectionCoefficient->NorthFacade;
    auto &NorthEastFacade = state.dataConvectionCoefficient->NorthEastFacade;
    auto &EastFacade = state.dataConvectionCoefficient->EastFacade;
    auto &SouthEastFacade = state.dataConvectionCoefficient->SouthEastFacade;
    auto &SouthFacade = state.dataConvectionCoefficient->SouthFacade;
    auto &SouthWestFacade = state.dataConvectionCoefficient->SouthWestFacade;
    auto &WestFacade = state.dataConvectionCoefficient->WestFacade;
    auto &NorthWestFacade = state.dataConvectionCoefficient->NorthWestFacade;

    // first pass over surfaces for outside face params
    for (int SurfLoop = 1; SurfLoop <= state.dataSurface->TotSurfaces; ++SurfLoop) {
        if (Surface(SurfLoop).ExtBoundCond != ExternalEnvironment) continue;
        if (!Surface(SurfLoop).HeatTransSurf) continue;
        thisAzimuth = Surface(SurfLoop).Azimuth;
        thisArea = Surface(SurfLoop).Area;
        thisZone = Surface(SurfLoop).Zone;
        if ((Surface(SurfLoop).Tilt >= 45.0) && (Surface(SurfLoop).Tilt < 135.0)) { // treat as vertical wall

            auto const &vertices(Surface(SurfLoop).Vertex);
            Real64 const x_min(minval(vertices, &Vector::x));
            Real64 const y_min(minval(vertices, &Vector::y));
            Real64 const z_min(minval(vertices, &Vector::z));
            Real64 const x_max(maxval(vertices, &Vector::x));
            Real64 const y_max(maxval(vertices, &Vector::y));
            Real64 const z_max(maxval(vertices, &Vector::z));

            if ((thisAzimuth >= NorthFacade.AzimuthRangeLow) || (thisAzimuth < NorthFacade.AzimuthRangeHi)) {
                NorthFacade.Area += thisArea;
                NorthFacade.Zmax = max(z_max, NorthFacade.Zmax);
                NorthFacade.Zmin = min(z_min, NorthFacade.Zmin);
                NorthFacade.Ymax = max(y_max, NorthFacade.Ymax);
                NorthFacade.Ymin = min(y_min, NorthFacade.Ymin);
                NorthFacade.Xmax = max(x_max, NorthFacade.Xmax);
                NorthFacade.Xmin = min(x_min, NorthFacade.Xmin);

            } else if ((thisAzimuth >= NorthEastFacade.AzimuthRangeLow) && (thisAzimuth < NorthEastFacade.AzimuthRangeHi)) {
                NorthEastFacade.Area += thisArea;
                NorthEastFacade.Zmax = max(z_max, NorthEastFacade.Zmax);
                NorthEastFacade.Zmin = min(z_min, NorthEastFacade.Zmin);
                NorthEastFacade.Ymax = max(y_max, NorthEastFacade.Ymax);
                NorthEastFacade.Ymin = min(y_min, NorthEastFacade.Ymin);
                NorthEastFacade.Xmax = max(x_max, NorthEastFacade.Xmax);
                NorthEastFacade.Xmin = min(x_min, NorthEastFacade.Xmin);

            } else if ((thisAzimuth >= EastFacade.AzimuthRangeLow) && (thisAzimuth < EastFacade.AzimuthRangeHi)) {
                EastFacade.Area += thisArea;
                EastFacade.Zmax = max(z_max, EastFacade.Zmax);
                EastFacade.Zmin = min(z_min, EastFacade.Zmin);
                EastFacade.Ymax = max(y_max, EastFacade.Ymax);
                EastFacade.Ymin = min(y_min, EastFacade.Ymin);
                EastFacade.Xmax = max(x_max, EastFacade.Xmax);
                EastFacade.Xmin = min(x_min, EastFacade.Xmin);
            } else if ((thisAzimuth >= SouthEastFacade.AzimuthRangeLow) && (thisAzimuth < SouthEastFacade.AzimuthRangeHi)) {
                SouthEastFacade.Area += thisArea;
                SouthEastFacade.Zmax = max(z_max, SouthEastFacade.Zmax);
                SouthEastFacade.Zmin = min(z_min, SouthEastFacade.Zmin);
                SouthEastFacade.Ymax = max(y_max, SouthEastFacade.Ymax);
                SouthEastFacade.Ymin = min(y_min, SouthEastFacade.Ymin);
                SouthEastFacade.Xmax = max(x_max, SouthEastFacade.Xmax);
                SouthEastFacade.Xmin = min(x_min, SouthEastFacade.Xmin);

            } else if ((thisAzimuth >= SouthFacade.AzimuthRangeLow) && (thisAzimuth < SouthFacade.AzimuthRangeHi)) {
                SouthFacade.Area += thisArea;
                SouthFacade.Zmax = max(z_max, SouthFacade.Zmax);
                SouthFacade.Zmin = min(z_min, SouthFacade.Zmin);
                SouthFacade.Ymax = max(y_max, SouthFacade.Ymax);
                SouthFacade.Ymin = min(y_min, SouthFacade.Ymin);
                SouthFacade.Xmax = max(x_max, SouthFacade.Xmax);
                SouthFacade.Xmin = min(x_min, SouthFacade.Xmin);

            } else if ((thisAzimuth >= SouthWestFacade.AzimuthRangeLow) && (thisAzimuth < SouthWestFacade.AzimuthRangeHi)) {
                SouthWestFacade.Area += thisArea;
                SouthWestFacade.Zmax = max(z_max, SouthWestFacade.Zmax);
                SouthWestFacade.Zmin = min(z_min, SouthWestFacade.Zmin);
                SouthWestFacade.Ymax = max(y_max, SouthWestFacade.Ymax);
                SouthWestFacade.Ymin = min(y_min, SouthWestFacade.Ymin);
                SouthWestFacade.Xmax = max(x_max, SouthWestFacade.Xmax);
                SouthWestFacade.Xmin = min(x_min, SouthWestFacade.Xmin);

            } else if ((thisAzimuth >= WestFacade.AzimuthRangeLow) && (thisAzimuth < WestFacade.AzimuthRangeHi)) {
                WestFacade.Area += thisArea;
                WestFacade.Zmax = max(z_max, WestFacade.Zmax);
                WestFacade.Zmin = min(z_min, WestFacade.Zmin);
                WestFacade.Ymax = max(y_max, WestFacade.Ymax);
                WestFacade.Ymin = min(y_min, WestFacade.Ymin);
                WestFacade.Xmax = max(x_max, WestFacade.Xmax);
                WestFacade.Xmin = min(x_min, WestFacade.Xmin);

            } else if ((thisAzimuth >= NorthWestFacade.AzimuthRangeLow) && (thisAzimuth < NorthWestFacade.AzimuthRangeHi)) {
                NorthWestFacade.Area += thisArea;
                NorthWestFacade.Zmax = max(z_max, NorthWestFacade.Zmax);
                NorthWestFacade.Zmin = min(z_min, NorthWestFacade.Zmin);
                NorthWestFacade.Ymax = max(y_max, NorthWestFacade.Ymax);
                NorthWestFacade.Ymin = min(y_min, NorthWestFacade.Ymin);
                NorthWestFacade.Xmax = max(x_max, NorthWestFacade.Xmax);
                NorthWestFacade.Xmin = min(x_min, NorthWestFacade.Xmin);
            }
        } else if (Surface(SurfLoop).Tilt < 45.0) { // TODO Double check tilt wrt outside vs inside

            if (state.dataConvectionCoefficient->FirstRoofSurf) { // Init with something in the group
                state.dataConvectionCoefficient->RoofGeo.XdYdZd.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XdYdZd.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XdYdZu.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XdYdZu.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XdYuZd.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XdYuZd.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XdYuZu.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XdYuZu.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XuYdZd.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XuYdZd.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XuYuZd.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XuYuZd.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XuYdZu.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XuYdZu.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->RoofGeo.XuYuZu.SurfNum = SurfLoop;
                state.dataConvectionCoefficient->RoofGeo.XuYuZu.VertNum = 1;
                state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex = Surface(SurfLoop).Vertex(1);

                state.dataConvectionCoefficient->FirstRoofSurf = false;
            }
            // treat as part of roof group
            state.dataConvectionCoefficient->RoofGeo.Area += thisArea;
            for (VertLoop = 1; VertLoop <= Surface(SurfLoop).Sides; ++VertLoop) {

                // 1 low x, low y, low z
                if ((Surface(SurfLoop).Vertex(VertLoop).x <= state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y <= state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z <= state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XdYdZd.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYdZd.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(1) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 2 low x, low y, hi z
                if ((Surface(SurfLoop).Vertex(VertLoop).x <= state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y <= state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z >= state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XdYdZu.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYdZu.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(2) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 3 low x, hi y, low z
                if ((Surface(SurfLoop).Vertex(VertLoop).x <= state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y >= state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z <= state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XdYuZd.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYuZd.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(3) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 4 low x, hi y, hi z
                if ((Surface(SurfLoop).Vertex(VertLoop).x <= state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y >= state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z >= state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XdYuZu.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYuZu.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(4) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 5 hi x, low y, low z
                if ((Surface(SurfLoop).Vertex(VertLoop).x >= state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y <= state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z <= state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XuYdZd.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYdZd.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(5) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 6 hi x, hi y, low z
                if ((Surface(SurfLoop).Vertex(VertLoop).x >= state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y >= state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z <= state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XuYuZd.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYuZd.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(6) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 7 hi x, low y, hi z
                if ((Surface(SurfLoop).Vertex(VertLoop).x >= state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y <= state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z >= state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XuYdZu.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYdZu.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(7) = Surface(SurfLoop).Vertex(VertLoop).z;
                }

                // 8 hi x, hi y, hi z
                if ((Surface(SurfLoop).Vertex(VertLoop).x >= state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.x) &&
                    (Surface(SurfLoop).Vertex(VertLoop).y >= state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.y) &&
                    (Surface(SurfLoop).Vertex(VertLoop).z >= state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.z)) {
                    // this point is more toward this bound
                    state.dataConvectionCoefficient->RoofGeo.XuYuZu.SurfNum = SurfLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYuZu.VertNum = VertLoop;
                    state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex = Surface(SurfLoop).Vertex(VertLoop);
                    RoofBoundZvals(8) = Surface(SurfLoop).Vertex(VertLoop).z;
                }
            }
        }
    } // fist loop over surfaces for outside face params

    NorthFacade.Perimeter = 2.0 * std::sqrt(pow_2(NorthFacade.Xmax - NorthFacade.Xmin) + pow_2(NorthFacade.Ymax - NorthFacade.Ymin)) +
                            2.0 * (NorthFacade.Zmax - NorthFacade.Zmin);
    NorthFacade.Height = NorthFacade.Zmax - NorthFacade.Zmin;

    NorthEastFacade.Perimeter =
        2.0 * std::sqrt(pow_2(NorthEastFacade.Xmax - NorthEastFacade.Xmin) + pow_2(NorthEastFacade.Ymax - NorthEastFacade.Ymin)) +
        2.0 * (NorthEastFacade.Zmax - NorthEastFacade.Zmin);
    NorthEastFacade.Height = NorthEastFacade.Zmax - NorthEastFacade.Zmin;

    EastFacade.Perimeter = 2.0 * std::sqrt(pow_2(EastFacade.Xmax - EastFacade.Xmin) + pow_2(EastFacade.Ymax - EastFacade.Ymin)) +
                           2.0 * (EastFacade.Zmax - EastFacade.Zmin);
    EastFacade.Height = EastFacade.Zmax - EastFacade.Zmin;

    SouthEastFacade.Perimeter =
        2.0 * std::sqrt(pow_2(SouthEastFacade.Xmax - SouthEastFacade.Xmin) + pow_2(SouthEastFacade.Ymax - SouthEastFacade.Ymin)) +
        2.0 * (SouthEastFacade.Zmax - SouthEastFacade.Zmin);
    SouthEastFacade.Height = SouthEastFacade.Zmax - SouthEastFacade.Zmin;

    SouthFacade.Perimeter = 2.0 * std::sqrt(pow_2(SouthFacade.Xmax - SouthFacade.Xmin) + pow_2(SouthFacade.Ymax - SouthFacade.Ymin)) +
                            2.0 * (SouthFacade.Zmax - SouthFacade.Zmin);
    SouthFacade.Height = SouthFacade.Zmax - SouthFacade.Zmin;

    SouthWestFacade.Perimeter =
        2.0 * std::sqrt(pow_2(SouthWestFacade.Xmax - SouthWestFacade.Xmin) + pow_2(SouthWestFacade.Ymax - SouthWestFacade.Ymin)) +
        2.0 * (SouthWestFacade.Zmax - SouthWestFacade.Zmin);
    SouthWestFacade.Height = SouthWestFacade.Zmax - SouthWestFacade.Zmin;

    WestFacade.Perimeter = 2.0 * std::sqrt(pow_2(WestFacade.Xmax - WestFacade.Xmin) + pow_2(WestFacade.Ymax - WestFacade.Ymin)) +
                           2.0 * (WestFacade.Zmax - WestFacade.Zmin);
    WestFacade.Height = WestFacade.Zmax - WestFacade.Zmin;

    NorthWestFacade.Perimeter =
        2.0 * std::sqrt(pow_2(NorthWestFacade.Xmax - NorthWestFacade.Xmin) + pow_2(NorthWestFacade.Ymax - NorthWestFacade.Ymin)) +
        2.0 * (NorthWestFacade.Zmax - NorthWestFacade.Zmin);
    NorthWestFacade.Height = NorthWestFacade.Zmax - NorthWestFacade.Zmin;

    // now model roof perimeter
    // move around bounding boxes side walls and find the longest of the four distances
    // Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
    TestDist(1) = distance(state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex);
    TestDist(2) = distance(state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex);
    TestDist(3) = distance(state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex);
    TestDist(4) = distance(state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex);
    SideALength = maxval(TestDist);

    // Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
    TestDist(1) = distance(state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(2) = distance(state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    TestDist(3) = distance(state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(4) = distance(state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    SideBLength = maxval(TestDist);

    // Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
    TestDist(1) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(2) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    TestDist(3) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(4) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    SideCLength = maxval(TestDist);

    // Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
    TestDist(1) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(2) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    TestDist(3) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex);
    TestDist(4) = distance(state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex, state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex);
    SideDLength = maxval(TestDist);

    state.dataConvectionCoefficient->RoofGeo.Perimeter = SideALength + SideBLength + SideCLength + SideDLength;

    state.dataConvectionCoefficient->RoofGeo.Height = maxval(RoofBoundZvals) - minval(RoofBoundZvals);

    // now find the longest bound face
    if ((SideALength >= SideBLength) && (SideALength >= SideCLength) && (SideALength >= SideDLength)) {
        // Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(1) = state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(2) = state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(3) = state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(4) = state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex;

    } else if ((SideBLength >= SideALength) && (SideBLength >= SideCLength) && (SideBLength >= SideDLength)) {
        // Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(1) = state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(2) = state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(3) = state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(4) = state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex;
    } else if ((SideCLength >= SideALength) && (SideCLength >= SideBLength) && (SideCLength >= SideDLength)) {
        // Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(1) = state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(2) = state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(3) = state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(4) = state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex;
    } else if ((SideDLength >= SideALength) && (SideDLength >= SideCLength) && (SideDLength >= SideBLength)) {
        // Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(1) = state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(2) = state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(3) = state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex;
        state.dataConvectionCoefficient->RoofGeo.BoundSurf(4) = state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex;
    }

    CreateNewellAreaVector(state.dataConvectionCoefficient->RoofGeo.BoundSurf, 4, BoundNewellAreaVec);
    surfacearea = VecLength(BoundNewellAreaVec);
    if (surfacearea > 0.001) { // Roof is not flat
        CreateNewellSurfaceNormalVector(state.dataConvectionCoefficient->RoofGeo.BoundSurf, 4, BoundNewellVec);
        DetermineAzimuthAndTilt(
            state.dataConvectionCoefficient->RoofGeo.BoundSurf, 4, BoundAzimuth, BoundTilt, dummy1, dummy2, dummy3, surfacearea, BoundNewellVec);
        state.dataConvectionCoefficient->RoofLongAxisOutwardAzimuth = BoundAzimuth;
    } else {
        state.dataConvectionCoefficient->RoofLongAxisOutwardAzimuth = 0.0; // flat roofs don't really have azimuth
    }

    for (int SurfLoop = 1; SurfLoop <= state.dataSurface->TotSurfaces; ++SurfLoop) {
        if (Surface(SurfLoop).ExtBoundCond != ExternalEnvironment) continue;
        if (!Surface(SurfLoop).HeatTransSurf) continue;
        thisAzimuth = Surface(SurfLoop).Azimuth;

        auto const &vertices(Surface(SurfLoop).Vertex);
        Real64 const z_min(minval(vertices, &Vector::z));
        Real64 const z_max(maxval(vertices, &Vector::z));
        Real64 const z_del(z_max - z_min);

        if ((Surface(SurfLoop).Tilt >= 45.0) && (Surface(SurfLoop).Tilt < 135.0)) { // treat as vertical wall
            if ((thisAzimuth >= NorthFacade.AzimuthRangeLow) || (thisAzimuth < NorthFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(NorthFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(NorthFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(NorthFacade.Height, z_del);
            } else if ((thisAzimuth >= NorthEastFacade.AzimuthRangeLow) && (thisAzimuth < NorthEastFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(NorthEastFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(NorthEastFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(NorthEastFacade.Height, z_del);
            } else if ((thisAzimuth >= EastFacade.AzimuthRangeLow) && (thisAzimuth < EastFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(EastFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(EastFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(EastFacade.Height, z_del);
            } else if ((thisAzimuth >= SouthEastFacade.AzimuthRangeLow) && (thisAzimuth < SouthEastFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(SouthEastFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(SouthEastFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(SouthEastFacade.Height, z_del);
            } else if ((thisAzimuth >= SouthFacade.AzimuthRangeLow) && (thisAzimuth < SouthFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(SouthFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(SouthFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(SouthFacade.Height, z_del);
            } else if ((thisAzimuth >= SouthWestFacade.AzimuthRangeLow) && (thisAzimuth < SouthWestFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(SouthWestFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(SouthWestFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(SouthWestFacade.Height, z_del);
            } else if ((thisAzimuth >= WestFacade.AzimuthRangeLow) && (thisAzimuth < WestFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(WestFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(WestFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(WestFacade.Height, z_del);
            } else if ((thisAzimuth >= NorthWestFacade.AzimuthRangeLow) && (thisAzimuth < NorthWestFacade.AzimuthRangeHi)) {
                state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(NorthWestFacade.Area, Surface(SurfLoop).GrossArea);
                state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = max(NorthWestFacade.Perimeter, Surface(SurfLoop).Perimeter);
                state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(NorthWestFacade.Height, z_del);
            }
        } else if (Surface(SurfLoop).Tilt < 45.0) { // assume part of roof
            state.dataSurface->SurfOutConvFaceArea(SurfLoop) = max(state.dataConvectionCoefficient->RoofGeo.Area, Surface(SurfLoop).GrossArea);
            state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) =
                max(state.dataConvectionCoefficient->RoofGeo.Perimeter, Surface(SurfLoop).Perimeter);
            state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = max(state.dataConvectionCoefficient->RoofGeo.Height, z_del);
        } else if (Surface(SurfLoop).Tilt >= 135.0) { // assume floor over exterior, just use surface's geometry
            state.dataSurface->SurfOutConvFaceArea(SurfLoop) = Surface(SurfLoop).GrossArea;
            state.dataSurface->SurfOutConvFacePerimeter(SurfLoop) = Surface(SurfLoop).Perimeter;
            state.dataSurface->SurfOutConvFaceHeight(SurfLoop) = z_del;
        }
    } // second pass thru surfs for outside face convection params.

    // now send to EIO if surface reporting selected
    ScanForReports(state, "Surfaces", DoReport, "Details");
    if (DoReport) { // echo out static geometry data related to convection models
        static constexpr fmt::string_view Format_900(
            "! <Surface Convection Parameters>, Surface Name, Outside Model Assignment, Outside Area [m2], Outside Perimeter [m], Outside Height "
            "[m], Inside Model Assignment, Inside Height [m], Inside Perimeter Envelope [m], Inside Hydraulic Diameter [m], Window Wall Ratio, "
            "Window Location, Near Radiant {{Yes/No}}, Has Active HVAC {{Yes/No}}\n");
        print(state.files.eio, Format_900); // header
        for (int SurfLoop : state.dataSurface->AllSurfaceListReportOrder) {
            if (!Surface(SurfLoop).HeatTransSurf) continue;
            if (state.dataSurface->SurfIntConvSurfGetsRadiantHeat(SurfLoop)) {
                YesNo1 = "Yes";
            } else {
                YesNo1 = "No";
            }
            if (state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfLoop)) {
                YesNo2 = "Yes";
            } else {
                YesNo2 = "No";
            }
            static constexpr fmt::string_view Format_901("Surface Convection Parameters,{},{},{:.2R},{:.2R},{:.2R},{},{:.2R},{:.2R},{:.2R},{:.2R},{},{},{}\n");
            print(state.files.eio,
                  Format_901,
                  Surface(SurfLoop).Name,
                  state.dataSurface->SurfExtConvCoeffIndex(SurfLoop),
                  state.dataSurface->SurfOutConvFaceArea(SurfLoop),
                  state.dataSurface->SurfOutConvFacePerimeter(SurfLoop),
                  state.dataSurface->SurfOutConvFaceHeight(SurfLoop),
                  state.dataSurface->SurfIntConvCoeffIndex(SurfLoop),
                  state.dataSurface->SurfIntConvZoneWallHeight(SurfLoop),
                  state.dataSurface->SurfIntConvZonePerimLength(SurfLoop),
                  state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfLoop),
                  state.dataSurface->SurfIntConvWindowWallRatio(SurfLoop),
                  state.dataSurface->SurfIntConvWindowLocation(SurfLoop),
                  YesNo1,
                  YesNo2);

            // [m] length of perimeter zone's exterior wall | [m] hydraulic diameter, usually 4 times the zone floor area div by
            // perimeter | [-] area of windows over area of exterior wall for zone | relative location of window in zone for
            // interior Hc models
        }

        // if display advanced reports also dump meta group data used for convection geometry
        if (state.dataGlobal->DisplayAdvancedReportVariables) {
            static constexpr fmt::string_view Format_8000(
                "! <Building Convection Parameters:North Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8000); // header for north facade
            static constexpr fmt::string_view Format_8001(
                "Building Convection Parameters:North Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8001,
                  NorthFacade.Perimeter,
                  NorthFacade.Height,
                  NorthFacade.Xmin,
                  NorthFacade.Xmax,
                  NorthFacade.Ymin,
                  NorthFacade.Ymax,
                  NorthFacade.Zmin,
                  NorthFacade.Zmax);
            static constexpr fmt::string_view Format_8100(
                "! <Building Convection Parameters:Northeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8100); // header for northeast facade
            static constexpr fmt::string_view Format_8101(
                "Building Convection Parameters:Northeast Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8101,
                  NorthEastFacade.Perimeter,
                  NorthEastFacade.Height,
                  NorthEastFacade.Xmin,
                  NorthEastFacade.Xmax,
                  NorthEastFacade.Ymin,
                  NorthEastFacade.Ymax,
                  NorthEastFacade.Zmin,
                  NorthEastFacade.Zmax);
            static constexpr fmt::string_view Format_8200(
                "! <Building Convection Parameters:East Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8200); // header for east facade
            static constexpr fmt::string_view Format_8201(
                "Building Convection Parameters:East Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8201,
                  EastFacade.Perimeter,
                  EastFacade.Height,
                  EastFacade.Xmin,
                  EastFacade.Xmax,
                  EastFacade.Ymin,
                  EastFacade.Ymax,
                  EastFacade.Zmin,
                  EastFacade.Zmax);

            static constexpr fmt::string_view Format_8300(
                "! <Building Convection Parameters:Southeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8300); // header for southeast facade
            static constexpr fmt::string_view Format_8301(
                "Building Convection Parameters:Southeast Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8301,
                  SouthEastFacade.Perimeter,
                  SouthEastFacade.Height,
                  SouthEastFacade.Xmin,
                  SouthEastFacade.Xmax,
                  SouthEastFacade.Ymin,
                  SouthEastFacade.Ymax,
                  SouthEastFacade.Zmin,
                  SouthEastFacade.Zmax);

            static constexpr fmt::string_view Format_8400(
                "! <Building Convection Parameters:South Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8400); // header for south facade
            static constexpr fmt::string_view Format_8401(
                "Building Convection Parameters:South Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8401,
                  SouthFacade.Perimeter,
                  SouthFacade.Height,
                  SouthFacade.Xmin,
                  SouthFacade.Xmax,
                  SouthFacade.Ymin,
                  SouthFacade.Ymax,
                  SouthFacade.Zmin,
                  SouthFacade.Zmax);
            static constexpr fmt::string_view Format_8500(
                "! <Building Convection Parameters:Southwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8500); // header for southwest facade
            static constexpr fmt::string_view Format_8501(
                "Building Convection Parameters:Southwest Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8501,
                  SouthWestFacade.Perimeter,
                  SouthWestFacade.Height,
                  SouthWestFacade.Xmin,
                  SouthWestFacade.Xmax,
                  SouthWestFacade.Ymin,
                  SouthWestFacade.Ymax,
                  SouthWestFacade.Zmin,
                  SouthWestFacade.Zmax);
            static constexpr fmt::string_view Format_8600(
                "! <Building Convection Parameters:West Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8600); // header for west facade
            static constexpr fmt::string_view Format_8601(
                "Building Convection Parameters:West Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8601,
                  WestFacade.Perimeter,
                  WestFacade.Height,
                  WestFacade.Xmin,
                  WestFacade.Xmax,
                  WestFacade.Ymin,
                  WestFacade.Ymax,
                  WestFacade.Zmin,
                  WestFacade.Zmax);
            static constexpr fmt::string_view Format_8700(
                "! <Building Convection Parameters:Northwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax \n");
            print(state.files.eio, Format_8700); // header for northwest facade
            static constexpr fmt::string_view Format_8701(
                "Building Convection Parameters:NorthwWest Facade, {:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R},{:.2R}\n");
            print(state.files.eio,
                  Format_8701,
                  NorthWestFacade.Perimeter,
                  NorthWestFacade.Height,
                  NorthWestFacade.Xmin,
                  NorthWestFacade.Xmax,
                  NorthWestFacade.Ymin,
                  NorthWestFacade.Ymax,
                  NorthWestFacade.Zmin,
                  NorthWestFacade.Zmax);
            static constexpr fmt::string_view Format_8800(
                "! <Building Convection Parameters:Roof>, Area [m2], Perimeter [m], Height [m], XdYdZd:X, XdYdZd:Y, XdYdZd:Z,XdYdZu:X, XdYdZu:Y, "
                "XdYdZu:Z,XdYuZd:X, XdYuZd:Y, XdYuZd:Z,XdYuZu:X, XdYuZu:Y, XdYuZu:Z,XuYdZd:X, XuYdZd:Y, XuYdZd:Z,XuYuZd:X, XuYuZd:Y, "
                "XuYuZd:Z,XuYdZu:X, XuYdZu:Y, XuYdZu:Z,XuYuZu:X, XuYuZu:Y, XuYuZu:Z\n");
            print(state.files.eio, Format_8800); // header for roof
            static constexpr fmt::string_view Format_8801(
                "Building Convection Parameters:Roof,{:.2R},{:.2R},{:.2R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},");
            print(state.files.eio,
                  Format_8801,
                  state.dataConvectionCoefficient->RoofGeo.Area,
                  state.dataConvectionCoefficient->RoofGeo.Perimeter,
                  state.dataConvectionCoefficient->RoofGeo.Height,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZd.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XdYdZu.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.x);
            static constexpr fmt::string_view Format_88012("{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},");
            print(state.files.eio,
                  Format_88012,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZd.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XdYuZu.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZd.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.y);
            static constexpr fmt::string_view Format_88013("{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R},{:.3R}\n");
            print(state.files.eio,
                  Format_88013,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZd.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XuYdZu.Vertex.z,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.x,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.y,
                  state.dataConvectionCoefficient->RoofGeo.XuYuZu.Vertex.z);
        }
    }
}

void SetupAdaptiveConvectionRadiantSurfaceData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Sept 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // identify Zones that have active radiant elements for convection model classifications

    // METHODOLOGY EMPLOYED:
    // Need to fill in values for ZoneEquipConfig%InWallActiveElement, ZoneEquipConfig%InCeilingActiveElement
    // and ZoneEquipConfig(ZoneNum)%InFloorActiveElement.

    // Using/Aliasing
    using namespace DataZoneEquipment;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneLoop;
    int SurfLoop;

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    for (ZoneLoop = 1; ZoneLoop <= state.dataGlobal->NumOfZones; ++ZoneLoop) {
        state.dataConvectionCoefficient->ActiveWallCount = 0;
        state.dataConvectionCoefficient->ActiveWallArea = 0.0;
        state.dataConvectionCoefficient->ActiveCeilingCount = 0;
        state.dataConvectionCoefficient->ActiveCeilingArea = 0.0;
        state.dataConvectionCoefficient->ActiveFloorCount = 0;
        state.dataConvectionCoefficient->ActiveFloorArea = 0.0;

        for (SurfLoop = Zone(ZoneLoop).HTSurfaceFirst; SurfLoop <= Zone(ZoneLoop).HTSurfaceLast; ++SurfLoop) {
            if (!state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfLoop)) continue;
            if (Surface(SurfLoop).Class == SurfaceClass::Wall || Surface(SurfLoop).Class == SurfaceClass::Door) {
                ++state.dataConvectionCoefficient->ActiveWallCount;
                state.dataConvectionCoefficient->ActiveWallArea += Surface(SurfLoop).Area;
            } else if (Surface(SurfLoop).Class == SurfaceClass::Roof) {
                ++state.dataConvectionCoefficient->ActiveCeilingCount;
                state.dataConvectionCoefficient->ActiveCeilingArea += Surface(SurfLoop).Area;
            } else if (Surface(SurfLoop).Class == SurfaceClass::Floor) {
                ++state.dataConvectionCoefficient->ActiveFloorCount;
                state.dataConvectionCoefficient->ActiveFloorArea += Surface(SurfLoop).Area;
            }
        } // surface loop

        if ((state.dataConvectionCoefficient->ActiveWallCount > 0) && (state.dataConvectionCoefficient->ActiveWallArea > 0.0)) {
            state.dataZoneEquip->ZoneEquipConfig(ZoneLoop).InWallActiveElement = true;
        }
        if ((state.dataConvectionCoefficient->ActiveCeilingCount > 0) && (state.dataConvectionCoefficient->ActiveCeilingArea > 0.0)) {
            state.dataZoneEquip->ZoneEquipConfig(ZoneLoop).InCeilingActiveElement = true;
        }
        if ((state.dataConvectionCoefficient->ActiveFloorCount > 0) && (state.dataConvectionCoefficient->ActiveFloorArea > 0)) {
            state.dataZoneEquip->ZoneEquipConfig(ZoneLoop).InFloorActiveElement = true;
        }
    } // zone loop
}

void ManageInsideAdaptiveConvectionAlgo(EnergyPlusData &state, int const SurfNum) // surface number for which coefficients are being calculated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages the calculation of interior convection coefficient for a surface.

    // METHODOLOGY EMPLOYED:
    // This routine implements the Adaptive Convection Algorithm developed by IB-M 2000 and IB-M 2002
    //  - first calls a large routine, DynamicIntConvSurfaceClassification, that has most of the complex logic
    //  - then calls a straightforward routine that maps the classification to model equation
    //  - then calls a routine with a large case statement that calls model equations.

    // USE STATEMENTS:

    // this next call sets up the flow regime and assigns a classification to surface
    //  TODO: candidate for rework to do zone level calcs once rather than for each surface
    DynamicIntConvSurfaceClassification(state, SurfNum);

    // simple worker routine takes surface classification and fills in model to use (IntConvHcModelEq) for that surface
    MapIntConvClassificationToHcModels(state, SurfNum);

    EvaluateIntHcModels(state, SurfNum, state.dataSurface->SurfIntConvHcModelEq(SurfNum), state.dataHeatBal->HConvIn(SurfNum));
    // if ( std::isnan( HConvIn( SurfNum ) ) ) { // Use IEEE_IS_NAN when GFortran supports it
    //// throw Error
    // ShowSevereError(state,  "Inside convection coefficient is out of bound = " + Surface( SurfNum ).Name );
    // ShowFatalError(state,  "Inside convection coefficient model number = " + TrimSigDigits( Surface( SurfNum ).IntConvHcModelEq ) );
    //}
}

void ManageOutsideAdaptiveConvectionAlgo(EnergyPlusData &state,
                                         int const SurfNum, // surface number for which coefficients are being calculated
                                         Real64 &Hc         // result for Hc Outside face, becomes HExt.
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calculates the convection coefficient for the outside face of a surface

    // METHODOLOGY EMPLOYED:
    // This routine implements an adaptive structure and classification system for outdoor
    //   It calls a series of separable worker routines

    DynamicExtConvSurfaceClassification(state, SurfNum);

    MapExtConvClassificationToHcModels(state, SurfNum);

    EvaluateExtHcModels(state, SurfNum, state.dataSurface->SurfOutConvHnModelEq(SurfNum), state.dataSurface->SurfOutConvHfModelEq(SurfNum), Hc);
}

void EvaluateIntHcModels(EnergyPlusData &state,
                         int const SurfNum,
                         int const ConvModelEquationNum,
                         Real64 &Hc // calculated Hc value
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // central case statement for calling inside convection models

    // METHODOLOGY EMPLOYED:
    //  - fills value for Hc by calling the appropriate convection model, usually as a function.
    //     preparation of argument values for the function calls is contained in each Case block (repeats)
    //  - also updates the reference air temperature type for use in the surface heat balance calcs

    // Using/Aliasing

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    Real64 tmpHc = 0.0;

    int const ZoneNum = Surface(SurfNum).Zone;
    Real64 &Tsurface = state.dataHeatBalSurf->TH(2, 1, SurfNum);
    Real64 &Tzone = state.dataHeatBalFanSys->MAT(ZoneNum);

    auto &HnFn = state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in;
    // now call appropriate function to calculate Hc
    {
        auto const SELECT_CASE_var(ConvModelEquationNum);

        if (SELECT_CASE_var == HcInt_UserCurve) {
            CalcUserDefinedInsideHcModel(state, SurfNum, state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum), tmpHc);
        } else if (SELECT_CASE_var == HcInt_ASHRAEVerticalWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [](double Tsurf, double Tamb, double, double, double) -> double { return CalcASHRAEVerticalWall(Tsurf - Tamb); };
            } else {
                tmpHc = CalcASHRAEVerticalWall((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_WaltonUnstableHorizontalOrTilt) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                    return CalcWaltonUnstableHorizontalOrTilt(Tsurf - Tamb, cosTilt);
                };
            } else {
                tmpHc = CalcWaltonUnstableHorizontalOrTilt((Tsurface - Tzone), Surface(SurfNum).CosTilt); // TODO verify CosTilt in vs out
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_WaltonStableHorizontalOrTilt) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                    return CalcWaltonStableHorizontalOrTilt(Tsurf - Tamb, cosTilt);
                };
            } else {
                tmpHc = CalcWaltonStableHorizontalOrTilt((Tsurface - Tzone), Surface(SurfNum).CosTilt); // TODO verify CosTilt in vs out
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserFloor) {
            Real64 AirChangeRate = CalcCeilingDiffuserACH(state, ZoneNum);
            Real64 AirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {

                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                    return CalcFisherPedersenCeilDiffuserFloor(state, AirChangeRate, Tsurf, Tamb, cosTilt, AirHumRat, Surface(SurfNum).Height);
                };
            } else {
                tmpHc = CalcFisherPedersenCeilDiffuserFloor(state,
                                                            AirChangeRate,
                                                            Tsurface,
                                                            Tzone,
                                                            Surface(SurfNum).CosTilt,
                                                            AirHumRat,
                                                            Surface(SurfNum).Height,
                                                            state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserCeiling) {
            Real64 AirChangeRate = CalcCeilingDiffuserACH(state, ZoneNum);
            Real64 AirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {

                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                    return CalcFisherPedersenCeilDiffuserCeiling(state, AirChangeRate, Tsurf, Tamb, cosTilt, AirHumRat, Surface(SurfNum).Height);
                };
            } else {
                tmpHc = CalcFisherPedersenCeilDiffuserCeiling(state,
                                                              AirChangeRate,
                                                              Tsurface,
                                                              Tzone,
                                                              Surface(SurfNum).CosTilt,
                                                              AirHumRat,
                                                              Surface(SurfNum).Height,
                                                              state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_FisherPedersenCeilDiffuserWalls) {
            Real64 AirChangeRate = CalcCeilingDiffuserACH(state, ZoneNum);
            Real64 AirHumRat = state.dataHeatBalFanSys->ZoneAirHumRatAvg(ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {

                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                    return CalcFisherPedersenCeilDiffuserWalls(state, AirChangeRate, Tsurf, Tamb, cosTilt, AirHumRat, Surface(SurfNum).Height);
                };
            } else {
                tmpHc = CalcFisherPedersenCeilDiffuserWalls(state,
                                                            AirChangeRate,
                                                            Tsurface,
                                                            Tzone,
                                                            Surface(SurfNum).CosTilt,
                                                            AirHumRat,
                                                            Surface(SurfNum).Height,
                                                            state.dataConstruction->Construct(Surface(SurfNum).Construction).TypeIsWindow);
            }
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double, double, double, double, double) -> double { return tmpHc; };
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_AlamdariHammondStableHorizontal) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 HorizHydrDiam = state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcAlamdariHammondStableHorizontal(Tsurf - Tamb, HorizHydrDiam);
                };
            } else {
                tmpHc =
                    CalcAlamdariHammondStableHorizontal(state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), SurfNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_AlamdariHammondVerticalWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 WallHeight = state.dataSurface->SurfIntConvZoneWallHeight(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcAlamdariHammondVerticalWall(Tsurf - Tamb, WallHeight);
                };
            } else {
                tmpHc = CalcAlamdariHammondVerticalWall(state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneWallHeight(SurfNum), SurfNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_AlamdariHammondUnstableHorizontal) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 HorizHydrDiam = state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcAlamdariHammondStableHorizontal(Tsurf - Tamb, HorizHydrDiam);
                };
            } else {
                tmpHc = CalcAlamdariHammondUnstableHorizontal(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), SurfNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KhalifaEq3WallAwayFromHeat) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKhalifaEq3WallAwayFromHeat(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKhalifaEq3WallAwayFromHeat((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KhalifaEq4CeilingAwayFromHeat) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKhalifaEq4CeilingAwayFromHeat(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKhalifaEq4CeilingAwayFromHeat((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KhalifaEq5WallNearHeat) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKhalifaEq5WallsNearHeat(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKhalifaEq5WallsNearHeat((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KhalifaEq6NonHeatedWalls) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKhalifaEq6NonHeatedWalls(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKhalifaEq6NonHeatedWalls((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KhalifaEq7Ceiling) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKhalifaEq7Ceiling(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKhalifaEq7Ceiling((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_AwbiHattonHeatedFloor) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 HorizHydrDiam = state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcAwbiHattonHeatedFloor(Tsurf - Tamb, HorizHydrDiam);
                };
            } else {
                tmpHc = CalcAwbiHattonHeatedFloor((Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_AwbiHattonHeatedWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 HorizHydrDiam = state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcAwbiHattonHeatedWall(Tsurf - Tamb, HorizHydrDiam);
                };
            } else {
                tmpHc = CalcAwbiHattonHeatedWall((Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedAssistingWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedAssistedWall(Tsurf - Tamb,
                                                                   state.dataSurface->SurfIntConvZoneWallHeight(SurfNum),
                                                                   Tsurf,
                                                                   CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                   CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedAssistedWall(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneWallHeight(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedOppossingWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedOpposingWall(Tsurf - Tamb,
                                                                   state.dataSurface->SurfIntConvZoneWallHeight(SurfNum),
                                                                   Tsurf,
                                                                   CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                   CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedOpposingWall(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneWallHeight(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedStableCeiling) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedStableCeiling(Tsurf - Tamb,
                                                                    state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum),
                                                                    Tsurf,
                                                                    CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                    CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedStableCeiling(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedUnstableCeiling) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedUnstableCeiling(Tsurf - Tamb,
                                                                      state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum),
                                                                      Tsurf,
                                                                      CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                      CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedUnstableCeiling(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedStableFloor) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedStableFloor(Tsurf - Tamb,
                                                                  state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum),
                                                                  Tsurf,
                                                                  CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                  CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedStableFloor(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_BeausoleilMorrisonMixedUnstableFloor) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcBeausoleilMorrisonMixedUnstableFloor(Tsurf - Tamb,
                                                                    state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum),
                                                                    Tsurf,
                                                                    CalcZoneSupplyAirTemp(state, ZoneNum),
                                                                    CalcZoneSystemACH(state, ZoneNum));
                };
            } else {
                tmpHc = CalcBeausoleilMorrisonMixedUnstableFloor(
                    state, (Tsurface - Tzone), state.dataSurface->SurfIntConvZoneHorizHydrDiam(SurfNum), Tsurface, ZoneNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_FohannoPolidoriVerticalWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                Real64 QdotConvection = -state.dataHeatBalSurf->QdotConvInRepPerArea(SurfNum);
                Real64 WallHeight = state.dataSurface->SurfIntConvZoneWallHeight(SurfNum);
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                    return CalcFohannoPolidoriVerticalWall(Tsurf - Tamb,
                                                           WallHeight,
                                                           Tsurf -
                                                               DataGlobalConstants::KelvinConv, // Kiva already uses Kelvin, but algorithm expects C
                                                           QdotConvection);
                };
            } else {
                tmpHc = CallCalcFohannoPolidoriVerticalWall(state,
                                                            (Tsurface - Tzone),
                                                            state.dataSurface->SurfIntConvZoneWallHeight(SurfNum),
                                                            Tsurface,
                                                            -state.dataHeatBalSurf->QdotConvInRepPerArea(SurfNum),
                                                            SurfNum);
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_KaradagChilledCeiling) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcKaradagChilledCeiling(Tsurf - Tamb); };
            } else {
                tmpHc = CalcKaradagChilledCeiling((Tsurface - Tzone));
            }
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_ISO15099Windows) {
            CalcISO15099WindowIntConvCoeff(state, SurfNum, Tsurface, Tzone);
            tmpHc = state.dataHeatBal->HConvIn(SurfNum);
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserWindow) {
            tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWindow(state.dataSurface->SurfIntConvZonePerimLength(SurfNum),
                                                                state.dataSurface->SurfIntConvWindowWallRatio(SurfNum),
                                                                state.dataSurface->SurfIntConvWindowLocation(SurfNum),
                                                                ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double, double, double, double, double) -> double { return tmpHc; };
            }
            int ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
            if (ZoneNode > 0) {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneSupplyAirTemp;
            } else {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
            }
        } else if (SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserWalls) {
            tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWall(
                state.dataSurface->SurfIntConvZonePerimLength(SurfNum), state.dataSurface->SurfIntConvWindowLocation(SurfNum), ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double, double, double, double, double) -> double { return tmpHc; };
            }
            int ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
            if (ZoneNode > 0) {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneSupplyAirTemp;
            } else {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
            }
        } else if (SELECT_CASE_var == HcInt_GoldsteinNovoselacCeilingDiffuserFloor) {
            tmpHc = CalcGoldsteinNovoselacCeilingDiffuserFloor(state.dataSurface->SurfIntConvZonePerimLength(SurfNum), ZoneNum);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=](double, double, double, double, double) -> double { return tmpHc; };
            }
            int ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
            if (ZoneNode > 0) {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneSupplyAirTemp;
            } else {
                state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
            }
        }
    }

    if (tmpHc < AdaptiveHcInsideLowLimit) tmpHc = AdaptiveHcInsideLowLimit;

    Hc = tmpHc;
}

void EvaluateExtHcModels(EnergyPlusData &state, int const SurfNum, int const NaturalConvModelEqNum, int const ForcedConvModelEqNum, Real64 &Hc)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // central case statement for evaluating exterior specific convection models

    // METHODOLOGY EMPLOYED:
    // separated out long case statement for selecting models.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 Hf(0.0); // the forced, or wind driven portion of film coefficient
    Real64 Hn(0.0); // the natural, or bouyancy driven portion of film coefficient
    Real64 SurfWindSpeed;
    Real64 SurfWindDir;
    Real64 HydraulicDiameter;

    // Kiva callback functions
    Kiva::ForcedConvectionTerm HfTermFn;
    Kiva::ConvectionAlgorithm HfFn(KIVA_CONST_CONV(0.0));
    Kiva::ConvectionAlgorithm HnFn(KIVA_CONST_CONV(0.0));

    auto &Surface(state.dataSurface->Surface);
    auto &QdotConvOutRepPerArea(state.dataHeatBalSurf->QdotConvOutRepPerArea);
    auto &TH(state.dataHeatBalSurf->TH);

    // first call Hn models
    {
        auto const SELECT_CASE_var(NaturalConvModelEqNum);

        if (SELECT_CASE_var == HcExt_None) {
            Hn = 0.0;
            HnFn = KIVA_CONST_CONV(0.0);
        } else if (SELECT_CASE_var == HcExt_UserCurve) {
            CalcUserDefinedOutsideHcModel(state, SurfNum, state.dataSurface->SurfOutConvHnUserCurveIndex(SurfNum), Hn);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HnFn = [=, &state](double Tsurf, double Tamb, double HfTerm, double Roughness, double CosTilt) -> double {
                    // Remove Hfterm since this is only used for the natural convection portion
                    return state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out(Tsurf, Tamb, HfTerm, Roughness, CosTilt) - HfTerm;
                };
            }
        } else if (SELECT_CASE_var == HcExt_NaturalASHRAEVerticalWall) {
            Hn = CalcASHRAEVerticalWall((TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)));
            HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double { return CalcASHRAEVerticalWall(Tsurf - Tamb); };
        } else if (SELECT_CASE_var == HcExt_NaturalWaltonUnstableHorizontalOrTilt) {
            Hn = CalcWaltonUnstableHorizontalOrTilt((TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)),
                                                    Surface(SurfNum).CosTilt); // TODO verify CosTilt in vs out
            HnFn = [=](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                return CalcWaltonUnstableHorizontalOrTilt(Tsurf - Tamb, cosTilt);
            };
        } else if (SELECT_CASE_var == HcExt_NaturalWaltonStableHorizontalOrTilt) {
            Hn = CalcWaltonStableHorizontalOrTilt((TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)),
                                                  Surface(SurfNum).CosTilt); // TODO verify CosTilt in vs out
            HnFn = [=](double Tsurf, double Tamb, double, double, double cosTilt) -> double {
                return CalcWaltonStableHorizontalOrTilt(Tsurf - Tamb, cosTilt);
            };
        } else if (SELECT_CASE_var == HcExt_AlamdariHammondVerticalWall) {
            Real64 FaceHeight = state.dataSurface->SurfOutConvFaceHeight(SurfNum);
            Hn = CalcAlamdariHammondVerticalWall(state, (TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)), FaceHeight, SurfNum);
            HnFn = [=](double Tsurf, double Tamb, double, double, double) -> double {
                return CalcAlamdariHammondVerticalWall(Tsurf - Tamb, FaceHeight);
            };
        } else if (SELECT_CASE_var == HcExt_FohannoPolidoriVerticalWall) {
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                // Not compatible with Kiva (Exterior surfaces in Kiva are not currently reported. Also need to add cell-level convection.)
                ShowFatalError(state, "Fohanno Polidori convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
            }
            Hn = CallCalcFohannoPolidoriVerticalWall(state,
                                                     (TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)),
                                                     state.dataSurface->SurfOutConvFaceHeight(SurfNum),
                                                     TH(1, 1, SurfNum),
                                                     -QdotConvOutRepPerArea(SurfNum),
                                                     SurfNum);
        } else if (SELECT_CASE_var == HcExt_AlamdariHammondStableHorizontal) {
            if (state.dataSurface->SurfOutConvFacePerimeter(SurfNum) > 0.0) {
                HydraulicDiameter = 4.0 * state.dataSurface->SurfOutConvFaceArea(SurfNum) / state.dataSurface->SurfOutConvFacePerimeter(SurfNum);
            } else {
                HydraulicDiameter = std::sqrt(state.dataSurface->SurfOutConvFaceArea(SurfNum));
            }
            Hn = CalcAlamdariHammondStableHorizontal(
                state, (TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)), HydraulicDiameter, SurfNum);
        } else if (SELECT_CASE_var == HcExt_AlamdariHammondUnstableHorizontal) {
            if (state.dataSurface->SurfOutConvFacePerimeter(SurfNum) > 0.0) {
                HydraulicDiameter = 4.0 * state.dataSurface->SurfOutConvFaceArea(SurfNum) / state.dataSurface->SurfOutConvFacePerimeter(SurfNum);
            } else {
                HydraulicDiameter = std::sqrt(state.dataSurface->SurfOutConvFaceArea(SurfNum));
            }
            Hn = CalcAlamdariHammondUnstableHorizontal(
                state, (TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)), HydraulicDiameter, SurfNum);
        }
    }

    {

        if (!Surface(SurfNum).ExtWind) {
            SurfWindSpeed = 0.0; // No wind exposure
        } else if (Surface(SurfNum).Class == SurfaceClass::Window && state.dataSurface->SurfWinShadingFlag(SurfNum) == WinShadingType::ExtShade) {
            SurfWindSpeed = 0.0; // Assume zero wind speed at outside glass surface of window with exterior shade
        } else {
            SurfWindSpeed = state.dataSurface->SurfOutWindSpeed(SurfNum);
        }

        int Roughness = state.dataMaterial->Material(state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1)).Roughness;

        auto const SELECT_CASE_var(ForcedConvModelEqNum);

        if (SELECT_CASE_var == HcExt_None) {
            Hf = 0.0;
            HfTermFn = KIVA_HF_DEF;
            HfFn = KIVA_CONST_CONV(0.0);
        } else if (SELECT_CASE_var == HcExt_UserCurve) {
            CalcUserDefinedOutsideHcModel(state, SurfNum, state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum), Hf);
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                HfTermFn = state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f;
                HnFn = state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out;
            }
        } else if (SELECT_CASE_var == HcExt_SparrowWindward) {
            Hf = CalcSparrowWindward(state,
                                     Roughness,
                                     state.dataSurface->SurfOutConvFacePerimeter(SurfNum),
                                     state.dataSurface->SurfOutConvFaceArea(SurfNum),
                                     SurfWindSpeed,
                                     SurfNum);

            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                // Assume very large area for grade (relative to perimeter).
                const double area = 9999999.;
                const double perim = 1.;
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcSparrowWindward(Roughness, perim, area, windSpeed); };
            } else {
                if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                    auto &fnd = state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].get_instance(0).first->foundation;
                    const double length = fnd.netPerimeter;
                    const double height = fnd.wall.heightAboveGrade;
                    const double area = length * height;
                    const double perim = 2.0 * (length + height);
                    HfTermFn = [=](double, double, double, double windSpeed) -> double {
                        // Average windward and leeward since all walls use same algorithm
                        double windwardHf = CalcSparrowWindward(Roughness, perim, area, windSpeed);
                        double leewardHf = CalcSparrowLeeward(Roughness, perim, area, windSpeed);
                        return (windwardHf + leewardHf) / 2.0;
                    };
                }
            }
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_SparrowLeeward) {
            Hf = CalcSparrowLeeward(state,
                                    Roughness,
                                    state.dataSurface->SurfOutConvFacePerimeter(SurfNum),
                                    state.dataSurface->SurfOutConvFaceArea(SurfNum),
                                    SurfWindSpeed,
                                    SurfNum);
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                // Assume very large area for grade (relative to perimeter).
                const double area = 9999999.;
                const double perim = 1.;
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcSparrowLeeward(Roughness, perim, area, windSpeed); };
            } else {
                if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                    auto &fnd = state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].get_instance(0).first->foundation;
                    const double length = fnd.netPerimeter;
                    const double height = fnd.wall.heightAboveGrade;
                    const double area = length * height;
                    const double perim = 2.0 * (length + height);
                    HfTermFn = [=](double, double, double, double windSpeed) -> double {
                        // Average windward and leeward since all walls use same algorithm
                        double windwardHf = CalcSparrowWindward(Roughness, perim, area, windSpeed);
                        double leewardHf = CalcSparrowLeeward(Roughness, perim, area, windSpeed);
                        return (windwardHf + leewardHf) / 2.0;
                    };
                }
            }
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_MoWiTTWindward) {
            Hf = CalcMoWITTWindward(TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum), SurfWindSpeed);
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcMoWITTForcedWindward(windSpeed); };
            } else {
                HfTermFn = [=](double, double, double, double windSpeed) -> double {
                    // Average windward and leeward since all walls use same algorithm
                    double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                    double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                    return (windwardHf + leewardHf) / 2.0;
                };
            }
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_MoWiTTLeeward) {
            Hf = CalcMoWITTLeeward((TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)), SurfWindSpeed);
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcMoWITTForcedLeeward(windSpeed); };
            } else {
                HfTermFn = [=](double, double, double, double windSpeed) -> double {
                    // Average windward and leeward since all walls use same algorithm
                    double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                    double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                    return (windwardHf + leewardHf) / 2.0;
                };
            }
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_DOE2Windward) {
            Hf = CalcDOE2Windward(
                TH(1, 1, SurfNum), state.dataSurface->SurfOutDryBulbTemp(SurfNum), Surface(SurfNum).CosTilt, SurfWindSpeed, Roughness);
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcMoWITTForcedWindward(windSpeed); };
            } else {
                HfTermFn = [=](double, double, double, double windSpeed) -> double {
                    // Average windward and leeward since all walls use same algorithm
                    double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                    double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                    return (windwardHf + leewardHf) / 2.0;
                };
            }
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_DOE2Leeward) {
            Hf = CalcDOE2Leeward(
                TH(1, 1, SurfNum), state.dataSurface->SurfOutDryBulbTemp(SurfNum), Surface(SurfNum).CosTilt, SurfWindSpeed, Roughness);
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcMoWITTForcedWindward(windSpeed); };
            } else {
                HfTermFn = [=](double, double, double, double windSpeed) -> double {
                    // Average windward and leeward since all walls use same algorithm
                    double windwardHf = CalcMoWITTForcedWindward(windSpeed);
                    double leewardHf = CalcMoWITTForcedLeeward(windSpeed);
                    return (windwardHf + leewardHf) / 2.0;
                };
            }
            HfFn = [=](double Tsurf, double Tamb, double hfTerm, double, double cosTilt) -> double {
                return CalcDOE2Forced(Tsurf, Tamb, cosTilt, hfTerm, Roughness);
            };
        } else if (SELECT_CASE_var == HcExt_NusseltJurges) {
            Hf = CalcNusseltJurges(SurfWindSpeed);
            HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcNusseltJurges(windSpeed); };
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_McAdams) {
            Hf = CalcMcAdams(SurfWindSpeed);
            HfTermFn = [=](double, double, double, double windSpeed) -> double { return CalcMcAdams(windSpeed); };
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_Mitchell) {
            Hf = CalcMitchell(state, SurfWindSpeed, state.dataConvectionCoefficient->CubeRootOfOverallBuildingVolume, SurfNum);
            HfTermFn = [&](double, double, double, double windSpeed) -> double {
                return CalcMitchell(windSpeed, state.dataConvectionCoefficient->CubeRootOfOverallBuildingVolume);
            };
            HfFn = [](double, double, double HfTerm, double, double) -> double { return HfTerm; };
        } else if (SELECT_CASE_var == HcExt_ClearRoof) {
            SurfWindDir = state.dataSurface->SurfOutWindDir(SurfNum);
            Hf = CalcClearRoof(state,
                               SurfNum,
                               TH(1, 1, SurfNum),
                               state.dataSurface->SurfOutDryBulbTemp(SurfNum),
                               SurfWindSpeed,
                               SurfWindDir,
                               state.dataSurface->SurfOutConvFaceArea(SurfNum),
                               state.dataSurface->SurfOutConvFacePerimeter(SurfNum));
            HfTermFn = [=](double, double, double, double windSpeed) -> double { return windSpeed; };
            if (Surface(SurfNum).Class == SurfaceClass::Floor) { // used for exterior grade
                // Assume very large area for grade (relative to perimeter).
                const double area = 9999999.;
                const double perim = 1.;
                HfFn = [=, &state](double Tsurf, double Tamb, double hfTerm, double, double) -> double {
                    return CalcClearRoof(state, Tsurf, Tamb, hfTerm, area, perim, Roughness);
                };
            } else {
                if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                    auto &fnd = state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].get_instance(0).first->foundation;
                    const double length = fnd.netPerimeter;
                    const double height = fnd.wall.heightAboveGrade;
                    const double area = length * height;
                    const double perim = 2.0 * (length + height);
                    HfFn = [=, &state](double Tsurf, double Tamb, double hfTerm, double, double) -> double {
                        return CalcClearRoof(state, Tsurf, Tamb, hfTerm, area, perim, Roughness);
                    };
                }
            }
        } else if (SELECT_CASE_var == HcExt_BlockenWindward) {
            Hf = CalcBlockenWindward(state.dataEnvrn->WindSpeed, state.dataEnvrn->WindDir, Surface(SurfNum).Azimuth);
            // Not compatible with Kiva (doesn't use weather station windspeed)
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                ShowFatalError(state, "Blocken Windward convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
            }
        } else if (SELECT_CASE_var == HcExt_EmmelVertical) {
            Hf = CalcEmmelVertical(state, state.dataEnvrn->WindSpeed, state.dataEnvrn->WindDir, Surface(SurfNum).Azimuth, SurfNum);
            // Not compatible with Kiva (doesn't use weather station windspeed)
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                ShowFatalError(state, "Emmel Vertical convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
            }
        } else if (SELECT_CASE_var == HcExt_EmmelRoof) {
            Hf = CalcEmmelRoof(
                state, state.dataEnvrn->WindSpeed, state.dataEnvrn->WindDir, state.dataConvectionCoefficient->RoofLongAxisOutwardAzimuth, SurfNum);
            // Not compatible with Kiva (doesn't use weather station windspeed)
            if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
                ShowFatalError(state, "Emmel Roof convection model not applicable for foundation surface =" + Surface(SurfNum).Name);
            }
        }
    }

    Hc = Hf + Hn;

    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = HfTermFn;
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out =
            [=](double Tsurf, double Tamb, double HfTerm, double Roughness, double cosTilt) -> double {
            Real64 HcExt = HfFn(Tsurf, Tamb, HfTerm, Roughness, cosTilt) + HnFn(Tsurf, Tamb, HfTerm, Roughness, cosTilt);
            if (HcExt < AdaptiveHcOutsideLowLimit) HcExt = AdaptiveHcOutsideLowLimit;
            return HcExt;
        };
        Hc = 0.0; // Not used in Kiva
    }

    if (Hc < AdaptiveHcOutsideLowLimit) Hc = AdaptiveHcOutsideLowLimit;
}

void DynamicExtConvSurfaceClassification(EnergyPlusData &state, int const SurfNum) // surface number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   August 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // METHODOLOGY EMPLOYED:
    // Decide surface classification based on wind and bouyancy, class, orientation

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 DeltaTemp(0.0);
    Real64 surfWindDir;

    auto &Surface(state.dataSurface->Surface);

    surfWindDir = state.dataSurface->SurfOutWindDir(SurfNum);

    if (Surface(SurfNum).Class == SurfaceClass::Roof ||
        (Surface(SurfNum).Class == SurfaceClass::Floor && Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) // Applies to exterior grade
    ) {
        if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
            DeltaTemp = state.dataSurfaceGeometry->kivaManager.surfaceMap[SurfNum].results.Tconv - state.dataSurface->SurfOutDryBulbTemp(SurfNum);
        } else {
            DeltaTemp = state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum);
        }

        if (DeltaTemp < 0.0) {
            state.dataSurface->SurfOutConvClassification(SurfNum) = OutConvClass_RoofStable;
        } else {
            state.dataSurface->SurfOutConvClassification(SurfNum) = OutConvClass_RoofUnstable;
        }

    } else {

        if (Windward(Surface(SurfNum).CosTilt, Surface(SurfNum).Azimuth, surfWindDir)) {
            state.dataSurface->SurfOutConvClassification(SurfNum) = OutConvClass_WindwardVertWall;
        } else {
            state.dataSurface->SurfOutConvClassification(SurfNum) = OutConvClass_LeewardVertWall;
        }
    }
}

void MapExtConvClassificationToHcModels(EnergyPlusData &state, int const SurfNum) // surface number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na
    {
        auto const SELECT_CASE_var(state.dataSurface->SurfOutConvClassification(SurfNum));

        if (SELECT_CASE_var == OutConvClass_WindwardVertWall) {
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) =
                state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardEqNum;
            if (state.dataSurface->SurfOutConvHfModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallWindwardUserCurveNum;
            }
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) = state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
            if (state.dataSurface->SurfOutConvHnModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHnUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == OutConvClass_LeewardVertWall) {
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) =
                state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardEqNum;
            if (state.dataSurface->SurfOutConvHfModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindWallLeewardUserCurveNum;
            }
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) = state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallEqNum;
            if (state.dataSurface->SurfOutConvHnModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatVertWallUserCurveNum;
            }

        } else if (SELECT_CASE_var == OutConvClass_RoofStable) {
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) = state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
            if (state.dataSurface->SurfOutConvHfModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum;
            }
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) =
                state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizEqNum;
            if (state.dataSurface->SurfOutConvHnModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == OutConvClass_RoofUnstable) {
            state.dataSurface->SurfOutConvHfModelEq(SurfNum) = state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofEqNum;
            if (state.dataSurface->SurfOutConvHfModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HWindHorizRoofUserCurveNum;
            }
            state.dataSurface->SurfOutConvHnModelEq(SurfNum) =
                state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUnstableHorizEqNum;
            if (state.dataSurface->SurfOutConvHnModelEq(SurfNum) == HcExt_UserCurve) {
                state.dataSurface->SurfOutConvHfUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->OutsideFaceAdaptiveConvectionAlgo.HNatUstableHorizUserCurveNum;
            }
        } else {
            ShowSevereError(state,
                            format("MapExtConvClassificationToHcModels: caught unknown outdoor surfce classification:{}",
                                   state.dataSurface->SurfOutConvClassification(SurfNum)));
        }
    }
}

void DynamicIntConvSurfaceClassification(EnergyPlusData &state, int const SurfNum) // surface number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR        Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // collects dynamic updates needed for adaptive convectin algorithm

    // METHODOLOGY EMPLOYED:
    // Decide flow regime to set IntConvClassification
    //  done by zone using the following rules

    // Using zone flow regime, and surface's characteristics assign IntConvHcModelEq

    // Using/Aliasing
    using namespace DataZoneEquipment;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdpPb;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const g(9.81);                     // gravity constant (m/s**2)
    Real64 const v(15.89e-6);                 // kinematic viscosity (m**2/s) for air at 300 K
    Real64 const ActiveDelTempThreshold(1.5); // deg C, temperature difference for surfaces to be considered "active"

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNum(0);
    int PriorityEquipOn(0);
    Array1D_int HeatingPriorityStack({0, 10}, 0);
    Array1D_int CoolingPriorityStack({0, 10}, 0);
    Array1D_int FlowRegimeStack({0, 10}, 0);
    int EquipNum(0);
    int ZoneNode(0);
    int EquipOnCount(0);
    int EquipOnLoop(0);
    int thisZoneInletNode(0);
    int FinalFlowRegime(0);
    Real64 Tmin(0.0);       // temporary min surf temp
    Real64 Tmax(0.0);       // temporary max surf temp
    Real64 GrH(0.0);        // Grashof number for zone height H
    Real64 Re(0.0);         // Reynolds number for zone air system flow
    Real64 Ri(0.0);         // Richardson Number, Gr/Re**2 for determining mixed regime
    Real64 AirDensity(0.0); // temporary zone air density
    Real64 DeltaTemp(0.0);  // temporary temperature difference (Tsurf - Tair)
    int SurfLoop;           // local for separate looping across surfaces in the zone that has SurfNum

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);
    auto &TH(state.dataHeatBalSurf->TH);

    EquipOnCount = 0;
    ZoneNum = Surface(SurfNum).Zone;
    ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
    FlowRegimeStack = 0;

    // HVAC connections
    if (!Zone(ZoneNum).IsControlled) { // no HVAC control
        FlowRegimeStack(0) = InConvFlowRegime_A3;
    } else { // is controlled, lets see by how and if that means is currently active

        if (!(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex > 0) || state.dataGlobal->SysSizingCalc ||
            state.dataGlobal->ZoneSizingCalc || !state.dataZoneEquip->ZoneEquipSimulatedOnce) {
            FlowRegimeStack(0) = InConvFlowRegime_A3;
        } else {

            for (EquipNum = 1;
                 EquipNum <= state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).NumOfEquipTypes;
                 ++EquipNum) {

                {
                    auto const SELECT_CASE_var(
                        state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).EquipType_Num(EquipNum));

                    if ((SELECT_CASE_var == AirDistUnit_Num) || (SELECT_CASE_var == PurchasedAir_Num)) { // central air equipment
                        if (!(allocated(state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                            .EquipData(EquipNum)
                                            .OutletNodeNums)))
                            continue;
                        // get inlet node, not zone node if possible
                        thisZoneInletNode = state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .EquipData(EquipNum)
                                                .OutletNodeNums(1);
                        if (thisZoneInletNode > 0) {
                            if (state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate > 0.0) {
                                EquipOnCount = min(EquipOnCount + 1, 10);
                                FlowRegimeStack(EquipOnCount) = InConvFlowRegime_C;
                                HeatingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .HeatingPriority(EquipNum);
                                CoolingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .CoolingPriority(EquipNum);
                            }
                        } else {
                            if (state.dataLoopNodes->Node(ZoneNode).MassFlowRate > 0.0) {
                                EquipOnCount = min(EquipOnCount + 1, 10);
                                FlowRegimeStack(EquipOnCount) = InConvFlowRegime_C;
                                HeatingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .HeatingPriority(EquipNum);
                                CoolingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .CoolingPriority(EquipNum);
                            }
                        }
                    } else if ((SELECT_CASE_var == WindowAC_Num) || (SELECT_CASE_var == PkgTermHPAirToAir_Num) ||
                               (SELECT_CASE_var == PkgTermACAirToAir_Num) || (SELECT_CASE_var == ZoneDXDehumidifier_Num) ||
                               (SELECT_CASE_var == PkgTermHPWaterToAir_Num) || (SELECT_CASE_var == FanCoil4Pipe_Num) ||
                               (SELECT_CASE_var == UnitVentilator_Num) || (SELECT_CASE_var == UnitHeater_Num) ||
                               (SELECT_CASE_var == OutdoorAirUnit_Num)) {
                        if (!(allocated(state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                            .EquipData(EquipNum)
                                            .OutletNodeNums)))
                            continue;
                        thisZoneInletNode = state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .EquipData(EquipNum)
                                                .OutletNodeNums(1);
                        if (thisZoneInletNode > 0) {
                            if (state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate > 0.0) {
                                EquipOnCount = min(EquipOnCount + 1, 10);
                                FlowRegimeStack(EquipOnCount) = InConvFlowRegime_D;
                                HeatingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .HeatingPriority(EquipNum);
                                CoolingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .CoolingPriority(EquipNum);
                            }
                        } else {
                            if (state.dataLoopNodes->Node(ZoneNode).MassFlowRate > 0.0) {
                                EquipOnCount = min(EquipOnCount + 1, 10);
                                FlowRegimeStack(EquipOnCount) = InConvFlowRegime_D;
                                HeatingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .HeatingPriority(EquipNum);
                                CoolingPriorityStack(EquipOnCount) =
                                    state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                        .CoolingPriority(EquipNum);
                            }
                        }
                    } else if ((SELECT_CASE_var == CoolingPanel_Num) || (SELECT_CASE_var == BBSteam_Num) ||
                               (SELECT_CASE_var == BBWaterConvective_Num) || (SELECT_CASE_var == BBElectricConvective_Num) ||
                               (SELECT_CASE_var == BBWater_Num)) {

                        if (state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).EquipData(EquipNum).ON) {
                            EquipOnCount = min(EquipOnCount + 1, 10);
                            FlowRegimeStack(EquipOnCount) = InConvFlowRegime_B;
                            HeatingPriorityStack(EquipOnCount) =
                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                    .HeatingPriority(EquipNum);
                            CoolingPriorityStack(EquipOnCount) =
                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                    .CoolingPriority(EquipNum);
                        }
                    } else if ((SELECT_CASE_var == BBElectric_Num) || (SELECT_CASE_var == HiTempRadiant_Num)) {
                        if (state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).EquipData(EquipNum).ON) {
                            EquipOnCount = min(EquipOnCount + 1, 10);
                            FlowRegimeStack(EquipOnCount) = InConvFlowRegime_B;
                            HeatingPriorityStack(EquipOnCount) =
                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                    .HeatingPriority(EquipNum);
                            CoolingPriorityStack(EquipOnCount) =
                                state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                    .CoolingPriority(EquipNum);
                        }
                    } else if ((SELECT_CASE_var == VentilatedSlab_Num) || (SELECT_CASE_var == LoTempRadiant_Num)) {

                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InFloorActiveElement) {
                            for (SurfLoop = Zone(ZoneNum).HTSurfaceFirst; SurfLoop <= Zone(ZoneNum).HTSurfaceLast; ++SurfLoop) {
                                if (!state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfLoop)) continue;
                                if (Surface(SurfLoop).Class == SurfaceClass::Floor) {
                                    DeltaTemp = TH(2, 1, SurfLoop) - state.dataHeatBalFanSys->MAT(ZoneNum);
                                    if (DeltaTemp > ActiveDelTempThreshold) { // assume heating with floor
                                        // system ON is not enough because floor surfaces can continue to heat because of thermal capacity
                                        EquipOnCount = min(EquipOnCount + 1, 10);
                                        FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A1;
                                        HeatingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                        CoolingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        break;
                                    }
                                }
                            }
                        }

                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InCeilingActiveElement) {
                            for (SurfLoop = Zone(ZoneNum).HTSurfaceFirst; SurfLoop <= Zone(ZoneNum).HTSurfaceLast; ++SurfLoop) {
                                if (!state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfLoop)) continue;
                                if (Surface(SurfLoop).Class == SurfaceClass::Roof) {
                                    DeltaTemp = TH(2, 1, SurfLoop) - state.dataHeatBalFanSys->MAT(ZoneNum);
                                    if (DeltaTemp < ActiveDelTempThreshold) { // assume cooling with ceiling
                                        // system ON is not enough because  surfaces can continue to cool because of thermal capacity
                                        EquipOnCount = min(EquipOnCount + 1, 10);
                                        FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A1;
                                        HeatingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                        CoolingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        break;
                                    }
                                }
                            }
                        }

                        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InWallActiveElement) {
                            for (SurfLoop = Zone(ZoneNum).HTSurfaceFirst; SurfLoop <= Zone(ZoneNum).HTSurfaceLast; ++SurfLoop) {
                                if (!state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfLoop)) continue;
                                if (Surface(SurfLoop).Class == SurfaceClass::Wall || Surface(SurfLoop).Class == SurfaceClass::Door) {
                                    DeltaTemp = TH(2, 1, SurfLoop) - state.dataHeatBalFanSys->MAT(ZoneNum);
                                    if (DeltaTemp > ActiveDelTempThreshold) { // assume heating with wall panel
                                        // system ON is not enough because  surfaces can continue to heat because of thermal capacity
                                        EquipOnCount = min(EquipOnCount + 1, 10);
                                        FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A2;
                                        HeatingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                        CoolingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                    } else { // not heating, no special models wall cooling so use simple bouyancy
                                        EquipOnCount = min(EquipOnCount + 1, 10);
                                        FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A3;
                                        HeatingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                        CoolingPriorityStack(EquipOnCount) =
                                            state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                    }
                                }
                            }
                        }
                    }
                }
            } // loop over equipment for this zone
        }
    }

    // now select which equipment type is dominant compared to all those that are ON
    if (EquipOnCount > 0) {
        if (state.dataHeatBal->SNLoadPredictedRate(ZoneNum) >= 0.0) { // heating load
            PriorityEquipOn = 1;
            for (EquipOnLoop = 1; EquipOnLoop <= EquipOnCount; ++EquipOnLoop) {
                // assume highest priority/first sim order is dominant for flow regime
                if (HeatingPriorityStack(EquipOnLoop) < HeatingPriorityStack(PriorityEquipOn)) {
                    PriorityEquipOn = EquipOnLoop;
                }
            }
        } else if (state.dataHeatBal->SNLoadPredictedRate(ZoneNum) < 0.0) { // cooling load
            PriorityEquipOn = 1;
            for (EquipOnLoop = 1; EquipOnLoop <= EquipOnCount; ++EquipOnLoop) {
                // assume highest priority/first sim order is dominant for flow regime
                if (CoolingPriorityStack(EquipOnLoop) < CoolingPriorityStack(PriorityEquipOn)) {
                    PriorityEquipOn = EquipOnLoop;
                }
            }
        }
        FinalFlowRegime = FlowRegimeStack(PriorityEquipOn);
    } else {
        // no equipment on, so simple bouyancy flow regime
        FinalFlowRegime = InConvFlowRegime_A3;
    }

    // now if flow regimes C or D, then check for Mixed regime or very low flow rates
    if ((FinalFlowRegime == InConvFlowRegime_C) || (FinalFlowRegime == InConvFlowRegime_D)) {

        // Calculate Grashof, Reynolds, and Richardson numbers for the zone
        // Grashof for zone air based on largest delta T between surfaces and zone height
        Tmin = minval(TH(2, 1, {Zone(ZoneNum).HTSurfaceFirst, Zone(ZoneNum).HTSurfaceLast}));
        Tmax = maxval(TH(2, 1, {Zone(ZoneNum).HTSurfaceFirst, Zone(ZoneNum).HTSurfaceLast}));
        GrH = (g * (Tmax - Tmin) * pow_3(Zone(ZoneNum).CeilingHeight)) /
              ((state.dataHeatBalFanSys->MAT(ZoneNum) + DataGlobalConstants::KelvinConv) * pow_2(v));

        // Reynolds number = Vdot supply / v * cube root of zone volume (Goldstein and Noveselac 2010)
        if (state.dataLoopNodes->Node(ZoneNode).MassFlowRate > 0.0) {
            AirDensity = PsyRhoAirFnPbTdbW(state,
                                           state.dataEnvrn->OutBaroPress,
                                           state.dataLoopNodes->Node(ZoneNode).Temp,
                                           PsyWFnTdpPb(state, state.dataLoopNodes->Node(ZoneNode).Temp, state.dataEnvrn->OutBaroPress));
            Re = state.dataLoopNodes->Node(ZoneNode).MassFlowRate / (v * AirDensity * std::pow(Zone(ZoneNum).Volume, OneThird));
        } else {
            Re = 0.0;
        }

        if (Re > 0.0) {
            Ri = GrH / pow_2(Re); // Richardson Number
            if (Ri > 10.0) {      // natural convection expected
                FinalFlowRegime = InConvFlowRegime_A3;
            } else if (Ri < 0.1) { // forced
                // no change, already a forced regime
            } else { // mixed
                FinalFlowRegime = InConvFlowRegime_E;
            }
        } else { // natural convection expected
            FinalFlowRegime = InConvFlowRegime_A3;
        }
    }

    // now finish out specific model eq for this surface
    // Surface(SurfNum)%IntConvClassification = 0 !init/check
    {
        auto const SELECT_CASE_var(FinalFlowRegime);

        if (SELECT_CASE_var == InConvFlowRegime_A1) {
            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_VertWalls;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableTilted;
                    }
                } else if (Surface(SurfNum).Tilt <= 85.0) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfNum)) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_ChilledCeil;
                } else if (Surface(SurfNum).Tilt < 5.0) {
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt >= 5.0) && ((Surface(SurfNum).Tilt < 95.0))) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableTilted;
                    }
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_VertWalls;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfNum)) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_HeatedFloor;
                } else if (Surface(SurfNum).Tilt > 175.0) { // floor
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt <= 175.0) && (Surface(SurfNum).Tilt >= 95.0)) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableTilted;
                    }
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                // assume horizontal upwards.
                if (DeltaTemp > 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_UnstableHoriz;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A1_StableHoriz;
                }
            }

            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state,
                                "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A1 surface named" + Surface(SurfNum).Name);
            }

        } else if (SELECT_CASE_var == InConvFlowRegime_A2) {
            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                if (state.dataSurface->SurfIntConvSurfHasActiveInIt(SurfNum)) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_HeatedVerticalWall;
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_VertWallsNonHeated;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableTilted;
                    }
                } else if (Surface(SurfNum).Tilt <= 85.0) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (Surface(SurfNum).Tilt < 5.0) {
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt >= 5.0) && ((Surface(SurfNum).Tilt < 95.0))) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableTilted;
                    }
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_VertWallsNonHeated;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (Surface(SurfNum).Tilt > 175.0) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt <= 175.0) && (Surface(SurfNum).Tilt >= 95.0)) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableTilted;
                    }
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                // assume horizontal upwards.
                if (DeltaTemp > 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_UnstableHoriz;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A2_StableHoriz;
                }
            }

            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state,
                                "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A2 surface named" + Surface(SurfNum).Name);
            }
        } else if (SELECT_CASE_var == InConvFlowRegime_A3) {
            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_VertWalls;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableTilted;
                    }
                } else if (Surface(SurfNum).Tilt <= 85.0) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (Surface(SurfNum).Tilt < 5.0) {
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt > 5.0) && ((Surface(SurfNum).Tilt < 85.0))) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableTilted;
                    }
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_VertWalls;
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (Surface(SurfNum).Tilt > 175.0) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt <= 175.0) && (Surface(SurfNum).Tilt >= 95.0)) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableTilted;
                    }
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                // assume horizontal upwards.
                if (DeltaTemp >= 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_UnstableHoriz;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_A3_StableHoriz;
                }
            }

            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state,
                                "DynamicIntConvSurfaceClassification: failed to resolve Hc model for A3 surface named" + Surface(SurfNum).Name);
            }
        } else if (SELECT_CASE_var == InConvFlowRegime_B) {
            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    if (state.dataSurface->SurfIntConvSurfGetsRadiantHeat(SurfNum)) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_VertWallsNearHeat;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_VertWalls;
                    }

                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableTilted;
                    }
                } else if (Surface(SurfNum).Tilt <= 85.0) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (Surface(SurfNum).Tilt < 5.0) {
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt >= 5.0) && ((Surface(SurfNum).Tilt < 85.0))) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableTilted;
                    }
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall
                    if (state.dataSurface->SurfIntConvSurfGetsRadiantHeat(SurfNum)) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_VertWallsNearHeat;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_VertWalls;
                    }
                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (Surface(SurfNum).Tilt > 175.0) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt <= 175.0) && (Surface(SurfNum).Tilt >= 95.0)) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableTilted;
                    }
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                // assume horizontal upwards.
                if (DeltaTemp > 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_UnstableHoriz;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_B_StableHoriz;
                }
            }

            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state, "DynamicIntConvSurfaceClassification: failed to resolve Hc model for B surface named" + Surface(SurfNum).Name);
            }
        } else if (SELECT_CASE_var == InConvFlowRegime_C) {
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_C_Walls;
            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_C_Ceiling;
            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_C_Floor;
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_C_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_C_Floor;
            }
            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state, "DynamicIntConvSurfaceClassification: failed to resolve Hc model for C surface named" + Surface(SurfNum).Name);
            }

        } else if (SELECT_CASE_var == InConvFlowRegime_D) {

            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall

                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_Walls;

                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableTilted;
                    }
                } else if (Surface(SurfNum).Tilt <= 85.0) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (Surface(SurfNum).Tilt < 5.0) {
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt >= 5.0) && ((Surface(SurfNum).Tilt <= 85.0))) { // tilted downwards
                    if (DeltaTemp < 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableTilted;
                    }
                } else if ((Surface(SurfNum).Tilt > 85.0) && (Surface(SurfNum).Tilt < 95.0)) { // vertical wall

                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_Walls;

                } else if (Surface(SurfNum).Tilt >= 95.0) { // tilted upwards
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableTilted;
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (Surface(SurfNum).Tilt > 175.0) { // floor
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableHoriz;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableHoriz;
                    }
                } else if ((Surface(SurfNum).Tilt <= 175.0) && (Surface(SurfNum).Tilt >= 95.0)) {
                    if (DeltaTemp > 0.0) {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableTilted;
                    } else {
                        state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableTilted;
                    }
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                // assume horizontal upwards.
                if (DeltaTemp > 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_UnstableHoriz;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_D_StableHoriz;
                }
            }

            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state, "DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named" + Surface(SurfNum).Name);
            }

        } else if (SELECT_CASE_var == InConvFlowRegime_E) {

            DeltaTemp = TH(2, 1, SurfNum) - state.dataHeatBalFanSys->MAT(ZoneNum);
            if (Surface(SurfNum).Class == SurfaceClass::Wall || Surface(SurfNum).Class == SurfaceClass::Door) {

                // mixed regime, but need to know what regime it was before it was mixed
                {
                    auto const SELECT_CASE_var1(FlowRegimeStack(PriorityEquipOn));

                    if (SELECT_CASE_var1 == InConvFlowRegime_C) {
                        // assume forced flow is down along wall (ceiling diffuser)
                        if (DeltaTemp > 0.0) { // surface is hotter so plume upwards and forces oppose
                            state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_OpposFlowWalls;
                        } else { // surface is cooler so plume down and forces assist
                            state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_AssistFlowWalls;
                        }
                    } else if (SELECT_CASE_var1 == InConvFlowRegime_D) {
                        // assume forced flow is upward along wall (perimeter zone HVAC with fan)
                        if (DeltaTemp > 0.0) { // surface is hotter so plume up and forces assist
                            state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_AssistFlowWalls;
                        } else { // surface is cooler so plume downward and forces oppose
                            state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_OpposFlowWalls;
                        }
                    }
                }

            } else if (Surface(SurfNum).Class == SurfaceClass::Roof) {
                if (DeltaTemp > 0.0) { // surface is hotter so stable
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_StableCeiling;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_UnstableCieling;
                }
            } else if (Surface(SurfNum).Class == SurfaceClass::Floor) {
                if (DeltaTemp > 0.0) { // surface is hotter so unstable
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_UnstableFloor;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_StableFloor;
                }
            } else if ((Surface(SurfNum).Class == SurfaceClass::Window) || (Surface(SurfNum).Class == SurfaceClass::GlassDoor) ||
                       (Surface(SurfNum).Class == SurfaceClass::TDD_Diffuser)) {
                state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_Windows;
            } else if (Surface(SurfNum).Class == SurfaceClass::IntMass) {
                if (DeltaTemp > 0.0) {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_UnstableFloor;
                } else {
                    state.dataSurface->SurfIntConvClassification(SurfNum) = InConvClass_E_StableFloor;
                }
            }
            if (state.dataSurface->SurfIntConvClassification(SurfNum) == 0) {
                ShowSevereError(state,
                                "DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named " + Surface(SurfNum).Name);
            }
        } else {
            ShowSevereError(state,
                            "DynamicIntConvSurfaceClassification: failed to deterime zone flow regime for surface named " + Surface(SurfNum).Name);
        }
    }
}

void MapIntConvClassificationToHcModels(EnergyPlusData &state, int const SurfNum) // surface pointer index
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Map Hc model equation data from central structure to surface structure

    // METHODOLOGY EMPLOYED:
    // Long case statement depends on surface classification determined in DynamicIntConvSurfaceClassification
    // then simply map data stored in InsideFaceAdaptiveConvectionAlgo into the surface's structure
    // if model type is user-defined, also store the index to the user curve to be used.
    {
        auto const SELECT_CASE_var(state.dataSurface->SurfIntConvClassification(SurfNum));

        if (SELECT_CASE_var == InConvClass_A1_VertWalls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_StableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_UnstableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_HeatedFloor) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolHeatedFloorUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_ChilledCeil) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolChilledCeilingUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_StableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolStableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_UnstableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolUnstableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A1_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.FloorHeatCeilingCoolWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_VertWallsNonHeated) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_HeatedVerticalWall) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatHeatedWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_StableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_UnstableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_StableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatStableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_UnstableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatUnstableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A2_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.WallPanelHeatWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_VertWalls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_StableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_UnstableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_StableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyStableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_UnstableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyUnstableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_A3_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.SimpleBouyWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_VertWalls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_VertWallsNearHeat) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatVertWallNearHeaterUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_StableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_UnstableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_StableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatStableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_UnstableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatUnstableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_B_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ConvectiveHeatWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_C_Walls) {
            if ((state.dataSurface->SurfIntConvZonePerimLength(SurfNum) == 0.0) &&
                (state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum ==
                 HcInt_GoldsteinNovoselacCeilingDiffuserWalls)) {
                // no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_FisherPedersenCeilDiffuserWalls;
            } else {
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallEqNum;
            }
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_C_Ceiling) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirCeilingUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_C_Floor) {
            if ((state.dataSurface->SurfIntConvZonePerimLength(SurfNum) == 0.0) &&
                (state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum ==
                 HcInt_GoldsteinNovoselacCeilingDiffuserFloor)) {
                // no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_FisherPedersenCeilDiffuserFloor;
            } else {
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorEqNum;
            }
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirFloorUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_C_Windows) {
            if ((state.dataSurface->SurfIntConvZonePerimLength(SurfNum) == 0.0) &&
                (state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum ==
                 HcInt_GoldsteinNovoselacCeilingDiffuserWindow)) {
                // no perimeter, Goldstein Novolselac model not good so revert to ISO15099
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) = HcInt_ISO15099Windows;
            } else {
                state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsEqNum;
            }
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.CentralAirWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_Walls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircVertWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_StableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_UnstableHoriz) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableHorizUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_StableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircStableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_UnstableTilted) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircUnstableTiltedUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_D_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.ZoneFanCircWindowsUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_AssistFlowWalls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyAssistingFlowWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_OpposFlowWalls) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedBouyOppossingFlowWallUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_StableFloor) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableFloorUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_UnstableFloor) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableFloorUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_StableCeiling) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedStableCeilingUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_UnstableCieling) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) =
                state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedUnstableCeilingUserCurveNum;
            }
        } else if (SELECT_CASE_var == InConvClass_E_Windows) {
            state.dataSurface->SurfIntConvHcModelEq(SurfNum) = state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsEqNum;
            if (state.dataSurface->SurfIntConvHcModelEq(SurfNum) == HcInt_UserCurve) {
                state.dataSurface->SurfIntConvHcUserCurveIndex(SurfNum) =
                    state.dataConvectionCoefficient->InsideFaceAdaptiveConvectionAlgo.MixedWindowsUserCurveNum;
            }
        }
    }
}

void CalcUserDefinedInsideHcModel(EnergyPlusData &state, int const SurfNum, int const UserCurveNum, Real64 &Hc)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate user-defined convection correlations for inside face

    // METHODOLOGY EMPLOYED:
    // call curve objects to evaluate user's model equation
    // prepare independent parameters for x values

    // Using/Aliasing
    using namespace DataZoneEquipment;
    using CurveManager::CurveValue;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyWFnTdpPb;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 tmpAirTemp;
    Real64 SupplyAirTemp;
    Real64 AirChangeRate;
    int ZoneNum;
    int ZoneNode;
    int EquipNum;
    Real64 SumMdotTemp;
    Real64 SumMdot;
    Real64 AirDensity;
    int thisZoneInletNode;

    auto &Zone(state.dataHeatBal->Zone);
    auto &Surface(state.dataSurface->Surface);

    ZoneNum = Surface(SurfNum).Zone;
    SumMdotTemp = 0.0;
    SumMdot = 0.0;
    SupplyAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
    if (Zone(ZoneNum).IsControlled) {
        ZoneNode = Zone(ZoneNum).SystemZoneNodeNumber;
        AirDensity = PsyRhoAirFnPbTdbW(state,
                                       state.dataEnvrn->OutBaroPress,
                                       state.dataLoopNodes->Node(ZoneNode).Temp,
                                       PsyWFnTdpPb(state, state.dataLoopNodes->Node(ZoneNode).Temp, state.dataEnvrn->OutBaroPress));
        AirChangeRate = (state.dataLoopNodes->Node(ZoneNode).MassFlowRate * DataGlobalConstants::SecInHour) / (AirDensity * Zone(ZoneNum).Volume);
        if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex > 0) {
            for (EquipNum = 1;
                 EquipNum <= state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex).NumOfEquipTypes;
                 ++EquipNum) {
                if (allocated(state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                  .EquipData(EquipNum)
                                  .OutletNodeNums)) {
                    thisZoneInletNode = state.dataZoneEquip->ZoneEquipList(state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex)
                                            .EquipData(EquipNum)
                                            .OutletNodeNums(1);
                    if ((thisZoneInletNode > 0) && (state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate > 0.0)) {
                        SumMdotTemp += state.dataLoopNodes->Node(thisZoneInletNode).MassFlowRate * state.dataLoopNodes->Node(thisZoneInletNode).Temp;
                    }
                }
            }
        }
        if (SumMdot > 0.0) {
            SupplyAirTemp = SumMdotTemp / SumMdot; // mass flow weighted inlet temperature
        }
    }

    auto &UserCurve = state.dataConvectionCoefficient->HcInsideUserCurve(UserCurveNum);

    {
        auto const SELECT_CASE_var(UserCurve.ReferenceTempType);

        if (SELECT_CASE_var == RefTempMeanAirTemp) {
            tmpAirTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneMeanAirTemp;
        } else if (SELECT_CASE_var == RefTempAdjacentAirTemp) {
            tmpAirTemp = state.dataHeatBal->SurfTempEffBulkAir(SurfNum);
            state.dataSurface->SurfTAirRef(SurfNum) = AdjacentAirTemp;
        } else if (SELECT_CASE_var == RefTempSupplyAirTemp) {
            tmpAirTemp = SupplyAirTemp;
            state.dataSurface->SurfTAirRef(SurfNum) = ZoneSupplyAirTemp;
        }
    }

    Real64 HcFnTempDiff(0.0), HcFnTempDiffDivHeight(0.0), HcFnACH(0.0), HcFnACHDivPerimLength(0.0);
    Kiva::ConvectionAlgorithm HcFnTempDiffFn(KIVA_CONST_CONV(0.0)), HcFnTempDiffDivHeightFn(KIVA_CONST_CONV(0.0));
    if (UserCurve.HcFnTempDiffCurveNum > 0) {
        HcFnTempDiff = CurveValue(state, UserCurve.HcFnTempDiffCurveNum, std::abs(state.dataHeatBalSurf->TH(2, 1, SurfNum) - tmpAirTemp));
        HcFnTempDiffFn = [&](double Tsurf, double Tamb, double, double, double) -> double {
            return CurveValue(state, UserCurve.HcFnTempDiffCurveNum, std::abs(Tsurf - Tamb));
        };
    }

    if (UserCurve.HcFnTempDiffDivHeightCurveNum > 0) {
        HcFnTempDiffDivHeight =
            CurveValue(state,
                       UserCurve.HcFnTempDiffDivHeightCurveNum,
                       (std::abs(state.dataHeatBalSurf->TH(2, 1, SurfNum) - tmpAirTemp) / state.dataSurface->SurfIntConvZoneWallHeight(SurfNum)));
        HcFnTempDiffDivHeightFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
            return CurveValue(
                state, UserCurve.HcFnTempDiffDivHeightCurveNum, std::abs(Tsurf - Tamb) / state.dataSurface->SurfIntConvZoneWallHeight(SurfNum));
        };
    }

    if (UserCurve.HcFnACHCurveNum > 0) {
        HcFnACH = CurveValue(state, UserCurve.HcFnACHCurveNum, AirChangeRate);
    }

    if (UserCurve.HcFnACHDivPerimLengthCurveNum > 0) {
        HcFnACHDivPerimLength =
            CurveValue(state, UserCurve.HcFnACHDivPerimLengthCurveNum, (AirChangeRate / state.dataSurface->SurfIntConvZonePerimLength(SurfNum)));
    }

    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].in =
            [=](double Tsurf, double Tamb, double HfTerm, double Roughness, double CosTilt) -> double {
            return HcFnTempDiffFn(Tsurf, Tamb, HfTerm, Roughness, CosTilt) + HcFnTempDiffDivHeightFn(Tsurf, Tamb, HfTerm, Roughness, CosTilt) +
                   HcFnACH + HcFnACHDivPerimLength;
        };
        Hc = 0.0;
    } else {
        Hc = HcFnTempDiff + HcFnTempDiffDivHeight + HcFnACH + HcFnACHDivPerimLength;
    }
}

void CalcUserDefinedOutsideHcModel(EnergyPlusData &state, int const SurfNum, int const UserCurveNum, Real64 &H)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate user-defined convection correlations for outside face

    // METHODOLOGY EMPLOYED:
    // call curve objects to evaluate user's model equation
    // prepare independent parameters for x values

    // Using/Aliasing
    using CurveManager::CurveValue;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 windVel;
    Real64 Theta;
    Real64 ThetaRad;

    auto &UserCurve = state.dataConvectionCoefficient->HcOutsideUserCurve(UserCurveNum);
    auto &Surface(state.dataSurface->Surface);

    {
        auto const SELECT_CASE_var(UserCurve.WindSpeedType);

        if (SELECT_CASE_var == RefWindWeatherFile) {
            windVel = state.dataEnvrn->WindSpeed;
        } else if (SELECT_CASE_var == RefWindAtZ) {
            windVel = state.dataSurface->SurfOutWindSpeed(SurfNum);
        } else if (SELECT_CASE_var == RefWindParallComp) {
            // WindSpeed , WindDir, surface Azimuth
            Theta = state.dataEnvrn->WindDir - Surface(SurfNum).Azimuth - 90.0; // TODO double check theta
            ThetaRad = Theta * DataGlobalConstants::DegToRadians;
            windVel = std::cos(ThetaRad) * state.dataEnvrn->WindSpeed;
        } else if (SELECT_CASE_var == RefWindParallCompAtZ) {
            // Surface WindSpeed , Surface WindDir, surface Azimuth
            Theta = state.dataSurface->SurfOutWindDir(SurfNum) - Surface(SurfNum).Azimuth - 90.0; // TODO double check theta
            ThetaRad = Theta * DataGlobalConstants::DegToRadians;
            windVel = std::cos(ThetaRad) * state.dataSurface->SurfOutWindSpeed(SurfNum);
        }
    }

    Kiva::ForcedConvectionTerm HfFnWindSpeedFn(KIVA_HF_DEF);
    Kiva::ConvectionAlgorithm HnFnTempDiffFn(KIVA_CONST_CONV(0.0)), HnFnTempDiffDivHeightFn(KIVA_CONST_CONV(0.0));

    Real64 HfFnWindSpeed(0.0), HnFnTempDiff(0.0), HnFnTempDiffDivHeight(0.0);
    if (UserCurve.HfFnWindSpeedCurveNum > 0) {
        HfFnWindSpeed = CurveValue(state, UserCurve.HfFnWindSpeedCurveNum, windVel);
        HfFnWindSpeedFn = [&](double, double, double, double windSpeed) -> double {
            return CurveValue(state, UserCurve.HfFnWindSpeedCurveNum, windSpeed);
        };
    }

    if (UserCurve.HnFnTempDiffCurveNum > 0) {
        HnFnTempDiff = CurveValue(state,
                                  UserCurve.HnFnTempDiffCurveNum,
                                  std::abs(state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum)));
        HnFnTempDiffFn = [&](double Tsurf, double Tamb, double, double, double) -> double {
            return CurveValue(state, UserCurve.HnFnTempDiffCurveNum, std::abs(Tsurf - Tamb));
        };
    }

    if (UserCurve.HnFnTempDiffDivHeightCurveNum > 0) {
        if (state.dataSurface->SurfOutConvFaceHeight(SurfNum) > 0.0) {
            HnFnTempDiffDivHeight =
                CurveValue(state,
                           UserCurve.HnFnTempDiffDivHeightCurveNum,
                           ((std::abs(state.dataHeatBalSurf->TH(1, 1, SurfNum) - state.dataSurface->SurfOutDryBulbTemp(SurfNum))) /
                            state.dataSurface->SurfOutConvFaceHeight(SurfNum)));
            HnFnTempDiffDivHeightFn = [=, &state](double Tsurf, double Tamb, double, double, double) -> double {
                return CurveValue(
                    state, UserCurve.HnFnTempDiffDivHeightCurveNum, ((std::abs(Tsurf - Tamb)) / state.dataSurface->SurfOutConvFaceHeight(SurfNum)));
            };
        }
    }

    if (Surface(SurfNum).ExtBoundCond == DataSurfaces::KivaFoundation) {
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].f = HfFnWindSpeedFn;
        state.dataSurfaceGeometry->kivaManager.surfaceConvMap[SurfNum].out =
            [=](double Tsurf, double Tamb, double HfTerm, double Roughness, double CosTilt) -> double {
            return HnFnTempDiffFn(Tsurf, Tamb, HfTerm, Roughness, CosTilt) + HnFnTempDiffDivHeightFn(Tsurf, Tamb, HfTerm, Roughness, CosTilt) +
                   HfTerm;
        };
    }
    H = HfFnWindSpeed + HnFnTempDiff + HnFnTempDiffDivHeight;
}

//** Begin catalog of Hc equation functions. **** !*************************************************

Real64 CalcASHRAEVerticalWall(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the model equation attributed to ASHRAE for vertical walls for natural convection

    // REFERENCES:
    // 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

    // Return value
    Real64 Hn; // function result

    Hn = 1.31 * std::pow(std::abs(DeltaTemp), OneThird);

    return Hn;
}

Real64 CalcWaltonUnstableHorizontalOrTilt(Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                          Real64 const CosineTilt // Cosine of tilt angle
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the model equation attributed to Walton's TARP program for horizontal
    // and tilted surfaces with enhanced, thermally unstable natural convection

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
    //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.

    // Return value
    Real64 Hn; // function result

    Hn = 9.482 * std::pow(std::abs(DeltaTemp), OneThird) / (7.238 - std::abs(CosineTilt));

    return Hn;
}

Real64 CalcWaltonStableHorizontalOrTilt(Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                        Real64 const CosineTilt // Cosine of tilt angle
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the model equation attributed to Walton's TARP program for horizontal
    // and tilted surfaces with reduced, thermally stable natural convection

    // REFERENCES:
    // 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
    //     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.

    // Return value
    Real64 Hn; // function result

    Hn = 1.810 * std::pow(std::abs(DeltaTemp), OneThird) / (1.382 + std::abs(CosineTilt));

    return Hn;
}

Real64 CalcFisherPedersenCeilDiffuserFloor(EnergyPlusData &state,
                                           Real64 const ACH, // [1/hr] air system air change rate
                                           Real64 const Tsurf,
                                           Real64 const Tair,
                                           Real64 const cosTilt,
                                           Real64 const humRat,
                                           Real64 const height,
                                           bool const isWindow)
{

    // AUTHOR: Brent Griffith (Aug 2010)
    // PURPOSE OF THIS FUNCTION: Calculate the model equation by Fisher and Pedersen for floors with ceiling diffusers
    // REFERENCE: Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and Thermal Load Calculations,
    //            ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

    Real64 Hforced;

    if (ACH >= 3.0) {
        Hforced = 3.873 + 0.082 * std::pow(ACH, 0.98);
        return Hforced;
    } else {                        // Revert to purely natural convection
        Hforced = 4.11365377688938; // Value of Hforced when ACH=3
        return CalcFisherPedersenCeilDiffuserNatConv(state, Hforced, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow);
    }
}

Real64 CalcFisherPedersenCeilDiffuserCeiling(EnergyPlusData &state,
                                             Real64 const ACH, // [1/hr] air system air change rate
                                             Real64 const Tsurf,
                                             Real64 const Tair,
                                             Real64 const cosTilt,
                                             Real64 const humRat,
                                             Real64 const height,
                                             bool const isWindow)
{

    // AUTHOR: Brent Griffith (Aug 2010)
    // PURPOSE OF THIS FUNCTION: Calculate the model equation by Fisher and Pedersen for floors with ceiling diffusers
    // REFERENCE: Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and Thermal Load Calculations,
    //            ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

    Real64 Hforced;

    if (ACH >= 3.0) {
        Hforced = 2.234 + 4.099 * std::pow(ACH, 0.503);
        return Hforced;
    } else {                        // Revert to purely natural convection
        Hforced = 9.35711423763866; // Value of Hforced when ACH=3
        return CalcFisherPedersenCeilDiffuserNatConv(state, Hforced, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow);
    }
}

Real64 CalcFisherPedersenCeilDiffuserWalls(EnergyPlusData &state,
                                           Real64 const ACH, // [1/hr] air system air change rate
                                           Real64 const Tsurf,
                                           Real64 const Tair,
                                           Real64 const cosTilt,
                                           Real64 const humRat,
                                           Real64 const height,
                                           bool const isWindow)
{

    // AUTHOR: Brent Griffith (Aug 2010)
    // PURPOSE OF THIS FUNCTION: Calculate the model equation by Fisher and Pedersen for floors with ceiling diffusers
    // REFERENCE: Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and Thermal Load Calculations,
    //            ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13

    Real64 Hforced;

    if (ACH >= 3.0) {
        Hforced = 1.208 + 1.012 * std::pow(ACH, 0.604);
        return Hforced;
    } else {                        // Revert to purely natural convection
        Hforced = 3.17299636062606; // Value of Hforced when ACH=3
        return CalcFisherPedersenCeilDiffuserNatConv(state, Hforced, ACH, Tsurf, Tair, cosTilt, humRat, height, isWindow);
    }
}

Real64 CalcFisherPedersenCeilDiffuserNatConv(EnergyPlusData &state,
                                             Real64 const Hforced,
                                             Real64 const ACH,
                                             Real64 const Tsurf,
                                             Real64 const Tair,
                                             Real64 const cosTilt,
                                             Real64 const humRat,
                                             Real64 const height,
                                             bool const isWindow)
{

    Real64 Hnatural;

    if (isWindow) {                        // Unlikely for a floor, but okay...
        Real64 const tilt = acos(cosTilt); // outward facing tilt
        Real64 const sinTilt = sin(tilt);
        Hnatural = CalcISO15099WindowIntConvCoeff(state, Tsurf, Tair, humRat, height, tilt, sinTilt);
    } else {
        Hnatural = CalcASHRAETARPNatural(Tsurf, Tair, -cosTilt); // negative cosTilt because interior of surface
    }
    if (ACH <= 0.5) {
        return Hnatural;
    } else {
        return Hnatural + ((Hforced - Hnatural) * ((ACH - 0.5) / 2.5)); // range for interpolation goes from ACH=0.5 to ACH=3.0 or a range of 2.5
    }
}

Real64 CalcAlamdariHammondUnstableHorizontal(Real64 const DeltaTemp,        // [C] temperature difference between surface and air
                                             Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Alamdari and Hammond
    // This function only for the Unstable heat flow direction for horizontal surfaces

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
    // for buoyancy-driven convection in rooms.  Building Services Engineering
    // Research & Technology. Vol. 4, No. 3.

    return std::pow(pow_6(1.4 * std::pow(std::abs(DeltaTemp) / HydraulicDiameter, OneFourth)) + (1.63 * pow_2(DeltaTemp)),
                    OneSixth); // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
}

Real64 CalcAlamdariHammondUnstableHorizontal(EnergyPlusData &state,
                                             Real64 const DeltaTemp,         // [C] temperature difference between surface and air
                                             Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                             int const SurfNum               // for messages
)
{
    Real64 Hn; // function result

    if (HydraulicDiameter > 0.0) {
        Hn = CalcAlamdariHammondUnstableHorizontal(DeltaTemp, HydraulicDiameter);
    } else {
        Hn = 9.999;
        if (state.dataConvectionCoefficient->AHUnstableHorizontalErrorIDX == 0) {
            ShowSevereMessage(state, "CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated (would divide by zero)");
            ShowContinueError(state,
                              "Effective hydraulic diameter is zero, convection model not applicable for surface =" +
                                  state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->AHUnstableHorizontalErrorIDX);
    }

    return Hn;
}

Real64 CalcAlamdariHammondStableHorizontal(Real64 const DeltaTemp,        // [C] temperature difference between surface and air
                                           Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Alamdari and Hammond
    // This function only for the Stable heat flow direction for horizontal surfaces

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
    // for buoyancy-driven convection in rooms.  Building Services Engineering
    // Research & Technology. Vol. 4, No. 3.

    return 0.6 * std::pow(std::abs(DeltaTemp) / pow_2(HydraulicDiameter), OneFifth);
}

Real64 CalcAlamdariHammondStableHorizontal(EnergyPlusData &state,
                                           Real64 const DeltaTemp,         // [C] temperature difference between surface and air
                                           Real64 const HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                           int const SurfNum               // for messages
)
{

    Real64 Hn; // function result, natural convection Hc value

    if (HydraulicDiameter > 0.0) {
        Hn = CalcAlamdariHammondStableHorizontal(DeltaTemp, HydraulicDiameter);
    } else {
        Hn = 9.999;
        if (state.dataConvectionCoefficient->AHStableHorizontalErrorIDX == 0) {
            ShowSevereMessage(state, "CalcAlamdariHammondStableHorizontal: Convection model not evaluated (would divide by zero)");
            ShowContinueError(state,
                              "Effective hydraulic diameter is zero, convection model not applicable for surface =" +
                                  state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcAlamdariHammondStableHorizontal: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->AHStableHorizontalErrorIDX);
    }

    return Hn;
}

Real64 CalcAlamdariHammondVerticalWall(Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                       Real64 const Height)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Alamdari and Hammond
    // This function only for the vertical wall surfaces

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
    // for buoyancy-driven convection in rooms.  Building Services Engineering
    // Research & Technology. Vol. 4, No. 3.

    return std::pow(pow_6(1.5 * std::pow(std::abs(DeltaTemp) / Height, OneFourth)) + (1.23 * pow_2(DeltaTemp)),
                    OneSixth); // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
}

Real64 CalcAlamdariHammondVerticalWall(EnergyPlusData &state,
                                       Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                       Real64 const Height,    // [m] characteristic size, = zone height
                                       int const SurfNum       // for messages
)
{
    // Return value
    Real64 Hn; // function result, natural convection Hc value

    if (Height > 0.0) {
        Hn = CalcAlamdariHammondVerticalWall(DeltaTemp, Height);
    } else {
        Hn = 9.999;
        if (state.dataConvectionCoefficient->AHVerticalWallErrorIDX == 0) {
            ShowSevereMessage(state, "CalcAlamdariHammondVerticalWall: Convection model not evaluated (would divide by zero)");
            ShowContinueError(state,
                              "Effective hydraulic diameter is zero, convection model not applicable for surface =" +
                                  state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcAlamdariHammondVerticalWall: Convection model not evaluated because zero hydraulic diameter and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->AHVerticalWallErrorIDX);
    }

    return Hn;
}

Real64 CalcKhalifaEq3WallAwayFromHeat(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Khalifa's Eq 3 for Walls Away From Heat

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
    //   University of Wales College of Cardiff, Cardiff, UK.
    // Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    // Return value
    Real64 Hc; // function result

    Hc = 2.07 * std::pow(std::abs(DeltaTemp), 0.23);

    return Hc;
}

Real64 CalcKhalifaEq4CeilingAwayFromHeat(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Khalifa's Eq 4 for Ceilings Away From Heat

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
    //   University of Wales College of Cardiff, Cardiff, UK.
    // Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    // Return value
    Real64 Hc; // function result

    Hc = 2.72 * std::pow(std::abs(DeltaTemp), 0.13);

    return Hc;
}

Real64 CalcKhalifaEq5WallsNearHeat(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Khalifa's Eq 5 for Walls near the heater

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
    //   University of Wales College of Cardiff, Cardiff, UK.
    // Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    // Return value
    Real64 Hc; // function result

    Hc = 1.98 * std::pow(std::abs(DeltaTemp), 0.32);

    return Hc;
}

Real64 CalcKhalifaEq6NonHeatedWalls(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Khalifa's Eq 6 for non-heated walls

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
    //   University of Wales College of Cardiff, Cardiff, UK.
    // Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    // Return value
    Real64 Hc; // function result

    Hc = 2.30 * std::pow(std::abs(DeltaTemp), 0.24);

    return Hc;
}

Real64 CalcKhalifaEq7Ceiling(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Khalifa's Eq 7 for ceilings

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
    //   University of Wales College of Cardiff, Cardiff, UK.
    // Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    // Return value
    Real64 Hc; // function result

    Hc = 3.10 * std::pow(std::abs(DeltaTemp), 0.17);

    return Hc;
}

Real64 CalcAwbiHattonHeatedFloor(Real64 const DeltaTemp,        // [C] temperature difference between surface and air
                                 Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Awbi and Hatton for heated floors

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.
    // apply numerical protection for low values of hydraulic diameter

    // REFERENCES:
    // Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
    //   Energy and Buildings 30 (1999) 233-244.
    //   This function is for equation 15 in the reference

    // Return value
    Real64 Hc; // function result

    Real64 const pow_fac(2.175 / std::pow(1.0, 0.076));

    if (HydraulicDiameter > 1.0) {
        Hc = 2.175 * std::pow(std::abs(DeltaTemp), 0.308) / std::pow(HydraulicDiameter, 0.076);
    } else {
        Hc = pow_fac * std::pow(std::abs(DeltaTemp), 0.308);
    }

    return Hc;
}

Real64 CalcAwbiHattonHeatedWall(Real64 const DeltaTemp,        // [C] temperature difference between surface and air
                                Real64 const HydraulicDiameter // [m] characteristic size, = (4 * area) / perimeter
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for Awbi and Hatton for heated walls

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
    //   Energy and Buildings 30 (1999) 233-244.
    //   This function is for equation 12 in the reference

    // Return value
    Real64 Hc; // function result

    if (HydraulicDiameter > 1.0) {
        Hc = 1.823 * std::pow(std::abs(DeltaTemp), 0.293) / std::pow(HydraulicDiameter, 0.121);
    } else {
        Hc = 1.823 * std::pow(std::abs(DeltaTemp), 0.293) / std::pow(1.0, 0.121);
    }

    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedAssistedWall(Real64 const &DeltaTemp,     // [C] temperature difference between surface and air
                                               Real64 const &Height,        // [m] characteristic size
                                               Real64 const &SurfTemp,      // [C] surface temperature
                                               Real64 const &SupplyAirTemp, // [C] temperature of supply air into zone
                                               Real64 const &AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces assisting each other along a Wall

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 cofpow =
        std::sqrt(pow_6(1.5 * std::pow(std::abs(DeltaTemp) / Height, OneFourth)) + std::pow(1.23 * pow_2(DeltaTemp), OneSixth)) +
        pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) *
              (-0.199 + 0.190 * std::pow(AirChangeRate,
                                         0.8)));      // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
    Real64 Hc = std::pow(std::abs(cofpow), OneThird); // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
    if (cofpow < 0.0) {
        Hc = -Hc;
    }
    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedAssistedWall(EnergyPlusData &state,
                                               Real64 const &DeltaTemp, // [C] temperature difference between surface and air
                                               Real64 const &Height,    // [m] characteristic size
                                               Real64 const &SurfTemp,  // [C] surface temperature
                                               int const ZoneNum        // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if ((std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) && (Height != 0.0)) {
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedAssistedWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);
    } else {
        if (Height == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedAssistedWallErrorIDX2 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "Effective height is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated because of zero height "
                                           "and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedAssistedWallErrorIDX2);
        }
        if (DeltaTemp == 0.0 && !state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedAssistedWallErrorIDX1 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated because of zero temperature "
                                           "difference and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedAssistedWallErrorIDX1);
        }
        return 9.999;
    }
}

Real64 CalcBeausoleilMorrisonMixedOpposingWall(Real64 const &DeltaTemp,     // [C] temperature difference between surface and air
                                               Real64 const &Height,        // [m] characteristic size
                                               Real64 const &SurfTemp,      // [C] surface temperature
                                               Real64 const &SupplyAirTemp, // [C] temperature of supply air into zone
                                               Real64 const &AirChangeRate  // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces opposing each other along a Wall

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 HcTmp1;
    Real64 HcTmp2;
    Real64 HcTmp3;
    Real64 cofpow;

    if (Height != 0.0) {
        cofpow = std::sqrt(pow_6(1.5 * std::pow(std::abs(DeltaTemp) / Height, OneFourth)) + std::pow(1.23 * pow_2(DeltaTemp), OneSixth)) -
                 pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) *
                       (-0.199 + 0.190 * std::pow(AirChangeRate,
                                                  0.8))); // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
        HcTmp1 = std::pow(std::abs(cofpow), OneThird);    // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
        if (cofpow < 0.0) {
            HcTmp1 = -HcTmp1;
        }

        HcTmp2 = 0.8 * std::pow(pow_6(1.5 * std::pow(std::abs(DeltaTemp) / Height, OneFourth)) + (1.23 * pow_2(DeltaTemp)),
                                OneSixth); // Tuned pow_6( std::pow( std::abs( DeltaTemp ), OneThird ) ) changed to pow_2( DeltaTemp )
    } else {
        HcTmp1 = 9.999;
        HcTmp2 = 9.999;
    }
    HcTmp3 = 0.8 * ((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) * (-0.199 + 0.190 * std::pow(AirChangeRate, 0.8));

    return max(max(HcTmp1, HcTmp2), HcTmp3);
}

Real64 CalcBeausoleilMorrisonMixedOpposingWall(EnergyPlusData &state,
                                               Real64 const &DeltaTemp, // [C] temperature difference between surface and air
                                               Real64 const &Height,    // [m] characteristic size
                                               Real64 const &SurfTemp,  // [C] surface temperature
                                               int const ZoneNum        // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff) { // protect divide by zero

        if (Height == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedOpposingWallErrorIDX2 == 0) {
                ShowSevereMessage(state, "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "Effective height is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringSevereErrorAtEnd(
                state,
                "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because of zero height and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->BMMixedOpposingWallErrorIDX2);
        }
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedOpposingWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate);

    } else {
        if (!state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedOpposingWallErrorIDX1 == 0) {
                ShowSevereMessage(state, "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringSevereErrorAtEnd(state,
                                          "CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because of zero temperature "
                                          "difference and set to 9.999 [W/m2-K]",
                                          state.dataConvectionCoefficient->BMMixedOpposingWallErrorIDX1);
        }
        return 9.999;
    }
}

Real64 CalcBeausoleilMorrisonMixedStableFloor(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                              Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                              Real64 const &SurfTemp,          // [C] surface temperature
                                              Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                              Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces acting on an thermally stable floor

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 cofpow = pow_3(0.6 * std::pow(std::abs(DeltaTemp) / HydraulicDiameter, OneFifth)) +
                    pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) * (0.159 + 0.116 * std::pow(AirChangeRate, 0.8)));
    Real64 Hc = std::pow(std::abs(cofpow), OneThird);
    if (cofpow < 0.0) {
        Hc = -Hc;
    }
    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedStableFloor(EnergyPlusData &state,
                                              Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                              Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                              Real64 const &SurfTemp,          // [C] surface temperature
                                              int const ZoneNum                // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if ((HydraulicDiameter != 0.0) && (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff)) {
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedStableFloor(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate);
    } else {
        if (HydraulicDiameter == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedStableFloorErrorIDX1 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state,
                                  "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(
                state,
                "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated because effective hydraulic diameter is zero "
                "and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->BMMixedStableFloorErrorIDX1);
        }
        if (DeltaTemp == 0.0 && !state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedStableFloorErrorIDX2 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated because of zero temperature "
                                           "difference and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedStableFloorErrorIDX2);
        }
        return 9.999;
    }
}

Real64 CalcBeausoleilMorrisonMixedUnstableFloor(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                Real64 const &SurfTemp,          // [C] surface temperature
                                                Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces acting on an thermally unstable floor

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 cofpow =
        std::sqrt(pow_6(1.4 * std::pow(std::abs(DeltaTemp) / HydraulicDiameter, OneFourth)) + pow_6(1.63 * std::pow(std::abs(DeltaTemp), OneThird))) +
        pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) * (0.159 + 0.116 * std::pow(AirChangeRate, 0.8)));
    Real64 Hc = std::pow(std::abs(cofpow), OneThird);
    if (cofpow < 0.0) {
        Hc = -Hc;
    }

    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedUnstableFloor(EnergyPlusData &state,
                                                Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                Real64 const &SurfTemp,          // [C] surface temperature
                                                int const ZoneNum                // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if ((HydraulicDiameter != 0.0) && (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff)) {
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedUnstableFloor(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate);
    } else {
        if (HydraulicDiameter == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedUnstableFloorErrorIDX1 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state,
                                  "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(
                state,
                "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated because effective hydraulic diameter is zero "
                "and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->BMMixedUnstableFloorErrorIDX1);
        }

        if (DeltaTemp == 0.0 && !state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedUnstableFloorErrorIDX2 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated because of zero temperature "
                                           "difference and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedUnstableFloorErrorIDX2);
        }
        return 9.999;
    }
}

Real64 CalcBeausoleilMorrisonMixedStableCeiling(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                Real64 const &SurfTemp,          // [C] surface temperature
                                                Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces acting on a thermally stable ceiling

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 cofpow = pow_3(0.6 * std::pow(std::abs(DeltaTemp) / HydraulicDiameter, OneFifth)) +
                    pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) * (-0.166 + 0.484 * std::pow(AirChangeRate, 0.8)));
    Real64 Hc = std::pow(std::abs(cofpow), OneThird);
    if (cofpow < 0.0) {
        Hc = -Hc;
    }
    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedStableCeiling(EnergyPlusData &state,
                                                Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                Real64 const &SurfTemp,          // [C] surface temperature
                                                int const ZoneNum                // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if ((HydraulicDiameter != 0.0) && (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff)) {
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedStableCeiling(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate);
    } else {
        if (HydraulicDiameter == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedStableCeilingErrorIDX1 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state,
                                  "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(
                state,
                "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated because effective hydraulic diameter is zero "
                "and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->BMMixedStableCeilingErrorIDX1);
        }
        if (DeltaTemp == 0.0 && !state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedStableCeilingErrorIDX2 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated because of zero temperature "
                                           "difference and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedStableCeilingErrorIDX2);
        }
        return 9.999;
    }
}

Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 const &SurfTemp,          // [C] surface temperature
                                                  Real64 const &SupplyAirTemp,     // [C] temperature of supply air into zone
                                                  Real64 const &AirChangeRate      // [ACH] [1/hour] supply air ACH for zone
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation Beausoleil-Morrison's mixed flow regime
    // with mechanical and bouyancy forces acting on a thermally unstable ceiling

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
    //  air flow modeling within dynamic whole-building simulations.
    //  PhD. Thesis. University of Strathclyde, Glasgow, UK.

    Real64 cofpow =
        std::sqrt(pow_6(1.4 * std::pow(std::abs(DeltaTemp) / HydraulicDiameter, OneFourth)) + pow_6(1.63 * std::pow(std::abs(DeltaTemp), OneThird))) +
        pow_3(((SurfTemp - SupplyAirTemp) / std::abs(DeltaTemp)) * (-0.166 + 0.484 * std::pow(AirChangeRate, 0.8)));
    Real64 Hc = std::pow(std::abs(cofpow), OneThird);
    if (cofpow < 0.0) {
        Hc = -Hc;
    }
    return Hc;
}

Real64 CalcBeausoleilMorrisonMixedUnstableCeiling(EnergyPlusData &state,
                                                  Real64 const &DeltaTemp,         // [C] temperature difference between surface and air
                                                  Real64 const &HydraulicDiameter, // [m] characteristic size, = (4 * area) / perimeter
                                                  Real64 const &SurfTemp,          // [C] surface temperature
                                                  int const ZoneNum                // index of zone for messaging
)
{
    auto &Zone(state.dataHeatBal->Zone);

    if ((HydraulicDiameter != 0.0) && (std::abs(DeltaTemp) > DataHVACGlobals::SmallTempDiff)) {
        Real64 SupplyAirTemp = CalcZoneSupplyAirTemp(state, ZoneNum);
        Real64 AirChangeRate = CalcZoneSystemACH(state, ZoneNum);
        return CalcBeausoleilMorrisonMixedUnstableCeiling(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate);
    } else {
        if (HydraulicDiameter == 0.0) {
            if (state.dataConvectionCoefficient->BMMixedUnstableCeilingErrorIDX1 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state,
                                  "Effective hydraulic diameter is zero, convection model not applicable for zone named =" + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(
                state,
                "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated because effective hydraulic diameter is zero "
                "and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->BMMixedUnstableCeilingErrorIDX1);
        }
        if (DeltaTemp == 0.0 && !state.dataGlobal->WarmupFlag) {
            if (state.dataConvectionCoefficient->BMMixedUnstableCeilingErrorIDX2 == 0) {
                ShowWarningMessage(state, "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)");
                ShowContinueError(state, "The temperature difference between surface and air is zero");
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }

            ShowRecurringWarningErrorAtEnd(state,
                                           "CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated because of zero "
                                           "temperature difference and set to 9.999 [W/m2-K]",
                                           state.dataConvectionCoefficient->BMMixedUnstableCeilingErrorIDX2);
        }
        return 9.999;
    }
}

Real64 CalcFohannoPolidoriVerticalWall(Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                       Real64 const Height,    // [m] characteristic size, height of zone
                                       Real64 const SurfTemp,  // [C] surface temperature
                                       Real64 const QdotConv   // [W/m2] heat flux rate for rayleigh #
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for natural convection

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Fohanno, S., and G. Polidori. 2006. Modelling of natural convective heat transfer
    // at an internal surface. Energy and Buildings 38 (2006) 548 - 553

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const g(9.81);     // gravity constant (m/s**2)
    Real64 const v(15.89e-6); // kinematic viscosity (m**2/s) for air at 300 K
    Real64 const k(0.0263);   // thermal conductivity (W/m K) for air at 300 K
    Real64 const Pr(0.71);    // Prandtl number for air at ?

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 RaH(0.0);
    Real64 BetaFilm(0.0);

    BetaFilm = 1.0 / (DataGlobalConstants::KelvinConv + SurfTemp + 0.5 * DeltaTemp); // TODO check sign on DeltaTemp
    RaH = (g * BetaFilm * QdotConv * pow_4(Height) * Pr) / (k * pow_2(v));

    if (RaH <= 6.3e09) {
        return 1.332 * std::pow(std::abs(DeltaTemp) / Height, OneFourth);
    } else {
        return 1.235 * std::exp(0.0467 * Height) * std::pow(std::abs(DeltaTemp), 0.316);
    }
}

Real64 CallCalcFohannoPolidoriVerticalWall(EnergyPlusData &state,
                                           Real64 const DeltaTemp, // [C] temperature difference between surface and air
                                           Real64 const Height,    // [m] characteristic size, height of zone
                                           Real64 const SurfTemp,  // [C] surface temperature
                                           Real64 const QdotConv,  // [W/m2] heat flux rate for rayleigh #
                                           int const SurfNum       // for messages
)
{

    if (Height > 0.0) {
        return CalcFohannoPolidoriVerticalWall(DeltaTemp, Height, SurfTemp, QdotConv);
    } else {
        // bad value for Height, but we have little info to identify calling culprit
        if (state.dataConvectionCoefficient->CalcFohannoPolidoriVerticalWallErrorIDX == 0) {
            ShowSevereMessage(state, "CalcFohannoPolidoriVerticalWall: Convection model not evaluated (would divide by zero)");
            ShowContinueError(
                state, "Effective surface height is zero, convection model not applicable for surface =" + state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcFohannoPolidoriVerticalWall: Convection model not evaluated because zero height and set to 9.999 [W/m2-K]",
                                      state.dataConvectionCoefficient->CalcFohannoPolidoriVerticalWallErrorIDX);
        return 9.999;
    }
}

Real64 CalcKaradagChilledCeiling(Real64 const DeltaTemp) // [C] temperature difference between surface and air
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Jul 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for natural convection developed by Karadag for chilled ceilings

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Karadag, R. 2009. New approach relevant to total heat transfer coefficient
    //   including the effect of radiation and convection at the ceiling in a cooled
    //   ceiling room.  Applied Thermal Engineering 29 (2009) 1561-1565
    //    This function is for equation 8 in the reference

    // Return value
    Real64 Hn; // function result, natural convection coefficient

    Hn = 3.1 * std::pow(std::abs(DeltaTemp), 0.22);

    return Hn;
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserWindow(Real64 const AirSystemFlowRate,  // [m3/s] air system flow rate
                                                   Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                   Real64 const WindWallRatio,      // [ ] fraction of window area to wall area for zone
                                                   int const WindowLocationType     // index for location types
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for windows in zones with slot diffusers on them
    //  developed by Novoselac for RP-1416

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
    //  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

    if (ZoneExtPerimLength > 0.0) {
        if (WindWallRatio <= 0.5) {

            if (WindowLocationType == InConvWinLoc_UpperPartOfExteriorWall) {
                return 0.117 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
            } else if (WindowLocationType == InConvWinLoc_LowerPartOfExteriorWall) {
                return 0.093 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
            } else if (WindowLocationType == InConvWinLoc_LargePartOfExteriorWall) {
                return 0.117 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8); // assumption for case not covered by model
            } else if (WindowLocationType == InConvWinLoc_NotSet) {
                return 0.117 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8); // assumption for case not covered by model
            } else {
                // shouldn'tcome
                return 9.999;
            }
        } else {
            return 0.103 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
        }
    } else {
        return 9.999;
    }
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserWindow(EnergyPlusData &state,
                                                   Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                   Real64 const WindWallRatio,      // [ ] fraction of window area to wall area for zone
                                                   int const WindowLocationType,    // index for location types
                                                   int const ZoneNum                // for messages
)
{
    auto &Zone(state.dataHeatBal->Zone);

    Real64 AirSystemFlowRate = CalcZoneSystemVolFlowRate(state, ZoneNum);

    if (ZoneExtPerimLength > 0.0) {
        if (WindWallRatio <= 0.5) {

            if (WindowLocationType != InConvWinLoc_UpperPartOfExteriorWall && WindowLocationType != InConvWinLoc_LowerPartOfExteriorWall &&
                WindowLocationType != InConvWinLoc_LargePartOfExteriorWall && WindowLocationType != InConvWinLoc_NotSet) {
                if (state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX1 == 0) {
                    ShowSevereMessage(state,
                                      "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated (bad relative window location)");
                    ShowContinueError(state, format("Value for window location = {}", WindowLocationType));
                    ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                    ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
                }
                ShowRecurringSevereErrorAtEnd(state,
                                              "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because bad window "
                                              "location and set to 9.999 [W/m2-K]",
                                              state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX1);
            }
        }
    } else {
        if (state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX2 == 0) {
            ShowSevereMessage(state,
                              "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated (zero zone exterior perimeter length)");
            ShowContinueError(state, format("Value for zone exterior perimeter length = {:.5R}", ZoneExtPerimLength));
            ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWindowErrorIDX2);
    }
    return CalcGoldsteinNovoselacCeilingDiffuserWindow(AirSystemFlowRate, ZoneExtPerimLength, WindWallRatio, WindowLocationType);
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserWall(Real64 const AirSystemFlowRate,  // [m3/s] air system flow rate
                                                 Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                 int const WindowLocationType     // index for location types
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for exterior walls in zones with slot diffusers on them
    //  developed by Novoselac for RP-1416

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
    //  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

    if (ZoneExtPerimLength > 0.0) {
        if (WindowLocationType == InConvWinLoc_WindowAboveThis) {
            return 0.063 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
        } else if (WindowLocationType == InConvWinLoc_WindowBelowThis) {
            return 0.093 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
        } else if (WindowLocationType == InConvWinLoc_NotSet) {
            return 0.063 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8); // assumption for case not covered by model
        } else {
            return 9.999;
        }
    } else {
        return 9.999;
    }
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserWall(EnergyPlusData &state,
                                                 Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                 int const WindowLocationType,    // index for location types
                                                 int const ZoneNum                // for messages
)
{
    auto &Zone(state.dataHeatBal->Zone);

    Real64 AirSystemFlowRate = CalcZoneSystemVolFlowRate(state, ZoneNum);

    if (ZoneExtPerimLength > 0.0) {
        if (WindowLocationType != InConvWinLoc_WindowAboveThis && WindowLocationType != InConvWinLoc_WindowBelowThis &&
            WindowLocationType != InConvWinLoc_NotSet) {
            if (state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX1 == 0) {
                ShowSevereMessage(state, "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated (bad relative window location)");
                ShowContinueError(state, format("Value for window location = {}", WindowLocationType));
                ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
                ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
            }
            ShowRecurringSevereErrorAtEnd(
                state,
                "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because bad window location and set to 9.999 [W/m2-K]",
                state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX1);
        }
    } else {
        if (state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX2 == 0) {
            ShowSevereMessage(state,
                              "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated (zero zone exterior perimeter length)");
            ShowContinueError(state, format("Value for zone exterior perimeter length = {:.5R}", ZoneExtPerimLength));
            ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserWallErrorIDX2);
    }
    return CalcGoldsteinNovoselacCeilingDiffuserWall(AirSystemFlowRate, ZoneExtPerimLength, WindowLocationType);
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserFloor(Real64 const AirSystemFlowRate, // [m3/s] air system flow rate
                                                  Real64 const ZoneExtPerimLength // [m] length of zone perimeter with exterior walls
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate model equation for floors in zones with slot diffusers on them
    //  developed by Novoselac for RP-1416

    // METHODOLOGY EMPLOYED:
    // isolate function for equation.

    // REFERENCES:
    // Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
    //  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

    if (ZoneExtPerimLength > 0.0) {
        return 0.048 * std::pow(AirSystemFlowRate / ZoneExtPerimLength, 0.8);
    } else {
        return 9.999; // safe but noticeable
    }
}

Real64 CalcGoldsteinNovoselacCeilingDiffuserFloor(EnergyPlusData &state,
                                                  Real64 const ZoneExtPerimLength, // [m] length of zone perimeter with exterior walls
                                                  int const ZoneNum                // for messages
)
{
    auto &Zone(state.dataHeatBal->Zone);

    Real64 AirSystemFlowRate = CalcZoneSystemVolFlowRate(state, ZoneNum);

    if (ZoneExtPerimLength <= 0.0) {
        if (state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserFloorErrorIDX == 0) {
            ShowSevereMessage(state,
                              "CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated (zero zone exterior perimeter length)");
            ShowContinueError(state, format("Value for zone exterior perimeter length = {:.5R}", ZoneExtPerimLength));
            ShowContinueError(state, "Occurs for zone named = " + Zone(ZoneNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated because bad perimeter length and set to 9.999 [W/m2-K]",
            state.dataConvectionCoefficient->CalcGoldsteinNovoselacCeilingDiffuserFloorErrorIDX);
    }
    return CalcGoldsteinNovoselacCeilingDiffuserFloor(AirSystemFlowRate, ZoneExtPerimLength);
}

Real64 CalcSparrowWindward(int const RoughnessIndex, Real64 const FacePerimeter, Real64 const FaceArea, Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate Sparrow Hf for windward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate equation as a function

    // REFERENCES:

    //   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
    //   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
    //   width on heat transfer and fluid flow about an inclined rectangular plate.
    //   Journal of Heat Transfer 101:  204.
    //   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
    //   procedure with a detailed study of outside heat transfer parameters.
    //   M.S. Thesis, Department of Mechanical and Industrial Engineering,
    //   University of Illinois at Urbana-Champaign.

    return 2.537 * RoughnessMultiplier(RoughnessIndex) * std::sqrt(FacePerimeter * WindAtZ / FaceArea);
}

Real64 CalcSparrowLeeward(int const RoughnessIndex, Real64 const FacePerimeter, Real64 const FaceArea, Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate Sparrow Hf for leeward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate equation as a function

    // REFERENCES:

    //   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
    //   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
    //   width on heat transfer and fluid flow about an inclined rectangular plate.
    //   Journal of Heat Transfer 101:  204.
    //   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
    //   procedure with a detailed study of outside heat transfer parameters.
    //   M.S. Thesis, Department of Mechanical and Industrial Engineering,
    //   University of Illinois at Urbana-Champaign.

    return 0.5 * CalcSparrowWindward(RoughnessIndex, FacePerimeter, FaceArea, WindAtZ);
}

Real64 CalcSparrowWindward(
    EnergyPlusData &state, int const RoughnessIndex, Real64 const FacePerimeter, Real64 const FaceArea, Real64 const WindAtZ, int const SurfNum)
{
    auto &Surface(state.dataSurface->Surface);

    if (FaceArea > 0.0) {
        return CalcSparrowWindward(RoughnessIndex, FacePerimeter, FaceArea, WindAtZ);

    } else {
        if (state.dataConvectionCoefficient->CalcSparrowWindwardErrorIDX == 0) {
            ShowSevereMessage(state, "CalcSparrowWindward: Convection model not evaluated (bad face area)");
            ShowContinueError(state, format("Value for effective face area = {:.5R}", FaceArea));
            ShowContinueError(state, "Occurs for surface named = " + Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcSparrowWindward: Convection model not evaluated because bad face area and set to 9.999 [W/m2-k]",
                                      state.dataConvectionCoefficient->CalcSparrowWindwardErrorIDX);
        return 9.999; // safe but noticeable
    }
}

Real64 CalcSparrowLeeward(
    EnergyPlusData &state, int const RoughnessIndex, Real64 const FacePerimeter, Real64 const FaceArea, Real64 const WindAtZ, int const SurfNum)
{
    auto &Surface(state.dataSurface->Surface);

    if (FaceArea > 0.0) {
        return CalcSparrowLeeward(RoughnessIndex, FacePerimeter, FaceArea, WindAtZ);
    } else {
        if (state.dataConvectionCoefficient->CalcSparrowLeewardErrorIDX == 0) {
            ShowSevereMessage(state, "CalcSparrowLeeward: Convection model not evaluated (bad face area)");
            ShowContinueError(state, format("Value for effective face area = {:.5R}", FaceArea));
            ShowContinueError(state, "Occurs for surface named = " + Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcSparrowLeeward: Convection model not evaluated because bad face area and set to 9.999 [W/m2-k]",
                                      state.dataConvectionCoefficient->CalcSparrowLeewardErrorIDX);

        return 9.999; // safe but noticeable
    }
}

Real64 CalcMoWITTNatural(Real64 DeltaTemp)
{
    Real64 constexpr temp_fac(0.84);
    return temp_fac * std::pow(std::abs(DeltaTemp), 1.0 / 3.0);
}

Real64 CalcMoWITTForcedWindward(Real64 const WindAtZ)
{
    Real64 constexpr wind_fac(3.26); // = a, Constant, W/(m2K(m/s)^b)
    Real64 constexpr wind_exp(0.89); // = b
    return wind_fac * std::pow(WindAtZ, wind_exp);
}

Real64 CalcMoWITTForcedLeeward(Real64 const WindAtZ)
{
    Real64 constexpr wind_fac(3.55);  // = a, Constant, W/(m2K(m/s)^b)
    Real64 constexpr wind_exp(0.617); // = b
    return wind_fac * std::pow(WindAtZ, wind_exp);
}

Real64 CalcMoWITTWindward(Real64 const DeltaTemp, Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate MoWITT Hc equation for windward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate model equation in a function

    // REFERENCES:
    //   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
    //   film coefficient for windows in low-rise buildings.
    //   ASHRAE Transactions 100(1):  1087.

    Real64 Hn = CalcMoWITTNatural(DeltaTemp);
    Real64 Hf = CalcMoWITTForcedWindward(WindAtZ);
    return std::sqrt(pow_2(Hn) + pow_2(Hf));
}

Real64 CalcMoWITTLeeward(Real64 const DeltaTemp, Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate MoWITT Hc equation for leeward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate model equation in a function

    // REFERENCES:
    //   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
    //   film coefficient for windows in low-rise buildings.
    //   ASHRAE Transactions 100(1):  1087.

    Real64 Hn = CalcMoWITTNatural(DeltaTemp);
    Real64 Hf = CalcMoWITTForcedLeeward(WindAtZ);
    return std::sqrt(pow_2(Hn) + pow_2(Hf));
}

Real64 CalcDOE2Forced(Real64 const SurfaceTemp, Real64 const AirTemp, Real64 const CosineTilt, Real64 const HfSmooth, int const RoughnessIndex)
{
    // This allows costly HfSmooth to be calculated independently (avoids excessive use of std::pow() in Kiva)
    Real64 Hn = CalcASHRAETARPNatural(SurfaceTemp, AirTemp, CosineTilt);
    Real64 HcSmooth = std::sqrt(pow_2(Hn) + pow_2(HfSmooth));
    return RoughnessMultiplier(RoughnessIndex) * (HcSmooth - Hn);
}

Real64 CalcDOE2Windward(Real64 const SurfaceTemp, Real64 const AirTemp, Real64 const CosineTilt, Real64 const WindAtZ, int const RoughnessIndex)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate DOE-2 Hf equation for windward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate model equation in a function

    // REFERENCES:
    //   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
    //   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
    //   film coefficient for windows in low-rise buildings.
    //   ASHRAE Transactions 100(1):  1087.
    Real64 HfSmooth = CalcMoWITTForcedWindward(WindAtZ);

    return CalcDOE2Forced(SurfaceTemp, AirTemp, CosineTilt, HfSmooth, RoughnessIndex);
}

Real64 CalcDOE2Leeward(Real64 const SurfaceTemp, Real64 const AirTemp, Real64 const CosineTilt, Real64 const WindAtZ, int const RoughnessIndex)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate DOE-2 Hf equation for leeward surfaces

    // METHODOLOGY EMPLOYED:
    // encapsulate model equation in a function

    // REFERENCES:
    //   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
    //   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
    //   film coefficient for windows in low-rise buildings.
    //   ASHRAE Transactions 100(1):  1087.

    Real64 HfSmooth = CalcMoWITTForcedLeeward(WindAtZ);

    return CalcDOE2Forced(SurfaceTemp, AirTemp, CosineTilt, HfSmooth, RoughnessIndex);
}

Real64 CalcNusseltJurges(Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using Nusselt Jurges correlation
    // model is attributed to Nusselt and Jurges but the equation is recast in current units
    // by Palyvos

    // METHODOLOGY EMPLOYED:
    // encapsulate the model equation in a function

    // REFERENCES:
    // 1. Nusselt, W., W. Jurges. 1922. Die Kuhlung einer ebenen Wand durch einen Luftstrom
    //     (The cooling of a plane wall by an air flow). Gesundheits Ingenieur 52, Heft, 45, Jargang.
    // 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
    //     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

    // Return value
    Real64 Hc;

    Hc = 5.8 + 3.94 * WindAtZ;

    return Hc;
}

Real64 CalcMcAdams(Real64 const WindAtZ)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using McAdams correlation
    // model is attributed to McAdams but the equation is as recast in current units
    // by Palyvos

    // METHODOLOGY EMPLOYED:
    // encapsulate the model equation in a function

    // REFERENCES:
    // 1. McAdams, W.H., 1954. Heat Transmission, third ed., McGraw-Hill, New York.
    // 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
    //     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

    // Return value
    Real64 Hc;

    Hc = 5.8 + 3.8 * WindAtZ;

    return Hc;
}

Real64 CalcMitchell(Real64 const WindAtZ, Real64 const LengthScale)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using Mitchell correlation
    // model is attributed to Mitchell but the equation is as recast in current units
    // by Palyvos

    // METHODOLOGY EMPLOYED:
    // encapsulate the model equation in a function

    // REFERENCES:
    // 1. Mitchell, J.W., 1976. Heat transfer from spheres and other animal forms. Biophy. J. 16 (1976) 561
    // 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
    //     envelope energy systems' modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

    return 8.6 * std::pow(WindAtZ, 0.6) / std::pow(LengthScale, 0.4);
}

Real64 CalcMitchell(EnergyPlusData &state, Real64 const WindAtZ, Real64 const LengthScale, int const SurfNum)
{
    if (LengthScale > 0.0) {
        return CalcMitchell(WindAtZ, LengthScale);
    } else {
        if (state.dataConvectionCoefficient->CalcMitchellErrorIDX == 0) {
            ShowSevereMessage(state, "CalcMitchell: Convection model not evaluated (bad length scale)");
            ShowContinueError(state, format("Value for effective length scale = {:.5R}", LengthScale));
            ShowContinueError(state, "Occurs for surface named = " + state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcMitchell: Convection model not evaluated because bad length scale and set to 9.999 [W/m2-k]",
                                      state.dataConvectionCoefficient->CalcMitchellErrorIDX);
        return 9.999; // safe but noticeable
    }
}

Real64 CalcBlockenWindward(Real64 const WindAt10m,
                           Real64 const WindDir,    // Wind direction measured clockwise from geographhic North
                           Real64 const SurfAzimuth // or Facing, Direction the surface outward normal faces (degrees)
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using Blocken correlation

    // METHODOLOGY EMPLOYED:
    // encapsulate model in function

    // REFERENCES:
    // Blocken, B., T. Defraeye, D. Derome, J. Carmeliet. 2009.
    //  High-Resolution CFD Simulations for Forced Convection
    //   Heat Transfer Coefficients at the Facade of a Low-Rise Building.
    //   Building and Environment 44 (2009) 2396 - 2412.

    // Return value
    Real64 Hf;

    Real64 Theta; // angle between wind and surface azimuth

    Theta = WindDir - SurfAzimuth - 90.0; // TODO double check theta
    if (Theta > 180.0) Theta -= 360.0;

    if (Theta <= 11.25) {
        Hf = 4.6 * std::pow(WindAt10m, 0.89);
    } else if ((11.25 < Theta) && (Theta <= 33.75)) {
        Hf = 5.0 * std::pow(WindAt10m, 0.8);
    } else if ((33.75 < Theta) && (Theta <= 56.25)) {
        Hf = 4.6 * std::pow(WindAt10m, 0.84);
    } else if ((56.25 < Theta) && (Theta <= 100.0)) {
        Hf = 4.5 * std::pow(WindAt10m, 0.81);
    } else {
        // should not be used for leeward... check why come here?
        Hf = 3.54 * std::pow(WindAt10m, 0.76); // emmel model for robustness?
    }
    return Hf;
}

Real64 CalcEmmelVertical(EnergyPlusData &state,
                         Real64 const WindAt10m,
                         Real64 const WindDir,     // Wind direction measured clockwise from geographhic North
                         Real64 const SurfAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
                         int const SurfNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using Emmel correlation
    // for vertical walls

    // METHODOLOGY EMPLOYED:
    // encapsulate model in function

    // REFERENCES:
    // Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
    //   heat transfer coefficient correlations for isolated low-rise buildings.
    //    Energy and Buildings 39 (2007) 335- 342

    // Return value
    Real64 Hf;

    Real64 Theta; // angle between wind and surface azimuth

    Theta = WindDir - SurfAzimuth - 90.0; // TODO double check theta
    if (Theta > 180.0) Theta -= 360.0;

    if (Theta <= 22.5) {
        Hf = 5.15 * std::pow(WindAt10m, 0.81);
    } else if ((22.5 < Theta) && (Theta <= 67.5)) {
        Hf = 3.34 * std::pow(WindAt10m, 0.84);
    } else if ((67.5 < Theta) && (Theta <= 112.5)) {
        Hf = 4.78 * std::pow(WindAt10m, 0.71);
    } else if ((112.5 < Theta) && (Theta <= 157.5)) {
        Hf = 4.05 * std::pow(WindAt10m, 0.77);
    } else if ((157.5 < Theta) && (Theta <= 180.0)) {
        Hf = 3.54 * std::pow(WindAt10m, 0.76);

    } else {
        if (state.dataConvectionCoefficient->CalcEmmelVerticalErrorIDX == 0) {
            ShowSevereMessage(state, "CalcEmmelVertical: Convection model wind angle calculation suspect (developer issue)");
            ShowContinueError(state, format("Value for theta angle = {:.5R}", Theta));
            ShowContinueError(state, "Occurs for surface named = " + state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection model uses high theta correlation and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcEmmelVertical: Convection model wind angle calculation suspect and high theta correlation",
                                      state.dataConvectionCoefficient->CalcEmmelVerticalErrorIDX);
        Hf = 3.54 * std::pow(WindAt10m, 0.76);
    }
    return Hf;
}

Real64 CalcEmmelRoof(EnergyPlusData &state,
                     Real64 const WindAt10m,
                     Real64 const WindDir,                // Wind direction measured clockwise from geographhic North
                     Real64 const LongAxisOutwardAzimuth, // or Facing, Direction the surface outward normal faces (degrees)
                     int const SurfNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   Aug 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // calculate model equation for forced convection using Emmel correlation
    // for horizontal roofs

    // METHODOLOGY EMPLOYED:
    // encapsulate model in function

    // REFERENCES:
    // Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
    //   heat transfer coefficient correlations for isolated low-rise buildings.
    //    Energy and Buildings 39 (2007) 335- 342

    // Return value
    Real64 Hf;

    Real64 Theta; // angle between wind and surface azimuth

    Theta = WindDir - LongAxisOutwardAzimuth - 90.0; // TODO double check theta
    if (Theta > 180.0) Theta -= 360.0;

    if (Theta <= 22.5) {
        Hf = 5.15 * std::pow(WindAt10m, 0.81);
    } else if ((22.5 < Theta) && (Theta <= 67.5)) {
        Hf = 3.34 * std::pow(WindAt10m, 0.84);
    } else if ((67.5 < Theta) && (Theta <= 112.5)) {
        Hf = 4.78 * std::pow(WindAt10m, 0.71);
    } else if ((112.5 < Theta) && (Theta <= 157.5)) {
        Hf = 4.05 * std::pow(WindAt10m, 0.77);
    } else if ((157.5 < Theta) && (Theta <= 180.0)) {
        Hf = 3.54 * std::pow(WindAt10m, 0.76);

    } else {
        if (state.dataConvectionCoefficient->CalcEmmelRoofErrorIDX == 0) {
            ShowSevereMessage(state, "CalcEmmelRoof: Convection model wind angle calculation suspect (developer issue)");
            ShowContinueError(state, format("Value for theta angle = {:.5R}", Theta));
            ShowContinueError(state, "Occurs for surface named = " + state.dataSurface->Surface(SurfNum).Name);
            ShowContinueError(state, "Convection model uses high theta correlation and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(state,
                                      "CalcEmmelRoof: Convection model wind angle calculation suspect and high theta correlation",
                                      state.dataConvectionCoefficient->CalcEmmelRoofErrorIDX);

        Hf = 3.54 * std::pow(WindAt10m, 0.76);
    }
    return Hf;
}

Real64 CalcClearRoof(EnergyPlusData &state,
                     Real64 const SurfTemp,
                     Real64 const AirTemp,
                     Real64 const WindAtZ,
                     Real64 const RoofArea,
                     Real64 const RoofPerimeter,
                     int const RoughnessIndex)
{
    // Using/Aliasing
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    // FUNCTION PARAMETER DEFINITIONS:
    Real64 const g(9.81);     // gravity constant (m/s**2)
    Real64 const v(15.89e-6); // kinematic viscosity (m**2/s) for air at 300 K
    Real64 const k(0.0263);   // thermal conductivity (W/m K) for air at 300 K
    Real64 const Pr(0.71);    // Prandtl number for air at ?

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 DeltaTemp;
    Real64 Ln;
    Real64 RaLn; // Rayleigh number
    Real64 GrLn; // Grashof number
    Real64 AirDensity;
    Real64 Rex; // Reynolds number
    Real64 x;   // distance to roof edge toward wind direction
    Real64 eta;
    Array1D<Real64> RfARR(6);
    Real64 BetaFilm;

    // find x, don't know x. avoid time consuming geometry algorithm
    x = std::sqrt(RoofArea) / 2.0; // quick simplification, geometry routines to develop

    if (RoofPerimeter > 0.0) {
        Ln = RoofArea / RoofPerimeter;
    } else {
        Ln = std::sqrt(RoofArea);
    }
    DeltaTemp = SurfTemp - AirTemp;
    BetaFilm = 1.0 / (DataGlobalConstants::KelvinConv + SurfTemp + 0.5 * DeltaTemp);
    AirDensity = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, AirTemp, state.dataEnvrn->OutHumRat);

    GrLn = g * pow_2(AirDensity) * pow_3(Ln) * std::abs(DeltaTemp) * BetaFilm / pow_2(v);
    RaLn = GrLn * Pr;

    Rex = WindAtZ * AirDensity * x / v;

    Real64 Rf = RoughnessMultiplier(RoughnessIndex);
    if (Rex > 0.1) { // avoid zero and crazy small denominators
        Real64 tmp = std::log(1.0 + GrLn / pow_2(Rex));
        eta = tmp / (1.0 + tmp);
    } else {
        eta = 1.0; // forced convection gone because no wind
    }

    return eta * (k / Ln) * 0.15 * std::pow(RaLn, OneThird) + (k / x) * Rf * 0.0296 * std::pow(Rex, FourFifths) * std::pow(Pr, OneThird);
}

Real64 CalcClearRoof(EnergyPlusData &state,
                     int const SurfNum,
                     Real64 const SurfTemp,
                     Real64 const AirTemp,
                     Real64 const WindAtZ,
                     [[maybe_unused]] Real64 const WindDirect, // Wind direction measured clockwise from geographhic North
                     Real64 const RoofArea,
                     Real64 const RoofPerimeter)
{
    auto &Surface(state.dataSurface->Surface);

    Real64 x; // distance to roof edge toward wind direction

    int const RoughnessIndex = state.dataMaterial->Material(state.dataConstruction->Construct(Surface(SurfNum).Construction).LayerPoint(1)).Roughness;
    // find x, don't know x. avoid time consuming geometry algorithm
    x = std::sqrt(RoofArea) / 2.0; // quick simplification, geometry routines to develop

    if (x > 0.0) {
        return CalcClearRoof(state, SurfTemp, AirTemp, WindAtZ, RoofArea, RoofPerimeter, RoughnessIndex);
    } else {
        if (state.dataConvectionCoefficient->CalcClearRoofErrorIDX == 0) {
            ShowSevereMessage(state, "CalcClearRoof: Convection model not evaluated (bad value for distance to roof edge)");
            ShowContinueError(state, format("Value for distance to roof edge ={:.3R}", x));
            ShowContinueError(state, "Occurs for surface named = " + Surface(SurfNum).Name);
            ShowContinueError(state, "Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues");
        }
        ShowRecurringSevereErrorAtEnd(
            state,
            "CalcClearRoof: Convection model not evaluated because bad value for distance to roof edge and set to 9.999 [W/m2-k]",
            state.dataConvectionCoefficient->CalcClearRoofErrorIDX);
        return 9.9999; // safe but noticeable
    }
}

void CalcASTMC1340ConvCoeff(EnergyPlusData &state,
                            int const SurfNum,                  // surface number for which coefficients are being calculated
                            Real64 const SurfaceTemperature,    // Temperature of surface for evaluation of HcIn
                            Real64 const ZoneMeanAirTemperature // Mean Air Temperature of Zone
)
{
    auto &Surface(state.dataSurface->Surface);

    int ZoneNum = Surface(SurfNum).Zone;
    Real64 Volume = state.dataHeatBal->Zone(ZoneNum).Volume; // Volume of the zone in m3
    Real64 Vair = std::pow(Volume, OneThird) * CalcZoneSystemACH(state, ZoneNum) / 3600;

    state.dataHeatBal->HConvIn(SurfNum) =
        CalcASTMC1340ConvCoeff(state, SurfNum, SurfaceTemperature, ZoneMeanAirTemperature, Vair, Surface(SurfNum).Tilt);

    // Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    if (state.dataHeatBal->HConvIn(SurfNum) < state.dataHeatBal->LowHConvLimit)
        state.dataHeatBal->HConvIn(SurfNum) = state.dataHeatBal->LowHConvLimit;
}

Real64 CalcASTMC1340ConvCoeff(EnergyPlusData &state, int const SurfNum, Real64 const Tsurf, Real64 const Tair, Real64 const Vair, Real64 const Tilt)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Dareum Nam
    //       DATE WRITTEN   Feb 2021
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Calculate the inside convection coefficient for attic zones containing radiant barriers

    // REFERENCES:
    // 1. ASTM C1340: Standard Practice for Estimation of Heat Gain or Loss Through Ceilings Under Attics
    // Containing Radiant Barriers by Use of a Computer Program
    // 2. Fontanini, A. D., Aguilar, J. L. C., Mitchell, M. S., Kosny, J., Merket, N., DeGraw, J. W., & Lee, E. (2018).
    // Predicting the performance of radiant technologies in attics: Reducing the discrepancies between attic specific
    // and whole-building energy models. Energy and Buildings, 169, 69-83.

    // Return Value
    Real64 h; // Combined convection coefficient

    Real64 Nun;       // Nusselt number for natural convection
    Real64 Nuf;       // Nusselt number for forced convection
    Real64 hn;        // Natural convection coefficient
    Real64 hf;        // Forced convection coefficient
    Real64 Grc;       // Critical Grashof number
    Real64 DeltaTemp; // Temperature difference between TSurf and Tair
    Real64 L;         // Characteristic length: the length along the heat flow direction
              // (the square root of surface area for floors and ceilings, average height for gables and walls, and length of pitched roof from soffit
              // to ridge)
    Real64 v;       // The velocity of the air stream in m/s, (for interior surfaces)
                    // Surface Outside Face Outdoor Air Wind Speed (for exterior surfaces)
    Real64 Pr;      // Prandtl number
    Real64 beta_SI; // Volume coefficient of expansion of air, 1/K
    Real64 rho_SI;  // Density of air, kg/m3
    Real64 cp_SI;   // Specific heat of air, J/kg.k
    Real64 dv;
    Real64 visc; // Kinematic viscosity of air, m2/s
    Real64 k_SI_n;
    Real64 k_SI_d;
    Real64 k_SI;                                               // Thermal conductivity of air, W/m.K
    Real64 Ra;                                                 // Rayleigh number
    Real64 Re;                                                 // Reynolds number
    constexpr Real64 g = DataGlobalConstants::GravityConstant; // Acceleration of gravity, m/s2
    constexpr Real64 OneThird((1.0 / 3.0));                    // 1/3 in highest precision

    auto &Surface(state.dataSurface->Surface);

    if (Tilt == 0 || Tilt == 180) { // Horizontal surface
        L = std::sqrt(Surface(SurfNum).Area);
    } else {
        L = Surface(SurfNum).Height;
    }

    if (Surface(SurfNum).ExtBoundCond == 0) {
        v = state.dataSurface->SurfOutWindSpeed(SurfNum);
    } else {
        v = Vair;
    }

    Pr = 0.7880 - (2.631 * std::pow(10, -4) * (Tair + 273.15));
    beta_SI = 1 / (Tair + 273.15);
    rho_SI = (22.0493 / (Tair + 273.15)) * 16;
    cp_SI = 0.068559 * (3.4763 + (1.066 * std::pow(10, -4) * (Tair + 273.15))) * 4186.8;
    dv = (241.9 * std::pow(10, -7)) * (145.8 * (Tair + 273.15) * std::pow((Tair + 273.15), 0.5)) / ((Tair + 273.15) + 110.4);
    visc = dv * (0.45359237 / (0.3048 * 3600)) / rho_SI;
    k_SI_n = (0.6325 * std::pow(10, -5) * std::pow((Tair + 273.15), 0.5) * 241.77);
    k_SI_d = (1.0 + (245.4 * std::pow(10, (-12 / (Tair + 273.15)))) / (Tair + 273.15));
    k_SI = 1.730735 * (k_SI_n / k_SI_d);

    // Calculation of DeltaTemp
    DeltaTemp = Tsurf - Tair;

    Ra = std::abs(g * beta_SI * rho_SI * cp_SI * DeltaTemp * (L * L * L)) / (visc * k_SI);
    Re = (v * L) / visc;

    // Natural convection (Nun)
    if (Tilt == 0) {         // Horizontal surface: Roof
        if (DeltaTemp > 0) { // heat flow down
            Nun = 0.58 * std::pow(Ra, 0.2);
        } else if (Ra < 8000000) { // heat flow up
            Nun = 0.54 * std::pow(Ra, 0.25);
        } else {
            Nun = 0.15 * std::pow(Ra, OneThird);
        }
    } else if (Tilt > 0 && Tilt < 90) { // Tilted roof
        if (DeltaTemp > 0) {            // heat flow down
            if (Tilt < 2) {
                Nun = 0.58 * std::pow(Ra, 0.2);
            } else {
                Nun = 0.56 * std::pow(Ra * (std::sin(Tilt * DataGlobalConstants::DegToRadians)), 0.25);
            }
        } else { // heat flow up
            if (Tilt < 15) {
                Grc = 1000000;
            } else if (Tilt <= 75.0) {
                Grc = std::pow(10, Tilt / (1.1870 + (0.0870 * Tilt)));
            } else {
                Grc = 5000000000;
            }
            if ((Ra / Pr) <= Grc) {
                Nun = 0.56 * std::pow(Ra * (std::sin(Tilt * 3.14159 / 180)), 0.25);
            } else {
                Nun = 0.14 * (std::pow(Ra, OneThird) - std::pow(Grc * Pr, OneThird)) +
                      0.56 * std::pow(Grc * Pr * (std::sin(Tilt * DataGlobalConstants::DegToRadians)), 0.25);
            }
        }
    } else if (Tilt == 180) { // Horizontal surface: Floor
        if (DeltaTemp <= 0) { // heat flow down
            Nun = 0.58 * std::pow(Ra, 0.2);
        } else if (Ra < 8000000) { // heat flow up
            Nun = 0.54 * std::pow(Ra, 0.25);
        } else {
            Nun = 0.15 * std::pow(Ra, OneThird);
        }
    } else if (Tilt > 90 && Tilt < 180) { // Tilted Floor
        if (DeltaTemp <= 0) {             // heat flow down
            if (Tilt > 178) {
                Nun = 0.58 * std::pow(Ra, 0.2);
            } else {
                Nun = 0.56 * std::pow(Ra * (std::sin(Tilt * DataGlobalConstants::DegToRadians)), 0.25);
            }
        } else { // heat flow up
            if (Tilt > 165) {
                Grc = 1000000;
            } else if (Tilt <= 105.0) {
                Grc = std::pow(10, Tilt / (1.1870 + (0.0870 * Tilt)));
            } else {
                Grc = 5000000000;
            }
            if ((Ra / Pr) <= Grc) {
                Nun = 0.56 * std::pow(Ra * (std::sin(Tilt * DataGlobalConstants::DegToRadians)), 0.25);
            } else {
                Nun = 0.14 * (std::pow(Ra, OneThird) - std::pow(Grc * Pr, OneThird)) +
                      0.56 * std::pow(Grc * Pr * (std::sin(Tilt * DataGlobalConstants::DegToRadians)), 0.25);
            }
        }
    } else { // Vertical wall (Tilt = 90)
        if (Ra < 1000000000) {
            Nun = 0.59 * std::pow(Ra, 0.25);
        } else {
            Nun = 0.10 * std::pow(Ra, OneThird);
        }
    }

    // Forced convection (Nuf)
    if (Re < 500000) {
        Nuf = 0.664 * std::pow(Pr, OneThird) * std::pow(Re, 0.5);
    } else {
        Nuf = std::pow(Pr, OneThird) * ((0.037 * std::pow(Re, 0.8)) - 850);
    }

    // Combined convection coefficient
    hf = Nuf * k_SI / L;
    hn = Nun * k_SI / L;
    h = std::pow((std::pow(hf, 3) + std::pow(hn, 3)), OneThird);

    return h;
}

} // namespace EnergyPlus::ConvectionCoefficients
