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

#ifndef HeatBalanceSurfaceManager_hh_INCLUDED
#define HeatBalanceSurfaceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DataSurfaces {
    struct SurfaceData;
}
namespace DataHeatBalance {
    struct ZoneData;
}

namespace HeatBalanceSurfaceManager {

    // Initialization routines for module

    // Algorithms for the module
    // These old external subroutines have been moved into the namespace and are no longer externals
    // CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
    //  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

    // Record Keeping/Utility Routines for Module

    // Reporting routines for module

    // Functions
    void clear_state();

    void ManageSurfaceHeatBalance(EnergyPlusData &state);

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitSurfaceHeatBalance(EnergyPlusData &state);

    void GatherForPredefinedReport(EnergyPlusData &state);

    void AllocateSurfaceHeatBalArrays(EnergyPlusData &state);

    void InitThermalAndFluxHistories(EnergyPlusData &state);

    void InitSolarHeatGains(EnergyPlusData &state);

    void InitIntSolarDistribution(EnergyPlusData &state);

    void ComputeIntThermalAbsorpFactors(EnergyPlusData &state);

    void ComputeIntSWAbsorpFactors(EnergyPlusData &state);

    void ComputeDifSolExcZonesWIZWindows(EnergyPlusData &state, int NumberOfEnclosures); // Number of solar enclosures

    void InitEMSControlledSurfaceProperties(EnergyPlusData &state);

    void InitEMSControlledConstructions(EnergyPlusData &state);

    // End Initialization Section of the Module
    //******************************************************************************

    // Begin Algorithm Section of the Module
    //******************************************************************************

    // Beginning of Record Keeping subroutines for the HB Module
    // *****************************************************************************

    void UpdateFinalSurfaceHeatBalance(EnergyPlusData &state);

    void UpdateThermalHistories(EnergyPlusData &state);

    void CalculateZoneMRT(EnergyPlusData &state, Optional_int_const ZoneToResimulate = _); // if passed in, then only calculate surfaces that have this zone

    // End of Record Keeping subroutines for the HB Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the HB Module
    // *****************************************************************************

    void ReportSurfaceHeatBalance();

    void ReportIntMovInsInsideSurfTemp();

    void CalcThermalResilience(EnergyPlusData &state);

    void ReportThermalResilience(EnergyPlusData &state);

    void ReportCO2Resilience(EnergyPlusData &state);

    void ReportVisualResilience(EnergyPlusData &state);

    // End of Reporting subroutines for the HB Module

    // Formerly EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager) but now moved into namespace HeatBalanceSurfaceManager

    void CalcHeatBalanceOutsideSurf(EnergyPlusData &state,
                                    Optional_int_const ZoneToResimulate = _); // if passed in, then only calculate surfaces that have this zone

    Real64 GetQdotConvOutRepPerArea(int SurfNum);

    void CalcHeatBalanceInsideSurf(EnergyPlusData &state,
                                   Optional_int_const ZoneToResimulate = _); // if passed in, then only calculate surfaces that have this zone

    void CalcHeatBalanceInsideSurf2(EnergyPlusData &state,
                                    const std::vector<int> &HTSurfs,          // Heat transfer surfaces to simulate (opaque and windows)
                                    const std::vector<int> &IZSurfs,          // Interzone heat transfer surfaces to simulate
                                    const std::vector<int> &HTNonWindowSurfs, // Non-window heat transfer surfaces to simulate
                                    const std::vector<int> &HTWindowSurfs,    // Window heat transfer surfaces to simulate
                                    Optional_int_const ZoneToResimulate = _);

    void CalcHeatBalanceInsideSurf2CTFOnly(EnergyPlusData &state,
                                           const int FirstZone,             // First zone to simulate
                                           const int LastZone,              // Last zone to simulate
                                           const std::vector<int> &IZSurfs, // Last zone to simulate
                                           Optional_int_const ZoneToResimulate = _);

    void TestSurfTempCalcHeatBalanceInsideSurf(Real64 TH12,
                                               DataSurfaces::SurfaceData &surface,
                                               DataHeatBalance::ZoneData &zone,
                                               int WarmupSurfTemp);

    void CalcOutsideSurfTemp(EnergyPlusData &state,
                             int SurfNum,      // Surface number DO loop counter
                             int ZoneNum,      // Zone number the current surface is attached to
                             int ConstrNum,    // Construction index for the current surface
                             Real64 HMovInsul, // "Convection" coefficient of movable insulation
                             Real64 TempExt,   // Exterior temperature boundary condition
                             bool &ErrorFlag         // Error flag for movable insulation problem
    );

    void CalcExteriorVentedCavity(EnergyPlusData &state, int const SurfNum); // index of surface

    void GatherComponentLoadsSurfAbsFact();

} // namespace HeatBalanceSurfaceManager

} // namespace EnergyPlus

#endif
