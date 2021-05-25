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

#ifndef DataHeatBalSurface_hh_INCLUDED
#define DataHeatBalSurface_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHeatBalSurface {

    // Data
    // MODULE PARAMETER DEFINITIONS
    Real64 constexpr MinSurfaceTempLimit(-100.0);            // Lowest inside surface temperature allowed in Celsius
    Real64 constexpr MinSurfaceTempLimitBeforeFatal(-250.0); // 2.5 times MinSurfaceTempLimit
    Real64 constexpr DefaultSurfaceTempLimit(200.0);         // Highest inside surface temperature allowed in Celsius
    Real64 constexpr IterDampConst(5.0);                     // Damping constant for inside surface temperature iterations
    int constexpr ItersReevalConvCoeff(30);                  // Number of iterations between inside convection coefficient reevaluations
    int constexpr MaxIterations(500);                        // Maximum number of iterations allowed for inside surface temps
    Real64 constexpr PoolIsOperatingLimit(0.0001);           // Limit to determine if swimming pool is operating or not
    int constexpr MinEMPDIterations(4);                      // Minimum number of iterations required for EMPD solution
    int constexpr IterationsForCondFDRelaxChange(5);         // number of iterations for inside temps that triggers a change

} // namespace DataHeatBalSurface

struct HeatBalSurfData : BaseGlobalStruct
{

    std::vector<bool> Zone_has_mixed_HT_models; // True if any surfaces in zone use CondFD, HAMT, or Kiva

    // Integer Variables for the Heat Balance Simulation
    Array1D_int SUMH; // From Old Bldctf.inc

    // Surface heat balance limits and convergence parameters
    Real64 MaxSurfaceTempLimit = 200.0;            // Highest inside surface temperature allowed in Celsius
    Real64 MaxSurfaceTempLimitBeforeFatal = 500.0; // 2.5 times MaxSurfaceTempLimit
    int MinIterations = 1;                         // Minimum number of iterations for surface heat balance

    // Variables Dimensioned to Max Number of Heat Transfer Surfaces (maxhts)
    Array1D<Real64> CTFConstInPart;  // Constant Inside Portion of the CTF calculation
    Array1D<Real64> CTFConstOutPart; // Constant Outside Portion of the CTF calculation
    // This group of arrays (soon to be vectors) added to facilitate vectorizable loops in CalcHeatBalanceInsideSurf2CTFOnly
    Array1D<Real64> CTFCross0;     // Construct.CTFCross(0)
    Array1D<Real64> CTFInside0;    // Construct.CTFInside(0)
    Array1D<Real64> CTFSourceIn0;  // Construct.CTFSourceIn(0)
    Array1D<Real64> TH11Surf;      // TH(1,1,SurfNum)
    Array1D<Real64> QsrcHistSurf1; // QsrcHist(SurfNum, 1)

    // todo: merge Is and IsNot and reduce the assignation at each time steps
    Array1D_int IsAdiabatic;      // 0 not adiabatic, 1 is adiabatic
    Array1D_int IsNotAdiabatic;   // 1 not adiabatic, 0 is adiabatic
    Array1D_int IsSource;         // 0 no internal source/sink, 1 has internal source/sing
    Array1D_int IsNotSource;      // 1 no internal source/sink, 0 has internal source/sing
    Array1D_int IsPoolSurf;       // 0 not pool, 1 is pool
    Array1D_int IsNotPoolSurf;    // 1 not pool, 0 is pool
    Array1D<Real64> TempTermSurf; // TempTerm for heatbalance equation
    Array1D<Real64> TempDivSurf;  // Divisor for heatbalance equation
    // end group added to support CalcHeatBalanceInsideSurf2CTFOnly
    Array1D<Real64> TempSurfIn;          // Temperature of the Inside Surface for each heat transfer surface
    Array1D<Real64> TempInsOld;          // TempSurfIn from previous iteration for convergence check
    Array1D<Real64> TempSurfInTmp;       // Inside Surface Temperature Of Each Heat Transfer Surface
    Array1D<Real64> HcExtSurf;           // Outside Convection Coefficient
    Array1D<Real64> HAirExtSurf;         // Outside Convection Coefficient to Air
    Array1D<Real64> HSkyExtSurf;         // Outside Convection Coefficient to Sky
    Array1D<Real64> HGrdExtSurf;         // Outside Convection Coefficient to Ground
    Array1D<Real64> TempSource;          // Temperature at the source location for each heat transfer surface
    Array1D<Real64> TempUserLoc;         // Temperature at the user specified location for each heat transfer surface
    Array1D<Real64> TempSurfInRep;       // Temperature of the Inside Surface for each heat transfer surface
    Array1D<Real64> TempSurfInMovInsRep; // Temperature of interior movable insulation on the side facing the zone

    // todo: to SurfRep arrays
    // (report)
    Array1D<Real64> QConvInReport; // Surface convection heat gain at inside face [J]
    Array1D<Real64> QdotConvInRep; // Surface convection heat transfer rate at inside face surface [W]
    // (report)
    Array1D<Real64> QdotConvInRepPerArea; // Surface conv heat transfer rate per m2 at inside face surf
    //  (report){w/m2]

    // these next three all are for net IR thermal radiation exchange with other surfaces in the model.
    Array1D<Real64> QRadNetSurfInReport;        // Surface thermal radiation heat gain at Inside face [J]
    Array1D<Real64> QdotRadNetSurfInRep;        // Surface thermal radiation heat transfer inside face surface [W]
    Array1D<Real64> QdotRadNetSurfInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Inside face surf
    // these next three all are for solar radiation gains on inside face
    Array1D<Real64> QRadSolarInReport;        // Surface thermal radiation heat gain at Inside face [J]
    Array1D<Real64> QdotRadSolarInRep;        // Surface thermal radiation heat transfer inside face surface [W]
    Array1D<Real64> QdotRadSolarInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Inside face surf
    // these next three all are for Lights visible radiation gains on inside face
    Array1D<Real64> QRadLightsInReport;        // Surface thermal radiation heat gain at Inside face [J]
    Array1D<Real64> QdotRadLightsInRep;        // Surface thermal radiation heat transfer inside face surface [W]
    Array1D<Real64> QdotRadLightsInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Inside face surf
    // these next three all are for Internal Gains sources of radiation gains on inside face
    Array1D<Real64> QRadIntGainsInReport;        // Surface thermal radiation heat gain at Inside face [J]
    Array1D<Real64> QdotRadIntGainsInRep;        // Surface thermal radiation heat transfer inside face surface [W]
    Array1D<Real64> QdotRadIntGainsInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Inside face surf
    // these next three all are for Radiative HVAC sources of radiation gains on inside face
    Array1D<Real64> QRadHVACInReport;        // Surface thermal radiation heat gain at Inside face [J]
    Array1D<Real64> QdotRadHVACInRep;        // Surface thermal radiation heat transfer inside face surface [W]
    Array1D<Real64> QdotRadHVACInRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Inside face surf

    Array1D<Real64> QConvOutReport;        // Surface convection heat gain at Outside face [J]
    Array1D<Real64> QdotConvOutRep;        // Surface convection heat transfer rate at Outside face surface [W]
    Array1D<Real64> QdotConvOutRepPerArea; // Surface conv heat transfer rate per m2 at Outside face surf
    //  (report){w/m2]

    Array1D<Real64> QRadOutReport;        // Surface thermal radiation heat gain at Outside face [J]
    Array1D<Real64> QdotRadOutRep;        // Surface thermal radiation heat transfer outside face surface [W]
    Array1D<Real64> QdotRadOutRepPerArea; // [W/m2]Surface thermal radiation heat transfer rate per m2 at
    //      Outside face surf
    Array1D<Real64> QAirExtReport;  // Surface Outside Face Thermal Radiation to Air Heat Transfer Rate [W]
    Array1D<Real64> QHeatEmiReport; // Surface Outside Face Heat Emission to Air Rate [W]

    Array1D<Real64> SurfOpaqInsFaceCondGainRep; // Equals Opaq Surf Ins Face Cond
    // when Opaq Surf Ins Face Cond >= 0
    Array1D<Real64> SurfOpaqInsFaceCondLossRep; // Equals -Opaq Surf Ins Face Cond
    // when Opaq Surf Ins Face Cond  < 0
    Array1D<Real64> SurfOpaqInsFaceConduction; // Opaque surface inside face heat conduction flow (W)
    // from inside of opaque surfaces, for reporting (W)
    Array1D<Real64> SurfOpaqInsFaceConductionFlux; // Opaque surface inside face heat conduction flux (W/m2)
    // from inside of opaque surfaces, for reporting (W/m2)
    Array1D<Real64> SurfOpaqInsFaceConductionEnergy; // Opaque surface inside face heat conduction flow (J)
    // from inside of opaque surfaces, for reporting (J)

    Array1D<Real64> SurfOpaqExtFaceCondGainRep; // Equals Opaq Surf Ext Face Cond
    // when Opaq Surf Ext Face Cond >= 0
    Array1D<Real64> SurfOpaqExtFaceCondLossRep; // Equals -Opaq Surf Ext Face Cond
    // when Opaq Surf Ext Face Cond  < 0
    Array1D<Real64> SurfOpaqOutsideFaceConduction; // Opaque surface outside face heat conduction flow (W)
    // from inside of opaque surfaces, for reporting (W)
    Array1D<Real64> SurfOpaqOutsideFaceConductionFlux; // Opaque surface outside face heat conduct flux (W/m2)
    // from outside of opaque surfaces, for reporting (W/m2)
    Array1D<Real64> SurfOpaqOutsideFaceConductionEnergy; // Opaque surface outside face heat conduction flow (J)
    // from inside of opaque surfaces, for reporting (J)

    Array1D<Real64> SurfOpaqAvgFaceCondGainRep; // Equals Opaq Surf average Face Cond
    // when Opaq Surf average Face Cond >= 0
    Array1D<Real64> SurfOpaqAvgFaceCondLossRep; // Equals -Opaq Surf average Face Cond
    // when Opaq Surf average Face Cond  < 0
    Array1D<Real64> SurfOpaqAvgFaceConduction; // Opaque surface average heat conduction flow (W)
    // net conduction from outside environ toward inside zone
    //  from inside of opaque surfaces, for reporting (W)
    Array1D<Real64> SurfOpaqAvgFaceConductionFlux; // Opaque surface average face heat conduction flux (W/m2)
    // net conduction from outside environ to inside zone
    //  from inside of opaque surfaces, for reporting (W/m2)
    Array1D<Real64> SurfOpaqAvgFaceConductionEnergy; // Opaque surface average heat conduction flow (J)
    // net conduction from outside environ toward inside zone
    //  from inside of opaque surfaces, for reporting (J)

    Array1D<Real64> SurfOpaqStorageGainRep; // Equals Opaque surface stored heat conduction flow
    // when Opaque surface stored heat conduction flow  >= 0
    Array1D<Real64> SurfOpaqStorageCondLossRep; // Equals -Opaque surface stored heat conduction flow
    // when Opaque surface stored heat conduction flow   < 0
    Array1D<Real64> SurfOpaqStorageConduction; // Opaque surface stored heat conduction flow (W)
    // storage of heat inside surface, positive is increasing in surf
    Array1D<Real64> SurfOpaqStorageConductionFlux; // Opaque surface stored heat conduction flux (W/m2)
    // storage of heat inside surface, positive is increasing in surf
    Array1D<Real64> SurfOpaqStorageConductionEnergy; // Opaque surface stored heat conduction flow (J)
    // storage of heat inside surface, positive is increasing in surf

    Array1D<Real64> SurfOpaqInsFaceBeamSolAbsorbed; // Opaque surface inside face absorbed beam solar,
    // for reporting (W)
    Array1D<Real64> SurfTempOut; // Temperature of the Outside Surface for each heat transfer surface
    // used for reporting purposes only.  Ref: TH(x,1,1)
    Array1D<Real64> SurfQRadSWOutMvIns; // Short wave radiation absorbed on outside of movable insulation
    // unusedREAL(r64), ALLOCATABLE, DIMENSION(:) :: QBV                 !Beam solar absorbed by interior shades in a zone, plus
    // diffuse from beam not absorbed in zone, plus
    // beam absorbed at inside surfaces

    Array1D<Real64> SurfNetLWRadToSurf;        // Net interior long wavelength radiation to a surface from other surfaces
    Array1D<Real64> SurfOpaqQRadSWLightsInAbs; // Short wave from Lights radiation absorbed on inside of opaque surface
    // Variables that are used in both the Surface Heat Balance and the Moisture Balance
    Array1D<Real64> SurfOpaqQRadSWOutAbs;  // Short wave radiation absorbed on outside of opaque surface
    Array1D<Real64> SurfOpaqQRadSWInAbs;   // Short wave radiation absorbed on inside of opaque surface
    Array1D<Real64> SurfQRadLWOutSrdSurfs; // Long wave radiation absorbed on outside of exterior surface

    Array1D<Real64> SurfQAdditionalHeatSourceOutside; // Additional heat source term on boundary conditions at outside surface
    Array1D<Real64> SurfQAdditionalHeatSourceInside;  // Additional heat source term on boundary conditions at inside surface

    Array1D<Real64> SurfOpaqInitialDifSolInAbs;  // Initial diffuse solar absorbed on inside of opaque surface [W/m2]
    Array1D<Real64> SurfWinInitialDifSolInTrans; // Initial diffuse solar transmitted out through window surface [W/m2]

    // REAL(r64) variables from BLDCTF.inc and only used in the Heat Balance
    Array3D<Real64> TH; // Temperature History (SurfNum,Hist Term,In/Out) where:
    // Hist Term (1 = Current Time, 2-MaxCTFTerms = previous times),
    // In/Out (1 = Outside, 2 = Inside)
    Array3D<Real64> QH; // Flux History (TH and QH are interpolated from THM and QHM for
    // the next user requested time step)
    Array3D<Real64> THM;        // Master Temperature History (on the time step for the construct)
    Array3D<Real64> QHM;        // Master Flux History (on the time step for the construct)
    Array2D<Real64> TsrcHist;   // Temperature history at the source location (SurfNum,Term)
    Array2D<Real64> TuserHist;  // Temperature history at the user specified location (SurfNum,Term)
    Array2D<Real64> QsrcHist;   // Heat source/sink history for the surface (SurfNum,Term)
    Array2D<Real64> TsrcHistM;  // Master temperature history at the source location (SurfNum,Term)
    Array2D<Real64> TuserHistM; // Master temperature history at the user specified location (SurfNum,Term)
    Array2D<Real64> QsrcHistM;  // Master heat source/sink history for the surface (SurfNum,Term)

    Array2D<Real64> FractDifShortZtoZ;   // Fraction of diffuse short radiation in Zone 2 transmitted to Zone 1
    Array1D_bool RecDifShortFromZ;       // True if Zone gets short radiation from another
    bool InterZoneWindow = false;        // True if there is an interzone window
    Real64 SumSurfaceHeatEmission = 0.0; // Heat emission from all surfaces

    // Surface Heat Balance
    Array1D<bool> SurfMovInsulExtPresent;       // True when interior movable insulation is present
    Array1D<bool> SurfMovInsulIntPresent;       // True when interior movable insulation is present
    Array1D<bool> SurfMovInsulIntPresentPrevTS; // True when interior movable insulation was present during the previous time step

    Array1D<Real64> SurfMovInsulHExt;  // Resistance or "h" value of exterior movable insulation
    Array1D<Real64> SurfMovInsulHInt;  // Resistance or "h" value of interior movable insulation
    Array1D<Real64> SurfAbsSolarExt;   // Solar Absorptivity of surface inside face or interior movable insulation if present
    Array1D<Real64> SurfAbsThermalExt; // Thermal Absorptivity of surface inside face or interior movable insulation if present
    Array1D<Real64> SurfAbsSolarInt;   // Solar absorptivity of surface outside face or exterior movable insulation if present
    Array1D<Real64> SurfRoughnessExt;  // Roughness of surface inside face or interior movable insulation if present
    Array1D<Real64> SurfAbsThermalInt; // Thermal absorptivity of surface outside face or exterior movable insulation if present
    std::vector<int> SurfMovInsulIndexList;
    std::vector<int> SurfMovSlatsIndexList;
    void clear_state() override
    {
        this->Zone_has_mixed_HT_models.clear();
        this->SUMH.deallocate();
        this->MaxSurfaceTempLimit = 200.0;
        this->MaxSurfaceTempLimitBeforeFatal = 500.0;
        this->MinIterations = 1;
        this->CTFConstInPart.deallocate();
        this->CTFConstOutPart.deallocate();
        this->CTFCross0.deallocate();
        this->CTFInside0.deallocate();
        this->CTFSourceIn0.deallocate();
        this->TH11Surf.deallocate();
        this->QsrcHistSurf1.deallocate();
        this->IsAdiabatic.deallocate();
        this->IsNotAdiabatic.deallocate();
        this->IsSource.deallocate();
        this->IsNotSource.deallocate();
        this->IsPoolSurf.deallocate();
        this->IsNotPoolSurf.deallocate();
        this->TempTermSurf.deallocate();
        this->TempDivSurf.deallocate();
        this->TempSurfIn.deallocate();
        this->TempInsOld.deallocate();
        this->TempSurfInTmp.deallocate();
        this->HcExtSurf.deallocate();
        this->HAirExtSurf.deallocate();
        this->HSkyExtSurf.deallocate();
        this->HGrdExtSurf.deallocate();
        this->TempSource.deallocate();
        this->TempUserLoc.deallocate();
        this->TempSurfInRep.deallocate();
        this->TempSurfInMovInsRep.deallocate();
        this->QConvInReport.deallocate();
        this->QdotConvInRep.deallocate();
        this->QdotConvInRepPerArea.deallocate();
        this->QRadNetSurfInReport.deallocate();
        this->QdotRadNetSurfInRep.deallocate();
        this->QdotRadNetSurfInRepPerArea.deallocate();
        this->QRadSolarInReport.deallocate();
        this->QdotRadSolarInRep.deallocate();
        this->QdotRadSolarInRepPerArea.deallocate();
        this->QRadLightsInReport.deallocate();
        this->QdotRadLightsInRep.deallocate();
        this->QdotRadLightsInRepPerArea.deallocate();
        this->QRadIntGainsInReport.deallocate();
        this->QdotRadIntGainsInRep.deallocate();
        this->QdotRadIntGainsInRepPerArea.deallocate();
        this->QRadHVACInReport.deallocate();
        this->QdotRadHVACInRep.deallocate();
        this->QdotRadHVACInRepPerArea.deallocate();
        this->QConvOutReport.deallocate();
        this->QdotConvOutRep.deallocate();
        this->QdotConvOutRepPerArea.deallocate();
        this->QRadOutReport.deallocate();
        this->QdotRadOutRep.deallocate();
        this->QdotRadOutRepPerArea.deallocate();
        this->SurfOpaqInsFaceCondGainRep.deallocate();
        this->SurfOpaqInsFaceCondLossRep.deallocate();
        this->SurfOpaqInsFaceConduction.deallocate();
        this->SurfOpaqInsFaceConductionFlux.deallocate();
        this->SurfOpaqInsFaceConductionEnergy.deallocate();
        this->SurfOpaqExtFaceCondGainRep.deallocate();
        this->SurfOpaqExtFaceCondLossRep.deallocate();
        this->SurfOpaqOutsideFaceConduction.deallocate();
        this->SurfOpaqOutsideFaceConductionFlux.deallocate();
        this->SurfOpaqOutsideFaceConductionEnergy.deallocate();
        this->SurfOpaqAvgFaceCondGainRep.deallocate();
        this->SurfOpaqAvgFaceCondLossRep.deallocate();
        this->SurfOpaqAvgFaceConduction.deallocate();
        this->SurfOpaqAvgFaceConductionFlux.deallocate();
        this->SurfOpaqAvgFaceConductionEnergy.deallocate();
        this->SurfOpaqStorageGainRep.deallocate();
        this->SurfOpaqStorageCondLossRep.deallocate();
        this->SurfOpaqStorageConduction.deallocate();
        this->SurfOpaqStorageConductionFlux.deallocate();
        this->SurfOpaqStorageConductionEnergy.deallocate();
        this->SurfOpaqInsFaceBeamSolAbsorbed.deallocate();
        this->SurfTempOut.deallocate();
        this->SurfQRadSWOutMvIns.deallocate();
        this->SurfNetLWRadToSurf.deallocate();
        this->SurfOpaqQRadSWLightsInAbs.deallocate();
        this->SurfOpaqQRadSWOutAbs.deallocate();
        this->SurfOpaqQRadSWInAbs.deallocate();
        this->SurfQRadLWOutSrdSurfs.deallocate();
        this->SurfQAdditionalHeatSourceOutside.deallocate();
        this->SurfQAdditionalHeatSourceInside.deallocate();
        this->SurfOpaqInitialDifSolInAbs.deallocate();
        this->SurfWinInitialDifSolInTrans.deallocate();
        this->TH.deallocate();
        this->QH.deallocate();
        this->THM.deallocate();
        this->QHM.deallocate();
        this->TsrcHist.deallocate();
        this->QsrcHist.deallocate();
        this->TsrcHistM.deallocate();
        this->QsrcHistM.deallocate();
        this->FractDifShortZtoZ.deallocate();
        this->RecDifShortFromZ.deallocate();
        this->InterZoneWindow = false;
        this->SumSurfaceHeatEmission = 0;
        this->SurfMovInsulExtPresent.deallocate();
        this->SurfMovInsulIntPresent.deallocate();
        this->SurfMovInsulIntPresentPrevTS.deallocate();
        this->SurfMovInsulHExt.deallocate();
        this->SurfMovInsulHInt.deallocate();
        this->SurfAbsSolarExt.deallocate();
        this->SurfAbsThermalExt.deallocate();
        this->SurfAbsSolarInt.deallocate();
        this->SurfAbsThermalInt.deallocate();
        this->SurfRoughnessExt.deallocate();
        this->SurfMovInsulIndexList.clear();
        this->SurfMovSlatsIndexList.clear();
    }
};

} // namespace EnergyPlus

#endif
