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

#ifndef ThermalISO15099Calc_hh_INCLUDED
#define ThermalISO15099Calc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array2A.hh>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus::ThermalISO15099Calc {

    void film(Real64 tex, Real64 tw, Real64 ws, int iwd, Real64 &hcout, int ibc);

    void Calc_ISO15099(EnergyPlusData &state, TARCOGOutput::Files &files,
                       int nlayer,
                       int iwd,
                       Real64 &tout,
                       Real64 &tind,
                       Real64 &trmin,
                       Real64 wso,
                       Real64 wsi,
                       Real64 dir,
                       Real64 outir,
                       int isky,
                       Real64 tsky,
                       Real64 &esky,
                       Real64 fclr,
                       Real64 VacuumPressure,
                       Real64 VacuumMaxGapThickness,
                       Array1D<Real64> &gap,
                       Array1D<Real64> &thick,
                       Array1D<Real64> &scon,
                       const Array1D<Real64> &tir,
                       const Array1D<Real64> &emis,
                       Real64 totsol,
                       Real64 tilt,
                       const Array1D<Real64> &asol,
                       Real64 height,
                       Real64 heightt,
                       Real64 width,
                       const Array1D<Real64> &presure,
                       Array2A_int iprop,
                       Array2A<Real64> frct,
                       Array2A<Real64> xgcon,
                       Array2A<Real64> xgvis,
                       Array2A<Real64> xgcp,
                       const Array1D<Real64> &xwght,
                       const Array1D<Real64> &gama,
                       const Array1D_int &nmix,
                       const Array1D_int &SupportPillar,
                       const Array1D<Real64> &PillarSpacing,
                       const Array1D<Real64> &PillarRadius,
                       Array1D<Real64> &theta,
                       Array1D<Real64> &q,
                       Array1D<Real64> &qv,
                       Real64 &ufactor,
                       Real64 &sc,
                       Real64 &hflux,
                       Real64 &hcin,
                       Real64 &hcout,
                       Real64 &hrin,
                       Real64 &hrout,
                       Real64 &hin,
                       Real64 &hout,
                       Array1D<Real64> &hcgas,
                       Array1D<Real64> &hrgas,
                       Real64 &shgc,
                       int &nperr,
                       std::string &ErrorMessage,
                       Real64 &shgct,
                       Real64 &tamb,
                       Real64 &troom,
                       const Array1D_int &ibc,
                       const Array1D<Real64> &Atop,
                       const Array1D<Real64> &Abot,
                       const Array1D<Real64> &Al,
                       const Array1D<Real64> &Ar,
                       const Array1D<Real64> &Ah,
                       const Array1D<Real64> &SlatThick,
                       const Array1D<Real64> &SlatWidth,
                       const Array1D<Real64> &SlatAngle,
                       const Array1D<Real64> &SlatCond,
                       const Array1D<Real64> &SlatSpacing,
                       const Array1D<Real64> &SlatCurve,
                       const Array1D<Real64> &vvent,
                       const Array1D<Real64> &tvent,
                       const Array1D_int &LayerType,
                       const Array1D_int &nslice,
                       const Array1D<Real64> &LaminateA,
                       const Array1D<Real64> &LaminateB,
                       const Array1D<Real64> &sumsol,
                       Array1D<Real64> &Ra,
                       Array1D<Real64> &Nu,
                       int ThermalMod,
                       int Debug_mode,
                       Real64 &ShadeEmisRatioOut,
                       Real64 &ShadeEmisRatioIn,
                       Real64 &ShadeHcRatioOut,
                       Real64 &ShadeHcRatioIn,
                       Real64 &HcUnshadedOut,
                       Real64 &HcUnshadedIn,
                       Array1D<Real64> &Keff,
                       Array1D<Real64> &ShadeGapKeffConv,
                       Real64 SDScalar,
                       int SHGCCalc,
                       int &NumOfIterations,
                       Real64 egdeGlCorrFac);

    void therm1d(TARCOGOutput::Files &files,
                 int nlayer,
                 int iwd,
                 Real64 &tout,
                 Real64 &tind,
                 Real64 wso,
                 Real64 wsi,
                 Real64 VacuumPressure,
                 Real64 VacuumMaxGapThickness,
                 Real64 dir,
                 Real64 &ebsky,
                 Real64 Gout,
                 Real64 trmout,
                 Real64 trmin,
                 Real64 &ebroom,
                 Real64 Gin,
                 const Array1D<Real64> &tir,
                 const Array1D<Real64> &rir,
                 const Array1D<Real64> &emis,
                 const Array1D<Real64> &gap,
                 const Array1D<Real64> &thick,
                 const Array1D<Real64> &scon,
                 Real64 tilt,
                 const Array1D<Real64> &asol,
                 Real64 height,
                 Real64 heightt,
                 Real64 width,
                 Array2_int const &iprop,
                 Array2<Real64> const &frct,
                 const Array1D<Real64> &presure,
                 const Array1D_int &nmix,
                 const Array1D<Real64> &wght,
                 Array2<Real64> const &gcon,
                 Array2<Real64> const &gvis,
                 Array2<Real64> const &gcp,
                 const Array1D<Real64> &gama,
                 const Array1D_int &SupportPillar,
                 const Array1D<Real64> &PillarSpacing,
                 const Array1D<Real64> &PillarRadius,
                 Array1D<Real64> &theta,
                 Array1D<Real64> &q,
                 Array1D<Real64> &qv,
                 Real64 &flux,
                 Real64 &hcin,
                 Real64 &hrin,
                 Real64 &hcout,
                 Real64 &hrout,
                 Real64 &hin,
                 Real64 &hout,
                 Array1D<Real64> &hcgas,
                 Array1D<Real64> &hrgas,
                 Real64 &ufactor,
                 int &nperr,
                 std::string &ErrorMessage,
                 Real64 &tamb,
                 Real64 &troom,
                 const Array1D_int &ibc,
                 const Array1D<Real64> &Atop,
                 const Array1D<Real64> &Abot,
                 const Array1D<Real64> &Al,
                 const Array1D<Real64> &Ar,
                 const Array1D<Real64> &Ah,
                 const Array1D<Real64> &EffectiveOpenness,
                 const Array1D<Real64> &vvent,
                 const Array1D<Real64> &tvent,
                 const Array1D_int &LayerType,
                 Array1D<Real64> &Ra,
                 Array1D<Real64> &Nu,
                 Array1D<Real64> &vfreevent,
                 Array1D<Real64> &qcgas,
                 Array1D<Real64> &qrgas,
                 Array1D<Real64> &Ebf,
                 Array1D<Real64> &Ebb,
                 Array1D<Real64> &Rf,
                 Array1D<Real64> &Rb,
                 Real64 &ShadeEmisRatioOut,
                 Real64 &ShadeEmisRatioIn,
                 Real64 &ShadeHcModifiedOut,
                 Real64 &ShadeHcModifiedIn,
                 int ThermalMod,
                 int Debug_mode,
                 Real64 &AchievedErrorTolerance,
                 int &TotalIndex,
                 Real64 edgeGlCorrFac);

    void guess(Real64 tout,
               Real64 tind,
               int nlayer,
               const Array1D<Real64> &gap,
               const Array1D<Real64> &thick,
               Real64 &width,
               Array1D<Real64> &theta,
               Array1D<Real64> &Ebb,
               Array1D<Real64> &Ebf,
               Array1D<Real64> &Tgap);

    void solarISO15099(Real64 totsol, Real64 rtot, const Array1D<Real64> &rs, int nlayer, const Array1D<Real64> &absol, Real64 &sf);

    void resist(int nlayer,
                Real64 trmout,
                Real64 Tout,
                Real64 trmin,
                Real64 tind,
                const Array1D<Real64> &hcgas,
                const Array1D<Real64> &hrgas,
                Array1D<Real64> &Theta,
                Array1D<Real64> &qlayer,
                const Array1D<Real64> &qv,
                const Array1D_int &LayerType,
                const Array1D<Real64> &thick,
                const Array1D<Real64> &scon,
                Real64 &ufactor,
                Real64 &flux,
                Array1D<Real64> &qcgas,
                Array1D<Real64> &qrgas);

    void hatter(int nlayer,
                int iwd,
                Real64 tout,
                Real64 tind,
                Real64 wso,
                Real64 wsi,
                Real64 VacuumPressure,
                Real64 VacuumMaxGapThickness,
                Real64 &ebsky,
                Real64 &tamb,
                Real64 &ebroom,
                Real64 &troom,
                const Array1D<Real64> &gap,
                Real64 height,
                Real64 heightt,
                const Array1D<Real64> &scon,
                Real64 tilt,
                Array1D<Real64> &theta,
                const Array1D<Real64> &Tgap,
                Array1D<Real64> &Radiation,
                Real64 trmout,
                Real64 trmin,
                Array2_int const &iprop,
                Array2<Real64> const &frct,
                const Array1D<Real64> &presure,
                const Array1D_int &nmix,
                const Array1D<Real64> &wght,
                Array2<Real64> const &gcon,
                Array2<Real64> const &gvis,
                Array2<Real64> const &gcp,
                const Array1D<Real64> &gama,
                const Array1D_int &SupportPillar,
                const Array1D<Real64> &PillarSpacing,
                const Array1D<Real64> &PillarRadius,
                Array1D<Real64> &hgas,
                Array1D<Real64> &hcgas,
                Array1D<Real64> &hrgas,
                Real64 &hcin,
                Real64 &hcout,
                Real64 hin,
                Real64 hout,
                int index,
                const Array1D_int &ibc,
                int &nperr,
                std::string &ErrorMessage,
                Real64 &hrin,
                Real64 &hrout,
                Array1D<Real64> &Ra,
                Array1D<Real64> &Nu);

    void effectiveLayerCond(int nlayer,
                            const Array1D_int &LayerType,             // Layer type
                            const Array1D<Real64> &scon,              // Layer thermal conductivity
                            const Array1D<Real64> &thick,             // Layer thickness
                            Array2A_int iprop,                 // Gas type in gaps
                            Array2A<Real64> frct,              // Fraction of gas
                            const Array1D_int &nmix,                  // Gas mixture
                            const Array1D<Real64> &pressure,          // Gas pressure [Pa]
                            const Array1D<Real64> &wght,              // Molecular weight
                            Array2A<Real64> gcon,              // Gas specific conductivity
                            Array2A<Real64> gvis,              // Gas specific viscosity
                            Array2A<Real64> gcp,               // Gas specific heat
                            const Array1D<Real64> &EffectiveOpenness, // Layer effective openneess [m2]
                            Array1D<Real64> &theta,                   // Layer surface tempeartures [K]
                            Array1D<Real64> &sconScaled,             // Layer conductivity divided by thickness
                            int &nperr,                              // Error message flag
                            std::string &ErrorMessage                // Error message
    );

    void filmi(Real64 tair,
               Real64 t,
               int nlayer,
               Real64 tilt,
               Real64 wsi,
               Real64 height,
               Array2A_int iprop,
               Array2A<Real64> frct,
               const Array1D<Real64> &presure,
               const Array1D_int &nmix,
               const Array1D<Real64> &wght,
               Array2A<Real64> gcon,
               Array2A<Real64> gvis,
               Array2A<Real64> gcp,
               Real64 &hcin,
               int ibc,
               int &nperr,
               std::string &ErrorMessage);

    void filmg(Real64 tilt,
               const Array1D<Real64> &theta,
               const Array1D<Real64> &Tgap,
               int nlayer,
               Real64 height,
               const Array1D<Real64> &gap,
               Array2A_int iprop,
               Array2A<Real64> frct,
               Real64 VacuumPressure,
               const Array1D<Real64> &presure,
               const Array1D_int &nmix,
               const Array1D<Real64> &wght,
               Array2A<Real64> gcon,
               Array2A<Real64> gvis,
               Array2A<Real64> gcp,
               const Array1D<Real64> &gama,
               Array1D<Real64> &hcgas,
               Array1D<Real64> &Rayleigh,
               Array1D<Real64> &Nu,
               int &nperr,
               std::string &ErrorMessage);

    void filmPillar(const Array1D_int &SupportPillar,     // Shows whether or not gap have support pillar
                    const Array1D<Real64> &scon,          // Conductivity of glass layers
                    const Array1D<Real64> &PillarSpacing, // Pillar spacing for each gap (used in case there is support pillar)
                    const Array1D<Real64> &PillarRadius,  // Pillar radius for each gap (used in case there is support pillar)
                    int nlayer,
                    const Array1D<Real64> &gap,
                    Array1D<Real64> &hcgas,
                    Real64 VacuumMaxGapThickness,
                    int &nperr,
                    std::string &ErrorMessage);

    void nusselt(Real64 tilt, Real64 ra, Real64 asp, Real64 &gnu, int &nperr, std::string &ErrorMessage);

    void storeIterationResults(TARCOGOutput::Files &files,
                               int nlayer,
                               int index,
                               const Array1D<Real64> &theta,
                               Real64 trmout,
                               Real64 tamb,
                               Real64 trmin,
                               Real64 troom,
                               Real64 ebsky,
                               Real64 ebroom,
                               Real64 hcin,
                               Real64 hcout,
                               Real64 hrin,
                               Real64 hrout,
                               Real64 hin,
                               Real64 hout,
                               const Array1D<Real64> &Ebb,
                               const Array1D<Real64> &Ebf,
                               const Array1D<Real64> &Rb,
                               const Array1D<Real64> &Rf,
                               int &);

    void CalculateFuncResults(int nlayer, Array2<Real64> const &a, const Array1D<Real64> &b, const Array1D<Real64> &x, Array1D<Real64> &FRes);

} // namespace EnergyPlus

#endif
