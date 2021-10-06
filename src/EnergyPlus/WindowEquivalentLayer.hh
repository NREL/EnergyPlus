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

#ifndef WindowEquivalentLayer_hh_INCLUDED
#define WindowEquivalentLayer_hh_INCLUDED

// C++ Headers
#include <functional>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataWindowEquivalentLayer.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace WindowEquivalentLayer {

    // Using/Aliasing
    using namespace DataWindowEquivalentLayer;

    void InitEquivalentLayerWindowCalculations(EnergyPlusData &state);

    void EQLWindowSurfaceHeatBalance(EnergyPlusData &state,
                                     int const SurfNum,       // Surface number
                                     Real64 const HcOut,      // outside convection coeficient at this timestep, W/m2K
                                     Real64 &SurfInsideTemp,  // Inside window surface temperature (innermost face) [C]
                                     Real64 &SurfOutsideTemp, // Outside surface temperature (C)
                                     Real64 &SurfOutsideEmiss,
                                     DataBSDFWindow::Condition const CalcCondition // Calucation condition (summer, winter or no condition)
    );

    Real64 P01(EnergyPlusData &state,
               Real64 const P,             // property
               std::string_view const WHAT // identifier for err msg
    );

    Real64
    HEMINT(EnergyPlusData &state,
           std::function<Real64(EnergyPlusData &state, Real64 const THETA, int const OPT, const Array1D<Real64> &)> F, // property integrand function
           int const F_Opt,           // options passed to F() (hipRHO, hipTAU)
           const Array1D<Real64> &F_P // parameters passed to F()
    );

    void ASHWAT_ThermalCalc(EnergyPlusData &state,
                            CFSTY &FS,          // fenestration system
                            Real64 const TIN,   // indoor air temperature, K
                            Real64 const TOUT,  // outdoor air temperature, K
                            Real64 const HCIN,  // indoor convective heat transfer
                            Real64 const HCOUT, // outdoor convective heat transfer
                            Real64 const TRMOUT,
                            Real64 const TRMIN,           // indoor / outdoor mean radiant temp, K
                            Array1S<Real64> const SOURCE, // absorbed solar by layer,  W/m2
                            Real64 const TOL,             // convergence tolerance, usually
                            Array1D<Real64> &QOCF,        // returned: heat flux to layer i from gaps i-1 and i
                            Real64 &QOCFRoom,             // returned: open channel heat gain to room, W/m2
                            Array1D<Real64> &T,           // returned: layer temperatures, 1=outside-most layer, K
                            Array1D<Real64> &Q,           // returned: heat flux at ith gap (betw layers i and i+1), W/m2
                            Array1D<Real64> &JF,          // returned: front (outside facing) radiosity of surfaces, W/m2
                            Array1D<Real64> &JB,          // returned: back (inside facing) radiosity, W/m2
                            Array1D<Real64> &HC           // returned: gap convective heat transfer coefficient, W/m2K
    );

    Real64 FRA(Real64 const TM, // mean gas temp, K
               Real64 const T,  // gas layer thickness, m
               Real64 const DT, // temp difference across layer, K
               Real64 const AK, // gas conductance coeffs, K = AK + BK*TM + CK*TM*TM
               Real64 const BK,
               Real64 const CK,
               Real64 const ACP, // gas specific heat coeffs, CP = ACP + BCP*TM + CCP*TM*TM
               Real64 const BCP,
               Real64 const CCP,
               Real64 const AVISC, // gas viscosity coeffs, VISC = AVISC + BVISC*TM + CVISC*TM*TM
               Real64 const BVISC,
               Real64 const CVISC,
               Real64 const RHOGAS // gas density, kg/m3
    );

    void
    TDMA(Array1D<Real64> &X, const Array1D<Real64> &AP, const Array1D<Real64> &AE, const Array1D<Real64> &AW, const Array1D<Real64> &BP, int const N);

    Real64 VB_CriticalSlatAngle(Real64 const OMEGA_DEG // incident profile angle (degrees)
    );

    float DensityCFSFillGas(CFSFILLGAS const &FG, // gas properties
                            Real64 const P,       // pressure, Pa
                            Real64 const T        // temperature, K
    );

    void CalcEQLOpticalProperty(EnergyPlusData &state,
                                int const SurfNum,
                                SolarArrays const BeamDIffFlag, // identifier index of diffuse and beam SW radiation
                                Array2A<Real64> CFSAbs          // absorbed beam solar radiation by layers fraction
    );

    Real64 EQLWindowInsideEffectiveEmiss(EnergyPlusData &state, int const ConstrNum);

    Real64 EQLWindowOutsideEffectiveEmiss(EnergyPlusData &state, int const ConstrNum);

} // namespace WindowEquivalentLayer

struct WindowEquivalentLayerData : BaseGlobalStruct
{

    // Data
    Real64 const RadiansToDeg; // Conversion for Radians to Degrees: Not using DataGlobalConstants::Pi() to avoid initialization order bug
    Real64 const PAtmSeaLevel; // Standard atmospheric pressure at sea level (Pa)
    int const hipRHO;          // return reflectance
    int const hipTAU;          // return transmittance
    Real64 const SMALL_ERROR;  // small number
                               // CFSGAP: space between layers (gap types)
    int const gtySEALED;       // sealed
    int const gtyOPENin;       // open to indoor air  (re Open Channel Flow (OCF))
    int const gtyOPENout;      // open to outdoor air (re Open Channel Flow (OCF))
                               // shade control options
    int const lscNONE;         // no control
    int const lscVBPROF;       // VB slatA = ProfA (max gain)
    int const lscVBNOBM;       // VB slatA just exclude beam
                               // Constants
    int const hipRHO_BT0;
    int const hipTAU_BT0;
    int const hipTAU_BB0;
    int const hipDIM; // dimension of parameter array

    Array3D<Real64> CFSDiffAbsTrans;
    Array1D_bool EQLDiffPropFlag;

    Real64 X1MRDiff = -1.0;
    Real64 XTAUDiff = -1.0;

    void clear_state()
    {
        this->CFSDiffAbsTrans.deallocate();
        this->EQLDiffPropFlag.deallocate();
        this->X1MRDiff = -1.0;
        this->XTAUDiff = -1.0;
    }
    // Default Constructor
    WindowEquivalentLayerData()
        : RadiansToDeg(180.0 / 3.141592653589793), PAtmSeaLevel(101325.0), hipRHO(1), hipTAU(2), SMALL_ERROR(0.000001), gtySEALED(1), gtyOPENin(2),
          gtyOPENout(3), lscNONE(0), lscVBPROF(1), lscVBNOBM(2), hipRHO_BT0(1), hipTAU_BT0(2), hipTAU_BB0(3), hipDIM(3)
    {
    }
};

} // namespace EnergyPlus

#endif
