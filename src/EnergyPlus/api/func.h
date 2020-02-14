// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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


#ifndef EnergyPlusAPIFunctional_h_INCLUDED
#define EnergyPlusAPIFunctional_h_INCLUDED

#include <EnergyPlus/TypeDefs.h>
#include <EnergyPlus/api/EnergyPlusAPI.hh>

#ifdef __cplusplus
extern "C" {
#endif

void functionalNoOp();


ENERGYPLUSLIB_API void initializeFunctionalAPI();
ENERGYPLUSLIB_API const char * apiVersionFromEPlus();
ENERGYPLUSLIB_API void registerErrorCallback(void (*f)(const char * errorMessage));

// Glycol("WATER")
ENERGYPLUSLIB_API typedef void * Glycol;
ENERGYPLUSLIB_API Glycol glycolNew(const char* glycolName);
ENERGYPLUSLIB_API void glycolDelete(Glycol);
ENERGYPLUSLIB_API Real64 glycolSpecificHeat(Glycol, Real64 temperature);
ENERGYPLUSLIB_API Real64 glycolDensity(Glycol, Real64 temperature);
ENERGYPLUSLIB_API Real64 glycolConductivity(Glycol, Real64 temperature);
ENERGYPLUSLIB_API Real64 glycolViscosity(Glycol, Real64 temperature);

//Real64 Refrig("STEAM")
ENERGYPLUSLIB_API typedef void * Refrigerant;
ENERGYPLUSLIB_API Refrigerant refrigerantNew(const char* refrigerantName);
ENERGYPLUSLIB_API void refrigerantDelete(Refrigerant);
ENERGYPLUSLIB_API Real64 refrigerantSaturationPressure(Refrigerant, Real64 temperature);
ENERGYPLUSLIB_API Real64 refrigerantSaturationTemperature(Refrigerant, Real64 pressure);
ENERGYPLUSLIB_API Real64 refrigerantSaturatedEnthalpy(Refrigerant, Real64 temperature, Real64 quality);
ENERGYPLUSLIB_API Real64 refrigerantSaturatedDensity(Refrigerant, Real64 temperature, Real64 quality);
ENERGYPLUSLIB_API Real64 refrigerantSaturatedSpecificHeat(Refrigerant, Real64 temperature, Real64 quality);
//ENERGYPLUSLIB_API Real64 refrigerantSuperHeatedEnthalpy(Refrigerant, Real64 temperature, Real64 pressure);
//ENERGYPLUSLIB_API Real64 refrigerantSuperHeatedPressure(Refrigerant, Real64 temperature, Real64 enthalpy);
//ENERGYPLUSLIB_API Real64 refrigerantSuperHeatedDensity(Refrigerant, Real64 temperature, Real64 pressure);

//Real64 Psychrometric(props...)
ENERGYPLUSLIB_API Real64 psyRhoFnPbTdbW(Real64 pb, Real64 tdb, Real64 dw);                                     
ENERGYPLUSLIB_API Real64 psyHfgAirFnWTdb(Real64 T);
ENERGYPLUSLIB_API Real64 psyHgAirFnWTdb(Real64 T);
ENERGYPLUSLIB_API Real64 psyHFnTdbW(Real64 TDB, Real64 dW);
ENERGYPLUSLIB_API Real64 psyCpAirFnW(Real64 dw);
ENERGYPLUSLIB_API Real64 psyTdbFnHW(Real64 H, Real64 dW);
ENERGYPLUSLIB_API Real64 psyRhovFnTdbWPb(Real64 Tdb, Real64 dW, Real64 PB);
ENERGYPLUSLIB_API Real64 psyTwbFnTdbWPb(Real64 Tdb, Real64 W, Real64 Pb);
ENERGYPLUSLIB_API Real64 psyVFnTdbWPb(Real64 TDB, Real64 dW, Real64 PB);
ENERGYPLUSLIB_API Real64 psyWFnTdbH(Real64 TDB, Real64 H);
ENERGYPLUSLIB_API Real64 psyPsatFnTemp(Real64 T);
ENERGYPLUSLIB_API Real64 psyTsatFnHPb(Real64 H, Real64 Pb);
ENERGYPLUSLIB_API Real64 psyRhovFnTdbRh(Real64 Tdb, Real64 RH);
ENERGYPLUSLIB_API Real64 psyRhFnTdbRhov(Real64 Tdb, Real64 Rhovapor);
ENERGYPLUSLIB_API Real64 psyRhFnTdbWPb(Real64 TDB, Real64 dW, Real64 PB);
ENERGYPLUSLIB_API Real64 psyWFnTdpPb(Real64 TDP, Real64 PB);
ENERGYPLUSLIB_API Real64 psyWFnTdbRhPb(Real64 TDB, Real64 RH, Real64 PB);
ENERGYPLUSLIB_API Real64 psyWFnTdbTwbPb(Real64 TDB, Real64 TWBin, Real64 PB);
ENERGYPLUSLIB_API Real64 psyHFnTdbRhPb(Real64 TDB, Real64 RH, Real64 PB);
ENERGYPLUSLIB_API Real64 psyTdpFnWPb(Real64 W, Real64 PB);
ENERGYPLUSLIB_API Real64 psyTdpFnTdbTwbPb(Real64 TDB, Real64 TWB, Real64 PB);

#ifdef __cplusplus
}
#endif


#endif //EnergyPlusAPIFunctional_h_INCLUDED
