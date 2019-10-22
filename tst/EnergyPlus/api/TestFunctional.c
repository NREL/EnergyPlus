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

#include <stdio.h>
#include <EnergyPlus/api/func.h>

int main() {
    initializeFunctionalAPI();

    Glycol glycol = NULL;
    glycol = glycolNew("WatEr");
    for (int temp=5; temp<35; temp+=10) {
        Real64 thisTemp = (float)temp;
        Real64 specificHeat = glycolSpecificHeat(glycol, thisTemp);
        Real64 density = glycolDensity(glycol, thisTemp);
        Real64 conductivity = glycolConductivity(glycol, thisTemp);
        Real64 viscosity = glycolViscosity(glycol, thisTemp);
        printf("C API Test: Calculated props at T=%4.1f: %8.4f, %8.4f, %8.4f, %8.4f \n", thisTemp, specificHeat, density, conductivity, viscosity);
    }
    glycolDelete(glycol);

    Refrigerant refrig = NULL;
    refrig = refrigerantNew("SteaM");
    Real64 temperature = 100.0;
    Real64 satPress = refrigerantSaturationPressure(refrig, temperature); // expecting about 100,000 Pa
    Real64 thisPress = 100000;
    Real64 satTemp = refrigerantSaturationTemperature(refrig, thisPress); // expecting about 100 degC
    Real64 satLiqDens = refrigerantSaturatedDensity(refrig, temperature, 0.0); // liq = 958 kg/m3
    Real64 satLiqCp = refrigerantSaturatedSpecificHeat(refrig, temperature, 0.0); // liq = 4,216 J/kgK
    Real64 satLiqEnth = refrigerantSaturatedEnthalpy(refrig, temperature, 0.0);
    Real64 satVapDens = refrigerantSaturatedDensity(refrig, temperature, 1.0); // vap = 1/1.6718 ~~ 0.59 kg/m3
    Real64 satVapCp = refrigerantSaturatedSpecificHeat(refrig, temperature, 1.0); // vap = 2,080 J/kgK
    Real64 satVapEnth = refrigerantSaturatedEnthalpy(refrig, temperature, 1.0);
    Real64 enthDifference = satVapEnth - satLiqEnth; // vap-liq = 2,675,570-419,170 ~ 2,256,400 J/kg
    temperature = 150;
//    Real64 supEnth = refrigerantSuperHeatedEnthalpy(refrig, temperature, thisPress);
//    Real64 thisEnth = 303;
//    Real64 supPress = refrigerantSuperHeatedPressure(refrig, temperature, thisEnth);
//    Real64 supDensity = refrigerantSuperHeatedDensity(refrig, temperature, thisPress);

    return 0;
}
