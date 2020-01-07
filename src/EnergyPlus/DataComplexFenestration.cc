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

// EnergyPlus Headers
#include <EnergyPlus/DataComplexFenestration.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataComplexFenestration {
    // MODULE INFORMATION:
    //       AUTHOR         Simon Vidanovic
    //       DATE WRITTEN   January 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module contains data necessary for complex fenestration calculations

    // Using/Aliasing
    using namespace DataPrecisionGlobals;

    // Data
    // Parameters for complex shade
    int const csVenetianHorizontal(1);
    int const csWoven(2);
    int const csPerforated(3);
    int const csOtherShadingType(4);
    int const csBSDF(5);
    int const csVenetianVertical(6);

    // Parameters for gas definitions
    int const GasCoeffsCustom(0);
    int const GasCoeffsAir(1);
    int const GasCoeffsArgon(2);
    int const GasCoeffsKrypton(3);
    int const GasCoeffsXenon(4);

    // Parameters for Thermal Algorithm
    // INTEGER, PARAMETER :: taTarcog = 0
    // INTEGER, PARAMETER :: taWinkelmann = 1

    // Parameters for calculation standard
    int const csISO15099(1);
    int const csEN673Declared(2);
    int const csEN673Design(3);

    // Parameters for thermal model
    int const tmISO15099(0);
    int const tmScaledCavityWidth(1);
    int const tmConvectiveScalarModel_NoSDThickness(2);
    int const tmConvectiveScalarModel_WithSDThickness(3);

    // Parameters for deflection model
    int const dmNoDeflection(0);
    int const dmTemperatureAndPressureInput(1);
    int const dmMeasuredDeflection(2);

} // namespace DataComplexFenestration

} // namespace EnergyPlus
