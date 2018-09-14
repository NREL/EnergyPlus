// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef DataUCSDSharedData_hh_INCLUDED
#define DataUCSDSharedData_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataUCSDSharedData {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    // The Eplus surface numbers will be stored in the arrays Apos according to the
    // type of surface. The PosZ_Wall array has dimension 2 times the Number of Zones and
    // for each zone it has 2 positions: the start and end positions in the Apos_Wall array
    // for that specific zone.
    extern Array1D_int APos_Wall;
    extern Array1D_int APos_Floor;
    extern Array1D_int APos_Ceiling;
    extern Array1D_int PosZ_Wall;
    extern Array1D_int PosZ_Floor;
    extern Array1D_int PosZ_Ceiling;
    extern Array1D_int APos_Window;
    extern Array1D_int APos_Door;
    extern Array1D_int APos_Internal;
    extern Array1D_int PosZ_Window;
    extern Array1D_int PosZ_Door;
    extern Array1D_int PosZ_Internal;
    // Convection coeficients for the various surfaces
    extern Array1D<Real64> HCeiling;
    extern Array1D<Real64> HWall;
    extern Array1D<Real64> HFloor;
    extern Array1D<Real64> HInternal;
    extern Array1D<Real64> HWindow;
    extern Array1D<Real64> HDoor;

    void clear_state();

} // namespace DataUCSDSharedData

} // namespace EnergyPlus

#endif
