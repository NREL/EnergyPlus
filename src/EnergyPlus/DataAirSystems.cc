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

// EnergyPlus Headers
#include <DataAirSystems.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataAirSystems {

    // MODULE INFORMATION:
    //       AUTHOR         Plant code authors?
    //       DATE WRITTEN
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This data-only module contains the structures for various parts of the Plant and
    // Condenser Loops.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules (only modules that should be used here and sparingly)
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataPlant;

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // DERIVED TYPE DEFINITIONS

    // DefinePrimaryAirSystem contains the data for a primary air HVAC system

    // The ConnectionPoint derived type is used to link quickly between loops at connection points
    // and avoids the need for repetitive searches.

    // INTERFACE BLOCK SPECIFICATIONS
    // None

    // MODULE VARIABLE DECLARATIONS
    // For each type of air path, define an array of DefineAirPaths

    // Temporary arrays

    // Object Data
    Array1D<DefinePrimaryAirSystem> PrimaryAirSystem;
    Array1D<ConnectionPoint> DemandSideConnect;               // Connections between loops
    Array1D<ConnectZoneComp> ZoneCompToPlant;                 // Connections between loops
    Array1D<ConnectZoneSubComp> ZoneSubCompToPlant;           // Connections between loops
    Array1D<ConnectZoneSubSubComp> ZoneSubSubCompToPlant;     // Connections between loops
    Array1D<ConnectAirSysComp> AirSysCompToPlant;             // Connections between loops
    Array1D<ConnectAirSysSubComp> AirSysSubCompToPlant;       // Connections between loops
    Array1D<ConnectAirSysSubSubComp> AirSysSubSubCompToPlant; // Connections between loops

    // Functions
    void clear_state()
    {

        PrimaryAirSystem.deallocate();
        DemandSideConnect.deallocate();       // Connections between loops
        ZoneCompToPlant.deallocate();         // Connections between loops
        ZoneSubCompToPlant.deallocate();      // Connections between loops
        ZoneSubSubCompToPlant.deallocate();   // Connections between loops
        AirSysCompToPlant.deallocate();       // Connections between loops
        AirSysSubCompToPlant.deallocate();    // Connections between loops
        AirSysSubSubCompToPlant.deallocate(); // Connections
    }

} // namespace DataAirSystems

} // namespace EnergyPlus
