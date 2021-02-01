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

// EnergyPlus Headers
#include <EnergyPlus/DataRoomAirModel.hh>

namespace EnergyPlus {

namespace DataRoomAirModel {

    // MODULE INFORMATION:
    //       AUTHOR         Weixiu Kong
    //       DATE WRITTEN   March 2003
    //       MODIFIED       July 2003, CC
    //                      Jan 2004, CC
    //                      Aug 2005, BG -- added structures for user-defined patterns
    //                      June 2008, BG -- revised for system time step history terms
    //                      Aug 2013, Sam Brunswick -- added structures for improved RoomAirModelCrossVent
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // This module contain global variables needed in air models

    // USE STATEMENTS:                       ! UCSD
    // Using/Aliasing
    // Data
    // module should be available to other modules and routines.  Thus,
    // all variables in this module must be PUBLIC.

    // Parameters to indicate type of air node, which is dependent on air models
    int const InletAirNode(0);               // air node at inlet (for Mundt and Rees&Haves Models)
    int const FloorAirNode(1);               // air node at floor (for Mundt and Rees&Haves Models)
    int const ControlAirNode(2);             // air node at control point (for Mundt Model)
    int const CeilingAirNode(3);             // air node at ceiling (for Mundt Model)
    int const MundtRoomAirNode(4);           // air node for vertical walls (for Mundt Model)
    int const ReturnAirNode(10);             // air node for return (for Mundt and Rees&Haves Models)
    int const AirflowNetworkRoomAirNode(11); // air node for airflow network based room air model
    int const PlumeAirNode1(2);              // air node for plume load (for Rees&Haves Model)
    int const PlumeAirNode2(3);              // air node for plume load (for Rees&Haves Model)
    int const PlumeAirNode3(4);              // air node for plume load (for Rees&Haves Model)
    int const PlumeAirNode4(5);              // air node for plume load (for Rees&Haves Model)
    int const RoomAirNode1(6);               // air node for vertical walls (for Rees&Haves Model)
    int const RoomAirNode2(7);               // air node for vertical walls (for Rees&Haves Model)
    int const RoomAirNode3(8);               // air node for vertical walls (for Rees&Haves Model)
    int const RoomAirNode4(9);               // air node for vertical walls (for Rees&Haves Model)

} // namespace DataRoomAirModel

} // namespace EnergyPlus
