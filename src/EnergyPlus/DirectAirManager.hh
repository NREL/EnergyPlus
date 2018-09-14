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

#ifndef DirectAirManager_hh_INCLUDED
#define DirectAirManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DirectAirManager {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:

    // Type declarations in DirectAir module

    // MODULE VARIABLE DECLARATIONS:
    extern int NumDirectAir;
    extern Array1D_bool CheckEquipName;
    // SUBROUTINE SPECIFICATIONS FOR MODULE AirLoopSplitter

    // Types

    struct DirectAirProps
    {
        // Members
        // Input Data
        std::string cObjectName;
        std::string EquipID;
        std::string Schedule;
        int ZoneSupplyAirNode;
        int SchedPtr;
        Real64 MaxAirVolFlowRate;    // Max Specified Volume Flow Rate of Sys [m3/sec]
        Real64 AirMassFlowRateMax;   // Max mass flow [kg/sec]
        Real64 InitMaxAvailMassFlow; // The Initial max mass Flow to set the Control Flow Fraction
        Real64 AirMassFlowFraction;
        int ZoneEquipAirInletNode;
        int AirTerminalSizingSpecIndex; // Pointer to DesignSpecification:AirTerminal:Sizing obect
        int TermUnitSizingNum;          // index to TermUnitSizing and TermUnitFinalZoneSizing for this terminal unit
        // Simulation Data
        Real64 SensOutputProvided;
        bool EMSOverrideAirFlow;     // if true, EMS is calling to override flow rate
        Real64 EMSMassFlowRateValue; // value EMS is directing to use for flow rate [kg/s]
        // Reporting Variables
        Real64 HeatRate;
        Real64 CoolRate;
        Real64 HeatEnergy;
        Real64 CoolEnergy;
        // pointers
        int ZoneEqNum;
        int ZoneNum;

        bool NoOAFlowInputFromUser; // avoids OA calculation if no input specified by user
        int OARequirementsPtr;      // - Index to DesignSpecification:OutdoorAir object
        int CtrlZoneInNodeIndex;    // which controlled zone inlet node number corresponds with this unit
        int AirLoopNum;             // air loop index
        int OAPerPersonMode;        // mode for how per person rates are determined, DCV or design.

        // Default Constructor
        DirectAirProps()
            : ZoneSupplyAirNode(0), SchedPtr(0), MaxAirVolFlowRate(0.0), AirMassFlowRateMax(0.0), InitMaxAvailMassFlow(0.0), AirMassFlowFraction(0.0),
              ZoneEquipAirInletNode(0), AirTerminalSizingSpecIndex(0), SensOutputProvided(0.0), EMSOverrideAirFlow(false), EMSMassFlowRateValue(0.0),
              HeatRate(0.0), CoolRate(0.0), HeatEnergy(0.0), CoolEnergy(0.0), ZoneEqNum(0), ZoneNum(0), NoOAFlowInputFromUser(true),
              OARequirementsPtr(0), CtrlZoneInNodeIndex(0), AirLoopNum(0), OAPerPersonMode(0)
        {
        }
    };

    // Object Data
    extern Array1D<DirectAirProps> DirectAir;

    // Functions

    void clear_state();

    void SimDirectAir(std::string const &EquipName,
                      int const ControlledZoneNum,
                      bool const FirstHVACIteration,
                      Real64 &SensOutputProvided,
                      Real64 &LatOutputProvided, // Latent output provided (kg/s), dehumidification = negative
                      int &CompIndex);

    void GetDirectAirInput();

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitDirectAir(int const DirectAirNum, int const ControlledZoneNum, bool const FirstHVACIteration);

    void SizeDirectAir(int const DirectAirNum);

    // End Initialization Section of the Module
    //******************************************************************************

    void CalcDirectAir(int const DirectAirNum,
                       int const ControlledZoneNum,
                       Real64 &SensOutputProvided,
                       Real64 &LatOutputProvided // Latent output provided, kg/s, dehumidification = negative
    );

} // namespace DirectAirManager

} // namespace EnergyPlus

#endif
