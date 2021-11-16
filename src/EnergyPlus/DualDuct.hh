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

#ifndef DualDuct_hh_INCLUDED
#define DualDuct_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace DualDuct {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS
    enum class DualDuctDamper
    {
        Unassigned,
        ConstantVolume,
        VariableVolume,
        OutdoorAir
    };

    enum class DualDuctOAMode
    {
        Unassigned,
        ConstantOAMode = 11,
        ScheduleOAMode = 12,
        DynamicOAMode = 13
    };

    enum class PerPersonMode
    {
        ModeNotSet = 20,
        DCVByCurrentLevel = 21,
        ByDesignLevel = 22
    };

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    struct DualDuctAirTerminalFlowConditions
    {
        // Members
        Real64 AirMassFlowRate;         // MassFlow through the Damper being Simulated [kg/Sec]
        Real64 AirMassFlowRateMaxAvail; // MassFlow through the Damper being Simulated [kg/Sec]
        Real64 AirMassFlowRateMinAvail; // MassFlow through the Damper being Simulated [kg/Sec]
        Real64 AirMassFlowRateMax;      // Max Mass Flow Rate or the Design Mass Flow Rate
        Real64 AirTemp;
        Real64 AirHumRat;
        Real64 AirEnthalpy;
        Real64 AirMassFlowRateHist1; // flow history back 1 iteration kg/s
        Real64 AirMassFlowRateHist2; // flow history back 2 iteration kg/s
        Real64 AirMassFlowRateHist3; // flow history back 3 iteration kg/s
        Real64 AirMassFlowDiffMag;   // flow difference scale, kg/s

        // Default Constructor
        DualDuctAirTerminalFlowConditions()
            : AirMassFlowRate(0.0), AirMassFlowRateMaxAvail(0.0), AirMassFlowRateMinAvail(0.0), AirMassFlowRateMax(0.0), AirTemp(0.0), AirHumRat(0.0),
              AirEnthalpy(0.0), AirMassFlowRateHist1(0.0), AirMassFlowRateHist2(0.0), AirMassFlowRateHist3(0.0), AirMassFlowDiffMag(0.0)
        {
        }
    };

    struct DualDuctAirTerminal
    {
        // Members
        std::string Name;          // Name of the Damper
        DualDuctDamper DamperType; // Type of Damper ie. VAV, Mixing, Inducing, etc.
        std::string Schedule;      // Damper Operation Schedule
        int SchedPtr;              // Pointer to the correct schedule
        Real64 MaxAirVolFlowRate;  // Max Specified Volume Flow Rate of Damper [m3/sec]
        Real64 MaxAirMassFlowRate; // Max Specified MAss Flow Rate of Damper [kg/s]
        int HotAirInletNodeNum;
        int ColdAirInletNodeNum;
        int OutletNodeNum;
        Real64 ZoneMinAirFracDes; // Fraction of supply air used as design minimum flow
        Real64 ZoneMinAirFrac;    // Fraction of supply air used as current minimum flow
        Real64 ColdAirDamperPosition;
        Real64 HotAirDamperPosition;
        int OAInletNodeNum;                  // Alternate Node for VAV:OutdoorAir for Outdoor Air
        int RecircAirInletNodeNum;           // Alternate Node for VAV:OutdoorAir for Recirc Air
        bool RecircIsUsed;                   // if true. then not using recirc duct, which is okay
        Real64 DesignOAFlowRate;             // Terminal Outdoor Air Design Flow Rate for VAV:OutdoorAir, m3/s
        Real64 DesignRecircFlowRate;         // Terminal Recirc Air Design Flow Rate for VAV:OutdoorAir, m3/s
        int OAControlMode;                   // Choice of scheduled, constant, or dynamic for VAV:OutdoorAir
        Real64 RecircAirDamperPosition;      // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
        Real64 OADamperPosition;             // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
        Real64 OAFraction;                   // Outdoor Air Fraction for VAV:OutdoorAir
        int ADUNum;                          // index of corresponding air distribution unit ZoneHVAC:AirdistributionUnit
        int CtrlZoneNum;                     // Pointer to CtrlZone data structure
        int CtrlZoneInNodeIndex;             // which controlled zone inlet node number corresponds with this unit
        int ActualZoneNum;                   // Pointer to Zone data Structure
        Real64 OutdoorAirFlowRate;           // report variable for TU outdoor air flow rate
        bool NoOAFlowInputFromUser;          // avoids OA calculation if no input specified by user
        int OARequirementsPtr;               // - Index to DesignSpecification:OutdoorAir object
        PerPersonMode OAPerPersonMode;       // mode for how per person rates are determined, DCV or design.
        Real64 OAPerPersonByDesignLevel;     // store sum of people and per person rate, constant, m3/s
        int AirLoopNum;                      // index to airloop that this terminal unit is connected to
        int ZoneTurndownMinAirFracSchPtr;    // pointer to the schedule for turndown minimum airflow fraction
        Real64 ZoneTurndownMinAirFrac;       // turndown minimum airflow fraction value, multiplier of zone design minimum air flow
        bool ZoneTurndownMinAirFracSchExist; // if true, if zone turndown min air frac schedule exist
        bool MyEnvrnFlag;                    // environment flag
        bool MySizeFlag;                     // sizing flag
        bool MyAirLoopFlag;                  // airloop flag
        DualDuctAirTerminalFlowConditions dd_airterminalHotAirInlet;
        DualDuctAirTerminalFlowConditions dd_airterminalColdAirInlet;
        DualDuctAirTerminalFlowConditions dd_airterminalOutlet;
        DualDuctAirTerminalFlowConditions dd_airterminalOAInlet;
        DualDuctAirTerminalFlowConditions dd_airterminalRecircAirInlet;

        // Default Constructor
        DualDuctAirTerminal()
            : DamperType(DualDuctDamper::Unassigned), SchedPtr(0), MaxAirVolFlowRate(0.0), MaxAirMassFlowRate(0.0), HotAirInletNodeNum(0),
              ColdAirInletNodeNum(0), OutletNodeNum(0), ZoneMinAirFracDes(0.0), ZoneMinAirFrac(0.0), ColdAirDamperPosition(0.0),
              HotAirDamperPosition(0.0), OAInletNodeNum(0), RecircAirInletNodeNum(0), RecircIsUsed(true), DesignOAFlowRate(0.0),
              DesignRecircFlowRate(0.0), OAControlMode(0), RecircAirDamperPosition(0.0), OADamperPosition(0.0), OAFraction(0.0), ADUNum(0),
              CtrlZoneNum(0), CtrlZoneInNodeIndex(0), ActualZoneNum(0), OutdoorAirFlowRate(0.0), NoOAFlowInputFromUser(true), OARequirementsPtr(0),
              OAPerPersonMode(PerPersonMode::ModeNotSet), OAPerPersonByDesignLevel(0.0), AirLoopNum(0), ZoneTurndownMinAirFracSchPtr(0),
              ZoneTurndownMinAirFrac(1.0), ZoneTurndownMinAirFracSchExist(false), MyEnvrnFlag(true), MySizeFlag(true), MyAirLoopFlag(true)
        {
        }

        void InitDualDuct(EnergyPlusData &state, bool const FirstHVACIteration);

        void SizeDualDuct(EnergyPlusData &state);

        // End Initialization Section of the Module
        //******************************************************************************

        // Begin Algorithm Section of the Module
        //******************************************************************************

        void SimDualDuctConstVol(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum);

        void SimDualDuctVarVol(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum);

        void SimDualDuctVAVOutdoorAir(EnergyPlusData &state, int const ZoneNum, int const ZoneNodeNum);

        void CalcOAMassFlow(EnergyPlusData &state,
                            Real64 &SAMassFlow,   // outside air based on optional user input
                            Real64 &AirLoopOAFrac // outside air based on optional user input
        );

        void CalcOAOnlyMassFlow(EnergyPlusData &state,
                                Real64 &OAMassFlow,               // outside air flow from user input kg/s
                                Optional<Real64> MaxOAVolFlow = _ // design level for outside air m3/s
        );

        // End Algorithm Section of the Module
        // *****************************************************************************

        // Beginning of Update subroutines for the Damper Module
        // *****************************************************************************

        void CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state);

        void UpdateDualDuct(EnergyPlusData &state);

        //        End of Update subroutines for the Damper Module
        // *****************************************************************************

        // Beginning of Reporting subroutines for the Damper Module
        // *****************************************************************************

        void ReportDualDuct(); // unused1208
    };

    // Functions

    void SimulateDualDuct(
        EnergyPlusData &state, std::string_view CompName, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum, int &CompIndex);

    // Get Input Section of the Module
    //******************************************************************************

    void GetDualDuctInput(EnergyPlusData &state);

    // End of Get Input subroutines for the Module
    //******************************************************************************

    void ReportDualDuctConnections(EnergyPlusData &state);

    void GetDualDuctOutdoorAirRecircUse(EnergyPlusData &state, std::string const &CompTypeName, std::string_view CompName, bool &RecircIsUsed);

    //        End of Reporting subroutines for the Damper Module
    // *****************************************************************************

    // Clears the global data in DualDuct.
    // Needed for unit tests, should not be normally called.

} // namespace DualDuct

struct DualDuctData : BaseGlobalStruct
{

    Array1D_bool CheckEquipName;
    int NumDDAirTerminal = 0; // The Number of Dampers found in the Input //Autodesk Poss used uninitialized in ReportDualDuctConnections
    int NumDualDuctConstVolDampers = 0;
    int NumDualDuctVarVolDampers = 0;
    int NumDualDuctVarVolOA = 0;
    Real64 MassFlowSetToler = 0.0;
    bool GetDualDuctInputFlag = true; // Flag set to make sure you get input once
    Array1D<DualDuct::DualDuctAirTerminal> dd_airterminal;
    std::unordered_map<std::string, std::string> UniqueDualDuctAirTerminalNames;
    bool InitDualDuctMyOneTimeFlag = true;
    bool ZoneEquipmentListChecked = false; // True after the Zone Equipment List has been checked for items
    bool GetDualDuctOutdoorAirRecircUseFirstTimeOnly = true;

    std::string const cCMO_DDConstantVolume = "AirTerminal:DualDuct:ConstantVolume";
    std::string const cCMO_DDVariableVolume = "AirTerminal:DualDuct:VAV";
    std::string const cCMO_DDVarVolOA = "AirTerminal:DualDuct:VAV:OutdoorAir";

    Array1D_bool RecircIsUsedARR;
    Array1D_string DamperNamesARR;

    void clear_state() override
    {
        this->CheckEquipName.clear();
        this->NumDDAirTerminal = 0;
        this->NumDualDuctConstVolDampers = 0;
        this->NumDualDuctVarVolDampers = 0;
        this->NumDualDuctVarVolOA = 0;
        this->MassFlowSetToler = 0.0;
        this->GetDualDuctInputFlag = true;
        this->dd_airterminal.clear();
        this->UniqueDualDuctAirTerminalNames.clear();
        this->InitDualDuctMyOneTimeFlag = true;
        this->ZoneEquipmentListChecked = false;
        this->GetDualDuctOutdoorAirRecircUseFirstTimeOnly = true;
        this->RecircIsUsedARR.clear();
        this->DamperNamesARR.clear();
    }
};

} // namespace EnergyPlus

#endif
