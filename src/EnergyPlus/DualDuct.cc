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

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DualDuct {
    // Module containing the DualDuct simulation routines

    // MODULE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   February 2000
    //       MODIFIED       Clayton Miller, Brent Griffith Aug. 2010 - Added DualDuctOA Terminal Unit to Simulate Decoupled OA/RA
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // manage the DualDuct Systems Simulation

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using DataEnvironment::StdRhoAir;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::NumOfZones;
    using DataGlobals::ScheduleAlwaysOn;
    using DataGlobals::SysSizingCalc;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallMassFlow;
    using namespace DataSizing;
    using namespace ScheduleManager;

    // MODULE PARAMETER DEFINITIONS
    int const DualDuct_ConstantVolume(1);
    int const DualDuct_VariableVolume(2);
    int const DualDuct_OutdoorAir(3);
    std::string const cCMO_DDConstantVolume("AirTerminal:DualDuct:ConstantVolume");
    std::string const cCMO_DDVariableVolume("AirTerminal:DualDuct:VAV");
    std::string const cCMO_DDVarVolOA("AirTerminal:DualDuct:VAV:OutdoorAir");

    int const DD_OA_ConstantOAMode(11);
    int const DD_OA_ScheduleOAMode(12);
    int const DD_OA_DynamicOAMode(13);

    int const PerPersonModeNotSet(20);
    int const PerPersonDCVByCurrentLevel(21);
    int const PerPersonByDesignLevel(22);

    static std::string const BlankString;

    // MODULE VARIABLE DECLARATIONS:
    Array1D_bool CheckEquipName;

    int NumDDAirTerminal(0); // The Number of Dampers found in the Input //Autodesk Poss used uninitialized in ReportDualDuctConnections
    int NumDualDuctConstVolDampers;
    int NumDualDuctVarVolDampers;
    int NumDualDuctVarVolOA;
    Real64 MassFlowSetToler;
    bool GetDualDuctInputFlag(true); // Flag set to make sure you get input once

    // Object Data
    Array1D<DualDuctAirTerminal> dd_airterminal;
    std::unordered_map<std::string, std::string> UniqueDualDuctAirTerminalNames;
    bool InitDualDuctMyOneTimeFlag(true);
    bool ZoneEquipmentListChecked(false); // True after the Zone Equipment List has been checked for items
    bool GetDualDuctOutdoorAirRecircUseFirstTimeOnly(true);

    void clear_state() {
        CheckEquipName.clear();
        NumDDAirTerminal = 0;
        NumDualDuctConstVolDampers = 0;
        NumDualDuctVarVolDampers = 0;
        NumDualDuctVarVolOA = 0;
        MassFlowSetToler = 0.0;
        GetDualDuctInputFlag = true;
        dd_airterminal.clear();
        UniqueDualDuctAirTerminalNames.clear();
        InitDualDuctMyOneTimeFlag = true;
        ZoneEquipmentListChecked = false;
        GetDualDuctOutdoorAirRecircUseFirstTimeOnly = true;
    }

    void SimulateDualDuct(std::string const &CompName, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum, int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   February 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Damper component simulation.
        // It is called from the SimAirLoopComponent
        // at the system time step.

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int DDNum; // The Damper that you are currently loading input into

        // FLOW:

        // Obtains and Allocates Damper related parameters from input file
        if (GetDualDuctInputFlag) { // First time subroutine has been entered
            GetDualDuctInput();
            GetDualDuctInputFlag = false;
        }

        // Find the correct DDNumber with the AirLoop & CompNum from AirLoop Derived Type
        if (CompIndex == 0) {
            DDNum = UtilityRoutines::FindItemInList(CompName, dd_airterminal, &DualDuctAirTerminal::Name);
            if (DDNum == 0) {
                ShowFatalError("SimulateDualDuct: Damper not found=" + CompName);
            }
            CompIndex = DDNum;
        } else {
            DDNum = CompIndex;
            if (DDNum > NumDDAirTerminal || DDNum < 1) {
                ShowFatalError("SimulateDualDuct: Invalid CompIndex passed=" + TrimSigDigits(CompIndex) +
                               ", Number of Dampers=" + TrimSigDigits(NumDDAirTerminal) + ", Damper name=" + CompName);
            }
            if (CheckEquipName(DDNum)) {
                if (CompName != dd_airterminal(DDNum).Name) {
                    ShowFatalError("SimulateDualDuct: Invalid CompIndex passed=" + TrimSigDigits(CompIndex) + ", Damper name=" + CompName +
                                   ", stored Damper Name for that index=" + dd_airterminal(DDNum).Name);
                }
                CheckEquipName(DDNum) = false;
            }
        }

        auto &thisDualDuct(dd_airterminal(DDNum));

        if (CompIndex > 0) {
            DataSizing::CurTermUnitSizingNum = DataDefineEquip::AirDistUnit(thisDualDuct.ADUNum).TermUnitSizingNum;
            // With the correct DDNum Initialize
            thisDualDuct.InitDualDuct(FirstHVACIteration); // Initialize all Damper related parameters

            // Calculate the Correct Damper Model with the current DDNum
            {
                auto const SELECT_CASE_var(thisDualDuct.DamperType);

                if (SELECT_CASE_var == DualDuct_ConstantVolume) { // 'AirTerminal:DualDuct:ConstantVolume'

                    thisDualDuct.SimDualDuctConstVol(ZoneNum, ZoneNodeNum);

                } else if (SELECT_CASE_var == DualDuct_VariableVolume) { // 'AirTerminal:DualDuct:VAV'

                    thisDualDuct.SimDualDuctVarVol(ZoneNum, ZoneNodeNum);

                } else if (SELECT_CASE_var == DualDuct_OutdoorAir) {

                    thisDualDuct.SimDualDuctVAVOutdoorAir(ZoneNum, ZoneNodeNum); // 'AirTerminal:DualDuct:VAV:OutdoorAir'
                }
            }

            // Update the current Damper to the outlet nodes
            thisDualDuct.UpdateDualDuct();

            // Report the current Damper
            thisDualDuct.ReportDualDuct();
        } else {
            ShowFatalError("SimulateDualDuct: Damper not found=" + CompName);
        }
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetDualDuctInput()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   April 1998
        //       MODIFIED       Julien Marrec of EffiBEM, 2017-12-18
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main routine to call other input routines and Get routines

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using DataDefineEquip::AirDistUnit;
        using DataDefineEquip::NumAirDistUnits;
        using DataZoneEquipment::ZoneEquipConfig;
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataIPShortCuts;
        using namespace DataHeatBalance;
        using General::RoundSigDigits;
        using ReportSizingManager::ReportSizingOutput;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetDualDuctInput: "); // include trailing bla

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int DDNum;   // The Damper that you are currently loading input into
        int DamperIndex; // Loop index to Damper that you are currently loading input into
        int NumAlphas;
        int NumNums;
        int IOStat;
        static Array1D<Real64> NumArray(2, 0.0);
        static Array1D_string AlphArray(7);
        static Array1D_string cAlphaFields(7);       // Alpha field names
        static Array1D_string cNumericFields(2);     // Numeric field names
        static Array1D_bool lAlphaBlanks(7, true);   // Logical array, alpha field input BLANK = .TRUE.
        static Array1D_bool lNumericBlanks(2, true); // Logical array, numeric field input BLANK = .TRUE.
        std::string CurrentModuleObject;             // for ease in getting objects
        static bool ErrorsFound(false);              // If errors detected in input
        int CtrlZone;                                // controlled zone do loop index
        int SupAirIn;                                // controlled zone supply air inlet index
        int ADUNum;                                  // loop control to search Air Distribution Units
        static Real64 DummyOAFlow(0.0);

        // Flow
        NumDualDuctConstVolDampers = inputProcessor->getNumObjectsFound(cCMO_DDConstantVolume);
        NumDualDuctVarVolDampers = inputProcessor->getNumObjectsFound(cCMO_DDVariableVolume);
        NumDualDuctVarVolOA = inputProcessor->getNumObjectsFound(cCMO_DDVarVolOA);
        NumDDAirTerminal = NumDualDuctConstVolDampers + NumDualDuctVarVolDampers + NumDualDuctVarVolOA;
        dd_airterminal.allocate(NumDDAirTerminal);
        UniqueDualDuctAirTerminalNames.reserve(NumDDAirTerminal);
        CheckEquipName.dimension(NumDDAirTerminal, true);

        if (NumDualDuctConstVolDampers > 0) {
            for (DamperIndex = 1; DamperIndex <= NumDualDuctConstVolDampers; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = cCMO_DDConstantVolume;

                inputProcessor->getObjectItem(CurrentModuleObject,
                                              DamperIndex,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex;
                GlobalNames::VerifyUniqueInterObjectName(
                    UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                dd_airterminal(DDNum).Name = AlphArray(1);
                dd_airterminal(DDNum).DamperType = DualDuct_ConstantVolume;
                dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                    dd_airterminal(DDNum).SchedPtr = ScheduleAlwaysOn;
                } else {
                    dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(AlphArray(2));
                    if (dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(CurrentModuleObject + ", \"" + dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) + " = " + AlphArray(2) +
                                        " not found.");
                        ErrorsFound = true;
                    }
                }
                dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(AlphArray(3),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject,
                                                                        AlphArray(1),
                                                                        NodeType_Air,
                                                                        NodeConnectionType_Outlet,
                                                                        1,
                                                                        ObjectIsNotParent,
                                                                        cAlphaFields(3));
                dd_airterminal(DDNum).HotAirInletNodeNum = GetOnlySingleNode(AlphArray(4),
                                                                             ErrorsFound,
                                                                             CurrentModuleObject,
                                                                             AlphArray(1),
                                                                             NodeType_Air,
                                                                             NodeConnectionType_Inlet,
                                                                             1,
                                                                             ObjectIsNotParent,
                                                                             cAlphaFields(4));
                dd_airterminal(DDNum).ColdAirInletNodeNum = GetOnlySingleNode(AlphArray(5),
                                                                              ErrorsFound,
                                                                              CurrentModuleObject,
                                                                              AlphArray(1),
                                                                              NodeType_Air,
                                                                              NodeConnectionType_Inlet,
                                                                              1,
                                                                              ObjectIsNotParent,
                                                                              cAlphaFields(5));

                dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                dd_airterminal(DDNum).ZoneMinAirFracDes = 0.0;

                // Register component set data - one for heat and one for cool
                TestCompSet(CurrentModuleObject + ":HEAT", dd_airterminal(DDNum).Name, AlphArray(4), AlphArray(3), "Air Nodes");
                TestCompSet(CurrentModuleObject + ":COOL", dd_airterminal(DDNum).Name, AlphArray(5), AlphArray(3), "Air Nodes");

                for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                    if (dd_airterminal(DDNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                        AirDistUnit(ADUNum).InletNodeNum = dd_airterminal(DDNum).ColdAirInletNodeNum;
                        AirDistUnit(ADUNum).InletNodeNum2 = dd_airterminal(DDNum).HotAirInletNodeNum;
                        dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if (dd_airterminal(DDNum).DamperType == DualDuct_ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if (dd_airterminal(DDNum).DamperType == DualDuct_VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if (dd_airterminal(DDNum).DamperType == DualDuct_OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(RoutineName + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" + CurrentModuleObject + ',' +
                                    dd_airterminal(DDNum).Name + "].");
                    ShowContinueError("...should have outlet node=" + NodeID(dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (dd_airterminal(DDNum).OutletNodeNum == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                    ShowSevereError("Error in connecting a terminal unit to a zone");
                                    ShowContinueError(NodeID(dd_airterminal(DDNum).OutletNodeNum) + " already connects to another zone");
                                    ShowContinueError("Occurs for terminal unit " + CurrentModuleObject + " = " + dd_airterminal(DDNum).Name);
                                    ShowContinueError("Check terminal unit node names for errors");
                                    ErrorsFound = true;
                                } else {
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = dd_airterminal(DDNum).ColdAirInletNodeNum;
                                    ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode = dd_airterminal(DDNum).HotAirInletNodeNum;
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = dd_airterminal(DDNum).OutletNodeNum;
                                    ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode = dd_airterminal(DDNum).OutletNodeNum;
                                    AirDistUnit(dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                        ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                    AirDistUnit(dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;
                                }
                                dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                dd_airterminal(DDNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                // Setup the Average damper Position output variable
                // CurrentModuleObject='AirTerminal:DualDuct:ConstantVolume'
                SetupOutputVariable("Zone Air Terminal Cold Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                     dd_airterminal(DDNum).ColdAirDamperPosition,
                                    "System",
                                    "Average",
                                     dd_airterminal(DDNum).Name);
                SetupOutputVariable("Zone Air Terminal Hot Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                     dd_airterminal(DDNum).HotAirDamperPosition,
                                    "System",
                                    "Average",
                                     dd_airterminal(DDNum).Name);

            } // end Number of Damper Loop
        }

        if (NumDualDuctVarVolDampers > 0) {
            for (DamperIndex = 1; DamperIndex <= NumDualDuctVarVolDampers; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = cCMO_DDVariableVolume;

                inputProcessor->getObjectItem(CurrentModuleObject,
                                              DamperIndex,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex + NumDualDuctConstVolDampers;
                GlobalNames::VerifyUniqueInterObjectName(UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                 dd_airterminal(DDNum).Name = AlphArray(1);
                 dd_airterminal(DDNum).DamperType = DualDuct_VariableVolume;
                 dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                     dd_airterminal(DDNum).SchedPtr = ScheduleAlwaysOn;
                } else {
                     dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(AlphArray(2));
                    if ( dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(CurrentModuleObject + ", \"" +  dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) + " = " + AlphArray(2) +
                                        " not found.");
                        ErrorsFound = true;
                    }
                }
                 dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(AlphArray(3),
                                                                    ErrorsFound,
                                                                    CurrentModuleObject,
                                                                    AlphArray(1),
                                                                    NodeType_Air,
                                                                    NodeConnectionType_Outlet,
                                                                    1,
                                                                    ObjectIsNotParent,
                                                                    cAlphaFields(3));
                 dd_airterminal(DDNum).HotAirInletNodeNum = GetOnlySingleNode(AlphArray(4),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject,
                                                                         AlphArray(1),
                                                                         NodeType_Air,
                                                                         NodeConnectionType_Inlet,
                                                                         1,
                                                                         ObjectIsNotParent,
                                                                         cAlphaFields(4));
                 dd_airterminal(DDNum).ColdAirInletNodeNum = GetOnlySingleNode(AlphArray(5),
                                                                          ErrorsFound,
                                                                          CurrentModuleObject,
                                                                          AlphArray(1),
                                                                          NodeType_Air,
                                                                          NodeConnectionType_Inlet,
                                                                          1,
                                                                          ObjectIsNotParent,
                                                                          cAlphaFields(5));

                 dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                 dd_airterminal(DDNum).ZoneMinAirFracDes = NumArray(2);

                // Register component set data - one for heat and one for cool
                TestCompSet(CurrentModuleObject + ":HEAT",  dd_airterminal(DDNum).Name, AlphArray(4), AlphArray(3), "Air Nodes");
                TestCompSet(CurrentModuleObject + ":COOL",  dd_airterminal(DDNum).Name, AlphArray(5), AlphArray(3), "Air Nodes");

                for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                    if (dd_airterminal(DDNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                        AirDistUnit(ADUNum).InletNodeNum = dd_airterminal(DDNum).ColdAirInletNodeNum;
                        AirDistUnit(ADUNum).InletNodeNum2 = dd_airterminal(DDNum).HotAirInletNodeNum;
                        dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if ( dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if ( dd_airterminal(DDNum).DamperType == DualDuct_ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if ( dd_airterminal(DDNum).DamperType == DualDuct_VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if ( dd_airterminal(DDNum).DamperType == DualDuct_OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(RoutineName + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" + CurrentModuleObject + ',' +
                                     dd_airterminal(DDNum).Name + "].");
                    ShowContinueError("...should have outlet node=" + NodeID( dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (dd_airterminal(DDNum).OutletNodeNum == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = dd_airterminal(DDNum).ColdAirInletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode = dd_airterminal(DDNum).HotAirInletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = dd_airterminal(DDNum).OutletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode = dd_airterminal(DDNum).OutletNodeNum;
                                AirDistUnit(dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;

                                dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                dd_airterminal(DDNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                if (!lAlphaBlanks(6)) {
                    dd_airterminal(DDNum).OARequirementsPtr = UtilityRoutines::FindItemInList(AlphArray(6), OARequirements);
                    if ( dd_airterminal(DDNum).OARequirementsPtr == 0) {
                        ShowSevereError(cAlphaFields(6) + " = " + AlphArray(6) + " not found.");
                        ShowContinueError("Occurs in " + cCMO_DDVariableVolume + " = " + dd_airterminal(DDNum).Name);
                        ErrorsFound = true;
                    } else {
                         dd_airterminal(DDNum).NoOAFlowInputFromUser = false;
                    }
                }

                if (lAlphaBlanks(7)) {
                    dd_airterminal(DDNum).ZoneTurndownMinAirFrac = 1.0;
                    dd_airterminal(DDNum).ZoneTurndownMinAirFracSchExist = false;
                } else {
                    dd_airterminal(DDNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(AlphArray(7));
                    if (dd_airterminal(DDNum).ZoneTurndownMinAirFracSchPtr == 0) {
                        ShowSevereError(cAlphaFields(7) + " = " + AlphArray(7) + " not found.");
                        ShowContinueError("Occurs in " + cCMO_DDVariableVolume + " = " + dd_airterminal(DDNum).Name);
                        ErrorsFound = true;
                    }
                    dd_airterminal(DDNum).ZoneTurndownMinAirFracSchExist = true;
                }

                // Setup the Average damper Position output variable
                // CurrentModuleObject='AirTerminal:DualDuct:VAV'
                SetupOutputVariable("Zone Air Terminal Cold Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    dd_airterminal(DDNum).ColdAirDamperPosition,
                                    "System",
                                    "Average",
                                     dd_airterminal(DDNum).Name);
                SetupOutputVariable("Zone Air Terminal Hot Supply Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    dd_airterminal(DDNum).HotAirDamperPosition,
                                    "System",
                                    "Average",
                                    dd_airterminal(DDNum).Name);
                SetupOutputVariable("Zone Air Terminal Outdoor Air Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    dd_airterminal(DDNum).OutdoorAirFlowRate,
                                    "System",
                                    "Average",
                                    dd_airterminal(DDNum).Name);
            } // end Number of Damper Loop
        }

        if (NumDualDuctVarVolOA > 0) {
            for (DamperIndex = 1; DamperIndex <= NumDualDuctVarVolOA; ++DamperIndex) {

                // Load the info from the damper
                CurrentModuleObject = cCMO_DDVarVolOA;

                inputProcessor->getObjectItem(CurrentModuleObject,
                                              DamperIndex,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields);

                // Anything below this line in this control block should use DDNum
                DDNum = DamperIndex + NumDualDuctConstVolDampers + NumDualDuctVarVolDampers;
                GlobalNames::VerifyUniqueInterObjectName(UniqueDualDuctAirTerminalNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
                 dd_airterminal(DDNum).Name = AlphArray(1);
                 dd_airterminal(DDNum).DamperType = DualDuct_OutdoorAir;
                 dd_airterminal(DDNum).Schedule = AlphArray(2);
                if (lAlphaBlanks(2)) {
                     dd_airterminal(DDNum).SchedPtr = ScheduleAlwaysOn;
                } else {
                     dd_airterminal(DDNum).SchedPtr = GetScheduleIndex(AlphArray(2));
                    if ( dd_airterminal(DDNum).SchedPtr == 0) {
                        ShowSevereError(CurrentModuleObject + ", \"" +  dd_airterminal(DDNum).Name + "\" " + cAlphaFields(2) + " = " + AlphArray(2) +
                                        " not found.");
                        ErrorsFound = true;
                    }
                }
                 dd_airterminal(DDNum).OutletNodeNum = GetOnlySingleNode(AlphArray(3),
                                                                    ErrorsFound,
                                                                    CurrentModuleObject,
                                                                    AlphArray(1),
                                                                    NodeType_Air,
                                                                    NodeConnectionType_Outlet,
                                                                    1,
                                                                    ObjectIsNotParent,
                                                                    cAlphaFields(3));
                 dd_airterminal(DDNum).OAInletNodeNum = GetOnlySingleNode(AlphArray(4),
                                                                     ErrorsFound,
                                                                     CurrentModuleObject,
                                                                     AlphArray(1),
                                                                     NodeType_Air,
                                                                     NodeConnectionType_Inlet,
                                                                     1,
                                                                     ObjectIsNotParent,
                                                                     cAlphaFields(4));

                if (!lAlphaBlanks(5)) {
                     dd_airterminal(DDNum).RecircAirInletNodeNum = GetOnlySingleNode(AlphArray(5),
                                                                                ErrorsFound,
                                                                                CurrentModuleObject,
                                                                                AlphArray(1),
                                                                                NodeType_Air,
                                                                                NodeConnectionType_Inlet,
                                                                                1,
                                                                                ObjectIsNotParent,
                                                                                cAlphaFields(5));
                } else {
                    // for this model, we intentionally allow not using the recirc side
                     dd_airterminal(DDNum).RecircIsUsed = false;
                }

                 dd_airterminal(DDNum).MaxAirVolFlowRate = NumArray(1);
                 dd_airterminal(DDNum).MaxAirMassFlowRate =  dd_airterminal(DDNum).MaxAirVolFlowRate * StdRhoAir;

                // Register component set data - one for OA and one for RA
                TestCompSet(CurrentModuleObject + ":OutdoorAir",  dd_airterminal(DDNum).Name, AlphArray(4), AlphArray(3), "Air Nodes");
                if (  dd_airterminal(DDNum).RecircIsUsed) {
                    TestCompSet(CurrentModuleObject + ":RecirculatedAir",  dd_airterminal(DDNum).Name, AlphArray(5), AlphArray(3), "Air Nodes");
                }

                {
                    auto const SELECT_CASE_var(AlphArray(7));
                    if (SELECT_CASE_var == "CURRENTOCCUPANCY") {
                        dd_airterminal(DDNum).OAPerPersonMode = PerPersonDCVByCurrentLevel;

                    } else if (SELECT_CASE_var == "DESIGNOCCUPANCY") {
                        dd_airterminal(DDNum).OAPerPersonMode = PerPersonByDesignLevel;
                    }
                }
                // checks on this are done later

                for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                    if (  dd_airterminal(DDNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                        AirDistUnit(ADUNum).InletNodeNum =  dd_airterminal(DDNum).OAInletNodeNum;
                        AirDistUnit(ADUNum).InletNodeNum2 =  dd_airterminal(DDNum).RecircAirInletNodeNum;
                         dd_airterminal(DDNum).ADUNum = ADUNum;
                    }
                }
                // one assumes if there isn't one assigned, it's an error?
                if (  dd_airterminal(DDNum).ADUNum == 0) {
                    // convenient String
                    if (  dd_airterminal(DDNum).DamperType == DualDuct_ConstantVolume) {
                        CurrentModuleObject = "ConstantVolume";
                    } else if (  dd_airterminal(DDNum).DamperType == DualDuct_VariableVolume) {
                        CurrentModuleObject = "VAV";
                    } else if (  dd_airterminal(DDNum).DamperType == DualDuct_OutdoorAir) {
                        CurrentModuleObject = "VAV:OutdoorAir";
                    } else {
                        CurrentModuleObject = "*invalid*";
                    }
                    ShowSevereError(RoutineName + "No matching List:Zone:AirTerminal for AirTerminal:DualDuct = [" + CurrentModuleObject + ',' +
                                    dd_airterminal(DDNum).Name + "].");
                    ShowContinueError("...should have outlet node=" + NodeID(  dd_airterminal(DDNum).OutletNodeNum));
                    ErrorsFound = true;
                } else {

                    // Fill the Zone Equipment data with the inlet node numbers of this unit.
                    for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                            if (  dd_airterminal(DDNum).OutletNodeNum == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                                if (  dd_airterminal(DDNum).RecircIsUsed) {
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =  dd_airterminal(DDNum).RecircAirInletNodeNum;
                                } else {
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =  dd_airterminal(DDNum).OAInletNodeNum;
                                }
                                ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).InNode =  dd_airterminal(DDNum).OAInletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =  dd_airterminal(DDNum).OutletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitHeat(SupAirIn).OutNode =  dd_airterminal(DDNum).OutletNodeNum;
                                AirDistUnit(  dd_airterminal(DDNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(  dd_airterminal(DDNum).ADUNum).ZoneEqNum = CtrlZone;

                                dd_airterminal(DDNum).CtrlZoneNum = CtrlZone;
                                dd_airterminal(DDNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                dd_airterminal(DDNum).CtrlZoneInNodeIndex = SupAirIn;
                            }
                        }
                    }
                }
                 dd_airterminal(DDNum).OARequirementsPtr = UtilityRoutines::FindItemInList(AlphArray(6), OARequirements);
                if (  dd_airterminal(DDNum).OARequirementsPtr == 0) {
                    ShowSevereError(cAlphaFields(6) + " = " + AlphArray(6) + " not found.");
                    ShowContinueError("Occurs in " + cCMO_DDVarVolOA + " = " +  dd_airterminal(DDNum).Name);
                    ErrorsFound = true;
                } else {
                     dd_airterminal(DDNum).NoOAFlowInputFromUser = false;

                    // now fill design OA rate
                     dd_airterminal(DDNum).CalcOAOnlyMassFlow(DummyOAFlow,  dd_airterminal(DDNum).DesignOAFlowRate);

                    if (  dd_airterminal(DDNum).MaxAirVolFlowRate != AutoSize) {
                        ReportSizingOutput(CurrentModuleObject,
                                            dd_airterminal(DDNum).Name,
                                           "Maximum Outdoor Air Flow Rate [m3/s]",
                                            dd_airterminal(DDNum).DesignOAFlowRate);

                        if (  dd_airterminal(DDNum).RecircIsUsed) {
                             dd_airterminal(DDNum).DesignRecircFlowRate =  dd_airterminal(DDNum).MaxAirVolFlowRate -  dd_airterminal(DDNum).DesignOAFlowRate;
                             dd_airterminal(DDNum).DesignRecircFlowRate = max(0.0,  dd_airterminal(DDNum).DesignRecircFlowRate);
                            ReportSizingOutput(CurrentModuleObject,
                                                dd_airterminal(DDNum).Name,
                                               "Maximum Recirculated Air Flow Rate [m3/s]",
                                                dd_airterminal(DDNum).DesignRecircFlowRate);
                        } else {
                            if (  dd_airterminal(DDNum).MaxAirVolFlowRate <  dd_airterminal(DDNum).DesignOAFlowRate) {
                                ShowSevereError("The value " + RoundSigDigits(  dd_airterminal(DDNum).MaxAirVolFlowRate, 5) + " in " + cNumericFields(1) +
                                                "is lower than the outdoor air requirement.");
                                ShowContinueError("Occurs in " + cCMO_DDVarVolOA + " = " +  dd_airterminal(DDNum).Name);
                                ShowContinueError("The design outdoor air requirement is " + RoundSigDigits(  dd_airterminal(DDNum).DesignOAFlowRate, 5));
                                ErrorsFound = true;
                            }
                        }
                    }
                }

                if (  dd_airterminal(DDNum).OAPerPersonMode == PerPersonModeNotSet) {
                    DummyOAFlow = OARequirements(  dd_airterminal(DDNum).OARequirementsPtr).OAFlowPerPerson;
                    if ((DummyOAFlow == 0.0) && (lAlphaBlanks(7))) {       // no worries
                                                                           // do nothing, okay since no per person requirement involved
                    } else if ((DummyOAFlow > 0.0) && (lAlphaBlanks(7))) { // missing input
                        ShowSevereError(cAlphaFields(7) + " was blank.");
                        ShowContinueError("Occurs in " + cCMO_DDVarVolOA + " = " + dd_airterminal(DDNum).Name);
                        ShowContinueError("Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"");
                        ErrorsFound = true;
                    } else if ((DummyOAFlow > 0.0) && !(lAlphaBlanks(7))) { // incorrect input
                        ShowSevereError(cAlphaFields(7) + " = " + AlphArray(7) + " not a valid key choice.");
                        ShowContinueError("Occurs in " + cCMO_DDVarVolOA + " = " + dd_airterminal(DDNum).Name);
                        ShowContinueError("Valid choices are \"CurrentOccupancy\" or \"DesignOccupancy\"");
                        ErrorsFound = true;
                    }
                }

                // Setup the Average damper Position output variable
                SetupOutputVariable("Zone Air Terminal Outdoor Air Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    dd_airterminal(DDNum).OADamperPosition,
                                    "System",
                                    "Average",
                                     dd_airterminal(DDNum).Name);
                SetupOutputVariable("Zone Air Terminal Recirculated Air Duct Damper Position",
                                    OutputProcessor::Unit::None,
                                    dd_airterminal(DDNum).RecircAirDamperPosition,
                                    "System",
                                    "Average",
                                     dd_airterminal(DDNum).Name);
                SetupOutputVariable("Zone Air Terminal Outdoor Air Fraction",
                                    OutputProcessor::Unit::None,
                                    dd_airterminal(DDNum).OAFraction,
                                    "System",
                                    "Average",
                                    dd_airterminal(DDNum).Name);

            } // end Number of Damper Loop
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    // End of Get Input subroutines for the Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void DualDuctAirTerminal::InitDualDuct(bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   February 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for  initializations of the Damper Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using DataConvergParams::HVACFlowRateToler;
        using DataDefineEquip::AirDistUnit;
        using DataHeatBalance::People;
        using DataHeatBalance::TotPeople;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ZoneEquipConfig;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int HotInNode;
        int ColdInNode;
        int OAInNode; // Outdoor Air Inlet Node for VAV:OutdoorAir units
        int RAInNode; // Reciruclated Air Inlet Node for VAV:OutdoorAir units
        int OutNode;
        //static Array1D_bool MyEnvrnFlag;
        //static Array1D_bool MySizeFlag;
        //static Array1D_bool MyAirLoopFlag;
        int Loop;                                    // Loop checking control variable
        Real64 PeopleFlow;                           // local sum variable, m3/s
        // FLOW:

        // Do the Begin Simulation initializations
        if (InitDualDuctMyOneTimeFlag) {

            //MyEnvrnFlag.allocate(NumDDAirTerminal);
            //MySizeFlag.allocate(NumDDAirTerminal);
            //MyAirLoopFlag.dimension(NumDDAirTerminal, true);
            //MyEnvrnFlag = true;
            //MySizeFlag = true;
            MassFlowSetToler = HVACFlowRateToler * 0.00001;

            InitDualDuctMyOneTimeFlag = false;
        }

        if (!ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            // Check to see if there is a Air Distribution Unit on the Zone Equipment List
            for (Loop = 1; Loop <= NumDDAirTerminal; ++Loop) {
                if (this->ADUNum == 0) continue;
                if (CheckZoneEquipmentList("ZONEHVAC:AIRDISTRIBUTIONUNIT", AirDistUnit(this->ADUNum).Name)) continue;
                ShowSevereError("InitDualDuct: ADU=[Air Distribution Unit," + AirDistUnit(this->ADUNum).Name +
                                "] is not on any ZoneHVAC:EquipmentList.");
                if (this->DamperType == DualDuct_ConstantVolume) {
                    ShowContinueError("...Dual Duct Damper=[" + cCMO_DDConstantVolume + ',' + this->Name + "] will not be simulated.");
                } else if (this->DamperType == DualDuct_VariableVolume) {
                    ShowContinueError("...Dual Duct Damper=[" + cCMO_DDVariableVolume + ',' + this->Name + "] will not be simulated.");
                } else if (this->DamperType == DualDuct_OutdoorAir) {
                    ShowContinueError("...Dual Duct Damper=[" + cCMO_DDVarVolOA + ',' + this->Name + "] will not be simulated.");
                } else {
                    ShowContinueError("...Dual Duct Damper=[unknown/invalid," + this->Name + "] will not be simulated.");
                }
            }
        }

        if (!SysSizingCalc && this->MySizeFlag) {

            this->SizeDualDuct();

            this->MySizeFlag = false;
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag && this->MyEnvrnFlag) {

            if (this->DamperType == DualDuct_ConstantVolume || this->DamperType == DualDuct_VariableVolume) {
                OutNode = this->OutletNodeNum;
                HotInNode = this->HotAirInletNodeNum;
                ColdInNode = this->ColdAirInletNodeNum;
                Node(OutNode).MassFlowRateMax = this->MaxAirVolFlowRate * StdRhoAir;
                if (this->DamperType == DualDuct_ConstantVolume) {
                    Node(OutNode).MassFlowRateMin = 0.0;
                } else if (this->DamperType == DualDuct_VariableVolume) {
                    // get dual duct air terminal box minimum flow fraction value
                    if (this->ZoneTurndownMinAirFracSchExist) {
                        this->ZoneTurndownMinAirFrac = ScheduleManager::GetScheduleMinValue(this->ZoneTurndownMinAirFracSchPtr);
                    } else {
                        this->ZoneTurndownMinAirFrac = 1.0;
                    }
                    Node(OutNode).MassFlowRateMin = Node(OutNode).MassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                } else {
                    Node(OutNode).MassFlowRateMin = 0.0;
                }
                this->dd_airterminalHotAirInlet.AirMassFlowRateMax = Node(OutNode).MassFlowRateMax;
                this->dd_airterminalColdAirInlet.AirMassFlowRateMax = Node(OutNode).MassFlowRateMax;
                Node(HotInNode).MassFlowRateMax = Node(OutNode).MassFlowRateMax;
                Node(ColdInNode).MassFlowRateMax = Node(OutNode).MassFlowRateMax;
                Node(HotInNode).MassFlowRateMin = 0.0;
                Node(ColdInNode).MassFlowRateMin = 0.0;
                this->MyEnvrnFlag = false;

            } else if (this->DamperType == DualDuct_OutdoorAir) {
                // Initialize for DualDuct:VAV:OutdoorAir
                OutNode = this->OutletNodeNum;
                OAInNode = this->OAInletNodeNum;
                if (this->RecircIsUsed) RAInNode = this->RecircAirInletNodeNum;
                Node(OutNode).MassFlowRateMax = this->MaxAirMassFlowRate;
                Node(OutNode).MassFlowRateMin = 0.0;
                this->dd_airterminalOAInlet.AirMassFlowRateMax = this->DesignOAFlowRate * StdRhoAir;
                if (this->RecircIsUsed) {
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateMax =
                        this->MaxAirMassFlowRate - this->dd_airterminalOAInlet.AirMassFlowRateMax;
                    Node(RAInNode).MassFlowRateMax = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    Node(RAInNode).MassFlowRateMin = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag = 1.0e-10 * this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                }
                Node(OAInNode).MassFlowRateMax = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                Node(OAInNode).MassFlowRateMin = 0.0;
                // figure per person by design level for the OA duct.
                PeopleFlow = 0.0;
                for (Loop = 1; Loop <= TotPeople; ++Loop) {
                    if (People(Loop).ZonePtr != this->ActualZoneNum) continue;
                    int damperOAFlowMethod = OARequirements(this->OARequirementsPtr).OAFlowMethod;
                    if (damperOAFlowMethod == OAFlowPPer || damperOAFlowMethod == OAFlowSum || damperOAFlowMethod == OAFlowMax) {
                        PeopleFlow += People(Loop).NumberOfPeople * OARequirements(this->OARequirementsPtr).OAFlowPerPerson;
                    }
                }
                this->OAPerPersonByDesignLevel = PeopleFlow;

                this->MyEnvrnFlag = false;
            }
        }

        if (!BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // Find air loop associated with this terminal unit
        if (this->MyAirLoopFlag) {
            if (this->AirLoopNum == 0) {
                if ((this->CtrlZoneNum > 0) && (this->CtrlZoneInNodeIndex > 0)) {
                    this->AirLoopNum = ZoneEquipConfig(this->CtrlZoneNum).InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
                    AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
                    // Don't set MyAirLoopFlag to false yet because airloopnums might not be populated yet
                }
            } else {
                this->MyAirLoopFlag = false;
            }
        }

        // Initialize the Inlet Nodes of the Sys
        if (  this->DamperType == DualDuct_ConstantVolume ||  this->DamperType == DualDuct_VariableVolume) {
            HotInNode =  this->HotAirInletNodeNum;
            ColdInNode =  this->ColdAirInletNodeNum;
            OutNode =  this->OutletNodeNum;
        } else if (  this->DamperType == DualDuct_OutdoorAir) {
            OAInNode =  this->OAInletNodeNum;
            if (  this->RecircIsUsed) RAInNode =  this->RecircAirInletNodeNum;
            OutNode =  this->OutletNodeNum;
        }

        if (FirstHVACIteration) {
            //     CALL DisplayString('Init First HVAC Iteration {'//TRIM(  dd_airterminal(DDNum)%DamperName)//'}') !-For debugging - REMOVE
            // The first time through set the mass flow rate to the Max
            // Take care of the flow rates first. For Const Vol and VAV.
            if (this->DamperType == DualDuct_ConstantVolume || this->DamperType == DualDuct_VariableVolume) {
                if ((Node(HotInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(HotInNode).MassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
                } else {
                    Node(HotInNode).MassFlowRate = 0.0;
                }
                if ((Node(ColdInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(ColdInNode).MassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
                } else {
                    Node(ColdInNode).MassFlowRate = 0.0;
                }
                // Next take care of the Max Avail Flow Rates
                if ((Node(HotInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(HotInNode).MassFlowRateMaxAvail = this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
                } else {
                    Node(HotInNode).MassFlowRateMaxAvail = 0.0;
                }
                if ((Node(ColdInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(ColdInNode).MassFlowRateMaxAvail = this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
                } else {
                    Node(ColdInNode).MassFlowRateMaxAvail = 0.0;
                }
                // get current time step air terminal box turndown minimum flow fraction
                if (this->ZoneTurndownMinAirFracSchExist) {
                    this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(this->ZoneTurndownMinAirFracSchPtr);
                } else {
                    this->ZoneTurndownMinAirFrac = 1.0;
                }
                // update to the current dual duct minimum air flow fraction
                this->ZoneMinAirFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                // The last item is to take care of the Min Avail Flow Rates
                if ((Node(HotInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(HotInNode).MassFlowRateMinAvail = this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                } else {
                    Node(HotInNode).MassFlowRateMinAvail = 0.0;
                }
                if ((Node(ColdInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(ColdInNode).MassFlowRateMinAvail = this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                } else {
                    Node(ColdInNode).MassFlowRateMinAvail = 0.0;
                }

            } else if (this->DamperType == DualDuct_OutdoorAir) {
                // The first time through set the mass flow rate to the Max for VAV:OutdoorAir
                if ((Node(OAInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(OAInNode).MassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                } else {
                    Node(OAInNode).MassFlowRate = 0.0;
                }
                if (this->RecircIsUsed) {
                    if ((Node(RAInNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                        Node(RAInNode).MassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    } else {
                        Node(RAInNode).MassFlowRate = 0.0;
                    }
                    // clear flow history
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1 = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2 = 0.0;
                    this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3 = 0.0;
                }
                // Next take care of the Max Avail Flow Rates
                if ((Node(OAInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                    Node(OAInNode).MassFlowRateMaxAvail = this->dd_airterminalOAInlet.AirMassFlowRateMax;
                } else {
                    Node(OAInNode).MassFlowRateMaxAvail = 0.0;
                }
                if (this->RecircIsUsed) {
                    if ((Node(RAInNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                        Node(RAInNode).MassFlowRateMaxAvail = this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
                    } else {
                        Node(RAInNode).MassFlowRateMaxAvail = 0.0;
                    }
                }
                // The last item is to take care of the Min Avail Flow Rates. VAV:OutdoorAir
                Node(OAInNode).MassFlowRateMinAvail = 0.0;
                if (this->RecircIsUsed) Node(RAInNode).MassFlowRateMinAvail = 0.0;
            }
        }

        // Initialize the Inlet Nodes of the Dampers for Const. Vol and VAV
        if (this->DamperType == DualDuct_ConstantVolume || this->DamperType == DualDuct_VariableVolume) {

            this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail = min(Node(OutNode).MassFlowRateMax, Node(HotInNode).MassFlowRateMaxAvail);
            this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail =
                min(max(Node(OutNode).MassFlowRateMin, Node(HotInNode).MassFlowRateMinAvail), Node(HotInNode).MassFlowRateMaxAvail);

            this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail = min(Node(OutNode).MassFlowRateMax, Node(ColdInNode).MassFlowRateMaxAvail);
            this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail =
                min(max(Node(OutNode).MassFlowRateMin, Node(ColdInNode).MassFlowRateMinAvail), Node(ColdInNode).MassFlowRateMaxAvail);

            // Do the following initializations (every time step): This should be the info from
            // the previous components outlets or the node data in this section.
            // Load the node data in this section for the component simulation
            this->dd_airterminalHotAirInlet.AirMassFlowRate = Node(HotInNode).MassFlowRate;
            this->dd_airterminalHotAirInlet.AirTemp = Node(HotInNode).Temp;
            this->dd_airterminalHotAirInlet.AirHumRat = Node(HotInNode).HumRat;
            this->dd_airterminalHotAirInlet.AirEnthalpy = Node(HotInNode).Enthalpy;
            this->dd_airterminalColdAirInlet.AirMassFlowRate = Node(ColdInNode).MassFlowRate;
            this->dd_airterminalColdAirInlet.AirTemp = Node(ColdInNode).Temp;
            this->dd_airterminalColdAirInlet.AirHumRat = Node(ColdInNode).HumRat;
            this->dd_airterminalColdAirInlet.AirEnthalpy = Node(ColdInNode).Enthalpy;

            // Initialize the Inlet Nodes of the Dampers for VAV:OutdoorAir
        } else if (this->DamperType == DualDuct_OutdoorAir) {
            this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail = Node(OAInNode).MassFlowRateMaxAvail;
            this->dd_airterminalOAInlet.AirMassFlowRateMinAvail = Node(OAInNode).MassFlowRateMinAvail;

            // Do the following initializations (every time step): This should be the info from
            // the previous components outlets or the node data in this section.
            // Load the node data in this section for the component simulation
            this->dd_airterminalOAInlet.AirMassFlowRate = Node(OAInNode).MassFlowRate;
            this->dd_airterminalOAInlet.AirTemp = Node(OAInNode).Temp;
            this->dd_airterminalOAInlet.AirHumRat = Node(OAInNode).HumRat;
            this->dd_airterminalOAInlet.AirEnthalpy = Node(OAInNode).Enthalpy;
            if (this->RecircIsUsed) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail = Node(RAInNode).MassFlowRateMaxAvail;
                this->dd_airterminalRecircAirInlet.AirMassFlowRateMinAvail = Node(RAInNode).MassFlowRateMinAvail;
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = Node(RAInNode).MassFlowRate;
                this->dd_airterminalRecircAirInlet.AirTemp = Node(RAInNode).Temp;
                this->dd_airterminalRecircAirInlet.AirHumRat = Node(RAInNode).HumRat;
                this->dd_airterminalRecircAirInlet.AirEnthalpy = Node(RAInNode).Enthalpy;
            }
        }
    }

    void DualDuctAirTerminal::SizeDualDuct()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   January 2002
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Dual Duct air terminal units for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays.

        // Using/Aliasing
        using ReportSizingManager::ReportSizingOutput;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string DamperType;

        if (this->MaxAirVolFlowRate == AutoSize) {

            if ((CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
                if (this->DamperType == DualDuct_ConstantVolume) {
                    DamperType = cCMO_DDConstantVolume;
                } else if (this->DamperType == DualDuct_VariableVolume) {
                    DamperType = cCMO_DDVariableVolume;
                } else if (this->DamperType == DualDuct_OutdoorAir) {
                    DamperType = cCMO_DDVarVolOA;
                } else {
                    DamperType = "Invalid/Unknown";
                }
                CheckZoneSizing(DamperType, this->Name);
                this->MaxAirVolFlowRate =
                    max(TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow, TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
                if (this->DamperType == DualDuct_OutdoorAir) {
                    if (this->RecircIsUsed) {
                        this->DesignRecircFlowRate = max(TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                                         TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
                        this->MaxAirVolFlowRate = this->DesignRecircFlowRate + this->DesignOAFlowRate;
                    } else {
                        this->MaxAirVolFlowRate = this->DesignOAFlowRate;
                        this->DesignRecircFlowRate = 0.0;
                    }
                    this->MaxAirMassFlowRate = this->MaxAirVolFlowRate * StdRhoAir;
                }

                if (this->MaxAirVolFlowRate < SmallAirVolFlow) {
                    this->MaxAirVolFlowRate = 0.0;
                    this->MaxAirMassFlowRate = 0.0;
                    this->DesignOAFlowRate = 0.0;
                    this->DesignRecircFlowRate = 0.0;
                }
                ReportSizingOutput(DamperType, this->Name, "Maximum Air Flow Rate [m3/s]", this->MaxAirVolFlowRate);
                if (this->DamperType == DualDuct_OutdoorAir) {
                    ReportSizingOutput(DamperType, this->Name, "Maximum Outdoor Air Flow Rate [m3/s]", this->DesignOAFlowRate);
                    if (this->RecircIsUsed) {
                        ReportSizingOutput(DamperType, this->Name, "Maximum Recirculated Air Flow Rate [m3/s]", this->DesignRecircFlowRate);
                    }
                }
            }
        }
    }

    // End Initialization Section of the Module
    //******************************************************************************

    // Begin Algorithm Section of the Module
    //******************************************************************************

    void DualDuctAirTerminal::SimDualDuctConstVol(int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Jan 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple mixing damper.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused0909   USE DataHeatBalFanSys, ONLY: Mat
        using DataHVACGlobals::SmallTempDiff;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;    // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 HumRat;      // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;    // [Watts]
        Real64 Temperature; // [C]
        Real64 QTotLoad;    // [W]
        Real64 QZnReq;      // [W]
        Real64 CpAirZn;
        Real64 CpAirSysHot;
        Real64 CpAirSysCold;

        // Get the calculated load from the Heat Balance from ZoneSysEnergyDemand
        QTotLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        // Need the design MassFlowRate for calculations
        if (GetCurrentScheduleValue(this->SchedPtr) > 0.0) {
            MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail / 2.0 + this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail / 2.0;
        } else {
            MassFlow = 0.0;
        }
        // If there is massflow then need to provide the correct amount of total
        //  required zone energy
        if (MassFlow > SmallMassFlow) {
            CpAirZn = PsyCpAirFnW(Node(ZoneNodeNum).HumRat);
            QZnReq = QTotLoad + MassFlow * CpAirZn * Node(ZoneNodeNum).Temp;
            // If the enthalpy is the same for the hot and cold duct then there would be a
            //  divide by zero so for heating or cooling set the damper to one max flow
            //  or the other.
            if (std::abs(this->dd_airterminalColdAirInlet.AirTemp - this->dd_airterminalHotAirInlet.AirTemp) > SmallTempDiff) {
                // CpAirSysHot = PsyCpAirFnWTdb(dd_airterminalHotAirInlet(DDNum)%AirHumRat,dd_airterminalHotAirInlet(DDNum)%AirTemp)
                // CpAirSysCold= PsyCpAirFnWTdb(dd_airterminalColdAirInlet(DDNum)%AirHumRat,dd_airterminalColdAirInlet(DDNum)%AirTemp)
                CpAirSysHot = CpAirZn;
                CpAirSysCold = CpAirZn;
                // Determine the Cold Air Mass Flow Rate
                this->dd_airterminalColdAirInlet.AirMassFlowRate =
                    (QZnReq - MassFlow * CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) /
                    (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp);
            } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRate > 0.0)) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }
            // Check to make sure that the calculated flow is not greater than the available flows
            if (this->dd_airterminalColdAirInlet.AirMassFlowRate > this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail;
            }
            // Using Mass Continuity to determine the other duct flow quantity
            this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow - this->dd_airterminalColdAirInlet.AirMassFlowRate;
            if (this->dd_airterminalHotAirInlet.AirMassFlowRate > this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            } else if (this->dd_airterminalHotAirInlet.AirMassFlowRate < this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail;
            }
            MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRate + this->dd_airterminalHotAirInlet.AirMassFlowRate;
        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }
        if (MassFlow > SmallMassFlow) {
            // After flows are calculated then calculate the mixed air flow properties.
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                      this->dd_airterminalColdAirInlet.AirHumRat * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                     MassFlow;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                        this->dd_airterminalColdAirInlet.AirEnthalpy * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                       MassFlow;

            // If there is no air flow than calculate the No Flow conditions
        } else {
            this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat + this->dd_airterminalColdAirInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy + this->dd_airterminalColdAirInlet.AirEnthalpy) / 2.0;
        }
        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        // Load all properties in the damper outlet
        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMinAvail =
            min(this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail, this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail);
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the hot and cold damper position in %
        if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMax == 0.0) || (this->dd_airterminalColdAirInlet.AirMassFlowRateMax == 0.0)) {
            this->ColdAirDamperPosition = 0.0;
            this->HotAirDamperPosition = 0.0;
        } else {
            this->ColdAirDamperPosition =
                this->dd_airterminalColdAirInlet.AirMassFlowRate / this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
            this->HotAirDamperPosition = this->dd_airterminalHotAirInlet.AirMassFlowRate / this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        }
    }

    void DualDuctAirTerminal::SimDualDuctVarVol(int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Jan 2000
        //       MODIFIED       na
        //                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
        //                                 air fraction - a TRACE feature
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple mixing damper.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused0909   USE DataHeatBalFanSys, ONLY: Mat
        using DataHVACGlobals::SmallTempDiff;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;    // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 HumRat;      // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;    // [Watts]
        Real64 Temperature; // [C]
        Real64 QTotLoad;    // [W]
        Real64 QZnReq;      // [W]
        Real64 CpAirZn;     // specific heat of zone air
        Real64 CpAirSysHot;
        Real64 CpAirSysCold;
        Real64 MassFlowBasedOnOA; // Supply air flow rate based on minimum OA requirement
        Real64 AirLoopOAFrac;     // fraction of outdoor air entering air loop outside air system

        // The calculated load from the Heat Balance
        QTotLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        // Calculate all of the required Cp's
        CpAirZn = PsyCpAirFnW(Node(ZoneNodeNum).HumRat);
        // CpAirSysHot = PsyCpAirFnW(DamperHotAirInlet(DDNum)%AirHumRat,DamperHotAirInlet(DDNum)%AirTemp)
        // CpAirSysCold= PsyCpAirFnW(DamperColdAirInlet(DDNum)%AirHumRat,DamperColdAirInlet(DDNum)%AirTemp)
        CpAirSysHot = CpAirZn;
        CpAirSysCold = CpAirZn;

        // calculate supply air flow rate based on user specified OA requirement
        this->CalcOAMassFlow(MassFlowBasedOnOA, AirLoopOAFrac);

        // Then depending on if the Load is for heating or cooling it is handled differently.  First
        // the massflow rate of either heating or cooling is determined to meet the entire load.  Then
        // if the massflow is below the minimum or greater than the Max it is set to either the Min
        // or the Max as specified for the VAV model.
        if (GetCurrentScheduleValue( this->SchedPtr) == 0.0) {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;

        } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // Then heating is needed
            // Next check for the denominator equal to zero
            if (std::abs((CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) - (CpAirZn * Node(ZoneNodeNum).Temp)) / CpAirZn > SmallTempDiff) {
                MassFlow = QTotLoad / (CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp - CpAirZn * Node(ZoneNodeNum).Temp);
            } else {
                // If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            }
            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            if (MassFlow <= (this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac)) {
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                MassFlow = max(MassFlow, this->dd_airterminalHotAirInlet.AirMassFlowRateMinAvail);
            } else if (MassFlow >= this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail) {
                MassFlow = this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail;
            }

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail);

        } else if ((QTotLoad < 0.0) && (this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // Then cooling is required
            // Next check for the denominator equal to zero
            if (std::abs((CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp) - (CpAirZn * Node(ZoneNodeNum).Temp)) / CpAirZn > SmallTempDiff) {
                MassFlow = QTotLoad / (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirZn * Node(ZoneNodeNum).Temp);
            } else {
                // If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            }

            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            if ((MassFlow <= (this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac)) && (MassFlow >= 0.0)) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMax * this->ZoneMinAirFrac;
                MassFlow = max(MassFlow, this->dd_airterminalColdAirInlet.AirMassFlowRateMinAvail);
            } else if (MassFlow < 0.0) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            } else if (MassFlow >= this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                MassFlow = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;
            }

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail);

        } else if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail > 0.0) || (this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail > 0.0)) {
            // No Load on Zone set to mixed condition
            MassFlow = (this->dd_airterminalHotAirInlet.AirMassFlowRateMax / 2.0) * this->ZoneMinAirFrac +
                       this->dd_airterminalColdAirInlet.AirMassFlowRateMax / 2.0 * this->ZoneMinAirFrac;

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, (this->dd_airterminalHotAirInlet.AirMassFlowRateMaxAvail + this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail));

        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }

        // Now the massflow for heating or cooling has been determined and if the massflow was reset to the
        // Min or Max we will need to mix the hot and cold deck to meet the zone load.  Knowing the enthalpy
        // of the zone and the hot and cold air flows we can determine exactly by using the Energy and Continuity
        // Eqns.  Of course we have to make sure that we are within the Min and Max flow conditions.
        if (MassFlow > SmallMassFlow) {
            // Determine the enthalpy required from Zone enthalpy and the zone load.
            QZnReq = QTotLoad + MassFlow * CpAirZn * Node(ZoneNodeNum).Temp;
            // Using the known enthalpies the cold air inlet mass flow is determined.  If the enthalpy of the hot and cold
            // air streams are equal the IF-Then block handles that condition.
            if (std::abs(this->dd_airterminalColdAirInlet.AirTemp - this->dd_airterminalHotAirInlet.AirTemp) > SmallTempDiff) {
                // Calculate the Cold air mass flow rate
                this->dd_airterminalColdAirInlet.AirMassFlowRate =
                    (QZnReq - MassFlow * CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp) /
                    (CpAirSysCold * this->dd_airterminalColdAirInlet.AirTemp - CpAirSysHot * this->dd_airterminalHotAirInlet.AirTemp);
            } else if ((QTotLoad > 0.0) && (this->dd_airterminalHotAirInlet.AirMassFlowRate > 0.0)) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }

            // Need to make sure that the flows are within limits
            if (this->dd_airterminalColdAirInlet.AirMassFlowRate > this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRateMaxAvail;

                // These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < 0.0) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate > MassFlow) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            }
            // Using Mass Continuity to determine the other duct flow quantity
            this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow - this->dd_airterminalColdAirInlet.AirMassFlowRate;

            if (this->dd_airterminalHotAirInlet.AirMassFlowRate < MassFlowSetToler) {
                this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
                this->dd_airterminalColdAirInlet.AirMassFlowRate = MassFlow;
            } else if (this->dd_airterminalColdAirInlet.AirMassFlowRate < MassFlowSetToler) {
                this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
                this->dd_airterminalHotAirInlet.AirMassFlowRate = MassFlow;
            }

            // After the flow rates are determined the properties are calculated.
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                      this->dd_airterminalColdAirInlet.AirHumRat * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                     MassFlow;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy * this->dd_airterminalHotAirInlet.AirMassFlowRate +
                        this->dd_airterminalColdAirInlet.AirEnthalpy * this->dd_airterminalColdAirInlet.AirMassFlowRate) /
                       MassFlow;

            // IF the system is OFF the properties are calculated for this special case.
        } else {
            this->dd_airterminalColdAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalHotAirInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalHotAirInlet.AirHumRat + this->dd_airterminalColdAirInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalHotAirInlet.AirEnthalpy + this->dd_airterminalColdAirInlet.AirEnthalpy) / 2.0;
        }
        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMinAvail = this->ZoneMinAirFrac * this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the hot and cold damper position in %
        if ((this->dd_airterminalHotAirInlet.AirMassFlowRateMax == 0.0) || (this->dd_airterminalColdAirInlet.AirMassFlowRateMax == 0.0)) {
            this->ColdAirDamperPosition = 0.0;
            this->HotAirDamperPosition = 0.0;
        } else {
            this->ColdAirDamperPosition =
                this->dd_airterminalColdAirInlet.AirMassFlowRate / this->dd_airterminalColdAirInlet.AirMassFlowRateMax;
            this->HotAirDamperPosition = this->dd_airterminalHotAirInlet.AirMassFlowRate / this->dd_airterminalHotAirInlet.AirMassFlowRateMax;
        }
    }

    void DualDuctAirTerminal::SimDualDuctVAVOutdoorAir(int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Clayton Miller
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       B. Griffith, Dec 2010, major rework
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Designed to accommodate for systems with outdoor air (OA) and recirculated air (RA)
        // as two separate air streams to controlled at the zone level in a dual duct system.

        // METHODOLOGY EMPLOYED:
        // The terminal unit is be designed to set the airflow of the of the OA stream at the zone
        // level based on the zonal ventilation requirements and the RA stream flowrate of recirculated
        // cooling air stream in order to meet the remaining thermal load.
        // If the zone calls for cooling but the inlet air temperature is too warm, recirc side set to zero
        // if the zone calls for heating and the inlet air is warm enough, modulate damper to meet load
        // if the zone calls for heating and the inlet air is too cold, zero flow (will not control sans reheat)

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using Psychrometrics::PsyCpAirFnW;
        using Psychrometrics::PsyTdbFnHW;
        using namespace DataGlobals;
        using DataHeatBalFanSys::ZoneThermostatSetPointHi;
        using DataHeatBalFanSys::ZoneThermostatSetPointLo;
        using DataHVACGlobals::SmallTempDiff;
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlowMax;     // [kg/sec]   Maximum Mass Flow Rate from OA and Recirc Inlets
        Real64 HumRat;          // [Kg Moisture / Kg dry air]
        Real64 Enthalpy;        // [Watts]
        Real64 Temperature;     // [C]
        Real64 QTotLoadRemain;  // [W]
        Real64 QtoHeatSPRemain; // [W]
        Real64 QtoCoolSPRemain; // [W]
        //  REAL(r64) :: QTotRemainAdjust  ! [W]
        Real64 QtoHeatSPRemainAdjust; // [W]
        Real64 QtoCoolSPRemainAdjust; // [W]
        Real64 QOALoadToHeatSP;       // [W]
        Real64 QOALoadToCoolSP;       // [W]
        Real64 QOALoad;               // Amount of cooling load accounted for by OA Stream [W]
        Real64 QRALoad;               // Amount of cooling load accounted for by Recirc Stream [W]
        Real64 CpAirZn;               // specific heat of zone air
        Real64 CpAirSysOA;            // specific heat of outdoor air
        Real64 CpAirSysRA;            // specific heat of recirculated air
        Real64 OAMassFlow;            // Supply air flow rate based on minimum OA requirement - for printing
        Real64 TotMassFlow;           // [kg/sec]   Total Mass Flow Rate from OA and Recirc Inlets
        int OAInletNodeNum;
        int RecircInletNodeNum;

        OAInletNodeNum = this->OAInletNodeNum;
        if (this->RecircIsUsed) {
            RecircInletNodeNum = this->RecircAirInletNodeNum;
        }
        // Calculate required ventilation air flow rate based on user specified OA requirement
        this->CalcOAOnlyMassFlow(OAMassFlow);

        // The calculated load from the Heat Balance, adjusted for any equipment sequenced before terminal
        QTotLoadRemain = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        QtoHeatSPRemain = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        QtoCoolSPRemain = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;

        // Calculate all of the required Cp's
        CpAirZn = PsyCpAirFnW(Node(ZoneNodeNum).HumRat);
        CpAirSysOA = PsyCpAirFnW(Node(OAInletNodeNum).HumRat);
        if (this->RecircIsUsed) CpAirSysRA = PsyCpAirFnW(Node(RecircInletNodeNum).HumRat);

        // Set the OA Damper to the calculated ventilation flow rate
        this->dd_airterminalOAInlet.AirMassFlowRate = OAMassFlow;
        // Need to make sure that the OA flows are within limits
        if (this->dd_airterminalOAInlet.AirMassFlowRate > this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail) {
            this->dd_airterminalOAInlet.AirMassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail;
        } else if (this->dd_airterminalOAInlet.AirMassFlowRate < 0.0) {
            this->dd_airterminalOAInlet.AirMassFlowRate = 0.0;
        }

        //..Find the amount of load that the OAMassFlow accounted for
        if (std::abs((CpAirSysOA * this->dd_airterminalOAInlet.AirTemp) - (CpAirZn * Node(ZoneNodeNum).Temp)) / CpAirZn > SmallTempDiff) {
            QOALoad = this->dd_airterminalOAInlet.AirMassFlowRate * (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp - CpAirZn * Node(ZoneNodeNum).Temp);

            QOALoadToHeatSP = this->dd_airterminalOAInlet.AirMassFlowRate *
                              (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp - CpAirZn * ZoneThermostatSetPointLo(ZoneNum));
            QOALoadToCoolSP = this->dd_airterminalOAInlet.AirMassFlowRate *
                              (CpAirSysOA * this->dd_airterminalOAInlet.AirTemp - CpAirZn * ZoneThermostatSetPointHi(ZoneNum));

        } else {
            QOALoad = 0.0;
            QOALoadToHeatSP = 0.0;
            QOALoadToCoolSP = 0.0;
        }

        if ( this->RecircIsUsed) {

            // correct load for recirc side to account for impact of OA side
            // QTotRemainAdjust      = QTotLoadRemain  - QOALoad
            QtoHeatSPRemainAdjust = QtoHeatSPRemain - QOALoadToHeatSP;
            QtoCoolSPRemainAdjust = QtoCoolSPRemain - QOALoadToCoolSP;

            if (QtoCoolSPRemainAdjust < 0.0) {
                QRALoad = QtoCoolSPRemainAdjust;
            } else if (QtoHeatSPRemainAdjust > 0.0) {
                QRALoad = QtoHeatSPRemainAdjust;
            } else {
                QRALoad = 0.0;
            }

            //  IF (QTotLoadRemain == 0.0d0) THEN  ! floating in deadband
            //    IF ((QTotRemainAdjust < 0.0d0) .AND. (QtoCoolSPRemainAdjust < 0.0d0)) THEN !really need cooling
            //      QRALoad = QtoCoolSPRemainAdjust
            //    ELSEIF ((QTotRemainAdjust > 0.0d0) .AND. (QtoHeatSPRemainAdjust > 0.0d0)) THEN ! really need heating
            //      QRALoad = QtoHeatSPRemainAdjust
            //    ELSE
            //      QRALoad = 0.0 ! still floating in deadband even with impact of OA side
            //    ENDIF
            //  ELSE
            //    QRALoad = QTotRemainAdjust
            //  ENDIF

            if (QRALoad < 0.0) {                                                                 // cooling
                if ((this->dd_airterminalRecircAirInlet.AirTemp - Node(ZoneNodeNum).Temp) < -0.5) { // can cool
                    //  Find the Mass Flow Rate of the RA Stream needed to meet the zone cooling load
                    if (std::abs((CpAirSysRA * this->dd_airterminalRecircAirInlet.AirTemp) - (CpAirZn * Node(ZoneNodeNum).Temp)) / CpAirZn >
                        SmallTempDiff) {
                        this->dd_airterminalRecircAirInlet.AirMassFlowRate =
                            QRALoad / (CpAirSysRA * this->dd_airterminalRecircAirInlet.AirTemp - CpAirZn * Node(ZoneNodeNum).Temp);
                    }
                } else {
                    this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
                }

            } else if (QRALoad > 0.0) { // heating
                //    IF ((dd_airterminalRecircAirInlet(DDNum)%AirTemp - Node(ZoneNodeNum)%Temp) > 2.0d0)  THEN ! can heat
                //      dd_airterminalRecircAirInlet(DDNum)%AirMassFlowRate = QRALoad / &
                //                         (CpAirSysRA*dd_airterminalRecircAirInlet(DDNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
                //    ELSE
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
                //    ENDIF

            } else { // none needed.
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            }

            // Need to make sure that the RA flows are within limits
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRate > this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail;
                // These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
            } else if (this->dd_airterminalRecircAirInlet.AirMassFlowRate < 0.0) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            }

        } else {
            this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail = 0.0;
        } // recirc used

        // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
        // equipment iteration. If detected, set flow rate to previous value.
        if (((std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2) <
              this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag) ||
             (std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3) <
              this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag)) &&
            (std::abs(this->dd_airterminalRecircAirInlet.AirMassFlowRate - this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1) >=
             this->dd_airterminalRecircAirInlet.AirMassFlowDiffMag)) {
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRate > 0.0) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1;
            }
        }

        // Find the Max Box Flow Rate.
        MassFlowMax = this->dd_airterminalOAInlet.AirMassFlowRateMaxAvail + this->dd_airterminalRecircAirInlet.AirMassFlowRateMaxAvail;
        if (GetCurrentScheduleValue( this->SchedPtr) > 0.0) {
            TotMassFlow = this->dd_airterminalOAInlet.AirMassFlowRate + this->dd_airterminalRecircAirInlet.AirMassFlowRate;
        } else {
            TotMassFlow = 0.0;
        }

        if (TotMassFlow > SmallMassFlow) {

            // If the sum of the two air streams' flow is greater than the Max Box Flow Rate then reset the RA Stream
            if (TotMassFlow > MassFlowMax) {
                this->dd_airterminalRecircAirInlet.AirMassFlowRate = MassFlowMax - this->dd_airterminalOAInlet.AirMassFlowRate;
            }
            // After the flow rates are determined the properties are calculated.
            TotMassFlow = this->dd_airterminalOAInlet.AirMassFlowRate + this->dd_airterminalRecircAirInlet.AirMassFlowRate;
            if (TotMassFlow > SmallMassFlow) {
                HumRat = (this->dd_airterminalOAInlet.AirHumRat * this->dd_airterminalOAInlet.AirMassFlowRate +
                          this->dd_airterminalRecircAirInlet.AirHumRat * this->dd_airterminalRecircAirInlet.AirMassFlowRate) /
                         TotMassFlow;
                Enthalpy = (this->dd_airterminalOAInlet.AirEnthalpy * this->dd_airterminalOAInlet.AirMassFlowRate +
                            this->dd_airterminalRecircAirInlet.AirEnthalpy * this->dd_airterminalRecircAirInlet.AirMassFlowRate) /
                           TotMassFlow;
            } else {
                HumRat = (this->dd_airterminalRecircAirInlet.AirHumRat + this->dd_airterminalOAInlet.AirHumRat) / 2.0;
                Enthalpy = (this->dd_airterminalRecircAirInlet.AirEnthalpy + this->dd_airterminalOAInlet.AirEnthalpy) / 2.0;
            }
        } else {

            // The Max Box Flow Rate is zero and the box is off.
            this->dd_airterminalRecircAirInlet.AirMassFlowRate = 0.0;
            this->dd_airterminalOAInlet.AirMassFlowRate = 0.0;
            HumRat = (this->dd_airterminalRecircAirInlet.AirHumRat + this->dd_airterminalOAInlet.AirHumRat) / 2.0;
            Enthalpy = (this->dd_airterminalRecircAirInlet.AirEnthalpy + this->dd_airterminalOAInlet.AirEnthalpy) / 2.0;
        }

        Temperature = PsyTdbFnHW(Enthalpy, HumRat);

        this->dd_airterminalOutlet.AirTemp = Temperature;
        this->dd_airterminalOutlet.AirHumRat = HumRat;
        this->dd_airterminalOutlet.AirMassFlowRate = TotMassFlow;
        this->dd_airterminalOutlet.AirMassFlowRateMaxAvail = MassFlowMax;
        this->dd_airterminalOutlet.AirEnthalpy = Enthalpy;

        // Calculate the OA and RA damper position in %
        if ( this->RecircIsUsed) {
            if (this->dd_airterminalRecircAirInlet.AirMassFlowRateMax == 0.0) { // protect div by zero
                this->RecircAirDamperPosition = 0.0;
            } else {
                this->RecircAirDamperPosition =
                    this->dd_airterminalRecircAirInlet.AirMassFlowRate / this->dd_airterminalRecircAirInlet.AirMassFlowRateMax;
            }
        }

        if (this->dd_airterminalOAInlet.AirMassFlowRateMax == 0.0) { // protect div by zero
            this->OADamperPosition = 0.0;
        } else {
            this->OADamperPosition = this->dd_airterminalOAInlet.AirMassFlowRate / this->dd_airterminalOAInlet.AirMassFlowRateMax;
        }

        // Calculate OAFraction of mixed air after the box
        if (TotMassFlow > 0) {
            if ( this->RecircIsUsed) {
                if (this->dd_airterminalOAInlet.AirMassFlowRate == 0.0) {
                    this->OAFraction = 0.0;
                } else if (this->dd_airterminalRecircAirInlet.AirMassFlowRate == 0.0) {
                    this->OAFraction = 1.0;
                } else {
                    this->OAFraction = this->dd_airterminalOAInlet.AirMassFlowRate / TotMassFlow;
                }
            } else {
                this->OAFraction = 1.0;
            }
        } else {
            this->OAFraction = 0.0;
        }

        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist3 = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2;
        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist2 = this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1;
        this->dd_airterminalRecircAirInlet.AirMassFlowRateHist1 = this->dd_airterminalRecircAirInlet.AirMassFlowRate;
    }


    void DualDuctAirTerminal::CalcOAMassFlow(Real64 &SAMassFlow,   // outside air based on optional user input
                        Real64 &AirLoopOAFrac // outside air based on optional user input
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad (FSEC)
        //       DATE WRITTEN   Mar 2010
        //       MODIFIED       Mangesh Basarkar, 06/2011: Modifying outside air based on airloop DCV flag
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the amount of outside air required based on optional user input.
        // Zone multipliers are included and are applied in GetInput.

        // METHODOLOGY EMPLOYED:
        // User input defines method used to calculate OA.

        using DataAirLoop::AirLoopControlInfo;
        using DataAirLoop::AirLoopFlow;
        using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
        using DataZoneEquipment::ZoneEquipConfig;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.

        Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
        Real64 OAMassFlow;       // outside air mass flow rate (kg/s)

        // initialize OA flow rate and OA report variable
        SAMassFlow = 0.0;
        AirLoopOAFrac = 0.0;
        int AirLoopNum = this->AirLoopNum;

        // Calculate the amount of OA based on optional user inputs
        if (AirLoopNum > 0) {
            AirLoopOAFrac = AirLoopFlow(AirLoopNum).OAFrac;
            // If no additional input from user, RETURN from subroutine
            if ( this->NoOAFlowInputFromUser) return;
            // Calculate outdoor air flow rate, zone multipliers are applied in GetInput
            if (AirLoopOAFrac > 0.0) {
                OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( this->OARequirementsPtr,
                                                                     this->ActualZoneNum,
                                                                     AirLoopControlInfo(AirLoopNum).AirLoopDCVFlag,
                                                                     UseMinOASchFlag);
                OAMassFlow = OAVolumeFlowRate * StdRhoAir;

                // convert OA mass flow rate to supply air flow rate based on air loop OA fraction
                SAMassFlow = OAMassFlow / AirLoopOAFrac;
            }
        }
    }

    void DualDuctAirTerminal::CalcOAOnlyMassFlow(Real64 &OAMassFlow,           // outside air flow from user input kg/s
                            Optional<Real64> MaxOAVolFlow // design level for outside air m3/s
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         C. Miller (Mod of CaclOAMassFlow by R. Raustad (FSEC))
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       B. Griffith, Dec 2010 clean up, sizing optional, scheduled OA
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the amount of outside air required based on optional user input. Returns
        // ONLY calculated OAMassFlow without consideration of AirLoopOAFrac. Used for
        // the DualDuct:VAV:OutdoorAir object which does not mix OA with RA

        // METHODOLOGY EMPLOYED:
        // User input defines method used to calculate OA.

        // REFERENCES:

        // Using/Aliasing
        using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.
        static std::string const RoutineName("HVACDualDuctSystem:CalcOAOnlyMassFlow");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

        Real64 OAVolumeFlowRate; // outside air volume flow rate (m3/s)
        bool UseOccSchFlag;      // TRUE = use actual occupancy, FALSE = use total zone people
        bool PerPersonNotSet;

        // Calculate the amount of OA based on optional user inputs
        OAMassFlow = 0.0;

        // If no additional input from user, RETURN from subroutine
        if ( this->NoOAFlowInputFromUser) {
            ShowSevereError("CalcOAOnlyMassFlow: Problem in AirTerminal:DualDuct:VAV:OutdoorAir = " + this->Name +
                            ", check outdoor air specification");
            if (present(MaxOAVolFlow)) MaxOAVolFlow = 0.0;
            return;
        }

        if ( this->OAPerPersonMode == PerPersonDCVByCurrentLevel) {
            UseOccSchFlag = true;
            PerPersonNotSet = false;
        } else {
            UseOccSchFlag = false;
            PerPersonNotSet = false;
            if ( this->OAPerPersonMode == PerPersonModeNotSet) PerPersonNotSet = true;
        }

        OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(
            this->OARequirementsPtr, this->ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, PerPersonNotSet);

        OAMassFlow = OAVolumeFlowRate * StdRhoAir;

        if (present(MaxOAVolFlow)) {
            OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(
                this->OARequirementsPtr, this->ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, _, true);
            MaxOAVolFlow = OAVolumeFlowRate;
        }
    }

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the Damper Module
    // *****************************************************************************

    void DualDuctAirTerminal::UpdateDualDuct()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   February 2000
        //       MODIFIED       Aug 2010 Clayton Miller - Added DualDuctVAVOutdoorAir
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the dampers.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataContaminantBalance::Contaminant;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OutletNode;
        int HotInletNode;
        int ColdInletNode;
        int OAInletNode; // Outdoor Air Duct Inlet Node - for DualDuctOutdoorAir
        int RAInletNode; // Recirculated Air Duct Inlet Node - for DualDuctOutdoorAir

        if ( this->DamperType == DualDuct_ConstantVolume || this->DamperType == DualDuct_VariableVolume) {

            OutletNode = this->OutletNodeNum;
            HotInletNode = this->HotAirInletNodeNum;
            ColdInletNode = this->ColdAirInletNodeNum;

            // Set the outlet air nodes of the Damper
            Node(HotInletNode).MassFlowRate = this->dd_airterminalHotAirInlet.AirMassFlowRate;
            Node(ColdInletNode).MassFlowRate = this->dd_airterminalColdAirInlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRate = this->dd_airterminalOutlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRateMaxAvail = this->dd_airterminalOutlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRateMinAvail = this->dd_airterminalOutlet.AirMassFlowRateMinAvail;
            Node(OutletNode).Temp = this->dd_airterminalOutlet.AirTemp;
            Node(OutletNode).HumRat = this->dd_airterminalOutlet.AirHumRat;
            Node(OutletNode).Enthalpy = this->dd_airterminalOutlet.AirEnthalpy;
            // Set the outlet nodes for properties that just pass through & not used
            // FIX THIS LATER!!!!
            Node(OutletNode).Quality = Node(HotInletNode).Quality;
            Node(OutletNode).Press = Node(HotInletNode).Press;

            if (Contaminant.CO2Simulation) {
                if (Node(OutletNode).MassFlowRate > 0.0) {
                    Node(OutletNode).CO2 =
                        (Node(HotInletNode).CO2 * Node(HotInletNode).MassFlowRate + Node(ColdInletNode).CO2 * Node(ColdInletNode).MassFlowRate) /
                        Node(OutletNode).MassFlowRate;
                } else {
                    Node(OutletNode).CO2 = max(Node(HotInletNode).CO2, Node(ColdInletNode).CO2);
                }
            }
            if (Contaminant.GenericContamSimulation) {
                if (Node(OutletNode).MassFlowRate > 0.0) {
                    Node(OutletNode).GenContam = (Node(HotInletNode).GenContam * Node(HotInletNode).MassFlowRate +
                                                  Node(ColdInletNode).GenContam * Node(ColdInletNode).MassFlowRate) /
                                                 Node(OutletNode).MassFlowRate;
                } else {
                    Node(OutletNode).GenContam = max(Node(HotInletNode).GenContam, Node(ColdInletNode).GenContam);
                }
            }

            this->CalcOutdoorAirVolumeFlowRate();

        } else if ( this->DamperType == DualDuct_OutdoorAir) {

            OutletNode = this->OutletNodeNum;
            OAInletNode = this->OAInletNodeNum;
            if ( this->RecircIsUsed) {
                RAInletNode = this->RecircAirInletNodeNum;
                Node(RAInletNode).MassFlowRate = this->dd_airterminalRecircAirInlet.AirMassFlowRate;
            }
            // Set the outlet air nodes of the Damper
            Node(OAInletNode).MassFlowRate = this->dd_airterminalOAInlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRate = this->dd_airterminalOutlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRateMaxAvail = this->dd_airterminalOutlet.AirMassFlowRate;
            Node(OutletNode).MassFlowRateMinAvail = this->dd_airterminalOutlet.AirMassFlowRateMinAvail;
            Node(OutletNode).Temp = this->dd_airterminalOutlet.AirTemp;
            Node(OutletNode).HumRat = this->dd_airterminalOutlet.AirHumRat;
            Node(OutletNode).Enthalpy = this->dd_airterminalOutlet.AirEnthalpy;
            // Set the outlet nodes for properties that just pass through & not used
            // FIX THIS LATER!!!!
            Node(OutletNode).Quality = Node(OAInletNode).Quality;
            Node(OutletNode).Press = Node(OAInletNode).Press;

            if ( this->RecircIsUsed) {
                if (Node(OutletNode).MassFlowRate > 0.0) {
                    if (Contaminant.CO2Simulation) {
                        Node(OutletNode).CO2 =
                            (Node(OAInletNode).CO2 * Node(OAInletNode).MassFlowRate + Node(RAInletNode).CO2 * Node(RAInletNode).MassFlowRate) /
                            Node(OutletNode).MassFlowRate;
                    }
                    if (Contaminant.GenericContamSimulation) {
                        Node(OutletNode).GenContam = (Node(OAInletNode).GenContam * Node(OAInletNode).MassFlowRate +
                                                      Node(RAInletNode).GenContam * Node(RAInletNode).MassFlowRate) /
                                                     Node(OutletNode).MassFlowRate;
                    }
                } else {
                    if (Contaminant.CO2Simulation) {
                        Node(OutletNode).CO2 = max(Node(OAInletNode).CO2, Node(RAInletNode).CO2);
                    }
                    if (Contaminant.GenericContamSimulation) {
                        Node(OutletNode).GenContam = max(Node(OAInletNode).GenContam, Node(RAInletNode).GenContam);
                    }
                }

            } else {
                if (Contaminant.CO2Simulation) {
                    Node(OutletNode).CO2 = Node(OAInletNode).CO2;
                }
                if (Contaminant.GenericContamSimulation) {
                    Node(OutletNode).GenContam = Node(OAInletNode).GenContam;
                }
            }
        }
    }

    //        End of Update subroutines for the Damper Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the Damper Module
    // *****************************************************************************

    void DualDuctAirTerminal::ReportDualDuct() // unused1208
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Unknown
        //       DATE WRITTEN   Unknown
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the damper report variables.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        // Still needs to report the Damper power from this component
    }

    void ReportDualDuctConnections(OutputFiles &outputFiles)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Michael J. Witte
        //       DATE WRITTEN   February 2004
        //       MODIFIED       B. Griffith, DOAS VAV dual duct
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Report dual duct damper connections to the BND file.

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataAirLoop::AirToZoneNodeInfo;
        using DataHVACGlobals::NumPrimaryAirSys;
        using DataZoneEquipment::NumSupplyAirPaths;
        using DataZoneEquipment::SupplyAirPath;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // Formats
        static constexpr auto Format_100("! <#Dual Duct Damper Connections>,<Number of Dual Duct Damper Connections>");
        static constexpr auto Format_102("! <Dual Duct Damper>,<Dual Duct Damper Count>,<Dual Duct Damper Name>,<Inlet Node>,<Outlet Node>,<Inlet "
                                         "Node Type>,<AirLoopHVAC Name>");

        if (!allocated(dd_airterminal))
            return; // Autodesk Bug: Can arrive here with Damper unallocated (SimulateDualDuct not yet called) with NumDDAirTerminal either set >0 or
                    // uninitialized

        // Report Dual Duct Dampers to BND File
        print(outputFiles.bnd, "{}\n", "! ===============================================================");
        print(outputFiles.bnd, "{}\n", Format_100);
        print(outputFiles.bnd, " #Dual Duct Damper Connections,{}\n", NumDDAirTerminal * 2);
        print(outputFiles.bnd, "{}\n", Format_102);

        for (int Count1 = 1; Count1 <= NumDDAirTerminal; ++Count1) {

            // Determine if this damper is connected to a supply air path
            int Found = 0;
            int SupplyAirPathNum = 0;
            for (int Count2 = 1; Count2 <= NumSupplyAirPaths; ++Count2) {
                SupplyAirPathNum = Count2;
                Found = 0;
                for (int Count3 = 1; Count3 <= SupplyAirPath(Count2).NumOutletNodes; ++Count3) {
                    if (dd_airterminal(Count1).HotAirInletNodeNum == SupplyAirPath(Count2).OutletNode(Count3)) Found = Count3;
                    if (dd_airterminal(Count1).ColdAirInletNodeNum == SupplyAirPath(Count2).OutletNode(Count3)) Found = Count3;
                    if (dd_airterminal(Count1).OAInletNodeNum == SupplyAirPath(Count2).OutletNode(Count3)) Found = Count3;
                    if (dd_airterminal(Count1).RecircAirInletNodeNum == SupplyAirPath(Count2).OutletNode(Count3)) Found = Count3;
                }
                if (Found != 0) break;
            }
            if (Found == 0) SupplyAirPathNum = 0;

            // Determine which air loop this dual duct damper is connected to
            Found = 0;
            std::string ChrName;
            for (int Count2 = 1; Count2 <= NumPrimaryAirSys; ++Count2) {
                ChrName = AirToZoneNodeInfo(Count2).AirLoopName;
                Found = 0;
                for (int Count3 = 1; Count3 <= AirToZoneNodeInfo(Count2).NumSupplyNodes; ++Count3) {
                    if (SupplyAirPathNum != 0) {
                        if (SupplyAirPath(SupplyAirPathNum).InletNodeNum == AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3)) Found = Count3;
                    } else {
                        if (dd_airterminal(Count1).HotAirInletNodeNum == AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3)) Found = Count3;
                        if (dd_airterminal(Count1).ColdAirInletNodeNum == AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3)) Found = Count3;
                        if (dd_airterminal(Count1).OAInletNodeNum == AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3)) Found = Count3;
                        if (dd_airterminal(Count1).RecircAirInletNodeNum == AirToZoneNodeInfo(Count2).ZoneEquipSupplyNodeNum(Count3)) Found = Count3;
                    }
                }
                if (Found != 0) break;
            }
            if (Found == 0) ChrName = "**Unknown**";

            std::string DamperType;
            if (dd_airterminal(Count1).DamperType == DualDuct_ConstantVolume) {
                DamperType = cCMO_DDConstantVolume;
            } else if (dd_airterminal(Count1).DamperType == DualDuct_VariableVolume) {
                DamperType = cCMO_DDVariableVolume;
            } else if (dd_airterminal(Count1).DamperType == DualDuct_OutdoorAir) {
                DamperType = cCMO_DDVarVolOA;
            } else {
                DamperType = "Invalid/Unknown";
            }

            if ((dd_airterminal(Count1).DamperType == DualDuct_ConstantVolume) || (dd_airterminal(Count1).DamperType == DualDuct_VariableVolume)) {
                print(outputFiles.bnd,
                      " Dual Duct Damper,{},{},{},{},{},Hot Air,{}\n",
                      Count1,
                      DamperType,
                      dd_airterminal(Count1).Name,
                      NodeID(dd_airterminal(Count1).HotAirInletNodeNum),
                      NodeID(dd_airterminal(Count1).OutletNodeNum),
                      ChrName);

                print(outputFiles.bnd,
                      " Dual Duct Damper,{},{},{},{},{},Cold Air,{}\n",
                      Count1,
                      DamperType,
                      dd_airterminal(Count1).Name,
                      NodeID(dd_airterminal(Count1).ColdAirInletNodeNum),
                      NodeID(dd_airterminal(Count1).OutletNodeNum),
                      ChrName);

            } else if (dd_airterminal(Count1).DamperType == DualDuct_OutdoorAir) {
                print(outputFiles.bnd,
                      "Dual Duct Damper, {},{},{},{},{},Outdoor Air,{}\n",
                      Count1,
                      DamperType,
                      dd_airterminal(Count1).Name,
                      NodeID(dd_airterminal(Count1).OAInletNodeNum),
                      NodeID(dd_airterminal(Count1).OutletNodeNum),
                      ChrName);
                print(outputFiles.bnd,
                      "Dual Duct Damper, {},{},{},{},{},Recirculated Air,{}\n",
                      Count1,
                      DamperType,
                      dd_airterminal(Count1).Name,
                      NodeID(dd_airterminal(Count1).RecircAirInletNodeNum),
                      NodeID(dd_airterminal(Count1).OutletNodeNum),
                      ChrName);
            }
        }
    }

    void GetDualDuctOutdoorAirRecircUse(std::string const &EP_UNUSED(CompTypeName), std::string const &CompName, bool &RecircIsUsed)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith
        //       DATE WRITTEN   Aug 2011
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // get routine to learn if a dual duct outdoor air unit is using its recirc deck

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D_bool RecircIsUsedARR;
        static Array1D_string DamperNamesARR;
        int DamperIndex;                 // Loop index to Damper that you are currently loading input into
        std::string CurrentModuleObject; // for ease in getting objects
        static Array1D<Real64> NumArray(2, 0.0);
        static Array1D_string AlphArray(7);
        static Array1D_string cAlphaFields(7);       // Alpha field names
        static Array1D_string cNumericFields(2);     // Numeric field names
        static Array1D_bool lAlphaBlanks(7, true);   // Logical array, alpha field input BLANK = .TRUE.
        static Array1D_bool lNumericBlanks(2, true); // Logical array, numeric field input BLANK = .TRUE.
        int NumAlphas;
        int NumNums;
        int IOStat;

        RecircIsUsed = true;

        // this doesn't work because it fires code that depends on things being further along
        //  IF (GetDualDuctInputFlag) THEN  !First time subroutine has been entered
        //    CALL GetDualDuctInput
        //    GetDualDuctInputFlag=.FALSE.
        //  END IF

        if (GetDualDuctOutdoorAirRecircUseFirstTimeOnly) {
            NumDualDuctVarVolOA = inputProcessor->getNumObjectsFound(cCMO_DDVarVolOA);
            RecircIsUsedARR.allocate(NumDualDuctVarVolOA);
            DamperNamesARR.allocate(NumDualDuctVarVolOA);
            if (NumDualDuctVarVolOA > 0) {
                for (DamperIndex = 1; DamperIndex <= NumDualDuctVarVolOA; ++DamperIndex) {

                    CurrentModuleObject = cCMO_DDVarVolOA;

                    inputProcessor->getObjectItem(CurrentModuleObject,
                                                  DamperIndex,
                                                  AlphArray,
                                                  NumAlphas,
                                                  NumArray,
                                                  NumNums,
                                                  IOStat,
                                                  lNumericBlanks,
                                                  lAlphaBlanks,
                                                  cAlphaFields,
                                                  cNumericFields);
                    DamperNamesARR(DamperIndex) = AlphArray(1);
                    if (!lAlphaBlanks(5)) {
                        RecircIsUsedARR(DamperIndex) = true;
                    } else {
                        RecircIsUsedARR(DamperIndex) = false;
                    }
                }
            }
            GetDualDuctOutdoorAirRecircUseFirstTimeOnly = false;
        }

        DamperIndex = UtilityRoutines::FindItemInList(CompName, DamperNamesARR, NumDualDuctVarVolOA);
        if (DamperIndex > 0) {
            RecircIsUsed = RecircIsUsedARR(DamperIndex);
        }
    }

    void DualDuctAirTerminal::CalcOutdoorAirVolumeFlowRate()
    {
        // calculates zone outdoor air volume flow rate using the supply air flow rate and OA fraction
        if (this->AirLoopNum > 0) {
            this->OutdoorAirFlowRate = (this->dd_airterminalOutlet.AirMassFlowRate / StdRhoAir) * DataAirLoop::AirLoopFlow(this->AirLoopNum).OAFrac;
        } else {
            // do nothing for now
        }
    }

    //        End of Reporting subroutines for the Damper Module
    // *****************************************************************************

} // namespace DualDuct

} // namespace EnergyPlus
