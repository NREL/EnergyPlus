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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DirectAirManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataZoneEquipment {

    // MODULE INFORMATION
    //             AUTHOR:  Russ Taylor
    //       DATE WRITTEN:  June 1998

    // PURPOSE OF THIS MODULE:
    // This module contains variable declarations for zone equipment configuration data

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using DataGlobals::NumOfZones;

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    int const PathInlet(1);
    int const CompInlet(2);
    int const Intermediate(3);
    int const Outlet(4);

    int const ZoneSplitter_Type(1);
    int const ZoneSupplyPlenum_Type(2);
    int const ZoneMixer_Type(3);
    int const ZoneReturnPlenum_Type(4);

    // Start zone equip objects
    // list units that are valid for zone system availability managers first
    int const FanCoil4Pipe_Num(1);
    int const PkgTermHPAirToAir_Num(2);
    int const PkgTermACAirToAir_Num(3);
    int const PkgTermHPWaterToAir_Num(4);
    int const WindowAC_Num(5);
    int const UnitHeater_Num(6);
    int const UnitVentilator_Num(7);
    int const ERVStandAlone_Num(8);
    int const VentilatedSlab_Num(9);
    int const OutdoorAirUnit_Num(10);
    int const VRFTerminalUnit_Num(11);
    int const PurchasedAir_Num(12);
    int const ZoneEvaporativeCoolerUnit_Num(13);
    int const ZoneHybridEvaporativeCooler_Num(14);
    int const AirDistUnit_Num(15);
    int const DirectAir_Num(16);
    int const BBWaterConvective_Num(17);
    int const BBElectricConvective_Num(18);
    int const HiTempRadiant_Num(19);
    int const LoTempRadiant_Num(20);
    int const ZoneExhaustFan_Num(21);
    int const HeatXchngr_Num(22);
    int const HPWaterHeater_Num(23);
    int const BBWater_Num(24);
    int const ZoneDXDehumidifier_Num(25);
    int const BBSteam_Num(26);
    int const BBElectric_Num(27);
    int const RefrigerationAirChillerSet_Num(28);
    int const UserDefinedZoneHVACForcedAir_Num(29);
    int const CoolingPanel_Num(30);
    int const ZoneUnitarySys_Num(31);
    int const TotalNumZoneEquipType(31);
    // **NOTE**... if you add another zone equipment object, then increment
    // TotalNumZoneEquipType above to match the total number of zone equipment types
    // End zone equip objects

    int const NumValidSysAvailZoneComponents(14);
    Array1D_string const cValidSysAvailManagerCompTypes(NumValidSysAvailZoneComponents,
                                                        {"ZoneHVAC:FourPipeFanCoil",
                                                         "ZoneHVAC:PackagedTerminalHeatPump",
                                                         "ZoneHVAC:PackagedTerminalAirConditioner",
                                                         "ZoneHVAC:WaterToAirHeatPump",
                                                         "ZoneHVAC:WindowAirConditioner",
                                                         "ZoneHVAC:UnitHeater",
                                                         "ZoneHVAC:UnitVentilator",
                                                         "ZoneHVAC:EnergyRecoveryVentilator",
                                                         "ZoneHVAC:VentilatedSlab",
                                                         "ZoneHVAC:OutdoorAirUnit",
                                                         "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
                                                         "ZoneHVAC:IdealLoadsAirSystem",
                                                         "ZoneHVAC:EvaporativeCoolerUnit",
                                                         "ZoneHVAC:HybridUnitaryHVAC"});

    // Per Person Ventilation Rate Mode
    int const PerPersonDCVByCurrentLevel(1);
    int const PerPersonByDesignLevel(2);

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:

    namespace {
        bool GetZoneEquipmentDataErrorsFound(false);
        int GetZoneEquipmentDataFound(0);
    } // namespace

    int NumSupplyAirPaths(0);
    int NumReturnAirPaths(0);
    bool ZoneEquipInputsFilled(false);
    bool ZoneEquipSimulatedOnce(false);
    int NumOfZoneEquipLists(0); // The Number of Zone Equipment List objects
    Array1D_int ZoneEquipAvail;

    Array1D_bool CrossMixingReportFlag;
    Array1D_bool MixingReportFlag;
    Array1D<Real64> VentMCP;
    Array1D<Real64> ZMAT;
    Array1D<Real64> ZHumRat;

    // Utility routines for module

    // Object Data
    Array1D<EquipConfiguration> ZoneEquipConfig;
    std::unordered_set<std::string> UniqueZoneEquipListNames;
    Array1D<EquipList> ZoneEquipList;
    Array1D<ControlList> HeatingControlList;
    Array1D<ControlList> CoolingControlList;
    Array1D<SupplyAir> SupplyAirPath;
    Array1D<ReturnAir> ReturnAirPath;

    // Functions
    // Clears the global data in DataZoneEquipment.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        NumSupplyAirPaths = 0;
        NumReturnAirPaths = 0;
        ZoneEquipInputsFilled = false;
        ZoneEquipSimulatedOnce = false;
        NumOfZoneEquipLists = 0; // The Number of Zone Equipment List objects
        GetZoneEquipmentDataErrorsFound = false;
        GetZoneEquipmentDataFound = 0;
        ZoneEquipAvail.deallocate();
        CrossMixingReportFlag.deallocate();
        MixingReportFlag.deallocate();
        VentMCP.deallocate();
        ZMAT.deallocate();
        ZHumRat.deallocate();
        ZoneEquipConfig.deallocate();
        ZoneEquipList.deallocate();
        HeatingControlList.deallocate();
        CoolingControlList.deallocate();
        SupplyAirPath.deallocate();
        ReturnAirPath.deallocate();
        UniqueZoneEquipListNames.clear();
    }

    void GetZoneEquipmentData()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is a stub routine to allow an outside module (ZoneEquipmentManager) to get input while
        // allowing the routine itself to remain PRIVATE to this module.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        GetZoneEquipmentData1();
    }

    void GetZoneEquipmentData1()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   June 1997
        //       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get all the system related equipment which may be attached to
        // a zone

        // Using/Aliasing
        using DataHeatBalance::Zone;
        using NodeInputManager::CheckUniqueNodes;
        using NodeInputManager::EndUniqueNodeCheck;
        using NodeInputManager::GetNodeNums;
        using NodeInputManager::GetOnlySingleNode;
        using NodeInputManager::InitUniqueNodeCheck;
        using namespace DataHVACGlobals;
        using BranchNodeConnections::SetUpCompSets;
        using namespace DataLoopNode;
        using DataGlobals::NumOfZones;
        using DataGlobals::ScheduleAlwaysOn;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using namespace ScheduleManager;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneEquipmentData1: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;
        int NumNums;
        int NodeNum;
        int PathNum;
        int CompNum;
        int ControlledZoneNum;
        int ControlledZoneLoop;
        int ZoneEquipTypeNum;
        int ZoneEquipListNum;
        int IOStat;
        std::string InletNodeListName;
        std::string ExhaustNodeListName;
        std::string ReturnNodeListName;
        std::string ReturnFlowBasisNodeListName;
        Array1D_string AlphArray;
        Array1D<Real64> NumArray;
        int MaxAlphas;
        int MaxNums;
        int NumParams;
        int NumNodes;
        Array1D_int NodeNums;
        int Counter;
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool ErrorsFound( false ); // If errors detected in input // GetZoneEquipmentDataErrorsFound
        // static int found( 0 );
        ////////////////////////////////////////////////////////////////////////////////////
        bool IsNotOK; // Flag to verify nam
        bool NodeListError;
        bool UniqueNodeError;
        int NumOfControlledZones;        // The number of Controlled Zone Equip Configuration objects
        std::string CurrentModuleObject; // Object type for getting and error messages
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        int maxEquipCount;
        int numEquipCount;
        int overallEquipCount;
        int Loop1;
        int Loop2;

        struct EquipListAudit
        {
            // Members
            std::string ObjectType;
            std::string ObjectName;
            int OnListNum;

            // Default Constructor
            EquipListAudit() : OnListNum(0)
            {
            }
        };

        // Object Data
        Array1D<EquipListAudit> ZoneEquipListAcct;

        ExhaustNodeListName = "";
        InletNodeListName = "";
        ReturnNodeListName = "";
        ReturnFlowBasisNodeListName = "";

        // Look in the input file for zones with air loop and zone equipment attached

        NumOfControlledZones = inputProcessor->getNumObjectsFound("ZoneHVAC:EquipmentConnections");
        NumOfZoneEquipLists = inputProcessor->getNumObjectsFound("ZoneHVAC:EquipmentList"); // Look for lists of equipment data - there should
        // be as many of these as there are controlled zones
        inputProcessor->getObjectDefMaxArgs("NodeList", NumParams, NumAlphas, NumNums);
        NodeNums.dimension(NumParams, 0);
        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:EquipmentList", NumParams, NumAlphas, NumNums);
        MaxAlphas = NumAlphas;
        MaxNums = NumNums;
        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:EquipmentConnections", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        inputProcessor->getObjectDefMaxArgs("AirLoopHVAC:SupplyPath", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        inputProcessor->getObjectDefMaxArgs("AirLoopHVAC:ReturnPath", NumParams, NumAlphas, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNums = max(MaxNums, NumNums);
        AlphArray.allocate(MaxAlphas);
        NumArray.dimension(MaxNums, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        if (!allocated(SupplyAirPath)) {
            // Look for and read in the air supply path
            // component (splitters) information for each zone
            NumSupplyAirPaths = inputProcessor->getNumObjectsFound("AirLoopHVAC:SupplyPath");
            SupplyAirPath.allocate(NumSupplyAirPaths);
        }

        if (!allocated(ReturnAirPath)) {
            // Look for and read in the air return path
            // component (mixers & plenums) information for each zone
            NumReturnAirPaths = inputProcessor->getNumObjectsFound("AirLoopHVAC:ReturnPath");
            ReturnAirPath.allocate(NumReturnAirPaths);
        }

        ZoneEquipConfig.allocate(NumOfZones); // Allocate the array containing the configuration
        // data for each zone to the number of controlled zones
        // found in the input file.  This may or may not
        // be the same as the number of zones in the building
        ZoneEquipList.allocate(NumOfZones);
        ZoneEquipAvail.dimension(NumOfZones, NoAction);
        UniqueZoneEquipListNames.reserve(NumOfZones);

        if (NumOfZoneEquipLists != NumOfControlledZones) {
            ShowSevereError(RoutineName + "Number of Zone Equipment lists [" + TrimSigDigits(NumOfZoneEquipLists) +
                            "] not equal Number of Controlled Zones [" + TrimSigDigits(NumOfControlledZones) + ']');
            ShowContinueError("..Each Controlled Zone [ZoneHVAC:EquipmentConnections] must have a corresponding (unique) ZoneHVAC:EquipmentList");
            ShowFatalError("GetZoneEquipment: Incorrect number of zone equipment lists");
        }

        if (NumOfControlledZones > NumOfZones) {
            ShowSevereError(RoutineName + "Number of Controlled Zone objects [" + TrimSigDigits(NumOfControlledZones) +
                            "] greater than Number of Zones [" + TrimSigDigits(NumOfZones) + ']');
            ShowFatalError(RoutineName + "Too many ZoneHVAC:EquipmentConnections objects.");
        }

        InitUniqueNodeCheck("ZoneHVAC:EquipmentConnections");

        overallEquipCount = 0;
        int locTermUnitSizingCounter = 0; // will increment for every zone inlet node

        for (ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfControlledZones; ++ControlledZoneLoop) {

            CurrentModuleObject = "ZoneHVAC:EquipmentConnections";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          ControlledZoneLoop,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields); // Get Equipment | data for one zone

            ControlledZoneNum = UtilityRoutines::FindItemInList(AlphArray(1), Zone);

            if (ControlledZoneNum == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\"");
                ShowContinueError("..Requested Controlled Zone not among Zones, remaining items for this object not processed.");
                GetZoneEquipmentDataErrorsFound = true;
                continue;
            } else {
                //    Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%ZoneEquipConfigNum = ControlledZoneNum
                if (Zone(ControlledZoneNum).IsControlled) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\"");
                    ShowContinueError("..Duplicate Controlled Zone entered, only one " + CurrentModuleObject + " per zone is allowed.");
                    GetZoneEquipmentDataErrorsFound = true;
                    continue;
                }
                Zone(ControlledZoneNum).IsControlled = true;
                Zone(ControlledZoneNum).ZoneEqNum = ControlledZoneNum;
                ZoneEquipConfig(ControlledZoneNum).IsControlled = true;
                ZoneEquipConfig(ControlledZoneNum).ActualZoneNum = ControlledZoneNum;
            }
            ZoneEquipConfig(ControlledZoneNum).ZoneName = AlphArray(1); // for x-referencing with the geometry data

            IsNotOK = false;
            GlobalNames::IntraObjUniquenessCheck(AlphArray(2), CurrentModuleObject, cAlphaFields(2), UniqueZoneEquipListNames, IsNotOK);
            if (IsNotOK) {
                ShowContinueError("..another Controlled Zone has been assigned that " + cAlphaFields(2) + '.');
                GetZoneEquipmentDataErrorsFound = true;
            }
            ZoneEquipConfig(ControlledZoneNum).EquipListName = AlphArray(2); // the name of the list containing all the zone eq.
            InletNodeListName = AlphArray(3);
            ExhaustNodeListName = AlphArray(4);
            ZoneEquipConfig(ControlledZoneNum).ZoneNode = GetOnlySingleNode(AlphArray(5),
                                                                            GetZoneEquipmentDataErrorsFound,
                                                                            CurrentModuleObject,
                                                                            AlphArray(1),
                                                                            NodeType_Air,
                                                                            NodeConnectionType_ZoneNode,
                                                                            1,
                                                                            ObjectIsNotParent); // all zone air state variables are
            if (ZoneEquipConfig(ControlledZoneNum).ZoneNode == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + ": " + cAlphaFields(1) + "=\"" + AlphArray(1) + "\", invalid");
                ShowContinueError(cAlphaFields(5) + " must be present.");
                GetZoneEquipmentDataErrorsFound = true;
            } else {
                UniqueNodeError = false;
                CheckUniqueNodes(cAlphaFields(5), "NodeName", UniqueNodeError, AlphArray(5), _, AlphArray(1));
                if (UniqueNodeError) {
                    // ShowContinueError( "Occurs for " + trim( cAlphaFields( 1 ) ) + " = " + trim( AlphArray( 1 ) ) );
                    GetZoneEquipmentDataErrorsFound = true;
                }
            }
            // assigned to this node
            if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum > 0) {
                Zone(ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).SystemZoneNodeNumber = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
            } // This error already detected and program will be terminated.

            ReturnNodeListName = AlphArray(6);
            if (lAlphaBlanks(7)) {
                ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum = ScheduleAlwaysOn;
            } else {
                ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum = GetScheduleIndex(AlphArray(7));
                if (ZoneEquipConfig(ControlledZoneNum).ReturnFlowSchedPtrNum == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + ": invalid " + cAlphaFields(7) + " entered =" + AlphArray(7) + " for " +
                                    cAlphaFields(1) + '=' + AlphArray(1));
                    GetZoneEquipmentDataErrorsFound = true;
                }
            }
            ReturnFlowBasisNodeListName = AlphArray(8);

            // Read in the equipment type, name and sequence information
            // for each equipment list

            CurrentModuleObject = "ZoneHVAC:EquipmentList";

            ZoneEquipListNum = inputProcessor->getObjectItemNum(CurrentModuleObject, ZoneEquipConfig(ControlledZoneNum).EquipListName);
            if (ZoneEquipListNum > 0) {

                EquipList &thisZoneEquipList = ZoneEquipList(ControlledZoneNum);

                inputProcessor->getObjectItem(CurrentModuleObject,
                                              ZoneEquipListNum,
                                              AlphArray,
                                              NumAlphas,
                                              NumArray,
                                              NumNums,
                                              IOStat,
                                              lNumericBlanks,
                                              lAlphaBlanks,
                                              cAlphaFields,
                                              cNumericFields); //  data for one zone
                UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, GetZoneEquipmentDataErrorsFound);
                thisZoneEquipList.Name = AlphArray(1);

                if (!lAlphaBlanks(2)) {
                    if (UtilityRoutines::SameString(AlphArray(2), "SequentialLoad")) {
                        thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::SequentialLoading;
                    } else if (UtilityRoutines::SameString(AlphArray(2), "UniformLoad")) {
                        thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::UniformLoading;
                    } else if (UtilityRoutines::SameString(AlphArray(2), "UniformPLR")) {
                        thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::UniformPLRLoading;
                    } else if (UtilityRoutines::SameString(AlphArray(2), "SequentialUniformPLR")) {
                        thisZoneEquipList.LoadDistScheme = DataZoneEquipment::LoadDist::SequentialUniformPLRLoading;
                    } else {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphArray(1) + "\", Invalid choice.");
                        ShowContinueError("..." + cAlphaFields(2) + "=\"" + AlphArray(2) + "\".");
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                }
                const int nAlphasInExtensible = 4;
                const int nNumsInExtensible = 2;
                const int nAlphasBeforeExtensible = 2;
                const int nNumsBeforeExtensible = 0;
                maxEquipCount = 0;
                numEquipCount = (NumAlphas - nAlphasBeforeExtensible) / nAlphasInExtensible;
                if (numEquipCount * nAlphasInExtensible != (NumAlphas - nAlphasBeforeExtensible)) ++numEquipCount;
                for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= numEquipCount; ++ZoneEquipTypeNum) {
                    if (!lAlphaBlanks(nAlphasInExtensible * (ZoneEquipTypeNum - 1) + nAlphasBeforeExtensible + 1) &&
                        !lAlphaBlanks(nAlphasInExtensible * (ZoneEquipTypeNum - 1) + nAlphasBeforeExtensible + 2)) {
                        ++maxEquipCount;
                        continue;
                    }
                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + thisZoneEquipList.Name +
                                     "\", truncated list at blank field; object count=" + RoundSigDigits(maxEquipCount));
                    break;
                }

                overallEquipCount += maxEquipCount;
                thisZoneEquipList.NumOfEquipTypes = maxEquipCount;
                thisZoneEquipList.EquipType.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.EquipType_Num.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.compPointer.resize(thisZoneEquipList.NumOfEquipTypes + 1);
                thisZoneEquipList.EquipName.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.EquipIndex.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.EquipData.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.CoolingPriority.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.HeatingPriority.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.CoolingCapacity.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.HeatingCapacity.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.SequentialCoolingFractionSchedPtr.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.SequentialHeatingFractionSchedPtr.allocate(thisZoneEquipList.NumOfEquipTypes);
                thisZoneEquipList.EquipType = "";
                thisZoneEquipList.EquipType_Num = 0;
                thisZoneEquipList.EquipName = "";
                thisZoneEquipList.EquipIndex = 0;
                thisZoneEquipList.CoolingPriority = 0;
                thisZoneEquipList.HeatingPriority = 0;
                thisZoneEquipList.CoolingCapacity = 0;
                thisZoneEquipList.HeatingCapacity = 0;
                thisZoneEquipList.SequentialCoolingFractionSchedPtr = 0;
                thisZoneEquipList.SequentialHeatingFractionSchedPtr = 0;

                for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= thisZoneEquipList.NumOfEquipTypes; ++ZoneEquipTypeNum) {
                    const int ZoneEquipTypeIdx = ZoneEquipTypeNum - 1;
                    thisZoneEquipList.EquipType(ZoneEquipTypeNum) = AlphArray(nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 1);
                    thisZoneEquipList.EquipName(ZoneEquipTypeNum) = AlphArray(nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 2);
                    ValidateComponent(thisZoneEquipList.EquipType(ZoneEquipTypeNum),
                                      thisZoneEquipList.EquipName(ZoneEquipTypeNum),
                                      IsNotOK,
                                      CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("In " + CurrentModuleObject + '=' + thisZoneEquipList.Name);
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                    thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) = nint(NumArray(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 1));
                    if ((thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) < 0) ||
                        (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > thisZoneEquipList.NumOfEquipTypes)) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ShowContinueError("invalid " + cNumericFields(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 1) + "=[" +
                                          RoundSigDigits(thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum)) + "].");
                        ShowContinueError("equipment sequence must be > 0 and <= number of equipments in the list.");
                        if (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > 0)
                            ShowContinueError("only " + RoundSigDigits(thisZoneEquipList.NumOfEquipTypes) + " in the list.");
                        GetZoneEquipmentDataErrorsFound = true;
                    }

                    thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) = nint(NumArray(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 2));
                    if ((thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) < 0) ||
                        (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > thisZoneEquipList.NumOfEquipTypes)) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                        ShowContinueError("invalid " + cNumericFields(nNumsInExtensible * ZoneEquipTypeIdx + nNumsBeforeExtensible + 2) + "=[" +
                                          RoundSigDigits(thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum)) + "].");
                        ShowContinueError("equipment sequence must be > 0 and <= number of equipments in the list.");
                        if (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > 0)
                            ShowContinueError("only " + RoundSigDigits(thisZoneEquipList.NumOfEquipTypes) + " in the list.");
                        GetZoneEquipmentDataErrorsFound = true;
                    }

                    const int coolingFractionArrayIdx = nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 3;
                    if (lAlphaBlanks(coolingFractionArrayIdx)) {
                        thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) = ScheduleAlwaysOn;
                    } else {
                        thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) = GetScheduleIndex(AlphArray(coolingFractionArrayIdx));
                        if (thisZoneEquipList.SequentialCoolingFractionSchedPtr(ZoneEquipTypeNum) == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                            ShowContinueError("invalid " + cAlphaFields(coolingFractionArrayIdx)  + "=[" + AlphArray(coolingFractionArrayIdx) + "].");
                            ShowContinueError("Schedule does not exist.");
                            GetZoneEquipmentDataErrorsFound = true;
                        }
                    }

                    const int heatingFractionArrayIdx = nAlphasInExtensible * ZoneEquipTypeIdx + nAlphasBeforeExtensible + 4;
                    if (lAlphaBlanks(heatingFractionArrayIdx)) {
                        thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) = ScheduleAlwaysOn;
                    } else {
                        thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) = GetScheduleIndex(AlphArray(heatingFractionArrayIdx));
                        if (thisZoneEquipList.SequentialHeatingFractionSchedPtr(ZoneEquipTypeNum) == 0) {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + AlphArray(1) + "\".");
                            ShowContinueError("invalid " + cAlphaFields(heatingFractionArrayIdx)  + "=[" + AlphArray(heatingFractionArrayIdx) + "].");
                            ShowContinueError("Schedule does not exist.");
                            GetZoneEquipmentDataErrorsFound = true;
                        }
                    }

                    // do this here for initial prototype, but later will call all the equipment in a separate function to see who is on - maybe
                    if (thisZoneEquipList.HeatingPriority(ZoneEquipTypeNum) > 0) ++thisZoneEquipList.NumAvailHeatEquip;
                    if (thisZoneEquipList.CoolingPriority(ZoneEquipTypeNum) > 0) ++thisZoneEquipList.NumAvailCoolEquip;

                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(thisZoneEquipList.EquipType(ZoneEquipTypeNum)));

                        if (SELECT_CASE_var == "ZONEHVAC:AIRDISTRIBUTIONUNIT") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = AirDistUnit_Num;

                        } else if (SELECT_CASE_var == "AIRTERMINAL:SINGLEDUCT:UNCONTROLLED") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = DirectAir_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:WINDOWAIRCONDITIONER") { // Window Air Conditioner
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = WindowAC_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALHEATPUMP") { // Packaged Terminal Heat Pump
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermHPAirToAir_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER") { // Packaged Terminal Air Conditioner
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermACAirToAir_Num;

                        } else if (SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM") { // Unitary System
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneUnitarySys_Num;
                            UnitarySystems::UnitarySys thisSys;
                            thisZoneEquipList.compPointer[ZoneEquipTypeNum] = thisSys.factory(
                                DataHVACGlobals::UnitarySys_AnyCoilType, thisZoneEquipList.EquipName(ZoneEquipTypeNum), true, 0);

                        } else if (SELECT_CASE_var == "ZONEHVAC:DEHUMIDIFIER:DX") { // Zone dehumidifier
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneDXDehumidifier_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:WATERTOAIRHEATPUMP") { // Zone Water to Air Heat Pump
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PkgTermHPWaterToAir_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:FOURPIPEFANCOIL") { // 4-Pipe Fan Coil
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = FanCoil4Pipe_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:UNITVENTILATOR") { // Unit Ventilator
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UnitVentilator_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:UNITHEATER") { // Unit Heater
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UnitHeater_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:IDEALLOADSAIRSYSTEM") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = PurchasedAir_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER") { // Hot Water Baseboard
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBWater_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER") { // Baseboard
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBWaterConvective_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC") { // Electric Baseboard
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBElectricConvective_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:COOLINGPANEL:RADIANTCONVECTIVE:WATER") { // Simple Cooling Panel
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = CoolingPanel_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:HIGHTEMPERATURERADIANT") { // High Temperature Radiators
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HiTempRadiant_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW") { // Low temperature radiant system (hydronic)
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                        } else if (SELECT_CASE_var ==
                                   "ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW") { // Low temperature radiant system (hydronic, constant flow)
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:LOWTEMPERATURERADIANT:ELECTRIC") { // Low temperature radiant system (electric)
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = LoTempRadiant_Num;

                        } else if (SELECT_CASE_var == "FAN:ZONEEXHAUST") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneExhaustFan_Num;

                        } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HeatXchngr_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:ENERGYRECOVERYVENTILATOR") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ERVStandAlone_Num;

                        } else if (SELECT_CASE_var == "WATERHEATER:HEATPUMP:PUMPEDCONDENSER") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HPWaterHeater_Num;

                        } else if (SELECT_CASE_var == "WATERHEATER:HEATPUMP:WRAPPEDCONDENSER") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = HPWaterHeater_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:VENTILATEDSLAB") { // Ventilated Slab
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = VentilatedSlab_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM") { // Steam Baseboard
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBSteam_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:OUTDOORAIRUNIT") { // Outdoor Air Unit
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = OutdoorAirUnit_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC") { // Radiant electric Baseboard
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = BBElectric_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW") { // VRF AC System
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = VRFTerminalUnit_Num;

                        } else if (SELECT_CASE_var ==
                                   "ZONEHVAC:REFRIGERATIONCHILLERSET") { // Refrigeration chiller designed for warehouse applications
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = RefrigerationAirChillerSet_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:FORCEDAIR:USERDEFINED") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = UserDefinedZoneHVACForcedAir_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:EVAPORATIVECOOLERUNIT") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneEvaporativeCoolerUnit_Num;

                        } else if (SELECT_CASE_var == "ZONEHVAC:HYBRIDUNITARYHVAC") {
                            thisZoneEquipList.EquipType_Num(ZoneEquipTypeNum) = ZoneHybridEvaporativeCooler_Num;

                        } else {
                            ShowSevereError(RoutineName + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                            ShowContinueError("..Invalid Equipment Type = " + thisZoneEquipList.EquipType(ZoneEquipTypeNum));
                            GetZoneEquipmentDataErrorsFound = true;
                        }
                    }
                }

                for (ZoneEquipTypeNum = 1; ZoneEquipTypeNum <= thisZoneEquipList.NumOfEquipTypes; ++ZoneEquipTypeNum) {
                    if (count_eq(thisZoneEquipList.CoolingPriority, ZoneEquipTypeNum) > 1) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                        ShowContinueError("...multiple assignments for Zone Equipment Cooling Sequence=" + RoundSigDigits(ZoneEquipTypeNum) +
                                          ", must be 1-1 correspondence between sequence assignments and number of equipments.");
                        GetZoneEquipmentDataErrorsFound = true;
                    } else if (count_eq(thisZoneEquipList.CoolingPriority, ZoneEquipTypeNum) == 0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                        ShowContinueError("...zero assigned to Zone Equipment Cooling Sequence=" + RoundSigDigits(ZoneEquipTypeNum) +
                                          ", apparent gap in sequence assignments in this equipment list.");
                    }
                    if (count_eq(thisZoneEquipList.HeatingPriority, ZoneEquipTypeNum) > 1) {
                        ShowSevereError(RoutineName + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                        ShowContinueError(
                            "...multiple assignments for Zone Equipment Heating or No-Load Sequence=" + RoundSigDigits(ZoneEquipTypeNum) +
                            ", must be 1-1 correspondence between sequence assignments and number of equipments.");
                        GetZoneEquipmentDataErrorsFound = true;
                    } else if (count_eq(thisZoneEquipList.HeatingPriority, ZoneEquipTypeNum) == 0) {
                        ShowWarningError(RoutineName + CurrentModuleObject + " = " + thisZoneEquipList.Name);
                        ShowContinueError("...zero assigned to Zone Equipment Heating or No-Load Sequence=" + RoundSigDigits(ZoneEquipTypeNum) +
                                          ", apparent gap in sequence assignments in this equipment list.");
                    }
                }

            } else {
                ShowSevereError(RoutineName + CurrentModuleObject + " not found = " + ZoneEquipConfig(ControlledZoneNum).EquipListName);
                ShowContinueError("In ZoneHVAC:EquipmentConnections object, for Zone = " + ZoneEquipConfig(ControlledZoneNum).ZoneName);
                GetZoneEquipmentDataErrorsFound = true;
            }

            // End ZoneHVAC:EquipmentList

            NodeListError = false;
            GetNodeNums(InletNodeListName,
                        NumNodes,
                        NodeNums,
                        NodeListError,
                        NodeType_Air,
                        "ZoneHVAC:EquipmentConnections",
                        ZoneEquipConfig(ControlledZoneNum).ZoneName,
                        NodeConnectionType_ZoneInlet,
                        1,
                        ObjectIsNotParent);

            if (!NodeListError) {
                ZoneEquipConfig(ControlledZoneNum).NumInletNodes = NumNodes;

                ZoneEquipConfig(ControlledZoneNum).InletNode.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).InletNodeADUNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).InletNodeSDUNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat.allocate(NumNodes);

                for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                    ZoneEquipConfig(ControlledZoneNum).InletNode(NodeNum) = NodeNums(NodeNum);
                    UniqueNodeError = false;
                    CheckUniqueNodes(
                        "Zone Air Inlet Nodes", "NodeNumber", UniqueNodeError, _, NodeNums(NodeNum), ZoneEquipConfig(ControlledZoneNum).ZoneName);
                    if (UniqueNodeError) {
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                    ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(NodeNum) = 0;
                    ZoneEquipConfig(ControlledZoneNum).InletNodeADUNum(NodeNum) = 0;
                    ZoneEquipConfig(ControlledZoneNum).InletNodeSDUNum(NodeNum) = 0;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).InNode = 0;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).InNode = 0;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).OutNode = 0;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).OutNode = 0;
                    ++locTermUnitSizingCounter;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitCool(NodeNum).TermUnitSizingIndex = locTermUnitSizingCounter;
                    ZoneEquipConfig(ControlledZoneNum).AirDistUnitHeat(NodeNum).TermUnitSizingIndex = locTermUnitSizingCounter;
                }
            } else {
                ShowContinueError("Invalid Zone Air Inlet Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone = " +
                                  ZoneEquipConfig(ControlledZoneNum).ZoneName);
                GetZoneEquipmentDataErrorsFound = true;
            }

            NodeListError = false;
            GetNodeNums(ExhaustNodeListName,
                        NumNodes,
                        NodeNums,
                        NodeListError,
                        NodeType_Air,
                        "ZoneHVAC:EquipmentConnections",
                        ZoneEquipConfig(ControlledZoneNum).ZoneName,
                        NodeConnectionType_ZoneExhaust,
                        1,
                        ObjectIsNotParent);

            if (!NodeListError) {
                ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes = NumNodes;

                ZoneEquipConfig(ControlledZoneNum).ExhaustNode.allocate(NumNodes);

                for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                    ZoneEquipConfig(ControlledZoneNum).ExhaustNode(NodeNum) = NodeNums(NodeNum);
                    UniqueNodeError = false;
                    CheckUniqueNodes(
                        "Zone Air Exhaust Nodes", "NodeNumber", UniqueNodeError, _, NodeNums(NodeNum), ZoneEquipConfig(ControlledZoneNum).ZoneName);
                    if (UniqueNodeError) {
                        // ShowContinueError( "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                }
            } else {
                ShowContinueError("Invalid Zone Air Exhaust Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                                  ZoneEquipConfig(ControlledZoneNum).ZoneName);
                GetZoneEquipmentDataErrorsFound = true;
            }

            NodeListError = false;
            GetNodeNums(ReturnNodeListName,
                        NumNodes,
                        NodeNums,
                        NodeListError,
                        NodeType_Air,
                        "ZoneHVAC:EquipmentConnections",
                        ZoneEquipConfig(ControlledZoneNum).ZoneName,
                        NodeConnectionType_ZoneReturn,
                        1,
                        ObjectIsNotParent);

            if (!NodeListError) {
                ZoneEquipConfig(ControlledZoneNum).NumReturnNodes = NumNodes;

                ZoneEquipConfig(ControlledZoneNum).ReturnNode.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).ReturnNodeAirLoopNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).ReturnNodeInletNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).FixedReturnFlow.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).ReturnNodePlenumNum.allocate(NumNodes);
                ZoneEquipConfig(ControlledZoneNum).ReturnNode = 0;           // initialize to zero here
                ZoneEquipConfig(ControlledZoneNum).ReturnNodeAirLoopNum = 0; // initialize to zero here
                ZoneEquipConfig(ControlledZoneNum).ReturnNodeInletNum = 0;   // initialize to zero here
                ZoneEquipConfig(ControlledZoneNum).FixedReturnFlow = false;  // initialize to false here
                ZoneEquipConfig(ControlledZoneNum).ReturnNodePlenumNum = 0;  // initialize to zero here

                for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                    ZoneEquipConfig(ControlledZoneNum).ReturnNode(NodeNum) = NodeNums(NodeNum);
                    UniqueNodeError = false;
                    CheckUniqueNodes(
                        "Zone Return Air Nodes", "NodeNumber", UniqueNodeError, _, NodeNums(NodeNum), ZoneEquipConfig(ControlledZoneNum).ZoneName);
                    if (UniqueNodeError) {
                        // ShowContinueError( "Occurs for Zone = " + trim( AlphArray( 1 ) ) );
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                }
            } else {
                ShowContinueError("Invalid Zone Return Air Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                                  ZoneEquipConfig(ControlledZoneNum).ZoneName);
                GetZoneEquipmentDataErrorsFound = true;
            }

            NodeListError = false;
            GetNodeNums(ReturnFlowBasisNodeListName,
                        NumNodes,
                        NodeNums,
                        NodeListError,
                        NodeType_Air,
                        "ZoneHVAC:EquipmentConnections",
                        ZoneEquipConfig(ControlledZoneNum).ZoneName,
                        NodeConnectionType_Sensor,
                        1,
                        ObjectIsNotParent);

            if (!NodeListError) {
                ZoneEquipConfig(ControlledZoneNum).NumReturnFlowBasisNodes = NumNodes;

                ZoneEquipConfig(ControlledZoneNum).ReturnFlowBasisNode.allocate(NumNodes);

                for (NodeNum = 1; NodeNum <= NumNodes; ++NodeNum) {
                    ZoneEquipConfig(ControlledZoneNum).ReturnFlowBasisNode(NodeNum) = NodeNums(NodeNum);
                }
            } else {
                ShowContinueError(
                    "Invalid Zone Return Air Node 1 Flow Rate Basis Node or NodeList Name in ZoneHVAC:EquipmentConnections object, for Zone=" +
                    ZoneEquipConfig(ControlledZoneNum).ZoneName);
                GetZoneEquipmentDataErrorsFound = true;
            }

        } // end loop over controlled zones

        // Allocate TermUnitSizing array and set zone number
        if (locTermUnitSizingCounter > 0) {
            DataSizing::NumAirTerminalUnits = locTermUnitSizingCounter;
            DataSizing::TermUnitSizing.allocate(DataSizing::NumAirTerminalUnits);
            for (int loopZoneNum = 1; loopZoneNum <= NumOfZones; ++loopZoneNum) {
                {
                    auto &thisZoneEqConfig(ZoneEquipConfig(loopZoneNum));
                    for (int loopNodeNum = 1; loopNodeNum <= thisZoneEqConfig.NumInletNodes; ++loopNodeNum) {
                        DataSizing::TermUnitSizing(thisZoneEqConfig.AirDistUnitCool(loopNodeNum).TermUnitSizingIndex).CtrlZoneNum = loopZoneNum;
                    }
                }
            }
        }
        if (GetZoneEquipmentDataErrorsFound) {
            ShowWarningError(RoutineName + CurrentModuleObject + ", duplicate items NOT CHECKED due to previous errors.");
            overallEquipCount = 0;
        }
        if (overallEquipCount > 0) {
            ZoneEquipListAcct.allocate(overallEquipCount);
            overallEquipCount = 0;
            for (Loop1 = 1; Loop1 <= NumOfControlledZones; ++Loop1) {
                for (Loop2 = 1; Loop2 <= ZoneEquipList(Loop1).NumOfEquipTypes; ++Loop2) {
                    ++overallEquipCount;
                    ZoneEquipListAcct(overallEquipCount).ObjectType = ZoneEquipList(Loop1).EquipType(Loop2);
                    ZoneEquipListAcct(overallEquipCount).ObjectName = ZoneEquipList(Loop1).EquipName(Loop2);
                    ZoneEquipListAcct(overallEquipCount).OnListNum = Loop1;
                }
            }
            // Now check for uniqueness
            for (Loop1 = 1; Loop1 <= overallEquipCount; ++Loop1) {
                for (Loop2 = Loop1 + 1; Loop2 <= overallEquipCount; ++Loop2) {
                    if (ZoneEquipListAcct(Loop1).ObjectType != ZoneEquipListAcct(Loop2).ObjectType ||
                        ZoneEquipListAcct(Loop1).ObjectName != ZoneEquipListAcct(Loop2).ObjectName)
                        continue;
                    // Duplicated -- not allowed
                    ShowSevereError(RoutineName + CurrentModuleObject + ", duplicate items in ZoneHVAC:EquipmentList.");
                    ShowContinueError("Equipment: Type=" + ZoneEquipListAcct(Loop1).ObjectType + ", Name=" + ZoneEquipListAcct(Loop1).ObjectName);
                    ShowContinueError("Found on List=\"" + ZoneEquipList(ZoneEquipListAcct(Loop1).OnListNum).Name + "\".");
                    ShowContinueError("Equipment Duplicated on List=\"" + ZoneEquipList(ZoneEquipListAcct(Loop2).OnListNum).Name + "\".");
                    GetZoneEquipmentDataErrorsFound = true;
                }
            }
            ZoneEquipListAcct.deallocate();
        }

        // map ZoneEquipConfig%EquipListIndex to ZoneEquipList%Name

        for (ControlledZoneLoop = 1; ControlledZoneLoop <= NumOfZones; ++ControlledZoneLoop) {
            GetZoneEquipmentDataFound =
                UtilityRoutines::FindItemInList(ZoneEquipList(ControlledZoneLoop).Name, ZoneEquipConfig, &EquipConfiguration::EquipListName);
            if (GetZoneEquipmentDataFound > 0) ZoneEquipConfig(GetZoneEquipmentDataFound).EquipListIndex = ControlledZoneLoop;
        } // end loop over controlled zones

        EndUniqueNodeCheck("ZoneHVAC:EquipmentConnections");

        CurrentModuleObject = "AirLoopHVAC:SupplyPath";
        for (PathNum = 1; PathNum <= NumSupplyAirPaths; ++PathNum) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          PathNum,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields); //  data for one zone
            UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, GetZoneEquipmentDataErrorsFound);
            SupplyAirPath(PathNum).Name = AlphArray(1);
            SupplyAirPath(PathNum).NumOfComponents = nint((double(NumAlphas) - 2.0) / 2.0);

            SupplyAirPath(PathNum).InletNodeNum = GetOnlySingleNode(AlphArray(2),
                                                                    GetZoneEquipmentDataErrorsFound,
                                                                    CurrentModuleObject,
                                                                    AlphArray(1),
                                                                    NodeType_Air,
                                                                    NodeConnectionType_Inlet,
                                                                    1,
                                                                    ObjectIsParent);

            SupplyAirPath(PathNum).ComponentType.allocate(SupplyAirPath(PathNum).NumOfComponents);
            SupplyAirPath(PathNum).ComponentType_Num.allocate(SupplyAirPath(PathNum).NumOfComponents);
            SupplyAirPath(PathNum).ComponentType_Num = 0;
            SupplyAirPath(PathNum).ComponentName.allocate(SupplyAirPath(PathNum).NumOfComponents);
            SupplyAirPath(PathNum).ComponentIndex.allocate(SupplyAirPath(PathNum).NumOfComponents);
            SupplyAirPath(PathNum).SplitterIndex.allocate(SupplyAirPath(PathNum).NumOfComponents);
            SupplyAirPath(PathNum).PlenumIndex.allocate(SupplyAirPath(PathNum).NumOfComponents);

            Counter = 3;

            for (CompNum = 1; CompNum <= SupplyAirPath(PathNum).NumOfComponents; ++CompNum) {

                if ((AlphArray(Counter) == "AIRLOOPHVAC:ZONESPLITTER") || (AlphArray(Counter) == "AIRLOOPHVAC:SUPPLYPLENUM")) {

                    SupplyAirPath(PathNum).ComponentType(CompNum) = AlphArray(Counter);
                    SupplyAirPath(PathNum).ComponentName(CompNum) = AlphArray(Counter + 1);
                    ValidateComponent(
                        SupplyAirPath(PathNum).ComponentType(CompNum), SupplyAirPath(PathNum).ComponentName(CompNum), IsNotOK, CurrentModuleObject);
                    SupplyAirPath(PathNum).ComponentIndex(CompNum) = 0;
                    SupplyAirPath(PathNum).SplitterIndex(CompNum) = 0;
                    SupplyAirPath(PathNum).PlenumIndex(CompNum) = 0;
                    if (AlphArray(Counter) == "AIRLOOPHVAC:ZONESPLITTER") SupplyAirPath(PathNum).ComponentType_Num(CompNum) = ZoneSplitter_Type;
                    if (AlphArray(Counter) == "AIRLOOPHVAC:SUPPLYPLENUM") SupplyAirPath(PathNum).ComponentType_Num(CompNum) = ZoneSupplyPlenum_Type;

                } else {
                    ShowSevereError(RoutineName + cAlphaFields(1) + "=\"" + SupplyAirPath(PathNum).Name + "\"");
                    ShowContinueError("Unhandled component type =\"" + AlphArray(Counter) + "\".");
                    ShowContinueError("Must be \"AirLoopHVAC:ZoneSplitter\" or \"AirLoopHVAC:SupplyPlenum\"");
                    GetZoneEquipmentDataErrorsFound = true;
                }

                Counter += 2;
            }

            SupplyAirPath(PathNum).NumOutletNodes = 0;
            SupplyAirPath(PathNum).NumNodes = 0;

        } // end loop over supply air paths

        CurrentModuleObject = "AirLoopHVAC:ReturnPath";
        for (PathNum = 1; PathNum <= NumReturnAirPaths; ++PathNum) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          PathNum,
                                          AlphArray,
                                          NumAlphas,
                                          NumArray,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields); //  data for one zone
            UtilityRoutines::IsNameEmpty(AlphArray(1), CurrentModuleObject, GetZoneEquipmentDataErrorsFound);
            ReturnAirPath(PathNum).Name = AlphArray(1);
            ReturnAirPath(PathNum).NumOfComponents = nint((double(NumAlphas) - 2.0) / 2.0);

            ReturnAirPath(PathNum).OutletNodeNum = GetOnlySingleNode(AlphArray(2),
                                                                     GetZoneEquipmentDataErrorsFound,
                                                                     CurrentModuleObject,
                                                                     AlphArray(1),
                                                                     NodeType_Air,
                                                                     NodeConnectionType_Outlet,
                                                                     1,
                                                                     ObjectIsParent);

            ReturnAirPath(PathNum).ComponentType.allocate(ReturnAirPath(PathNum).NumOfComponents);
            ReturnAirPath(PathNum).ComponentType_Num.allocate(ReturnAirPath(PathNum).NumOfComponents);
            ReturnAirPath(PathNum).ComponentType_Num = 0;
            ReturnAirPath(PathNum).ComponentName.allocate(ReturnAirPath(PathNum).NumOfComponents);
            ReturnAirPath(PathNum).ComponentIndex.allocate(ReturnAirPath(PathNum).NumOfComponents);

            Counter = 3;

            for (CompNum = 1; CompNum <= ReturnAirPath(PathNum).NumOfComponents; ++CompNum) {

                if ((AlphArray(Counter) == "AIRLOOPHVAC:ZONEMIXER") || (AlphArray(Counter) == "AIRLOOPHVAC:RETURNPLENUM")) {

                    ReturnAirPath(PathNum).ComponentType(CompNum) = AlphArray(Counter);
                    ReturnAirPath(PathNum).ComponentName(CompNum) = AlphArray(Counter + 1);
                    ReturnAirPath(PathNum).ComponentIndex(CompNum) = 0;
                    ValidateComponent(
                        ReturnAirPath(PathNum).ComponentType(CompNum), ReturnAirPath(PathNum).ComponentName(CompNum), IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError("In " + CurrentModuleObject + " = " + ReturnAirPath(PathNum).Name);
                        GetZoneEquipmentDataErrorsFound = true;
                    }
                    if (AlphArray(Counter) == "AIRLOOPHVAC:ZONEMIXER") ReturnAirPath(PathNum).ComponentType_Num(CompNum) = ZoneMixer_Type;
                    if (AlphArray(Counter) == "AIRLOOPHVAC:RETURNPLENUM") ReturnAirPath(PathNum).ComponentType_Num(CompNum) = ZoneReturnPlenum_Type;
                } else {
                    ShowSevereError(RoutineName + cAlphaFields(1) + "=\"" + ReturnAirPath(PathNum).Name + "\"");
                    ShowContinueError("Unhandled component type =\"" + AlphArray(Counter) + "\".");
                    ShowContinueError("Must be \"AirLoopHVAC:ZoneMixer\" or \"AirLoopHVAC:ReturnPlenum\"");
                    GetZoneEquipmentDataErrorsFound = true;
                }

                Counter += 2;
            }

        } // end loop over return air paths

        AlphArray.deallocate();
        NumArray.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        // setup zone equipment info for convection correlations
        SetupZoneEquipmentForConvectionFlowRegime();

        if (GetZoneEquipmentDataErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in getting Zone Equipment input.");
        }
    }

    void SetupZoneEquipmentForConvectionFlowRegime()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   Aug 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Decide a few one-time things for later
        // determination of flow regime for convection

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // na

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneLoop;

        for (ZoneLoop = 1; ZoneLoop <= NumOfZones; ++ZoneLoop) {
        }
    }

    bool CheckZoneEquipmentList(std::string const &ComponentType, // Type of component
                                std::string const &ComponentName, // Name of component
                                Optional_int CtrlZoneNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   May 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provides a way to check if a component name is listed on a zone equipment list.

        // Return value
        bool IsOnList; // True if item is on a list, false if not.

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Loop;
        int ListLoop;
        int CtrlZoneNumLocal;

        CtrlZoneNumLocal = 0;
        IsOnList = false;
        for (Loop = 1; Loop <= NumOfZones; ++Loop) {      // NumOfZoneEquipLists
            if (ZoneEquipList(Loop).Name == "") continue; // dimensioned by NumOfZones.  Only valid ones have names.
            for (ListLoop = 1; ListLoop <= ZoneEquipList(Loop).NumOfEquipTypes; ++ListLoop) {
                if (!UtilityRoutines::SameString(ZoneEquipList(Loop).EquipType(ListLoop), ComponentType)) continue;
                if (ComponentName == "*") {
                    IsOnList = true;
                    CtrlZoneNumLocal = Loop;
                    goto EquipList_exit;
                }
                if (!UtilityRoutines::SameString(ZoneEquipList(Loop).EquipName(ListLoop), ComponentName)) continue;
                IsOnList = true;
                CtrlZoneNumLocal = Loop;
                goto EquipList_exit;
            }
        }
    EquipList_exit:;
        if (present(CtrlZoneNum)) {
            CtrlZoneNum = CtrlZoneNumLocal;
        }
        return IsOnList;
    }

    int GetControlledZoneIndex(std::string const &ZoneName) // Zone name to match into Controlled Zone structure
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the index into the Controlled Zone Equipment structure
        // of the indicated zone.

        // Return value
        int ControlledZoneIndex; // Index into Controlled Zone structure

        if (!ZoneEquipInputsFilled) {
            GetZoneEquipmentData1();
            ZoneEquipInputsFilled = true;
        }

        ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName);

        return ControlledZoneIndex;
    }

    int FindControlledZoneIndexFromSystemNodeNumberForZone(int const TrialZoneNodeNum) // Node number to match into Controlled Zone structure
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Brent Griffith
        //       DATE WRITTEN   August 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the zone number for the indicated
        // zone node num.  Returns 0 if did not find zone node in any Zone

        // Return value
        int ControlledZoneIndex; // Index into Controlled Zone structure

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        bool FoundIt;
        int ZoneNum;

        FoundIt = false;

        if (!ZoneEquipInputsFilled) {
            GetZoneEquipmentData1();
            ZoneEquipInputsFilled = true;
        }
        ControlledZoneIndex = 0;
        for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
            if (ZoneEquipConfig(ZoneNum).ActualZoneNum > 0) {
                if (TrialZoneNodeNum == ZoneEquipConfig(ZoneNum).ZoneNode) {
                    // found it.
                    FoundIt = true;
                    ControlledZoneIndex = ZoneEquipConfig(ZoneNum).ActualZoneNum;
                }
            }
        }

        return ControlledZoneIndex;
    }

    int GetSystemNodeNumberForZone(std::string const &ZoneName) // Zone name to match into Controlled Zone structure
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the system node number for the indicated
        // zone.  Returns 0 if the Zone is not a controlled zone.

        // Return value
        int SystemZoneNodeNumber; // System node number for controlled zone

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int ControlledZoneIndex;

        if (!ZoneEquipInputsFilled) {
            GetZoneEquipmentData1();
            ZoneEquipInputsFilled = true;
        }

        ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName);
        SystemZoneNodeNumber = 0; // default is not found
        if (ControlledZoneIndex > 0) {
            if (ZoneEquipConfig(ControlledZoneIndex).ActualZoneNum > 0) {
                SystemZoneNodeNumber = ZoneEquipConfig(ControlledZoneIndex).ZoneNode;
            }
        }

        return SystemZoneNodeNumber;
    }

    int GetReturnAirNodeForZone(std::string const &ZoneName,             // Zone name to match into Controlled Zone structure
                                std::string const &NodeName,             // Return air node name to match (may be blank)
                                std::string const &calledFromDescription // String identifying the calling function and object
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2008
        //       MODIFIED       Feb 2017 expanded for multiple return nodes in a zone

        // PURPOSE OF THIS FUNCTION:
        // This function returns the return air node number for the indicated
        // zone and node name.  If NodeName is blank, return the first return node number,
        // otherwise return the node number of the matching return node name.
        // Returns 0 if the Zone is not a controlled zone or the node name does not match.

        // Return value
        int ReturnAirNodeNumber; // Return Air node number for controlled zone
        int ControlledZoneIndex;

        if (!ZoneEquipInputsFilled) {
            GetZoneEquipmentData1();
            ZoneEquipInputsFilled = true;
        }

        ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName);
        ReturnAirNodeNumber = 0; // default is not found
        if (ControlledZoneIndex > 0) {
            {
                auto const &thisZoneEquip(ZoneEquipConfig(ControlledZoneIndex));
                if (thisZoneEquip.ActualZoneNum > 0) {
                    if (NodeName == "") {
                        // If NodeName is blank, return first return node number, but warn if there are multiple return nodes for this zone
                        ReturnAirNodeNumber = thisZoneEquip.ReturnNode(1);
                        if (thisZoneEquip.NumReturnNodes > 1) {
                            ShowWarningError("GetReturnAirNodeForZone: " + calledFromDescription + ", request for zone return node is ambiguous.");
                            ShowContinueError("Zone=" + thisZoneEquip.ZoneName + " has " + General::RoundSigDigits(thisZoneEquip.NumReturnNodes) +
                                              " return nodes. First return node will be used.");
                        }
                    } else {
                        for (int nodeCount = 1; nodeCount <= thisZoneEquip.NumReturnNodes; ++nodeCount) {
                            int curNodeNum = thisZoneEquip.ReturnNode(nodeCount);
                            if (NodeName == DataLoopNode::NodeID(curNodeNum)) {
                                ReturnAirNodeNumber = curNodeNum;
                            }
                        }
                    }
                }
            }
        }

        return ReturnAirNodeNumber;
    }

    int GetReturnNumForZone(std::string const &ZoneName, // Zone name to match into Controlled Zone structure
                            std::string const &NodeName  // Return air node name to match (may be blank)
    )
    {

        // PURPOSE OF THIS FUNCTION:
        // This function returns the zone return number (not the node number) for the indicated
        // zone and node name.  If NodeName is blank, return 1 (the first return node)
        // otherwise return the index of the matching return node name.
        // Returns 0 if the Zone is not a controlled zone or the node name does not match.

        // Return value
        int ReturnIndex; // Return number for the given zone (not the node number)

        int ControlledZoneIndex;

        if (!ZoneEquipInputsFilled) {
            GetZoneEquipmentData1();
            ZoneEquipInputsFilled = true;
        }

        ControlledZoneIndex = UtilityRoutines::FindItemInList(ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName);
        ReturnIndex = 0; // default if not found
        if (ControlledZoneIndex > 0) {
            if (ZoneEquipConfig(ControlledZoneIndex).ActualZoneNum > 0) {
                if (NodeName == "") {
                    // If NodeName is blank, return first return node number
                    ReturnIndex = 1;
                } else {
                    for (int nodeCount = 1; nodeCount <= ZoneEquipConfig(ControlledZoneIndex).NumReturnNodes; ++nodeCount) {
                        int curNodeNum = ZoneEquipConfig(ControlledZoneIndex).ReturnNode(nodeCount);
                        if (NodeName == DataLoopNode::NodeID(curNodeNum)) {
                            ReturnIndex = nodeCount;
                        }
                    }
                }
            }
        }

        return ReturnIndex;
    }

    Real64 CalcDesignSpecificationOutdoorAir(int const DSOAPtr,          // Pointer to DesignSpecification:OutdoorAir object
                                             int const ActualZoneNum,    // Zone index
                                             bool const UseOccSchFlag,   // Zone occupancy schedule will be used instead of using total zone occupancy
                                             bool const UseMinOASchFlag, // Use min OA schedule in DesignSpecification:OutdoorAir object
                                             Optional_bool_const PerPersonNotSet, // when calculation should not include occupants (e.g., dual duct)
                                             Optional_bool_const MaxOAVolFlowFlag // TRUE when calculation uses occupancy schedule  (e.g., dual duct)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   October 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns the air volume flow rate based on DesignSpecification:OutdoorAir object.

        // METHODOLOGY EMPLOYED:
        // User inputs and zone index allows calculation of outdoor air quantity.
        // Sizing does not use occupancy or min OA schedule and will call with flags set to FALSE
        // Ventilation Rate Procedure uses occupancy schedule based on user input.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataContaminantBalance::OutdoorCO2;
        using DataContaminantBalance::ZoneAirCO2;
        using DataContaminantBalance::ZoneCO2GainFromPeople;
        using DataContaminantBalance::ZoneSysContDemand;
        using DataEnvironment::StdRhoAir;
        using DataGlobals::DisplayExtraWarnings;
        using DataHeatBalance::People;
        using DataHeatBalance::TotPeople;
        using DataHeatBalance::Zone;
        using DataHeatBalance::ZoneIntGain;
        using DataSizing::OAFlow;
        using DataSizing::OAFlowACH;
        using DataSizing::OAFlowMax;
        using DataSizing::OAFlowNone;
        using DataSizing::OAFlowPerArea;
        using DataSizing::OAFlowPPer;
        using DataSizing::OAFlowSum;
        using DataSizing::OARequirements; // to access DesignSpecification:OutdoorAir inputs
        using DataSizing::ZOAM_IAQP;
        using DataSizing::ZOAM_ProportionalControlDesOcc;
        using DataSizing::ZOAM_ProportionalControlSchOcc;
        using General::RoundSigDigits;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleMaxValue;

        // Return value
        Real64 OAVolumeFlowRate; // Return value for calculated outdoor air volume flow rate [m3/s]

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        Real64 DSOAFlowPeople;  // Outdoor air volume flow rate based on occupancy (m3/s)
        Real64 DSOAFlowPerZone; // Outdoor air volume flow rate (m3/s)
        Real64 DSOAFlowPerArea; // Outdoor air volume flow rate based on zone floor area (m3/s)
        Real64 DSOAFlowACH;     // Outdoor air volume flow rate based on air changes per hour (m3/s)
        Real64 PeopleCount;     // total count of people in people objects
        int Loop;               // index counter in LOOP
        bool PerPersonModeNotSet;
        bool MaxOAFlag;
        Real64 ZoneOAPeople;              // Zone OA flow rate based on number of occupants [m3/s]
        Real64 ZoneOAArea;                // Zone OA flow rate based on space floor area [m3/s]
        Real64 ZoneOAMin;                 // Minimum Zone OA flow rate when the zone is unoccupied (i.e. ZoneOAPeople = 0)
                                          // used for "ProportionalControl" System outdoor air method
        Real64 ZoneOAMax;                 // Maximum Zone OA flow rate (ZoneOAPeople + ZoneOAArea)
                                          // used for "ProportionalControl" System outdoor air method
        Real64 ZoneMaxCO2;                // Breathing-zone CO2 concentartion
        Real64 ZoneMinCO2;                // Minimum CO2 concentration in zone
        Real64 ZoneContamControllerSched; // Schedule value for ZoneControl:ContaminantController
        Real64 CO2PeopleGeneration;       // CO2 generation from people at design level
        int PeopleNum;
        static Array1D_bool MyEnvrnFlag;
        static bool OneTimeFlag(true);

        OAVolumeFlowRate = 0.0;
        if (DSOAPtr == 0) return OAVolumeFlowRate;

        if (OneTimeFlag) {
            MyEnvrnFlag.allocate(DataSizing::NumOARequirements);
            MyEnvrnFlag = true;
            OneTimeFlag = false;
        }

        if (present(PerPersonNotSet)) {
            PerPersonModeNotSet = PerPersonNotSet;
        } else {
            PerPersonModeNotSet = false;
        }

        if (present(MaxOAVolFlowFlag)) {
            MaxOAFlag = MaxOAVolFlowFlag;
        } else {
            MaxOAFlag = false;
        }

        if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_IAQP && MyEnvrnFlag(DSOAPtr)) {
            if (!DataContaminantBalance::Contaminant.CO2Simulation) {
                ShowSevereError("DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                "\" valid Outdoor Air Method =\" IndoorAirQualityProcedure\" requires CO2 simulation.");
                ShowContinueError("The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                ShowFatalError("CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
            }
            MyEnvrnFlag(DSOAPtr) = false;
        }
        if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc && MyEnvrnFlag(DSOAPtr)) {
            if (!DataContaminantBalance::Contaminant.CO2Simulation) {
                ShowSevereError("DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                "\" valid Outdoor Air Method =\" ProportionalControlBasedOnDesignOccupancy\" requires CO2 simulation.");
                ShowContinueError("The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                ShowFatalError("CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
            }
            MyEnvrnFlag(DSOAPtr) = false;
        }
        if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc && MyEnvrnFlag(DSOAPtr)) {
            if (!DataContaminantBalance::Contaminant.CO2Simulation) {
                ShowSevereError("DesignSpecification:OutdoorAir=\"" + OARequirements(DSOAPtr).Name +
                                "\" valid Outdoor Air Method =\" ProportionalControlBasedonOccupancySchedule\" requires CO2 simulation.");
                ShowContinueError("The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                ShowFatalError("CalcDesignSpecificationOutdoorAir: Errors found in input. Preceding condition(s) cause termination.");
            }
            MyEnvrnFlag(DSOAPtr) = false;
        }

        // Calculate people outdoor air flow rate as needed
        {
            auto const SELECT_CASE_var(OARequirements(DSOAPtr).OAFlowMethod);
            if ((SELECT_CASE_var == OAFlowPPer) || (SELECT_CASE_var == OAFlowSum) || (SELECT_CASE_var == OAFlowMax)) {
                if (UseOccSchFlag) {
                    if (MaxOAFlag) {
                        // OAPerPersonMode == PerPersonDCVByCurrentLevel (UseOccSchFlag = TRUE)
                        // for dual duct, get max people according to max schedule value when requesting MaxOAFlow
                        PeopleCount = 0.0;
                        for (Loop = 1; Loop <= TotPeople; ++Loop) {
                            if (ActualZoneNum != People(Loop).ZonePtr) continue;
                            PeopleCount += People(Loop).NumberOfPeople * GetScheduleMaxValue(People(Loop).NumberOfPeoplePtr);
                        }
                        DSOAFlowPeople = PeopleCount * OARequirements(DSOAPtr).OAFlowPerPerson;
                    } else {
                        DSOAFlowPeople = ZoneIntGain(ActualZoneNum).NOFOCC * OARequirements(DSOAPtr).OAFlowPerPerson;
                    }
                } else {
                    if (MaxOAFlag) {
                        // OAPerPersonMode == PerPersonByDesignLevel (UseOccSchFlag = FALSE)
                        // use total people when requesting MaxOAFlow
                        DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                    } else {
                        DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                    }
                }
                if (PerPersonModeNotSet) DSOAFlowPeople = 0.0; // for Dual Duct if Per Person Ventilation Rate Mode is not entered
            } else {
                DSOAFlowPeople = 0.0;
            }
        }

        // Calculate minimum outdoor air flow rate
        {
            auto const SELECT_CASE_var(OARequirements(DSOAPtr).OAFlowMethod);
            if (SELECT_CASE_var == OAFlowNone) {
                // Special case for no DesignSpecification:OutdoorAir object in Sizing:Zone object
                // probably won't get to this CASE statement since it will RETURN above (Ptr=0)
                // See SizingManager GetZoneSizingInput for Sizing:Zone input field Design Specification Outdoor Air Object Name
                OAVolumeFlowRate = 0.0;
            } else if (SELECT_CASE_var == OAFlowPPer) {
                // Multiplied by occupancy
                OAVolumeFlowRate = DSOAFlowPeople;
            } else if (SELECT_CASE_var == OAFlow) {
                // User input
                OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowPerZone;
            } else if (SELECT_CASE_var == OAFlowPerArea) {
                // Multiplied by zone floor area
                OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
            } else if (SELECT_CASE_var == OAFlowACH) {
                // Multiplied by zone volume
                OAVolumeFlowRate = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
            } else if ((SELECT_CASE_var == OAFlowSum) || (SELECT_CASE_var == OAFlowMax)) {
                // Use sum or max of per person and the following
                DSOAFlowPerZone = OARequirements(DSOAPtr).OAFlowPerZone;
                DSOAFlowPerArea = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
                DSOAFlowACH = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
                if (OARequirements(DSOAPtr).OAFlowMethod == OAFlowMax) {
                    OAVolumeFlowRate = max(DSOAFlowPeople, DSOAFlowPerZone, DSOAFlowPerArea, DSOAFlowACH);
                } else {
                    OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH;
                }
            } else if (SELECT_CASE_var == ZOAM_IAQP) {
                if (DataGlobals::DoingSizing) {
                    DSOAFlowPeople = Zone(ActualZoneNum).TotOccupants * OARequirements(DSOAPtr).OAFlowPerPerson;
                    DSOAFlowPerZone = OARequirements(DSOAPtr).OAFlowPerZone;
                    DSOAFlowPerArea = OARequirements(DSOAPtr).OAFlowPerArea * Zone(ActualZoneNum).FloorArea;
                    DSOAFlowACH = OARequirements(DSOAPtr).OAFlowACH * Zone(ActualZoneNum).Volume / 3600.0;
                    OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH;
                } else {
                    OAVolumeFlowRate = ZoneSysContDemand(ActualZoneNum).OutputRequiredToCO2SP / StdRhoAir;
                }

            } else if (SELECT_CASE_var == ZOAM_ProportionalControlSchOcc || SELECT_CASE_var == ZOAM_ProportionalControlDesOcc) {
                Real64 ZoneEz = 1.0;
                ZoneOAPeople = 0.0;
                if (OARequirements(DSOAPtr).OAFlowMethod != ZOAM_ProportionalControlDesOcc) {
                    ZoneOAPeople = ZoneIntGain(ActualZoneNum).NOFOCC * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier *
                                   OARequirements(DSOAPtr).OAFlowPerPerson;
                } else {
                    ZoneOAPeople = Zone(ActualZoneNum).TotOccupants * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier *
                                   OARequirements(DSOAPtr).OAFlowPerPerson;
                    CO2PeopleGeneration = 0.0;
                    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                        // Accumulate CO2 generation from people at design occupancy and current activity level
                        for (PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum) {
                            if (People(PeopleNum).ZonePtr != ActualZoneNum) continue;
                            CO2PeopleGeneration += People(PeopleNum).NumberOfPeople * People(PeopleNum).CO2RateFactor *
                                                   GetCurrentScheduleValue(People(PeopleNum).ActivityLevelPtr);
                        }
                    }
                }
                ZoneOAArea = Zone(ActualZoneNum).FloorArea * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier *
                             OARequirements(DSOAPtr).OAFlowPerArea;
                ZoneOAMin = ZoneOAArea / ZoneEz;
                ZoneOAMax = (ZoneOAArea + ZoneOAPeople) / ZoneEz;
                if (Zone(ActualZoneNum).ZoneContamControllerSchedIndex > 0.0) {
                    // Check the availability schedule value for ZoneControl:ContaminantController
                    ZoneContamControllerSched = GetCurrentScheduleValue(Zone(ActualZoneNum).ZoneContamControllerSchedIndex);
                    if (ZoneContamControllerSched > 0.0) {
                        if (ZoneOAPeople > 0.0) {
                            if (ZoneCO2GainFromPeople(ActualZoneNum) > 0.0) {
                                if (Zone(ActualZoneNum).ZoneMinCO2SchedIndex > 0.0) {
                                    // Take the schedule value of "Minimum Carbon Dioxide Concentration Schedule Name"
                                    // in the ZoneControl:ContaminantController
                                    ZoneMinCO2 = GetCurrentScheduleValue(Zone(ActualZoneNum).ZoneMinCO2SchedIndex);
                                } else {
                                    ZoneMinCO2 = OutdoorCO2;
                                }

                                // Calculate zone maximum target CO2 concentration in PPM
                                if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                    ZoneMaxCO2 = OutdoorCO2 +
                                                 (CO2PeopleGeneration * Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier * 1.0e6) /
                                                     ZoneOAMax;
                                } else {
                                    ZoneMaxCO2 = OutdoorCO2 + (ZoneCO2GainFromPeople(ActualZoneNum) * Zone(ActualZoneNum).Multiplier *
                                                               Zone(ActualZoneNum).ListMultiplier * 1.0e6) /
                                                                  ZoneOAMax;
                                }

                                if (ZoneMaxCO2 <= ZoneMinCO2) {
                                    ++OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount;
                                    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc) {
                                        if (OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount < 2) {
                                            ShowSevereError("CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                            ShowContinueError("For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, maximum "
                                                              "target CO2 concentration (" +
                                                              RoundSigDigits(ZoneMaxCO2, 2) +
                                                              "), is not greater than minimum target CO2 concentration (" +
                                                              RoundSigDigits(ZoneMinCO2, 2) + ").");
                                            ShowContinueError("\"ProportionalControlBasedonOccupancySchedule\" will not be modeled. Default "
                                                              "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                            ShowContinueErrorTimeStamp("");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(
                                                "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                    "\", For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, maximum target "
                                                    "CO2 concentration is not greater than minimum target CO2 concentration. Error continues...",
                                                OARequirements(DSOAPtr).CO2MaxMinLimitErrorIndex);
                                        }
                                    }
                                    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                        if (OARequirements(DSOAPtr).CO2MaxMinLimitErrorCount < 2) {
                                            ShowSevereError("CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                            ShowContinueError("For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, maximum "
                                                              "target CO2 concentration (" +
                                                              RoundSigDigits(ZoneMaxCO2, 2) +
                                                              "), is not greater than minimum target CO2 concentration (" +
                                                              RoundSigDigits(ZoneMinCO2, 2) + ").");
                                            ShowContinueError("\"ProportionalControlBasedonDesignOccupancy\" will not be modeled. Default "
                                                              "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                            ShowContinueErrorTimeStamp("");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(
                                                "DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                    "\", For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, maximum target "
                                                    "CO2 concentration is not greater than minimum target CO2 concentration. Error continues...",
                                                OARequirements(DSOAPtr).CO2MaxMinLimitErrorIndex);
                                        }
                                    }

                                    OAVolumeFlowRate = ZoneOAMax / ZoneEz;
                                } else {

                                    if (ZoneAirCO2(ActualZoneNum) <= ZoneMinCO2) {
                                        // Zone air CO2 concentration is less than minimum zone CO2 concentration, set the Zone OA flow rate to
                                        // minimum Zone OA flow rate when the zone is unoccupied
                                        OAVolumeFlowRate = ZoneOAMin;
                                    } else if (ZoneAirCO2(ActualZoneNum) >= ZoneMaxCO2) {
                                        // Zone air CO2 concentration is greater than maximum zone CO2 concentration, set the Zone OA flow rate to
                                        // maximum Zone OA flow rate (i.e. ZoneOAArea + ZoneOAPeople)
                                        OAVolumeFlowRate = ZoneOAMax;
                                    } else {
                                        // Zone air CO2 concentration is between maximum and minimum limits of zone CO2 concentration,
                                        // set Zone OA flow rate by proportionally adjusting between ZoneOAMin and ZoneOAMax
                                        OAVolumeFlowRate = ZoneOAMin + (ZoneOAMax - ZoneOAMin) *
                                                                           ((ZoneAirCO2(ActualZoneNum) - ZoneMinCO2) / (ZoneMaxCO2 - ZoneMinCO2));
                                    }
                                }
                            } else {
                                if (DisplayExtraWarnings) {
                                    ++OARequirements(DSOAPtr).CO2GainErrorCount;
                                    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlSchOcc) {
                                        if (OARequirements(DSOAPtr).CO2GainErrorCount < 2) {
                                            ShowSevereError("CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                            ShowContinueError("For System Outdoor Air Method = ProportionalControlBasedonOccupancySchedule, CO2 "
                                                              "generation from people is not greater than zero. Occurs in Zone =\"" +
                                                              Zone(ActualZoneNum).Name + "\". ");
                                            ShowContinueError("\"ProportionalControlBasedonOccupancySchedule\" will not be modeled. Default "
                                                              "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                            ShowContinueErrorTimeStamp("");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd("DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                                               "\", For System Outdoor Air Method = "
                                                                               "ProportionalControlBasedonOccupancySchedule, CO2 generation from "
                                                                               "people is not greater than zero. Error continues...",
                                                                           OARequirements(DSOAPtr).CO2GainErrorIndex);
                                        }
                                    }
                                    if (OARequirements(DSOAPtr).OAFlowMethod == ZOAM_ProportionalControlDesOcc) {
                                        if (OARequirements(DSOAPtr).CO2GainErrorCount < 2) {
                                            ShowSevereError("CalcDesignSpecificationOutdoorAir DesignSpecification:OutdoorAir = \"" +
                                                            OARequirements(DSOAPtr).Name + "\".");
                                            ShowContinueError("For System Outdoor Air Method = ProportionalControlBasedonDesignOccupancy, CO2 "
                                                              "generation from people is not greater than zero. Occurs in Zone =\"" +
                                                              Zone(ActualZoneNum).Name + "\". ");
                                            ShowContinueError("\"ProportionalControlBasedonDesignOccupancy\" will not be modeled. Default "
                                                              "\"Flow/Person+Flow/Area\" will be modeled. Simulation continues...");
                                            ShowContinueErrorTimeStamp("");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd("DesignSpecification:OutdoorAir = \"" + OARequirements(DSOAPtr).Name +
                                                                               "\", For System Outdoor Air Method = "
                                                                               "ProportionalControlBasedonDesignOccupancy, CO2 generation from "
                                                                               "people is not greater than zero. Error continues...",
                                                                           OARequirements(DSOAPtr).CO2GainErrorIndex);
                                        }
                                    }
                                }
                                OAVolumeFlowRate = ZoneOAMax / ZoneEz;
                            }
                        } else {
                            // ZoneOAPeople is less than or equal to zero
                            OAVolumeFlowRate = ZoneOAMax / ZoneEz;
                        }
                    } else {
                        // ZoneControl:ContaminantController is scheduled off (not available)
                        OAVolumeFlowRate = ZoneOAMax / ZoneEz;
                    }
                } else {
                    // "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController not found
                    OAVolumeFlowRate = ZoneOAMax / ZoneEz;
                }

            } else {
                // Will never get here
                OAVolumeFlowRate = 0.0;
            }
        }

        // Apply zone multipliers and zone list multipliers
        OAVolumeFlowRate *= Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier;

        // Apply schedule as needed. Sizing does not use schedule.
        if (OARequirements(DSOAPtr).OAFlowFracSchPtr > 0 && UseMinOASchFlag) {
            if (MaxOAFlag) {
                OAVolumeFlowRate *= GetScheduleMaxValue(OARequirements(DSOAPtr).OAFlowFracSchPtr);
            } else {
                OAVolumeFlowRate *= GetCurrentScheduleValue(OARequirements(DSOAPtr).OAFlowFracSchPtr);
            }
        }

        return OAVolumeFlowRate;
    }

    void EquipList::getPrioritiesforInletNode(int const inletNodeNum, // Zone inlet node number to match
                                              int &coolingPriority,   // Cooling priority num for matching equipment
                                              int &heatingPriority    // Heating priority num for matching equipment
    )
    {
        bool equipFound = false;
        for (int equipNum = 1; equipNum <= this->NumOfEquipTypes; ++equipNum) {
            if (this->EquipType_Num(equipNum) == AirDistUnit_Num) {
                if (inletNodeNum == DataDefineEquip::AirDistUnit(this->EquipIndex(equipNum)).OutletNodeNum) {
                    equipFound = true;
                }
            } else if (this->EquipType_Num(equipNum) == DirectAir_Num) {
                if (inletNodeNum == DirectAirManager::DirectAir(this->EquipIndex(equipNum)).ZoneSupplyAirNode) {
                    equipFound = true;
                }
            }
            if (equipFound) {
                coolingPriority = this->CoolingPriority(equipNum);
                heatingPriority = this->HeatingPriority(equipNum);
                break;
            }
        }
        // Set MinAirLoopIterationsAfterFirst for equipment that uses sequenced loads, based on zone equip load distribution scheme
        int minIterations = DataHVACGlobals::MinAirLoopIterationsAfterFirst;
        if (this->LoadDistScheme == DataZoneEquipment::LoadDist::SequentialLoading) {
            // Sequential needs one extra iterations up to the highest airterminal unit equipment number
            minIterations = max(coolingPriority, heatingPriority, minIterations);
        }
        else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::UniformLoading) {
            // Uniform needs one extra iteration which is the default
        }
        else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::UniformPLRLoading) {
            // UniformPLR needs two extra iterations, regardless of unit equipment number
            minIterations = max(2, minIterations);
        }
        else if (this->LoadDistScheme == DataZoneEquipment::LoadDist::SequentialUniformPLRLoading) {
            // SequentialUniformPLR needs one extra iterations up to the highest airterminal unit equipment number plus one more
            minIterations = max((coolingPriority + 1), (heatingPriority + 1), minIterations);
        }
        DataHVACGlobals::MinAirLoopIterationsAfterFirst = minIterations;
    }

    Real64 EquipList::SequentialHeatingFraction(const int equipNum)
    {
        return ScheduleManager::GetCurrentScheduleValue(SequentialHeatingFractionSchedPtr(equipNum));
    }

    Real64 EquipList::SequentialCoolingFraction(const int equipNum)
    {
        return ScheduleManager::GetCurrentScheduleValue(SequentialCoolingFractionSchedPtr(equipNum));
    }


} // namespace DataZoneEquipment

} // namespace EnergyPlus
