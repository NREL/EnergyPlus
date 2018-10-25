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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <BranchNodeConnections.hh>
#include <DataContaminantBalance.hh>
#include <DataDefineEquip.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DirectAirManager.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <GlobalNames.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

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
    int const ZoneUnitarySys_Num(31); // AirloopHVAC:UnitarySystem configured as zone equipment
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
        bool IdealLoadsOnEquipmentList;
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
    }

} // namespace DataZoneEquipment

} // namespace EnergyPlus
