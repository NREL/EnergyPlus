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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace HVACStandAloneERV {

    // Module containing the routines dealing with stand alone energy recovery ventilators (ERVs)

    // MODULE INFORMATION:
    //       AUTHOR         Richard Raustad, FSEC
    //       DATE WRITTEN   June 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms needed to simulate stand alone
    // energy recovery ventilators that condition outdoor ventilation air and
    // supply that air directly to a zone.

    // METHODOLOGY EMPLOYED:
    // These units are modeled as a collection of components: air-to-air generic heat exchanger,
    // supply air fan, exhaust air fan and an optional controller to avoid overheating
    // of the supply air (economizer or free cooling operation).

    // REFERENCES: none

    // OTHER NOTES: none

    // USE STATEMENTS:
    // Use statements for data only modules
    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using DataEnvironment::StdBaroPress;
    using DataEnvironment::StdRhoAir;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::NumOfZones;
    using DataGlobals::ScheduleAlwaysOn;
    using DataGlobals::SecInHour;
    using DataGlobals::SysSizingCalc;
    using DataGlobals::WarmupFlag;
    using namespace DataHVACGlobals;
    using ScheduleManager::GetCurrentScheduleValue;
    using ScheduleManager::GetScheduleIndex;

    // Data
    // MODULE PARAMETER DEFINITIONS

    static std::string const BlankString;

    int const ControllerSimple(1);
    int const ControllerOutsideAir(2);
    int const ControllerStandAloneERV(3);

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:

    int NumStandAloneERVs; // Total number of stand alone ERVs defined in the idf

    Array1D_bool MySizeFlag;
    Array1D_bool CheckEquipName;
    bool GetERVInputFlag(true); // First time, input is "gotten"

    // SUBROUTINE SPECIFICATIONS FOR MODULE

    // Driver/Manager Routine

    // Algorithms/Calculation routine for the module

    // Get Input routine for module

    // Sizing routine for the module

    // Initialization routine for module

    // Utility routines for module

    // Object Data
    Array1D<StandAloneERVData> StandAloneERV;
    std::unordered_set<std::string> HeatExchangerUniqueNames;
    std::unordered_set<std::string> SupplyAirFanUniqueNames;
    std::unordered_set<std::string> ExhaustAirFanUniqueNames;
    std::unordered_set<std::string> ControllerUniqueNames;

    // Functions

    void clear_state()
    {
        NumStandAloneERVs = 0;
        GetERVInputFlag = true;
        MySizeFlag.deallocate();
        CheckEquipName.deallocate();
        StandAloneERV.deallocate();
        HeatExchangerUniqueNames.clear();
        SupplyAirFanUniqueNames.clear();
        ExhaustAirFanUniqueNames.clear();
        ControllerUniqueNames.clear();
    }

    void SimStandAloneERV(std::string const &CompName,   // name of the Stand Alone ERV unit
                          int const ZoneNum,             // number of zone being served unused1208
                          bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                          Real64 &SensLoadMet,           // net sensible load supplied by the ERV unit to the zone (W)
                          Real64 &LatLoadMet,            // net latent load supplied by ERV unit to the zone (kg/s),
                          int &CompIndex                 // pointer to correct component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2003
        //       MODIFIED       Don Shirey, Aug 2009 (LatLoadMet)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the simulation of a Stand Alone ERV unit. Called from SimZoneEquipment

        // Using/Aliasing
        using General::TrimSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // ZoneNum not used at this time, future modifications may require zone information
        // dehumid = negative

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int StandAloneERVNum; // index of Stand Alone ERV unit being simulated

        // First time SimStandAloneERV is called, get the input for all Stand Alone ERV units
        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        // Find the correct Stand Alone ERV unit index
        if (CompIndex == 0) {
            StandAloneERVNum = UtilityRoutines::FindItem(CompName, StandAloneERV);
            if (StandAloneERVNum == 0) {
                ShowFatalError("SimStandAloneERV: Unit not found=" + CompName);
            }
            CompIndex = StandAloneERVNum;
        } else {
            StandAloneERVNum = CompIndex;
            if (StandAloneERVNum > NumStandAloneERVs || StandAloneERVNum < 1) {
                ShowFatalError("SimStandAloneERV:  Invalid CompIndex passed=" + TrimSigDigits(StandAloneERVNum) +
                               ", Number of Units=" + TrimSigDigits(NumStandAloneERVs) + ", Entered Unit name=" + CompName);
            }
            if (CheckEquipName(StandAloneERVNum)) {
                if (CompName != StandAloneERV(StandAloneERVNum).Name) {
                    ShowFatalError("SimStandAloneERV: Invalid CompIndex passed=" + TrimSigDigits(StandAloneERVNum) + ", Unit name=" + CompName +
                                   ", stored Unit Name for that index=" + StandAloneERV(StandAloneERVNum).Name);
                }
                CheckEquipName(StandAloneERVNum) = false;
            }
        }

        // Initialize the Stand Alone ERV unit
        InitStandAloneERV(StandAloneERVNum, ZoneNum, FirstHVACIteration);

        CalcStandAloneERV(StandAloneERVNum, FirstHVACIteration, SensLoadMet, LatLoadMet);

        ReportStandAloneERV(StandAloneERVNum);
    }

    void GetStandAloneERV()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   June 2003
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for Stand Alone ERV units and stores it in the Stand Alone ERV data structure

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using DataHeatBalance::Zone;
        using DataSizing::AutoSize;
        using DataZoneControls::HumidityControlZone;
        using DataZoneControls::NumHumidityControlZones;
        using DataZoneEquipment::ZoneEquipConfig;
        using Fans::GetFanAvailSchPtr;
        using Fans::GetFanDesignVolumeFlowRate;
        using Fans::GetFanIndex;
        using Fans::GetFanOutletNode;
        using Fans::GetFanType;
        using General::RoundSigDigits;
        using MixedAir::OAController;
        using NodeInputManager::GetOnlySingleNode;
        auto &GetGenericSupplyAirFlowRate(HeatRecovery::GetSupplyAirFlowRate);
        using HeatRecovery::GetHeatExchangerObjectTypeNum;
        auto &GetHXSupplyInletNode(HeatRecovery::GetSupplyInletNode);
        auto &GetHXSecondaryInletNode(HeatRecovery::GetSecondaryInletNode);
        using CurveManager::GetCurveIndex;
        using OutAirNodeManager::CheckOutAirNodeNumber;
        using namespace DataIPShortCuts;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int StandAloneERVIndex;  // loop index
        int StandAloneERVNum;    // current Stand Alone ERV number
        Array1D_string Alphas;   // Alpha items for object
        Array1D<Real64> Numbers; // Numeric items for object
        Array1D_string cAlphaFields;
        Array1D_string cNumericFields;
        Array1D_bool lAlphaBlanks;
        Array1D_bool lNumericBlanks;
        std::string CompSetSupplyFanInlet;
        std::string CompSetSupplyFanOutlet;
        std::string CompSetExhaustFanInlet;
        std::string CompSetExhaustFanOutlet;
        std::string CurrentModuleObject; // Object type for getting and error messages
        int SAFanTypeNum;                // Integer equivalent to fan type
        int EAFanTypeNum;                // Integer equivalent to fan type
        int NumArg;
        int NumAlphas;                    // Number of Alphas for each GetObjectItem call
        int NumNumbers;                   // Number of Numbers for each GetObjectItem call
        int MaxAlphas;                    // Max between the two objects gotten here
        int MaxNumbers;                   // Max between the two objects gotten here
        int IOStatus;                     // Used in GetObjectItem
        static bool ErrorsFound(false);   // Set to true if errors in input, fatal at end of routine
        int NumERVCtrlrs;                 // total number of CONTROLLER:STAND ALONE ERV objects
        int ERVControllerNum;             // index to ERV controller
        int WhichERV;                     // used in controller GetInput
        Real64 AirFlowRate;               // used to find zone with humidistat
        int NodeNumber;                   // used to find zone with humidistat
        int HStatZoneNum;                 // used to find zone with humidistat
        int NumHstatZone;                 // index to humidity controlled zones
        static int ControlledZoneNum(0);  // used to find zone with humidistat
        static bool ZoneNodeFound(false); // used to find zone with humidistat
        static bool HStatFound(false);    // used to find zone with humidistat
        bool errFlag;                     // Error flag used in mining calls
        Real64 SAFanVolFlowRate;          // supply air fan volumetric flow rate [m3/s]
        Real64 EAFanVolFlowRate;          // exhaust air fan volumetric flow rate [m3/s]
        Real64 HXSupAirFlowRate;          // HX supply air flow rate [m3/s]
        Real64 HighRHOARatio;             // local variable for HighRHOAFlowRatio
        bool ZoneInletNodeFound;          // used for warning when zone node not listed in equipment connections
        bool ZoneExhaustNodeFound;        // used for warning when zone node not listed in equipment connections
        int ZoneInletCZN;                 // used for warning when zone node not listed in equipment connections
        int ZoneExhaustCZN;               // used for warning when zone node not listed in equipment connections

        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:EnergyRecoveryVentilator", NumArg, NumAlphas, NumNumbers);
        MaxAlphas = NumAlphas;
        MaxNumbers = NumNumbers;
        inputProcessor->getObjectDefMaxArgs("ZoneHVAC:EnergyRecoveryVentilator:Controller", NumArg, NumAlphas, NumNumbers);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        MaxNumbers = max(MaxNumbers, NumNumbers);

        Alphas.allocate(MaxAlphas);
        Numbers.dimension(MaxNumbers, 0.0);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNumbers);
        lNumericBlanks.dimension(MaxNumbers, false);
        lAlphaBlanks.dimension(MaxAlphas, false);

        GetERVInputFlag = false;

        // find the number of each type of Stand Alone ERV unit
        CurrentModuleObject = "ZoneHVAC:EnergyRecoveryVentilator";

        NumStandAloneERVs = inputProcessor->getNumObjectsFound(CurrentModuleObject);

        // allocate the data structures
        StandAloneERV.allocate(NumStandAloneERVs);
        HeatExchangerUniqueNames.reserve(static_cast<unsigned>(NumStandAloneERVs));
        SupplyAirFanUniqueNames.reserve(static_cast<unsigned>(NumStandAloneERVs));
        ExhaustAirFanUniqueNames.reserve(static_cast<unsigned>(NumStandAloneERVs));
        ControllerUniqueNames.reserve(static_cast<unsigned>(NumStandAloneERVs));
        CheckEquipName.dimension(NumStandAloneERVs, true);

        // loop over Stand Alone ERV units; get and load the input data
        for (StandAloneERVIndex = 1; StandAloneERVIndex <= NumStandAloneERVs; ++StandAloneERVIndex) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          StandAloneERVIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            StandAloneERVNum = StandAloneERVIndex; // separate variables in case other objects read by this module at some point later
            UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);
            StandAloneERV(StandAloneERVNum).Name = Alphas(1);
            StandAloneERV(StandAloneERVNum).UnitType = CurrentModuleObject;

            if (lAlphaBlanks(2)) {
                StandAloneERV(StandAloneERVNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                StandAloneERV(StandAloneERVNum).SchedPtr = GetScheduleIndex(Alphas(2)); // convert schedule name to pointer
                if (StandAloneERV(StandAloneERVNum).SchedPtr == 0) {
                    ShowSevereError(CurrentModuleObject + ", \"" + StandAloneERV(StandAloneERVNum).Name + "\" " + cAlphaFields(2) +
                                    " not found = " + Alphas(2));
                    ErrorsFound = true;
                }
            }

            GlobalNames::IntraObjUniquenessCheck(Alphas(3), CurrentModuleObject, cAlphaFields(3), HeatExchangerUniqueNames, ErrorsFound);
            StandAloneERV(StandAloneERVNum).HeatExchangerName = Alphas(3);
            errFlag = false;
            StandAloneERV(StandAloneERVNum).HeatExchangerTypeNum =
                GetHeatExchangerObjectTypeNum(StandAloneERV(StandAloneERVNum).HeatExchangerName, errFlag);
            if (errFlag) {
                ShowContinueError("... occurs in " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ErrorsFound = true;
            }

            errFlag = false;
            HXSupAirFlowRate = GetGenericSupplyAirFlowRate(StandAloneERV(StandAloneERVNum).HeatExchangerName, errFlag);
            if (errFlag) {
                ShowContinueError("... occurs in " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ErrorsFound = true;
            }
            StandAloneERV(StandAloneERVNum).DesignHXVolFlowRate = HXSupAirFlowRate;

            StandAloneERV(StandAloneERVNum).SupplyAirFanName = Alphas(4);
            GlobalNames::IntraObjUniquenessCheck(Alphas(4), CurrentModuleObject, cAlphaFields(4), SupplyAirFanUniqueNames, ErrorsFound);

            errFlag = false;
            if (HVACFan::checkIfFanNameIsAFanSystem(
                    StandAloneERV(StandAloneERVNum).SupplyAirFanName)) { // no object type in input, so check if Fan:SystemModel
                StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(StandAloneERV(StandAloneERVNum).SupplyAirFanName)); // call constructor
                StandAloneERV(StandAloneERVNum).SupplyAirFanIndex =
                    HVACFan::getFanObjectVectorIndex(StandAloneERV(StandAloneERVNum).SupplyAirFanName);
                StandAloneERV(StandAloneERVNum).SupplyAirFanSchPtr =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->availSchedIndex;
                StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->designAirVolFlowRate;
                StandAloneERV(StandAloneERVNum).SupplyAirOutletNode =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->outletNodeNum;
            } else {
                GetFanType(StandAloneERV(StandAloneERVNum).SupplyAirFanName,
                           SAFanTypeNum,
                           errFlag,
                           CurrentModuleObject,
                           StandAloneERV(StandAloneERVNum).Name);
                if (errFlag) {
                    ErrorsFound = true;
                }
                StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num = SAFanTypeNum;

                errFlag = false;
                StandAloneERV(StandAloneERVNum).SupplyAirFanSchPtr =
                    GetFanAvailSchPtr(cFanTypes(SAFanTypeNum), StandAloneERV(StandAloneERVNum).SupplyAirFanName, errFlag);
                if (errFlag) {
                    ShowContinueError("... occurs in " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ErrorsFound = true;
                }

                GetFanIndex(StandAloneERV(StandAloneERVNum).SupplyAirFanName,
                            StandAloneERV(StandAloneERVNum).SupplyAirFanIndex,
                            errFlag,
                            CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");

                // Set the SA Design Fan Volume Flow Rate
                // get from fan module
                errFlag = false;
                SAFanVolFlowRate = GetFanDesignVolumeFlowRate(cFanTypes(SAFanTypeNum), StandAloneERV(StandAloneERVNum).SupplyAirFanName, errFlag);
                if (errFlag) {
                    ShowContinueError("... occurs in " + CurrentModuleObject + " =" + StandAloneERV(StandAloneERVNum).Name);
                    ErrorsFound = true;
                }
                StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate = SAFanVolFlowRate;
                errFlag = false;
                StandAloneERV(StandAloneERVNum).SupplyAirOutletNode =
                    GetFanOutletNode(cFanTypes(SAFanTypeNum), StandAloneERV(StandAloneERVNum).SupplyAirFanName, errFlag);
            }

            StandAloneERV(StandAloneERVNum).ExhaustAirFanName = Alphas(5);
            GlobalNames::IntraObjUniquenessCheck(Alphas(5), CurrentModuleObject, cAlphaFields(5), ExhaustAirFanUniqueNames, ErrorsFound);
            errFlag = false;
            if (HVACFan::checkIfFanNameIsAFanSystem(
                    StandAloneERV(StandAloneERVNum).ExhaustAirFanName)) { // no object type in input, so check if Fan:SystemModel
                StandAloneERV(StandAloneERVNum).ExhaustAirFanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(StandAloneERV(StandAloneERVNum).ExhaustAirFanName)); // call constructor

                StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex =
                    HVACFan::getFanObjectVectorIndex(StandAloneERV(StandAloneERVNum).ExhaustAirFanName);
                StandAloneERV(StandAloneERVNum).ExhaustAirFanSchPtr =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->availSchedIndex;
                StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->designAirVolFlowRate;
                StandAloneERV(StandAloneERVNum).ExhaustAirOutletNode =
                    HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->outletNodeNum;

            } else {
                GetFanType(StandAloneERV(StandAloneERVNum).ExhaustAirFanName,
                           EAFanTypeNum,
                           errFlag,
                           CurrentModuleObject,
                           StandAloneERV(StandAloneERVNum).Name);
                if (!errFlag) {
                    StandAloneERV(StandAloneERVNum).ExhaustAirFanType_Num = EAFanTypeNum;
                    // error for fan availability schedule?
                    StandAloneERV(StandAloneERVNum).ExhaustAirFanSchPtr =
                        GetFanAvailSchPtr(cFanTypes(EAFanTypeNum), StandAloneERV(StandAloneERVNum).ExhaustAirFanName, errFlag);
                    GetFanIndex(StandAloneERV(StandAloneERVNum).ExhaustAirFanName,
                                StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex,
                                errFlag,
                                CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                } else {
                    ErrorsFound = true;
                }

                // Set the EA Design Fan Volume Flow Rate
                // get from fan module
                errFlag = false;
                EAFanVolFlowRate = GetFanDesignVolumeFlowRate(cFanTypes(EAFanTypeNum), StandAloneERV(StandAloneERVNum).ExhaustAirFanName, errFlag);
                if (errFlag) {
                    ShowContinueError("... occurs in " + CurrentModuleObject + " =" + StandAloneERV(StandAloneERVNum).Name);
                    ErrorsFound = true;
                }
                StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate = EAFanVolFlowRate;

                StandAloneERV(StandAloneERVNum).ExhaustAirOutletNode =
                    GetFanOutletNode(cFanTypes(EAFanTypeNum), StandAloneERV(StandAloneERVNum).ExhaustAirFanName, errFlag);
                if (errFlag) {
                    ShowContinueError("... occurs in " + CurrentModuleObject + " =" + StandAloneERV(StandAloneERVNum).Name);
                    ErrorsFound = true;
                }
            }

            errFlag = false;
            StandAloneERV(StandAloneERVNum).SupplyAirInletNode = GetHXSupplyInletNode(StandAloneERV(StandAloneERVNum).HeatExchangerName, errFlag);
            StandAloneERV(StandAloneERVNum).ExhaustAirInletNode = GetHXSecondaryInletNode(StandAloneERV(StandAloneERVNum).HeatExchangerName, errFlag);
            if (errFlag) {
                ShowContinueError("... occurs in " + CurrentModuleObject + " =" + StandAloneERV(StandAloneERVNum).Name);
                ErrorsFound = true;
            }
            StandAloneERV(StandAloneERVNum).SupplyAirInletNode = GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum).SupplyAirInletNode),
                                                                                   ErrorsFound,
                                                                                   CurrentModuleObject,
                                                                                   Alphas(1),
                                                                                   NodeType_Air,
                                                                                   NodeConnectionType_Inlet,
                                                                                   1,
                                                                                   ObjectIsParent);
            StandAloneERV(StandAloneERVNum).SupplyAirOutletNode = GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum).SupplyAirOutletNode),
                                                                                    ErrorsFound,
                                                                                    CurrentModuleObject,
                                                                                    Alphas(1),
                                                                                    NodeType_Air,
                                                                                    NodeConnectionType_Outlet,
                                                                                    1,
                                                                                    ObjectIsParent);
            StandAloneERV(StandAloneERVNum).ExhaustAirInletNode = GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum).ExhaustAirInletNode),
                                                                                    ErrorsFound,
                                                                                    CurrentModuleObject,
                                                                                    Alphas(1),
                                                                                    NodeType_Air,
                                                                                    NodeConnectionType_Inlet,
                                                                                    2,
                                                                                    ObjectIsParent);
            StandAloneERV(StandAloneERVNum).ExhaustAirOutletNode = GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum).ExhaustAirOutletNode),
                                                                                     ErrorsFound,
                                                                                     CurrentModuleObject,
                                                                                     Alphas(1),
                                                                                     NodeType_Air,
                                                                                     NodeConnectionType_ReliefAir,
                                                                                     2,
                                                                                     ObjectIsParent);

            //   Check that supply air inlet node is an OA node
            if (!CheckOutAirNodeNumber(StandAloneERV(StandAloneERVNum).SupplyAirInletNode)) {
                ShowSevereError("For " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError(" Node name of supply air inlet node not valid Outdoor Air Node = " +
                                  NodeID(StandAloneERV(StandAloneERVNum).SupplyAirInletNode));
                ShowContinueError("...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }

            //   Check to make sure inlet and exhaust nodes are listed in a ZoneHVAC:EquipmentConnections object
            ZoneInletNodeFound = false;
            ZoneExhaustNodeFound = false;
            for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                if (!ZoneInletNodeFound) {
                    for (NodeNumber = 1; NodeNumber <= ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++NodeNumber) {
                        if (ZoneEquipConfig(ControlledZoneNum).InletNode(NodeNumber) == StandAloneERV(StandAloneERVNum).SupplyAirOutletNode) {
                            ZoneInletNodeFound = true;
                            ZoneInletCZN = ControlledZoneNum;
                            break; // found zone inlet node
                        }
                    }
                }
                if (!ZoneExhaustNodeFound) {
                    for (NodeNumber = 1; NodeNumber <= ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++NodeNumber) {
                        if (ZoneEquipConfig(ControlledZoneNum).ExhaustNode(NodeNumber) == StandAloneERV(StandAloneERVNum).ExhaustAirInletNode) {
                            ZoneExhaustNodeFound = true;
                            ZoneExhaustCZN = ControlledZoneNum;
                            break; // found zone exhaust node
                        }
                    }
                }
            }
            if (!ZoneInletNodeFound) {
                ShowSevereError("For " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... Node name of supply air outlet node does not appear in a ZoneHVAC:EquipmentConnections object.");
                ShowContinueError("... Supply air outlet node = " + NodeID(StandAloneERV(StandAloneERVNum).SupplyAirOutletNode));
                ErrorsFound = true;
            }
            if (!ZoneExhaustNodeFound) {
                ShowSevereError("For " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... Node name of exhaust air inlet node does not appear in a ZoneHVAC:EquipmentConnections object.");
                ShowContinueError("... Exhaust air inlet node = " + NodeID(StandAloneERV(StandAloneERVNum).ExhaustAirInletNode));
                ErrorsFound = true;
            }
            //   If nodes are found, make sure they are in the same zone
            if (ZoneInletNodeFound && ZoneExhaustNodeFound) {
                if (ZoneInletCZN != ZoneExhaustCZN) {
                    ShowSevereError("For " + CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ShowContinueError("... Node name of supply air outlet node and exhasut air inlet node must appear in the same "
                                      "ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError("... Supply air outlet node = " + NodeID(StandAloneERV(StandAloneERVNum).SupplyAirOutletNode));
                    ShowContinueError("... ZoneHVAC:EquipmentConnections Zone Name = " + ZoneEquipConfig(ZoneInletCZN).ZoneName);
                    ShowContinueError("... Exhaust air inlet node = " + NodeID(StandAloneERV(StandAloneERVNum).ExhaustAirInletNode));
                    ShowContinueError("... ZoneHVAC:EquipmentConnections Zone Name = " + ZoneEquipConfig(ZoneExhaustCZN).ZoneName);
                    ErrorsFound = true;
                }
            }

            StandAloneERV(StandAloneERVNum).ControllerName = Alphas(6);
            // If controller name is blank the ERV unit will operate with no controller
            if (lAlphaBlanks(6)) {
                StandAloneERV(StandAloneERVNum).ControllerName = "xxxxx";
                StandAloneERV(StandAloneERVNum).ControllerNameDefined = false;
            } else {
                // Verify controller name in Stand Alone ERV object matches name of valid controller object
                GlobalNames::IntraObjUniquenessCheck(Alphas(6), CurrentModuleObject, cAlphaFields(6), ControllerUniqueNames, ErrorsFound);
                StandAloneERV(StandAloneERVNum).ControllerNameDefined = true;
                if (ErrorsFound) {
                    StandAloneERV(StandAloneERVNum).ControllerNameDefined = false;
                }

                if (inputProcessor->getObjectItemNum("ZoneHVAC:EnergyRecoveryVentilator:Controller",
                                                     StandAloneERV(StandAloneERVNum).ControllerName) <= 0) {
                    ShowSevereError(CurrentModuleObject + " controller type ZoneHVAC:EnergyRecoveryVentilator:Controller not found = " + Alphas(6));
                    ErrorsFound = true;
                    StandAloneERV(StandAloneERVNum).ControllerNameDefined = false;
                }
            }

            if (!lAlphaBlanks(7)) {
                StandAloneERV(StandAloneERVNum).AvailManagerListName = Alphas(7);
            }

            // Read supply and exhaust air flow rates
            StandAloneERV(StandAloneERVNum).SupplyAirVolFlow = Numbers(1);
            StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow = Numbers(2);

            // Read ventilation rate per floor area for autosizing HX and fans
            StandAloneERV(StandAloneERVNum).AirVolFlowPerFloorArea = Numbers(3);
            StandAloneERV(StandAloneERVNum).AirVolFlowPerOccupant = Numbers(4);

            if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow == AutoSize && StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate != AutoSize) {
                ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... When autosizing ERV, supply air fan = " + cFanTypes(SAFanTypeNum) + " \"" +
                                  StandAloneERV(StandAloneERVNum).SupplyAirFanName + "\" must also be autosized.");
            }

            if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow == AutoSize && StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate != AutoSize) {
                ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... When autosizing ERV, exhaust air fan = " + cFanTypes(EAFanTypeNum) + " \"" +
                                  StandAloneERV(StandAloneERVNum).ExhaustAirFanName + "\" must also be autosized.");
            }

            if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow == AutoSize && HXSupAirFlowRate != AutoSize) {
                ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... When autosizing ERV " + cNumericFields(1) + ", nominal supply air flow rate for heat exchanger with name = " +
                                  StandAloneERV(StandAloneERVNum).HeatExchangerName + " must also be autosized.");
            }

            if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow == AutoSize && HXSupAirFlowRate != AutoSize) {
                ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                ShowContinueError("... When autosizing ERV " + cNumericFields(2) + ", nominal supply air flow rate for heat exchanger with name = " +
                                  StandAloneERV(StandAloneERVNum).HeatExchangerName + " must also be autosized.");
            }

            // Compare the ERV SA flow rates to SA fan object.
            if (StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate != AutoSize && StandAloneERV(StandAloneERVNum).SupplyAirVolFlow != AutoSize) {
                if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow > StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate) {
                    ShowWarningError(CurrentModuleObject + " = " + StandAloneERV(StandAloneERVNum).Name + " has a " + cNumericFields(1) +
                                     " > Max Volume Flow Rate defined in the associated fan object, should be <=");
                    ShowContinueError("... Entered value=" + RoundSigDigits(StandAloneERV(StandAloneERVNum).SupplyAirVolFlow, 2) + "... Fan [" +
                                      cFanTypes(SAFanTypeNum) + " \"" + StandAloneERV(StandAloneERVNum).SupplyAirFanName +
                                      "\"] Max Value = " + RoundSigDigits(StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate, 2));
                    ShowContinueError(" The ERV " + cNumericFields(1) + " is reset to the supply air fan flow rate and the simulation continues.");
                    StandAloneERV(StandAloneERVNum).SupplyAirVolFlow = StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate;
                }
            }
            if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow != AutoSize) {
                if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow <= 0.0) {
                    ShowSevereError(CurrentModuleObject + " = " + StandAloneERV(StandAloneERVNum).Name + " has a " + cNumericFields(1) +
                                    " <= 0.0, it must be >0.0");
                    ShowContinueError("... Entered value=" + RoundSigDigits(StandAloneERV(StandAloneERVNum).SupplyAirVolFlow, 2));
                    ErrorsFound = true;
                }
            } else {
                if (StandAloneERV(StandAloneERVNum).AirVolFlowPerFloorArea == 0.0 && StandAloneERV(StandAloneERVNum).AirVolFlowPerOccupant == 0.0) {
                    ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ShowContinueError("... Autosizing " + cNumericFields(1) + " requires at least one input for " + cNumericFields(3) + " or " +
                                      cNumericFields(4) + '.');
                    ErrorsFound = true;
                }
                // both inputs must be autosized
                if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ShowContinueError("... When autosizing, " + cNumericFields(1) + " and " + cNumericFields(2) + " must both be autosized.");
                    ErrorsFound = true;
                }
            }

            // Compare the ERV EA flow rates to EA fan object.
            if (StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate != AutoSize && StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow != AutoSize) {
                if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow > StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate) {
                    ShowWarningError(CurrentModuleObject + " = " + StandAloneERV(StandAloneERVNum).Name + " has an " + cNumericFields(2) +
                                     " > Max Volume Flow Rate defined in the associated fan object, should be <=");
                    ShowContinueError("... Entered value=" + RoundSigDigits(StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow, 2) + "... Fan [" +
                                      cFanTypes(EAFanTypeNum) + ':' + StandAloneERV(StandAloneERVNum).ExhaustAirFanName +
                                      "] Max Value = " + RoundSigDigits(StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate, 2));
                    ShowContinueError(" The ERV " + cNumericFields(2) + " is reset to the exhaust air fan flow rate and the simulation continues.");
                    StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow = StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate;
                }
            }
            if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow != AutoSize) {
                if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow <= 0.0) {
                    ShowSevereError(CurrentModuleObject + " = " + StandAloneERV(StandAloneERVNum).Name + " has an " + cNumericFields(2) +
                                    " <= 0.0, it must be >0.0");
                    ShowContinueError("... Entered value=" + RoundSigDigits(StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow, 2));
                    ErrorsFound = true;
                }
            } else {
                if (StandAloneERV(StandAloneERVNum).AirVolFlowPerFloorArea == 0.0 && StandAloneERV(StandAloneERVNum).AirVolFlowPerOccupant == 0.0) {
                    ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ShowContinueError("... Autosizing " + cNumericFields(2) + " requires at least one input for " + cNumericFields(3) + " or " +
                                      cNumericFields(4) + '.');
                    ErrorsFound = true;
                }
                if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow != AutoSize) {
                    ShowSevereError(CurrentModuleObject + " \"" + StandAloneERV(StandAloneERVNum).Name + "\"");
                    ShowContinueError("... When autosizing, " + cNumericFields(1) + " and " + cNumericFields(2) + " must both be autosized.");
                    ErrorsFound = true;
                }
            }

            // Add supply fan to component sets array
            CompSetSupplyFanInlet = "UNDEFINED";
            CompSetSupplyFanOutlet = NodeID(StandAloneERV(StandAloneERVNum).SupplyAirOutletNode);

            // Add exhaust fan to component sets array
            CompSetExhaustFanInlet = "UNDEFINED";
            CompSetExhaustFanOutlet = NodeID(StandAloneERV(StandAloneERVNum).ExhaustAirOutletNode);

            // Add HX to component sets array
            SetUpCompSets(StandAloneERV(StandAloneERVNum).UnitType,
                          StandAloneERV(StandAloneERVNum).Name,
                          "UNDEFINED",
                          StandAloneERV(StandAloneERVNum).HeatExchangerName,
                          "UNDEFINED",
                          "UNDEFINED");

            // Add supply fan to component sets array
            SetUpCompSets(StandAloneERV(StandAloneERVNum).UnitType,
                          StandAloneERV(StandAloneERVNum).Name,
                          "UNDEFINED",
                          StandAloneERV(StandAloneERVNum).SupplyAirFanName,
                          CompSetSupplyFanInlet,
                          CompSetSupplyFanOutlet);

            // Add exhaust fan to component sets array
            SetUpCompSets(StandAloneERV(StandAloneERVNum).UnitType,
                          StandAloneERV(StandAloneERVNum).Name,
                          "UNDEFINED",
                          StandAloneERV(StandAloneERVNum).ExhaustAirFanName,
                          CompSetExhaustFanInlet,
                          CompSetExhaustFanOutlet);

            // Verify HX name in Stand Alone ERV object matches name of valid HX object
            if (inputProcessor->getObjectItemNum("HeatExchanger:AirToAir:SensibleAndLatent", StandAloneERV(StandAloneERVNum).HeatExchangerName) <=
                0) {
                ShowSevereError(CurrentModuleObject + " heat exchanger type HeatExchanger:AirToAir:SensibleAndLatent not found = " +
                                StandAloneERV(StandAloneERVNum).HeatExchangerName);
                ErrorsFound = true;
            }
            // Verify supply air fan name in Stand Alone ERV object matches name of valid fan object
            if (StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                if (inputProcessor->getObjectItemNum("Fan:OnOff", StandAloneERV(StandAloneERVNum).SupplyAirFanName) <= 0) {
                    ShowSevereError(CurrentModuleObject +
                                    " supply fan type Fan:OnOff not found = " + StandAloneERV(StandAloneERVNum).SupplyAirFanName);
                    ErrorsFound = true;
                }
            } else {
                if (inputProcessor->getObjectItemNum("Fan:SystemModel", StandAloneERV(StandAloneERVNum).SupplyAirFanName) <= 0) {
                    ShowSevereError(CurrentModuleObject +
                                    " supply fan type Fan:SystemModel not found = " + StandAloneERV(StandAloneERVNum).SupplyAirFanName);
                    ErrorsFound = true;
                }
            }

            // Verify exhaust air fan name in Stand Alone ERV object matches name of valid fan object
            if (StandAloneERV(StandAloneERVNum).ExhaustAirFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                if (inputProcessor->getObjectItemNum("Fan:OnOff", StandAloneERV(StandAloneERVNum).ExhaustAirFanName) <= 0) {
                    ShowSevereError(CurrentModuleObject +
                                    " exhaust fan type Fan:OnOff not found = " + StandAloneERV(StandAloneERVNum).ExhaustAirFanName);
                    ErrorsFound = true;
                }
            } else {
                if (inputProcessor->getObjectItemNum("Fan:SystemModel", StandAloneERV(StandAloneERVNum).ExhaustAirFanName) <= 0) {
                    ShowSevereError(CurrentModuleObject +
                                    " exhaust fan type Fan:SystemModel not found = " + StandAloneERV(StandAloneERVNum).ExhaustAirFanName);
                    ErrorsFound = true;
                }
            }
        }

        int OutAirNum = 0;
        CurrentModuleObject = "ZoneHVAC:EnergyRecoveryVentilator:Controller";
        NumERVCtrlrs = inputProcessor->getNumObjectsFound(CurrentModuleObject);

        for (ERVControllerNum = 1; ERVControllerNum <= NumERVCtrlrs; ++ERVControllerNum) {
            inputProcessor->getObjectItem(CurrentModuleObject,
                                          ERVControllerNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);
            MixedAir::CheckOAControllerName(Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            ++OutAirNum;
            auto &thisOAController(OAController(OutAirNum));

            thisOAController.Name = Alphas(1);
            thisOAController.ControllerType = CurrentModuleObject;
            thisOAController.ControllerType_Num = ControllerStandAloneERV;
            WhichERV = UtilityRoutines::FindItemInList(Alphas(1), StandAloneERV, &StandAloneERVData::ControllerName);
            if (WhichERV != 0) {
                AirFlowRate = StandAloneERV(WhichERV).SupplyAirVolFlow;
                StandAloneERV(WhichERV).ControllerIndex = OutAirNum;
            } else {
                ShowSevereError("GetERVController: Could not find ZoneHVAC:EnergyRecoveryVentilator with " + cAlphaFields(1) + " = \"" + Alphas(1) +
                                "\"");
                ErrorsFound = true;
                AirFlowRate = -1000.0;
            }
            thisOAController.MaxOA = AirFlowRate;
            thisOAController.MinOA = AirFlowRate;
            //    OAController(OutAirNum)%TempLim = Numbers(1)
            if (lNumericBlanks(1)) {
                thisOAController.TempLim = BlankNumeric;
            } else {
                thisOAController.TempLim = Numbers(1);
            }
            //    OAController(OutAirNum)%TempLowLim = Numbers(2)
            if (lNumericBlanks(2)) {
                thisOAController.TempLowLim = BlankNumeric;
            } else {
                thisOAController.TempLowLim = Numbers(2);
            }
            //    OAController(OutAirNum)%EnthLim = Numbers(3)
            if (lNumericBlanks(3)) {
                thisOAController.EnthLim = BlankNumeric;
            } else {
                thisOAController.EnthLim = Numbers(3);
            }
            //    OAController(OutAirNum)%DPTempLim = Numbers(4)
            if (lNumericBlanks(4)) {
                thisOAController.DPTempLim = BlankNumeric;
            } else {
                thisOAController.DPTempLim = Numbers(4);
            }

            if (WhichERV != 0) {
                NodeNumber = StandAloneERV(WhichERV).SupplyAirInletNode;
            } else {
                NodeNumber = 0;
            }
            thisOAController.OANode = NodeNumber;
            // set the inlet node to also equal the OA node because this is a special controller for economizing stand alone ERV
            // with the assumption that equipment is bypassed....(moved from module MixedAir)
            thisOAController.InletNode = NodeNumber;

            if (WhichERV != 0) {
                NodeNumber = StandAloneERV(WhichERV).ExhaustAirInletNode;
            } else {
                NodeNumber = 0;
            }
            thisOAController.RetNode = NodeNumber;

            if (!lAlphaBlanks(2)) {
                thisOAController.EnthalpyCurvePtr = GetCurveIndex(Alphas(2));
                if (GetCurveIndex(Alphas(2)) == 0) {
                    ShowSevereError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("..." + cAlphaFields(2) + " not found:" + Alphas(2));
                    ErrorsFound = true;
                } else {
                    // Verify Curve Object, only legal types are Quadratic and Cubic
                    ErrorsFound |= CurveManager::CheckCurveDims(thisOAController.EnthalpyCurvePtr, // Curve index
                                                                {1},                               // Valid dimensions
                                                                "GetStandAloneERV: ",              // Routine name
                                                                CurrentModuleObject,               // Object Type
                                                                thisOAController.Name,             // Object Name
                                                                cAlphaFields(2));                  // Field Name
                }
            }

            // Changed by AMIT for new implementation of the controller:outside air
            if (Alphas(3) == "EXHAUSTAIRTEMPERATURELIMIT" && Alphas(4) == "EXHAUSTAIRENTHALPYLIMIT") {
                thisOAController.Econo = MixedAir::DifferentialDryBulbAndEnthalpy;
            } else if (Alphas(3) == "EXHAUSTAIRTEMPERATURELIMIT" && Alphas(4) == "NOEXHAUSTAIRENTHALPYLIMIT") {
                thisOAController.Econo = MixedAir::DifferentialDryBulb;
            } else if (Alphas(3) == "NOEXHAUSTAIRTEMPERATURELIMIT" && Alphas(4) == "EXHAUSTAIRENTHALPYLIMIT") {
                thisOAController.Econo = MixedAir::DifferentialEnthalpy;
            } else if (Alphas(3) == "NOEXHAUSTAIRTEMPERATURELIMIT" && Alphas(4) == "NOEXHAUSTAIRENTHALPYLIMIT") {
                if ((!lNumericBlanks(1)) || (!lNumericBlanks(3)) || (!lNumericBlanks(4)) || (!lAlphaBlanks(2))) {
                    // This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
                    // ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
                    thisOAController.Econo = MixedAir::FixedDryBulb;
                }
            } else if ((!lAlphaBlanks(3)) && (!lAlphaBlanks(4))) {
                if ((lNumericBlanks(1)) && (lNumericBlanks(3)) && (lNumericBlanks(4)) && lAlphaBlanks(2)) {
                    ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("... Invalid " + cAlphaFields(3) + cAlphaFields(4) + " = " + Alphas(3) + Alphas(4));
                    ShowContinueError("... Assumed NO EXHAUST AIR TEMP LIMIT and NO EXHAUST AIR ENTHALPY LIMIT.");
                    thisOAController.Econo = MixedAir::NoEconomizer;
                } else {
                    // This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
                    // ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
                    thisOAController.Econo = MixedAir::FixedDryBulb;
                }
            } else if ((lAlphaBlanks(3)) && (!lAlphaBlanks(4))) {
                if ((lNumericBlanks(1)) && (lNumericBlanks(3)) && (lNumericBlanks(4)) && lAlphaBlanks(2)) {
                    ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("... Invalid " + cAlphaFields(4) + " = " + Alphas(4));
                    ShowContinueError("... Assumed  NO EXHAUST AIR ENTHALPY LIMIT.");
                    thisOAController.Econo = MixedAir::NoEconomizer;
                } else {
                    // This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
                    // ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
                    thisOAController.Econo = MixedAir::FixedDryBulb;
                }
            } else if ((!lAlphaBlanks(3)) && (lAlphaBlanks(4))) {
                if ((lNumericBlanks(1)) && (lNumericBlanks(3)) && (lNumericBlanks(4)) && lAlphaBlanks(2)) {
                    ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("... Invalid " + cAlphaFields(3) + " = " + Alphas(3));
                    ShowContinueError("... Assumed NO EXHAUST AIR TEMP LIMIT ");
                    thisOAController.Econo = MixedAir::NoEconomizer;
                } else {
                    // This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
                    // ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
                    thisOAController.Econo = MixedAir::FixedDryBulb;
                }
            } else { // NO Economizer
                thisOAController.Econo = MixedAir::NoEconomizer;
            }

            thisOAController.FixedMin = false;
            thisOAController.EconBypass = true;

            //   Initialize to one in case high humidity control is NOT used
            HighRHOARatio = 1.0;
            //   READ Modify Air Flow Data
            //   High humidity control option is YES, read in additional data
            if (UtilityRoutines::SameString(Alphas(6), "Yes")) {

                HStatZoneNum = UtilityRoutines::FindItemInList(Alphas(7), Zone);
                thisOAController.HumidistatZoneNum = HStatZoneNum;

                // Get the node number for the zone with the humidistat
                if (HStatZoneNum > 0) {
                    ZoneNodeFound = false;
                    HStatFound = false;
                    for (ControlledZoneNum = 1; ControlledZoneNum <= NumOfZones; ++ControlledZoneNum) {
                        if (ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != HStatZoneNum) continue;
                        //         Find the controlled zone number for the specified humidistat location
                        thisOAController.NodeNumofHumidistatZone = ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                        ZoneNodeFound = true;
                        break; // found zone node
                    }
                    if (!ZoneNodeFound) {
                        ShowSevereError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError("... Did not find Air Node (Zone with Humidistat)");
                        ShowContinueError("... Specified " + cAlphaFields(7) + " = " + Alphas(7));
                        ShowContinueError("... A ZoneHVAC:EquipmentConnections object must be specified for this zone.");
                        ErrorsFound = true;
                    } else {
                        for (NumHstatZone = 1; NumHstatZone <= NumHumidityControlZones; ++NumHstatZone) {
                            if (HumidityControlZone(NumHstatZone).ActualZoneNum != HStatZoneNum) continue;
                            HStatFound = true;
                            break;
                        }
                        if (!HStatFound) {
                            ShowSevereError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                            ShowContinueError("... Did not find zone humidistat");
                            ShowContinueError("... A ZoneControl:Humidistat object must be specified for this zone.");
                            ErrorsFound = true;
                        }
                    }
                } else {
                    ShowSevereError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("... Did not find Air Node (Zone with Humidistat)");
                    ShowContinueError("... A ZoneHVAC:EquipmentConnections object must be specified for this zone.");
                    ErrorsFound = true;
                }

                if (Numbers(5) <= 0.0 && NumNumbers > 4) {

                    ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                    ShowContinueError("... " + cNumericFields(5) + " must be greater than 0.");
                    ShowContinueError("... " + cNumericFields(5) + " is reset to 1 and the simulation continues.");

                    HighRHOARatio = 1.0;

                } else if (NumNumbers > 4) {

                    HighRHOARatio = Numbers(5);

                } else {

                    HighRHOARatio = 1.0;
                }

                if (UtilityRoutines::SameString(Alphas(8), "Yes")) {
                    thisOAController.ModifyDuringHighOAMoisture = false;
                } else {
                    thisOAController.ModifyDuringHighOAMoisture = true;
                }

            } else if (!UtilityRoutines::SameString(Alphas(6), "No") && NumAlphas > 4 && (!lAlphaBlanks(5))) {
                ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                ShowContinueError("... Invalid " + cAlphaFields(6) + " = " + Alphas(6));
                ShowContinueError("... " + cAlphaFields(6) + " is assumed to be \"No\" and the simulation continues.");
            } // IF(UtilityRoutines::SameString(Alphas(6),'Yes'))THEN

            thisOAController.HighRHOAFlowRatio = HighRHOARatio;
            if (WhichERV != 0) {
                StandAloneERV(WhichERV).HighRHOAFlowRatio = HighRHOARatio;
            }

            //   Check for a time of day outside air schedule
            thisOAController.EconomizerOASchedPtr = GetScheduleIndex(Alphas(5));

            if (WhichERV != 0) {
                StandAloneERV(WhichERV).EconomizerOASchedPtr = GetScheduleIndex(Alphas(5));

                // Compare the ERV SA fan flow rates to modified air flow rate.
                if (HighRHOARatio > 1.0 && StandAloneERV(WhichERV).SupplyAirVolFlow != AutoSize &&
                    StandAloneERV(WhichERV).DesignSAFanVolFlowRate != AutoSize) {
                    if (StandAloneERV(WhichERV).SupplyAirVolFlow * HighRHOARatio > StandAloneERV(WhichERV).DesignSAFanVolFlowRate) {
                        ShowWarningError(CurrentModuleObject + " \"" + Alphas(1) + "\"");
                        ShowContinueError("... A " + cNumericFields(5) + " was entered as " + RoundSigDigits(HighRHOARatio, 4));
                        ShowContinueError("... This flow ratio results in a Supply Air Volume Flow Rate through the ERV which is greater than the "
                                          "Max Volume specified in the supply air fan object.");
                        ShowContinueError("... Associated fan object = " + cFanTypes(SAFanTypeNum) + " \"" +
                                          StandAloneERV(WhichERV).SupplyAirFanName + "\"");
                        ShowContinueError("... Modified value                   = " +
                                          RoundSigDigits(StandAloneERV(WhichERV).SupplyAirVolFlow * HighRHOARatio, 2));
                        ShowContinueError(" ... Supply Fan Max Volume Flow Rate = " +
                                          RoundSigDigits(StandAloneERV(WhichERV).DesignSAFanVolFlowRate, 2));
                        ShowContinueError("... The ERV supply air fan will limit the air flow through the ERV and the simulation continues.");
                    }
                }

                // Compare the ERV EA fan flow rates to modified air flow rate.
                if (HighRHOARatio > 1.0 && StandAloneERV(WhichERV).ExhaustAirVolFlow != AutoSize &&
                    StandAloneERV(WhichERV).DesignEAFanVolFlowRate != AutoSize) {
                    if (StandAloneERV(WhichERV).ExhaustAirVolFlow * HighRHOARatio > StandAloneERV(WhichERV).DesignEAFanVolFlowRate) {
                        ShowWarningError("ZoneHVAC:EnergyRecoveryVentilator:Controller \"" + Alphas(1) + "\"");
                        ShowContinueError("... A " + cNumericFields(5) + " was entered as " + RoundSigDigits(HighRHOARatio, 4));
                        ShowContinueError("... This flow ratio results in an Exhaust Air Volume Flow Rate through the ERV which is greater than the "
                                          "Max Volume specified in the exhaust air fan object.");
                        ShowContinueError("... Associated fan object = " + cFanTypes(EAFanTypeNum) + " \"" +
                                          StandAloneERV(WhichERV).ExhaustAirFanName + "\"");
                        ShowContinueError("... Modified value                    = " +
                                          RoundSigDigits(StandAloneERV(WhichERV).ExhaustAirVolFlow * HighRHOARatio, 2));
                        ShowContinueError(" ... Exhaust Fan Max Volume Flow Rate = " +
                                          RoundSigDigits(StandAloneERV(WhichERV).DesignEAFanVolFlowRate, 2));
                        ShowContinueError("... The ERV exhaust air fan will limit the air flow through the ERV and the simulation continues.");
                    }
                }
            } // IF(WhichERV /= 0)THEN
        }

        if (ErrorsFound) {
            ShowFatalError("Errors found in getting ZoneHVAC:EnergyRecoveryVentilator input.");
        }

        // Setup report variables for the stand alone ERVs
        for (StandAloneERVIndex = 1; StandAloneERVIndex <= NumStandAloneERVs; ++StandAloneERVIndex) {
            SetupOutputVariable("Zone Ventilator Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).SensCoolingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).SensCoolingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).LatCoolingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).LatCoolingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).TotCoolingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).TotCoolingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);

            SetupOutputVariable("Zone Ventilator Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).SensHeatingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).SensHeatingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Latent Heating Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).LatHeatingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Latent Heating Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).LatHeatingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Total Heating Rate",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).TotHeatingRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Total Heating Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).TotHeatingEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);

            SetupOutputVariable("Zone Ventilator Electric Power",
                                OutputProcessor::Unit::W,
                                StandAloneERV(StandAloneERVIndex).ElecUseRate,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Electric Energy",
                                OutputProcessor::Unit::J,
                                StandAloneERV(StandAloneERVIndex).ElecUseEnergy,
                                "System",
                                "Sum",
                                StandAloneERV(StandAloneERVIndex).Name);
            SetupOutputVariable("Zone Ventilator Supply Fan Availability Status",
                                OutputProcessor::Unit::None,
                                StandAloneERV(StandAloneERVIndex).AvailStatus,
                                "System",
                                "Average",
                                StandAloneERV(StandAloneERVIndex).Name);
        }

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lNumericBlanks.deallocate();
        lAlphaBlanks.deallocate();
    }

    void InitStandAloneERV(int const StandAloneERVNum,   // number of the current Stand Alone ERV unit being simulated
                           int const ZoneNum,            // number of zone being served unused1208
                           bool const FirstHVACIteration // TRUE if first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2003
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Stand Alone ERV unit information.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // USE STATEMENTS:
        //  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW
        // Using/Aliasing
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ERVStandAlone_Num;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using MixedAir::OAController;
        using MixedAir::SimOAController;

        // Locals
        static Array1D_bool MySizeFlag;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SupInNode;    // supply air inlet node number
        int ExhInNode;    // exhaust air inlet node number
        int SupInletNode; // supply air inlet node number for Stand Alone ERV 'StandAloneERVNum'
        Real64 RhoAir;    // air density at SupInNode, standard conditions (dry air @ 20C,actual elevation pressure)
        static bool MyOneTimeFlag(true);
        static Array1D_bool MyEnvrnFlag;
        static Array1D_bool MyZoneEqFlag;            // used to set up zone equipment availability managers
        static bool ZoneEquipmentListChecked(false); // True after the Zone Equipment List has been checked for items
        int Loop;                                    // loop counter

        // Do the one time initializations
        if (MyOneTimeFlag) {

            MyEnvrnFlag.allocate(NumStandAloneERVs);
            MySizeFlag.allocate(NumStandAloneERVs);
            MyZoneEqFlag.allocate(NumStandAloneERVs);
            MyEnvrnFlag = true;
            MySizeFlag = true;
            MyZoneEqFlag = true;
            MyOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (MyZoneEqFlag(StandAloneERVNum)) { // initialize the name of each availability manager list and zone number
                ZoneComp(ERVStandAlone_Num).ZoneCompAvailMgrs(StandAloneERVNum).AvailManagerListName =
                    StandAloneERV(StandAloneERVNum).AvailManagerListName;
                ZoneComp(ERVStandAlone_Num).ZoneCompAvailMgrs(StandAloneERVNum).ZoneNum = ZoneNum;
                MyZoneEqFlag(StandAloneERVNum) = false;
            }
            StandAloneERV(StandAloneERVNum).AvailStatus = ZoneComp(ERVStandAlone_Num).ZoneCompAvailMgrs(StandAloneERVNum).AvailStatus;
        }

        // need to check all units to see if they are on Zone Equipment List or issue warning
        if (!ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= NumStandAloneERVs; ++Loop) {
                if (CheckZoneEquipmentList(StandAloneERV(Loop).UnitType, StandAloneERV(Loop).Name)) continue;
                ShowSevereError("InitStandAloneERV: Unit=[" + StandAloneERV(Loop).UnitType + ',' + StandAloneERV(Loop).Name +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!SysSizingCalc && MySizeFlag(StandAloneERVNum)) {
            SizeStandAloneERV(StandAloneERVNum);
            MySizeFlag(StandAloneERVNum) = false;
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag && MyEnvrnFlag(StandAloneERVNum)) {
            SupInNode = StandAloneERV(StandAloneERVNum).SupplyAirInletNode;
            ExhInNode = StandAloneERV(StandAloneERVNum).ExhaustAirInletNode;
            RhoAir = StdRhoAir;
            // set the mass flow rates from the input volume flow rates
            StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow = StdRhoAir * StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
            StandAloneERV(StandAloneERVNum).MaxExhAirMassFlow = StdRhoAir * StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow;
            StandAloneERV(StandAloneERVNum).DesignSAFanMassFlowRate = StdRhoAir * StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate;
            StandAloneERV(StandAloneERVNum).DesignEAFanMassFlowRate = StdRhoAir * StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate;
            // set the node max and min mass flow rates
            Node(SupInNode).MassFlowRateMax = StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow;
            Node(SupInNode).MassFlowRateMin = 0.0;
            Node(ExhInNode).MassFlowRateMax = StandAloneERV(StandAloneERVNum).MaxExhAirMassFlow;
            Node(ExhInNode).MassFlowRateMin = 0.0;
            MyEnvrnFlag(StandAloneERVNum) = false;
            //   Initialize OA Controller on BeginEnvrnFlag
            if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
                SimOAController(
                    StandAloneERV(StandAloneERVNum).ControllerName, StandAloneERV(StandAloneERVNum).ControllerIndex, FirstHVACIteration, 0);
            }
        } // end one time inits

        if (!BeginEnvrnFlag) {
            MyEnvrnFlag(StandAloneERVNum) = true;
        }

        // These initializations are done every iteration
        StandAloneERV(StandAloneERVNum).ElecUseRate = 0.0;
        StandAloneERV(StandAloneERVNum).SensCoolingRate = 0.0;
        StandAloneERV(StandAloneERVNum).LatCoolingRate = 0.0;
        StandAloneERV(StandAloneERVNum).TotCoolingRate = 0.0;
        StandAloneERV(StandAloneERVNum).SensHeatingRate = 0.0;
        StandAloneERV(StandAloneERVNum).LatHeatingRate = 0.0;
        StandAloneERV(StandAloneERVNum).TotHeatingRate = 0.0;
        SupInletNode = StandAloneERV(StandAloneERVNum).SupplyAirInletNode;
        ExhInNode = StandAloneERV(StandAloneERVNum).ExhaustAirInletNode;

        // Set the inlet node mass flow rate
        if (GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum).SchedPtr) > 0.0) {

            //   IF optional ControllerName is defined SimOAController ONLY to set economizer and Modifyairflow flags
            if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
                //     Initialize a flow rate for controller
                Node(SupInletNode).MassFlowRate = StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow;
                SimOAController(
                    StandAloneERV(StandAloneERVNum).ControllerName, StandAloneERV(StandAloneERVNum).ControllerIndex, FirstHVACIteration, 0);
            }

            if (GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum).SupplyAirFanSchPtr) > 0 || (ZoneCompTurnFansOn && !ZoneCompTurnFansOff)) {
                if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
                    if (OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).HighHumCtrlActive) {
                        Node(SupInletNode).MassFlowRate =
                            min(StandAloneERV(StandAloneERVNum).DesignSAFanMassFlowRate,
                                StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio);
                    } else {
                        Node(SupInletNode).MassFlowRate =
                            min(StandAloneERV(StandAloneERVNum).DesignSAFanMassFlowRate, StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow);
                    }
                } else {
                    Node(SupInletNode).MassFlowRate =
                        min(StandAloneERV(StandAloneERVNum).DesignSAFanMassFlowRate, StandAloneERV(StandAloneERVNum).MaxSupAirMassFlow);
                }
            } else {
                Node(SupInletNode).MassFlowRate = 0.0;
            }
            Node(SupInletNode).MassFlowRateMaxAvail = Node(SupInletNode).MassFlowRate;
            Node(SupInletNode).MassFlowRateMinAvail = Node(SupInletNode).MassFlowRate;

            if (GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum).ExhaustAirFanSchPtr) > 0) {
                if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
                    if (OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).HighHumCtrlActive) {
                        Node(ExhInNode).MassFlowRate =
                            min(StandAloneERV(StandAloneERVNum).DesignEAFanMassFlowRate,
                                StandAloneERV(StandAloneERVNum).MaxExhAirMassFlow * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio);
                    } else {
                        Node(ExhInNode).MassFlowRate =
                            min(StandAloneERV(StandAloneERVNum).DesignEAFanMassFlowRate, StandAloneERV(StandAloneERVNum).MaxExhAirMassFlow);
                    }
                } else {
                    Node(ExhInNode).MassFlowRate =
                        min(StandAloneERV(StandAloneERVNum).DesignEAFanMassFlowRate, StandAloneERV(StandAloneERVNum).MaxExhAirMassFlow);
                }
            } else {
                Node(ExhInNode).MassFlowRate = 0.0;
            }
            Node(ExhInNode).MassFlowRateMaxAvail = Node(ExhInNode).MassFlowRate;
            Node(ExhInNode).MassFlowRateMinAvail = Node(ExhInNode).MassFlowRate;
        } else {
            Node(SupInletNode).MassFlowRate = 0.0;
            Node(SupInletNode).MassFlowRateMaxAvail = 0.0;
            Node(SupInletNode).MassFlowRateMinAvail = 0.0;
            Node(ExhInNode).MassFlowRate = 0.0;
            Node(ExhInNode).MassFlowRateMaxAvail = 0.0;
            Node(ExhInNode).MassFlowRateMinAvail = 0.0;
        }
    }

    void SizeStandAloneERV(int const StandAloneERVNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   October 2007
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Stand Alone ERV Components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays.

        // Using/Aliasing
        using DataHeatBalance::People;
        using DataHeatBalance::TotPeople;
        using DataHeatBalance::Zone;
        using DataSizing::AutoSize;
        using DataSizing::AutoVsHardSizingThreshold;
        using DataSizing::CurZoneEqNum;
        using DataSizing::ZoneEqSizing;
        using DataZoneEquipment::ZoneEquipConfig;
        using Fans::SetFanData;
        using Fans::SimulateFanComponents;
        using General::RoundSigDigits;
        using HeatRecovery::SetHeatExchangerData;
        using MixedAir::OAController;
        using ReportSizingManager::ReportSizingOutput;
        using ScheduleManager::GetScheduleMaxValue;

        static std::string const RoutineName("SizeStandAloneERV: ");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;                       // Index to zone object
        int ActualZoneNum;                 // Actual zone number
        std::string ZoneName;              // Name of zone
        Real64 ZoneMult;                   // Zone multiplier
        int PeopleNum;                     // Index to people object
        Real64 NumberOfPeople;             // Maximum number of people in zone
        int PeopleSchPtr;                  // Pointer to people schedule
        Real64 MaxPeopleSch;               // maximum people schedule value
        Real64 FloorArea;                  // Floor area of zone (m2)
        bool IsAutoSize;                   // Indicator to autosize
        Real64 SupplyAirVolFlowDes;        // Autosized supply air flow for reporting
        Real64 SupplyAirVolFlowUser;       // Hardsized supply air flow for reporting
        Real64 DesignSAFanVolFlowRateDes;  // Autosized supply air fan flow for reporting
        Real64 DesignSAFanVolFlowRateUser; // Hardsized supply air fan flow for reporting
        Real64 ExhaustAirVolFlowDes;       // Autosized exhaust air flow for reporting
        Real64 ExhaustAirVolFlowUser;      // Hardsized exhaust air flow for reporting

        IsAutoSize = false;
        SupplyAirVolFlowDes = 0.0;
        SupplyAirVolFlowUser = 0.0;
        DesignSAFanVolFlowRateDes = 0.0;
        DesignSAFanVolFlowRateUser = 0.0;
        ExhaustAirVolFlowDes = 0.0;
        ExhaustAirVolFlowUser = 0.0;
        std::string CompType("ZoneHVAC:EnergyRecoveryVentilator");
        std::string CompName(StandAloneERV(StandAloneERVNum).Name);
        bool PrintFlag = true;
        int SizingMethod = AutoCalculateSizing;
        DataSizing::DataFractionUsedForSizing = 1.0;

        if (StandAloneERV(StandAloneERVNum).SupplyAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }

        if (CurZoneEqNum > 0) {

            //      Sizing objects are not required for stand alone ERV
            //      CALL CheckZoneSizing('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name)
            ZoneName = ZoneEquipConfig(CurZoneEqNum).ZoneName;
            ActualZoneNum = ZoneEquipConfig(CurZoneEqNum).ActualZoneNum;
            ZoneMult = Zone(ActualZoneNum).Multiplier * Zone(ActualZoneNum).ListMultiplier;
            FloorArea = 0.0;
            if (UtilityRoutines::SameString(ZoneName, Zone(ActualZoneNum).Name)) {
                FloorArea = Zone(ActualZoneNum).FloorArea;
            } else {
                for (ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum) {
                    if (!UtilityRoutines::SameString(ZoneName, Zone(ZoneNum).Name)) continue;
                    FloorArea = Zone(ZoneNum).FloorArea;
                    break;
                }
            }
            NumberOfPeople = 0.0;
            MaxPeopleSch = 0.0;
            for (PeopleNum = 1; PeopleNum <= TotPeople; ++PeopleNum) {
                if (ActualZoneNum != People(PeopleNum).ZonePtr) continue;
                PeopleSchPtr = People(PeopleNum).NumberOfPeoplePtr;
                MaxPeopleSch = GetScheduleMaxValue(PeopleSchPtr);
                NumberOfPeople = NumberOfPeople + (People(PeopleNum).NumberOfPeople * MaxPeopleSch);
            }
            SupplyAirVolFlowDes = FloorArea * StandAloneERV(StandAloneERVNum).AirVolFlowPerFloorArea +
                                  NumberOfPeople * StandAloneERV(StandAloneERVNum).AirVolFlowPerOccupant;
            SupplyAirVolFlowDes = ZoneMult * SupplyAirVolFlowDes;

            if (SupplyAirVolFlowDes < SmallAirVolFlow) {
                SupplyAirVolFlowDes = 0.0;
            }

            // Size ERV supply flow rate
            Real64 TempSize = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
            std::string SizingString = "Supply Air Flow Rate [m3/s]";
            if (IsAutoSize) {
                DataSizing::DataConstantUsedForSizing = SupplyAirVolFlowDes;
                TempSize = SupplyAirVolFlowDes;
                if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
                    OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).MaxOA =
                        SupplyAirVolFlowDes * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio;
                    OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).MinOA = SupplyAirVolFlowDes;
                }
            } else {
                DataSizing::DataConstantUsedForSizing = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
            }
            if (TempSize > 0.0) ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            StandAloneERV(StandAloneERVNum).SupplyAirVolFlow = TempSize;
        }

        // Size ERV exhaust flow rate
        IsAutoSize = false;
        if (StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }

        if (CurZoneEqNum > 0) {

            ExhaustAirVolFlowDes = SupplyAirVolFlowDes;

            if (ExhaustAirVolFlowDes < SmallAirVolFlow) {
                ExhaustAirVolFlowDes = 0.0;
            }

            if (ExhaustAirVolFlowDes > StandAloneERV(StandAloneERVNum).SupplyAirVolFlow) {
                ExhaustAirVolFlowDes = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
            }

            std::string SizingString = "Exhaust Air Flow Rate [m3/s]";
            Real64 TempSize = StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow;
            if (IsAutoSize) {
                TempSize = ExhaustAirVolFlowDes;
                DataSizing::DataConstantUsedForSizing = ExhaustAirVolFlowDes;
            } else {
                DataSizing::DataConstantUsedForSizing = StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow;
            }
            if (TempSize > 0.0) ReportSizingManager::RequestSizing(CompType, CompName, SizingMethod, SizingString, TempSize, PrintFlag, RoutineName);
            StandAloneERV(StandAloneERVNum).ExhaustAirVolFlow = TempSize;
            StandAloneERV(StandAloneERVNum).DesignEAFanVolFlowRate = TempSize * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio;
        }

        // Set Zone equipment sizing data for autosizing the fans and heat exchanger
        ZoneEqSizing(CurZoneEqNum).AirVolFlow = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio;
        ZoneEqSizing(CurZoneEqNum).OAVolFlow = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
        ZoneEqSizing(CurZoneEqNum).DesignSizeFromParent = true;

        // Check supply fan flow rate or set flow rate if autosized in fan object
        IsAutoSize = false;
        if (StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate == AutoSize) {
            IsAutoSize = true;
        }
        DesignSAFanVolFlowRateDes = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow * StandAloneERV(StandAloneERVNum).HighRHOAFlowRatio;
        if (IsAutoSize) {
            StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate = DesignSAFanVolFlowRateDes;
        } else {
            if (StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate > 0.0 && DesignSAFanVolFlowRateDes > 0.0) {
                DesignSAFanVolFlowRateUser = StandAloneERV(StandAloneERVNum).DesignSAFanVolFlowRate;
                if (DisplayExtraWarnings) {
                    if ((std::abs(DesignSAFanVolFlowRateDes - DesignSAFanVolFlowRateUser) / DesignSAFanVolFlowRateUser) > AutoVsHardSizingThreshold) {
                        ShowMessage("SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator " +
                                    cFanTypes(StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num) + ' ' +
                                    StandAloneERV(StandAloneERVNum).SupplyAirFanName);
                        ShowContinueError("User-Specified Supply Fan Maximum Flow Rate of " + RoundSigDigits(DesignSAFanVolFlowRateUser, 5) +
                                          " [m3/s]");
                        ShowContinueError("differs from the ERV Supply Air Flow Rate of " + RoundSigDigits(DesignSAFanVolFlowRateDes, 5) + " [m3/s]");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }

        // simulate the fan to size using the flow rate specified above
        // (i.e., ZoneEqSizing( CurZoneEqNum ).AirVolFlow = StandAloneERV( StandAloneERVNum ).SupplyAirVolFlow * StandAloneERV( StandAloneERVNum
        // ).HighRHOAFlowRatio;)
        if (!(StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num == DataHVACGlobals::FanType_SystemModelObject)) {
            SimulateFanComponents(StandAloneERV(StandAloneERVNum).SupplyAirFanName, true, StandAloneERV(StandAloneERVNum).SupplyAirFanIndex);
        } else {
            HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }
        if (!(StandAloneERV(StandAloneERVNum).ExhaustAirFanType_Num == DataHVACGlobals::FanType_SystemModelObject)) {
            SimulateFanComponents(StandAloneERV(StandAloneERVNum).ExhaustAirFanName, true, StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex);
        } else {
            HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }

        // now reset the ZoneEqSizing variable to NOT use the multiplier for HighRHOAFlowRatio for sizing HXs
        ZoneEqSizing(CurZoneEqNum).AirVolFlow = StandAloneERV(StandAloneERVNum).SupplyAirVolFlow;
    }

    void CalcStandAloneERV(int const StandAloneERVNum,    // Unit index in ERV data structure
                           bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                           Real64 &SensLoadMet,           // sensible zone load met by unit (W)
                           Real64 &LatentMassLoadMet      // latent zone load met by unit (kg/s), dehumid = negative
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2003
        //       MODIFIED       Don Shirey, Aug 2009 (LatentMassLoadMet)
        //                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate the components making up the Stand Alone ERV unit.

        // METHODOLOGY EMPLOYED:
        // Simulates the unit components sequentially in the air flow direction.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHeatBalance::ZoneAirMassFlow;
        using DataZoneEquipment::ZoneEquipConfig;
        using Fans::SimulateFanComponents;
        using General::RoundSigDigits;
        using HeatRecovery::SimHeatRecovery;
        using MixedAir::OAController;
        using Psychrometrics::PsyHFnTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SupOutletNode;    // unit supply air outlet node
        int ExhaustInletNode; // unit exhaust air inlet node
        int SupInletNode;     // unit supply air inlet node
        Real64 AirMassFlow;   // total mass flow through supply side of the ERV (supply air outlet node)
        Real64 MinHumRatio;   // minimum humidity ratio for calculating sensible load met
        // (so enthalpy routines work without error)
        Real64 TotLoadMet;    // total zone load met by unit (W)
        Real64 LatLoadMet;    // latent zone load met by unit (W)
        bool HXUnitOn;        // flag to operate heat exchanger heat recovery
        bool EconomizerFlag;  // economizer signal from OA controller
        bool HighHumCtrlFlag; // high humditiy control signal from OA controller
        //  INTEGER :: ControlledZoneNum ! index to controlled zones
        //  INTEGER :: ExhaustNodeNum    ! index to exhaust nodes in controlled zones
        //  INTEGER :: SupplyNodeNum     ! index to supply nodes in controlled zone
        //  LOGICAL :: ExhaustNodeFound  ! used in controlled zone exhaust node search
        Real64 TotalExhaustMassFlow; // total exhaust air mass flow rate in controlled zone
        Real64 TotalSupplyMassFlow;  // total supply air mass flow rate in controlled zone

        SupInletNode = StandAloneERV(StandAloneERVNum).SupplyAirInletNode;
        SupOutletNode = StandAloneERV(StandAloneERVNum).SupplyAirOutletNode;
        ExhaustInletNode = StandAloneERV(StandAloneERVNum).ExhaustAirInletNode;

        // Stand alone ERV's HX is ON by default
        HXUnitOn = true;

        // Get stand alone ERV's controller economizer and high humidity control status
        if (StandAloneERV(StandAloneERVNum).ControllerNameDefined) {
            EconomizerFlag = OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).EconoActive;
            HighHumCtrlFlag = OAController(StandAloneERV(StandAloneERVNum).ControllerIndex).HighHumCtrlActive;
        } else {
            EconomizerFlag = false;
            HighHumCtrlFlag = false;
        }

        SimHeatRecovery(StandAloneERV(StandAloneERVNum).HeatExchangerName,
                        FirstHVACIteration,
                        StandAloneERV(StandAloneERVNum).HeatExchangerIndex,
                        ContFanCycCoil,
                        _,
                        HXUnitOn,
                        _,
                        _,
                        EconomizerFlag,
                        HighHumCtrlFlag);
        StandAloneERV(StandAloneERVNum).ElecUseRate = AirToAirHXElecPower;

        if (StandAloneERV(StandAloneERVNum).SupplyAirFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            SimulateFanComponents(StandAloneERV(StandAloneERVNum).SupplyAirFanName,
                                  FirstHVACIteration,
                                  StandAloneERV(StandAloneERVNum).SupplyAirFanIndex,
                                  _,
                                  ZoneCompTurnFansOn,
                                  ZoneCompTurnFansOff);
            StandAloneERV(StandAloneERVNum).ElecUseRate += Fans::GetFanPower(StandAloneERV(StandAloneERVNum).SupplyAirFanIndex);
        } else {
            HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            StandAloneERV(StandAloneERVNum).ElecUseRate += HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).SupplyAirFanIndex]->fanPower();
        }

        if (StandAloneERV(StandAloneERVNum).ExhaustAirFanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            SimulateFanComponents(StandAloneERV(StandAloneERVNum).ExhaustAirFanName,
                                  FirstHVACIteration,
                                  StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex); // why no Turn on off flags here?
            StandAloneERV(StandAloneERVNum).ElecUseRate += Fans::GetFanPower(StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex);
        } else {
            HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            StandAloneERV(StandAloneERVNum).ElecUseRate += HVACFan::fanObjs[StandAloneERV(StandAloneERVNum).ExhaustAirFanIndex]->fanPower();
        }

        MinHumRatio = Node(ExhaustInletNode).HumRat;
        if (Node(SupOutletNode).HumRat < Node(ExhaustInletNode).HumRat) MinHumRatio = Node(SupOutletNode).HumRat;

        AirMassFlow = Node(SupOutletNode).MassFlowRate;
        SensLoadMet = AirMassFlow * (PsyHFnTdbW(Node(SupOutletNode).Temp, MinHumRatio) - PsyHFnTdbW(Node(ExhaustInletNode).Temp, MinHumRatio));
        TotLoadMet = AirMassFlow * (PsyHFnTdbW(Node(SupOutletNode).Temp, Node(SupOutletNode).HumRat) -
                                    PsyHFnTdbW(Node(ExhaustInletNode).Temp, Node(ExhaustInletNode).HumRat));
        LatLoadMet = TotLoadMet - SensLoadMet; // watts

        LatentMassLoadMet = AirMassFlow * (Node(SupOutletNode).HumRat - Node(ExhaustInletNode).HumRat); // kg/s, dehumidification = negative

        if (SensLoadMet < 0.0) {
            StandAloneERV(StandAloneERVNum).SensCoolingRate = std::abs(SensLoadMet);
            StandAloneERV(StandAloneERVNum).SensHeatingRate = 0.0;
        } else {
            StandAloneERV(StandAloneERVNum).SensCoolingRate = 0.0;
            StandAloneERV(StandAloneERVNum).SensHeatingRate = SensLoadMet;
        }
        if (TotLoadMet < 0.0) {
            StandAloneERV(StandAloneERVNum).TotCoolingRate = std::abs(TotLoadMet);
            StandAloneERV(StandAloneERVNum).TotHeatingRate = 0.0;
        } else {
            StandAloneERV(StandAloneERVNum).TotCoolingRate = 0.0;
            StandAloneERV(StandAloneERVNum).TotHeatingRate = TotLoadMet;
        }
        if (LatLoadMet < 0.0) {
            StandAloneERV(StandAloneERVNum).LatCoolingRate = std::abs(LatLoadMet);
            StandAloneERV(StandAloneERVNum).LatHeatingRate = 0.0;
        } else {
            StandAloneERV(StandAloneERVNum).LatCoolingRate = 0.0;
            StandAloneERV(StandAloneERVNum).LatHeatingRate = LatLoadMet;
        }

        // Provide a one time message when exhaust flow rate is greater than supply flow rate
        if (StandAloneERV(StandAloneERVNum).FlowError && !WarmupFlag) {

            //! Adding up zone inlet/outlet nodes is not working correctly. When imbalance flow occurs, the difference
            //! is placed on the zone return node even when there is nothing connected to it.

            //    IF(StandAloneERV(StandAloneERVNum)%ControlledZoneNum .GT. 0)THEN
            //      TotalExhaustMassFlow = 0.0
            //      DO ExhaustNodeNum = 1, ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%NumExhaustNodes
            //         TotalExhaustMassFlow = TotalExhaustMassFlow + &
            //             Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ExhaustNode(ExhaustNodeNum))%MassFlowRate
            //      END DO
            //    ELSE
            //      DO ControlledZoneNum = 1, NumOfControlledZones
            //        TotalExhaustMassFlow = 0.0
            //        ExhaustNodeFound = .FALSE.
            //        DO ExhaustNodeNum = 1, ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
            //          TotalExhaustMassFlow = TotalExhaustMassFlow + &
            //                                 Node(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ExhaustNodeNum))%MassFlowRate
            //          IF(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ExhaustNodeNum) .EQ. ExhaustInletNode) THEN
            //            ExhaustNodeFound = .TRUE.
            //            StandAloneERV(StandAloneERVNum)%ControlledZoneNum = ControlledZoneNum
            //          END IF
            //        END DO
            //        IF(ExhaustNodeFound)EXIT
            //      END DO
            //    END IF
            //    IF(StandAloneERV(StandAloneERVNum)%ControlledZoneNum .GT. 0)THEN
            //!     Add in return node mass flow rate to total exhaust
            //      IF(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ReturnAirNode .GT. 0)THEN
            //        TotalExhaustMassFlow = TotalExhaustMassFlow + &
            //            Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ReturnAirNode)%MassFlowRate
            //      END IF
            //      TotalSupplyMassFlow = 0.0
            //      DO SupplyNodeNum = 1, ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%NumInletNodes
            //        TotalSupplyMassFlow = TotalSupplyMassFlow + &
            //            Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%InletNode(SupplyNodeNum))%MassFlowRate
            //      END DO
            TotalExhaustMassFlow = Node(ExhaustInletNode).MassFlowRate;
            TotalSupplyMassFlow = Node(SupInletNode).MassFlowRate;
            if (TotalExhaustMassFlow > TotalSupplyMassFlow && !ZoneAirMassFlow.EnforceZoneMassBalance) {
                ShowWarningError("For " + StandAloneERV(StandAloneERVNum).UnitType + " \"" + StandAloneERV(StandAloneERVNum).Name +
                                 "\" there is unbalanced exhaust air flow.");
                ShowContinueError("... The exhaust air mass flow rate = " + RoundSigDigits(Node(ExhaustInletNode).MassFlowRate, 6));
                ShowContinueError("... The  supply air mass flow rate = " + RoundSigDigits(Node(SupInletNode).MassFlowRate, 6));
                ShowContinueErrorTimeStamp("");
                ShowContinueError("... Unless there is balancing infiltration / ventilation air flow, this will result in");
                ShowContinueError("... load due to induced outside air being neglected in the simulation.");
                StandAloneERV(StandAloneERVNum).FlowError = false;
            }
            //    END IF
        }
    }

    void ReportStandAloneERV(int const StandAloneERVNum) // number of the current Stand Alone ERV being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad, FSEC
        //       DATE WRITTEN   June 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fill remaining report variables

        // METHODOLOGY EMPLOYED:
        // na

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
        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * SecInHour;
        StandAloneERV(StandAloneERVNum).ElecUseEnergy = StandAloneERV(StandAloneERVNum).ElecUseRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).SensCoolingEnergy = StandAloneERV(StandAloneERVNum).SensCoolingRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).LatCoolingEnergy = StandAloneERV(StandAloneERVNum).LatCoolingRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).TotCoolingEnergy = StandAloneERV(StandAloneERVNum).TotCoolingRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).SensHeatingEnergy = StandAloneERV(StandAloneERVNum).SensHeatingRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).LatHeatingEnergy = StandAloneERV(StandAloneERVNum).LatHeatingRate * ReportingConstant;
        StandAloneERV(StandAloneERVNum).TotHeatingEnergy = StandAloneERV(StandAloneERVNum).TotHeatingRate * ReportingConstant;

        if (StandAloneERV(StandAloneERVNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!DataGlobals::SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, 0, StandAloneERV(StandAloneERVNum).FirstPass);
            }
        }
    }

    //        End of Reporting subroutines for the Module

    //        Utility subroutines/functions for the HeatingCoil Module

    Real64 GetSupplyAirFlowRate(std::string const &ERVType,     // must be "ZoneHVAC:EnergyRecoveryVentilator"
                                std::string const &ERVCtrlName, // must match a controller name in the ERV data structure
                                bool &ErrorsFound               // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
        // Supply Air Flow rate, if found.  If incorrect name is given, ErrorsFound is returned as true
        // and supply air flow rate as negative.

        // Return value
        Real64 AirFlowRate; // returned supply air flow rate of the ERV unit

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichERV;

        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        if (UtilityRoutines::SameString(ERVType, "ZoneHVAC:EnergyRecoveryVentilator")) {
            WhichERV = UtilityRoutines::FindItem(ERVCtrlName, StandAloneERV, &StandAloneERVData::ControllerName);
            if (WhichERV != 0) {
                AirFlowRate = StandAloneERV(WhichERV).SupplyAirVolFlow;
            }
        } else {
            WhichERV = 0;
        }

        if (WhichERV == 0) {
            ShowSevereError("Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name=\"" + ERVCtrlName + "\"");
            ErrorsFound = true;
            AirFlowRate = -1000.0;
        }

        return AirFlowRate;
    }

    int GetSupplyAirInletNode(std::string const &ERVType,     // must be "ZoneHVAC:EnergyRecoveryVentilator"
                              std::string const &ERVCtrlName, // must match a controller name in the ERV data structure
                              bool &ErrorsFound               // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
        // Supply Air Inlet Node Number, if found.  If incorrect name is given, ErrorsFound is returned as true
        // and Supply Air Inlet Node Number as zero.

        // Return value
        int AirInletNode(0); // returned air inlet node number of the ERV unit

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichERV;

        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        if (UtilityRoutines::SameString(ERVType, "ZoneHVAC:EnergyRecoveryVentilator")) {
            WhichERV = UtilityRoutines::FindItem(ERVCtrlName, StandAloneERV, &StandAloneERVData::ControllerName);
            if (WhichERV != 0) {
                AirInletNode = StandAloneERV(WhichERV).SupplyAirInletNode;
            }
        } else {
            WhichERV = 0;
        }

        if (WhichERV == 0) {
            ShowSevereError("Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name=\"" + ERVCtrlName + "\"");
            ErrorsFound = true;
            AirInletNode = 0;
        }

        return AirInletNode;
    }

    int GetExhaustAirInletNode(std::string const &ERVType,     // must be "ZoneHVAC:EnergyRecoveryVentilator"
                               std::string const &ERVCtrlName, // must match a controller name in the ERV data structure
                               bool &ErrorsFound               // set to true if problem
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   October 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
        // Exhaust Air Inlet Node Number, if found.  If incorrect name is given, ErrorsFound is returned as true
        // and Exhaust Air Inlet Node Number as zero.

        // Return value
        int AirInletNode(0); // returned air inlet node number of the ERV unit

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int WhichERV;

        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        if (UtilityRoutines::SameString(ERVType, "ZoneHVAC:EnergyRecoveryVentilator")) {
            WhichERV = UtilityRoutines::FindItem(ERVCtrlName, StandAloneERV, &StandAloneERVData::ControllerName);
            if (WhichERV != 0) {
                AirInletNode = StandAloneERV(WhichERV).ExhaustAirInletNode;
            }
        } else {
            WhichERV = 0;
        }

        if (WhichERV == 0) {
            ShowSevereError("Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name=\"" + ERVCtrlName + "\"");
            ErrorsFound = true;
            AirInletNode = 0;
        }

        return AirInletNode;
    }

    int GetStandAloneERVOutAirNode(int const StandAloneERVNum)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node for ventilation rate reporting

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int GetStandAloneERVOutAirNode;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na
        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        GetStandAloneERVOutAirNode = 0;
        if (StandAloneERVNum > 0 && StandAloneERVNum <= NumStandAloneERVs) {
            GetStandAloneERVOutAirNode = StandAloneERV(StandAloneERVNum).SupplyAirInletNode;
        }

        return GetStandAloneERVOutAirNode;
    }

    int GetStandAloneERVZoneInletAirNode(int const StandAloneERVNum)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node for ventilation rate reporting

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int GetStandAloneERVZoneInletAirNode;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na
        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        GetStandAloneERVZoneInletAirNode = 0;
        if (StandAloneERVNum > 0 && StandAloneERVNum <= NumStandAloneERVs) {
            GetStandAloneERVZoneInletAirNode = StandAloneERV(StandAloneERVNum).SupplyAirOutletNode;
        }

        return GetStandAloneERVZoneInletAirNode;
    }

    int GetStandAloneERVReturnAirNode(int const StandAloneERVNum)
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node for ventilation rate reporting

        // METHODOLOGY EMPLOYED:
        // <description>

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int GetStandAloneERVReturnAirNode;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        // na
        if (GetERVInputFlag) {
            GetStandAloneERV();
            GetERVInputFlag = false;
        }

        GetStandAloneERVReturnAirNode = 0;
        if (StandAloneERVNum > 0 && StandAloneERVNum <= NumStandAloneERVs) {
            GetStandAloneERVReturnAirNode = StandAloneERV(StandAloneERVNum).ExhaustAirInletNode;
        }

        return GetStandAloneERVReturnAirNode;
    }

} // namespace HVACStandAloneERV

} // namespace EnergyPlus
