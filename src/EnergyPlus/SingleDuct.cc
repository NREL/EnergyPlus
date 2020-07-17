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
#include <AirflowNetwork/Elements.hpp>
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
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ReportSizingManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/TempSolveRoot.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

namespace EnergyPlus {

namespace SingleDuct {
    // Module containing the Single Duct Systems as a single component/ or really a single driver

    // MODULE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   January 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms required to
    // simulate single duct systems as a single driver or inter-connecting controllers.

    // Using/Aliasing
    using namespace DataPrecisionGlobals;
    using namespace DataLoopNode;
    using BranchNodeConnections::SetUpCompSets;
    using BranchNodeConnections::TestCompSet;
    using DataEnvironment::StdBaroPress;
    using DataEnvironment::StdRhoAir;
    using DataGlobals::BeginDayFlag;
    using DataGlobals::BeginEnvrnFlag;
    using DataGlobals::DisplayExtraWarnings;
    using DataGlobals::NumOfZones;
    using DataGlobals::SysSizingCalc;
    using DataHeatBalFanSys::TempControlType;
    using DataHVACGlobals::ATMixer_InletSide;
    using DataHVACGlobals::ATMixer_SupplySide;
    using DataHVACGlobals::DualSetPointWithDeadBand;
    using DataHVACGlobals::SingleCoolingSetPoint;
    using DataHVACGlobals::SingleHeatCoolSetPoint;
    using DataHVACGlobals::SingleHeatingSetPoint;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using DataHVACGlobals::TurnFansOn;
    using namespace DataSizing;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace FluidProperties;
    using namespace ScheduleManager;
    using namespace SteamCoils;

    // Data
    // MODULE PARAMETER DEFINITIONS
    int const Normal(1);
    int const ReverseAction(2);
    int const ReverseActionWithLimits(3);
    int const HeatingActionNotUsed(0);
    // SysTypes represented here
    int const SingleDuctVAVReheat(3);
    int const SingleDuctVAVNoReheat(4);
    int const SingleDuctConstVolReheat(5);
    int const SingleDuctConstVolNoReheat(6);
    int const SingleDuctVAVReheatVSFan(7);
    int const SingleDuctCBVAVReheat(10);
    int const SingleDuctCBVAVNoReheat(11);
    // Reheat Coil Types used here
    int const HCoilType_None(0);
    int const HCoilType_Gas(1);
    int const HCoilType_Electric(2);
    int const HCoilType_SimpleHeating(3);
    int const HCoilType_SteamAirHeating(4);

    // Minimum Flow Fraction Input Method
    int const ConstantMinFrac(1);
    int const ScheduledMinFrac(2);
    int const FixedMin(3);
    int const MinFracNotUsed(0);
    int NumATMixers(0);

    static std::string const fluidNameSteam("STEAM");
    static std::string const fluidNameWater("WATER");
    static std::string const BlankString;

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    bool GetInputFlag(true);   // Flag set to make sure you get input once
    bool GetATMixerFlag(true); // Flag set to make sure you get input once
    int NumConstVolSys(0);
    Array1D_bool CheckEquipName;

    // INTERFACE BLOCK SPECIFICATIONS

    int NumSDAirTerminal(0); // The Number of Systems found in the Input

    // Subroutine Specifications for the Module
    // Driver/Manager Routines

    // Get Input routines for module

    // Initialization routines for module

    // Algorithms for the module

    // Update routine to check convergence and update nodes

    // Reporting routines for module

    // Object Data
    Array1D<SingleDuctAirTerminal> sd_airterminal;
    std::unordered_map<std::string, std::string> SysUniqueNames;
    Array1D<AirTerminalMixerData> SysATMixer;
    // Array1D< AirTerminalSingleDuctConstantVolumeNoReheat > SingleDuctConstantVolumeNoReheat;

    namespace {
        // These were static variables within different functions. They were pulled out into the namespace
        // to facilitate easier unit testing of those functions.
        // These are purposefully not in the header file as an extern variable. No one outside of this should
        // use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
        // This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
        bool InitSysFlag(true);     // Flag set to make sure you do begin simulation initializaztions once
        bool InitATMixerFlag(true); // Flag set to make sure you do begin simulation initializaztions once for mixer
    }                               // namespace

    // MODULE SUBROUTINES:
    //*************************************************************************

    // Functions

    void clear_state()
    {
        GetInputFlag = true;
        GetATMixerFlag = true;
        InitSysFlag = true;
        SysUniqueNames.clear();
        SysATMixer.deallocate();
        sd_airterminal.deallocate();
        InitATMixerFlag = true;
    }

    void SimulateSingleDuct(EnergyPlusData &state, std::string const &CompName, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum, int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   January 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine manages Sys system simulation.
        // It is called from the ManageZoneEquip
        // at the system time step.

        // Using/Aliasing
        using General::TrimSigDigits;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int SysNum; // The Sys that you are currently loading input into

        // FLOW:

        // Obtains and Allocates Sys related parameters from input file
        if (GetInputFlag) { // First time subroutine has been entered
            GetSysInput(state);
            GetInputFlag = false;
        }

        // Find the correct SysNumber with the Component Name
        if (CompIndex == 0) {
            SysNum = UtilityRoutines::FindItemInList(CompName, sd_airterminal, &SingleDuctAirTerminal::SysName);
            if (SysNum == 0) {
                ShowFatalError("SimulateSingleDuct: System not found=" + CompName);
            }
            CompIndex = SysNum;
        } else {
            SysNum = CompIndex;
            if (SysNum > NumSDAirTerminal || SysNum < 1) {
                ShowFatalError("SimulateSingleDuct: Invalid CompIndex passed=" + TrimSigDigits(CompIndex) +
                               ", Number of Systems=" + TrimSigDigits(NumSDAirTerminal) + ", System name=" + CompName);
            }
            if (CheckEquipName(SysNum)) {
                if (CompName != sd_airterminal(SysNum).SysName) {
                    ShowFatalError("SimulateSingleDuct: Invalid CompIndex passed=" + TrimSigDigits(CompIndex) + ", System name=" + CompName +
                                   ", stored System Name for that index=" + sd_airterminal(SysNum).SysName);
                }
                CheckEquipName(SysNum) = false;
            }
        }

        auto &thisATU(sd_airterminal(SysNum));

        TermUnitSingDuct = true;
        DataSizing::CurTermUnitSizingNum = DataDefineEquip::AirDistUnit(thisATU.ADUNum).TermUnitSizingNum;

        // With the correct SysNum Initialize the system
        thisATU.InitSys(state, FirstHVACIteration); // Initialize all Sys related parameters

        // Calculate the Correct Sys Model with the current SysNum
        {
            auto const SELECT_CASE_var(thisATU.SysType_Num);

            if (SELECT_CASE_var == SingleDuctConstVolReheat) { // AirTerminal:SingleDuct:ConstantVolume:Reheat
                thisATU.SimConstVol(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);

            } else if (SELECT_CASE_var == SingleDuctConstVolNoReheat) { // AirTerminal:SingleDuct:ConstantVolume:NoReheat
                thisATU.SimConstVolNoReheat(ZoneNodeNum);
            } else if (SELECT_CASE_var == SingleDuctVAVReheat) { // SINGLE DUCT:VAV:REHEAT
                thisATU.SimVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);

            } else if (SELECT_CASE_var == SingleDuctVAVNoReheat) { // SINGLE DUCT:VAV:NOREHEAT
                thisATU.SimVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);

            } else if (SELECT_CASE_var == SingleDuctVAVReheatVSFan) { // SINGLE DUCT:VAV:REHEAT:VS FAN
                thisATU.SimVAVVS(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);

            } else if (SELECT_CASE_var == SingleDuctCBVAVReheat) { // SINGLE DUCT:VAVHEATANDCOOL:REHEAT
                thisATU.SimCBVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);

            } else if (SELECT_CASE_var == SingleDuctCBVAVNoReheat) { // SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT
                thisATU.SimCBVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);
            }
        }

        // Report the current Sys
        thisATU.ReportSys();

        TermUnitSingDuct = false;
    }

    // Get Input Section of the Module
    //******************************************************************************

    void GetSysInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   April 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is the main routine to call other input routines and Get routines

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using DataDefineEquip::AirDistUnit;
        using DataDefineEquip::NumAirDistUnits;
        using DataZoneEquipment::ZoneEquipConfig;
        using NodeInputManager::GetOnlySingleNode;
        using SteamCoils::GetCoilAirOutletNode;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetSteamCoilIndex;
        using WaterCoils::GetCoilOutletNode;
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);
        auto &GetHeatingCoilOutletNode(HeatingCoils::GetCoilOutletNode);
        using Fans::GetFanInletNode;
        using Fans::GetFanOutletNode;
        using namespace DataIPShortCuts;
        using namespace DataHeatBalance;
        using DataGlobals::DoZoneSizing;
        using DataGlobals::ScheduleAlwaysOn;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataSizing::OARequirements;
        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSysInput: "); // include trailing blank

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        static int SysNum(0);   // The Sys that you are currently loading input into
        static int SysIndex(0); // The Sys that you are currently loading input into
        static int NumVAVSys(0);
        static int NumNoRHVAVSys(0);
        static int NumVAVVS(0);
        static int NumCBVAVSys(0);
        static int NumNoRHCBVAVSys(0);
        static int NumAlphas(0);
        static int NumNums(0);
        static int NumCVNoReheatSys(0);
        int NumZoneSiz;
        int ZoneSizIndex;
        int IOStat;
        static bool ErrorsFound(false);  // If errors detected in input
        bool IsNotOK;                    // Flag to verify name
        int CtrlZone;                    // controlled zone do loop index
        int SupAirIn;                    // controlled zone supply air inlet index
        int ADUNum;                      // air distribution unit index
        std::string CurrentModuleObject; // for ease in getting objects
        Array1D_string Alphas;           // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> Numbers;         // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        static int MaxNums(0);           // Maximum number of numeric input fields
        static int MaxAlphas(0);         // Maximum number of alpha input fields
        static int TotalArgs(0);         // Total number of alpha and numeric arguments (max) for a
        //  certain object in the input file
        std::string AirTermSysInletNodeName;  // air terminal single duct system inlet node name
        std::string AirTermSysOutletNodeName; // air terminal single duct system outlet node name

        // Flow
        NumVAVSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:VAV:Reheat");
        NumNoRHVAVSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:VAV:NoReheat");
        NumConstVolSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:ConstantVolume:Reheat");
        NumCVNoReheatSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:ConstantVolume:NoReheat");
        NumVAVVS = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan");
        NumCBVAVSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat");
        NumNoRHCBVAVSys = inputProcessor->getNumObjectsFound("AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat");
        NumSDAirTerminal = NumVAVSys + NumConstVolSys + NumCVNoReheatSys + NumNoRHVAVSys + NumVAVVS + NumCBVAVSys + NumNoRHCBVAVSys;

        sd_airterminal.allocate(NumSDAirTerminal);
        SysUniqueNames.reserve(static_cast<unsigned>(NumSDAirTerminal));
        CheckEquipName.dimension(NumSDAirTerminal, true);

        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:VAV:Reheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:VAV:NoReheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:ConstantVolume:Reheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:ConstantVolume:NoReheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        inputProcessor->getObjectDefMaxArgs("AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat", TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        Alphas.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        cNumericFields.allocate(MaxNums);
        Numbers.dimension(MaxNums, 0.0);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);

        // Start Loading the System Input
        for (SysIndex = 1; SysIndex <= NumVAVSys; ++SysIndex) {

            CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctVAVReheat;
            sd_airterminal(SysNum).ReheatComp = Alphas(7);
            if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Fuel")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Gas;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Electric")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Electric;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Water")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SimpleHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Steam")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SteamAirHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
            } else if (sd_airterminal(SysNum).ReheatComp != "") {
                ShowSevereError("Illegal " + cAlphaFields(8) + " = " + sd_airterminal(SysNum).ReheatComp + '.');
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).ReheatName = Alphas(8);
            ValidateComponent(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            // For node connections, this object is both a parent and a non-parent, because the
            // VAV damper is not called out as a separate component, its nodes must be connected
            // as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(Alphas(3),
                                                          ErrorsFound,
                                                          sd_airterminal(SysNum).SysType,
                                                          Alphas(1),
                                                          NodeType_Air,
                                                          NodeConnectionType_Outlet,
                                                          1,
                                                          ObjectIsNotParent,
                                                          cAlphaFields(3));
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(Alphas(4),
                                                         ErrorsFound,
                                                         sd_airterminal(SysNum).SysType,
                                                         Alphas(1),
                                                         NodeType_Air,
                                                         NodeConnectionType_Inlet,
                                                         1,
                                                         ObjectIsNotParent,
                                                         cAlphaFields(4));
            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);

            if (UtilityRoutines::SameString(Alphas(5), "Constant")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = ConstantMinFrac;
            } else if (UtilityRoutines::SameString(Alphas(5), "FixedFlowRate")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = FixedMin;
            } else if (UtilityRoutines::SameString(Alphas(5), "Scheduled")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = ScheduledMinFrac;
            } else {
                ShowSevereError(cAlphaFields(5) + " = " + Alphas(5) + " not found.");
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(2);
            if (lNumericBlanks(2)) {
                sd_airterminal(SysNum).ConstantMinAirFracSetByUser = false;
                sd_airterminal(SysNum).DesignMinAirFrac = 0.0;
            } else {
                sd_airterminal(SysNum).ConstantMinAirFracSetByUser = true;
                sd_airterminal(SysNum).DesignMinAirFrac = Numbers(2);
                if (sd_airterminal(SysNum).ZoneMinAirFracMethod == FixedMin) {
                    ShowWarningError("Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(2) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
                }
            }

            sd_airterminal(SysNum).ZoneFixedMinAir = Numbers(3);
            if (lNumericBlanks(3)) {
                sd_airterminal(SysNum).FixedMinAirSetByUser = false;
                sd_airterminal(SysNum).DesignMinAirFrac = 0.0;
            } else {
                sd_airterminal(SysNum).FixedMinAirSetByUser = true;
                sd_airterminal(SysNum).DesignMinAirFrac = Numbers(3);
                if (sd_airterminal(SysNum).ZoneMinAirFracMethod == ConstantMinFrac) {
                    ShowWarningError("Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(3) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    sd_airterminal(SysNum).ZoneFixedMinAir = 0.0;
                }
            }

            sd_airterminal(SysNum).ZoneMinAirFracSchPtr = GetScheduleIndex(Alphas(6));
            if ((sd_airterminal(SysNum).ZoneMinAirFracSchPtr == 0) && (sd_airterminal(SysNum).ZoneMinAirFracMethod == ScheduledMinFrac)) {
                ShowSevereError(cAlphaFields(6) + " = " + Alphas(6) + " not found.");
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ShowContinueError("A valid schedule is required");
                ErrorsFound = true;
            } else if ((sd_airterminal(SysNum).ZoneMinAirFracSchPtr > 0) && (sd_airterminal(SysNum).ZoneMinAirFracMethod == ScheduledMinFrac)) {
                // check range of values in schedule
                if (!CheckScheduleValueMinMax(sd_airterminal(SysNum).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError("Error found in " + cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ShowContinueError("Schedule values must be (>=0., <=1.)");
                }
            }

            // The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
            // electric or gas reheat.
            if (sd_airterminal(SysNum).ReheatComp_Num != HCoilType_Gas && sd_airterminal(SysNum).ReheatComp_Num != HCoilType_Electric) {
                if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilSteamInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                } else {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilWaterInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                }
            }
            sd_airterminal(SysNum).ReheatAirOutletNode = GetOnlySingleNode(
                Alphas(9), ErrorsFound, sd_airterminal(SysNum).SysType, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields(9));
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                sd_airterminal(SysNum).MaxReheatSteamVolFlow = Numbers(4);
                sd_airterminal(SysNum).MinReheatSteamVolFlow = Numbers(5);
            } else {
                sd_airterminal(SysNum).MaxReheatWaterVolFlow = Numbers(4);
                sd_airterminal(SysNum).MinReheatWaterVolFlow = Numbers(5);
            }
            sd_airterminal(SysNum).ControllerOffset = Numbers(6);
            // Set default convergence tolerance
            if (sd_airterminal(SysNum).ControllerOffset <= 0.0) {
                sd_airterminal(SysNum).ControllerOffset = 0.001;
            }
            if (UtilityRoutines::SameString(Alphas(10), "Reverse")) {
                sd_airterminal(SysNum).DamperHeatingAction = ReverseAction;
            } else if (UtilityRoutines::SameString(Alphas(10), "Normal")) {
                sd_airterminal(SysNum).DamperHeatingAction = Normal;
            } else if (UtilityRoutines::SameString(Alphas(10), "ReverseWithLimits")) {
                sd_airterminal(SysNum).DamperHeatingAction = ReverseActionWithLimits;
            } else {
                ShowSevereError(cAlphaFields(10) + " = " + Alphas(10) + " not found.");
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            // Register component set data
            TestCompSet(
                sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).ReheatAirOutletNode), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).ReheatAirOutletNode == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).ReheatAirOutletNode == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).ReheatAirOutletNode) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).ReheatAirOutletNode;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }

                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }
            if (Numbers(7) == AutoCalculate) {
                sd_airterminal(SysNum).MaxAirVolFlowRateDuringReheat = Numbers(7);
            } else {
                sd_airterminal(SysNum).MaxAirVolFlowRateDuringReheat = Numbers(7) * sd_airterminal(SysNum).ZoneFloorArea;
            }

            sd_airterminal(SysNum).MaxAirVolFractionDuringReheat = Numbers(8);

            if (sd_airterminal(SysNum).DamperHeatingAction != ReverseActionWithLimits) {
                if (sd_airterminal(SysNum).MaxAirVolFlowRateDuringReheat > 0.0) {
                    ShowWarningError("Since " + cAlphaFields(10) + " = " + Alphas(10) + ", input for " + cNumericFields(7) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                }
                if (sd_airterminal(SysNum).MaxAirVolFractionDuringReheat > 0.0) {
                    ShowWarningError("Since " + cAlphaFields(10) + " = " + Alphas(10) + ", input for " + cNumericFields(8) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                }
            }

            // Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
            if (!lNumericBlanks(9)) {
                sd_airterminal(SysNum).MaxReheatTemp = Numbers(9);
                sd_airterminal(SysNum).MaxReheatTempSetByUser = true;
            } else {
                // user does not specify maximum supply air temperature
                // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
                sd_airterminal(SysNum).MaxReheatTempSetByUser = false;
            }

            if (!lAlphaBlanks(11)) {
                sd_airterminal(SysNum).OARequirementsPtr = UtilityRoutines::FindItemInList(Alphas(11), OARequirements);
                if (sd_airterminal(SysNum).OARequirementsPtr == 0) {
                    ShowSevereError(cAlphaFields(11) + " = " + Alphas(11) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                } else {
                    sd_airterminal(SysNum).NoOAFlowInputFromUser = false;
                }
            }

            if (lAlphaBlanks(12)) {
                sd_airterminal( SysNum ).ZoneTurndownMinAirFrac = 1.0;
                sd_airterminal( SysNum ).ZoneTurndownMinAirFracSchExist = false;
            } else {
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(Alphas(12));
                if (sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr == 0) {
                    ShowSevereError(cAlphaFields(12) + " = " + Alphas(12) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
                sd_airterminal( SysNum ).ZoneTurndownMinAirFracSchExist = true;
            }

            ValidateComponent(Alphas(7), Alphas(8), IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            // Add reheat coil to component sets array
            SetUpCompSets(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, Alphas(7), Alphas(8), Alphas(3), Alphas(9));

            // Setup the Average damper Position output variable
            SetupOutputVariable("Zone Air Terminal VAV Damper Position",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).DamperPosition,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);
            SetupOutputVariable("Zone Air Terminal Minimum Air Flow Fraction",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).ZoneMinAirFracReport,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);

        } // end Number of Sys Loop

        for (SysIndex = 1; SysIndex <= NumCBVAVSys; ++SysIndex) {

            CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctCBVAVReheat;
            sd_airterminal(SysNum).ReheatComp = Alphas(5);
            if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Fuel")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Gas;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Electric")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Electric;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Water")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SimpleHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Steam")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SteamAirHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
            } else if (sd_airterminal(SysNum).ReheatComp != "") {
                ShowSevereError("Illegal " + cAlphaFields(5) + " = " + sd_airterminal(SysNum).ReheatComp + '.');
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).ReheatName = Alphas(6);
            ValidateComponent(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            // For node connections, this object is both a parent and a non-parent, because the
            // VAV damper is not called out as a separate component, its nodes must be connected
            // as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(Alphas(3),
                                                          ErrorsFound,
                                                          sd_airterminal(SysNum).SysType,
                                                          Alphas(1),
                                                          NodeType_Air,
                                                          NodeConnectionType_Outlet,
                                                          1,
                                                          ObjectIsNotParent,
                                                          cAlphaFields(3));
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(Alphas(4),
                                                         ErrorsFound,
                                                         sd_airterminal(SysNum).SysType,
                                                         Alphas(1),
                                                         NodeType_Air,
                                                         NodeConnectionType_Inlet,
                                                         1,
                                                         ObjectIsNotParent,
                                                         cAlphaFields(4));
            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);
            sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(2);
            if (sd_airterminal(SysNum).ZoneMinAirFracDes < 0.0) {
                ShowWarningError(sd_airterminal(SysNum).SysType + " \"" + sd_airterminal(SysNum).SysName + "\"");
                ShowContinueError(cNumericFields(2) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues.");
                sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
            }
            if (sd_airterminal(SysNum).ZoneMinAirFracDes > 1.0) {
                ShowWarningError(sd_airterminal(SysNum).SysType + " \"" + sd_airterminal(SysNum).SysName + "\"");
                ShowContinueError(cNumericFields(2) + " must be less than or equal to 1. Resetting to 1 and the simulation continues.");
                sd_airterminal(SysNum).ZoneMinAirFracDes = 1.0;
            }
            // The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
            // electric or gas reheat.
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Gas || sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Electric) {
                //          IF(.NOT. lAlphaBlanks(5)) THEN
                //            CALL ShowWarningError('In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' //TRIM(sd_airterminal(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
                //            CALL ShowContinueError('  It is used for hot water and steam reheat coils only.')
                //          END IF
            } else {
                //          IF(lAlphaBlanks(5)) THEN
                //            CALL ShowSevereError('In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' //TRIM(sd_airterminal(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
                //            ErrorsFound=.TRUE.
                //          ELSE
                if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilSteamInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                    //                GetOnlySingleNode(Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                    //                              NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
                } else {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilWaterInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                    //                GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                    //                              NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
                }
                //  END IF
            }
            sd_airterminal(SysNum).ReheatAirOutletNode = GetOnlySingleNode(
                Alphas(7), ErrorsFound, sd_airterminal(SysNum).SysType, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields(7));
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                sd_airterminal(SysNum).MaxReheatSteamVolFlow = Numbers(3);
                sd_airterminal(SysNum).MinReheatSteamVolFlow = Numbers(4);
            } else {
                sd_airterminal(SysNum).MaxReheatWaterVolFlow = Numbers(3);
                sd_airterminal(SysNum).MinReheatWaterVolFlow = Numbers(4);
            }
            sd_airterminal(SysNum).ControllerOffset = Numbers(5);
            // Set default convergence tolerance
            if (sd_airterminal(SysNum).ControllerOffset <= 0.0) {
                sd_airterminal(SysNum).ControllerOffset = 0.001;
            }

            sd_airterminal(SysNum).DamperHeatingAction = ReverseAction;

            // Register component set data
            TestCompSet(
                sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).ReheatAirOutletNode), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).ReheatAirOutletNode == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).ReheatAirOutletNode == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).ReheatAirOutletNode) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).ReheatAirOutletNode;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }
                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }
            if (!lNumericBlanks(6)) {
                sd_airterminal(SysNum).MaxReheatTemp = Numbers(6);
                sd_airterminal(SysNum).MaxReheatTempSetByUser = true;
            } else {
                // user does not specify maximum supply air temperature
                // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
                sd_airterminal(SysNum).MaxReheatTempSetByUser = false;
            }

            ValidateComponent(Alphas(5), Alphas(6), IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            if (lAlphaBlanks(8)) {
                sd_airterminal(SysNum).ZoneTurndownMinAirFrac = 1.0;
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = false;
            } else {
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(Alphas(8));
                if (sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr == 0) {
                    ShowSevereError(cAlphaFields(8) + " = " + Alphas(8) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = true;
            }

            // Add reheat coil to component sets array
            SetUpCompSets(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, Alphas(5), Alphas(6), Alphas(3), Alphas(7));

            // Setup the Average damper Position output variable
            SetupOutputVariable("Zone Air Terminal VAV Damper Position",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).DamperPosition,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);

        } // end Number of VAVHeatandCool Sys Loop

        CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:Reheat";

        for (SysIndex = 1; SysIndex <= NumConstVolSys; ++SysIndex) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys + NumCBVAVSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctConstVolReheat;
            sd_airterminal(SysNum).ReheatComp = Alphas(5);
            if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Fuel")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Gas;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Electric")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Electric;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Water")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SimpleHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Steam")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SteamAirHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
            } else {
                ShowSevereError("Illegal " + cAlphaFields(5) + " = " + sd_airterminal(SysNum).ReheatComp + '.');
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).ReheatName = Alphas(6);
            ValidateComponent(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(
                Alphas(3), ErrorsFound, sd_airterminal(SysNum).SysType, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent, cAlphaFields(3));
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(
                Alphas(4), ErrorsFound, sd_airterminal(SysNum).SysType, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent, cAlphaFields(4));
            // The reheat coil control node is necessary for hot water reheat, but not necessary for
            // electric or gas reheat.
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Gas || sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Electric) {
                //          IF(.NOT. lAlphaBlanks(5)) THEN
                //            CALL ShowWarningError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
                //            CALL ShowContinueError('  It is used for hot water reheat coils only.')
                //          END IF
            } else {
                //          IF(lAlphaBlanks(5)) THEN
                //            CALL ShowSevereError('In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
                //            ErrorsFound=.TRUE.
                //          END IF
                if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilSteamInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                    //                 GetOnlySingleNode(Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                    //                               NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
                } else {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilWaterInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    }
                    //                 GetOnlySingleNode(Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                    //                               NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
                }
            }
            sd_airterminal(SysNum).ReheatAirOutletNode = sd_airterminal(SysNum).OutletNodeNum;
            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);
            sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
            sd_airterminal(SysNum).ZoneMinAirFracMethod = MinFracNotUsed;
            sd_airterminal(SysNum).DamperHeatingAction = HeatingActionNotUsed;
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                sd_airterminal(SysNum).MaxReheatSteamVolFlow = Numbers(2);
                sd_airterminal(SysNum).MinReheatSteamVolFlow = Numbers(3);
            } else {
                sd_airterminal(SysNum).MaxReheatWaterVolFlow = Numbers(2);
                sd_airterminal(SysNum).MinReheatWaterVolFlow = Numbers(3);
            }
            sd_airterminal(SysNum).ControllerOffset = Numbers(4);
            // Set default convergence tolerance
            if (sd_airterminal(SysNum).ControllerOffset <= 0.0) {
                sd_airterminal(SysNum).ControllerOffset = 0.001;
            }

            // Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
            if (!lNumericBlanks(5)) {
                sd_airterminal(SysNum).MaxReheatTemp = Numbers(5);
                sd_airterminal(SysNum).MaxReheatTempSetByUser = true;
            } else {
                // user does not specify maximum supply air temperature
                // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
                sd_airterminal(SysNum).MaxReheatTempSetByUser = false;
            }
            // Register component set data
            TestCompSet(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).OutletNodeNum), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).ReheatAirOutletNode == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).OutletNodeNum == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).OutletNodeNum) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).OutletNodeNum;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }
                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }

            ValidateComponent(Alphas(5), Alphas(6), IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            // Add reheat coil to component sets array
            SetUpCompSets(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, Alphas(5), Alphas(6), Alphas(4), Alphas(3));

            // Setup the Average damper Position output variable
            // BG removed 9-10-2009 during work on CR 7770, constant volume has no damper
            //  CALL SetupOutputVariable('Damper Position', Sys(SysNum)%DamperPosition, &
            //                        'System','Average',Sys(SysNum)%SysName)

        } // End Number of Sys Loop

        CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:NoReheat";

        for (SysIndex = 1; SysIndex <= NumCVNoReheatSys; ++SysIndex) {

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctConstVolNoReheat;

            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(Alphas(3),
                                                         ErrorsFound,
                                                         sd_airterminal(SysNum).SysType,
                                                         Alphas(1),
                                                         NodeType_Air,
                                                         NodeConnectionType_Inlet,
                                                         1,
                                                         ObjectIsNotParent,
                                                         cAlphaFields(3));
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(Alphas(4),
                                                          ErrorsFound,
                                                          sd_airterminal(SysNum).SysType,
                                                          Alphas(1),
                                                          NodeType_Air,
                                                          NodeConnectionType_Outlet,
                                                          1,
                                                          ObjectIsNotParent,
                                                          cAlphaFields(4));

            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);
            sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
            sd_airterminal(SysNum).ZoneMinAirFracMethod = MinFracNotUsed;
            sd_airterminal(SysNum).DamperHeatingAction = HeatingActionNotUsed;

            sd_airterminal(SysNum).ReheatControlNode = 0;
            sd_airterminal(SysNum).ReheatAirOutletNode = sd_airterminal(SysNum).OutletNodeNum;
            sd_airterminal(SysNum).MaxReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MaxReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).ControllerOffset = 0.000001;

            // Register component set data
            TestCompSet(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).OutletNodeNum), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).OutletNodeNum));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).OutletNodeNum == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).OutletNodeNum) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).OutletNodeNum;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }
                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }

            if (lAlphaBlanks(5)) {
                sd_airterminal(SysNum).NoOAFlowInputFromUser = true;
            } else {
                sd_airterminal(SysNum).OARequirementsPtr = UtilityRoutines::FindItemInList(Alphas(5), DataSizing::OARequirements);
                if (sd_airterminal(SysNum).OARequirementsPtr == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFields(5) + "=\"" + Alphas(5) + "\".");
                    ErrorsFound = true;
                } else {
                    sd_airterminal(SysNum).NoOAFlowInputFromUser = false;
                }
            }

            if (lAlphaBlanks(6)) {
                sd_airterminal(SysNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
            } else {
                if (Alphas(6) == "CURRENTOCCUPANCY") {
                    sd_airterminal(SysNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                } else if (Alphas(6) == "DESIGNOCCUPANCY") {
                    sd_airterminal(SysNum).OAPerPersonMode = DataZoneEquipment::PerPersonByDesignLevel;
                } else {
                    sd_airterminal(SysNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFields(6) + "=\"" + Alphas(6) + "\". The default input of CurrentOccupancy is assigned");
                }
            }

            if (DataGlobals::AnyEnergyManagementSystemInModel) {
                // model results related actuators
                SetupEMSActuator("AirTerminal:SingleDuct:ConstantVolume:NoReheat",
                                 sd_airterminal(SysNum).SysName,
                                 "Mass Flow Rate",
                                 "[kg/s]",
                                 sd_airterminal(SysNum).EMSOverrideAirFlow,
                                 sd_airterminal(SysNum).EMSMassFlowRateValue);
                // model input related internal variables
                SetupEMSInternalVariable("AirTerminal:SingleDuct:ConstantVolume:NoReheat Maximum Mass Flow Rate",
                                         sd_airterminal(SysNum).SysName,
                                         "[kg/s]",
                                         sd_airterminal(SysNum).AirMassFlowRateMax);
            }

        } // End Number of Sys Loop

        for (SysIndex = 1; SysIndex <= NumNoRHVAVSys; ++SysIndex) {

            CurrentModuleObject = "AirTerminal:SingleDuct:VAV:NoReheat";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumCVNoReheatSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctVAVNoReheat;
            sd_airterminal(SysNum).ReheatComp = "";
            sd_airterminal(SysNum).ReheatName = "";
            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(Alphas(3),
                                                          ErrorsFound,
                                                          sd_airterminal(SysNum).SysType,
                                                          Alphas(1),
                                                          NodeType_Air,
                                                          NodeConnectionType_Outlet,
                                                          1,
                                                          ObjectIsNotParent,
                                                          cAlphaFields(3));
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(Alphas(4),
                                                         ErrorsFound,
                                                         sd_airterminal(SysNum).SysType,
                                                         Alphas(1),
                                                         NodeType_Air,
                                                         NodeConnectionType_Inlet,
                                                         1,
                                                         ObjectIsNotParent,
                                                         cAlphaFields(4));
            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);

            if (UtilityRoutines::SameString(Alphas(5), "Constant")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = ConstantMinFrac;
            } else if (UtilityRoutines::SameString(Alphas(5), "FixedFlowRate")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = FixedMin;
            } else if (UtilityRoutines::SameString(Alphas(5), "Scheduled")) {
                sd_airterminal(SysNum).ZoneMinAirFracMethod = ScheduledMinFrac;
            } else {
                ShowSevereError(cAlphaFields(5) + " = " + Alphas(5) + " not found.");
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }

            sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(2);
            if (lNumericBlanks(2)) {
                sd_airterminal(SysNum).ConstantMinAirFracSetByUser = false;
                sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
            } else {
                sd_airterminal(SysNum).ConstantMinAirFracSetByUser = true;
                sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(2);
                if (sd_airterminal(SysNum).ZoneMinAirFracMethod == FixedMin) {
                    ShowWarningError("Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(2) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
                }
            }

            sd_airterminal(SysNum).ZoneFixedMinAir = Numbers(3);
            if (lNumericBlanks(3)) {
                sd_airterminal(SysNum).FixedMinAirSetByUser = false;
                sd_airterminal(SysNum).DesignFixedMinAir = 0.0;
            } else {
                sd_airterminal(SysNum).FixedMinAirSetByUser = true;
                sd_airterminal(SysNum).DesignFixedMinAir = Numbers(3);
                if (sd_airterminal(SysNum).ZoneMinAirFracMethod == ConstantMinFrac) {
                    ShowWarningError("Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(3) + " will be ignored.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    sd_airterminal(SysNum).ZoneFixedMinAir = 0.0;
                }
            }
            sd_airterminal(SysNum).ZoneMinAirFracSchPtr = GetScheduleIndex(Alphas(6));
            if ((sd_airterminal(SysNum).ZoneMinAirFracSchPtr == 0) && (sd_airterminal(SysNum).ZoneMinAirFracMethod == ScheduledMinFrac)) {
                ShowSevereError(cAlphaFields(6) + " = " + Alphas(6) + " not found.");
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ShowContinueError("A valid schedule is required");
                ErrorsFound = true;
            } else if ((sd_airterminal(SysNum).ZoneMinAirFracSchPtr > 0) && (sd_airterminal(SysNum).ZoneMinAirFracMethod == ScheduledMinFrac)) {
                // check range of values in schedule
                if (!CheckScheduleValueMinMax(sd_airterminal(SysNum).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0)) {
                    ShowSevereError("Error found in " + cAlphaFields(6) + " = " + Alphas(6));
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ShowContinueError("Schedule values must be (>=0., <=1.)");
                }
            }

            sd_airterminal(SysNum).ReheatControlNode = 0;
            sd_airterminal(SysNum).ReheatAirOutletNode = sd_airterminal(SysNum).OutletNodeNum;
            sd_airterminal(SysNum).MaxReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MaxReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).ControllerOffset = 0.000001;
            sd_airterminal(SysNum).DamperHeatingAction = HeatingActionNotUsed;

            // Register component set data
            TestCompSet(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).OutletNodeNum), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).ReheatAirOutletNode == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).ReheatAirOutletNode) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).ReheatAirOutletNode;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }

                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }
            if (!lAlphaBlanks(7)) {
                sd_airterminal(SysNum).OARequirementsPtr = UtilityRoutines::FindItemInList(Alphas(7), OARequirements);
                if (sd_airterminal(SysNum).OARequirementsPtr == 0) {
                    ShowSevereError(cAlphaFields(7) + " = " + Alphas(7) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                } else {
                    sd_airterminal(SysNum).NoOAFlowInputFromUser = false;
                }
            }

            if (lAlphaBlanks(8)) {
                sd_airterminal(SysNum).ZoneTurndownMinAirFrac = 1.0;
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = false;
            } else {
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(Alphas(8));
                if (sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr == 0) {
                    ShowSevereError(cAlphaFields(8) + " = " + Alphas(8) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = true;
            }

            // Setup the Average damper Position output variable
            SetupOutputVariable("Zone Air Terminal VAV Damper Position",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).DamperPosition,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);
            SetupOutputVariable("Zone Air Terminal Minimum Air Flow Fraction",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).ZoneMinAirFracReport,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);

        } // end Number of Sys Loop

        for (SysIndex = 1; SysIndex <= NumNoRHCBVAVSys; ++SysIndex) {

            CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumCVNoReheatSys + NumNoRHVAVSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctCBVAVNoReheat;
            sd_airterminal(SysNum).ReheatComp = "";
            sd_airterminal(SysNum).ReheatName = "";
            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }
            sd_airterminal(SysNum).OutletNodeNum = GetOnlySingleNode(Alphas(3),
                                                          ErrorsFound,
                                                          sd_airterminal(SysNum).SysType,
                                                          Alphas(1),
                                                          NodeType_Air,
                                                          NodeConnectionType_Outlet,
                                                          1,
                                                          ObjectIsNotParent,
                                                          cAlphaFields(3));
            sd_airterminal(SysNum).InletNodeNum = GetOnlySingleNode(Alphas(4),
                                                         ErrorsFound,
                                                         sd_airterminal(SysNum).SysType,
                                                         Alphas(1),
                                                         NodeType_Air,
                                                         NodeConnectionType_Inlet,
                                                         1,
                                                         ObjectIsNotParent,
                                                         cAlphaFields(4));
            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);
            sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(2);
            if (sd_airterminal(SysNum).ZoneMinAirFracDes < 0.0) {
                ShowWarningError(sd_airterminal(SysNum).SysType + " = \"" + sd_airterminal(SysNum).SysName);
                ShowContinueError(cNumericFields(2) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues.");
                sd_airterminal(SysNum).ZoneMinAirFracDes = 0.0;
            }
            if (sd_airterminal(SysNum).ZoneMinAirFracDes > 1.0) {
                ShowWarningError(sd_airterminal(SysNum).SysType + " = \"" + sd_airterminal(SysNum).SysName);
                ShowContinueError(cNumericFields(2) + " must be less than or equal to 1. Resetting to 1 and the simulation continues.");
                sd_airterminal(SysNum).ZoneMinAirFracDes = 1.0;
            }

            sd_airterminal(SysNum).ReheatControlNode = 0;
            sd_airterminal(SysNum).ReheatAirOutletNode = sd_airterminal(SysNum).OutletNodeNum;
            sd_airterminal(SysNum).MaxReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MaxReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatWaterVolFlow = 0.0;
            sd_airterminal(SysNum).MinReheatSteamVolFlow = 0.0;
            sd_airterminal(SysNum).ControllerOffset = 0.000001;
            sd_airterminal(SysNum).DamperHeatingAction = HeatingActionNotUsed;

            // Register component set data
            TestCompSet(sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).OutletNodeNum), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).OutletNodeNum == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).ReheatAirOutletNode == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).ReheatAirOutletNode) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).ReheatAirOutletNode;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }
                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }

            if (lAlphaBlanks(5)) {
                sd_airterminal(SysNum).ZoneTurndownMinAirFrac = 1.0;
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = false;
            } else {
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(Alphas(5));
                if (sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr == 0) {
                    ShowSevereError(cAlphaFields(5) + " = " + Alphas(5) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = true;
            }

            // Setup the Average damper Position output variable
            SetupOutputVariable("Zone Air Terminal VAV Damper Position",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).DamperPosition,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);

        } // end Number of VAVHeatandCool:NoReheat Sys Loop

        // read in the SINGLE DUCT:VAV:REHEAT:VS FAN data
        for (SysIndex = 1; SysIndex <= NumVAVVS; ++SysIndex) {

            CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan";

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          SysIndex,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNums,
                                          IOStat,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumCVNoReheatSys + NumNoRHVAVSys + NumNoRHCBVAVSys;
            sd_airterminal(SysNum).SysNum = SysNum;
            GlobalNames::VerifyUniqueInterObjectName(SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
            sd_airterminal(SysNum).SysName = Alphas(1);
            sd_airterminal(SysNum).SysType = CurrentModuleObject;
            sd_airterminal(SysNum).SysType_Num = SingleDuctVAVReheatVSFan;
            sd_airterminal(SysNum).ReheatComp = Alphas(7);
            sd_airterminal(SysNum).ReheatName = Alphas(8);
            IsNotOK = false;
            if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Fuel")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Gas;
                sd_airterminal(SysNum).ReheatAirOutletNode = GetHeatingCoilOutletNode(state, sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                sd_airterminal(SysNum).ReheatCoilMaxCapacity = GetHeatingCoilCapacity(state, sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                if (IsNotOK) ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Electric")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_Electric;
                sd_airterminal(SysNum).ReheatAirOutletNode = GetHeatingCoilOutletNode(state, sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                sd_airterminal(SysNum).ReheatCoilMaxCapacity = GetHeatingCoilCapacity(state, sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                if (IsNotOK) ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Water")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SimpleHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).ReheatComp, "Coil:Heating:Steam")) {
                sd_airterminal(SysNum).ReheatComp_Num = HCoilType_SteamAirHeating;
                sd_airterminal(SysNum).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
            } else if (sd_airterminal(SysNum).ReheatComp != "") {
                ShowSevereError("Illegal " + cAlphaFields(7) + " = " + sd_airterminal(SysNum).ReheatComp + '.');
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            ValidateComponent(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).FanType = Alphas(5);
            if (UtilityRoutines::SameString(sd_airterminal(SysNum).FanType, "Fan:VariableVolume")) {
                sd_airterminal(SysNum).Fan_Num = DataHVACGlobals::FanType_SimpleVAV;
            } else if (UtilityRoutines::SameString(sd_airterminal(SysNum).FanType, "Fan:SystemModel")) {
                sd_airterminal(SysNum).Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
            } else if (sd_airterminal(SysNum).FanType != "") {
                ShowSevereError("Illegal " + cAlphaFields(5) + " = " + sd_airterminal(SysNum).FanType + '.');
                ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            sd_airterminal(SysNum).FanName = Alphas(6);
            ValidateComponent(sd_airterminal(SysNum).FanType, sd_airterminal(SysNum).FanName, IsNotOK, sd_airterminal(SysNum).SysType);
            if (IsNotOK) {
                ShowContinueError("In " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                ErrorsFound = true;
            }
            if (sd_airterminal(SysNum).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(
                    state.files, sd_airterminal(SysNum).FanName)); // call constructor, safe here because get input is not using DataIPShortCuts.
                sd_airterminal(SysNum).Fan_Index = HVACFan::getFanObjectVectorIndex(sd_airterminal(SysNum).FanName);
                sd_airterminal(SysNum).OutletNodeNum = HVACFan::fanObjs[sd_airterminal(SysNum).Fan_Index]->outletNodeNum;
                sd_airterminal(SysNum).InletNodeNum = HVACFan::fanObjs[sd_airterminal(SysNum).Fan_Index]->inletNodeNum;
                HVACFan::fanObjs[sd_airterminal(SysNum).Fan_Index]->fanIsSecondaryDriver = true;
            } else if (sd_airterminal(SysNum).Fan_Num == DataHVACGlobals::FanType_SimpleVAV) {
                IsNotOK = false;

                sd_airterminal(SysNum).OutletNodeNum = GetFanOutletNode(state.fans, state.files, sd_airterminal(SysNum).FanType, sd_airterminal(SysNum).FanName, IsNotOK);
                if (IsNotOK) {
                    ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }

                IsNotOK = false;
                sd_airterminal(SysNum).InletNodeNum = GetFanInletNode(state.fans, state.files, sd_airterminal(SysNum).FanType, sd_airterminal(SysNum).FanName, IsNotOK);
                if (IsNotOK) {
                    ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }

            sd_airterminal(SysNum).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                sd_airterminal(SysNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                sd_airterminal(SysNum).SchedPtr = GetScheduleIndex(Alphas(2));
                if (sd_airterminal(SysNum).SchedPtr == 0) {
                    ShowSevereError(cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
            }

            AirTermSysInletNodeName = NodeID(sd_airterminal(SysNum).InletNodeNum);
            if (!UtilityRoutines::SameString(Alphas(3), AirTermSysInletNodeName)) {
                ShowWarningError(RoutineName + "Invalid air terminal object air inlet node name in " + sd_airterminal(SysNum).SysType + " = " +
                                 sd_airterminal(SysNum).SysName);
                ShowContinueError(" Specified air inlet node name is = " + Alphas(3) + ".");
                ShowContinueError(" Expected air inlet node name is = " + AirTermSysInletNodeName + ".");
                // ErrorsFound = true;
            }

            sd_airterminal(SysNum).MaxAirVolFlowRate = Numbers(1);
            sd_airterminal(SysNum).MaxHeatAirVolFlowRate = Numbers(2);
            sd_airterminal(SysNum).ZoneMinAirFracDes = Numbers(3);
            // The reheat coil control node is necessary for hot water reheat, but not necessary for
            // electric or gas reheat.
            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Gas || sd_airterminal(SysNum).ReheatComp_Num == HCoilType_Electric) {
                //          IF(.NOT. lAlphaBlanks(6)) THEN
                //            CALL ShowWarningError('In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' // TRIM(sd_airterminal(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(6))//' is not needed and will be ignored.')
                //            CALL ShowContinueError('  It is used for hot water reheat coils only.')
                //          END IF
            } else {
                //          IF(lAlphaBlanks(6)) THEN
                //            CALL ShowSevereError('In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' // TRIM(sd_airterminal(SysNum)%SysName) &
                //                                 // ' the '//TRIM(cAlphaFields(6))//' is undefined')
                //            ErrorsFound=.TRUE.
                //          END IF
                if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilSteamInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    } else {
                        //  A4,     \field Unit supply air outlet node
                        //          \note same as heating coil air outlet node
                        //          \note same as zone inlet node
                        //          \type alpha
                        IsNotOK = false;
                        sd_airterminal(SysNum).ReheatAirOutletNode = GetCoilAirOutletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                        if (IsNotOK) {
                            ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                            ErrorsFound = true;
                        }
                    }
                    //               GetOnlySingleNode(Alphas(6),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                    //                                NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
                } else {
                    IsNotOK = false;
                    sd_airterminal(SysNum).ReheatControlNode = GetCoilWaterInletNode(sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                        ErrorsFound = true;
                    } else {
                        //  A4,     \field Unit supply air outlet node
                        //          \note same as heating coil air outlet node
                        //          \note same as zone inlet node
                        //          \type alpha
                        IsNotOK = false;
                        sd_airterminal(SysNum).ReheatAirOutletNode = GetCoilOutletNode(state, sd_airterminal(SysNum).ReheatComp, sd_airterminal(SysNum).ReheatName, IsNotOK);
                        if (IsNotOK) {
                            ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                            ErrorsFound = true;
                        }
                    }
                    //               GetOnlySingleNode(Alphas(6),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                    //                                NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
                }
            }
            //  A4,     \field Unit supply air outlet node
            //          \note same as heating coil air outlet node
            //          \note same as zone inlet node
            //          \type alpha
            //        sd_airterminal(SysNum)%ReheatAirOutletNode  = &
            //               GetOnlySingleNode(Alphas(4),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
            //                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
            AirTermSysOutletNodeName = NodeID(sd_airterminal(SysNum).ReheatAirOutletNode);
            if (!UtilityRoutines::SameString(Alphas(4), AirTermSysOutletNodeName)) {
                ShowWarningError(RoutineName + "Invalid air terminal object air outlet node name in " + sd_airterminal(SysNum).SysType + " = " +
                                 sd_airterminal(SysNum).SysName);
                ShowContinueError(" Specified air outlet node name is = " + Alphas(4) + ".");
                ShowContinueError(" Expected air outlet node name is = " + AirTermSysOutletNodeName + ".");
                // ErrorsFound = true;
            }

            if (sd_airterminal(SysNum).ReheatComp_Num == HCoilType_SteamAirHeating) {
                sd_airterminal(SysNum).MaxReheatSteamVolFlow = Numbers(4);
                sd_airterminal(SysNum).MinReheatSteamVolFlow = Numbers(5);
            } else {
                sd_airterminal(SysNum).MaxReheatWaterVolFlow = Numbers(4);
                sd_airterminal(SysNum).MinReheatWaterVolFlow = Numbers(5);
            }
            sd_airterminal(SysNum).ControllerOffset = Numbers(6);
            // Set default convergence tolerance
            if (sd_airterminal(SysNum).ControllerOffset <= 0.0) {
                sd_airterminal(SysNum).ControllerOffset = 0.001;
            }
            sd_airterminal(SysNum).DamperHeatingAction = HeatingActionNotUsed;

            // Register component set data
            TestCompSet(
                sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).ReheatAirOutletNode), "Air Nodes");

            for (ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (sd_airterminal(SysNum).ReheatAirOutletNode == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = sd_airterminal(SysNum).InletNodeNum;
                    sd_airterminal(SysNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (sd_airterminal(SysNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + sd_airterminal(SysNum).SysType + ',' + sd_airterminal(SysNum).SysName +
                                "].");
                ShowContinueError("...should have outlet node = " + NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
                ErrorsFound = true;
            } else {

                // Fill the Zone Equipment data with the inlet node number of this unit.
                // what if not found?  error?
                IsNotOK = true;
                for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (SupAirIn = 1; SupAirIn <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                        if (sd_airterminal(SysNum).ReheatAirOutletNode == ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                            IsNotOK = false;
                            if (ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                                ShowSevereError("Error in connecting a terminal unit to a zone");
                                ShowContinueError(NodeID(sd_airterminal(SysNum).ReheatAirOutletNode) + " already connects to another zone");
                                ShowContinueError("Occurs for terminal unit " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                                ShowContinueError("Check terminal unit node names for errors");
                                ErrorsFound = true;
                            } else {
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = sd_airterminal(SysNum).InletNodeNum;
                                ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = sd_airterminal(SysNum).ReheatAirOutletNode;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).TermUnitSizingNum =
                                    ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                                AirDistUnit(sd_airterminal(SysNum).ADUNum).ZoneEqNum = CtrlZone;
                            }
                            sd_airterminal(SysNum).CtrlZoneNum = CtrlZone;
                            sd_airterminal(SysNum).CtrlZoneInNodeIndex = SupAirIn;
                            sd_airterminal(SysNum).ActualZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            sd_airterminal(SysNum).ZoneFloorArea = Zone(sd_airterminal(SysNum).ActualZoneNum).FloorArea * Zone(sd_airterminal(SysNum).ActualZoneNum).Multiplier *
                                                        Zone(sd_airterminal(SysNum).ActualZoneNum).ListMultiplier;
                        }
                    }
                }
            }
            if (IsNotOK) {
                ShowWarningError("Did not Match Supply Air Outlet Node to any Zone Node");
                ShowContinueError("..Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
            }

            if (lAlphaBlanks(9)) {
                sd_airterminal(SysNum).ZoneTurndownMinAirFrac = 1.0;
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = false;
            } else {
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(Alphas(9));
                if (sd_airterminal(SysNum).ZoneTurndownMinAirFracSchPtr == 0) {
                    ShowSevereError(cAlphaFields(9) + " = " + Alphas(9) + " not found.");
                    ShowContinueError("Occurs in " + sd_airterminal(SysNum).SysType + " = " + sd_airterminal(SysNum).SysName);
                    ErrorsFound = true;
                }
                sd_airterminal(SysNum).ZoneTurndownMinAirFracSchExist = true;
            }

            // Add reheat coil to component sets array
            SetUpCompSets(sd_airterminal(SysNum).SysType,
                          sd_airterminal(SysNum).SysName,
                          Alphas(7),
                          Alphas(8),
                          NodeID(sd_airterminal(SysNum).OutletNodeNum),
                          NodeID(sd_airterminal(SysNum).ReheatAirOutletNode));
            // Add fan to component sets array
            SetUpCompSets(
                sd_airterminal(SysNum).SysType, sd_airterminal(SysNum).SysName, Alphas(5), Alphas(6), NodeID(sd_airterminal(SysNum).InletNodeNum), NodeID(sd_airterminal(SysNum).OutletNodeNum));

            // Setup the Average damper Position output variable
            SetupOutputVariable("Zone Air Terminal VAV Damper Position",
                                OutputProcessor::Unit::None,
                                sd_airterminal(SysNum).DamperPosition,
                                "System",
                                "Average",
                                sd_airterminal(SysNum).SysName);
        }

        // common report variable for all single duct air terminals
        for ( int sdIndex = 1; sdIndex <= NumSDAirTerminal; ++sdIndex) {
            SetupOutputVariable("Zone Air Terminal Outdoor Air Volume Flow Rate",
                OutputProcessor::Unit::m3_s,
                sd_airterminal(sdIndex).OutdoorAirFlowRate,
                "System",
                "Average",
                sd_airterminal(sdIndex).SysName );
        }

        // Error check to see if a single duct air terminal is assigned to zone that has zone secondary recirculation
        // specified in the Sizing:Zone object

        NumZoneSiz = inputProcessor->getNumObjectsFound("Sizing:Zone");
        if (NumZoneSiz > 0) {
            for (SysIndex = 1; SysIndex <= NumSDAirTerminal; ++SysIndex) {
                for (ZoneSizIndex = 1; ZoneSizIndex <= NumZoneSiz; ++ZoneSizIndex) {
                    if (DoZoneSizing) {
                        if (FinalZoneSizing(ZoneSizIndex).ActualZoneNum == sd_airterminal(SysIndex).ActualZoneNum) {
                            if (FinalZoneSizing(ZoneSizIndex).ZoneSecondaryRecirculation > 0.0) {
                                ShowWarningError(RoutineName + "A zone secondary recirculation fraction is specified for zone served by ");
                                ShowContinueError("...terminal unit \"" + sd_airterminal(SysIndex).SysName + "\" , that indicates a single path system");
                                ShowContinueError("...The zone secondary recirculation for that zone was set to 0.0");
                                FinalZoneSizing(ZoneSizIndex).ZoneSecondaryRecirculation = 0.0;
                                goto SizLoop_exit;
                            }
                        }
                    }
                }
            SizLoop_exit:;
            }
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    // End of Get Input subroutines for the Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void SingleDuctAirTerminal::InitSys(EnergyPlusData &state, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   January 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for  initializations of the Sys Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger events.

        // Using/Aliasing
        using DataDefineEquip::AirDistUnit;
        using DataGlobals::AnyPlantInModel;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::ZoneEquipConfig;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("InitSys");
        static std::string const RoutineNameFull("InitHVACSingleDuct");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int InletNode;
        int OutletNode;
        int SysIndex;
        static bool ZoneEquipmentListChecked(false); // True after the Zone Equipment List has been checked for items
        //static Array1D_bool MyEnvrnFlag;
        //static Array1D_bool MySizeFlag;
        //static Array1D_bool GetGasElecHeatCoilCap; // Gets autosized value of coil capacity
        Real64 SteamTemp;
        Real64 SteamDensity;
        Real64 rho;
        bool errFlag;

        //static Array1D_bool PlantLoopScanFlag;

        // FLOW:

        // Do the Begin Simulation initializations
        if (InitSysFlag) {

            //MyEnvrnFlag.allocate(NumSDAirTerminal);
            //MySizeFlag.allocate(NumSDAirTerminal);
            //PlantLoopScanFlag.allocate(NumSDAirTerminal);
            //GetGasElecHeatCoilCap.allocate(NumSDAirTerminal);
            //MyEnvrnFlag = true;
            //MySizeFlag = true;
            //PlantLoopScanFlag = true;
            //GetGasElecHeatCoilCap = true;
            InitSysFlag = false;
        }

        if (this->PlantLoopScanFlag && allocated(PlantLoop)) {
            if ((this->ReheatComp_PlantType == TypeOf_CoilWaterSimpleHeating) ||
                (this->ReheatComp_PlantType == TypeOf_CoilSteamAirHeating)) {
                // setup plant topology indices for plant fed heating coils
                errFlag = false;
                ScanPlantLoopsForObject(state.dataBranchInputManager,
                                        this->ReheatName,
                                        this->ReheatComp_PlantType,
                                        this->HWLoopNum,
                                        this->HWLoopSide,
                                        this->HWBranchIndex,
                                        this->HWCompIndex,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);

                if (errFlag) {
                    ShowContinueError("Reference Unit=\"" + this->SysName + "\", type=" + this->SysType);
                    ShowFatalError("InitSys: Program terminated for previous conditions.");
                }

                this->ReheatCoilOutletNode = PlantLoop(this->HWLoopNum)
                                                       .LoopSide(this->HWLoopSide)
                                                       .Branch(this->HWBranchIndex)
                                                       .Comp(this->HWCompIndex)
                                                       .NodeNumOut;

                this->PlantLoopScanFlag = false;
            } else {
                this->PlantLoopScanFlag = false;
            }
        } else if (this->PlantLoopScanFlag && !AnyPlantInModel) {
            this->PlantLoopScanFlag = false;
        }

        if (!ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            ZoneEquipmentListChecked = true;
            // Check to see if there is a Air Distribution Unit on the Zone Equipment List
            for (SysIndex = 1; SysIndex <= NumSDAirTerminal; ++SysIndex) {
                if (sd_airterminal(SysIndex).ADUNum == 0) continue;
                if (CheckZoneEquipmentList("ZoneHVAC:AirDistributionUnit", AirDistUnit(sd_airterminal(SysIndex).ADUNum).Name)) continue;
                ShowSevereError("InitSingleDuctSystems: ADU=[Air Distribution Unit," + AirDistUnit(sd_airterminal(SysIndex).ADUNum).Name +
                                "] is not on any ZoneHVAC:EquipmentList.");
                ShowContinueError("...System=[" + sd_airterminal(SysIndex).SysType + ',' + sd_airterminal(SysIndex).SysName + "] will not be simulated.");
            }
        }

        // get current time step air terminal box turndown minimum flow fraction
        if (this->ZoneTurndownMinAirFracSchExist) {
            this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(this->ZoneTurndownMinAirFracSchPtr);
        } else {
            this->ZoneTurndownMinAirFrac = 1.0;
        }

        if (!SysSizingCalc && this->MySizeFlag) {

            this->SizeSys();

            this->MySizeFlag = false;
        }

        if (this->GetGasElecHeatCoilCap) {
            if (this->ReheatComp_Num == HCoilType_Electric || this->ReheatComp_Num == HCoilType_Gas) {
                if (this->ReheatCoilMaxCapacity == AutoSize) {
                    errFlag = false;
                    this->ReheatCoilMaxCapacity = GetHeatingCoilCapacity(state, this->ReheatComp, this->ReheatName, errFlag);
                    if (errFlag) ShowContinueError("Occurs for terminal unit " + this->SysType + " = " + this->SysName);
                }
                if (this->ReheatCoilMaxCapacity != AutoSize) {
                    this->GetGasElecHeatCoilCap = false;
                }
            } else {
                this->GetGasElecHeatCoilCap = false;
            }
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag && this->MyEnvrnFlag) {

            // Set the outlet node max mass flow rate to the Max Air Flow specified for the Sys
            OutletNode = this->OutletNodeNum;
            InletNode = this->InletNodeNum;
            Node(OutletNode).MassFlowRateMax = this->MaxAirVolFlowRate * StdRhoAir;
            this->AirMassFlowRateMax = this->MaxAirVolFlowRate * StdRhoAir;
            this->HeatAirMassFlowRateMax = this->MaxHeatAirVolFlowRate * StdRhoAir;
            Node(InletNode).MassFlowRateMax = this->MaxAirVolFlowRate * StdRhoAir;
            this->MassFlowDiff = 1.0e-10 * this->AirMassFlowRateMax;

            if (this->HWLoopNum > 0 && this->ReheatComp_Num != HCoilType_SteamAirHeating) { // protect early calls before plant is setup
                rho = GetDensityGlycol(PlantLoop(this->HWLoopNum).FluidName,
                                       DataGlobals::HWInitConvTemp,
                                       PlantLoop(this->HWLoopNum).FluidIndex,
                                       RoutineName);
            } else {
                rho = 1000.0;
            }

            this->MaxReheatWaterFlow = rho * this->MaxReheatWaterVolFlow;
            this->MinReheatWaterFlow = rho * this->MinReheatWaterVolFlow;

            this->AirMassFlowDuringReheatMax = this->MaxAirVolFlowRateDuringReheat * StdRhoAir;

            // set the upstream leakage flowrate - remove from here - done in ZoneAirLoopEquipmentManager::SimZoneAirLoopEquipment

            if (this->ReheatComp_Num == HCoilType_SteamAirHeating) {
                SteamTemp = 100.0;
                SteamDensity = GetSatDensityRefrig(fluidNameSteam, SteamTemp, 1.0, this->FluidIndex, RoutineNameFull);
                this->MaxReheatSteamFlow = SteamDensity * this->MaxReheatSteamVolFlow;
                this->MinReheatSteamFlow = SteamDensity * this->MinReheatSteamVolFlow;
            }

            // get current environment air terminal box turndown minimum flow fraction
            Real64 CurrentEnvZoneTurndownMinAirFrac = 1.0;
            if (this->ZoneTurndownMinAirFracSchExist) {
                CurrentEnvZoneTurndownMinAirFrac = ScheduleManager::GetScheduleMinValue(this->ZoneTurndownMinAirFracSchPtr);
            }
            if ((this->SysType_Num == SingleDuctVAVReheat || this->SysType_Num == SingleDuctCBVAVReheat) ||
                (this->SysType_Num == SingleDuctCBVAVNoReheat)) {
                // need the lowest schedule value
                if (this->ZoneMinAirFracMethod == ScheduledMinFrac) {
                    this->ZoneMinAirFracDes = GetScheduleMinValue(this->ZoneMinAirFracSchPtr);
                }
                Node(OutletNode).MassFlowRateMin = Node(OutletNode).MassFlowRateMax * this->ZoneMinAirFracDes * CurrentEnvZoneTurndownMinAirFrac;
                Node(InletNode).MassFlowRateMin = Node(InletNode).MassFlowRateMax * this->ZoneMinAirFracDes * CurrentEnvZoneTurndownMinAirFrac;
            } else {
                Node(OutletNode).MassFlowRateMin = 0.0;
                Node(InletNode).MassFlowRateMin = 0.0;
            }
            if ((this->ReheatControlNode > 0) && !this->PlantLoopScanFlag) {
                if (this->ReheatComp_Num == HCoilType_SteamAirHeating) {
                    InitComponentNodes(this->MinReheatSteamFlow,
                                       this->MaxReheatSteamFlow,
                                       this->ReheatControlNode,
                                       this->ReheatCoilOutletNode,
                                       this->HWLoopNum,
                                       this->HWLoopSide,
                                       this->HWBranchIndex,
                                       this->HWCompIndex);
                } else {
                    InitComponentNodes(this->MinReheatWaterFlow,
                                       this->MaxReheatWaterFlow,
                                       this->ReheatControlNode,
                                       this->ReheatCoilOutletNode,
                                       this->HWLoopNum,
                                       this->HWLoopSide,
                                       this->HWBranchIndex,
                                       this->HWCompIndex);
                }
            }
            // Find air loop associated with terminal unit
            if ((this->CtrlZoneNum > 0) && (this->CtrlZoneInNodeIndex > 0)) {
                this->AirLoopNum = ZoneEquipConfig(this->CtrlZoneNum).InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
                AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
            }

            this->MyEnvrnFlag = false;
        }

        if (!BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        // Initialize the Inlet Nodes of the air side of air terminal
        InletNode = this->InletNodeNum;
        OutletNode = this->OutletNodeNum;

        Real64 mDotFromOARequirement(0.0);

        if (this->SysType_Num == SingleDuctConstVolNoReheat) {
            /*Real64 mDotFromOARequirement( 0.0 );*/
            if (!this->NoOAFlowInputFromUser) {
                mDotFromOARequirement = this->AirMassFlowRateMax;
                int airLoopNum(0);
                Real64 airLoopOAFrac(0.0);
                airLoopNum = this->AirLoopNum;
                if (airLoopNum > 0) {
                    airLoopOAFrac = DataAirLoop::AirLoopFlow(airLoopNum).OAFrac;
                    bool UseOccSchFlag = false;
                    if (this->OAPerPersonMode == DataZoneEquipment::PerPersonDCVByCurrentLevel) UseOccSchFlag = true;
                    if (airLoopOAFrac > 0.0) {
                        Real64 vDotOAReq = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(
                            this->OARequirementsPtr, this->CtrlZoneNum, UseOccSchFlag, true);
                        mDotFromOARequirement = vDotOAReq * DataEnvironment::StdRhoAir / airLoopOAFrac;
                        mDotFromOARequirement = min(mDotFromOARequirement, this->AirMassFlowRateMax);
                    } else {
                        mDotFromOARequirement = this->AirMassFlowRateMax;
                    }
                }
            }
        }

        if (this->ZoneMinAirFracMethod == ScheduledMinFrac) {
            this->ZoneMinAirFracDes = GetCurrentScheduleValue(this->ZoneMinAirFracSchPtr);
            // now reset inlet node min avail
            Node(InletNode).MassFlowRateMinAvail = this->AirMassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
        }

        if (FirstHVACIteration) {
            // The first time through set the mass flow rate to the Max
            if ((Node(InletNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                if (!(AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                      AirflowNetwork::AirflowNetworkFanActivated)) {
                    Node(InletNode).MassFlowRate = this->AirMassFlowRateMax;
                }
            } else {
                Node(InletNode).MassFlowRate = 0.0;
            }
            if ((Node(InletNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                if (!(AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                      AirflowNetwork::AirflowNetworkFanActivated)) {
                    if (this->SysType_Num == SingleDuctConstVolNoReheat) {
                        if (this->NoOAFlowInputFromUser) {
                            Node(InletNode).MassFlowRate = this->AirMassFlowRateMax;
                            Node(InletNode).MassFlowRateMaxAvail = this->AirMassFlowRateMax;
                        } else {
                            Node(InletNode).MassFlowRate = mDotFromOARequirement;
                            Node(InletNode).MassFlowRateMaxAvail = mDotFromOARequirement;
                        }
                        if (this->EMSOverrideAirFlow) {
                            Node(InletNode).MassFlowRate = this->EMSMassFlowRateValue;
                            Node(InletNode).MassFlowRateMaxAvail = this->EMSMassFlowRateValue;
                        }
                    } else {
                        Node(InletNode).MassFlowRateMaxAvail = this->AirMassFlowRateMax;
                    }
                }
            } else {
                Node(InletNode).MassFlowRateMaxAvail = 0.0;
            }

            if ((Node(InletNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                if (!(AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                      AirflowNetwork::AirflowNetworkFanActivated)) {
                    Node(InletNode).MassFlowRateMinAvail = this->AirMassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                }
            } else {
                Node(InletNode).MassFlowRateMinAvail = 0.0;
            }
            // reset the mass flow rate histories
            this->MassFlow1 = 0.0;
            this->MassFlow2 = 0.0;
            this->MassFlow3 = 0.0;
            this->MassFlow3 = 0.0;

        } else {
            if (this->SysType_Num == SingleDuctConstVolNoReheat) {
                if (!this->EMSOverrideAirFlow) {
                    if ((Node(InletNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
                        if (this->NoOAFlowInputFromUser) {
                            if (Node(InletNode).MassFlowRateMaxAvail < Node(InletNode).MassFlowRateMax) {
                                Node(InletNode).MassFlowRate = Node(InletNode).MassFlowRateMaxAvail;
                            } else if (Node(InletNode).MassFlowRateMinAvail > Node(InletNode).MassFlowRateMin) {
                                Node(InletNode).MassFlowRate = Node(InletNode).MassFlowRateMinAvail;
                            } else {
                                Node(InletNode).MassFlowRate = Node(InletNode).MassFlowRateMaxAvail;
                            }
                        } else {
                            Node(InletNode).MassFlowRate = mDotFromOARequirement;
                            // but also apply constraints
                            Node(InletNode).MassFlowRate = min(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMaxAvail);
                            Node(InletNode).MassFlowRate = min(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMax);
                            Node(InletNode).MassFlowRate = max(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMinAvail);
                            Node(InletNode).MassFlowRate = max(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMin);
                        }
                    } else {
                        Node(InletNode).MassFlowRate = 0.0;
                        Node(InletNode).MassFlowRateMaxAvail = 0.0;
                        Node(InletNode).MassFlowRateMinAvail = 0.0;
                    }
                } else { // EMS override on
                    Node(InletNode).MassFlowRate = this->EMSMassFlowRateValue;
                    // but also apply constraints
                    Node(InletNode).MassFlowRate = min(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMaxAvail);
                    Node(InletNode).MassFlowRate = min(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMax);
                    Node(InletNode).MassFlowRate = max(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMinAvail);
                    Node(InletNode).MassFlowRate = max(Node(InletNode).MassFlowRate, Node(InletNode).MassFlowRateMin);
                }
            }
        }

        // Do a check and make sure that the max and min available(control) flow is
        //  between the physical max and min while operating.
        this->sd_airterminalInlet.AirMassFlowRateMaxAvail = min(this->AirMassFlowRateMax, Node(InletNode).MassFlowRateMaxAvail);
        this->sd_airterminalInlet.AirMassFlowRateMinAvail =
            min(max(Node(OutletNode).MassFlowRateMin, Node(InletNode).MassFlowRateMinAvail), this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

        // Do the following initializations (every time step): This should be the info from
        // the previous components outlets or the node data in this section.
        // Load the node data in this section for the component simulation
        this->sd_airterminalInlet.AirMassFlowRate = Node(InletNode).MassFlowRate;
        this->sd_airterminalInlet.AirTemp = Node(InletNode).Temp;
        this->sd_airterminalInlet.AirHumRat = Node(InletNode).HumRat;
        this->sd_airterminalInlet.AirEnthalpy = Node(InletNode).Enthalpy;
        // set to zero, now it is used for constant volume with no reheat air terminal
        this->HeatRate = 0.0;
        this->CoolRate = 0.0;
        this->HeatEnergy = 0.0;
        this->CoolEnergy = 0.0;

        // update to the current minimum air flow fraction
        this->ZoneMinAirFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;

    }

    void SingleDuctAirTerminal::SizeSys()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   September 2001
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Sys Components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays.

        // Using/Aliasing
        using DataGlobals::AutoCalculate;
        using DataHeatBalance::Zone;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;
        using General::RoundSigDigits;
        using General::SafeDivide;
        using General::TrimSigDigits;
        using PlantUtilities::MyPlantSizingIndex;
        using ReportSizingManager::ReportSizingOutput;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetCoilSteamOutletNode;
        using WaterCoils::GetCoilWaterInletNode;
        using WaterCoils::GetCoilWaterOutletNode;
        using WaterCoils::SetCoilDesFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeSys");
        static std::string const RoutineNameFull("SizeHVACSingleDuct");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum; // index of plant sizing object for 1st heating loop
        static Real64 CoilInTemp(0.0);
        static Real64 DesCoilLoad(0.0);
        static Real64 DesZoneHeatLoad(0.0);
        static Real64 ZoneDesTemp(0.0);
        static Real64 ZoneDesHumRat(0.0);
        Real64 DesMassFlow;
        Real64 TempSteamIn;
        Real64 EnthSteamOutWet;
        Real64 EnthSteamInDry;
        Real64 LatentHeatSteam;
        Real64 SteamDensity;
        static int CoilWaterInletNode(0);
        static int CoilWaterOutletNode(0);
        static int CoilSteamInletNode(0);
        static int CoilSteamOutletNode(0);

        bool ErrorsFound;
        bool PlantSizingErrorsFound;
        Real64 rho; // local fluid density
        Real64 Cp;  // local fluid specific heat
        static int DummyWaterIndex(1);
        static Real64 UserInputMaxHeatAirVolFlowRate(0.0); // user input for MaxHeatAirVolFlowRate
        bool IsAutoSize;
        int ZoneNum(0);
        Real64 MinMinFlowRatio(0.0);              // the minimum minimum flow ratio
        Real64 MaxAirVolFlowRateDes;              // Autosized maximum air flow rate for reporting
        Real64 MaxAirVolFlowRateUser;             // Hardsized maximum air flow rate for reporting
        Real64 MaxHeatAirVolFlowRateDes;          // Autosized maximum heating air flow rate for reporting
        Real64 MaxHeatAirVolFlowRateUser;         // Hardsized maximum heating air flow rate for reporting
        Real64 MinAirFlowFracDes;                 // Autosized minimum cooling air flow fraction for reporting
        Real64 MinAirFlowFracUser;                // User input minimum cooling air flow fraction for reporting
        Real64 FixedMinAirDes;                    // Autosized minimum cooling air flow rate for reporting [m3/s]
        Real64 FixedMinAirUser;                   // User input minimum cooling air flow rate for reporting [m3/s]
        Real64 MaxAirVolFlowRateDuringReheatDes;  // Autosized maximum air flow durign reheat for reporting
        Real64 MaxAirVolFlowRateDuringReheatUser; // Hardsized maximum air flow durign reheat for reporting
        Real64 MaxAirVolFractionDuringReheatDes;  // Autosized maximum air fraction durign reheat for reporting
        Real64 MaxAirVolFractionDuringReheatUser; // Hardsized maximum air flow durign reheat for reporting
        Real64 MaxReheatWaterVolFlowDes;          // Autosized reheat water flow or reporting
        Real64 MaxReheatWaterVolFlowUser;         // Hardsized reheat water flow for reporting
        Real64 MaxReheatSteamVolFlowDes;          // Autosized reheat steam flow for reporting
        Real64 MaxReheatSteamVolFlowUser;         // Hardsized reheat steam flow for reporting

        PltSizHeatNum = 0;
        DesMassFlow = 0.0;
        ErrorsFound = false;
        IsAutoSize = false;
        MaxAirVolFlowRateDes = 0.0;
        MaxAirVolFlowRateUser = 0.0;
        MaxHeatAirVolFlowRateDes = 0.0;
        MaxHeatAirVolFlowRateUser = 0.0;
        MinAirFlowFracDes = 0.0;
        MinAirFlowFracUser = 0.0;
        FixedMinAirDes = 0.0;
        FixedMinAirUser = 0.0;
        MaxAirVolFlowRateDuringReheatDes = 0.0;
        MaxAirVolFlowRateDuringReheatUser = 0.0;
        MaxAirVolFractionDuringReheatDes = 0.0;
        MaxAirVolFractionDuringReheatUser = 0.0;
        MaxReheatWaterVolFlowDes = 0.0;
        MaxReheatWaterVolFlowUser = 0.0;
        MaxReheatSteamVolFlowDes = 0.0;
        MaxReheatSteamVolFlowUser = 0.0;
        MinMinFlowRatio = 0.0;

        ZoneNum = this->ActualZoneNum;

        if (this->MaxAirVolFlowRate == AutoSize) {
            IsAutoSize = true;
        }

        if (CurTermUnitSizingNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) { // simulation continue
                if (this->MaxAirVolFlowRate > 0.0) {
                    ReportSizingOutput(
                        this->SysType, this->SysName, "User-Specified Maximum Air Flow Rate [m3/s]", this->MaxAirVolFlowRate);
                }
            } else { // Autosize or hard-size with sizing run

                CheckZoneSizing(this->SysType, this->SysName);

                MaxAirVolFlowRateDes =
                    max(TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow, TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);

                if (MaxAirVolFlowRateDes < SmallAirVolFlow) {
                    MaxAirVolFlowRateDes = 0.0;
                }
                if (IsAutoSize) {
                    this->MaxAirVolFlowRate = MaxAirVolFlowRateDes;
                    ReportSizingOutput(this->SysType, this->SysName, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes);
                } else { // Hard-size with sizing data
                    if (this->MaxAirVolFlowRate > 0.0 && MaxAirVolFlowRateDes > 0.0) {
                        MaxAirVolFlowRateUser = this->MaxAirVolFlowRate;
                        ReportSizingOutput(this->SysType,
                                           this->SysName,
                                           "Design Size Maximum Air Flow Rate [m3/s]",
                                           MaxAirVolFlowRateDes,
                                           "User-Specified Maximum Air Flow Rate [m3/s]",
                                           MaxAirVolFlowRateUser);
                        if (DisplayExtraWarnings) {
                            if ((std::abs(MaxAirVolFlowRateDes - MaxAirVolFlowRateUser) / MaxAirVolFlowRateUser) > AutoVsHardSizingThreshold) {
                                ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                            this->SysName + "\".");
                                ShowContinueError("User-Specified Maximum Air Flow Rate of " + RoundSigDigits(MaxAirVolFlowRateUser, 5) + " [m3/s]");
                                ShowContinueError("differs from Design Size Maximum Air Flow Rate of " + RoundSigDigits(MaxAirVolFlowRateDes, 5) +
                                                  " [m3/s]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (this->MaxHeatAirVolFlowRate == AutoSize) {
            IsAutoSize = true;
        }
        if (CurTermUnitSizingNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) { // simulation should continue
                UserInputMaxHeatAirVolFlowRate = this->MaxHeatAirVolFlowRate;
                if (this->MaxHeatAirVolFlowRate > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "User-Specified Maximum Heating Air Flow Rate [m3/s]",
                                       this->MaxHeatAirVolFlowRate);
                }
            } else {
                CheckZoneSizing(this->SysType, this->SysName);
                MaxHeatAirVolFlowRateDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow;
                if (MaxHeatAirVolFlowRateDes < SmallAirVolFlow) {
                    MaxHeatAirVolFlowRateDes = 0.0;
                }
                if (IsAutoSize) {
                    this->MaxHeatAirVolFlowRate = MaxHeatAirVolFlowRateDes;
                    UserInputMaxHeatAirVolFlowRate = 0.0;
                    ReportSizingOutput(
                        this->SysType, this->SysName, "Design Size Maximum Heating Air Flow Rate [m3/s]", MaxHeatAirVolFlowRateDes);
                } else { // Hard-size with sizing data
                    if (this->MaxHeatAirVolFlowRate > 0.0 && MaxHeatAirVolFlowRateDes > 0.0) {
                        MaxHeatAirVolFlowRateUser = this->MaxHeatAirVolFlowRate;
                        UserInputMaxHeatAirVolFlowRate = this->MaxHeatAirVolFlowRate;
                        ReportSizingOutput(this->SysType,
                                           this->SysName,
                                           "Design Size Maximum Heating Air Flow Rate [m3/s]",
                                           MaxHeatAirVolFlowRateDes,
                                           "User-Specified Maximum Heating Air Flow Rate [m3/s]",
                                           MaxHeatAirVolFlowRateUser);
                        if (DisplayExtraWarnings) {
                            if ((std::abs(MaxHeatAirVolFlowRateDes - MaxHeatAirVolFlowRateUser) / MaxHeatAirVolFlowRateUser) >
                                AutoVsHardSizingThreshold) {
                                ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                            this->SysName + "\".");
                                ShowContinueError("User-Specified Maximum Heating Air Flow Rate of " + RoundSigDigits(MaxHeatAirVolFlowRateUser, 5) +
                                                  " [m3/s]");
                                ShowContinueError("differs from Design Size Maximum Heating Air Flow Rate of " +
                                                  RoundSigDigits(MaxHeatAirVolFlowRateDes, 5) + " [m3/s]");
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        // get design day terminal unit turndown minimum flow fraction
        if (this->ZoneTurndownMinAirFracSchExist) {
            this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(this->ZoneTurndownMinAirFracSchPtr);
        } else {
            this->ZoneTurndownMinAirFrac = 1.0;
        }

        IsAutoSize = false;
        if (this->ZoneMinAirFracDes == AutoSize) {
            IsAutoSize = true;
        }
        if (this->ZoneMinAirFracMethod == ConstantMinFrac) {
            if (ZoneSizingRunDone) {
                if (CurTermUnitSizingNum > 0) {
                    // use the combined defaults or other user inputs stored in DesCoolVolFlowMin
                    if (this->MaxAirVolFlowRate > 0.0) {
                        MinAirFlowFracDes = min(1.0, TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlowMin / this->MaxAirVolFlowRate);
                    } else {
                        MinAirFlowFracDes = 0.0;
                    }
                }
            } else {
                // if no zone sizing values available; use max of min frac = 0.2 and 0.000762 [m3/s-m2]
                if (this->MaxAirVolFlowRate > 0.0) {
                    MinMinFlowRatio = (0.000762 * Zone(ZoneNum).FloorArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier) /
                                      this->MaxAirVolFlowRate;
                    MinAirFlowFracDes = max(0.2, MinMinFlowRatio);
                } else {
                    MinAirFlowFracDes = 0.0;
                }
            }
            if (IsAutoSize) {
                // report out autosized result and save value in Sys array
                ReportSizingOutput(this->SysType, this->SysName, "Design Size Constant Minimum Air Flow Fraction", MinAirFlowFracDes * this->ZoneTurndownMinAirFrac);
                this->ZoneMinAirFracDes = MinAirFlowFracDes;
            } else {
                // report out hard (user set) value and issue warning if appropriate
                MinAirFlowFracUser = this->ZoneMinAirFracDes;
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Constant Minimum Air Flow Fraction",
                                   MinAirFlowFracDes * this->ZoneTurndownMinAirFrac,
                                   "User-Specified Constant Minimum Air Flow Fraction",
                                   MinAirFlowFracUser * this->ZoneTurndownMinAirFrac);
                if (DisplayExtraWarnings) {
                    if ((std::abs(MinAirFlowFracDes - MinAirFlowFracUser) / MinAirFlowFracUser) > AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Minimum Cooling Air Flow Fraction of " + RoundSigDigits(MinAirFlowFracUser, 5) + " [m3/s]");
                        ShowContinueError("differs from Design Size Minimum Cooling Air Flow Fraction of " + RoundSigDigits(MinAirFlowFracDes, 5) +
                                          " [m3/s]");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
            // report out the min air flow rate set by min air flow frac
            ReportSizingOutput(this->SysType,
                               this->SysName,
                               "Design Size Minimum Air Flow Rate [m3/s]",
                               this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
        } else {
            if (IsAutoSize) {
                this->ZoneMinAirFracDes = 0.0;
            }
        }

        IsAutoSize = false;
        if (this->ZoneFixedMinAir == AutoSize) {
            IsAutoSize = true;
        }
        if (this->ZoneMinAirFracMethod == FixedMin) {
            if (ZoneSizingRunDone) {
                if (CurTermUnitSizingNum > 0) {
                    // use the combined defaults or other user inputs stored in DesCoolVolFlowMin
                    if (this->MaxAirVolFlowRate > 0.0) {
                        FixedMinAirDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlowMin;
                    } else {
                        MinAirFlowFracDes = 0.0;
                    }
                }
            } else {
                // if no zone sizing values available; use max of min frac = 0.2 and 0.000762 [m3/s-m2]
                if (this->MaxAirVolFlowRate > 0.0) {
                    FixedMinAirDes = max(0.2 * this->MaxAirVolFlowRate,
                                         0.000762 * Zone(ZoneNum).FloorArea * Zone(ZoneNum).Multiplier * Zone(ZoneNum).ListMultiplier);
                } else {
                    MinAirFlowFracDes = 0.0;
                }
            }
            if (IsAutoSize) {
                // report out autosized result and save value in Sys array
                ReportSizingOutput(this->SysType, this->SysName, "Design Size Fixed Minimum Air Flow Rate [m3/s]", FixedMinAirDes * this->ZoneTurndownMinAirFrac);
                this->ZoneFixedMinAir = FixedMinAirDes;
            } else {
                // report out hard (user set) value and issue warning if appropriate
                FixedMinAirUser = this->ZoneFixedMinAir;
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Fixed Minimum Air Flow Rate [m3/s]",
                                   FixedMinAirDes * this->ZoneTurndownMinAirFrac,
                                   "User-Specified Fixed Minimum Air Flow Rate [m3/s]",
                                   FixedMinAirUser * this->ZoneTurndownMinAirFrac);
                if (DisplayExtraWarnings) {
                    if ((std::abs(FixedMinAirDes - FixedMinAirUser) / FixedMinAirUser) > AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Minimum Cooling Air Flow Rate of " + RoundSigDigits(FixedMinAirUser, 5) + " [m3/s]");
                        ShowContinueError("differs from Design Size Minimum Cooling Air Flow Rate of " + RoundSigDigits(FixedMinAirDes, 5) +
                                          " [m3/s]");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
            // report out the min air flow frac set by the min air flow rate
            if (this->MaxAirVolFlowRate > 0.0) {
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Minimum Air Flow Fraction [m3/s]",
                                   this->ZoneFixedMinAir * this->ZoneTurndownMinAirFrac / this->MaxAirVolFlowRate);
            }
        } else {
            if (IsAutoSize) {
                this->ZoneFixedMinAir = 0.0;
            }
        }

        if (this->ZoneMinAirFracMethod == ScheduledMinFrac) {
            // need a value for sizing.
            if (this->ConstantMinAirFracSetByUser) {
                this->ZoneMinAirFracDes = this->DesignMinAirFrac;
                // if both inputs are defined, use the max
                if (this->FixedMinAirSetByUser) {
                    this->ZoneMinAirFracDes =
                        min(1.0, max(this->ZoneMinAirFracDes, SafeDivide(this->DesignFixedMinAir, this->MaxAirVolFlowRate)));
                }
                // if only fixed is defined, use the value
            } else if (this->FixedMinAirSetByUser) {
                this->ZoneMinAirFracDes = min(1.0, SafeDivide(this->DesignFixedMinAir, this->MaxAirVolFlowRate));
            } else {
                // use an average of min and max in schedule
                this->ZoneMinAirFracDes =
                    (GetScheduleMinValue(this->ZoneMinAirFracSchPtr) + GetScheduleMaxValue(this->ZoneMinAirFracSchPtr)) / 2.0;
            }
        }

        if (this->ZoneMinAirFracMethod == FixedMin) {
            // need a value for sizing.
            this->ZoneMinAirFracDes = min(1.0, SafeDivide(this->ZoneFixedMinAir, this->MaxAirVolFlowRate));
        }

        if (this->DamperHeatingAction == ReverseActionWithLimits) {
            if (ZoneSizingRunDone) {
                if (CurTermUnitSizingNum > 0) {
                    // if zone sizing run done, set the design max reheat air flow to the value from the design calcs
                    MaxAirVolFlowRateDuringReheatDes = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlowMax;
                }
            } else {
                // if no design calc use 0.002032 [m3/s-m2] times floor area. That's .40 cfm/ft2
                MaxAirVolFlowRateDuringReheatDes = min(0.002032 * this->ZoneFloorArea, this->MaxAirVolFlowRate);
            }
            // check that result is not greater than the max flow or less than the min flow.
            MaxAirVolFlowRateDuringReheatDes = min(MaxAirVolFlowRateDuringReheatDes, this->MaxAirVolFlowRate);
            MaxAirVolFlowRateDuringReheatDes = max(MaxAirVolFlowRateDuringReheatDes, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes));
            if (this->MaxAirVolFlowRate > 0.0) {
                MaxAirVolFractionDuringReheatDes = MaxAirVolFlowRateDuringReheatDes / this->MaxAirVolFlowRate;
            } else {
                MaxAirVolFractionDuringReheatDes = 0.0;
            }
            if (this->MaxAirVolFlowRateDuringReheat == AutoCalculate && this->MaxAirVolFractionDuringReheat == AutoCalculate) {
                // if both inputs are autosize (the default) report both out and save in the Sys array.
                ReportSizingOutput(
                    this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes);
                if (this->ZoneFloorArea > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                       MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea);
                }
                this->MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes;
                this->MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes;
            } else if (this->MaxAirVolFlowRateDuringReheat == AutoCalculate && this->MaxAirVolFractionDuringReheat != AutoCalculate) {
                // if max reheat flow fraction was input, set the max reheat flow design value correspondingly, report both out.
                // Check for optional caution message that user input value is not within 10% of the design value.
                MaxAirVolFlowRateDuringReheatDes = this->MaxAirVolFractionDuringReheat * this->MaxAirVolFlowRate;
                MaxAirVolFractionDuringReheatUser = this->MaxAirVolFractionDuringReheat;
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Maximum Flow Fraction during Reheat []",
                                   MaxAirVolFractionDuringReheatDes,
                                   "User-Specified Maximum Flow Fraction during Reheat []",
                                   MaxAirVolFractionDuringReheatUser);
                if (this->ZoneFloorArea > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                       MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea);
                }
                this->MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes;
                if (DisplayExtraWarnings) {
                    if ((std::abs(MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser) / MaxAirVolFractionDuringReheatUser) >
                        AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Maximum Flow Fraction during Reheat of " +
                                          RoundSigDigits(MaxAirVolFractionDuringReheatUser, 5) + " []");
                        ShowContinueError("differs from Design Size Maximum Flow Fraction during Reheat of " +
                                          RoundSigDigits(MaxAirVolFractionDuringReheatDes, 5) + " []");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            } else if (this->MaxAirVolFlowRateDuringReheat != AutoCalculate && this->MaxAirVolFractionDuringReheat == AutoCalculate) {
                // if max reheat flow was input set the design max reheat flow frac to the corresponding value, report both out, save the design value
                // of the flow frac in Sys. Check for optional caution message that user input value is not within 10% of the design value.
                if (this->MaxAirVolFlowRate > 0.0) {
                    MaxAirVolFractionDuringReheatDes = MaxAirVolFlowRateDuringReheatDes / this->MaxAirVolFlowRate;
                } else {
                    MaxAirVolFractionDuringReheatDes = 0.0;
                }
                MaxAirVolFlowRateDuringReheatUser = this->MaxAirVolFlowRateDuringReheat;
                ReportSizingOutput(
                    this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes);
                if (this->ZoneFloorArea > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "Design Size Maximum Flow per Zone Floor Area during Reheat [ m3/s-m2 ]",
                                       MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea,
                                       "User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                       MaxAirVolFlowRateDuringReheatUser / this->ZoneFloorArea);
                }
                this->MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes;
                if (DisplayExtraWarnings) {
                    if ((std::abs(MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser) / MaxAirVolFlowRateDuringReheatUser) >
                        AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Maximum Flow per Zone Floor Area during Reheat of " +
                                          RoundSigDigits(MaxAirVolFlowRateDuringReheatUser, 5) + " [m3/s-m2]");
                        ShowContinueError("differs from Design Size Maximum Flow per Zone Floor Area during Reheat of " +
                                          RoundSigDigits(MaxAirVolFlowRateDuringReheatDes, 5) + " [m3/s-m2]");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            } else {
                // both fields have user input. Report both out, use the larger of the 2 values. Note that only sd_airterminal( SysNum
                // ).MaxAirVolFlowRateDuringReheat is used subsequently. Check both inputs for optional caution message that user input value is not
                // within 10% of the design value.
                MaxAirVolFlowRateDuringReheatUser = this->MaxAirVolFlowRateDuringReheat;
                MaxAirVolFractionDuringReheatUser = this->MaxAirVolFractionDuringReheat;
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Maximum Flow Fraction during Reheat []",
                                   MaxAirVolFractionDuringReheatDes,
                                   "User-Specified Maximum Flow Fraction during Reheat []",
                                   MaxAirVolFractionDuringReheatUser);
                if (this->ZoneFloorArea > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                       MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea,
                                       "User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                       MaxAirVolFlowRateDuringReheatUser / this->ZoneFloorArea);
                }
                this->MaxAirVolFlowRateDuringReheat =
                    max(this->MaxAirVolFlowRateDuringReheat, this->MaxAirVolFractionDuringReheat * this->MaxAirVolFlowRate);
                if (DisplayExtraWarnings) {
                    if ((std::abs(MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser) / MaxAirVolFractionDuringReheatUser) >
                        AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Maximum Flow Fraction during Reheat of " +
                                          RoundSigDigits(MaxAirVolFractionDuringReheatUser, 5) + " []");
                        ShowContinueError("differs from Design Size Maximum Flow Fraction during Reheat of " +
                                          RoundSigDigits(MaxAirVolFractionDuringReheatDes, 5) + " []");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
                if (DisplayExtraWarnings) {
                    if ((std::abs(MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser) / MaxAirVolFlowRateDuringReheatUser) >
                        AutoVsHardSizingThreshold) {
                        ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                    this->SysName + "\".");
                        ShowContinueError("User-Specified Maximum Flow per Zone Floor Area during Reheat of " +
                                          RoundSigDigits(MaxAirVolFlowRateDuringReheatUser, 5) + " [m3/s-m2]");
                        ShowContinueError("differs from Design Size Maximum Flow per Zone Floor Area during Reheat of " +
                                          RoundSigDigits(MaxAirVolFlowRateDuringReheatDes, 5) + " [m3/s-m2]");
                        ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
            // check that MaxAirVolFlowRateDuringReheat is greater than the min and less than the max
            this->MaxAirVolFlowRateDuringReheat = min(MaxAirVolFlowRateDuringReheatDes, this->MaxAirVolFlowRate);
            this->MaxAirVolFlowRateDuringReheat =
                max(MaxAirVolFlowRateDuringReheatDes, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes));
        } else if (this->DamperHeatingAction == Normal) {
            // for Normal action, max reheat flow is equal to the minimum. Report it.
            if (this->ZoneFloorArea > 0.0) {
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                   (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes) / this->ZoneFloorArea);
            }
            ReportSizingOutput(
                this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", this->ZoneMinAirFracDes);
            // zero the ReverseActioWithLimits inputs
            this->MaxAirVolFlowRateDuringReheat = max(this->MaxAirVolFlowRateDuringReheat, 0.0);
            this->MaxAirVolFractionDuringReheat = max(this->MaxAirVolFractionDuringReheat, 0.0);
        } else if (this->DamperHeatingAction == ReverseAction) {
            // for ReverseAction, max reheat flow is equal to the maximum. Report it.
            if (this->ZoneFloorArea > 0.0) {
                ReportSizingOutput(this->SysType,
                                   this->SysName,
                                   "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                   this->MaxAirVolFlowRate / this->ZoneFloorArea);
            }
            ReportSizingOutput(this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", 1.0);
            // zero the ReverseActioWithLimits inputs
            this->MaxAirVolFlowRateDuringReheat = max(this->MaxAirVolFlowRateDuringReheat, 0.0);
            this->MaxAirVolFractionDuringReheat = max(this->MaxAirVolFractionDuringReheat, 0.0);
        }

        if (CurTermUnitSizingNum > 0) {
            TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult = 1.0;
            TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = 1.0;
            if (ZoneSizingRunDone) {
                if (this->SysType_Num == SingleDuctVAVReheatVSFan) {
                    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = max(UserInputMaxHeatAirVolFlowRate,
                                                                          TermUnitFinalZoneSizing(CurTermUnitSizingNum).NonAirSysDesHeatVolFlow,
                                                                          this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
                } else {
                    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = max(TermUnitFinalZoneSizing(CurTermUnitSizingNum).NonAirSysDesHeatVolFlow,
                                                                          this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
                }
            } else {
                if (this->SysType_Num == SingleDuctVAVReheatVSFan) {
                    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow =
                        max(this->MaxHeatAirVolFlowRate, this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
                } else if (this->SysType_Num == SingleDuctConstVolReheat || this->SysType_Num == SingleDuctConstVolNoReheat) {
                    TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = this->MaxAirVolFlowRate;
                } else {
                    if (this->DamperHeatingAction == ReverseAction) {
                        TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = this->MaxAirVolFlowRate;
                    } else if (this->DamperHeatingAction == ReverseActionWithLimits) {
                        TermUnitSizing(CurTermUnitSizingNum).AirVolFlow =
                            max(this->MaxAirVolFlowRateDuringReheat, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac));
                    } else {
                        TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                    }
                }
            }

            if (TermUnitSizing(CurTermUnitSizingNum).AirVolFlow > SmallAirVolFlow) {
                if (this->DamperHeatingAction == ReverseActionWithLimits) {
                    TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult =
                        min(this->MaxAirVolFlowRateDuringReheat, this->MaxAirVolFlowRate) /
                        TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                } else if (this->DamperHeatingAction == ReverseAction) {
                    TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult =
                        this->MaxAirVolFlowRate / TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                } else if (this->DamperHeatingAction == Normal && this->MaxAirVolFlowRateDuringReheat > 0.0) {
                    TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult =
                        min(this->MaxAirVolFlowRateDuringReheat, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac)) /
                        TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = 1.0;
                } else if (this->DamperHeatingAction == Normal && this->MaxAirVolFlowRateDuringReheat == 0.0) {
                    TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult =
                        (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac) / TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = 1.0;
                } else {
                    TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult =
                        this->MaxAirVolFlowRate / TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                    TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult;
                }
                TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult = max(1.0, TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult);
                TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = max(1.0, TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult);
            } else {
                TermUnitSizing(CurTermUnitSizingNum).ReheatAirFlowMult = 1.0;
                TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult = 1.0;
            }
            if (this->ReheatComp_Index > 0) {
                coilSelectionReportObj->setCoilReheatMultiplier(
                    this->ReheatName, this->ReheatComp, TermUnitSizing(CurTermUnitSizingNum).ReheatLoadMult);
            }
        }

        IsAutoSize = false;
        if (this->MaxReheatWaterVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurTermUnitSizingNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) {
                if (this->MaxReheatWaterVolFlow > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                       this->MaxReheatWaterVolFlow);
                }
            } else {
                CheckZoneSizing(this->SysType, this->SysName);
                if (UtilityRoutines::SameString(this->ReheatComp, "Coil:Heating:Water")) {
                    CoilWaterInletNode = GetCoilWaterInletNode("Coil:Heating:Water", this->ReheatName, ErrorsFound);
                    CoilWaterOutletNode = GetCoilWaterOutletNode("Coil:Heating:Water", this->ReheatName, ErrorsFound);
                    if (IsAutoSize) {
                        PlantSizingErrorsFound = false;
                        PltSizHeatNum = MyPlantSizingIndex(
                            "Coil:Heating:Water", this->ReheatName, CoilWaterInletNode, CoilWaterOutletNode, PlantSizingErrorsFound);
                        if (PlantSizingErrorsFound) {
                            ShowContinueError("...Occurs in " + this->SysType + ':' + this->SysName);
                            ErrorsFound = true;
                        }
                        if (PltSizHeatNum > 0) {
                            CoilInTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU;
                            DesMassFlow = StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesZoneHeatLoad = TermUnitFinalZoneSizing(CurTermUnitSizingNum).NonAirSysDesHeatLoad;
                            ZoneDesTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                            ZoneDesHumRat = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                            // the coil load is the zone design heating load plus (or minus!) the reheat load
                            DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnW(ZoneDesHumRat) * DesMassFlow * (ZoneDesTemp - CoilInTemp);
                            if (DesCoilLoad >= SmallLoad) {

                                rho = GetDensityGlycol(PlantLoop(this->HWLoopNum).FluidName,
                                                       DataGlobals::HWInitConvTemp,
                                                       PlantLoop(this->HWLoopNum).FluidIndex,
                                                       RoutineName);

                                Cp = GetSpecificHeatGlycol(PlantLoop(this->HWLoopNum).FluidName,
                                                           DataGlobals::HWInitConvTemp,
                                                           PlantLoop(this->HWLoopNum).FluidIndex,
                                                           RoutineName);

                                MaxReheatWaterVolFlowDes = DesCoilLoad / (PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                            } else {
                                MaxReheatWaterVolFlowDes = 0.0;
                            }
                        } else {
                            ShowSevereError("Autosizing of water flow requires a heating loop Sizing:Plant object");
                            ShowContinueError("Occurs in AirTerminal Object=" + this->SysName);
                            ErrorsFound = true;
                        }
                    }
                    if (IsAutoSize) {
                        this->MaxReheatWaterVolFlow = MaxReheatWaterVolFlowDes;
                        ReportSizingOutput(
                            this->SysType, this->SysName, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxReheatWaterVolFlowDes);
                        ReportSizingOutput(this->SysType,
                                           this->SysName,
                                           "Design Size Reheat Coil Sizing Air Volume Flow Rate [m3/s]",
                                           TermUnitSizing(CurTermUnitSizingNum).AirVolFlow);
                        ReportSizingOutput(this->SysType,
                                           this->SysName,
                                           "Design Size Reheat Coil Sizing Inlet Air Temperature [C]",
                                           TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU);
                        ReportSizingOutput(this->SysType,
                                           this->SysName,
                                           "Design Size Reheat Coil Sizing Inlet Air Humidity Ratio [kgWater/kgDryAir]",
                                           TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU);
                    } else { // Hard-size with sizing data
                        if (this->MaxReheatWaterVolFlow > 0.0 && MaxReheatWaterVolFlowDes > 0.0) {
                            MaxReheatWaterVolFlowUser = this->MaxReheatWaterVolFlow;
                            ReportSizingOutput(this->SysType,
                                               this->SysName,
                                               "Design Size Maximum Reheat Water Flow Rate [m3/s]",
                                               MaxReheatWaterVolFlowDes,
                                               "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                               MaxReheatWaterVolFlowUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(MaxReheatWaterVolFlowDes - MaxReheatWaterVolFlowUser) / MaxReheatWaterVolFlowUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                                this->SysName + "\".");
                                    ShowContinueError("User-Specified Maximum Reheat Water Flow Rate of " +
                                                      RoundSigDigits(MaxReheatWaterVolFlowUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Reheat Water Flow Rate of " +
                                                      RoundSigDigits(MaxReheatWaterVolFlowDes, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            this->MaxReheatWaterVolFlow = 0.0;
        }

        IsAutoSize = false;
        if (this->MaxReheatSteamVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurTermUnitSizingNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) {
                if (this->MaxReheatSteamVolFlow > 0.0) {
                    ReportSizingOutput(this->SysType,
                                       this->SysName,
                                       "User-Specified Maximum Reheat Steam Flow Rate [m3/s]",
                                       this->MaxReheatSteamVolFlow);
                }
            } else {
                CheckZoneSizing(this->SysType, this->SysName);
                if (UtilityRoutines::SameString(this->ReheatComp, "Coil:Heating:Steam")) {
                    CoilSteamInletNode = GetCoilSteamInletNode("Coil:Heating:Steam", this->ReheatName, ErrorsFound);
                    CoilSteamOutletNode = GetCoilSteamOutletNode("Coil:Heating:Steam", this->ReheatName, ErrorsFound);
                    if (IsAutoSize) {
                        PlantSizingErrorsFound = false;
                        PltSizHeatNum = MyPlantSizingIndex(
                            "Coil:Heating:Steam", this->ReheatName, CoilSteamInletNode, CoilSteamOutletNode, PlantSizingErrorsFound);
                        if (PlantSizingErrorsFound) {
                            ShowContinueError("...Occurs in " + this->SysType + ':' + this->SysName);
                            ErrorsFound = true;
                        }
                        if (PltSizHeatNum > 0) {
                            CoilInTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU;
                            DesMassFlow = StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesZoneHeatLoad = TermUnitFinalZoneSizing(CurTermUnitSizingNum).NonAirSysDesHeatLoad;
                            ZoneDesTemp = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                            ZoneDesHumRat = TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                            // the coil load is the zone design heating load plus (or minus!) the reheat load
                            DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnW(ZoneDesHumRat) * DesMassFlow * (ZoneDesTemp - CoilInTemp);
                            if (DesCoilLoad >= SmallLoad) {
                                TempSteamIn = 100.00;
                                EnthSteamInDry = GetSatEnthalpyRefrig(fluidNameSteam, TempSteamIn, 1.0, this->FluidIndex, RoutineNameFull);
                                EnthSteamOutWet = GetSatEnthalpyRefrig(fluidNameSteam, TempSteamIn, 0.0, this->FluidIndex, RoutineNameFull);
                                LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                                SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, this->FluidIndex, RoutineNameFull);

                                Cp = GetSpecificHeatGlycol(fluidNameWater, PlantSizData(PltSizHeatNum).ExitTemp, DummyWaterIndex, RoutineName);
                                MaxReheatSteamVolFlowDes = DesCoilLoad / (SteamDensity * (LatentHeatSteam + PlantSizData(PltSizHeatNum).DeltaT * Cp));
                            } else {
                                MaxReheatSteamVolFlowDes = 0.0;
                            }
                        } else {
                            ShowSevereError("Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                            ShowContinueError("Occurs in AirTerminal:SingleDuct:ConstantVolume:Reheat Object=" + this->SysName);
                            ErrorsFound = true;
                        }
                    }
                    if (IsAutoSize) {
                        this->MaxReheatSteamVolFlow = MaxReheatSteamVolFlowDes;
                        ReportSizingOutput(
                            this->SysType, this->SysName, "Design Size Maximum Reheat Steam Flow Rate [m3/s]", MaxReheatSteamVolFlowDes);
                    } else {
                        if (this->MaxReheatSteamVolFlow > 0.0 && MaxReheatSteamVolFlowDes > 0.0) {
                            MaxReheatSteamVolFlowUser = this->MaxReheatSteamVolFlow;
                            ReportSizingOutput(this->SysType,
                                               this->SysName,
                                               "Design Size Maximum Reheat Steam Flow Rate [m3/s]",
                                               MaxReheatSteamVolFlowDes,
                                               "User-Specified Maximum Reheat Steam Flow Rate [m3/s]",
                                               MaxReheatSteamVolFlowUser);
                            if (DisplayExtraWarnings) {
                                if ((std::abs(MaxReheatSteamVolFlowDes - MaxReheatSteamVolFlowUser) / MaxReheatSteamVolFlowUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                                this->SysName + "\".");
                                    ShowContinueError("User-Specified Maximum Reheat Steam Flow Rate of " +
                                                      RoundSigDigits(MaxReheatSteamVolFlowUser, 5) + " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Reheat Steam Flow Rate of " +
                                                      RoundSigDigits(MaxReheatSteamVolFlowDes, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            this->MaxReheatSteamVolFlow = 0.0;
        }

        if (CurTermUnitSizingNum > 0) {
            TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
            TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow = this->MaxReheatWaterVolFlow;
            TermUnitSizing(CurTermUnitSizingNum).MaxSTVolFlow = this->MaxReheatSteamVolFlow;
            TermUnitSizing(CurTermUnitSizingNum).DesHeatingLoad = DesCoilLoad; // Coil Summary report
            if (this->ReheatComp_Num == HCoilType_SimpleHeating) {
                if (this->DamperHeatingAction == Normal) {
                    SetCoilDesFlow(
                        this->ReheatComp, this->ReheatName, this->ZoneMinAirFracDes * this->MaxAirVolFlowRate, ErrorsFound);
                } else {
                    SetCoilDesFlow(this->ReheatComp, this->ReheatName, TermUnitSizing(CurTermUnitSizingNum).AirVolFlow, ErrorsFound);
                }
            }
        }

        if (this->MaxAirVolFlowRateDuringReheat > 0.0) {
            // check for inconsistent dual max input
            if (this->MaxAirVolFlowRateDuringReheat < (this->ZoneMinAirFracDes * this->MaxAirVolFlowRate)) {
                // Only warn when really out of bounds
                if ((this->ZoneMinAirFracDes * this->MaxAirVolFlowRate) - this->MaxAirVolFlowRateDuringReheat > 1.e-8) {
                    ShowWarningError("SingleDuctSystem:SizeSys: Air Terminal Unit flow limits are not consistent, minimum flow limit is larger than "
                                     "reheat maximum");
                    ShowContinueError("Air Terminal Unit name = " + this->SysName);
                    ShowContinueError("Maximum terminal flow during reheat = " + RoundSigDigits(this->MaxAirVolFlowRateDuringReheat, 6) +
                                      " [m3/s] or flow fraction = " +
                                      RoundSigDigits((this->MaxAirVolFlowRateDuringReheat / this->MaxAirVolFlowRate), 4));
                    ShowContinueError("Minimum terminal flow = " + RoundSigDigits((this->ZoneMinAirFracDes * this->MaxAirVolFlowRate), 6) +
                                      " [m3/s] or flow fraction = " + RoundSigDigits(this->ZoneMinAirFracDes, 4));
                    ShowContinueError("The reheat maximum flow limit will be replaced by the minimum limit, and the simulation continues");
                }
                this->MaxAirVolFlowRateDuringReheat = (this->ZoneMinAirFracDes * this->MaxAirVolFlowRate);
            }
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    // End Initialization Section of the Module
    //******************************************************************************

    // Begin Algorithm Section of the Module
    //******************************************************************************

    void SingleDuctAirTerminal::SimVAV(EnergyPlusData &state, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   January 2000
        //       MODIFIED       Fred Buhl: added reverse action damper heating action: August 2001
        //                      KHL/TH 7/2010: revise to support dual max
        //                      FB/KHL/TH 9/2010: added maximum supply air temperature leaving reheat coil
        //                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
        //                                 air fraction - a TRACE feature
        //                      Brent Griffith, 5/2012, general cleanup, fix negatives CR 8767, fix phantom coil flows CR 8854
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple single duct volume VAV.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused   USE DataHeatBalFanSys, ONLY: Mat
        using DataDefineEquip::AirDistUnit;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;
        using DataHVACGlobals::SmallLoad;
        using PlantUtilities::SetActuatedBranchFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;     // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 QTotLoad;     // [Watts] Remaining load required for this zone
        Real64 QZnReq;       // [Watts] Load calculated for heating coil
        Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
        Real64 CpAirAvg;
        Real64 DeltaTemp;
        int SysOutletNode;                       // The node number of the terminal unit outlet node
        int SysInletNode;                        // the node number of the terminal unit inlet node
        int WaterControlNode;                    // This is the Actuated Reheat Control Node
        Real64 MaxFlowWater;                     // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MinFlowWater;                     // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 QActualHeating;                   // the heating load seen by the reheat coil
        Real64 QHeatingDelivered;                // the actual output from heating coil
        Real64 LeakLoadMult;                     // load multiplier to adjust for downstream leaks
        Real64 MinFlowFrac;                      // minimum flow fraction (and minimum damper position)
        static Real64 MinAirMassFlowRevAct(0.0); // minimum air mass flow rate used in "reverse action" air mass flow rate calculation
        static Real64 MaxAirMassFlowRevAct(0.0); // maximum air mass flow rate used in "reverse action" air mass flow rate calculation
        Real64 MassFlowBasedOnOA;                // supply air mass flow rate based on zone OA requirements
        Real64 AirLoopOAFrac;                    // fraction of outside air entering air loop
        Real64 DummyMdot;                        // temporary mass flow rate argument

        static Real64 ZoneTemp(0.0);                      // zone air temperature [C]
        static Real64 MaxHeatTemp(0.0);                   // maximum supply air temperature [C]
        static Real64 MaxDeviceAirMassFlowReheat(0.0);    // air mass flow rate required to meet the coil heating load [W]
        static Real64 MassFlowReqToLimitLeavingTemp(0.0); // air mass flow rate actually used [W]
        static Real64 QZoneMaxRHTempLimit(0.0);           // maximum zone heat addition rate given constraints of MaxHeatTemp and max
        // available air mass flow rate [W]
        static Real64 MinMassAirFlow(0.0); // the air flow rate during heating for normal acting damper
        static Real64 QZoneMax2(0.0);      // temporary variable

        // Note to the perplexed
        // The SINGLE DUCT:VAV:REHEAT terminal unit originally contained 2 components: a damper
        // and a reheat coil. The damper has become a virtual component - it consists only of
        // an air inlet node and an air outlet node. The damper is upstream of the heating coil.
        // sd_airterminal(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
        // sd_airterminal(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
        // sd_airterminal(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

        // The calculated load from the Heat Balance
        LeakLoadMult = AirDistUnit(this->ADUNum).LeakLoadMult;
        QTotLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired * LeakLoadMult;
        QToHeatSetPt = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP * LeakLoadMult;
        SysOutletNode = this->ReheatAirOutletNode;
        SysInletNode = this->InletNodeNum;
        CpAirAvg = PsyCpAirFnW(0.5 * (Node(ZoneNodeNum).HumRat + this->sd_airterminalInlet.AirHumRat));
        MinFlowFrac = this->ZoneMinAirFrac;
        MassFlowBasedOnOA = 0.0;
        ZoneTemp = Node(ZoneNodeNum).Temp;
        MinMassAirFlow = MinFlowFrac * StdRhoAir * this->MaxAirVolFlowRate;

        // Then depending on if the Load is for heating or cooling it is handled differently.  First
        // the massflow rate for cooling is determined to meet the entire load.  Then
        // if the massflow is below the minimum or greater than the Max it is set to either the Min
        // or the Max as specified for the VAV model.
        if ((QTotLoad < 0.0) && (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) && (TempControlType(ZoneNum) != SingleHeatingSetPoint) &&
            (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
            // Calculate the flow required for cooling

            DeltaTemp = CpAirAvg * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

            // Need to check DeltaTemp and ensure that it is not zero
            if (DeltaTemp != 0.0) {
                MassFlow = QTotLoad / DeltaTemp;
            } else {
                MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
            }

            // Apply the zone maximum outdoor air fraction FOR VAV boxes - a TRACE feature
            if (ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            // calculate supply air flow rate based on user specified OA requirement
            this->CalcOAMassFlow(MassFlowBasedOnOA, AirLoopOAFrac);
            MassFlow = max(MassFlow, MassFlowBasedOnOA);

            // used for normal acting damper
            MinMassAirFlow = max(MinMassAirFlow, MassFlowBasedOnOA);
            MinMassAirFlow = max(MinMassAirFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);
            MinMassAirFlow = min(MinMassAirFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

            // limit the OA based supply air flow rate based on optional user input
            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            MassFlow = max(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);
            MassFlow = min(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

            if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                AirflowNetwork::AirflowNetworkFanActivated && AirflowNetwork::VAVTerminalRatio > 0.0) {
                MassFlow *= AirflowNetwork::VAVTerminalRatio;
                if (MassFlow > Node(this->InletNodeNum).MassFlowRate) {
                    MassFlow = Node(this->InletNodeNum).MassFlowRate;
                }
            }

        } else if ((this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) && (QTotLoad >= 0.0 || TempControlType(ZoneNum) == SingleHeatingSetPoint) &&
                   (GetCurrentScheduleValue(this->SchedPtr) > 0.0)) {
            //     IF (sd_airterminal(SysNum)%DamperHeatingAction .EQ. ReverseAction .AND. this->sd_airterminalInlet%AirMassFlowRateMinAvail <= SmallMassFlow) THEN
            // special case for heating: reverse action and damper allowed to close - set the minimum flow rate to a small but nonzero value
            //       MassFlow = 0.01d0*this->sd_airterminalInlet%AirMassFlowRateMaxAvail
            //     ELSE
            // usual case for heating: set the air mass flow rate to the minimum
            MassFlow = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
            //     END IF

            // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
            if (ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
                MassFlow *= ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
            }

            // calculate supply air flow rate based on user specified OA requirement
            this->CalcOAMassFlow(MassFlowBasedOnOA, AirLoopOAFrac);
            MassFlow = max(MassFlow, MassFlowBasedOnOA);

            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            if (MassFlow <= this->sd_airterminalInlet.AirMassFlowRateMinAvail) {
                MassFlow = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
            } else if (MassFlow >= this->sd_airterminalInlet.AirMassFlowRateMaxAvail) {
                MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
            }

            // the AirflowNetwork model overrids the mass flow rate value
            if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                AirflowNetwork::AirflowNetworkFanActivated && AirflowNetwork::VAVTerminalRatio > 0.0) {
                MassFlow *= AirflowNetwork::VAVTerminalRatio;
                if (MassFlow > Node(this->InletNodeNum).MassFlowRate) {
                    MassFlow = Node(this->InletNodeNum).MassFlowRate;
                }
            }

        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
            AirLoopOAFrac = 0.0;
        }

        // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
        // equipment iteration. If detected, set flow rate to previous value.
        if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) || (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
            (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
            if (MassFlow > 0.0) MassFlow = this->MassFlow1;
        }

        // Move data to the damper outlet node
        this->sd_airterminalOutlet.AirTemp = this->sd_airterminalInlet.AirTemp;
        this->sd_airterminalOutlet.AirHumRat = this->sd_airterminalInlet.AirHumRat;
        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->sd_airterminalOutlet.AirMassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        this->sd_airterminalOutlet.AirMassFlowRateMinAvail = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
        this->sd_airterminalOutlet.AirEnthalpy = this->sd_airterminalInlet.AirEnthalpy;

        //   ! Calculate the Damper Position when there is a Max air flow specified.
        //  If (MassFlow == 0.0D0) THEN
        //    sd_airterminal(SysNum)%DamperPosition = 0.0D0
        //  ELSE IF (this->sd_airterminalInlet%AirMassFlowRateMaxAvail > this->sd_airterminalInlet%AirMassFlowRateMinAvail) THEN
        //    sd_airterminal(SysNum)%DamperPosition = ((MassFlow-this->sd_airterminalInlet%AirMassFlowRateMinAvail) / &
        //                                   (this->sd_airterminalInlet%AirMassFlowRateMaxAvail-this->sd_airterminalInlet%AirMassFlowRateMinAvail)) * &
        //                                  (1.0d0-MinFlowFrac) + MinFlowFrac
        //  ELSE
        //    sd_airterminal(SysNum)%DamperPosition = 1.0D0
        //  END IF

        if (MassFlow == 0.0) {
            this->DamperPosition = 0.0;
            this->ZoneMinAirFracReport = 0.0;
        } else if ((MassFlow > 0.0) && (MassFlow < this->AirMassFlowRateMax)) {
            this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
            this->ZoneMinAirFracReport = this->ZoneMinAirFrac;
        } else if (MassFlow == this->AirMassFlowRateMax) {
            this->DamperPosition = 1.0;
            this->ZoneMinAirFracReport = this->ZoneMinAirFrac;
        }

        // Need to make sure that the damper outlets are passed to the coil inlet
        this->UpdateSys();

        // At the current air mass flow rate, calculate heating coil load
        QActualHeating = QToHeatSetPt - MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - ZoneTemp); // reheat needed

        // do the reheat calculation if there's some air nass flow (or the damper action is "reverse action"), the flow is <= minimum ,
        // there's a heating requirement, and there's a thermostat with a heating setpoint
        // Reverse damper option is working only for water coils for now.
        if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (TempControlType(ZoneNum) != SingleCoolingSetPoint)) {
            // At this point we know that there is a heating requirement: i.e., the heating coil needs to
            // be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
            // situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
            // the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
            // temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
            // max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
            // and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
            // max we are in condition 2.

            QZoneMax2 = QToHeatSetPt;

            // fill dual-max reheat flow limit, if any
            if (this->DamperHeatingAction == ReverseAction) {
                MaxDeviceAirMassFlowReheat = this->AirMassFlowRateMax;
            } else if (this->DamperHeatingAction == ReverseActionWithLimits) {
                MaxDeviceAirMassFlowReheat = this->AirMassFlowDuringReheatMax;
            } else if (this->DamperHeatingAction == Normal) {
                MaxDeviceAirMassFlowReheat = this->ZoneMinAirFrac * this->AirMassFlowRateMax;
            } else {
                // used for AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT or SingleDuctVAVNoReheat
                MaxDeviceAirMassFlowReheat = this->AirMassFlowRateMax;
            }

            // determine flow based on leaving reheat temperature limit
            if (this->MaxReheatTempSetByUser) {

                MaxHeatTemp = this->MaxReheatTemp;
                if (QToHeatSetPt > SmallLoad) { // zone has a postive load to heating setpoint
                    MassFlowReqToLimitLeavingTemp = QToHeatSetPt / (CpAirAvg * (MaxHeatTemp - ZoneTemp));
                } else {
                    MassFlowReqToLimitLeavingTemp = 0.0;
                }
            }

            // (re)apply limits to find air mass flow
            MassFlow = max(MassFlow, MassFlowReqToLimitLeavingTemp);
            MassFlow = min(MassFlow, MaxDeviceAirMassFlowReheat);
            MassFlow = max(MassFlow, MassFlowBasedOnOA);
            MassFlow = min(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
            MassFlow = max(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);

            if (AirflowNetwork::SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                AirflowNetwork::AirflowNetworkFanActivated && AirflowNetwork::VAVTerminalRatio > 0.0) {
                MassFlow *= AirflowNetwork::VAVTerminalRatio;
                if (MassFlow > Node(this->InletNodeNum).MassFlowRate) {
                    MassFlow = Node(this->InletNodeNum).MassFlowRate;
                }
            }

            // now make any corrections to heating coil loads
            if (this->MaxReheatTempSetByUser) {
                QZoneMaxRHTempLimit = MassFlow * CpAirAvg * (MaxHeatTemp - ZoneTemp);
                QZoneMax2 = min(QZoneMaxRHTempLimit, QToHeatSetPt);
            }

            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;

            this->UpdateSys();

            // Now do the heating coil calculation for each heating coil type
            {
                auto const SELECT_CASE_var(this->ReheatComp_Num); // Reverse damper option is working only for water coils for now.

                // hot water heating coil
                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Determine the load required to pass to the Component controller
                    // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                    // and is working as-is, temperature setpoints are maintained as expected.
                    QZnReq = QZoneMax2 + MassFlow * CpAirAvg * ZoneTemp;

                    // Initialize hot water flow rate to zero.
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if (FirstHVACIteration) {
                        MaxFlowWater = this->MaxReheatWaterFlow;
                        MinFlowWater = this->MinReheatWaterFlow;
                    } else {
                        WaterControlNode = this->ReheatControlNode;
                        MaxFlowWater = Node(WaterControlNode).MassFlowRateMaxAvail;
                        MinFlowWater = Node(WaterControlNode).MassFlowRateMinAvail;
                    }

                    // Simulate the reheat coil at constant air flow. Control by varying the
                    // hot water flow rate.
                    // FB use QActualHeating, change ControlCompOutput to use new
                    ControlCompOutput(state, this->ReheatName,
                                      this->ReheatComp,
                                      this->ReheatComp_Index,
                                      FirstHVACIteration,
                                      QZnReq,
                                      this->ReheatControlNode,
                                      MaxFlowWater,
                                      MinFlowWater,
                                      this->ControllerOffset,
                                      this->ControlCompTypeNum,
                                      this->CompErrIndex,
                                      _,
                                      SysOutletNode,
                                      MassFlow,
                                      _,
                                      _,
                                      this->HWLoopNum,
                                      this->HWLoopSide,
                                      this->HWBranchIndex);

                    // If reverse action damper and the hot water flow is at maximum, simulate the
                    // hot water coil with fixed (maximum) hot water flow but allow the air flow to
                    // vary up to the maximum (air damper opens to try to meet zone load)
                    if (this->DamperHeatingAction == ReverseAction || this->DamperHeatingAction == ReverseActionWithLimits) {
                        if (Node(this->ReheatControlNode).MassFlowRate == MaxFlowWater) {
                            // fill limits for air flow for controller
                            MinAirMassFlowRevAct = this->AirMassFlowRateMax * this->ZoneMinAirFrac;
                            MinAirMassFlowRevAct = min(MinAirMassFlowRevAct, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
                            MinAirMassFlowRevAct = max(MinAirMassFlowRevAct, this->sd_airterminalInlet.AirMassFlowRateMinAvail);

                            MaxAirMassFlowRevAct = this->AirMassFlowRateMax;
                            MaxAirMassFlowRevAct = min(MaxAirMassFlowRevAct, MaxDeviceAirMassFlowReheat);
                            MaxAirMassFlowRevAct = max(MaxAirMassFlowRevAct, MinAirMassFlowRevAct);
                            MaxAirMassFlowRevAct = min(MaxAirMassFlowRevAct, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

                            Node(this->OutletNodeNum).MassFlowRateMaxAvail =
                                MaxAirMassFlowRevAct; // suspect, check how/if used in ControlCompOutput
                            ControlCompOutput(state, this->ReheatName,
                                              this->ReheatComp,
                                              this->ReheatComp_Index,
                                              FirstHVACIteration,
                                              QZoneMax2,
                                              this->OutletNodeNum,
                                              MaxAirMassFlowRevAct,
                                              MinAirMassFlowRevAct,
                                              this->ControllerOffset,
                                              this->ControlCompTypeNum,
                                              this->CompErrIndex,
                                              ZoneNodeNum,
                                              SysOutletNode); // why not QZnReq  ?
                            // air flow controller, not on plant, don't pass plant topology info
                            // reset terminal unit inlet air mass flow to new value.
                            Node(this->OutletNodeNum).MassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
                            MassFlow = Node(SysOutletNode).MassFlowRate;

                            //         ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
                            //         ! equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
                            if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) ||
                                 (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
                                (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
                                if (MassFlow > 0.0) MassFlow = this->MassFlow1;
                                this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                                this->UpdateSys();

                                // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                                // and is working as-is, temperature setpoints are maintained as expected.
                                QZnReq = QZoneMax2 + MassFlow * CpAirAvg * ZoneTemp;
                                ControlCompOutput(state, this->ReheatName,
                                                  this->ReheatComp,
                                                  this->ReheatComp_Index,
                                                  FirstHVACIteration,
                                                  QZnReq,
                                                  this->ReheatControlNode,
                                                  MaxFlowWater,
                                                  MinFlowWater,
                                                  this->ControllerOffset,
                                                  this->ControlCompTypeNum,
                                                  this->CompErrIndex,
                                                  _,
                                                  SysOutletNode,
                                                  MassFlow,
                                                  _,
                                                  _,
                                                  this->HWLoopNum,
                                                  this->HWLoopSide,
                                                  this->HWBranchIndex);
                            }

                            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                            // reset OA report variable
                            this->UpdateSys();
                        } // IF (Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate .EQ. MaxFlowWater) THEN
                    }     // IF (sd_airterminal(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN

                    // Recalculate the Damper Position.
                    if (MassFlow == 0.0) {
                        this->DamperPosition = 0.0;
                        this->ZoneMinAirFracReport = 0.0;
                    } else if ((MassFlow > 0.0) && (MassFlow < this->AirMassFlowRateMax)) {
                        this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
                        this->ZoneMinAirFracReport = this->ZoneMinAirFrac;
                    } else if (MassFlow == this->AirMassFlowRateMax) {
                        this->DamperPosition = 1.0;
                        this->ZoneMinAirFracReport = this->ZoneMinAirFrac;
                    }

                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // ! COIL:STEAM:AIRHEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QZoneMax2 - MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);

                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QZoneMax2 - MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QZoneMax2 - MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state,
                        this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index, QHeatingDelivered);

                } else if (SELECT_CASE_var == HCoilType_None) { // blank
                                                                // I no reheat is defined then assume that the damper is the only component.
                    // If something else is there that is not a reheat coil or a blank then give the error message

                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }

            // the COIL is OFF the properties are calculated for this special case.
        } else {
            {
                auto const SELECT_CASE_var(this->ReheatComp_Num);

                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Simulate reheat coil for the Const Volume system
                    // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0  !DSU
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                    // call the reheat coil with the NO FLOW condition to make sure that the Node values
                    // are passed through to the coil outlet correctly
                    SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // COIL:STEAM:AIRHEATING
                    // Simulate reheat coil for the VAV system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_None) { // blank
                                                                // If no reheat is defined then assume that the damper is the only component.
                    // If something else is that is not a reheat coil or a blank then give the error message

                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }
        }

        // push the flow rate history
        this->MassFlow3 = this->MassFlow2;
        this->MassFlow2 = this->MassFlow1;
        this->MassFlow1 = MassFlow;
    }

    void SingleDuctAirTerminal::CalcOAMassFlow(Real64 &SAMassFlow,   // outside air based on optional user input
                        Real64 &AirLoopOAFrac // outside air based on optional user input
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         R. Raustad (FSEC)
        //       DATE WRITTEN   Jan 2010
        //       MODIFIED       Mangesh Basarkar, 06/2011: Modifying outside air based on airloop DCV flag
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Calculates the amount of outside air required based on optional user input.
        // Zone multipliers are included and are applied in GetInput.

        // METHODOLOGY EMPLOYED:
        // User input defines method used to calculate OA.

        // REFERENCES:

        // Using/Aliasing
        using DataAirLoop::AirLoopControlInfo;
        using DataAirLoop::AirLoopFlow;
        using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
        using DataZoneEquipment::ZoneEquipConfig;
        using Psychrometrics::PsyRhoAirFnPbTdbW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
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
            if (this->NoOAFlowInputFromUser) return;
            // Calculate outdoor air flow rate, zone multipliers are applied in GetInput
            if (AirLoopOAFrac > 0.0) {
                OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(
                    this->OARequirementsPtr, this->ActualZoneNum, AirLoopControlInfo(AirLoopNum).AirLoopDCVFlag, UseMinOASchFlag);
                OAMassFlow = OAVolumeFlowRate * StdRhoAir;

                // convert OA mass flow rate to supply air flow rate based on air loop OA fraction
                SAMassFlow = OAMassFlow / AirLoopOAFrac;
            }
        }
    }

    void SingleDuctAirTerminal::SimCBVAV(EnergyPlusData &state, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard Raustad
        //       DATE WRITTEN   August 2006
        //       MODIFIED       KHL/TH 10/2010: added maximum supply air temperature leaving reheat coil
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the VAV box with varying airflow in heating and cooling.
        // Modified version of SimVAV.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataHVACGlobals::SmallLoad;
        // unused   USE DataHeatBalFanSys,    ONLY: Mat
        using DataDefineEquip::AirDistUnit;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;
        // unused   USE DataHeatBalFanSys,    ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
        using PlantUtilities::SetActuatedBranchFlowRate;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;      // Total Mass Flow Rate from Hot & Cold Inlets [kg/sec]
        Real64 QTotLoad;      // Total load based on thermostat setpoint temperature [Watts]
        Real64 QZnReq;        // Total load to be met by terminal heater [Watts]
        Real64 QToHeatSetPt;  // Remaining load to heating setpoint [W]
        Real64 QSupplyAir;    // Zone load met by VAVHeatandCool system
        Real64 CpAirZn;       // Specific heat of zone air [J/kg-C]
        Real64 CpAirSysIn;    // Specific heat of VAVHeatandCool box entering air [J/kg-C]
        Real64 DeltaTemp;     // Temperature difference multiplied by specific heat [J/kg]
        Real64 MaxFlowWater;  // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MinFlowWater;  // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 LeakLoadMult;  // Load multiplier to adjust for downstream leaks
        int SysOutletNode;    // The node number of the terminal unit outlet node
        int SysInletNode;     // The node number of the terminal unit inlet node
        int WaterControlNode; // This is the Actuated Reheat Control Node
        Real64 DummyMdot;
        Real64 QActualHeating;
        Real64 MinFlowFrac;                // minimum flow fraction (and minimum damper position)
        static Real64 ZoneTemp(0.0);       // zone air temperature [C]
        static Real64 MaxHeatTemp(0.0);    // maximum supply air temperature [C]
        static Real64 MassFlowReq(0.0);    // air mass flow rate required to meet the coil heating load [W]
        static Real64 MassFlowActual(0.0); // air mass flow rate actually used [W]
        static Real64 QZoneMax(0.0);       // maximum zone heat addition rate given constraints of MaxHeatTemp and max
        // available air mass flow rate [W]
        static Real64 MinMassAirFlow(0.0); // the air flow rate during heating for normal acting damper
        static Real64 QZoneMax2(0.0);      // temporary variable
        static Real64 QZoneMax3(0.0);      // temporary variable

        // sd_airterminal(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
        // sd_airterminal(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
        // sd_airterminal(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

        // The calculated load from the Heat Balance
        LeakLoadMult = AirDistUnit(this->ADUNum).LeakLoadMult;
        QTotLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired * LeakLoadMult;
        QToHeatSetPt = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP * LeakLoadMult;
        SysOutletNode = this->ReheatAirOutletNode;
        SysInletNode = this->InletNodeNum;
        CpAirZn = PsyCpAirFnW(Node(ZoneNodeNum).HumRat);
        MinFlowFrac = this->ZoneMinAirFrac;
        MinMassAirFlow = MinFlowFrac * StdRhoAir * this->MaxAirVolFlowRate;
        ZoneTemp = Node(ZoneNodeNum).Temp;

        // Then depending on if the Load is for heating or cooling it is handled differently.  First
        // the massflow rate for cooling is determined to meet the entire load.  Then
        // if the massflow is below the minimum or greater than the Max it is set to either the Min
        // or the Max as specified for the VAV model.
        if (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) {
            // Calculate the flow required for cooling
            CpAirSysIn = PsyCpAirFnW(this->sd_airterminalInlet.AirHumRat);
            DeltaTemp = CpAirSysIn * this->sd_airterminalInlet.AirTemp - CpAirZn * ZoneTemp;

            // Need to check DeltaTemp and ensure that it is not zero
            if (DeltaTemp != 0.0) {
                MassFlow = QTotLoad / DeltaTemp;
            } else {
                MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
            }

            // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
            MassFlow = max(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);
            MassFlow = min(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
        } else {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }
        // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
        // equipment iteration. If detected, set flow rate to previous value.
        if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) || (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
            (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
            MassFlow = this->MassFlow1;
        }

        // Move data to the damper outlet node
        this->sd_airterminalOutlet.AirTemp = this->sd_airterminalInlet.AirTemp;
        this->sd_airterminalOutlet.AirHumRat = this->sd_airterminalInlet.AirHumRat;
        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->sd_airterminalOutlet.AirMassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        this->sd_airterminalOutlet.AirMassFlowRateMinAvail = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
        this->sd_airterminalOutlet.AirEnthalpy = this->sd_airterminalInlet.AirEnthalpy;

        // Calculate the Damper Position when there is a Max air flow specified.
        if (this->AirMassFlowRateMax == 0.0) {
            this->DamperPosition = 0.0;
        } else {
            this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
        }

        // Need to make sure that the damper outlets are passed to the coil inlet
        this->UpdateSys();

        QActualHeating = QToHeatSetPt - MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

        if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (TempControlType(ZoneNum) != SingleCoolingSetPoint)) {
            //   VAVHeatandCool boxes operate at varying mass flow rates when reheating, VAV boxes operate at min flow
            //      (MassFlow <= this->sd_airterminalInlet%AirMassFlowRateMinAvail) .AND. &
            //   Per Fred Buhl, don't use DeadBandOrSetback to determine if heaters operate
            //      (.NOT. DeadBandOrSetback(ZoneNum))) Then

            // At this point we know that there is a heating requirement: i.e., the heating coil needs to
            // be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
            // situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
            // the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
            // temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
            // max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
            // and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
            // max we are in condition 2.

            QZoneMax2 = QToHeatSetPt;

            if (this->MaxReheatTempSetByUser) {

                MaxHeatTemp = this->MaxReheatTemp;
                if (QToHeatSetPt > SmallLoad) { // zone has a postive load to heating setpoint
                    MassFlowReq = QToHeatSetPt / (CpAirZn * (MaxHeatTemp - ZoneTemp));
                } else {
                    MassFlowReq = MassFlow;
                }

                QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlow;

                MassFlowActual = MassFlow;

                if (QZoneMax3 < QToHeatSetPt) {
                    MassFlowActual = MassFlowReq;
                    // QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlowActual
                }

                if (MassFlowActual <= MinMassAirFlow) {
                    MassFlowActual = MinMassAirFlow;
                } else if (MassFlowActual >= this->AirMassFlowRateMax) {
                    MassFlowActual = this->AirMassFlowRateMax;
                }

                QZoneMax = CpAirZn * MassFlowActual * (MaxHeatTemp - ZoneTemp);

                // temporary variable
                QZoneMax2 = min(QZoneMax, QToHeatSetPt);

                MassFlow = MassFlowActual;

            } // IF (sd_airterminal(SysNum)%MaxReheatTempSetByUser) THEN

            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;

            this->UpdateSys();

            {
                auto const SELECT_CASE_var(this->ReheatComp_Num);

                // hot water heating coil
                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Determine the load required to pass to the Component controller
                    // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                    // and is working as-is, temperature setpoints are maintained as expected.
                    QZnReq = QZoneMax2 + MassFlow * CpAirZn * Node(ZoneNodeNum).Temp;
                    if (QZnReq < SmallLoad) QZnReq = 0.0;

                    // Initialize hot water flow rate to zero.
                    // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if (FirstHVACIteration) {
                        MaxFlowWater = this->MaxReheatWaterFlow;
                        MinFlowWater = this->MinReheatWaterFlow;
                    } else {
                        WaterControlNode = this->ReheatControlNode;
                        MaxFlowWater = Node(WaterControlNode).MassFlowRateMaxAvail;
                        MinFlowWater = Node(WaterControlNode).MassFlowRateMinAvail;
                    }

                    // Simulate the reheat coil at constant air flow. Control by varying the
                    // hot water flow rate.
                    ControlCompOutput(state, this->ReheatName,
                                      this->ReheatComp,
                                      this->ReheatComp_Index,
                                      FirstHVACIteration,
                                      QZnReq,
                                      this->ReheatControlNode,
                                      MaxFlowWater,
                                      MinFlowWater,
                                      this->ControllerOffset,
                                      this->ControlCompTypeNum,
                                      this->CompErrIndex,
                                      _,
                                      SysOutletNode,
                                      MassFlow,
                                      _,
                                      _,
                                      this->HWLoopNum,
                                      this->HWLoopSide,
                                      this->HWBranchIndex);

                    // If reverse action damper and the hot water flow is at maximum, simulate the
                    // hot water coil with fixed (maximum) hot water flow but allow the air flow to
                    // vary up to the maximum (air damper opens to try to meet zone load).
                    if (this->DamperHeatingAction == ReverseAction) {
                        if (Node(this->ReheatControlNode).MassFlowRate == this->MaxReheatWaterFlow) {
                            ControlCompOutput(state, this->ReheatName,
                                              this->ReheatComp,
                                              this->ReheatComp_Index,
                                              FirstHVACIteration,
                                              QZoneMax2,
                                              this->OutletNodeNum,
                                              this->sd_airterminalInlet.AirMassFlowRateMaxAvail,
                                              this->sd_airterminalInlet.AirMassFlowRateMinAvail,
                                              this->ControllerOffset,
                                              this->ControlCompTypeNum,
                                              this->CompErrIndex,
                                              ZoneNodeNum,
                                              SysOutletNode);
                            //                                   ! air flow controller, not on plant, don't pass plant topology info

                            // reset terminal unit inlet air mass flow to new value.
                            MassFlow = Node(SysOutletNode).MassFlowRate;
                            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                            this->UpdateSys();
                        }
                        // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
                        // equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
                        if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) ||
                             (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
                            (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
                            MassFlow = this->MassFlow1;
                            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                            this->UpdateSys();
                            ControlCompOutput(state, this->ReheatName,
                                              this->ReheatComp,
                                              this->ReheatComp_Index,
                                              FirstHVACIteration,
                                              QZnReq,
                                              this->ReheatControlNode,
                                              MaxFlowWater,
                                              MinFlowWater,
                                              this->ControllerOffset,
                                              this->ControlCompTypeNum,
                                              this->CompErrIndex,
                                              _,
                                              SysOutletNode,
                                              MassFlow,
                                              _,
                                              _,
                                              this->HWLoopNum,
                                              this->HWLoopSide,
                                              this->HWBranchIndex);
                        }
                        // recalculate damper position
                        if (this->AirMassFlowRateMax == 0.0) {
                            this->DamperPosition = 0.0;
                        } else {
                            this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
                        }
                    }
                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // ! COIL:STEAM:AIRHEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QZoneMax2 - MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - ZoneTemp);
                    if (QZnReq < SmallLoad) QZnReq = 0.0;

                    // Simulate reheat coil for the VAV system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);

                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Determine the load required to pass to the Component controller
                    QSupplyAir = MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - ZoneTemp);
                    QZnReq = QZoneMax2 - QSupplyAir;
                    if (QZnReq < SmallLoad) QZnReq = 0.0;

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QZoneMax2 - MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - ZoneTemp);
                    if (QZnReq < SmallLoad) QZnReq = 0.0;

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_None) { // blank
                                                                // If no reheat is defined then assume that the damper is the only component.
                    // If something else is there that is not a reheat coil then give the error message below.

                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }

            // the COIL is OFF the properties are calculated for this special case.
        } else {
            {
                auto const SELECT_CASE_var(this->ReheatComp_Num);

                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Simulate reheat coil for the Const Volume system
                    // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                    // Initialize hot water flow rate to zero.
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                    // call the reheat coil with the NO FLOW condition to make sure that the Node values
                    // are passed through to the coil outlet correctly
                    SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // COIL:STEAM:AIRHEATING
                    // Simulate reheat coil for the VAV system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_None) { // blank
                                                                // If no reheat is defined then assume that the damper is the only component.
                                                                // If something else is there that is not a reheat coil then give the error message

                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }
        }
        // push the flow rate history
        this->MassFlow3 = this->MassFlow2;
        this->MassFlow2 = this->MassFlow1;
        this->MassFlow1 = MassFlow;
    }

    void SingleDuctAirTerminal::SimVAVVS(EnergyPlusData &state, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates a single duct VAV terminal unit with a variable-speed fan upstream
        // and a reheat coil on the downstream side.

        // METHODOLOGY EMPLOYED:
        // Define a compound component in CalcVAVVS. Break the heating/cooling load into 4 regions based
        // on equip on/off combinations. Assign the heating load to the appropriate region and iteratively
        // solve for the appropriate control variable value using Regula-Falsi solver.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataConvergParams::HVACFlowRateToler;
        using General::SolveRoot;
        using TempSolveRoot::SolveRoot;
        using SteamCoils::GetCoilCapacity;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        Real64 const BigLoad(1.0e+20);

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow = 0; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 QTotLoad; // [Watts]
        // unused  REAL(r64) :: QZnReq      ! [Watts]
        Real64 CpAirZn;
        // unused  REAL(r64) :: CpAirSysIn
        // unused  REAL(r64) :: DeltaTemp
        int SysOutletNode;    // The node number of the terminal unit outlet node
        int SysInletNode;     // the node number of the terminal unit inlet node
        int WaterControlNode; // This is the Actuated Reheat Control Node
        int SteamControlNode;
        Real64 MaxFlowWater;    // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MinFlowWater;    // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MaxFlowSteam;    // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MinFlowSteam;    // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 HWFlow;          // the hot water flow rate [kg/s]
        Real64 QCoolFanOnMax;   // max cooling - fan at max flow; note that cooling is always < 0. [W]
        Real64 QCoolFanOnMin;   // min active cooling with fan on - fan at lowest speed. [W]
        Real64 QHeatFanOnMax;   // max heating - fan at heat flow max, hot water flow at max [W]
        Real64 QHeatFanOnMin;   // min heating - fan at min flow, hot water at max flow [W]
        Real64 QHeatFanOffMax;  // max heating - fan off, hot water flow at max [W]
        Real64 QNoHeatFanOff;   // min heating - fan off, hot water at min flow [W]
        int HCType;             // heating coil type (as a number)
        int FanType;            // fan type (as a number)
        Real64 HCLoad;          // load passed to a gas or electric heating coil [W]
        int FanOp;              // 1 if fan is on; 0 if off.
        Real64 MaxCoolMassFlow; // air flow at max cooling [kg/s]
        Real64 MaxHeatMassFlow; // air flow at max heating [kg/s]
        Real64 MinMassFlow;     // minimum air flow rate [kg/s]
        Real64 UnitFlowToler;   // flow rate tolerance
        Real64 QDelivered;
        Real64 FracDelivered;
        Array1D<Real64> Par(11);
        int SolFlag;
        Real64 ErrTolerance;
        Real64 MaxSteamCap; // steam coil capacity at full load
        bool ErrorsFound;   // returned from mining function call

        // The calculated load from the Heat Balance
        QTotLoad = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
        SysOutletNode = this->ReheatAirOutletNode;
        SysInletNode = this->InletNodeNum;
        CpAirZn = PsyCpAirFnW(Node(ZoneNodeNum).HumRat);
        HCType = this->ReheatComp_Num;
        FanType = this->Fan_Num;
        MaxCoolMassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        MaxHeatMassFlow = min(this->HeatAirMassFlowRateMax, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
        MinMassFlow = MaxCoolMassFlow * this->ZoneMinAirFrac;
        UnitFlowToler = 0.001 * HVACFlowRateToler;
        QDelivered = 0.0;
        HWFlow = 0.0;
        if (this->sd_airterminalInlet.AirMassFlowRateMaxAvail <= 0.0 || CurDeadBandOrSetback(ZoneNum)) {
            MassFlow = 0.0;
            FanOp = 0;
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered);
            return;
        }

        if (HCType == HCoilType_SimpleHeating) {
            WaterControlNode = this->ReheatControlNode;
            HCLoad = 0.0;
            if (FirstHVACIteration) {
                MaxFlowWater = this->MaxReheatWaterFlow;
                MinFlowWater = this->MinReheatWaterFlow;
            } else {
                WaterControlNode = this->ReheatControlNode;
                MaxFlowWater = Node(WaterControlNode).MassFlowRateMaxAvail;
                MinFlowWater = Node(WaterControlNode).MassFlowRateMinAvail;
            }
        } else {
            WaterControlNode = 0;
            HCLoad = BigLoad;
            MaxFlowWater = 0.0;
            MinFlowWater = 0.0;
        }

        if (HCType == HCoilType_SteamAirHeating) {
            SteamControlNode = this->ReheatControlNode;
            HCLoad = 0.0;
            if (FirstHVACIteration) {
                MaxFlowSteam = this->MaxReheatSteamFlow;
                MinFlowSteam = this->MinReheatSteamFlow;
            } else {
                MaxFlowSteam = Node(SteamControlNode).MassFlowRateMaxAvail;
                MinFlowSteam = Node(SteamControlNode).MassFlowRateMinAvail;
            }
        } else {
            SteamControlNode = 0;
            HCLoad = BigLoad;
            MaxFlowSteam = 0.0;
            MinFlowSteam = 0.0;
        }

        // define 3 load regions and assign the current load to the correct region.
        // region 1: active cooling with fan on
        FanOp = 1;
        if (HCType == HCoilType_SteamAirHeating) {
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin);
            // region 2: active heating with fan on
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax);
            MaxSteamCap = GetCoilCapacity(state, this->ReheatComp, this->ReheatName, ErrorsFound);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin);
            // region 3: active heating with fan off
            FanOp = 0;
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowSteam, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff);
        } else {
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin);
            // region 2: active heating with fan on
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin);
            // region 3: active heating with fan off
            FanOp = 0;
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax);
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff);
        }

        // Active cooling with fix for issue #5592
        if (QTotLoad < (-1.0 * SmallLoad) && QTotLoad < QCoolFanOnMin - SmallLoad && this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 &&
            !CurDeadBandOrSetback(ZoneNum)) {
            // check that it can meet the load
            FanOp = 1;
            if (QCoolFanOnMax < QTotLoad - SmallLoad) {
                Par(1) = double(this->SysNum);
                if (FirstHVACIteration) {
                    Par(2) = 1.0;
                } else {
                    Par(2) = 0.0;
                }
                Par(3) = double(ZoneNodeNum);
                Par(4) = double(HCType);
                if (HCType == HCoilType_SteamAirHeating) {
                    Par(5) = MinFlowSteam;
                } else {
                    Par(5) = MinFlowWater;
                }
                Par(6) = double(FanType);
                Par(7) = double(FanOp);
                Par(8) = QTotLoad;
                SolveRoot(state, UnitFlowToler, 50, SolFlag, MassFlow, this->VAVVSCoolingResidual, MinMassFlow, MaxCoolMassFlow, Par);
                if (SolFlag == -1) {
                    if (this->IterationLimit == 0) {
                        ShowWarningError("Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError("  Iteration limit exceeded in calculating air flow rate");
                    }
                    ShowRecurringWarningErrorAtEnd("Supply air flow Iteration limit exceeded in VS VAV terminal unit " + this->SysName,
                                                   this->IterationLimit);
                } else if (SolFlag == -2) {
                    if (this->IterationFailed == 0) {
                        ShowWarningError("Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError("  Bad air flow limits");
                    }
                    ShowRecurringWarningErrorAtEnd("Supply air flow control failed in VS VAV terminal unit " + this->SysName,
                                                   this->IterationFailed);
                }

            } else {

                MassFlow = MaxCoolMassFlow;

                if (HCType == HCoilType_SteamAirHeating) {
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, 0.0, FanType, MassFlow, FanOp, QDelivered);
                } else {
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
                }
            }

            // no active heating or cooling
        } else if ((QTotLoad >= QCoolFanOnMin - SmallLoad && QTotLoad <= QNoHeatFanOff + SmallLoad &&
                    this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) ||
                   (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 && CurDeadBandOrSetback(ZoneNum))) {
            MassFlow = MinMassFlow;
            FanOp = 0;
            if (HCType == HCoilType_SteamAirHeating) {
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowSteam, QTotLoad, FanType, MassFlow, FanOp, QNoHeatFanOff);
            } else {
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QNoHeatFanOff);
            }

            // active heating
        } else if (QTotLoad > QNoHeatFanOff + SmallLoad && this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 && !CurDeadBandOrSetback(ZoneNum)) {
            // hot water coil
            if (HCType == HCoilType_SimpleHeating) {
                if (QTotLoad < QHeatFanOffMax - SmallLoad) {
                    // vary HW flow, leave air flow at minimum
                    MassFlow = MinMassFlow;
                    FanOp = 0;
                    Par(1) = double(this->SysNum);
                    if (FirstHVACIteration) {
                        Par(2) = 1.0;
                    } else {
                        Par(2) = 0.0;
                    }
                    Par(3) = double(ZoneNodeNum);
                    Par(4) = double(HCType);
                    Par(5) = MassFlow;
                    Par(6) = double(FanType);
                    Par(7) = double(FanOp);
                    Par(8) = QTotLoad;
                    ErrTolerance = this->ControllerOffset;
                    SolveRoot(state, ErrTolerance, 500, SolFlag, HWFlow, this->VAVVSHWNoFanResidual, MinFlowWater, MaxFlowWater, Par);
                    if (SolFlag == -1) {
                        ShowRecurringWarningErrorAtEnd("Hot Water flow control failed in VS VAV terminal unit " + this->SysName,
                                                       this->ErrCount1);
                        ShowRecurringContinueErrorAtEnd("...Iteration limit (500) exceeded in calculating the hot water flow rate",
                                                        this->ErrCount1c);
                        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered);
                    } else if (SolFlag == -2) {
                        ShowRecurringWarningErrorAtEnd("Hot Water flow control failed (bad air flow limits) in VS VAV terminal unit " +
                                                           this->SysName,
                                                       this->ErrCount2);
                    }
                } else if (QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad) {
                    MassFlow = MinMassFlow;
                    FanOp = 0;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
                } else if (QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad) {
                    // set hot water flow to max and vary the supply air flow rate
                    FanOp = 1;
                    Par(1) = double(this->SysNum);
                    if (FirstHVACIteration) {
                        Par(2) = 1.0;
                    } else {
                        Par(2) = 0.0;
                    }
                    Par(3) = double(ZoneNodeNum);
                    Par(4) = double(HCType);
                    Par(5) = MaxFlowWater;
                    Par(6) = double(FanType);
                    Par(7) = double(FanOp);
                    Par(8) = QTotLoad;
                    SolveRoot(state, UnitFlowToler, 50, SolFlag, MassFlow, this->VAVVSHWFanOnResidual, MinMassFlow, MaxHeatMassFlow, Par);
                    if (SolFlag == -1) {
                        if (this->IterationLimit == 0) {
                            ShowWarningError("Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Iteration limit exceeded in calculating air flow rate");
                        }
                        ShowRecurringWarningErrorAtEnd("Supply air flow Iteration limit exceeded in VS VAV terminal unit " + this->SysName,
                                                       this->IterationLimit);
                    } else if (SolFlag == -2) {
                        if (this->IterationFailed == 0) {
                            ShowWarningError("Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Bad air flow limits");
                        }
                        ShowRecurringWarningErrorAtEnd("Supply air flow control failed in VS VAV terminal unit " + this->SysName,
                                                       this->IterationFailed);
                    }
                } else {
                    MassFlow = MaxHeatMassFlow;
                    FanOp = 1;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
                }
            } else if (HCType == HCoilType_SteamAirHeating) {
                //      IF (QTotLoad > QNoHeatFanOff + SmallLoad .AND. QTotLoad < QHeatFanOffMax - SmallLoad) THEN
                if (QTotLoad < QHeatFanOffMax - SmallLoad) {
                    // vary steam flow, leave air flow at minimum
                    MassFlow = MinMassFlow;
                    FanOp = 0;
                    Par(1) = double(this->SysNum);
                    if (FirstHVACIteration) {
                        Par(2) = 1.0;
                    } else {
                        Par(2) = 0.0;
                    }
                    Par(3) = double(ZoneNodeNum);
                    Par(4) = double(HCType);
                    Par(5) = MassFlow;
                    Par(6) = double(FanType);
                    Par(7) = double(FanOp);
                    Par(8) = QTotLoad;
                    Par(9) = MinFlowSteam;
                    Par(10) = MaxFlowSteam;
                    Par(11) = MaxSteamCap;
                    ErrTolerance = this->ControllerOffset;
                    SolveRoot(state, ErrTolerance, 500, SolFlag, HWFlow, this->VAVVSHWNoFanResidual, MinFlowSteam, MaxFlowSteam, Par);
                    if (SolFlag == -1) {
                        ShowRecurringWarningErrorAtEnd("Steam flow control failed in VS VAV terminal unit " + this->SysName,
                                                       this->ErrCount1);
                        ShowRecurringContinueErrorAtEnd("...Iteration limit (500) exceeded in calculating the hot water flow rate",
                                                        this->ErrCount1c);
                        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered);
                    } else if (SolFlag == -2) {
                        ShowRecurringWarningErrorAtEnd(
                            "Steam flow control failed (bad air flow limits) in VS VAV terminal unit " + this->SysName, this->ErrCount2);
                    }
                } else if (QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad) {
                    MassFlow = MinMassFlow;
                    FanOp = 0;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
                } else if (QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad) {
                    FanOp = 1;
                    Par(1) = double(this->SysNum);
                    if (FirstHVACIteration) {
                        Par(2) = 1.0;
                    } else {
                        Par(2) = 0.0;
                    }
                    Par(3) = double(ZoneNodeNum);
                    Par(4) = double(HCType);
                    Par(5) = MaxFlowSteam;
                    Par(6) = double(FanType);
                    Par(7) = double(FanOp);
                    Par(8) = QTotLoad;
                    SolveRoot(state, UnitFlowToler, 50, SolFlag, MassFlow, this->VAVVSHWFanOnResidual, MinMassFlow, MaxHeatMassFlow, Par);
                    if (SolFlag == -1) {
                        if (this->IterationLimit == 0) {
                            ShowWarningError("Steam heating coil control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Iteration limit exceeded in calculating air flow rate");
                        }
                        ShowRecurringWarningErrorAtEnd("Steam heating coil iteration limit exceeded in VS VAV terminal unit " + this->SysName,
                                                       this->IterationLimit);
                    } else if (SolFlag == -2) {
                        if (this->IterationFailed == 0) {
                            ShowWarningError("Steam heating coil control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Bad air flow limits");
                        }
                        ShowRecurringWarningErrorAtEnd("Steam heating coil control failed in VS VAV terminal unit " + this->SysName,
                                                       this->IterationFailed);
                    }
                } else {
                    MassFlow = MaxHeatMassFlow;
                    FanOp = 1;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, QTotLoad, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
                }
            } else if (HCType == HCoilType_Gas || HCType == HCoilType_Electric) {
                if (QTotLoad <= QHeatFanOnMin + SmallLoad) {
                    // vary heating coil power, leave mass flow at minimum
                    MassFlow = MinMassFlow;
                    FanOp = 0;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
                } else if (QTotLoad > QHeatFanOnMin + SmallLoad && QTotLoad < QHeatFanOnMax - SmallLoad) {
                    FanOp = 1;
                    Par(1) = double(this->SysNum);
                    if (FirstHVACIteration) {
                        Par(2) = 1.0;
                    } else {
                        Par(2) = 0.0;
                    }
                    Par(3) = double(ZoneNodeNum);
                    Par(4) = double(HCType);
                    Par(5) = this->ReheatCoilMaxCapacity;
                    Par(6) = double(FanType);
                    Par(7) = double(FanOp);
                    Par(8) = QTotLoad;
                    SolveRoot(state, UnitFlowToler, 50, SolFlag, FracDelivered, this->VAVVSHCFanOnResidual, 0.0, 1.0, Par);
                    MassFlow = Node(SysInletNode).MassFlowRate;
                    if (SolFlag == -1) {
                        if (this->IterationLimit == 0) {
                            ShowWarningError("Heating coil control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Iteration limit exceeded in calculating air flow rate");
                        }
                        ShowRecurringWarningErrorAtEnd("Heating coil control iteration limit exceeded in VS VAV terminal unit " + this->SysName,
                                                       this->IterationLimit);
                    } else if (SolFlag == -2) {
                        if (this->IterationFailed == 0) {
                            ShowWarningError("Heating coil control failed in VS VAV terminal unit " + this->SysName);
                            ShowContinueError("  Bad air flow limits");
                        }
                        ShowRecurringWarningErrorAtEnd("Heating coil control failed in VS VAV terminal unit " + this->SysName,
                                                       this->IterationFailed);
                    }
                } else {
                    MassFlow = MaxHeatMassFlow;
                    FanOp = 1;
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
                }
            } else {
                ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
            }

        } else {

            MassFlow = 0.0;
            FanOp = 0;
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HCType, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered);
        }

        // Move mass flow rates to the damper outlet node
        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->sd_airterminalOutlet.AirMassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        this->sd_airterminalOutlet.AirMassFlowRateMinAvail = this->sd_airterminalInlet.AirMassFlowRateMinAvail;

        // calculate VAV damper Position.
        if (this->AirMassFlowRateMax == 0.0) {
            this->DamperPosition = 0.0;
        } else {
            this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
        }
        // update the air terminal outlet node data
        this->UpdateSys();

    }

    void SingleDuctAirTerminal::SimConstVol(EnergyPlusData &state, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   February 2000
        //       MODIFIED       FB/KHL/TH 2/2011: added maximum supply air temperature leaving reheat coil
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple single duct constant volume systems.

        // METHODOLOGY EMPLOYED:
        // There is method to this madness.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        // unused   USE DataHeatBalFanSys, ONLY: Mat
        using HeatingCoils::SimulateHeatingCoilComponents;
        using PlantUtilities::SetActuatedBranchFlowRate;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MassFlow;     // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
        Real64 QZnReq;       // [Watts]
        Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
        Real64 CpAir;
        int WaterControlNode;        // This is the Actuated Reheat Control Node
        Real64 MaxFlowWater;         // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 MinFlowWater;         // This is the value passed to the Controller depending if FirstHVACIteration or not
        Real64 QActualHeating;       // the heating load seen by the reheat coil
        static Real64 TAirMax(0.0);  // Maximum zone supply air temperature [C]
        static Real64 QMax(0.0);     // Maximum heat addition rate imposed by the max zone supply air temperature [W]
        static Real64 ZoneTemp(0.0); // Zone temperature [C]
        static Real64 QMax2(0.0);
        Real64 DummyMdot; // local fluid mass flow rate

        QToHeatSetPt = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP; // The calculated load from the Heat Balance
        MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;                    // System massflow is set to the Available
        QMax2 = QToHeatSetPt;
        ZoneTemp = Node(ZoneNodeNum).Temp;
        CpAir = PsyCpAirFnW(Node(ZoneNodeNum).HumRat); // zone air specific heat
        if (this->MaxReheatTempSetByUser) {
            TAirMax = this->MaxReheatTemp;
            QMax = CpAir * MassFlow * (TAirMax - ZoneTemp);
            QMax2 = min(QToHeatSetPt, QMax);
        } // if (this->MaxReheatTempSetByUser) {

        if (((this->sd_airterminalInlet.AirMassFlowRateMaxAvail == 0.0) && (this->sd_airterminalInlet.AirMassFlowRateMinAvail == 0.0)) ||
            (this->sd_airterminalInlet.AirMassFlowRate == 0.0)) {
            // System is Off set massflow to 0.0
            MassFlow = 0.0;
        }

        // Calculate the Damper Position when there is a Max air flow specified.
        if (this->AirMassFlowRateMax == 0.0) {
            this->DamperPosition = 0.0;
        } else {
            this->DamperPosition = MassFlow / this->AirMassFlowRateMax;
        }

        // make sure the inlet node flow rate is updated if the mass flow has been limited
        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->sd_airterminalOutlet.AirMassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        this->sd_airterminalOutlet.AirMassFlowRateMinAvail = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
        this->UpdateSys();

        QActualHeating = QToHeatSetPt - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - ZoneTemp); // reheat needed
        // Now the massflow for reheating has been determined. If it is zero, or in SetBack, or the
        // system scheduled OFF then not operational and shut the system down.
        if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (TempControlType(ZoneNum) != SingleCoolingSetPoint)) {

            {
                auto const SELECT_CASE_var(this->ReheatComp_Num);

                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QMax2 + MassFlow * CpAir * ZoneTemp;

                    // Before Iterating through the Reheat Coil and Controller set the flags for the
                    // Do Loop to initialized conditions.
                    // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                    // Initialize hot water flow rate to zero.
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if (FirstHVACIteration) {
                        MaxFlowWater = this->MaxReheatWaterFlow;
                        MinFlowWater = this->MinReheatWaterFlow;
                    } else {
                        WaterControlNode = this->ReheatControlNode;
                        MaxFlowWater = Node(WaterControlNode).MassFlowRateMaxAvail;
                        MinFlowWater = Node(WaterControlNode).MassFlowRateMinAvail;
                    }

                    // Simulate reheat coil for the Const Volume system
                    // Set Converged to True & when controller is not converged it will set to False.
                    ControlCompOutput(state, this->ReheatName,
                                      this->ReheatComp,
                                      this->ReheatComp_Index,
                                      FirstHVACIteration,
                                      QZnReq,
                                      this->ReheatControlNode,
                                      MaxFlowWater,
                                      MinFlowWater,
                                      this->ControllerOffset,
                                      this->ControlCompTypeNum,
                                      this->CompErrIndex,
                                      _,
                                      this->ReheatAirOutletNode,
                                      MassFlow,
                                      _,
                                      _,
                                      this->HWLoopNum,
                                      this->HWLoopSide,
                                      this->HWBranchIndex);

                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // COIL:STEAM:STEAMAIRHEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QMax2 - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);
                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QMax2 - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Determine the load required to pass to the Component controller
                    QZnReq = QMax2 - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - ZoneTemp);

                    // Simulate reheat coil for the VAV system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);
                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }

            // the COIL is OFF the properties are calculated for this special case.
        } else {
            {
                auto const SELECT_CASE_var(this->ReheatComp_Num);

                if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                    // Simulate reheat coil for the Const Volume system
                    // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                    // Initialize hot water flow rate to zero.
                    DummyMdot = 0.0;
                    SetActuatedBranchFlowRate(
                        DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                    // call the reheat coil with the NO FLOW condition to make sure that the Node values
                    // are passed through to the coil outlet correctly
                    SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
                } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // COIL:STEAM:AIRHEATING
                    // Simulate reheat coil for the Const Volume system
                    SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

                } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                    // Simulate reheat coil for the Const Volume system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

                } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                    // Simulate reheat coil for the Const Volume system
                    SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
                } else {
                    ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
                }
            }
        }
    }

    void SingleDuctAirTerminal::SimConstVolNoReheat(int const ZoneNodeNum)
    {

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the simple single duct constant volume systems with no reheat.

        using DataGlobals::SecInHour;
        using DataHVACGlobals::TimeStepSys;
        using Psychrometrics::PsyHFnTdbW;

        Real64 MassFlow;           // [kg/sec]   mass flow rate at the inlet
        Real64 SensOutputProvided; // heating and cooling provided to the zone [W]

        MassFlow = this->sd_airterminalInlet.AirMassFlowRate; // system air mass flow rate

        if (GetCurrentScheduleValue(this->SchedPtr) > 0.0 && MassFlow > SmallMassFlow) {
            Real64 CpAir = PsyCpAirFnW(0.5 * (Node(this->OutletNodeNum).HumRat + Node(ZoneNodeNum).HumRat));
            SensOutputProvided = MassFlow * CpAir * (Node(this->OutletNodeNum).Temp - Node(ZoneNodeNum).Temp);
        } else {
            SensOutputProvided = 0.0;
        }

        // set the outlet node air conditions to that of the inlet
        this->sd_airterminalOutlet.AirTemp = this->sd_airterminalInlet.AirTemp;
        this->sd_airterminalOutlet.AirHumRat = this->sd_airterminalInlet.AirHumRat;
        this->sd_airterminalOutlet.AirEnthalpy = this->sd_airterminalInlet.AirEnthalpy;
        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
        this->sd_airterminalOutlet.AirMassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        this->sd_airterminalOutlet.AirMassFlowRateMinAvail = this->sd_airterminalInlet.AirMassFlowRateMinAvail;

        // air heat transfer rate and energy
        this->HeatRate = max(SensOutputProvided, 0.0);
        this->CoolRate = std::abs(min(SensOutputProvided, 0.0));
        this->HeatEnergy = this->HeatRate * TimeStepSys * SecInHour;
        this->CoolEnergy = this->CoolRate * TimeStepSys * SecInHour;

        // update the air terminal outlet node data
        this->UpdateSys();
    }

    void SingleDuctAirTerminal::CalcVAVVS(EnergyPlusData &state, bool const FirstHVACIteration,  // flag for 1st HVAV iteration in the time step
                   int const ZoneNode,             // zone node number
                   int const EP_UNUSED(HCoilType), // type of hot water coil !unused1208
                   Real64 const HWFlow,            // hot water flow (kg/s)
                   Real64 const HCoilReq,          // gas or elec coil demand requested
                   int const FanType,              // type of fan
                   Real64 const AirFlow,           // air flow rate (kg/s)
                   int const FanOn,                // 1 means fan is on
                   Real64 &LoadMet                 // load met by unit (watts)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate the components making up the VAV terminal unit with variable speed fan.

        // METHODOLOGY EMPLOYED:
        // Simulates the unit components sequentially in the air flow direction.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::TurnFansOff;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using PlantUtilities::SetComponentFlowRate;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int FanInNode;       // unit air inlet node (fan inlet)
        int FanOutNode;      // fan outlet node (heating coil inlet node)
        int HCOutNode;       // unit air outlet node (heating coil air outlet node)
        int HotControlNode;  // the hot water inlet node
        Real64 AirMassFlow;  // total air mass flow rate [kg/s]
        Real64 CpAirZn;      // zone air specific heat [J/kg-C]
        bool TurnFansOffSav; // save the fan off flag
        Real64 mdot;

        TurnFansOffSav = TurnFansOff;
        FanInNode = this->InletNodeNum;
        FanOutNode = this->OutletNodeNum;
        HCOutNode = this->ReheatAirOutletNode;
        HotControlNode = this->ReheatControlNode;
        AirMassFlow = AirFlow;
        Node(FanInNode).MassFlowRate = AirMassFlow;
        CpAirZn = PsyCpAirFnW(Node(ZoneNode).HumRat);
        if (FanType == DataHVACGlobals::FanType_SimpleVAV && FanOn == 1) {
            Fans::SimulateFanComponents(state, this->FanName, FirstHVACIteration, this->Fan_Index);
        } else if (FanType == DataHVACGlobals::FanType_SystemModelObject && FanOn == 1) {
            HVACFan::fanObjs[this->Fan_Index]->simulate(state, _, _, _, _);

        } else { // pass through conditions
            TurnFansOff = true;
            if (FanType == DataHVACGlobals::FanType_SimpleVAV) {
                Fans::SimulateFanComponents(state, this->FanName, FirstHVACIteration, this->Fan_Index);
            } else if (FanType == DataHVACGlobals::FanType_SystemModelObject) {
                HVACFan::fanObjs[this->Fan_Index]->simulate(state, _, _, TurnFansOff, _);
            }
            TurnFansOff = TurnFansOffSav;
            Node(FanOutNode).MassFlowRate = Node(FanInNode).MassFlowRate;
            Node(FanOutNode).MassFlowRateMaxAvail = Node(FanInNode).MassFlowRateMaxAvail;
            Node(FanOutNode).MassFlowRateMinAvail = Node(FanInNode).MassFlowRateMinAvail;
        }
        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);
            if (SELECT_CASE_var == HCoilType_SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                mdot = HWFlow;
                if (this->HWLoopNum > 0) {
                    SetComponentFlowRate(mdot,
                                         this->ReheatControlNode,
                                         this->ReheatCoilOutletNode,
                                         this->HWLoopNum,
                                         this->HWLoopSide,
                                         this->HWBranchIndex,
                                         this->HWCompIndex);
                }

                SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HCoilType_SteamAirHeating) { // HW Flow is steam mass flow here
                mdot = HWFlow;
                if (this->HWLoopNum > 0) {
                    SetComponentFlowRate(mdot,
                                         this->ReheatControlNode,
                                         this->ReheatCoilOutletNode,
                                         this->HWLoopNum,
                                         this->HWLoopSide,
                                         this->HWBranchIndex,
                                         this->HWCompIndex);
                }
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, HCoilReq);
            } else if (SELECT_CASE_var == HCoilType_Electric) { // COIL:ELECTRIC:HEATING
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, HCoilReq, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HCoilType_Gas) { // COIL:GAS:HEATING
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, HCoilReq, this->ReheatComp_Index);
            } else {
                ShowFatalError("Invalid Reheat Component=" + this->ReheatComp);
            }
        }

        LoadMet = AirMassFlow * CpAirZn * (Node(HCOutNode).Temp - Node(ZoneNode).Temp);
    }

    Real64 SingleDuctAirTerminal::VAVVSCoolingResidual(EnergyPlusData &state, Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
                                Array1D<Real64> const &Par       // Par(1) = REAL(SysNum)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
        // Unit Output depends on the supply air flow rate which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcVAVVS, and calculates
        // the residual as defined above.

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // Par(2) = FirstHVACIteration (1. or 0.)
        // Par(3) = REAL(ZoneNodeNum)
        // Par(4) = REAL(HCType)
        // Par(5) = minimum HW flow rate [kg/s]
        // Par(6) = REAL(FanType)
        // Par(7) = REAL(FanOp)
        // Par(8) = cooling demand [W] (negative)

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int UnitIndex;
        bool FirstHVACSoln;
        int ZoneNodeIndex;
        Real64 MinHWFlow;  // min hot water flow rate
        int HCType;        // heating coil type (integer)
        int FanType;       // fan type (as an integer)
        int FanOp;         // fan operation; 0=off, 1=on.
        Real64 UnitOutput; // cooling output [W] (cooling is negative)

        UnitIndex = int(Par(1));
        FirstHVACSoln = (Par(2) > 0.0);
        ZoneNodeIndex = int(Par(3));
        HCType = int(Par(4));
        MinHWFlow = Par(5);
        FanType = int(Par(6));
        FanOp = int(Par(7));
        sd_airterminal(UnitIndex).CalcVAVVS(state, FirstHVACSoln, ZoneNodeIndex, HCType, MinHWFlow, 0.0, FanType, SupplyAirMassFlow, FanOp, UnitOutput);

        Residuum = (Par(8) - UnitOutput) / Par(8);

        return Residuum;
    }

    Real64 SingleDuctAirTerminal::VAVVSHWNoFanResidual(EnergyPlusData &state, Real64 const HWMassFlow,  // hot water mass flow rate [kg/s]
                                Array1D<Real64> const &Par // Par(1) = REAL(SysNum)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
        // Unit Output depends on the hot water flow rate which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcVAVVS, and calculates
        // the residual as defined above.

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // Par(2) = FirstHVACIteration (1. or 0.)
        // Par(3) = REAL(ZoneNodeNum)
        // Par(4) = REAL(HCType)
        // Par(5) = air mass flow flow rate [kg/s]
        // Par(6) = REAL(FanType)
        // Par(7) = REAL(FanOp)
        // Par(8) = heating demand [W]
        // Par(9) = min steam flow rate [m3/s] - steam only
        // Par(10 = max steam flow rate [m3/s] - steam only

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int UnitIndex;
        bool FirstHVACSoln;
        int ZoneNodeIndex;
        Real64 AirMassFlow; // supply air mass flow rate [kg/s]
        int HCType;         // heating coil type (integer)
        int FanType;        // fan type (as an integer)
        int FanOp;          // fan operation; 0=off, 1=on.
        Real64 UnitOutput;  // heating output [W]
        Real64 QSteamLoad;  // proportional load to calculate steam flow [W]
        Real64 MinSteamFlow;
        Real64 MaxSteamFlow;
        Real64 MaxSteamCoilCapacity;

        UnitIndex = int(Par(1));
        FirstHVACSoln = (Par(2) > 0.0);
        ZoneNodeIndex = int(Par(3));
        HCType = int(Par(4));
        AirMassFlow = Par(5);
        FanType = int(Par(6));
        FanOp = int(Par(7));
        QSteamLoad = 0.0;
        // vary the load to be met by the steam coil to converge on a steam flow rate to meet the load
        if (HCType == HCoilType_SteamAirHeating) {
            //   backwards way of varying steam flow rate. Steam coil calculates a flow rate to meet a load.
            MinSteamFlow = Par(9);
            MaxSteamFlow = Par(10);
            MaxSteamCoilCapacity = Par(11);
            if ((MaxSteamFlow - MinSteamFlow) == 0.0) {
                QSteamLoad = Par(8); // Use QTotLoad, bad starting value error for RegulaFalsi will occur
            } else {
                QSteamLoad = MaxSteamCoilCapacity * HWMassFlow / (MaxSteamFlow - MinSteamFlow);
            }
        }
        sd_airterminal(UnitIndex).CalcVAVVS(state, FirstHVACSoln, ZoneNodeIndex, HCType, HWMassFlow, QSteamLoad, FanType, AirMassFlow, FanOp, UnitOutput);

        Residuum = (Par(8) - UnitOutput) / Par(8);

        return Residuum;
    }

    Real64 SingleDuctAirTerminal::VAVVSHWFanOnResidual(EnergyPlusData &state, Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
                                Array1D<Real64> const &Par       // Par(1) = REAL(SysNum)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
        // Unit Output depends on the supply air flow rate which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcVAVVS, and calculates
        // the residual as defined above.

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // Par(2) = FirstHVACIteration (1. or 0.)
        // Par(3) = REAL(ZoneNodeNum)
        // Par(4) = REAL(HCType)
        // Par(5) = hot water mass flow rate [kg/s]
        // Par(6) = REAL(FanType)
        // Par(7) = REAL(FanOp)
        // Par(8) = heating demand [W]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int UnitIndex;
        bool FirstHVACSoln;
        int ZoneNodeIndex;
        Real64 HWMassFlow; // hot water mass flow rate [kg/s]
        int HCType;        // heating coil type (integer)
        int FanType;       // fan type (as an integer)
        int FanOp;         // fan operation; 0=off, 1=on.
        Real64 UnitOutput; // heating output [W]

        UnitIndex = int(Par(1));
        FirstHVACSoln = (Par(2) > 0.0);
        ZoneNodeIndex = int(Par(3));
        HCType = int(Par(4));
        HWMassFlow = Par(5);
        FanType = int(Par(6));
        FanOp = int(Par(7));
        sd_airterminal(UnitIndex).CalcVAVVS(state, FirstHVACSoln, ZoneNodeIndex, HCType, HWMassFlow, Par(8), FanType, SupplyAirMassFlow, FanOp, UnitOutput);

        Residuum = (Par(8) - UnitOutput) / Par(8);

        return Residuum;
    }

    Real64 SingleDuctAirTerminal::VAVVSHCFanOnResidual(EnergyPlusData &state, Real64 const HeatingFrac, // fraction of maximum heating output
                                Array1D<Real64> const &Par // Par(1) = REAL(SysNum)
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   July 2004
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS FUNCTION:
        // Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
        // Unit Output depends on the heating coil output which is being varied to zero the residual.

        // METHODOLOGY EMPLOYED:
        // Calls CalcVAVVS, and calculates
        // the residual as defined above.

        // REFERENCES:

        // USE STATEMENTS:
        // na

        // Return value
        Real64 Residuum; // residual to be minimized to zero

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        // Par(2) = FirstHVACIteration (1. or 0.)
        // Par(3) = REAL(ZoneNodeNum)
        // Par(4) = REAL(HCType)
        // Par(5) = max heating coil output [W]
        // Par(6) = REAL(FanType)
        // Par(7) = REAL(FanOp)
        // Par(8) = heating demand [W]

        // FUNCTION PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int UnitIndex;
        bool FirstHVACSoln;
        int ZoneNodeIndex;
        Real64 MaxHeatOut;      // maximum heating output [W]
        int HCType;             // heating coil type (integer)
        int FanType;            // fan type (as an integer)
        int FanOp;              // fan operation; 0=off, 1=on.
        Real64 UnitOutput;      // heating output [W]
        Real64 AirMassFlowRate; // [kg/s]
        Real64 HeatOut;         // heating coil output [W]

        UnitIndex = int(Par(1));
        FirstHVACSoln = (Par(2) > 0.0);
        ZoneNodeIndex = int(Par(3));
        HCType = int(Par(4));
        MaxHeatOut = Par(5);
        FanType = int(Par(6));
        FanOp = int(Par(7));
        HeatOut = HeatingFrac * MaxHeatOut;
        AirMassFlowRate =
            max(HeatingFrac * sd_airterminal(UnitIndex).HeatAirMassFlowRateMax, sd_airterminal(UnitIndex).sd_airterminalInlet.AirMassFlowRateMaxAvail * sd_airterminal(UnitIndex).ZoneMinAirFrac);

        sd_airterminal(UnitIndex).CalcVAVVS(state, FirstHVACSoln, ZoneNodeIndex, HCType, 0.0, HeatOut, FanType, AirMassFlowRate, FanOp, UnitOutput);

        Residuum = (Par(8) - UnitOutput) / Par(8);

        return Residuum;
    }

    // End Algorithm Section of the Module
    // *****************************************************************************

    // Beginning of Update subroutines for the Sys Module
    // *****************************************************************************

    void SingleDuctAirTerminal::UpdateSys()
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   january 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the Syss.

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
        int InletNode;

        OutletNode = this->OutletNodeNum;
        InletNode = this->InletNodeNum;

        if (this->SysType_Num == SingleDuctVAVReheat || this->SysType_Num == SingleDuctCBVAVReheat ||
            this->SysType_Num == SingleDuctCBVAVNoReheat || this->SysType_Num == SingleDuctVAVNoReheat ||
            this->SysType_Num == SingleDuctConstVolNoReheat) {
            // Set the outlet air nodes of the Sys
            Node(OutletNode).MassFlowRate = this->sd_airterminalOutlet.AirMassFlowRate;
            Node(OutletNode).Temp = this->sd_airterminalOutlet.AirTemp;
            Node(OutletNode).HumRat = this->sd_airterminalOutlet.AirHumRat;
            Node(OutletNode).Enthalpy = this->sd_airterminalOutlet.AirEnthalpy;
            // Set the outlet nodes for properties that just pass through & not used
            Node(OutletNode).Quality = Node(InletNode).Quality;
            Node(OutletNode).Press = Node(InletNode).Press;
        }

        // After all of the Outlets are updated the mass flow information needs to be
        // passed back to the system inlet.
        Node(InletNode).MassFlowRate = this->sd_airterminalOutlet.AirMassFlowRate;
        Node(OutletNode).MassFlowRateMaxAvail = min(this->sd_airterminalOutlet.AirMassFlowRateMaxAvail, Node(OutletNode).MassFlowRateMax);
        Node(OutletNode).MassFlowRateMinAvail = this->sd_airterminalOutlet.AirMassFlowRateMinAvail;

        if (Contaminant.CO2Simulation) {
            Node(OutletNode).CO2 = Node(InletNode).CO2;
        }

        if (Contaminant.GenericContamSimulation) {
            Node(OutletNode).GenContam = Node(InletNode).GenContam;
        }
    }

    //        End of Update subroutines for the Sys Module
    // *****************************************************************************

    // Beginning of Reporting subroutines for the Sys Module
    // *****************************************************************************

    void SingleDuctAirTerminal::ReportSys() // unused1208
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Unknown
        //       DATE WRITTEN   Unknown
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine updates the Sys report variables.

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

        // Still needs to report the Sys power from this component

        // set zone OA volume flow rate
        this->CalcOutdoorAirVolumeFlowRate();
    }

    void GetHVACSingleDuctSysIndex(EnergyPlusData &state, std::string const &SDSName,
                                   int &SDSIndex,
                                   bool &ErrorsFound,
                                   Optional_string_const ThisObjectType,
                                   Optional_int DamperInletNode, // Damper inlet node number
                                   Optional_int DamperOutletNode // Damper outlet node number
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   February 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine sets an index for a given single duct system -- issues error message if that system
        // is not a legal system.

        if (GetInputFlag) { // First time subroutine has been entered
            GetSysInput(state);
            GetInputFlag = false;
        }

        SDSIndex = UtilityRoutines::FindItemInList(SDSName, sd_airterminal, &SingleDuctAirTerminal::SysName);
        if (SDSIndex == 0) {
            if (present(ThisObjectType)) {
                ShowSevereError(ThisObjectType() + ", GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName);
            } else {
                ShowSevereError("GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName);
            }
            ErrorsFound = true;
        } else {
            if ((sd_airterminal(SDSIndex).SysType_Num != SingleDuctConstVolReheat) && (sd_airterminal(SDSIndex).SysType_Num != SingleDuctVAVReheat)) {
                ShowSevereError(ThisObjectType() + ", GetHVACSingleDuctSysIndex: Could not find allowed types=" + SDSName);
                ShowContinueError("The allowed types are: AirTerminal:SingleDuct:ConstantVolume:Reheat and AirTerminal:SingleDuct:VAV:Reheat");
                ErrorsFound = true;
            }
            if (sd_airterminal(SDSIndex).SysType_Num == SingleDuctVAVReheat) {
                if (present(DamperInletNode)) DamperInletNode = sd_airterminal(SDSIndex).InletNodeNum;
                if (present(DamperOutletNode)) DamperOutletNode = sd_airterminal(SDSIndex).OutletNodeNum;
            }
        }
    }

    void SimATMixer(std::string const &SysName, bool const FirstHVACIteration, int &SysIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Simulate an Air Terminal Mixer component

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static int SysNum(0);

        if (GetATMixerFlag) {
            GetATMixerFlag = false;
        }

        if (SysIndex == 0) {
            SysNum = UtilityRoutines::FindItemInList(SysName, SysATMixer);
            SysIndex = SysNum;
            if (SysNum == 0) {
                ShowFatalError("Object " + SysName + " not found");
            }
        } else {
            SysNum = SysIndex;
        }

        SysATMixer(SysNum).InitATMixer(FirstHVACIteration);

        CalcATMixer(SysNum);

        UpdateATMixer(SysNum);
    }

    void GetATMixers(ZoneAirLoopEquipmentManagerData &dataZoneAirLoopEquipmentManager)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Get input for inlet side air terminal mixers and store it in the inlet side air terminal mixer array

        // METHODOLOGY EMPLOYED:
        // Use the Get routines from the InputProcessor module.

        // Using/Aliasing
        using DataZoneEquipment::EquipmentData;
        using DataZoneEquipment::SubEquipmentData;
        using DataZoneEquipment::ZoneEquipConfig;
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;
        using namespace DataIPShortCuts;
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using DataDefineEquip::AirDistUnit;
        using DataDefineEquip::NumAirDistUnits;
        using DataGlobals::NumOfZones;
        using DataHVACGlobals::ATMixer_InletSide;
        using DataHVACGlobals::ATMixer_SupplySide;
        using DataSizing::NumZoneSizingInput;
        using DataSizing::ZoneSizingInput;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumNums;    // Number of REAL(r64) numbers returned by GetObjectItem
        int NumAlphas;  // Number of alphanumerics returned by GetObjectItem
        int ATMixerNum; // Index of inlet side mixer air terminal unit
        int IOStat;
        static std::string const RoutineName("GetATMixers: "); // include trailing blank space
        static bool ErrorsFound(false);                        // Error flag
        int NodeNum;                                           // Index to node number
        int CtrlZone;                                          // Index to control zone
        bool ZoneNodeNotFound;                                 // Flag for error checking
        bool errFlag;                                          // error flag from component validation

        if (!GetATMixerFlag) {
            return;
        }
        GetATMixerFlag = false;

        cCurrentModuleObject = "AirTerminal:SingleDuct:Mixer";
        NumATMixers = inputProcessor->getNumObjectsFound(cCurrentModuleObject);
        SysATMixer.allocate(NumATMixers);

        // Need air distribution units first
        ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(dataZoneAirLoopEquipmentManager);

        for (ATMixerNum = 1; ATMixerNum <= NumATMixers; ++ATMixerNum) {
            inputProcessor->getObjectItem(cCurrentModuleObject,
                                          ATMixerNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNums,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            SysATMixer(ATMixerNum).Name = cAlphaArgs(1);
            if (cAlphaArgs(7) == "INLETSIDE") {
                SysATMixer(ATMixerNum).MixerType = ATMixer_InletSide; // inlet side mixer
            } else if (cAlphaArgs(7) == "SUPPLYSIDE") {
                SysATMixer(ATMixerNum).MixerType = ATMixer_SupplySide; // supply side mixer
            }
            if (cAlphaArgs(2) == "ZONEHVAC:WATERTOAIRHEATPUMP") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 1;
            } else if (cAlphaArgs(2) == "ZONEHVAC:FOURPIPEFANCOIL") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 2;
            } else if (cAlphaArgs(2) == "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 3;
            } else if (cAlphaArgs(2) == "ZONEHVAC:PACKAGEDTERMINALHEATPUMP") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 4;
            } else if (cAlphaArgs(2) == "ZONEHVAC:VARIABLEREFRIGERANTFLOW") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 5;
            } else if (cAlphaArgs(2) == "AIRLOOPHVAC:UNITARYSYSTEM") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 6;
            } else if (cAlphaArgs(2) == "ZONEHVAC:UNITVENTILATOR") {
                SysATMixer(ATMixerNum).ZoneHVACUnitType = 7;
            }

            SysATMixer(ATMixerNum).ZoneHVACUnitName = cAlphaArgs(3);

            ValidateComponent(cAlphaArgs(2), SysATMixer(ATMixerNum).ZoneHVACUnitName, errFlag, cCurrentModuleObject);

            SysATMixer(ATMixerNum).MixedAirOutNode = GetOnlySingleNode(cAlphaArgs(4),
                                                                       ErrorsFound,
                                                                       cCurrentModuleObject,
                                                                       cAlphaArgs(1),
                                                                       NodeType_Air,
                                                                       NodeConnectionType_Outlet,
                                                                       1,
                                                                       ObjectIsNotParent,
                                                                       cAlphaFieldNames(4));

            SysATMixer(ATMixerNum).PriInNode = GetOnlySingleNode(cAlphaArgs(5),
                                                                 ErrorsFound,
                                                                 cCurrentModuleObject,
                                                                 cAlphaArgs(1),
                                                                 NodeType_Air,
                                                                 NodeConnectionType_Inlet,
                                                                 1,
                                                                 ObjectIsNotParent,
                                                                 cAlphaFieldNames(5));
            SysATMixer(ATMixerNum).SecInNode = GetOnlySingleNode(cAlphaArgs(6),
                                                                 ErrorsFound,
                                                                 cCurrentModuleObject,
                                                                 cAlphaArgs(1),
                                                                 NodeType_Air,
                                                                 NodeConnectionType_Inlet,
                                                                 1,
                                                                 ObjectIsNotParent,
                                                                 cAlphaFieldNames(6));

            if (lAlphaFieldBlanks(8)) {
                SysATMixer(ATMixerNum).NoOAFlowInputFromUser = true;
            } else {
                SysATMixer(ATMixerNum).OARequirementsPtr = UtilityRoutines::FindItemInList(cAlphaArgs(8), DataSizing::OARequirements);
                if (SysATMixer(ATMixerNum).OARequirementsPtr == 0) {
                    ShowSevereError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFieldNames(8) + "=\"" + cAlphaArgs(8) + "\".");
                    ErrorsFound = true;
                } else {
                    SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
                }
            }

            if (lAlphaFieldBlanks(9)) {
                SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
            } else {
                if (cAlphaArgs(9) == "CURRENTOCCUPANCY") {
                    SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                } else if (cAlphaArgs(9) == "DESIGNOCCUPANCY") {
                    SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonByDesignLevel;
                } else {
                    SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                    ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                    ShowContinueError("..invalid " + cAlphaFieldNames(9) + "=\"" + cAlphaArgs(9) +
                                      "\". The default input of CurrentOccupancy is assigned");
                }
            }

            // Check for dupes in the three nodes.
            if (SysATMixer(ATMixerNum).SecInNode == SysATMixer(ATMixerNum).PriInNode) {
                ShowSevereError(cCurrentModuleObject + " = " + SysATMixer(ATMixerNum).Name + ' ' + cAlphaArgs(5) + " = " +
                                NodeID(SysATMixer(ATMixerNum).PriInNode) + " duplicates the " + cAlphaArgs(4) + '.');
                ErrorsFound = true;
            } else if (SysATMixer(ATMixerNum).SecInNode == SysATMixer(ATMixerNum).MixedAirOutNode) {
                ShowSevereError(cCurrentModuleObject + " = " + SysATMixer(ATMixerNum).Name + ' ' + cAlphaArgs(6) + " = " +
                                NodeID(SysATMixer(ATMixerNum).MixedAirOutNode) + " duplicates the " + cAlphaArgs(4) + '.');
                ErrorsFound = true;
            }

            if (SysATMixer(ATMixerNum).PriInNode == SysATMixer(ATMixerNum).MixedAirOutNode) {
                ShowSevereError(cCurrentModuleObject + " = " + SysATMixer(ATMixerNum).Name + ' ' + cAlphaArgs(6) + " = " +
                                NodeID(SysATMixer(ATMixerNum).MixedAirOutNode) + " duplicates the " + cAlphaArgs(5) + '.');
                ErrorsFound = true;
            }

            for (int ADUNum = 1; ADUNum <= NumAirDistUnits; ++ADUNum) {
                if (SysATMixer(ATMixerNum).MixedAirOutNode == AirDistUnit(ADUNum).OutletNodeNum) {
                    AirDistUnit(ADUNum).InletNodeNum = SysATMixer(ATMixerNum).PriInNode;
                    SysATMixer(ATMixerNum).ADUNum = ADUNum;
                    break;
                }
            }
            // one assumes if there isn't one assigned, it's an error?
            if (SysATMixer(ATMixerNum).ADUNum == 0) {
                ShowSevereError(RoutineName + "No matching Air Distribution Unit, for System = [" + cCurrentModuleObject + ',' +
                                SysATMixer(ATMixerNum).Name + "].");
                ShowContinueError("...should have outlet node = " + NodeID(SysATMixer(ATMixerNum).MixedAirOutNode));
                ErrorsFound = true;
            } else {

                if (SysATMixer(ATMixerNum).MixerType == ATMixer_InletSide) {
                    // Air Terminal inlet node must be the same as a zone exhaust node
                    ZoneNodeNotFound = true;
                    for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                            if (SysATMixer(ATMixerNum).SecInNode == ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                                ZoneNodeNotFound = false;
                                AirDistUnit(SysATMixer(ATMixerNum).ADUNum).ZoneEqNum = CtrlZone;
                                SysATMixer(ATMixerNum).ZoneEqNum = CtrlZone;
                                SysATMixer(ATMixerNum).ZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                // Must wait until InitATMixer to fill other zone equip config data because ultimate zone inlet node is not known yet
                                // for inlet side mixers
                                if (!SysATMixer(ATMixerNum).NoOAFlowInputFromUser) {
                                    bool UseOccSchFlag = false;
                                    bool UseMinOASchFlag = false;
                                    SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(
                                        SysATMixer(ATMixerNum).OARequirementsPtr, SysATMixer(ATMixerNum).ZoneNum, UseOccSchFlag, UseMinOASchFlag);
                                }
                                goto ControlledZoneLoop_exit;
                            }
                        }
                    }
                ControlledZoneLoop_exit:;
                    if (ZoneNodeNotFound) {
                        ShowSevereError(cCurrentModuleObject + " = \"" + SysATMixer(ATMixerNum).Name +
                                        "\". Inlet Side Air Terminal Mixer air inlet node name must be the same as a zone exhaust node name.");
                        ShowContinueError("..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                        ShowContinueError("..Inlet Side CONNECTED Air Terminal Mixer inlet node name = " + NodeID(SysATMixer(ATMixerNum).SecInNode));
                        ErrorsFound = true;
                    }
                }

                if (SysATMixer(ATMixerNum).MixerType == ATMixer_SupplySide) {
                    ZoneNodeNotFound = true;
                    for (CtrlZone = 1; CtrlZone <= NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                            if (SysATMixer(ATMixerNum).MixedAirOutNode == ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                                ZoneNodeNotFound = false;
                                AirDistUnit(SysATMixer(ATMixerNum).ADUNum).ZoneEqNum = CtrlZone;
                                SysATMixer(ATMixerNum).ZoneEqNum = CtrlZone;
                                SysATMixer(ATMixerNum).ZoneNum = ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                // Wait until InitATMixer to fill other zone equip config data

                                if (!SysATMixer(ATMixerNum).NoOAFlowInputFromUser) {
                                    bool UseOccSchFlag = false;
                                    bool UseMinOASchFlag = false;
                                    SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(
                                        SysATMixer(ATMixerNum).OARequirementsPtr, SysATMixer(ATMixerNum).ZoneNum, UseOccSchFlag, UseMinOASchFlag);
                                }
                                goto ControlZoneLoop_exit;
                            }
                        }
                    }
                ControlZoneLoop_exit:;
                    if (ZoneNodeNotFound) {
                        ShowSevereError(cCurrentModuleObject + " = \"" + SysATMixer(ATMixerNum).Name +
                                        "\". Supply Side Air Terminal Mixer air outlet node name must be the same as a zone inlet node name.");
                        ShowContinueError("..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                        ShowContinueError("..Supply Side connected Air Terminal Mixer outlet node name = " +
                                          NodeID(SysATMixer(ATMixerNum).MixedAirOutNode));
                        ErrorsFound = true;
                    }
                }
            }
            TestCompSet(cCurrentModuleObject, SysATMixer(ATMixerNum).Name, cAlphaArgs(5), cAlphaArgs(4), "Air Nodes");

            if (SysATMixer(ATMixerNum).OARequirementsPtr == 0) {
                if (ZoneSizingInput.allocated()) {
                    for (int SizingInputNum = 1; SizingInputNum <= NumZoneSizingInput; ++SizingInputNum) {
                        if (ZoneSizingInput(SizingInputNum).ZoneNum == SysATMixer(ATMixerNum).ZoneNum) {
                            if (ZoneSizingInput(SizingInputNum).ZoneDesignSpecOAIndex == 0) {
                                ShowWarningError(RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid data.");
                                ShowContinueError(cAlphaFieldNames(8) + " is blank in both the mixer and the Sizing:Zone object for the same zone.");
                                ShowContinueError("The mixer outdoor airflow rate is set to zero.");
                                SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = 0.0;
                            } else {
                                SysATMixer(ATMixerNum).OARequirementsPtr = ZoneSizingInput(SizingInputNum).ZoneDesignSpecOAIndex;
                                SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(
                                    SysATMixer(ATMixerNum).OARequirementsPtr, SysATMixer(ATMixerNum).ZoneNum, false, false);
                                SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
                            }
                        }
                    }
                } else {
                    ShowWarningError(cAlphaFieldNames(8) +
                                     "is blank and there is no Sizing:Zone for the same zone. The mixer outdoor airflow rate is set to zero.");
                    SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = 0.0;
                }
            }
            SysATMixer(ATMixerNum).MassFlowRateMaxAvail = SysATMixer(ATMixerNum).DesignPrimaryAirVolRate * DataEnvironment::StdRhoAir;
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + "Errors found in input.  Program terminates.");
        }
    }

    void AirTerminalMixerData::InitATMixer(bool const FirstHVACIteration)
    {
        // Purpose: Initialize the AirTerminalMixers data structure with node data
        if (this->OneTimeInitFlag) {
            {
                auto &thisADU(DataDefineEquip::AirDistUnit(this->ADUNum));
                {
                    auto &thisZoneEqConfig(DataZoneEquipment::ZoneEquipConfig(thisADU.ZoneEqNum));
                    for (int SupAirIn = 1; SupAirIn <= thisZoneEqConfig.NumInletNodes; ++SupAirIn) {
                        if (this->ZoneInletNode == thisZoneEqConfig.InletNode(SupAirIn)) {
                            thisZoneEqConfig.AirDistUnitCool(SupAirIn).InNode = this->PriInNode;
                            thisZoneEqConfig.AirDistUnitCool(SupAirIn).OutNode = this->MixedAirOutNode;
                            thisZoneEqConfig.AirDistUnitHeat(SupAirIn).InNode = this->PriInNode;
                            thisZoneEqConfig.AirDistUnitHeat(SupAirIn).OutNode = this->MixedAirOutNode;
                            thisADU.TermUnitSizingNum = thisZoneEqConfig.AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            this->CtrlZoneInNodeIndex = SupAirIn;
                            {
                                auto &thisTermUnitSizingData(DataSizing::TermUnitSizing(thisADU.TermUnitSizingNum));
                                thisTermUnitSizingData.ADUName = thisADU.Name;
                                // Fill TermUnitSizing with specs from DesignSpecification:AirTerminal:Sizing if there is one attached to this
                                // terminal unit
                                if (thisADU.AirTerminalSizingSpecIndex > 0) {
                                    {
                                        auto const &thisAirTermSizingSpec(DataSizing::AirTerminalSizingSpec(thisADU.AirTerminalSizingSpecIndex));
                                        thisTermUnitSizingData.SpecDesCoolSATRatio = thisAirTermSizingSpec.DesCoolSATRatio;
                                        thisTermUnitSizingData.SpecDesHeatSATRatio = thisAirTermSizingSpec.DesHeatSATRatio;
                                        thisTermUnitSizingData.SpecDesSensCoolingFrac = thisAirTermSizingSpec.DesSensCoolingFrac;
                                        thisTermUnitSizingData.SpecDesSensHeatingFrac = thisAirTermSizingSpec.DesSensHeatingFrac;
                                        thisTermUnitSizingData.SpecMinOAFrac = thisAirTermSizingSpec.MinOAFrac;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            this->OneTimeInitFlag = false;
        }

        // Keep trying until we find it, the airloopnum, that is
        if (this->OneTimeInitFlag2) {
            this->AirLoopNum = DataZoneEquipment::ZoneEquipConfig(DataDefineEquip::AirDistUnit(this->ADUNum).ZoneEqNum)
                                   .InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
            DataDefineEquip::AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
            if (this->AirLoopNum > 0) {
                this->OneTimeInitFlag2 = false;
            }
        }

        // Every iteration
        Real64 mDotFromOARequirement(0.0);
        Real64 vDotOAReq(0.0);
        if (!this->NoOAFlowInputFromUser) {
            Real64 airLoopOAFrac(0.0);
            bool UseOccSchFlag = false;
            if (this->OAPerPersonMode == DataZoneEquipment::PerPersonDCVByCurrentLevel) UseOccSchFlag = true;
            if (this->AirLoopNum > 0) {
                airLoopOAFrac = DataAirLoop::AirLoopFlow(this->AirLoopNum).OAFrac;
                if (airLoopOAFrac > 0.0) {
                    vDotOAReq = DataZoneEquipment::CalcDesignSpecificationOutdoorAir(this->OARequirementsPtr, this->ZoneNum, UseOccSchFlag, true);
                    mDotFromOARequirement = vDotOAReq * DataEnvironment::StdRhoAir / airLoopOAFrac;
                } else {
                    mDotFromOARequirement = Node(this->PriInNode).MassFlowRate;
                }
            }
            if (FirstHVACIteration) {
                Node(this->PriInNode).MassFlowRate = mDotFromOARequirement;
                Node(this->PriInNode).MassFlowRateMaxAvail = this->MassFlowRateMaxAvail;
                Node(this->PriInNode).MassFlowRateMinAvail = 0.0;
            } else {
                Node(this->PriInNode).MassFlowRate = mDotFromOARequirement;

                Node(this->PriInNode).MassFlowRate = min(Node(this->PriInNode).MassFlowRate, Node(this->PriInNode).MassFlowRateMaxAvail);
                Node(this->PriInNode).MassFlowRate = max(Node(this->PriInNode).MassFlowRate, Node(this->PriInNode).MassFlowRateMinAvail);
                Node(this->PriInNode).MassFlowRate = max(Node(this->PriInNode).MassFlowRate, Node(this->PriInNode).MassFlowRateMin);
            }
        }
        if (this->MixerType == ATMixer_InletSide) {
            Node(this->PriInNode).MassFlowRate = min(Node(this->PriInNode).MassFlowRate, Node(this->MixedAirOutNode).MassFlowRate);
        }
    }

    void CalcATMixer(int const SysNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Calculate the mixed air flow and conditions in the air terminal mixer

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // Using/Aliasing
        using DataEnvironment::StdRhoAir;
        using Psychrometrics::PsyTdbFnHW;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        static Real64 PriMassFlowRate(0.0);
        static Real64 PriEnthalpy(0.0);
        static Real64 PriHumRat(0.0);
        static Real64 PriTemp(0.0);

        static Real64 SecAirMassFlowRate(0.0);
        static Real64 SecAirEnthalpy(0.0);
        static Real64 SecAirHumRat(0.0);
        static Real64 SecAirTemp(0.0);

        static Real64 MixedAirMassFlowRate(0.0);
        static Real64 MixedAirEnthalpy(0.0);
        static Real64 MixedAirHumRat(0.0);
        static Real64 MixedAirTemp(0.0);

        PriEnthalpy = Node(SysATMixer(SysNum).PriInNode).Enthalpy;
        PriHumRat = Node(SysATMixer(SysNum).PriInNode).HumRat;
        PriTemp = Node(SysATMixer(SysNum).PriInNode).Temp;
        PriMassFlowRate = Node(SysATMixer(SysNum).PriInNode).MassFlowRate;

        SecAirMassFlowRate = Node(SysATMixer(SysNum).SecInNode).MassFlowRate;
        SecAirEnthalpy = Node(SysATMixer(SysNum).SecInNode).Enthalpy;
        SecAirHumRat = Node(SysATMixer(SysNum).SecInNode).HumRat;
        SecAirTemp = Node(SysATMixer(SysNum).SecInNode).Temp;

        if (SysATMixer(SysNum).MixerType == ATMixer_SupplySide) {
            MixedAirMassFlowRate = SecAirMassFlowRate + PriMassFlowRate;
        } else {
            // for inlet side mixer, the mixed air flow has been set, but we don't know the secondary flow
            MixedAirMassFlowRate = Node(SysATMixer(SysNum).MixedAirOutNode).MassFlowRate;
            SecAirMassFlowRate = max(MixedAirMassFlowRate - PriMassFlowRate, 0.0);
            Node(SysATMixer(SysNum).SecInNode).MassFlowRate = SecAirMassFlowRate;
            if (std::abs(PriMassFlowRate + SecAirMassFlowRate - MixedAirMassFlowRate) > SmallMassFlow) {
                ShowSevereError("CalcATMixer: Invalid mass flow rates in AirTerminal:SingleDuct:Mixer=" + SysATMixer(SysNum).Name);
                ShowContinueErrorTimeStamp("Primary mass flow rate=" + General::RoundSigDigits(PriMassFlowRate, 6) +
                                           "Secondary mass flow rate=" + General::RoundSigDigits(SecAirMassFlowRate, 6) +
                                           "Mixed mass flow rate=" + General::RoundSigDigits(MixedAirMassFlowRate, 6));
                ShowFatalError("Simulation terminates.");
            }
        }
        // now calculate the mixed (outlet) conditions
        if (MixedAirMassFlowRate > 0.0) {
            MixedAirEnthalpy = (SecAirMassFlowRate * SecAirEnthalpy + PriMassFlowRate * PriEnthalpy) / MixedAirMassFlowRate;
            MixedAirHumRat = (SecAirMassFlowRate * SecAirHumRat + PriMassFlowRate * PriHumRat) / MixedAirMassFlowRate;
            // Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
            MixedAirTemp = PsyTdbFnHW(MixedAirEnthalpy, MixedAirHumRat);
        }

        SysATMixer(SysNum).MixedAirMassFlowRate = MixedAirMassFlowRate;
        SysATMixer(SysNum).MixedAirEnthalpy = MixedAirEnthalpy;
        SysATMixer(SysNum).MixedAirHumRat = MixedAirHumRat;
        SysATMixer(SysNum).MixedAirTemp = MixedAirTemp;
    }

    void UpdateATMixer(int const SysNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR
        //       DATE WRITTEN   March 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Move the results of CalcATMixer to the affected nodes

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // Using/Aliasing
        using namespace DataLoopNode;
        using DataContaminantBalance::Contaminant;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PriInNode = SysATMixer(SysNum).PriInNode;
        int SecInNode = SysATMixer(SysNum).SecInNode;
        int MixedAirOutNode = SysATMixer(SysNum).MixedAirOutNode;

        // mixed air data
        Node(MixedAirOutNode).Temp = SysATMixer(SysNum).MixedAirTemp;
        Node(MixedAirOutNode).HumRat = SysATMixer(SysNum).MixedAirHumRat;
        Node(MixedAirOutNode).Enthalpy = SysATMixer(SysNum).MixedAirEnthalpy;
        Node(MixedAirOutNode).Press = SysATMixer(SysNum).MixedAirPressure;
        Node(MixedAirOutNode).MassFlowRate = SysATMixer(SysNum).MixedAirMassFlowRate;

        if (Contaminant.CO2Simulation) {
            if (SysATMixer(SysNum).MixedAirMassFlowRate <= DataHVACGlobals::VerySmallMassFlow) {
                Node(MixedAirOutNode).CO2 = Node(PriInNode).CO2;
            } else {
                Node(MixedAirOutNode).CO2 =
                    (Node(SecInNode).MassFlowRate * Node(SecInNode).CO2 + Node(PriInNode).MassFlowRate * Node(PriInNode).CO2) /
                    Node(MixedAirOutNode).MassFlowRate;
            }
        }

        if (Contaminant.GenericContamSimulation) {
            if (SysATMixer(SysNum).MixedAirMassFlowRate <= DataHVACGlobals::VerySmallMassFlow) {
                Node(MixedAirOutNode).GenContam = Node(PriInNode).GenContam;
            } else {
                Node(MixedAirOutNode).GenContam =
                    (Node(SecInNode).MassFlowRate * Node(SecInNode).GenContam + Node(PriInNode).MassFlowRate * Node(PriInNode).GenContam) /
                    Node(MixedAirOutNode).MassFlowRate;
            }
        }

        // update ADU flow data - because SimATMixer is called from the various zone equipment so the updates in SimZoneAirLoopEquipment won't work
        int aduNum = SysATMixer(SysNum).ADUNum;
        DataDefineEquip::AirDistUnit(aduNum).MassFlowRateTU = Node(PriInNode).MassFlowRate;
        DataDefineEquip::AirDistUnit(aduNum).MassFlowRateZSup = Node(PriInNode).MassFlowRate;
        DataDefineEquip::AirDistUnit(aduNum).MassFlowRateSup = Node(PriInNode).MassFlowRate;
    }

    void GetATMixer(ZoneAirLoopEquipmentManagerData &dataZoneAirLoopEquipmentManager, std::string const &ZoneEquipName, // zone unit name name
                    std::string &ATMixerName,         // air terminal mixer name
                    int &ATMixerNum,                  // air terminal mixer index
                    int &ATMixerType,                 // air teminal mixer type
                    int &ATMixerPriNode,              // air terminal mixer primary air node number
                    int &ATMixerSecNode,              // air terminal mixer secondary air node number
                    int &ATMixerOutNode,              // air terminal mixer outlet air node number
                    int const &ZoneEquipOutletNode    // zone equipment outlet node (used with inlet side mixers)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets: 1) the index of the named AT Mixer in the SysATMixer data array
        //                       2) the node number of the primary air inlet node of the AT Mixer
        //                       3) set the AT Mixer ultimate zone inlet node

        int ATMixerIndex; // local air terminal mixer index

        if (GetATMixerFlag) {
            // CALL GetZoneAirLoopEquipment
            GetATMixers(dataZoneAirLoopEquipmentManager);
            GetATMixerFlag = false;
        }

        if (NumATMixers <= 0) {
            ATMixerNum = 0;
            ATMixerName = "";
            ATMixerPriNode = 0;
            ATMixerSecNode = 0;
            ATMixerOutNode = 0;
            ATMixerType = 0;
            return;
        }

        ATMixerIndex = UtilityRoutines::FindItemInList(ZoneEquipName, SysATMixer, &AirTerminalMixerData::ZoneHVACUnitName);
        if (ATMixerIndex > 0) {
            ATMixerNum = ATMixerIndex;
            ATMixerName = SysATMixer(ATMixerIndex).Name;
            ATMixerPriNode = SysATMixer(ATMixerIndex).PriInNode;
            ATMixerSecNode = SysATMixer(ATMixerIndex).SecInNode;
            ATMixerOutNode = SysATMixer(ATMixerIndex).MixedAirOutNode;
            ATMixerType = SysATMixer(ATMixerIndex).MixerType;
            if (ATMixerType == ATMixer_InletSide) {
                SysATMixer(ATMixerIndex).ZoneInletNode = ZoneEquipOutletNode;
            } else {
                SysATMixer(ATMixerIndex).ZoneInletNode = ATMixerOutNode;
            }
            SysATMixer(ATMixerNum).InitATMixer(false);
        } else {
            ATMixerNum = 0;
            ATMixerName = "";
            ATMixerPriNode = 0;
            ATMixerSecNode = 0;
            ATMixerOutNode = 0;
            ATMixerType = 0;
        }
    }

    void SetATMixerPriFlow(int const ATMixerNum,                     // Air terminal mixer index
                           Optional<Real64 const> PriAirMassFlowRate // Air terminal mixer primary air mass flow rate [kg/s]
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   April 2012
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This Subroutine sets the primary air mass flow rate on the primary air inlet
        // node of a terminal unit mixer component.

        // METHODOLOGY EMPLOYED:
        // The flow is set to either the input PriAirMassFlowRate if this optional input
        // parameter is present, or to the maximum available mass flow rate of the primary
        // air inlet node.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PriAirNode; // air terminal mixer primary air inlet node number

        if (ATMixerNum <= 0) return;
        PriAirNode = SysATMixer(ATMixerNum).PriInNode;
        if (present(PriAirMassFlowRate)) {
            Node(PriAirNode).MassFlowRate = PriAirMassFlowRate;
        } else {
            Node(PriAirNode).MassFlowRate = Node(PriAirNode).MassFlowRateMaxAvail;
        }
    }

    void setATMixerSizingProperties(int const &inletATMixerIndex, // index to ATMixer at inlet of zone equipment
                                    int const &controlledZoneNum, // controlled zone number
                                    int const &curZoneEqNum       // current zone equipment being simulated
    )
    {
        if (inletATMixerIndex == 0) return; // protect this function from bad inputs
        if (controlledZoneNum == 0) return;
        if (curZoneEqNum == 0) return;
        if (SingleDuct::SysATMixer(inletATMixerIndex).MixerType == DataHVACGlobals::No_ATMixer) return;

        // ATMixer properties only affect coil sizing when the mixer is on the inlet side of zone equipment
        if (SingleDuct::SysATMixer(inletATMixerIndex).MixerType == DataHVACGlobals::ATMixer_SupplySide) {
            // check if user has selected No to account for DOAS system
            if (FinalZoneSizing.allocated() && SingleDuct::SysATMixer(inletATMixerIndex).printWarning) {
                if (!FinalZoneSizing(curZoneEqNum).AccountForDOAS && FinalZoneSizing(curZoneEqNum).DOASControlStrategy != DOANeutralSup) {
                    ShowWarningError("AirTerminal:SingleDuct:Mixer: " + SingleDuct::SysATMixer(inletATMixerIndex).Name);
                    ShowContinueError(
                        " Supply side Air Terminal Mixer does not adjust zone equipment coil sizing and may result in inappropriately sized coils.");
                    ShowContinueError(" Set Account for Dedicated Outdoor Air System = Yes in Sizing:Zone object for zone = " +
                                      FinalZoneSizing(curZoneEqNum).ZoneName);
                }
                SingleDuct::SysATMixer(inletATMixerIndex).printWarning = false;
            }
            return; // do nothing else if this is a supply side ATMixer
        }
        // check if user has selected Yes to account for DOAS system
        if (FinalZoneSizing.allocated() && SingleDuct::SysATMixer(inletATMixerIndex).printWarning) {
            if (FinalZoneSizing(curZoneEqNum).AccountForDOAS && FinalZoneSizing(curZoneEqNum).DOASControlStrategy != DOANeutralSup) {
                ShowWarningError("AirTerminal:SingleDuct:Mixer: " + SingleDuct::SysATMixer(inletATMixerIndex).Name);
                ShowContinueError(" Inlet side Air Terminal Mixer automatically adjusts zone equipment coil sizing.");
                ShowContinueError(" Set Account for Dedicated Outdoor Air System = No in Sizing:Zone object for zone = " +
                                  FinalZoneSizing(curZoneEqNum).ZoneName);
                SingleDuct::SysATMixer(inletATMixerIndex).printWarning = false;
            }
        }

        // proceed to set ATMixer properties used for sizing coils

        int airLoopIndex = // find air loop associated with ATMixer
            DataZoneEquipment::ZoneEquipConfig(controlledZoneNum).InletNodeAirLoopNum(SingleDuct::SysATMixer(inletATMixerIndex).CtrlZoneInNodeIndex);

        // must be a system sizing run or calculations are not possible
        bool SizingDesRunThisAirSys = false;                               // Sizing:System object found flag
        CheckThisAirSystemForSizing(airLoopIndex, SizingDesRunThisAirSys); // check for Sizing:System object

        if (SizingDesRunThisAirSys) {

            // set ATMixer outlet air flow rate in ZoneEqSizing array for ATMixer. If this value > 0, then RequestSizing will know an ATMixer exists
            ZoneEqSizing(curZoneEqNum).ATMixerVolFlow = SingleDuct::SysATMixer(inletATMixerIndex).DesignPrimaryAirVolRate;

            // If air loop has heating coil use SA conditions, else if OA sys has coils then use precool conditions, else use OA conditions
            if (DataAirSystems::PrimaryAirSystem(airLoopIndex).CentralHeatCoilExists) {
                // if central heating coil exists, ATMixer outlet is assumed to be at supply air conditions described in sizing input
                ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = FinalSysSizing(airLoopIndex).HeatSupTemp;
                ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = FinalSysSizing(airLoopIndex).HeatSupHumRat;
            } else if (DataAirSystems::PrimaryAirSystem(airLoopIndex).NumOAHeatCoils > 0) {
                // if no central heating coil exists and an outdoor air coil does exist, then ATMixer outlet is mixture of preheat and return
                if (FinalSysSizing(airLoopIndex).DesMainVolFlow == 0.0) { // protect divide by 0
                    // doesn't matter, just pick a condition
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = FinalSysSizing(airLoopIndex).PreheatTemp;
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = FinalSysSizing(airLoopIndex).PreheatHumRat;
                } else {
                    // mix preheat condition with return air condition based on OA frac. OA frac should nearly always be 1.
                    // OA frac is based on air loop fraction, not ATMixer flow fraction since air loop can serve multiple ATMixers
                    Real64 OutAirFrac = FinalSysSizing(airLoopIndex).DesOutAirVolFlow / FinalSysSizing(airLoopIndex).DesMainVolFlow;
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                    // calculate humrat based on simple mixing
                    Real64 CoilInHumRatForSizing =
                        OutAirFrac * FinalSysSizing(airLoopIndex).PreheatHumRat + (1 - OutAirFrac) * FinalSysSizing(airLoopIndex).HeatRetHumRat;

                    // calculate enthalpy based on simple mixing
                    Real64 CoilInEnthalpyForSizing = OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).PreheatTemp,
                                                                                             FinalSysSizing(airLoopIndex).PreheatHumRat) +
                                                     (1 - OutAirFrac) * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatRetTemp,
                                                                                                   FinalSysSizing(airLoopIndex).HeatRetHumRat);

                    // back calculate temperature based on humrat and enthalpy state points
                    Real64 CoilInTempForSizing = Psychrometrics::PsyTdbFnHW(CoilInEnthalpyForSizing, CoilInHumRatForSizing);

                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = CoilInTempForSizing;
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = CoilInHumRatForSizing;
                }
            } else {
                // else no coils exist in air loop so mix OA condition with return air condition
                if (FinalSysSizing(airLoopIndex).DesMainVolFlow == 0.0) { // protect divide by 0
                    // doesn't matter, just pick a condition
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = FinalSysSizing(airLoopIndex).HeatOutTemp;
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = FinalSysSizing(airLoopIndex).HeatOutHumRat;
                } else {
                    // OA frac is based on air loop fraction, not ATMixer flow fraction since air loop can serve multiple ATMixers
                    Real64 OutAirFrac = FinalSysSizing(airLoopIndex).DesOutAirVolFlow / FinalSysSizing(airLoopIndex).DesMainVolFlow;
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                    // calculate humrat based on simple mixing
                    Real64 CoilInHumRatForSizing =
                        OutAirFrac * FinalSysSizing(airLoopIndex).HeatOutHumRat + (1 - OutAirFrac) * FinalSysSizing(airLoopIndex).HeatRetHumRat;

                    // calculate enthalpy based on simple mixing
                    Real64 CoilInEnthalpyForSizing = OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatOutTemp,
                                                                                             FinalSysSizing(airLoopIndex).HeatOutHumRat) +
                                                     (1 - OutAirFrac) * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatRetTemp,
                                                                                                   FinalSysSizing(airLoopIndex).HeatRetHumRat);

                    // back calculate temperature based on humrat and enthalpy state points
                    Real64 CoilInTempForSizing = Psychrometrics::PsyTdbFnHW(CoilInEnthalpyForSizing, CoilInHumRatForSizing);

                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = CoilInTempForSizing;
                    ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = CoilInHumRatForSizing;
                }
            }

            // If air loop has cooling coil use SA conditions, else if OA sys has coils then use precool conditions, else use OA conditions
            if (DataAirSystems::PrimaryAirSystem(airLoopIndex).CentralCoolCoilExists) {
                // if central cooling coil exists, ATMixer outlet is assumed to be at supply air conditions described in sizing input
                ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = FinalSysSizing(airLoopIndex).CoolSupTemp;
                ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = FinalSysSizing(airLoopIndex).CoolSupHumRat;
            } else if (DataAirSystems::PrimaryAirSystem(airLoopIndex).NumOACoolCoils > 0) {
                // if no central cooling coil exists and an outdoor air coil does exist, then ATMixer outlet is mixture of precool and return
                if (FinalSysSizing(airLoopIndex).DesMainVolFlow == 0.0) { // protect divide by 0
                    // doesn't matter, just pick a condition
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = FinalSysSizing(airLoopIndex).PrecoolTemp;
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = FinalSysSizing(airLoopIndex).PrecoolHumRat;
                } else {
                    // mix precool condition with return air condition based on OA frac. OA frac should nearly always be 1.
                    // OA frac is based on air loop fraction, not ATMixer flow fraction since air loop can serve multiple ATMixers
                    Real64 OutAirFrac = FinalSysSizing(airLoopIndex).DesOutAirVolFlow / FinalSysSizing(airLoopIndex).DesMainVolFlow;
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                    // calculate humrat based on simple mixing
                    Real64 CoilInHumRatForSizing =
                        OutAirFrac * FinalSysSizing(airLoopIndex).PrecoolHumRat + (1 - OutAirFrac) * FinalSysSizing(airLoopIndex).RetHumRatAtCoolPeak;

                    // calculate enthalpy based on simple mixing
                    Real64 CoilInEnthalpyForSizing = OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).PrecoolTemp,
                                                                                             FinalSysSizing(airLoopIndex).PrecoolHumRat) +
                                                     (1 - OutAirFrac) * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).RetTempAtCoolPeak,
                                                                                                   FinalSysSizing(airLoopIndex).RetHumRatAtCoolPeak);

                    // back calculate temperature based on humrat and enthalpy state points
                    Real64 CoilInTempForSizing = Psychrometrics::PsyTdbFnHW(CoilInEnthalpyForSizing, CoilInHumRatForSizing);

                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = CoilInTempForSizing;
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = CoilInHumRatForSizing;
                }
            } else {
                // else no coils exist in air loop so mix OA condition with return air condition
                if (FinalSysSizing(airLoopIndex).DesMainVolFlow == 0.0) { // protect divide by 0
                    // doesn't matter, just pick a condition
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = FinalSysSizing(airLoopIndex).OutTempAtCoolPeak;
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = FinalSysSizing(airLoopIndex).OutHumRatAtCoolPeak;
                } else {
                    // OA frac is based on air loop fraction, not ATMixer flow fraction since air loop can serve multiple ATMixers
                    Real64 OutAirFrac = FinalSysSizing(airLoopIndex).DesOutAirVolFlow / FinalSysSizing(airLoopIndex).DesMainVolFlow;
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                    // calculate humrat based on simple mixing
                    Real64 CoilInHumRatForSizing = OutAirFrac * FinalSysSizing(airLoopIndex).OutHumRatAtCoolPeak +
                                                   (1 - OutAirFrac) * FinalSysSizing(airLoopIndex).RetHumRatAtCoolPeak;

                    // calculate enthalpy based on simple mixing
                    Real64 CoilInEnthalpyForSizing = OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).OutTempAtCoolPeak,
                                                                                             FinalSysSizing(airLoopIndex).OutHumRatAtCoolPeak) +
                                                     (1 - OutAirFrac) * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).RetTempAtCoolPeak,
                                                                                                   FinalSysSizing(airLoopIndex).RetHumRatAtCoolPeak);

                    // back calculate temperature based on humrat and enthalpy state points
                    Real64 CoilInTempForSizing = Psychrometrics::PsyTdbFnHW(CoilInEnthalpyForSizing, CoilInHumRatForSizing);

                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = CoilInTempForSizing;
                    ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = CoilInHumRatForSizing;
                }
            }

        } else {
            // warn user that system sizing is needed to size coils when AT Mixer is used ?
            // if there were a message here then this function should only be called when SizingDesRunThisZone is true
        }
    }

    void SingleDuctAirTerminal::CalcOutdoorAirVolumeFlowRate()
    {
        // calculates zone outdoor air volume flow rate using the supply air flow rate and OA fraction
        if (this->AirLoopNum > 0) {
            this->OutdoorAirFlowRate = (this->sd_airterminalOutlet.AirMassFlowRate / StdRhoAir) * DataAirLoop::AirLoopFlow(this->AirLoopNum).OAFrac;
        } else {
            this->OutdoorAirFlowRate = 0.0;
        }
    }

    //        End of Reporting subroutines for the Sys Module
    // *****************************************************************************

} // namespace SingleDuct

} // namespace EnergyPlus
