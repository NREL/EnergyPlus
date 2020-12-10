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
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UnitVentilator.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace UnitVentilator {

    // Module containing the routines dealing with the Unit Ventilator

    // MODULE INFORMATION:
    //       AUTHOR         Rick Strand
    //       DATE WRITTEN   May 2000
    //       MODIFIED       March 2001   (addition of gas and electric coils)
    //                      October 2003 (addition of cooling coil type)
    //       MODIFIED       Bereket Nigusse, FSEC, October 2013, Added cycling fan operating mode
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To simulate unit ventilators.

    // METHODOLOGY EMPLOYED:
    // Units are modeled as a collection of components: outside air mixer,
    // fan, heating coil and/or cooling coil plus an integrated control
    // algorithm that adjusts the hot or cold water flow to meet the zone
    // load.  Outside air mixing is handled locally as either fixed percent
    // or as attempting to meet a prescribed mixed air temperature.

    // REFERENCES:
    // ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
    // Fred Buhl's fan coil module (FanCoilUnits.cc)

    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::ATMixer_InletSide;
    using DataHVACGlobals::ATMixer_SupplySide;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;

    using namespace ScheduleManager;
    using namespace Psychrometrics;
    using namespace FluidProperties;

    static std::string const fluidNameSteam("STEAM");
    static std::string const fluidNameWater("WATER");

    void SimUnitVentilator(std::string const &CompName,   // name of the fan coil unit
                           int const ZoneNum,             // number of zone being served
                           bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           Real64 &PowerMet,              // Sensible power supplied (W)
                           Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                           int &CompIndex)
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This is the main driver subroutine for the Unit Ventilator simulation.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        using DataSizing::ZoneEqUnitVent;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UnitVentNum; // index of unit ventilator being simulated

        // FLOW:
        if (state.dataUnitVentilators->GetUnitVentilatorInputFlag) {
            GetUnitVentilatorInput();
            state.dataUnitVentilators->GetUnitVentilatorInputFlag = false;
        }

        // Find the correct Unit Ventilator Equipment
        if (CompIndex == 0) {
            UnitVentNum = UtilityRoutines::FindItemInList(CompName, state.dataUnitVentilators->UnitVent);
            if (UnitVentNum == 0) {
                ShowFatalError("SimUnitVentilator: Unit not found=" + CompName);
            }
            CompIndex = UnitVentNum;
        } else {
            UnitVentNum = CompIndex;
            if (UnitVentNum > state.dataUnitVentilators->NumOfUnitVents || UnitVentNum < 1) {
                ShowFatalError(format("SimUnitVentilator:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      UnitVentNum,
                                      state.dataUnitVentilators->NumOfUnitVents,
                                      CompName));
            }
            if (state.dataUnitVentilators->CheckEquipName(UnitVentNum)) {
                if (CompName != state.dataUnitVentilators->UnitVent(UnitVentNum).Name) {
                    ShowFatalError(format("SimUnitVentilator: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          UnitVentNum,
                                          CompName,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).Name));
                }
                state.dataUnitVentilators->CheckEquipName(UnitVentNum) = false;
            }
        }

        ZoneEqUnitVent = true;

        InitUnitVentilator(UnitVentNum, FirstHVACIteration, ZoneNum);

        CalcUnitVentilator(UnitVentNum, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided);

        //  CALL UpdateUnitVentilator

        ReportUnitVentilator(UnitVentNum);

        ZoneEqUnitVent = false;
    }

    void GetUnitVentilatorInput()
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
        //                      Bereket Nigusse, FSEC, April 2011: eliminated input node names
        //                                                         & added fan object type
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine obtains the input for unit ventilators and sets
        // up the appropriate derived type.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // REFERENCES:
        // Fred Buhl's fan coil module (FanCoilUnits.cc)

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using NodeInputManager::GetOnlySingleNode;
        using OutAirNodeManager::CheckAndAddAirNodeNumber;
        auto &GetWaterCoilMaxFlowRate(WaterCoils::GetCoilMaxWaterFlowRate);
        using WaterCoils::GetCoilWaterInletNode;
        auto &GetSteamCoilMaxFlowRate(SteamCoils::GetCoilMaxWaterFlowRate);
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetSteamCoilIndex;
        auto &GetHXAssistedCoilFlowRate(HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate);
        auto &GetHXCoilWaterInletNode(HVACHXAssistedCoolingCoil::GetCoilWaterInletNode);
        using DataHVACGlobals::FanType_SimpleConstVolume;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::FanType_SimpleVAV;
        using Fans::GetFanAvailSchPtr;
        using Fans::GetFanIndex;
        using Fans::GetFanOutletNode;
        using Fans::GetFanType;
        using Fans::GetFanVolFlow;
        using HVACHXAssistedCoolingCoil::GetHXCoilTypeAndName;

        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataSizing::AutoSize;
        using DataSizing::ZoneHVACSizing;
        using DataZoneEquipment::ZoneEquipConfig;

        using SingleDuct::GetATMixer;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetUnitVentilatorInput: "); // include trailing blank

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;                   // Used in GetObjectItem
        bool IsNotOK;                   // TRUE if there was a problem with a list name
        int NumFields;                  // Total number of fields in object
        int NumAlphas;                  // Number of Alphas for each GetObjectItem call
        int NumNumbers;                 // Number of Numbers for each GetObjectItem call
        int UnitVentNum;                // Item to be "gotten"
        bool IsValid;                   // Set for outside air node check
        static bool errFlag(false);     // interim error flag
        std::string cCoolingCoilType;   // Cooling coil object type
        std::string cHeatingCoilType;   // Heating coil object type
        int FanIndex;                   // index to fan used for flow checks
        Real64 FanVolFlow;              // volumetric flow rate of fan
        std::string CurrentModuleObject;
        Array1D_string Alphas;         // Alpha items for object
        Array1D<Real64> Numbers;       // Numeric items for object
        Array1D_string cAlphaFields;   // Alpha field names
        Array1D_string cNumericFields; // Numeric field names
        Array1D_bool lAlphaBlanks;     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
        int CtrlZone;                  // index to loop counter
        int NodeNum;                   // index to loop counter
        bool ZoneNodeNotFound;         // used in error checking

        // FLOW:

        // Figure out how many unit ventilators there are in the input file

        CurrentModuleObject = state.dataUnitVentilators->cMO_UnitVentilator;
        state.dataUnitVentilators->NumOfUnitVents = inputProcessor->getNumObjectsFound(CurrentModuleObject);
        inputProcessor->getObjectDefMaxArgs(CurrentModuleObject, NumFields, NumAlphas, NumNumbers);

        Alphas.allocate(NumAlphas);
        Numbers.dimension(NumNumbers, 0.0);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        // Allocate the local derived type and do one-time initializations for all parts of it
        if (state.dataUnitVentilators->NumOfUnitVents > 0) {
            state.dataUnitVentilators->UnitVent.allocate(state.dataUnitVentilators->NumOfUnitVents);
            state.dataUnitVentilators->CheckEquipName.allocate(state.dataUnitVentilators->NumOfUnitVents);
            state.dataUnitVentilators->UnitVentNumericFields.allocate(state.dataUnitVentilators->NumOfUnitVents);
        }
        state.dataUnitVentilators->CheckEquipName = true;

        for (UnitVentNum = 1; UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents;
             ++UnitVentNum) { // Begin looping over all of the unit ventilators found in the input file...

            inputProcessor->getObjectItem(CurrentModuleObject,
                                          UnitVentNum,
                                          Alphas,
                                          NumAlphas,
                                          Numbers,
                                          NumNumbers,
                                          IOStatus,
                                          lNumericBlanks,
                                          lAlphaBlanks,
                                          cAlphaFields,
                                          cNumericFields);

            state.dataUnitVentilators->UnitVentNumericFields(UnitVentNum).FieldNames.allocate(NumNumbers);
            state.dataUnitVentilators->UnitVentNumericFields(UnitVentNum).FieldNames = "";
            state.dataUnitVentilators->UnitVentNumericFields(UnitVentNum).FieldNames = cNumericFields;
            UtilityRoutines::IsNameEmpty(Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataUnitVentilators->UnitVent(UnitVentNum).Name = Alphas(1);
            state.dataUnitVentilators->UnitVent(UnitVentNum).SchedName = Alphas(2);
            if (lAlphaBlanks(2)) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataUnitVentilators->UnitVent(UnitVentNum).SchedPtr = GetScheduleIndex(Alphas(2)); // convert schedule name to pointer
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).SchedPtr == 0) {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid");
                    ShowContinueError("not found: " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                    ErrorsFound = true;
                }
            }

            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow = Numbers(1);

            // Outside air information:
            state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow = Numbers(2);

            state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedName = Alphas(4);
            state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedPtr = GetScheduleIndex(Alphas(4)); // convert schedule name to pointer
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedPtr == 0) {
                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                ShowContinueError("not found: " + cAlphaFields(4) + "=\"" + Alphas(4) + "\".");
                ErrorsFound = true;
            }

            state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow = Numbers(3);
            cCoolingCoilType = "";
            cHeatingCoilType = "";

            {
                auto const SELECT_CASE_var(Alphas(3));
                if (SELECT_CASE_var == "VARIABLEPERCENT") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType = state.dataUnitVentilators->VariablePercent;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName = Alphas(5);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr = GetScheduleIndex(Alphas(5)); // convert schedule name to pointer
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                        ShowContinueError("not found:" + cAlphaFields(5) + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName + "\".");
                        ErrorsFound = true;
                    } else if (!CheckScheduleValueMinMax(state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr, ">=0", 0.0, "<=", 1.0)) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                        ShowContinueError("out of range [0,1]: " + cAlphaFields(5) + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName + "\".");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "FIXEDAMOUNT") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType = state.dataUnitVentilators->FixedOAControl;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName = Alphas(5);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr = GetScheduleIndex(Alphas(5)); // convert schedule name to pointer
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr == 0) {
                        ShowSevereError(cAlphaFields(5) + " not found = " + state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName);
                        ShowContinueError("Occurs in " + CurrentModuleObject + " = " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                        ErrorsFound = true;
                    } else if (!CheckScheduleValueMinMax(state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr, ">=0", 0.0)) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                        ShowContinueError("out of range [0,1]: " + cAlphaFields(5) + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName + "\".");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "FIXEDTEMPERATURE") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType = state.dataUnitVentilators->FixedTemperature;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedName = Alphas(5);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr = GetScheduleIndex(Alphas(5)); // convert schedule name to pointer
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr == 0) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                        ShowContinueError(" not found: " + cAlphaFields(5) + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                    ShowContinueError("Illegal " + cAlphaFields(3) + "=\"" + Alphas(3) + "\".");
                }
            }

            // Main air nodes (except outside air node):
            // For node connections, this object is both a parent and a non-parent, because the
            // OA mixing box is not called out as a separate component, its nodes must be connected
            // as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
            // To support the diagramming tool, the unit ventilator inlet node must appear both as
            // an inlet to the unit ventilator parent object and as an inlet to the implied
            // non-parent OA mixing box within the unit ventilator.
            // Because there is overlap between the nodes that are parent and non-parent, use a different
            // object type for the non parent nodes
            state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode =
                GetOnlySingleNode(Alphas(6), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsParent);
            state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode =
                GetOnlySingleNode(Alphas(7), ErrorsFound, CurrentModuleObject, Alphas(1), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent);

            // Get AirTerminal mixer data
            GetATMixer(state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerName,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerPriNode,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerSecNode,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode,
                       state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode);
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide || state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_SupplySide) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists = true;
            }

            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode = GetOnlySingleNode(Alphas(6),
                                                                    ErrorsFound,
                                                                    CurrentModuleObject + "-OA MIXER",
                                                                    Alphas(1),
                                                                    NodeType_Air,
                                                                    NodeConnectionType_Inlet,
                                                                    1,
                                                                    ObjectIsNotParent);
            }

            state.dataUnitVentilators->UnitVent(UnitVentNum).FanType = Alphas(11);
            state.dataUnitVentilators->UnitVent(UnitVentNum).FanName = Alphas(12);
            errFlag = false;
            ValidateComponent(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType, state.dataUnitVentilators->UnitVent(UnitVentNum).FanName, errFlag, CurrentModuleObject);
            if (errFlag) {
                ShowContinueError("specified in " + CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                ErrorsFound = true;
            } else {
                if (!UtilityRoutines::SameString(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType, "Fan:SystemModel")) {
                    GetFanType(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                               state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num,
                               errFlag,
                               CurrentModuleObject,
                               state.dataUnitVentilators->UnitVent(UnitVentNum).Name);

                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num);
                        if ((SELECT_CASE_var == FanType_SimpleConstVolume) || (SELECT_CASE_var == FanType_SimpleVAV) ||
                            (SELECT_CASE_var == FanType_SimpleOnOff)) {

                            // Get fan outlet node
                            state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode =
                                GetFanOutletNode(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType, state.dataUnitVentilators->UnitVent(UnitVentNum).FanName, errFlag);
                            if (errFlag) {
                                ShowContinueError("specified in " + CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                                ErrorsFound = true;
                            } else {
                                GetFanIndex(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName, FanIndex, errFlag, CurrentModuleObject);
                                // Other error checks should trap before it gets to this point in the code, but including just in case.

                                GetFanVolFlow(FanIndex, FanVolFlow);
                                if (FanVolFlow != AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow != AutoSize &&
                                    FanVolFlow < state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow) {
                                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                                    ShowContinueError(format("...air flow rate [{:.7T}] in fan object {} is less than the unit ventilator maximum "
                                                             "supply air flow rate [{:.7T}].",
                                                             FanVolFlow,
                                                             state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                             state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow));
                                    ShowContinueError("...the fan flow rate must be greater than or equal to the unit ventilator maximum supply air flow rate.");
                                    ErrorsFound = true;
                                } else if (FanVolFlow == AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow != AutoSize) {
                                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                                    ShowContinueError("...the fan flow rate is autosized while the unit ventilator flow rate is not.");
                                    ShowContinueError("...this can lead to unexpected results where the fan flow rate is less than required.");
                                } else if (FanVolFlow != AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow == AutoSize) {
                                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                                    ShowContinueError("...the unit ventilator flow rate is autosized while the fan flow rate is not.");
                                    ShowContinueError("...this can lead to unexpected results where the fan flow rate is less than required.");
                                }
                                // Get the fan's availability schedule
                                errFlag = false;
                                state.dataUnitVentilators->UnitVent(UnitVentNum).FanAvailSchedPtr =
                                    GetFanAvailSchPtr(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType, state.dataUnitVentilators->UnitVent(UnitVentNum).FanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                                    ErrorsFound = true;
                                }
                            }
                        } else {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                            ShowContinueError("Fan Type must be Fan:ConstantVolume or Fan:VariableVolume.");
                            ErrorsFound = true;
                        }
                    }
                } else if (UtilityRoutines::SameString(state.dataUnitVentilators->UnitVent(UnitVentNum).FanType, "Fan:SystemModel")) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                    HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName));       // call constructor
                    state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index = HVACFan::getFanObjectVectorIndex(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName); // zero-based
                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode = HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->outletNodeNum;
                    FanVolFlow = HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->designAirVolFlowRate;
                    if (FanVolFlow != AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow != AutoSize &&
                        FanVolFlow < state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow) {
                        ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                        ShowContinueError(
                            format(
                                "...air flow rate [{:.7T}] in fan object {} is less than the unit ventilator maximum supply air flow rate [{:.7T}].",
                                FanVolFlow,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow));
                        ShowContinueError("...the fan flow rate must be greater than or equal to the unit ventilator maximum supply air flow rate.");
                        ErrorsFound = true;
                    } else if (FanVolFlow == AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow != AutoSize) {
                        ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                        ShowContinueError("...the fan flow rate is autosized while the unit ventilator flow rate is not.");
                        ShowContinueError("...this can lead to unexpected results where the fan flow rate is less than required.");
                    } else if (FanVolFlow != AutoSize && state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow == AutoSize) {
                        ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\"");
                        ShowContinueError("...the unit ventilator flow rate is autosized while the fan flow rate is not.");
                        ShowContinueError("...this can lead to unexpected results where the fan flow rate is less than required.");
                    }
                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanAvailSchedPtr = HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->availSchedIndex;
                }
            }
            // For node connections, this object is both a parent and a non-parent, because the
            // OA mixing box is not called out as a separate component, its nodes must be connected
            // as ObjectIsNotParent.  But for the fan and coils, the nodes are connected as ObjectIsParent
            // Because there is overlap between the nodes that are parent and non-parent, use a different
            // object type for the non parent nodes
            //  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode = GetOnlySingleNode(Alphas(8),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject + "-OA MIXER",
                                                                         Alphas(1),
                                                                         NodeType_Air,
                                                                         NodeConnectionType_OutsideAirReference,
                                                                         1,
                                                                         ObjectIsNotParent);
                if (!lAlphaBlanks(8)) {
                    CheckAndAddAirNodeNumber(state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode, IsValid);
                    if (!IsValid) {
                        ShowWarningError(RoutineName + CurrentModuleObject + ", Adding " + cAlphaFields(8) + '=' + Alphas(8));
                    }
                }

                state.dataUnitVentilators->UnitVent(UnitVentNum).AirReliefNode = GetOnlySingleNode(Alphas(9),
                                                                        ErrorsFound,
                                                                        CurrentModuleObject + "-OA MIXER",
                                                                        Alphas(1),
                                                                        NodeType_Air,
                                                                        NodeConnectionType_ReliefAir,
                                                                        1,
                                                                        ObjectIsNotParent);

                state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode = GetOnlySingleNode(Alphas(10),
                                                                         ErrorsFound,
                                                                         CurrentModuleObject + "-OA MIXER",
                                                                         Alphas(1),
                                                                         NodeType_Air,
                                                                         NodeConnectionType_Outlet,
                                                                         1,
                                                                         ObjectIsNotParent);
            } else {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerPriNode;
                state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode;
                if (!lAlphaBlanks(8) || !lAlphaBlanks(9) || !lAlphaBlanks(10)) {
                    ShowWarningError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\" is connected to central DOA.");
                    if (!lAlphaBlanks(8)) {
                        ShowContinueError("... input field " + cAlphaFields(8) + " should have been blank. Specified = " + Alphas(8));
                    }
                    if (!lAlphaBlanks(9)) {
                        ShowContinueError("... input field " + cAlphaFields(9) + " should have been blank. Specified = " + Alphas(9));
                    }
                    if (!lAlphaBlanks(10)) {
                        ShowContinueError("... input field " + cAlphaFields(10) + " should have been blank. Specified = " + Alphas(10));
                    }
                }
            }

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType == state.dataUnitVentilators->FixedOAControl) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow;
                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedName = state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedName;
                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr = GetScheduleIndex(state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedName);
            }

            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                // Add fan to component sets array
                SetUpCompSets(CurrentModuleObject,
                              state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                              state.dataUnitVentilators->UnitVent(UnitVentNum).FanType,
                              state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                              NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode),
                              NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode));
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide) {
                    // Add fan to component sets array
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).FanType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode),
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode));
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_SupplySide) {
                    // Add fan to component sets array
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).FanType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode),
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode));
                }
            }

            if (!lAlphaBlanks(18)) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).AvailManagerListName = Alphas(18);
            }

            state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex = 0;
            if (!lAlphaBlanks(20)) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(20), ZoneHVACSizing);
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex == 0) {
                    ShowSevereError(cAlphaFields(20) + " = " + Alphas(20) + " not found.");
                    ShowContinueError("Occurs in " + state.dataUnitVentilators->cMO_UnitVentilator + " = " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                    ErrorsFound = true;
                }
            }
            //   A13, \field Coil Option
            //        \required-field
            //        \type choice
            //        \key None
            //        \key Heating
            //        \key Cooling
            //        \key HeatingAndCooling

            {
                auto const SELECT_CASE_var(Alphas(13));
                if (SELECT_CASE_var == "HEATINGANDCOOLING") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption = state.dataUnitVentilators->BothOption;
                } else if (SELECT_CASE_var == "HEATING") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption = state.dataUnitVentilators->HeatingOption;
                } else if (SELECT_CASE_var == "COOLING") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption = state.dataUnitVentilators->CoolingOption;
                } else if (SELECT_CASE_var == "NONE") {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption = state.dataUnitVentilators->NoneOption;
                } else {
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                    ShowContinueError("illegal value: " + cAlphaFields(13) + "=\"" + Alphas(13) + "\".");
                    ErrorsFound = true;
                }
            }

            state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr = GetScheduleIndex(Alphas(14));
            // Default to cycling fan when fan mode schedule is not present
            if (!lAlphaBlanks(14) && state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr == 0) {
                ShowSevereError(CurrentModuleObject + " \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\" " + cAlphaFields(14) + " not found: " + Alphas(14));
                ErrorsFound = true;
            } else if (lAlphaBlanks(14)) {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == FanType_SimpleOnOff ||
                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OpMode = CycFanCycCoil;
                } else {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OpMode = ContFanCycCoil;
                }
            }

            // Check fan's schedule for cycling fan operation if constant volume fan is used
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr > 0 && state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == FanType_SimpleConstVolume) {
                if (!CheckScheduleValueMinMax(state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr, ">", 0.0, "<=", 1.0)) {
                    ShowSevereError(CurrentModuleObject + " = " + Alphas(1));
                    ShowContinueError("For " + cAlphaFields(11) + " = " + Alphas(11));
                    ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                    ShowContinueError("Error found in " + cAlphaFields(14) + " = " + Alphas(14));
                    ShowContinueError("...schedule values must be (>0., <=1.)");
                    ErrorsFound = true;
                }
            }

            // Get Coil information
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->BothOption || state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->HeatingOption) {
                // Heating coil information:
                // A14, \field Heating Coil Object Type
                //      \type choice
                //      \key Coil:Heating:Water
                //      \key Coil:Heating:Electric
                //      \key Coil:Heating:Fuel
                //      \key Coil:Heating:Steam
                // A15, \field Heating Coil Name
                //      \type object-list
                //      \object-list HeatingCoilName
                if ((!lAlphaBlanks(16))) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent = true;
                    errFlag = false;

                    cHeatingCoilType = Alphas(15);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh = cHeatingCoilType;
                    {
                        auto const SELECT_CASE_var(cHeatingCoilType);
                        if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType = state.dataUnitVentilators->Heating_WaterCoilType;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
                        } else if (SELECT_CASE_var == "COIL:HEATING:STEAM") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType = state.dataUnitVentilators->Heating_SteamCoilType;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
                        } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType = state.dataUnitVentilators->Heating_ElectricCoilType;
                        } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType = state.dataUnitVentilators->Heating_GasCoilType;
                        } else {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                            ShowContinueError("illegal value: " + cAlphaFields(15) + "=\"" + Alphas(15) + "\".");
                            ErrorsFound = true;
                            errFlag = true;
                        }
                    }
                    if (!errFlag) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName = Alphas(16);
                        ValidateComponent(cHeatingCoilType, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, IsNotOK, CurrentModuleObject);
                        if (IsNotOK) {
                            ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                            ErrorsFound = true;
                        } else {
                            // The heating coil control node is necessary for a hot water coil, but not necessary for an
                            // electric or gas coil.
                            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_WaterCoilType ||
                                state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_SteamCoilType) {
                                // mine the hot water or steam node from the coil object
                                errFlag = false;
                                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_WaterCoilType) {
                                    state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode =
                                        GetCoilWaterInletNode("Coil:Heating:Water", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, errFlag);
                                } else {
                                    state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index =
                                        GetSteamCoilIndex("COIL:HEATING:STEAM", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, errFlag);
                                    state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode =
                                        GetCoilSteamInletNode(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, errFlag);
                                }
                                // Other error checks should trap before it gets to this point in the code, but including just in case.
                                if (errFlag) {
                                    ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                                    ErrorsFound = true;
                                }
                            }
                        }
                    }

                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolHotWaterFlow = 0.0;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolHotSteamFlow = 0.0;

                    state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlOffset = Numbers(4);
                    // Set default convergence tolerance
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlOffset <= 0.0) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlOffset = 0.001;
                    }
                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->Heating_WaterCoilType) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow =
                                GetWaterCoilMaxFlowRate("Coil:Heating:Water", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->Heating_SteamCoilType) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow =
                                GetSteamCoilMaxFlowRate("Coil:Heating:Steam", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->Heating_ElectricCoilType) {
                        } else if (SELECT_CASE_var == state.dataUnitVentilators->Heating_GasCoilType) {
                        } else {
                        }
                    }
                } else { // heating coil is required for these options
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", missing heating coil");
                    ShowContinueError("a heating coil is required for " + cAlphaFields(13) + "=\"" + Alphas(13) + "\".");
                    ErrorsFound = true;
                } // IF (.NOT. lAlphaBlanks(15)) THEN - from the start of heating coil information
            }     // is option both or heating only

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->BothOption || state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->CoolingOption) {
                // Cooling coil information (if one is present):
                // A16, \field Cooling Coil Object Type
                //      \type choice
                //      \key Coil:Cooling:Water
                //      \key Coil:Cooling:Water:DetailedGeometry
                //      \key CoilSystem:Cooling:Water:HeatExchangerAssisted
                // A17, \field Cooling Coil Name
                //      \type object-list
                //      \object-list CoolingCoilsWater
                if (!lAlphaBlanks(18)) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent = true;
                    errFlag = false;

                    cCoolingCoilType = Alphas(17);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh = cCoolingCoilType;
                    {
                        auto const SELECT_CASE_var(cCoolingCoilType);
                        if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType = state.dataUnitVentilators->Cooling_CoilWaterCooling;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantName = Alphas(18);
                        } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType = state.dataUnitVentilators->Cooling_CoilDetailedCooling;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantName = Alphas(18);
                        } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType = state.dataUnitVentilators->Cooling_CoilHXAssisted;
                            GetHXCoilTypeAndName(cCoolingCoilType,
                                                 Alphas(18),
                                                 ErrorsFound,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantType,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantName);
                            if (UtilityRoutines::SameString(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantType, "Coil:Cooling:Water")) {
                                state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum = TypeOf_CoilWaterCooling;
                            } else if (UtilityRoutines::SameString(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantType, "Coil:Cooling:Water:DetailedGeometry")) {
                                state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling;
                            } else {
                                ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                                ShowContinueError("For: " + cAlphaFields(17) + "=\"" + Alphas(17) + "\".");
                                ShowContinueError("Invalid Coil Type=" + state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantType +
                                                  ", Name=" + state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantName);
                                ShowContinueError("must be \"Coil:Cooling:Water\" or \"Coil:Cooling:Water:DetailedGeometry\"");
                                ErrorsFound = true;
                            }
                        } else {
                            ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", invalid");
                            ShowContinueError("illegal value: " + cAlphaFields(17) + "=\"" + cCoolingCoilType + "\".");
                            ErrorsFound = true;
                            errFlag = true;
                        }
                    }

                    if (!errFlag) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName = Alphas(18);
                        ValidateComponent(cCoolingCoilType, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, IsNotOK, CurrentModuleObject);
                        if (IsNotOK) {
                            ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                            ErrorsFound = true;
                        } else {
                            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType != state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                                // mine the cold water node from the coil object
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode =
                                    GetCoilWaterInletNode(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, errFlag);
                            } else {
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode =
                                    GetHXCoilWaterInletNode(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, errFlag);
                            }
                            // Other error checks should trap before it gets to this point in the code, but including just in case.
                            if (errFlag) {
                                ShowContinueError("...specified in " + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\".");
                                ErrorsFound = true;
                            }
                        }
                    }

                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolColdWaterFlow = 0.0;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlOffset = Numbers(5);
                    // Set default convergence tolerance
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlOffset <= 0.0) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlOffset = 0.001;
                    }
                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->Cooling_CoilWaterCooling) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow =
                                GetWaterCoilMaxFlowRate("Coil:Cooling:Water", state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
                        } else if (SELECT_CASE_var == state.dataUnitVentilators->Cooling_CoilDetailedCooling) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow =
                                GetWaterCoilMaxFlowRate("Coil:Cooling:Water:DetailedGeometry", state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
                        } else if (SELECT_CASE_var == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow = GetHXAssistedCoilFlowRate(
                                "CoilSystem:Cooling:Water:HeatExchangerAssisted", state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
                        } else {
                        }
                    }
                } else { // Cooling Coil is required for this/these options
                    ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", missing cooling coil");
                    ShowContinueError("a cooling coil is required for " + cAlphaFields(13) + "=\"" + Alphas(13) + "\".");
                    ErrorsFound = true;
                } // IF (.NOT. lAlphaBlanks(17)) THEN - from the start of cooling coil information
            }
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                // check that unit ventilator air inlet node is the same as a zone exhaust node
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode == ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                    "\". Unit ventilator air inlet node name must be the same as a zone exhaust node name.");
                    ShowContinueError("..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError("..Unit ventilator air inlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode));
                    ErrorsFound = true;
                }
                // check that unit ventilator air outlet node is the same as a zone inlet node.
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode == ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr = CtrlZone;
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                    "\". Unit ventilator air outlet node name must be the same as a zone inlet node name.");
                    ShowContinueError("..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError("..Unit ventilator air outlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));
                    ErrorsFound = true;
                }
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide) {
                    // check that unit ventilator air outlet node is the same as a zone inlet node.
                    ZoneNodeNotFound = true;
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                            if (state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode == ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr = CtrlZone;
                                ZoneNodeNotFound = false;
                                break;
                            }
                        }
                    }
                    if (ZoneNodeNotFound) {
                        ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                        "\". Unit ventilator air outlet node name must be the same as a zone inlet node name.");
                        ShowContinueError("..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                        ShowContinueError("..Unit ventilator air outlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));
                        ErrorsFound = true;
                    }

                    // check that the air mixer out node is the unit ventilator air inlet node
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode != state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode) {
                        ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                        "\". unit ventilator air inlet node name must be the same as the mixer outlet node name.");
                        ShowContinueError("..Air terminal mixer outlet node name is specified in AirTerminal:SingleDuct:Mixer object.");
                        ShowContinueError("..Unit ventilator air inlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode));
                        ErrorsFound = true;
                    }
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_SupplySide) {
                    // check that the mixer secondary air node is the unit ventilator air outlet node
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode != state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerSecNode) {
                        ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                        "\". unit ventilator air outlet node name must be the same as the mixer secondary air inlet node name.");
                        ShowContinueError("..Air terminal mixer secondary node name is specified in AirTerminal:SingleDuct:Mixer object.");
                        ShowContinueError("..Unit ventilator air outlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));
                        ErrorsFound = true;
                    }

                    // check that air teminal mixer outlet node is the same as a zone inlet node.
                    ZoneNodeNotFound = true;
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        if (!ZoneEquipConfig(CtrlZone).IsControlled) continue;
                        for (NodeNum = 1; NodeNum <= ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode == ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr = CtrlZone;
                                ZoneNodeNotFound = false;
                                break;
                            }
                        }
                    }
                    if (ZoneNodeNotFound) {
                        ShowSevereError(CurrentModuleObject + " = \"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name +
                                        "\". Air mixer outlet node name must be the same as a zone inlet node name.");
                        ShowContinueError("..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                        ShowContinueError("..Air terminal mixer outlet node name = " + NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode));
                        ErrorsFound = true;
                    }
                }
            }
            {
                auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption);
                if (SELECT_CASE_var == state.dataUnitVentilators->BothOption) { // 'HeatingAndCooling'
                    // Add cooling coil to component sets array when present
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  cCoolingCoilType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode),
                                  "UNDEFINED");

                    // Add heating coil to component sets array when cooling coil present
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  cHeatingCoilType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                  "UNDEFINED",
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));

                } else if (SELECT_CASE_var == state.dataUnitVentilators->HeatingOption) { // 'Heating'
                    // Add heating coil to component sets array when no cooling coil present
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  cHeatingCoilType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode),
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));

                } else if (SELECT_CASE_var == state.dataUnitVentilators->CoolingOption) { // 'Cooling'
                    // Add cooling coil to component sets array when no heating coil present
                    SetUpCompSets(CurrentModuleObject,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                  cCoolingCoilType,
                                  state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode),
                                  NodeID(state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode));

                } else if (SELECT_CASE_var == state.dataUnitVentilators->NoneOption) {

                } else {
                }
            }

        } // ...loop over all of the unit ventilators found in the input file

        Alphas.deallocate();
        Numbers.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) ShowFatalError(RoutineName + "Errors found in input.");

        // Setup Report variables for the Unit Ventilators, CurrentModuleObject='ZoneHVAC:UnitVentilator'
        for (UnitVentNum = 1; UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents; ++UnitVentNum) {
            SetupOutputVariable("Zone Unit Ventilator Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).HeatPower,
                                "System",
                                "Average",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).HeatEnergy,
                                "System",
                                "Sum",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).TotCoolPower,
                                "System",
                                "Average",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).TotCoolEnergy,
                                "System",
                                "Sum",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).SensCoolPower,
                                "System",
                                "Average",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).SensCoolEnergy,
                                "System",
                                "Sum",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Fan Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ElecPower,
                                "System",
                                "Average",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            // Note that the unit vent fan electric is NOT metered because this value is already metered through the fan component
            SetupOutputVariable("Zone Unit Ventilator Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).ElecEnergy,
                                "System",
                                "Sum",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            SetupOutputVariable("Zone Unit Ventilator Fan Availability Status",
                                OutputProcessor::Unit::None,
                                state.dataUnitVentilators->UnitVent(UnitVentNum).AvailStatus,
                                "System",
                                "Average",
                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == FanType_SimpleOnOff) {
                SetupOutputVariable("Zone Unit Ventilator Fan Part Load Ratio",
                                    OutputProcessor::Unit::None,
                                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio,
                                    "System",
                                    "Average",
                                    state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            }
        }

        for (UnitVentNum = 1; UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents; ++UnitVentNum) {
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::objectVectorOOFanSystemModel,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::objectVectorOOFanSystemModel,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::structArrayLegacyFanModels,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::structArrayLegacyFanModels,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
            }
        }
        for (UnitVentNum = 1; UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents; ++UnitVentNum) {
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::objectVectorOOFanSystemModel,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::objectVectorOOFanSystemModel,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::structArrayLegacyFanModels,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {
                    coilSelectionReportObj->setCoilSupplyFanInfo(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                                                 DataAirSystems::structArrayLegacyFanModels,
                                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
                }
            }
        }
    }

    void InitUnitVentilator(int const UnitVentNum,         // index for the current unit ventilator
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            int const ZoneNum              // number of zone being served
    )
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes all of the data elements which are necessary
        // to simulate a unit ventilator.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::ZoneComp;
        using DataHVACGlobals::ZoneCompTurnFansOff;
        using DataHVACGlobals::ZoneCompTurnFansOn;
        using DataPlant::PlantLoop;
        using DataPlant::TypeOf_CoilSteamAirHeating;
        using DataPlant::TypeOf_CoilWaterCooling;
        using DataPlant::TypeOf_CoilWaterDetailedFlatCooling;
        using DataPlant::TypeOf_CoilWaterSimpleHeating;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::UnitVentilator_Num;
        using DataZoneEquipment::ZoneEquipInputsFilled;
        using FluidProperties::GetDensityGlycol;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using namespace DataZoneEnergyDemands;

        static std::string const RoutineName("InitUnitVentilator");

        int AirRelNode;  // relief air node number in unit ventilator loop
        int ColdConNode; // cold water control node number in unit ventilator loop
        int Loop;
        static Array1D_bool MyEnvrnFlag;
        static Array1D_bool MyPlantScanFlag;
        static Array1D_bool MyZoneEqFlag; // used to set up zone equipment availability managers
        int HotConNode;                   // hot water control node number in unit ventilator loop
        int InNode;                       // inlet node number in unit ventilator loop
        int OutNode;                      // outlet node number in unit ventilator loop
        int OutsideAirNode;               // outside air node number in unit ventilator loop
        Real64 RhoAir;                    // air density at InNode
        Real64 TempSteamIn;
        Real64 SteamDensity;
        Real64 rho; // local fluid density
        bool errFlag;
        bool SetMassFlowRateToZero; // TRUE when mass flow rates need to be set to zero

        SetMassFlowRateToZero = false;
        // FLOW:

        // Do the one time initializations
        if (state.dataUnitVentilators->MyOneTimeFlag) {

            MyEnvrnFlag.allocate(state.dataUnitVentilators->NumOfUnitVents);
            state.dataUnitVentilators->MySizeFlag.allocate(state.dataUnitVentilators->NumOfUnitVents);
            MyPlantScanFlag.allocate(state.dataUnitVentilators->NumOfUnitVents);
            MyZoneEqFlag.allocate(state.dataUnitVentilators->NumOfUnitVents);
            MyEnvrnFlag = true;
            state.dataUnitVentilators->MySizeFlag = true;
            MyPlantScanFlag = true;
            MyZoneEqFlag = true;
            state.dataUnitVentilators->MyOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (MyZoneEqFlag(UnitVentNum)) { // initialize the name of each availability manager list and zone number
                ZoneComp(UnitVentilator_Num).ZoneCompAvailMgrs(UnitVentNum).AvailManagerListName = state.dataUnitVentilators->UnitVent(UnitVentNum).AvailManagerListName;
                ZoneComp(UnitVentilator_Num).ZoneCompAvailMgrs(UnitVentNum).ZoneNum = ZoneNum;
                MyZoneEqFlag(UnitVentNum) = false;
            }
            state.dataUnitVentilators->UnitVent(UnitVentNum).AvailStatus = ZoneComp(UnitVentilator_Num).ZoneCompAvailMgrs(UnitVentNum).AvailStatus;
        }

        if (MyPlantScanFlag(UnitVentNum) && allocated(PlantLoop)) {
            if ((state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) ||
                (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating)) {
                errFlag = false;
                ScanPlantLoopsForObject(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_PlantTypeNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError("Reference Unit=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", type=ZoneHVAC:UnitVentilator");
                    ShowFatalError("InitUnitVentilator: Program terminated due to previous condition(s).");
                }

                state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum = PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum)
                                                              .LoopSide(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide)
                                                              .Branch(state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum)
                                                              .Comp(state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum)
                                                              .NodeNumOut;
            }
            if ((state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) ||
                (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling)) {
                errFlag = false;
                ScanPlantLoopsForObject(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPlantName,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_PlantTypeNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum,
                                        state.dataUnitVentilators->UnitVent(UnitVentNum).CWCompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                if (errFlag) {
                    ShowContinueError("Reference Unit=\"" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + "\", type=ZoneHVAC:UnitVentilator");
                    ShowFatalError("InitUnitVentilator: Program terminated due to previous condition(s).");
                }

                state.dataUnitVentilators->UnitVent(UnitVentNum).ColdCoilOutNodeNum = PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum)
                                                               .LoopSide(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide)
                                                               .Branch(state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum)
                                                               .Comp(state.dataUnitVentilators->UnitVent(UnitVentNum).CWCompNum)
                                                               .NodeNumOut;
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent)
                    ShowFatalError("InitUnitVentilator: Unit=" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name + ", invalid cooling coil type. Program terminated.");
            }
            MyPlantScanFlag(UnitVentNum) = false;
        } else if (MyPlantScanFlag(UnitVentNum) && !state.dataGlobal->AnyPlantInModel) {
            MyPlantScanFlag(UnitVentNum) = false;
        }

        if (!state.dataUnitVentilators->ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            state.dataUnitVentilators->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataUnitVentilators->NumOfUnitVents; ++Loop) {
                if (CheckZoneEquipmentList("ZoneHVAC:UnitVentilator", state.dataUnitVentilators->UnitVent(Loop).Name)) continue;
                ShowSevereError("InitUnitVentilator: Unit=[UNIT VENTILATOR," + state.dataUnitVentilators->UnitVent(Loop).Name +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataUnitVentilators->MySizeFlag(UnitVentNum) && !MyPlantScanFlag(UnitVentNum)) {

            SizeUnitVentilator(UnitVentNum);

            state.dataUnitVentilators->MySizeFlag(UnitVentNum) = false;
        }

        // Do the one time initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(UnitVentNum) && !MyPlantScanFlag(UnitVentNum)) {
            InNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
            OutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode;
            HotConNode = state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode;
            ColdConNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode;
            OutsideAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode;
            RhoAir = state.dataEnvrn->StdRhoAir;

            // set the mass flow rates from the input volume flow rates
            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow = RhoAir * state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow;
            state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow = RhoAir * state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow;
            state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirMassFlow = RhoAir * state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow;
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow > state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
                state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirMassFlow =
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow * (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow / state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow);
                ShowWarningError("Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
            }

            // set the node max and min mass flow rates
            Node(OutsideAirNode).MassFlowRateMax = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
            Node(OutsideAirNode).MassFlowRateMin = 0.0;

            Node(OutNode).MassFlowRateMax = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(OutNode).MassFlowRateMin = 0.0;

            Node(InNode).MassFlowRateMax = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(InNode).MassFlowRateMin = 0.0;

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) { // Only initialize these if a heating coil is actually present

                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_WaterCoilType) {

                    rho = GetDensityGlycol(PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidName,
                                           DataGlobalConstants::HWInitConvTemp,
                                           PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidIndex,
                                           RoutineName);

                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotWaterFlow = rho * state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotWaterFlow = rho * state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolHotWaterFlow;

                    InitComponentNodes(state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotWaterFlow,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotWaterFlow,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum);
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_SteamCoilType) {
                    TempSteamIn = 100.00;
                    SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_FluidIndex, RoutineName);
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotSteamFlow = SteamDensity * state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow;
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotSteamFlow = SteamDensity * state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolHotSteamFlow;

                    InitComponentNodes(state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotSteamFlow,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotSteamFlow,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                       state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum);
                }
            } //(state.dataUnitVentilators->UnitVent(UnitVentNum)%HCoilPresent)

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) { // Only initialize these if a cooling coil is actually present
                rho = GetDensityGlycol(
                    PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidName, 5.0, PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidIndex, RoutineName);

                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxColdWaterFlow = rho * state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow;
                state.dataUnitVentilators->UnitVent(UnitVentNum).MinColdWaterFlow = rho * state.dataUnitVentilators->UnitVent(UnitVentNum).MinVolColdWaterFlow;
                InitComponentNodes(state.dataUnitVentilators->UnitVent(UnitVentNum).MinColdWaterFlow,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).MaxColdWaterFlow,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).ColdCoilOutNodeNum,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum,
                                   state.dataUnitVentilators->UnitVent(UnitVentNum).CWCompNum);
            }
            MyEnvrnFlag(UnitVentNum) = false;
        } // ...end start of environment inits

        if (!state.dataGlobal->BeginEnvrnFlag) MyEnvrnFlag(UnitVentNum) = true;

        // These initializations are done every iteration...
        InNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        OutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode;
        OutsideAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode;
        AirRelNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirReliefNode;

        state.dataUnitVentilators->QZnReq = ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired; // zone load needed
        state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = 0.0;

        if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr > 0) {
            if (GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).FanSchedPtr) == 0.0) {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OpMode = CycFanCycCoil;
            } else {
                state.dataUnitVentilators->UnitVent(UnitVentNum).OpMode = ContFanCycCoil;
            }
        }

        if (GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).SchedPtr) > 0) {
            if ((GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).FanAvailSchedPtr) > 0 || ZoneCompTurnFansOn) && !ZoneCompTurnFansOff) {
                if ((std::abs(ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired) < SmallLoad) || (CurDeadBandOrSetback(ZoneNum))) {
                    SetMassFlowRateToZero = true;
                }
            } else {
                SetMassFlowRateToZero = true;
            }
        } else {
            SetMassFlowRateToZero = true;
        }

        if (SetMassFlowRateToZero) {
            Node(InNode).MassFlowRate = 0.0;
            Node(InNode).MassFlowRateMaxAvail = 0.0;
            Node(InNode).MassFlowRateMinAvail = 0.0;
            Node(OutNode).MassFlowRate = 0.0;
            Node(OutNode).MassFlowRateMaxAvail = 0.0;
            Node(OutNode).MassFlowRateMinAvail = 0.0;
            Node(OutsideAirNode).MassFlowRate = 0.0;
            Node(OutsideAirNode).MassFlowRateMaxAvail = 0.0;
            Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                Node(AirRelNode).MassFlowRate = 0.0;
                Node(AirRelNode).MassFlowRateMaxAvail = 0.0;
                Node(AirRelNode).MassFlowRateMinAvail = 0.0;
            }
        } else {
            Node(InNode).MassFlowRate = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(InNode).MassFlowRateMaxAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(InNode).MassFlowRateMinAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(OutNode).MassFlowRate = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(OutNode).MassFlowRateMaxAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(OutNode).MassFlowRateMinAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirMassFlow;
            Node(OutsideAirNode).MassFlowRate = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
            Node(OutsideAirNode).MassFlowRateMaxAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
            Node(OutsideAirNode).MassFlowRateMinAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                Node(AirRelNode).MassFlowRate = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
                Node(AirRelNode).MassFlowRateMaxAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
                Node(AirRelNode).MassFlowRateMinAvail = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirMassFlow;
            }
        }

        // Initialize the relief air (same as inlet conditions to the unit ventilator...
        // Note that mass flow rates will be taken care of later.
        if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
            Node(AirRelNode) = Node(InNode);
        }
        state.dataUnitVentilators->OAMassFlowRate = 0.0;

        // Just in case the unit is off and conditions do not get sent through
        // the unit for some reason, set the outlet conditions equal to the inlet
        // conditions of the unit ventilator
        Node(OutNode).Temp = Node(InNode).Temp;
        Node(OutNode).Press = Node(InNode).Press;
        Node(OutNode).HumRat = Node(InNode).HumRat;
        Node(OutNode).Enthalpy = Node(InNode).Enthalpy;

        // These initializations only need to be done once at the start of the iterations...
        if (FirstHVACIteration) {
            // Initialize the outside air conditions...
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                Node(OutsideAirNode).Temp = Node(OutsideAirNode).OutAirDryBulb;
            }
            //    Node(OutsideAirNode)%HumRat   = OutHumRat
            //    Node(OutsideAirNode)%Press    = OutBaroPress
            //    Node(OutsideAirNode)%Enthalpy = PsyHFnTdbW(OutDryBulbTemp,OutHumRat)
        }
    }

    void SizeUnitVentilator(int const UnitVentNum)
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Unit Ventilator components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data.

        using namespace DataSizing;
        using DataHeatBalance::Zone;
        using DataHVACGlobals::CoolingAirflowSizing;
        using DataHVACGlobals::CoolingCapacitySizing;
        using DataHVACGlobals::HeatingAirflowSizing;
        using DataHVACGlobals::HeatingCapacitySizing;
        using DataPlant::PlantLoop;
        using FluidProperties::GetDensityGlycol;
        using FluidProperties::GetSpecificHeatGlycol;

        using HVACHXAssistedCoolingCoil::GetHXCoilType;
        using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
        using PlantUtilities::MyPlantSizingIndex;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetCoilSteamOutletNode;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeUnitVentilator");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum; // index of plant sizing object for 1st heating loop
        int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
        bool ErrorsFound;
        Real64 DesCoolingLoad;
        Real64 DesHeatingLoad;
        Real64 TempSteamIn;
        Real64 EnthSteamInDry;
        Real64 EnthSteamOutWet;
        Real64 LatentHeatSteam;
        Real64 SteamDensity;
        static int RefrigIndex(0);
        static int CoilWaterInletNode(0);
        static int CoilWaterOutletNode(0);
        static int CoilSteamInletNode(0);
        static int CoilSteamOutletNode(0);
        std::string CoolingCoilName;
        std::string CoolingCoilType;
        Real64 rho;
        Real64 Cp;
        static int DummyWaterIndex(1);
        bool IsAutoSize;                // Index to autosize
        Real64 MaxAirVolFlowDes;        // Autosized maximum air flow for reporting
        Real64 MaxAirVolFlowUser;       // Hardsized maximum air flow for reporting
        Real64 OutAirVolFlowDes;        // Autosized outdoor air flow for reporting
        Real64 OutAirVolFlowUser;       // Hardsized outdoor air flow for reporting
        Real64 MinOutAirVolFlowDes;     // Autosized minimum outdoor air flow for reporting
        Real64 MinOutAirVolFlowUser;    // Hardsized minimum outdoor air flow for reporting
        Real64 MaxVolHotWaterFlowDes;   // Autosized maximum water flow for reporting
        Real64 MaxVolHotWaterFlowUser;  // Hardsized maximum water flow for reporting
        Real64 MaxVolHotSteamFlowDes;   // Autosized maximum steam flow for reporting
        Real64 MaxVolHotSteamFlowUser;  // Hardsized maximum steam flow for reporting
        Real64 MaxVolColdWaterFlowDes;  // Autosized maximum chilled water flow for reporting
        Real64 MaxVolColdWaterFlowUser; // Hardsized maximum chilled water flow for reporting

        std::string CompName;     // component name
        std::string CompType;     // component type
        std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;          // autosized value of coil input field
        int FieldNum = 2;         // IDD numeric field number where input field description is found
        int SizingMethod;  // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                           // HeatingCapacitySizing, etc.)
        bool PrintFlag;    // TRUE when sizing information is reported in the eio file
        int zoneHVACIndex; // index of zoneHVAC equipment sizing specification
        int SAFMethod(0);  // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                           // FractionOfAutosizedHeatingAirflow ...)
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                                // FractionOfAutosizedHeatingCapacity )
        Real64 CoolingAirVolFlowScalable; // cooling airvolume for rate determined using scalable sizing method
        Real64 HeatingAirVolFlowScalable; // heating airvolume for rate determined using scalable sizing method
        bool DoWaterCoilSizing = false;   // if TRUE do water coil sizing calculation
        Real64 WaterCoilSizDeltaT;        // water coil deltaT for design water flow rate autosizing
        int CoilNum;                      // index of water coil object

        PltSizHeatNum = 0;
        ErrorsFound = false;
        IsAutoSize = false;
        MaxAirVolFlowDes = 0.0;
        MaxAirVolFlowUser = 0.0;
        OutAirVolFlowDes = 0.0;
        OutAirVolFlowUser = 0.0;
        MinOutAirVolFlowDes = 0.0;
        MinOutAirVolFlowUser = 0.0;
        MaxVolHotWaterFlowDes = 0.0;
        MaxVolHotWaterFlowUser = 0.0;
        MaxVolHotSteamFlowDes = 0.0;
        MaxVolHotSteamFlowUser = 0.0;
        MaxVolColdWaterFlowDes = 0.0;
        MaxVolColdWaterFlowUser = 0.0;
        CoolingAirVolFlowScalable = 0.0;
        HeatingAirVolFlowScalable = 0.0;
        DataScalableSizingON = false;
        DataScalableCapSizingON = false;
        CompType = state.dataUnitVentilators->cMO_UnitVentilator;
        CompName = state.dataUnitVentilators->UnitVent(UnitVentNum).Name;
        DataZoneNumber = state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr;
        ZoneCoolingOnlyFan = false;
        ZoneHeatingOnlyFan = false;
        DoWaterCoilSizing = false;
        CoilNum = 0;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            DataSizing::DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
        } else {
            DataSizing::DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
        }
        DataSizing::DataFanIndex = state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index;
        // unit ventilator is always blow thru
        DataSizing::DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;

        if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->BothOption) {
            ZoneCoolingOnlyFan = true;
            ZoneHeatingOnlyFan = true;
        } else if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->HeatingOption) {
            ZoneHeatingOnlyFan = true;
        } else if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->CoolingOption) {
            ZoneCoolingOnlyFan = true;
        } else if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->NoneOption) {
        }

        if (CurZoneEqNum > 0) {
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex > 0) {

                // initialize OA flow for sizing other inputs (e.g., inlet temp, capacity, etc.)
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow == AutoSize) {
                    ZoneEqSizing(CurZoneEqNum).OAVolFlow = FinalZoneSizing(CurZoneEqNum).MinOA;
                } else {
                    ZoneEqSizing(CurZoneEqNum).OAVolFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow;
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {      // set up ATMixer conditions for scalable capacity sizing
                    ZoneEqSizing(CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
                    SingleDuct::setATMixerSizingProperties(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex, state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr, CurZoneEqNum);
                }

                zoneHVACIndex = state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex;
                // N1 , \field Maximum Supply Air Flow Rate
                PrintFlag = true;

                if (ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod > 0 && ZoneCoolingOnlyFan && !ZoneHeatingOnlyFan) {

                    SAFMethod = ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                    SizingMethod = CoolingAirflowSizing;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                    if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                        SAFMethod == FractionOfAutosizedCoolingAirflow) {
                        if (SAFMethod == SupplyAirFlowRate) {
                            if (ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                                ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                            }
                            TempSize = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        } else if (SAFMethod == FlowPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                            ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow * Zone(DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                            DataScalableSizingON = true;
                        } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                            DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            TempSize = AutoSize;
                            DataScalableSizingON = true;
                        } else {
                            TempSize = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        }
                        bool errorsFound = false;
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                        if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                        sizingCoolingAirFlow.overrideSizingString(stringOverride);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(TempSize, errorsFound);

                    } else if (SAFMethod == FlowPerCoolingCapacity) {
                        SizingMethod = CoolingCapacitySizing;
                        TempSize = AutoSize;
                        PrintFlag = false;
                        DataScalableSizingON = true;
                        DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                        CoolingCapacitySizer sizerCoolingCapacity;
                        sizerCoolingCapacity.overrideSizingString(SizingString);
                        sizerCoolingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(TempSize, ErrorsFound);
                        DataFlowPerCoolingCapacity = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        bool errorsFound = false;
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                        if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                        sizingCoolingAirFlow.overrideSizingString(stringOverride);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(TempSize, errorsFound);
                    }
                    // DataScalableSizingON = false;

                } else if (ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod > 0 && ZoneHeatingOnlyFan && !ZoneCoolingOnlyFan) {
                    SizingMethod = HeatingAirflowSizing;
                    SAFMethod = ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                    if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                        SAFMethod == FractionOfAutosizedHeatingAirflow) {
                        if (SAFMethod == SupplyAirFlowRate) {
                            if (ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                                ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                            }
                            TempSize = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        } else if (SAFMethod == FlowPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                            ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow * Zone(DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                            DataScalableSizingON = true;
                        } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                            DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                            TempSize = AutoSize;
                            DataScalableSizingON = true;
                        } else {
                            TempSize = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        }
                        bool errorsFound = false;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(TempSize, errorsFound);

                    } else if (SAFMethod == FlowPerHeatingCapacity) {
                        SizingMethod = HeatingCapacitySizing;
                        TempSize = AutoSize;
                        PrintFlag = false;
                        DataScalableSizingON = true;
                        DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        DataAutosizedHeatingCapacity = sizerHeatingCapacity.size(TempSize, errorsFound);
                        DataFlowPerHeatingCapacity = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        SizingMethod = HeatingAirflowSizing;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        errorsFound = false;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(TempSize, errorsFound);
                    }
                    // DataScalableSizingON = false;
                } else {

                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption != state.dataUnitVentilators->NoneOption) {
                        if (ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod > 0) {
                            SAFMethod = ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                            SizingMethod = CoolingAirflowSizing;
                            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                            if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                                SAFMethod == FractionOfAutosizedCoolingAirflow) {
                                if (SAFMethod == SupplyAirFlowRate) {
                                    if (ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                                        ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                                    }
                                    TempSize = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                } else if (SAFMethod == FlowPerFloorArea) {
                                    ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                                    ZoneEqSizing(CurZoneEqNum).AirVolFlow =
                                        ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow * Zone(DataZoneNumber).FloorArea;
                                    TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                                    DataScalableSizingON = true;
                                } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                                    DataFracOfAutosizedCoolingAirflow = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                    TempSize = AutoSize;
                                    DataScalableSizingON = true;
                                } else {
                                    TempSize = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                }
                                bool errorsFound = false;
                                CoolingAirFlowSizer sizingCoolingAirFlow;
                                std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                                if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                                // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                                sizingCoolingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(TempSize, errorsFound);

                            } else if (SAFMethod == FlowPerCoolingCapacity) {
                                SizingMethod = CoolingCapacitySizing;
                                TempSize = AutoSize;
                                PrintFlag = false;
                                DataScalableSizingON = true;
                                DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                CoolingCapacitySizer sizerCoolingCapacity;
                                sizerCoolingCapacity.overrideSizingString(SizingString);
                                sizerCoolingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(TempSize, ErrorsFound);
                                DataFlowPerCoolingCapacity = ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                PrintFlag = true;
                                TempSize = AutoSize;
                                bool errorsFound = false;
                                CoolingAirFlowSizer sizingCoolingAirFlow;
                                std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                                if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                                // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                                sizingCoolingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                CoolingAirVolFlowScalable = sizingCoolingAirFlow.size(TempSize, errorsFound);
                            }
                        } else if (ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod > 0) {
                            SizingMethod = HeatingAirflowSizing;
                            SAFMethod = ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
                            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                            if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                                SAFMethod == FractionOfAutosizedHeatingAirflow) {
                                if (SAFMethod == SupplyAirFlowRate) {
                                    if (ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                                        ZoneEqSizing(CurZoneEqNum).AirVolFlow = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                                    }
                                    TempSize = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                } else if (SAFMethod == FlowPerFloorArea) {
                                    ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                                    ZoneEqSizing(CurZoneEqNum).AirVolFlow =
                                        ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow * Zone(DataZoneNumber).FloorArea;
                                    TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                                    DataScalableSizingON = true;
                                } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                                    DataFracOfAutosizedHeatingAirflow = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                    TempSize = AutoSize;
                                    DataScalableSizingON = true;
                                } else {
                                    TempSize = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                }
                                bool errorsFound = false;
                                HeatingAirFlowSizer sizingHeatingAirFlow;
                                sizingHeatingAirFlow.overrideSizingString(SizingString);
                                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                                sizingHeatingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(TempSize, errorsFound);

                            } else if (SAFMethod == FlowPerHeatingCapacity) {
                                SizingMethod = HeatingCapacitySizing;
                                TempSize = AutoSize;
                                PrintFlag = false;
                                DataScalableSizingON = true;
                                DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                bool errorsFound = false;
                                HeatingCapacitySizer sizerHeatingCapacity;
                                sizerHeatingCapacity.overrideSizingString(SizingString);
                                sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                DataAutosizedHeatingCapacity = sizerHeatingCapacity.size(TempSize, errorsFound);
                                DataFlowPerHeatingCapacity = ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                                SizingMethod = HeatingAirflowSizing;
                                PrintFlag = true;
                                TempSize = AutoSize;
                                errorsFound = false;
                                HeatingAirFlowSizer sizingHeatingAirFlow;
                                sizingHeatingAirFlow.overrideSizingString(SizingString);
                                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                                sizingHeatingAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                HeatingAirVolFlowScalable = sizingHeatingAirFlow.size(TempSize, errorsFound);
                            }
                        }
                        // DataScalableSizingON = false;
                    } else { // if ( UnitVent (UnitVentNum ).CoilOption /= state.dataUnitVentilators->NoneOption )

                        PrintFlag = true;
                        FieldNum = 1;
                        SizingString = state.dataUnitVentilators->UnitVentNumericFields(UnitVentNum).FieldNames(FieldNum) + " [m3/s]";
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow == AutoSize) {
                            TempSize = FinalZoneSizing(CurZoneEqNum).MinOA;
                        } else {
                            TempSize = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow;
                        }
                        bool errorsFound = false;
                        SystemAirFlowSizer sizerSystemAirFlow;
                        // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizerSystemAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowScalable = sizerSystemAirFlow.size(TempSize, errorsFound);
                    }
                }

                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow = max(CoolingAirVolFlowScalable, HeatingAirVolFlowScalable);

            } else {
                // no scalble sizing method has been specified. Sizing proceeds using the method
                // specified in the zoneHVAC object
                // N1 , \field Maximum Supply Air Flow Rate
                PrintFlag = true;
                FieldNum = 1;
                SizingString = state.dataUnitVentilators->UnitVentNumericFields(UnitVentNum).FieldNames(FieldNum) + " [m3/s]";
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption == state.dataUnitVentilators->NoneOption) {

                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow == AutoSize) {
                        TempSize = FinalZoneSizing(CurZoneEqNum).MinOA;
                    } else {
                        TempSize = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow;
                    }

                } else {
                    TempSize = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow;
                }
                bool errorsFound = false;
                SystemAirFlowSizer sizerSystemAirFlow;
                // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizerSystemAirFlow.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow = sizerSystemAirFlow.size(TempSize, errorsFound);
            }
        }

        IsAutoSize = false;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurZoneEqNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) { // Simulation continue
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                 "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow);
                }
            } else {
                CheckZoneSizing(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                OutAirVolFlowDes = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow;
                if (IsAutoSize) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow = OutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name, "Design Size Maximum Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes);
                } else {
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0) {
                        OutAirVolFlowUser = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow;
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                     "Design Size Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowDes,
                                                     "User-Specified Maximum Outdoor Air Flow Rate [m3/s]",
                                                     OutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(OutAirVolFlowDes - OutAirVolFlowUser) / OutAirVolFlowUser) > AutoVsHardSizingThreshold) {
                                ShowMessage("SizeUnitVentilator: Potential issue with equipment sizing for " + state.dataUnitVentilators->cMO_UnitVentilator + ' ' +
                                            state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                ShowContinueError(format("User-Specified Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowUser));
                                ShowContinueError(
                                    format("differs from Design Size Maximum Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowDes));
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
            ZoneEqSizing(CurZoneEqNum).OAVolFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).OutAirVolFlow;

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {      // set up ATMixer conditions for use in component sizing
                ZoneEqSizing(CurZoneEqNum).OAVolFlow = 0.0; // Equipment OA flow should always be 0 when ATMixer is used
                SingleDuct::setATMixerSizingProperties(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex, state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr, CurZoneEqNum);
            }
        }

        IsAutoSize = false;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (CurZoneEqNum > 0) {
            if (!IsAutoSize && !ZoneSizingRunDone) { // Simulation continue
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                 "User-Specified Minimum Outdoor Air Flow Rate [m3/s]",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow);
                }
            } else {
                CheckZoneSizing(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                MinOutAirVolFlowDes = min(FinalZoneSizing(CurZoneEqNum).MinOA, state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow);
                if (MinOutAirVolFlowDes < SmallAirVolFlow) {
                    MinOutAirVolFlowDes = 0.0;
                }
                if (IsAutoSize) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow = MinOutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name, "Design Size Minimum Outdoor Air Flow Rate [m3/s]", MinOutAirVolFlowDes);
                } else {
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow > 0.0 && MinOutAirVolFlowDes > 0.0) {
                        MinOutAirVolFlowUser = state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirVolFlow;
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                     "Design Size Minimum Outdoor Air Flow Rate [m3/s]",
                                                     MinOutAirVolFlowDes,
                                                     "User-Specified Minimum Outdoor Air Flow Rate [m3/s]",
                                                     MinOutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MinOutAirVolFlowDes - MinOutAirVolFlowUser) / MinOutAirVolFlowUser) > AutoVsHardSizingThreshold) {
                                ShowMessage("SizeUnitVentilator: Potential issue with equipment sizing for " + state.dataUnitVentilators->cMO_UnitVentilator + ' ' +
                                            state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                ShowContinueError(format("User-Specified Minimum Outdoor Air Flow Rate of {:.5R} [m3/s]", MinOutAirVolFlowUser));
                                ShowContinueError(
                                    format("differs from Design Size Minimum Outdoor Air Flow Rate of {:.5R} [m3/s]", MinOutAirVolFlowDes));
                                ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_WaterCoilType) {
            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // Simulation continue
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                     "User-Specified Maximum Hot Water Flow [m3/s]",
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow);
                    }
                } else {
                    CheckZoneSizing(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name);

                    CoilWaterInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                    CoilWaterOutletNode = WaterCoils::GetCoilWaterOutletNode("Coil:Heating:Water", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizHeatNum = MyPlantSizingIndex("COIL:HEATING:WATER", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound);

                        CoilNum = WaterCoils::GetWaterCoilIndex("COIL:HEATING:WATER", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                            WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                            DoWaterCoilSizing = true;
                        } else {
                            if (PltSizHeatNum > 0) {
                                WaterCoilSizDeltaT = PlantSizData(PltSizHeatNum).DeltaT;
                                DoWaterCoilSizing = true;
                            } else {
                                DoWaterCoilSizing = false;
                                // If there is no heating Plant Sizing object and autosizing was requested, issue fatal error message
                                ShowSevereError("Autosizing of water flow requires a heating loop Sizing:Plant object");
                                ShowContinueError("Occurs in " + state.dataUnitVentilators->cMO_UnitVentilator + " Object=" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                ErrorsFound = true;
                            }
                        }

                        if (DoWaterCoilSizing) {
                            if (FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow >= SmallAirVolFlow) {
                                SizingMethod = HeatingCapacitySizing;
                                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex > 0) {
                                    zoneHVACIndex = state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex;
                                    CapSizingMethod = ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                        if (CapSizingMethod == HeatingDesignCapacity) {
                                            if (ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            } else {
                                                DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            }
                                            TempSize = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity * Zone(DataZoneNumber).FloorArea;
                                            DataScalableCapSizingON = true;
                                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                            DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            TempSize = AutoSize;
                                            DataScalableCapSizingON = true;
                                        }
                                    }
                                    SizingString = "";
                                    PrintFlag = false;
                                    bool errorsFound = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesHeatingLoad = sizerHeatingCapacity.size(TempSize, errorsFound);
                                    DataScalableCapSizingON = false;
                                } else {
                                    SizingString = "";
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                    bool errorsFound = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesHeatingLoad = sizerHeatingCapacity.size(TempSize, errorsFound);
                                }
                                rho = GetDensityGlycol(PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidName,
                                                       DataGlobalConstants::HWInitConvTemp,
                                                       PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidIndex,
                                                       RoutineName);
                                Cp = GetSpecificHeatGlycol(PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidName,
                                                           DataGlobalConstants::HWInitConvTemp,
                                                           PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum).FluidIndex,
                                                           RoutineName);
                                MaxVolHotWaterFlowDes = DesHeatingLoad / (WaterCoilSizDeltaT * Cp * rho);

                            } else {
                                MaxVolHotWaterFlowDes = 0.0;
                            }
                        }
                    }
                    if (IsAutoSize) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name, "Design Size Maximum Hot Water Flow [m3/s]", MaxVolHotWaterFlowDes);
                    } else {
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0) {
                            MaxVolHotWaterFlowUser = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow;
                            BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                         state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                         "Design Size Maximum Hot Water Flow [m3/s]",
                                                         MaxVolHotWaterFlowDes,
                                                         "User-Specified Maximum Hot Water Flow [m3/s]",
                                                         MaxVolHotWaterFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser) / MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeUnitVentilator: Potential issue with equipment sizing for " + state.dataUnitVentilators->cMO_UnitVentilator + ' ' +
                                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                    ShowContinueError(format("User-Specified Maximum Hot Water Flow of {:.5R} [m3/s]", MaxVolHotWaterFlowUser));
                                    ShowContinueError(
                                        format("differs from Design Size Maximum Hot Water Flow of {:.5R} [m3/s]", MaxVolHotWaterFlowDes));
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow = 0.0;
        }

        IsAutoSize = false;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_SteamCoilType) {
            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // Simulation continue
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                     "User-Specified Maximum Steam Flow [m3/s]",
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow);
                    }
                } else {
                    CheckZoneSizing(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name);

                    CoilSteamInletNode = GetCoilSteamInletNode("Coil:Heating:Steam", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                    CoilSteamOutletNode = GetCoilSteamInletNode("Coil:Heating:Steam", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizHeatNum = MyPlantSizingIndex("Coil:Heating:Steam", state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound);
                        if (PltSizHeatNum > 0) {
                            if (FinalZoneSizing(CurZoneEqNum).DesHeatMassFlow >= SmallAirVolFlow) {
                                SizingMethod = HeatingCapacitySizing;
                                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex > 0) {
                                    zoneHVACIndex = state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex;
                                    CapSizingMethod = ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                        if (CapSizingMethod == HeatingDesignCapacity) {
                                            if (ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            } else {
                                                DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            }
                                            TempSize = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                                ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity * Zone(DataZoneNumber).FloorArea;
                                            DataScalableCapSizingON = true;
                                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                                            DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                                            DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                            TempSize = AutoSize;
                                            DataScalableCapSizingON = true;
                                        }
                                    }
                                    SizingString = "";
                                    PrintFlag = false;
                                    bool errorsFound = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesHeatingLoad = sizerHeatingCapacity.size(TempSize, errorsFound);
                                    DataScalableCapSizingON = false;
                                } else {
                                    SizingString = "";
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesHeatVolFlow;
                                    bool errorsFound = false;
                                    HeatingCapacitySizer sizerHeatingCapacity;
                                    sizerHeatingCapacity.overrideSizingString(SizingString);
                                    sizerHeatingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesHeatingLoad = sizerHeatingCapacity.size(TempSize, errorsFound);
                                }
                                TempSteamIn = 100.00;
                                EnthSteamInDry = GetSatEnthalpyRefrig(fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName);
                                EnthSteamOutWet = GetSatEnthalpyRefrig(fluidNameSteam, TempSteamIn, 0.0, RefrigIndex, RoutineName);
                                LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                                SteamDensity = GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, RefrigIndex, RoutineName);
                                Cp = GetSpecificHeatGlycol(fluidNameWater, PlantSizData(PltSizHeatNum).ExitTemp, DummyWaterIndex, RoutineName);
                                MaxVolHotSteamFlowDes = DesHeatingLoad / (SteamDensity * (LatentHeatSteam + PlantSizData(PltSizHeatNum).DeltaT * Cp));
                            } else {
                                MaxVolHotSteamFlowDes = 0.0;
                            }
                        } else {
                            ShowSevereError("Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                            ShowContinueError("Occurs in " + state.dataUnitVentilators->cMO_UnitVentilator + " Object=" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                            ErrorsFound = true;
                        }
                    }
                    if (IsAutoSize) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name, "Design Size Maximum Steam Flow [m3/s]", MaxVolHotSteamFlowDes);
                    } else {
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0) {
                            MaxVolHotSteamFlowUser = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow;
                            BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                         state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                         "Design Size Maximum Steam Flow [m3/s]",
                                                         MaxVolHotSteamFlowDes,
                                                         "User-Specified Maximum Steam Flow [m3/s]",
                                                         MaxVolHotSteamFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser) / MaxVolHotSteamFlowUser) > AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeUnitVentilator: Potential issue with equipment sizing for " + state.dataUnitVentilators->cMO_UnitVentilator + ' ' +
                                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                    ShowContinueError(format("User-Specified Maximum Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowUser));
                                    ShowContinueError(format("differs from Design Size Maximum Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowDes));
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotSteamFlow = 0.0;
        }

        IsAutoSize = false;
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilWaterCooling || state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilDetailedCooling ||
            state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilHXAssisted) {

            if (CurZoneEqNum > 0) {
                if (!IsAutoSize && !ZoneSizingRunDone) { // Simulation continue
                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow > 0.0) {
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                     "User-Specified Maximum Cold Water Flow [m3/s]",
                                                     state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow);
                    }
                } else {
                    CheckZoneSizing(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name);

                    if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                        CoolingCoilName = GetHXDXCoilName(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
                        CoolingCoilType = GetHXCoilType(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
                    } else {
                        CoolingCoilName = state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName;
                        CoolingCoilType = state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh;
                    }
                    CoilWaterInletNode = WaterCoils::GetCoilWaterInletNode(CoolingCoilType, CoolingCoilName, ErrorsFound);
                    CoilWaterOutletNode = WaterCoils::GetCoilWaterOutletNode(CoolingCoilType, CoolingCoilName, ErrorsFound);
                    if (IsAutoSize) {
                        PltSizCoolNum = MyPlantSizingIndex(CoolingCoilType, CoolingCoilName, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound);
                        CoilNum = WaterCoils::GetWaterCoilIndex(CoolingCoilType, CoolingCoilName, ErrorsFound);
                        if (state.dataWaterCoils->WaterCoil(CoilNum).UseDesignWaterDeltaTemp) {
                            WaterCoilSizDeltaT = state.dataWaterCoils->WaterCoil(CoilNum).DesignWaterDeltaTemp;
                            DoWaterCoilSizing = true;
                        } else {
                            if (PltSizCoolNum > 0) {
                                WaterCoilSizDeltaT = PlantSizData(PltSizCoolNum).DeltaT;
                                DoWaterCoilSizing = true;
                            } else {
                                DoWaterCoilSizing = false;
                                // If there is no cooling Plant Sizing object and autosizing was requested, issue fatal error message
                                ShowSevereError("Autosizing of water coil requires a cooling loop Sizing:Plant object");
                                ShowContinueError("Occurs in " + state.dataUnitVentilators->cMO_UnitVentilator + " Object=" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                ErrorsFound = true;
                            }
                        }
                        if (DoWaterCoilSizing) {
                            if (FinalZoneSizing(CurZoneEqNum).DesCoolMassFlow >= SmallAirVolFlow) {
                                SizingMethod = CoolingCapacitySizing;
                                if (state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex > 0) {
                                    zoneHVACIndex = state.dataUnitVentilators->UnitVent(UnitVentNum).HVACSizingIndex;
                                    CapSizingMethod = ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
                                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                                    if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                                        CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                        if (CapSizingMethod == CoolingDesignCapacity) {
                                            if (ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                                                ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                                                ZoneEqSizing(CurZoneEqNum).DesCoolingLoad = ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                            } else {
                                                DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                            }
                                            TempSize = ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                                            ZoneEqSizing(CurZoneEqNum).CoolingCapacity = true;
                                            ZoneEqSizing(CurZoneEqNum).DesCoolingLoad =
                                                ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity * Zone(DataZoneNumber).FloorArea;
                                            DataScalableCapSizingON = true;
                                        } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                                            DataFracOfAutosizedHeatingCapacity = ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                                            DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                            TempSize = AutoSize;
                                            DataScalableCapSizingON = true;
                                        }
                                    }
                                    SizingString = "";
                                    PrintFlag = false;
                                    CoolingCapacitySizer sizerCoolingCapacity;
                                    sizerCoolingCapacity.overrideSizingString(SizingString);
                                    sizerCoolingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesCoolingLoad = sizerCoolingCapacity.size(TempSize, ErrorsFound);
                                    DataScalableCapSizingON = false;
                                } else {
                                    SizingString = "";
                                    PrintFlag = false;
                                    TempSize = AutoSize;
                                    DataFlowUsedForSizing = FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                                    CoolingCapacitySizer sizerCoolingCapacity;
                                    sizerCoolingCapacity.overrideSizingString(SizingString);
                                    sizerCoolingCapacity.initializeWithinEP(CompType, CompName, PrintFlag, RoutineName);
                                    DesCoolingLoad = sizerCoolingCapacity.size(TempSize, ErrorsFound);
                                }
                                rho = GetDensityGlycol(PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidName,
                                                       5.,
                                                       PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidIndex,
                                                       RoutineName);
                                Cp = GetSpecificHeatGlycol(PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidName,
                                                           5.,
                                                           PlantLoop(state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum).FluidIndex,
                                                           RoutineName);
                                MaxVolColdWaterFlowDes = DesCoolingLoad / (WaterCoilSizDeltaT * Cp * rho);

                                if (MaxVolColdWaterFlowDes < 0.0) {
                                    ShowWarningError("Autosizing of water flow resulted in negative value.");
                                    ShowContinueError("Occurs in " + state.dataUnitVentilators->cMO_UnitVentilator + " Object=" + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                    ShowContinueError("...Sizing information found during sizing simulation:");
                                    ShowContinueError(format("...Calculated coil design load = {:.3T} W", DesCoolingLoad));
                                    ShowContinueError(format("...Calculated water flow rate  = {:.3T} m3/s", MaxVolColdWaterFlowDes));
                                    ShowContinueError("...Water flow rate will be set to 0. Check sizing inputs for zone and plant, inputs for water "
                                                      "cooling coil object, and design day specifications.");
                                    ShowContinueError("...Consider autosizing all inputs if not already doing so.");
                                    MaxVolColdWaterFlowDes = 0.0;
                                }
                            } else {
                                MaxVolColdWaterFlowDes = 0.0;
                            }
                        }
                    }
                    if (IsAutoSize) {
                        state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow = MaxVolColdWaterFlowDes;
                        BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator, state.dataUnitVentilators->UnitVent(UnitVentNum).Name, "Design Size Maximum Cold Water Flow [m3/s]", MaxVolColdWaterFlowDes);
                    } else {
                        if (state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow > 0.0 && MaxVolColdWaterFlowDes > 0.0) {
                            MaxVolColdWaterFlowUser = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow;
                            BaseSizer::reportSizerOutput(state.dataUnitVentilators->cMO_UnitVentilator,
                                                         state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                         "Design Size Maximum Cold Water Flow [m3/s]",
                                                         MaxVolColdWaterFlowDes,
                                                         "User-Specified Maximum Cold Water Flow [m3/s]",
                                                         MaxVolColdWaterFlowUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                if ((std::abs(MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser) / MaxVolColdWaterFlowUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeUnitVentilator: Potential issue with equipment sizing for " + state.dataUnitVentilators->cMO_UnitVentilator + ' ' +
                                                state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                                    ShowContinueError(format("User-Specified Maximum Cold Water Flow of {:.5R} [m3/s]", MaxVolColdWaterFlowUser));
                                    ShowContinueError(
                                        format("differs from Design Size Maximum Cold Water Flow of {:.5R} [m3/s]", MaxVolColdWaterFlowDes));
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        }

        // set the design air flow rates for the heating and cooling coils
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
            CoolingCoilName = GetHXDXCoilName(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
            CoolingCoilType = GetHXCoilType(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, ErrorsFound);
        } else {
            CoolingCoilName = state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName;
            CoolingCoilType = state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilTypeCh;
        }
        WaterCoils::SetCoilDesFlow(CoolingCoilType, CoolingCoilName, state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow, ErrorsFound);
        WaterCoils::SetCoilDesFlow(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilTypeCh, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, state.dataUnitVentilators->UnitVent(UnitVentNum).MaxAirVolFlow, ErrorsFound);

        if (CurZoneEqNum > 0) {
            ZoneEqSizing(CurZoneEqNum).MaxHWVolFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolHotWaterFlow;
            ZoneEqSizing(CurZoneEqNum).MaxCWVolFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxVolColdWaterFlow;
        }

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
    }

    void CalcUnitVentilator(int &UnitVentNum,              // number of the current fan coil unit being simulated
                            int const ZoneNum,             // number of zone being served
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            Real64 &PowerMet,              // Sensible power supplied (W)
                            Real64 &LatOutputProvided      // Latent power supplied (kg/s), negative = dehumidification
    )
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine mainly controls the action of the unit ventilator
        // (or more exactly, it controls the amount of outside air brought in)
        // based on the user input for controls and the defined controls
        // algorithms.  There are currently (at the initial creation of this
        // subroutine) two control methods: variable percent (ASHRAE "Cycle I"
        // or "Cycle II") and fixed temperature (ASHRAE "Cycle III").

        // METHODOLOGY EMPLOYED:
        // Unit is controlled based on user input and what is happening in the
        // simulation.  There are various cases to consider:
        // 1. OFF: Unit is schedule off or there is no load on it.  All flow
        //    rates are set to zero and the temperatures are set to zone conditions
        //    (except for the outside air inlet).
        // 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
        //    and variable percent control is specified.  The outside air fraction
        //    is set to the minimum outside air fraction (schedule based) and the
        //    heating coil is activated.
        // 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
        //    and fixed temperature control is specified.  The outside air fraction
        //    is varied in an attempt to obtain a mixed air temperature equal to
        //    the user specified temperature (schedule based).  The heating coil
        //    is activated, if necessary.
        // 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
        //    coil is present or it has been scheduled off.  Set the amount of
        //    outside air based on the control type.  Simulate the "mixing box".
        // 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
        //    a cooling coil is present and scheduled on.  Tries to use outside
        //    air as best as possible and then calls a cooling coil
        // Note: controls are strictly temperature based and do not factor
        // humidity into the equation (not an enthalpy economy cycle but rather
        // a simple return air economy cycle).  In addition, temperature predictions
        // are not strict energy balances here in the control routine though
        // in the mixing routine an energy balance is preserved.

        // REFERENCES:
        // ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

        // Using/Aliasing
        using namespace DataZoneEnergyDemands;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::ZoneCompTurnFansOff;
        using DataHVACGlobals::ZoneCompTurnFansOn;
        using DataZoneEquipment::UnitVentilator_Num;
        using General::SolveRoot;
        using HeatingCoils::CheckHeatingCoilSchedule;
        using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
        using PlantUtilities::SetComponentFlowRate;
        using SteamCoils::CheckSteamCoilSchedule;
        using WaterCoils::CheckWaterCoilSchedule;

        Real64 const LowTempDiff(0.1); // Smallest allowed temperature difference for comparisons
        // (below this value the temperatures are assumed equal)
        Real64 const LowOAFracDiff(0.01); // Smallest allowed outside air fraction difference for comparison
        // (below this value the fractions are assumed equal)
        int const MaxIter(50); // maximum number of iterations

        Real64 AirMassFlow;   // air mass flow rate [kg/sec]
        int AirRelNode;       // outside air relief node
        int ControlNode;      // the hot water or cold water inlet node
        Real64 ControlOffset; // tolerance for output control
        int InletNode;        // unit air inlet node
        Real64 MaxOAFrac;     // maximum possible outside air fraction
        Real64 MaxWaterFlow;  // maximum water flow for heating or cooling [kg/sec]
        Real64 MinOAFrac;     // minimum possible outside air fraction
        Real64 MinWaterFlow;  // minimum water flow for heating or cooling [kg/sec]
        int OutletNode;       // unit air outlet node
        int OutsideAirNode;   // outside air node
        Real64 QTotUnitOut;   // total unit output [watts]
        Real64 QUnitOut;      // heating or sens. cooling provided by fan coil unit [watts]
        Real64 Tdesired;      // desired temperature after mixing inlet and outdoor air [degrees C]
        Real64 Tinlet;        // temperature of air coming into the unit ventilator [degrees C]
        Real64 Toutdoor;      // temperature of outdoor air being introduced into the unit ventilator [degrees C]
        Real64 MaxSteamFlow;
        Real64 MinSteamFlow;
        Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
        Real64 SpecHumOut;   // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn;    // Specific humidity ratio of inlet air (kg moisture / kg moist air)
        Real64 mdot;
        Array1D<Real64> Par(3); // parameters passed to RegulaFalsi function
        int OpMode;             // operatin gmode of the fan
        Real64 PartLoadFrac;    // part load ratio of the unit ventilator
        Real64 NoOutput;        // no load output of the unit ventilator
        Real64 FullOutput;      // full load output of the unit ventilator
        int SolFlag;            // return flag from RegulaFalsi for sensible load

        {
            auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).CoilOption);
            if (SELECT_CASE_var == state.dataUnitVentilators->BothOption) {

                {
                    auto const SELECT_CASE_var1(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                    if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_WaterCoilType) {
                        CheckWaterCoilSchedule("Coil:Heating:Water",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_SteamCoilType) {
                        CheckSteamCoilSchedule("Coil:Heating:Steam",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_ElectricCoilType) {
                        CheckHeatingCoilSchedule("Coil:Heating:Electric",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_GasCoilType) {
                        CheckHeatingCoilSchedule("Coil:Heating:Fuel",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else {
                        //      CALL ShowFatalError('Illegal coil type='//TRIM(state.dataUnitVentilators->UnitVent(UnitVentNum)%HCoilType))
                    }
                }

                {
                    auto const SELECT_CASE_var1(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType);

                    if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilWaterCooling) {
                        CheckWaterCoilSchedule("Coil:Cooling:Water",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilDetailedCooling) {
                        CheckWaterCoilSchedule("Coil:Cooling:Water:DetailedGeometry",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                        CheckHXAssistedCoolingCoilSchedule("CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else {
                        //      CALL ShowFatalError('Illegal coil type='//TRIM(state.dataUnitVentilators->UnitVent(UnitVentNum)%CCoilType))
                    }
                }

            } else if (SELECT_CASE_var == state.dataUnitVentilators->HeatingOption) {

                {
                    auto const SELECT_CASE_var1(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                    if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_WaterCoilType) {
                        CheckWaterCoilSchedule("Coil:Heating:Water",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_SteamCoilType) {
                        CheckSteamCoilSchedule("Coil:Heating:Steam",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_ElectricCoilType) {
                        CheckHeatingCoilSchedule("Coil:Heating:Electric",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Heating_GasCoilType) {
                        CheckHeatingCoilSchedule("Coil:Heating:Fuel",
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue,
                                                 state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    } else {
                        //      CALL ShowFatalError('Illegal coil type='//TRIM(state.dataUnitVentilators->UnitVent(UnitVentNum)%HCoilType))
                    }
                }

            } else if (SELECT_CASE_var == state.dataUnitVentilators->CoolingOption) {

                {
                    auto const SELECT_CASE_var1(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType);

                    if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilWaterCooling) {
                        CheckWaterCoilSchedule("Coil:Cooling:Water",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilDetailedCooling) {
                        CheckWaterCoilSchedule("Coil:Cooling:Water:DetailedGeometry",
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                               state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else if (SELECT_CASE_var1 == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                        CheckHXAssistedCoolingCoilSchedule("CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue,
                                                           state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                    } else {
                        //      CALL ShowFatalError('Illegal coil type='//TRIM(state.dataUnitVentilators->UnitVent(UnitVentNum)%CCoilType))
                    }
                }

            } else if (SELECT_CASE_var == state.dataUnitVentilators->NoneOption) {
            }
        }

        // initialize local variables
        ControlNode = 0;
        QUnitOut = 0.0;
        LatentOutput = 0.0;
        ControlOffset = 0.0;
        MaxWaterFlow = 0.0;
        MinWaterFlow = 0.0;
        NoOutput = 0.0;
        FullOutput = 0.0;
        SolFlag = 0; // # of iterations IF positive, -1 means failed to converge, -2 means bounds are incorrect
        InletNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        OutletNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode;
        OutsideAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode;
        AirRelNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirReliefNode;

        OpMode = state.dataUnitVentilators->UnitVent(UnitVentNum).OpMode;
        PartLoadFrac = 0.0;

        if ((std::abs(state.dataUnitVentilators->QZnReq) < SmallLoad) || (CurDeadBandOrSetback(ZoneNum)) || (GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).SchedPtr) <= 0) ||
            ((GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).FanAvailSchedPtr) <= 0 && !ZoneCompTurnFansOn) || ZoneCompTurnFansOff)) {

            // Unit is off or has no load upon it; set the flow rates to zero and then
            // simulate the components with the no flow conditions
            AirMassFlow = Node(OutletNode).MassFlowRate;
            state.dataUnitVentilators->HCoilOn = false;
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode > 0) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum);
            }
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode > 0) {
                mdot = 0.0;
                SetComponentFlowRate(mdot,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).ColdCoilOutNodeNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWCompNum);
            }

            if (OpMode == CycFanCycCoil) {
                CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac);
                if (Node(InletNode).MassFlowRateMax > 0.0) {
                    state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = Node(InletNode).MassFlowRate / Node(InletNode).MassFlowRateMax;
                }
            } else {
                CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut);
            }
        } else { // Unit is on-->this section is intended to control the outside air and the main
            //              result is to set the outside air flow rate variable OAMassFlowRate
            state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = 1.0;
            if (state.dataUnitVentilators->QZnReq > SmallLoad) { // HEATING MODE

                ControlNode = state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode;
                ControlOffset = state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlOffset;
                MaxWaterFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotWaterFlow;
                MinWaterFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotWaterFlow;
                MaxSteamFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotSteamFlow;
                MinSteamFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MinHotSteamFlow;
                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (!FirstHVACIteration && state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_WaterCoilType) {
                    MaxWaterFlow = Node(ControlNode).MassFlowRateMaxAvail;
                    MinWaterFlow = Node(ControlNode).MassFlowRateMinAvail;
                }

                if (!FirstHVACIteration && state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType == state.dataUnitVentilators->Heating_SteamCoilType) {
                    MaxSteamFlow = Node(ControlNode).MassFlowRateMaxAvail;
                    MinSteamFlow = Node(ControlNode).MassFlowRateMinAvail;
                }
                state.dataUnitVentilators->HCoilOn = true;

                if (Node(OutsideAirNode).MassFlowRate > 0.0) {
                    MinOAFrac = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedPtr) *
                                (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirMassFlow / Node(OutsideAirNode).MassFlowRate);
                } else {
                    MinOAFrac = 0.0;
                }
                MinOAFrac = min(1.0, max(0.0, MinOAFrac));

                if ((!state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) || (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilSchedValue <= 0.0)) {
                    // In heating mode, but there is no coil to provide heating.  This is handled
                    // differently than if there was a heating coil present.  Fixed temperature
                    // will still try to vary the amount of outside air to meet the desired
                    // mixed air temperature, while variable percent will go to full ventilation
                    // when it is most advantageous.

                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the minimum value
                            // which is equal to the maximum value, regardless of all the other conditions.

                            state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->VariablePercent) {
                            // This algorithm is probably a bit simplistic in that it just bounces
                            // back and forth between the maximum outside air and the minimum.  In
                            // REAL(r64)ity, a system *might* vary between the two based on the load in
                            // the zone.
                            Tinlet = Node(InletNode).Temp;
                            Toutdoor = Node(OutsideAirNode).Temp;

                            if (Tinlet >= Toutdoor) {

                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                            } else { // Tinlet < Toutdoor

                                MaxOAFrac = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr);
                                state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                            }

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->FixedTemperature) {
                            // In heating mode, the outside air for "fixed temperature" attempts
                            // to control the outside air fraction so that a desired temperature
                            // is met (if possible).  If this desired temperature is between the
                            // outside air temperature and the zone air temperature (inlet air
                            // temperature), then this is possible.  If not, the control will try
                            // to maximize the amount of air coming from the source that is closer
                            // in temperature to the desired temperature.
                            Tdesired = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr);
                            Tinlet = Node(InletNode).Temp;
                            Toutdoor = Node(OutsideAirNode).Temp;
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataUnitVentilators->OAMassFlowRate = ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * Node(InletNode).MassFlowRate;
                                state.dataUnitVentilators->OAMassFlowRate = max(state.dataUnitVentilators->OAMassFlowRate, (MinOAFrac * Node(OutsideAirNode).MassFlowRate));
                                state.dataUnitVentilators->OAMassFlowRate = min(state.dataUnitVentilators->OAMassFlowRate, (MaxOAFrac * Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError("ZoneHVAC:UnitVentilator simulation control: illogical condition for " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                            }
                        }
                    }

                    if (OpMode == CycFanCycCoil) {
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac);
                        if (Node(InletNode).MassFlowRateMax > 0.0) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = Node(InletNode).MassFlowRate / Node(InletNode).MassFlowRateMax;
                        }
                    } else {
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut);
                    }

                } else { //  Coil/no coil block
                    // There is a heating load and there is a heating coil present (presumably).
                    // Variable percent will throttle outside air back to the minimum while
                    // fixed temperature will still try to vary the outside air amount to meet
                    // the desired mixed air temperature.

                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->VariablePercent) {
                            // In heating mode, the outside air for "variable percent" control
                            // is set to the minimum value
                            state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->FixedTemperature) {
                            // In heating mode, the outside air for "fixed temperature" attempts
                            // to control the outside air fraction so that a desired temperature
                            // is met (if possible).  If this desired temperature is between the
                            // outside air temperature and the zone air temperature (inlet air
                            // temperature), then this is possible.  If not, the control will try
                            // to maximize the amount of air coming from the source that is closer
                            // in temperature to the desired temperature.
                            Tdesired = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr);
                            Tinlet = Node(InletNode).Temp;
                            Toutdoor = Node(OutsideAirNode).Temp;
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataUnitVentilators->OAMassFlowRate = ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * Node(InletNode).MassFlowRate;
                                state.dataUnitVentilators->OAMassFlowRate = max(state.dataUnitVentilators->OAMassFlowRate, (MinOAFrac * Node(OutsideAirNode).MassFlowRate));
                                state.dataUnitVentilators->OAMassFlowRate = min(state.dataUnitVentilators->OAMassFlowRate, (MaxOAFrac * Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError("ZoneHVAC:UnitVentilator simulation control: illogical condition for " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                            }
                        }
                    }

                    if (OpMode == CycFanCycCoil) {

                        // Find part load ratio of unit ventilator coils
                        PartLoadFrac = 0.0;
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, NoOutput, OpMode, PartLoadFrac);
                        if ((NoOutput - state.dataUnitVentilators->QZnReq) < SmallLoad) {
                            // Unit ventilator is unable to meet the load with coil off, set PLR = 1
                            PartLoadFrac = 1.0;
                            CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, FullOutput, OpMode, PartLoadFrac);
                            if ((FullOutput - state.dataUnitVentilators->QZnReq) > SmallLoad) {
                                // Unit ventilator full load capacity is able to meet the load, Find PLR
                                Par(1) = double(UnitVentNum);
                                Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                                if (FirstHVACIteration) Par(2) = 1.0;
                                Par(3) = double(OpMode);
                                // Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                                SolveRoot(0.001, MaxIter, SolFlag, PartLoadFrac, CalcUnitVentilatorResidual, 0.0, 1.0, Par);
                            }
                        }

                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac);
                        state.dataUnitVentilators->UnitVent(UnitVentNum).PartLoadFrac = PartLoadFrac;
                        state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = PartLoadFrac;

                    } else { // Not a cycling operating mode

                        {
                            auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                            if (SELECT_CASE_var == state.dataUnitVentilators->Heating_WaterCoilType) {
                                // control water flow to obtain output matching QZnReq
                                ControlCompOutput(state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                                  state.dataUnitVentilators->cMO_UnitVentilator,
                                                  UnitVentNum,
                                                  FirstHVACIteration,
                                                  state.dataUnitVentilators->QZnReq,
                                                  ControlNode,
                                                  MaxWaterFlow,
                                                  MinWaterFlow,
                                                  ControlOffset,
                                                  state.dataUnitVentilators->UnitVent(UnitVentNum).ControlCompTypeNum,
                                                  state.dataUnitVentilators->UnitVent(UnitVentNum).CompErrIndex,
                                                  _,
                                                  _,
                                                  _,
                                                  _,
                                                  _,
                                                  state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                                  state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                                  state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum);

                            } else if ((SELECT_CASE_var == state.dataUnitVentilators->Heating_GasCoilType) || (SELECT_CASE_var == state.dataUnitVentilators->Heating_ElectricCoilType) ||
                                       (SELECT_CASE_var == state.dataUnitVentilators->Heating_SteamCoilType)) {

                                CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut);
                            }
                        }
                    }
                } //  Coil/no coil block

            } else { // COOLING MODE

                ControlNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode;
                ControlOffset = state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlOffset;
                MaxWaterFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxColdWaterFlow;
                MinWaterFlow = state.dataUnitVentilators->UnitVent(UnitVentNum).MinColdWaterFlow;
                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if ((!FirstHVACIteration) && (ControlNode > 0) && (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent)) {
                    MaxWaterFlow = Node(ControlNode).MassFlowRateMaxAvail;
                    MinWaterFlow = Node(ControlNode).MassFlowRateMinAvail;
                }
                state.dataUnitVentilators->HCoilOn = false;

                Tinlet = Node(InletNode).Temp;
                Toutdoor = Node(OutsideAirNode).Temp;

                if (Node(OutsideAirNode).MassFlowRate > 0.0) {
                    MinOAFrac = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).MinOASchedPtr) *
                                (state.dataUnitVentilators->UnitVent(UnitVentNum).MinOutAirMassFlow / Node(OutsideAirNode).MassFlowRate);
                } else {
                    MinOAFrac = 0.0;
                }
                MinOAFrac = min(1.0, max(0.0, MinOAFrac));

                if ((!state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) || (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilSchedValue <= 0.0)) {
                    // In cooling mode, but there is no coil to provide cooling.  This is handled
                    // differently than if there was a cooling coil present.  Fixed temperature
                    // will still try to vary the amount of outside air to meet the desired
                    // mixed air temperature, while variable percent will go to full ventilation
                    // when it is most advantageous.
                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->VariablePercent) {

                            state.dataUnitVentilators->OAMassFlowRate = SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,
                                                                                        MinOAFrac,
                                                                                        Node(OutsideAirNode).MassFlowRate,
                                                                                        GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr),
                                                                                        Tinlet,
                                                                                        Toutdoor);

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr);
                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataUnitVentilators->OAMassFlowRate = ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * Node(InletNode).MassFlowRate;
                                state.dataUnitVentilators->OAMassFlowRate = max(state.dataUnitVentilators->OAMassFlowRate, (MinOAFrac * Node(OutsideAirNode).MassFlowRate));
                                state.dataUnitVentilators->OAMassFlowRate = min(state.dataUnitVentilators->OAMassFlowRate, (MaxOAFrac * Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError("ZoneHVAC:UnitVentilator simulation control: illogical condition for " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                            }
                        }
                    }

                    if (OpMode == CycFanCycCoil) {
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac);
                        if (Node(InletNode).MassFlowRateMax > 0.0) {
                            state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = Node(InletNode).MassFlowRate / Node(InletNode).MassFlowRateMax;
                        }
                    } else {
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut);
                    }

                } else {
                    // There is a cooling load and there is a cooling coil present (presumably).
                    // Variable percent will throttle outside air back to the minimum while
                    // fixed temperature will still try to vary the outside air amount to meet
                    // the desired mixed air temperature.

                    {
                        auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).OAControlType);

                        if (SELECT_CASE_var == state.dataUnitVentilators->FixedOAControl) {
                            // In this control type, the outdoor air flow rate is fixed to the maximum value
                            // which is equal to the minimum value, regardless of all the other conditions.
                            state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->VariablePercent) {

                            state.dataUnitVentilators->OAMassFlowRate = SetOAMassFlowRateForCoolingVariablePercent(UnitVentNum,
                                                                                        MinOAFrac,
                                                                                        Node(OutsideAirNode).MassFlowRate,
                                                                                        GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).MaxOASchedPtr),
                                                                                        Tinlet,
                                                                                        Toutdoor);

                        } else if (SELECT_CASE_var == state.dataUnitVentilators->FixedTemperature) {
                            // This is basically the same algorithm as for the heating case...
                            Tdesired = GetCurrentScheduleValue(state.dataUnitVentilators->UnitVent(UnitVentNum).TempSchedPtr);

                            MaxOAFrac = 1.0;

                            if (std::abs(Tinlet - Toutdoor) <= LowTempDiff) { // no difference in indoor and outdoor conditions-->set OA to minimum
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (std::abs(MaxOAFrac - MinOAFrac) <= LowOAFracDiff) { // no difference in outside air fractions
                                state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                            } else if (((Tdesired <= Tinlet) && (Tdesired >= Toutdoor)) || ((Tdesired >= Tinlet) && (Tdesired <= Toutdoor))) {
                                // Desired temperature is between the inlet and outdoor temperatures
                                // so vary the flow rate between no outside air and no recirculation air
                                // then applying the maximum and minimum limits the user has scheduled
                                // to make sure too much/little outside air is being introduced
                                state.dataUnitVentilators->OAMassFlowRate = ((Tdesired - Tinlet) / (Toutdoor - Tinlet)) * Node(InletNode).MassFlowRate;
                                state.dataUnitVentilators->OAMassFlowRate = max(state.dataUnitVentilators->OAMassFlowRate, (MinOAFrac * Node(OutsideAirNode).MassFlowRate));
                                state.dataUnitVentilators->OAMassFlowRate = min(state.dataUnitVentilators->OAMassFlowRate, (MaxOAFrac * Node(OutsideAirNode).MassFlowRate));
                            } else if ((Tdesired < Tinlet) && (Tdesired < Toutdoor)) {
                                // Desired temperature is below both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet < Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else if ((Tdesired > Tinlet) && (Tdesired > Toutdoor)) {
                                // Desired temperature is above both the inlet and outdoor temperatures
                                // so use whichever flow rate (max or min) that will get closer
                                if (Tinlet > Toutdoor) { // Tinlet closer to Tdesired so use minimum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MinOAFrac * Node(OutsideAirNode).MassFlowRate;
                                } else { // Toutdoor closer to Tdesired so use maximum outside air
                                    state.dataUnitVentilators->OAMassFlowRate = MaxOAFrac * Node(OutsideAirNode).MassFlowRate;
                                }
                            } else {
                                // It should NEVER get to this point, but just in case...
                                ShowFatalError("ZoneHVAC:UnitVentilator simulation control: illogical condition for " + state.dataUnitVentilators->UnitVent(UnitVentNum).Name);
                            }
                        }
                    }

                    if (OpMode == CycFanCycCoil) {

                        state.dataUnitVentilators->HCoilOn = false;
                        // Find part load ratio of unit ventilator coils
                        PartLoadFrac = 0.0;
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, NoOutput, OpMode, PartLoadFrac);
                        if ((NoOutput - state.dataUnitVentilators->QZnReq) > SmallLoad) {
                            // Unit ventilator is unable to meet the load with coil off, set PLR = 1
                            PartLoadFrac = 1.0;
                            CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, FullOutput, OpMode, PartLoadFrac);
                            if ((FullOutput - state.dataUnitVentilators->QZnReq) < SmallLoad) {
                                // Unit ventilator full load capacity is able to meet the load, Find PLR
                                Par(1) = double(UnitVentNum);
                                Par(2) = 0.0; // FLAG, IF 1.0 then FirstHVACIteration equals TRUE, if 0.0 then FirstHVACIteration equals false
                                if (FirstHVACIteration) Par(2) = 1.0;
                                Par(3) = double(OpMode);
                                // Tolerance is in fraction of load, MaxIter = 30, SolFalg = # of iterations or error as appropriate
                                SolveRoot(0.001, MaxIter, SolFlag, PartLoadFrac, CalcUnitVentilatorResidual, 0.0, 1.0, Par);
                            }
                        }
                        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadFrac);
                        state.dataUnitVentilators->UnitVent(UnitVentNum).PartLoadFrac = PartLoadFrac;
                        state.dataUnitVentilators->UnitVent(UnitVentNum).FanPartLoadRatio = PartLoadFrac;

                    } else { // NOT a cycling operating mode
                        // control water flow to obtain output matching QZnReq
                        state.dataUnitVentilators->HCoilOn = false;
                        ControlCompOutput(state.dataUnitVentilators->UnitVent(UnitVentNum).Name,
                                          state.dataUnitVentilators->cMO_UnitVentilator,
                                          UnitVentNum,
                                          FirstHVACIteration,
                                          state.dataUnitVentilators->QZnReq,
                                          ControlNode,
                                          MaxWaterFlow,
                                          MinWaterFlow,
                                          ControlOffset,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).ControlCompTypeNum,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).CompErrIndex,
                                          _,
                                          _,
                                          _,
                                          _,
                                          _,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide,
                                          state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum);

                    } // end from IF (OpMode .EQ. CycFanCycCoil) THEN
                }

            } // ...end of HEATING/COOLING IF-THEN block

            AirMassFlow = Node(OutletNode).MassFlowRate;
            QUnitOut =
                AirMassFlow * (PsyHFnTdbW(Node(OutletNode).Temp, Node(InletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));

        } // ...end of unit ON/OFF IF-THEN block

        // CR9155 Remove specific humidity calculations
        SpecHumOut = Node(OutletNode).HumRat;
        SpecHumIn = Node(InletNode).HumRat;
        LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative

        QTotUnitOut = AirMassFlow * (Node(OutletNode).Enthalpy - Node(InletNode).Enthalpy);

        // Report variables...
        state.dataUnitVentilators->UnitVent(UnitVentNum).HeatPower = max(0.0, QUnitOut);
        state.dataUnitVentilators->UnitVent(UnitVentNum).SensCoolPower = std::abs(min(0.0, QUnitOut));
        state.dataUnitVentilators->UnitVent(UnitVentNum).TotCoolPower = std::abs(min(0.0, QTotUnitOut));
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            state.dataUnitVentilators->UnitVent(UnitVentNum).ElecPower = Fans::GetFanPower(state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index);
        } else {
            state.dataUnitVentilators->UnitVent(UnitVentNum).ElecPower = HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->fanPower();
        }

        PowerMet = QUnitOut;
        LatOutputProvided = LatentOutput;
    }

    void CalcUnitVentilatorComponents(int const UnitVentNum,              // Unit index in unit ventilator array
                                      bool const FirstHVACIteration,      // flag for 1st HVAV iteration in the time step
                                      Real64 &LoadMet,                    // load met by unit (watts)
                                      Optional_int_const OpMode,          // Fan Type
                                      Optional<Real64 const> PartLoadFrac // Part Load Ratio of coil and fan
    )
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine launches the individual component simulations.
        // This is called either when the unit is off to carry null conditions
        // through the unit or during control iterations to continue updating
        // what is going on within the unit.

        // METHODOLOGY EMPLOYED:
        // Simply calls the different components in order.  Only slight wrinkles
        // here are that the unit ventilator has it's own outside air mixed and
        // that a cooling coil must be present in order to call a cooling coil
        // simulation.  Other than that, the subroutine is very straightforward.

        using DataHVACGlobals::FanType_SimpleOnOff;
        using DataHVACGlobals::ZoneCompTurnFansOff;
        using DataHVACGlobals::ZoneCompTurnFansOn;
        using DataZoneEquipment::UnitVentilator_Num;
        using DataZoneEquipment::ZoneEquipConfig;
        using HeatingCoils::SimulateHeatingCoilComponents;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using PlantUtilities::SetComponentFlowRate;
        using SingleDuct::SimATMixer;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        Real64 AirMassFlow;           // total mass flow through the unit
        Real64 CpAirZn;               // specific heat of dry air at zone conditions (zone conditions same as unit inlet)
        int HCoilInAirNode;           // inlet node number for fan exit/coil inlet
        int InletNode;                // unit air inlet node
        int OutletNode;               // unit air outlet node
        Real64 QCoilReq;              // Heat addition required from an electric/gas heating coil
        Real64 mdot;                  // hot water or steam mass flow rate for current time step
        Real64 PartLoadRatio;         // unit ventilator part load ratio
        int FanOpMode;                // fan operating mode or fan type
        static int ATMixOutNode(0);   // outlet node of ATM Mixer
        static int ATMixerPriNode(0); // primary air node of ATM Mixer
        static int ZoneNode(0);       // zone node
        Real64 SpecHumMin(0);         // Specific humidity ratio of inlet air (kg moisture / kg moist air)

        // FLOW:
        InletNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        OutletNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode;
        ZoneNode = ZoneEquipConfig(state.dataUnitVentilators->UnitVent(UnitVentNum).ZonePtr).ZoneNode;
        QCoilReq = state.dataUnitVentilators->QZnReq;

        if (present(PartLoadFrac)) {
            PartLoadRatio = PartLoadFrac;
        } else {
            PartLoadRatio = 1.0;
        }
        if (present(OpMode)) {
            FanOpMode = OpMode;
        } else {
            FanOpMode = ContFanCycCoil;
        }

        if (FanOpMode != CycFanCycCoil) {

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                ATMixOutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode;
                ATMixerPriNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerPriNode;
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide) {
                    // set the primary air inlet mass flow rate
                    Node(ATMixerPriNode).MassFlowRate =
                        min(min(Node(ATMixerPriNode).MassFlowRateMaxAvail, state.dataUnitVentilators->OAMassFlowRate), Node(InletNode).MassFlowRate);
                    // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                    SimATMixer(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex);
                }
            } else {
                SimUnitVentOAMixer(UnitVentNum, FanOpMode);
            }
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                            FirstHVACIteration,
                                            state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index,
                                            _,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                DataHVACGlobals::OnOffFanPartLoadFraction = 1.0; // used for cycling fan, set to 1.0 to be sure
                HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                    SimHXAssistedCoolingCoil(
                        state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, FirstHVACIteration, state.dataUnitVentilators->On, 0.0, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index, ContFanCycCoil);
                } else {
                    SimulateWaterCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index);
                }
            }

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {

                {
                    auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                    if (SELECT_CASE_var == state.dataUnitVentilators->Heating_WaterCoilType) {

                        SimulateWaterCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);

                    } else if (SELECT_CASE_var == state.dataUnitVentilators->Heating_SteamCoilType) {

                        if (!state.dataUnitVentilators->HCoilOn) {
                            QCoilReq = 0.0;
                        } else {
                            HCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
                            CpAirZn = PsyCpAirFnW(Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).HumRat);
                            QCoilReq = state.dataUnitVentilators->QZnReq - Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                                    (Node(HCoilInAirNode).Temp - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Temp);
                        }

                        if (QCoilReq < 0.0) QCoilReq = 0.0; // a heating coil can only heat, not cool

                        SimulateSteamCoilComponents(
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index, QCoilReq);

                    } else if ((SELECT_CASE_var == state.dataUnitVentilators->Heating_ElectricCoilType) || (SELECT_CASE_var == state.dataUnitVentilators->Heating_GasCoilType)) {

                        if (!state.dataUnitVentilators->HCoilOn) {
                            QCoilReq = 0.0;
                        } else {
                            HCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
                            CpAirZn = PsyCpAirFnW(Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).HumRat);
                            QCoilReq = state.dataUnitVentilators->QZnReq - Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                                    (Node(HCoilInAirNode).Temp - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Temp);
                        }

                        if (QCoilReq < 0.0) QCoilReq = 0.0; // a heating coil can only heat, not cool

                        SimulateHeatingCoilComponents(
                            state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName, FirstHVACIteration, QCoilReq, state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index);
                    }
                }

            } // (state.dataUnitVentilators->UnitVent(UnitVentNum)%HCoilPresent)

        } else { // Fan is Fan:OnOff and is cycling

            Node(InletNode).MassFlowRate = Node(InletNode).MassFlowRateMax * PartLoadRatio;
            AirMassFlow = Node(InletNode).MassFlowRate;
            // Set the fan inlet node maximum available mass flow rates for cycling fans
            Node(InletNode).MassFlowRateMaxAvail = AirMassFlow;

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                ATMixOutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode;
                ATMixerPriNode = state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerPriNode;
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide) {
                    // set the primary air inlet mass flow rate
                    Node(ATMixerPriNode).MassFlowRate =
                        min(min(Node(ATMixerPriNode).MassFlowRateMaxAvail, state.dataUnitVentilators->OAMassFlowRate), Node(InletNode).MassFlowRate);
                    // now calculate the mixer outlet conditions (and the secondary air inlet flow rate)
                    SimATMixer(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex);
                }
            } else {
                SimUnitVentOAMixer(UnitVentNum, FanOpMode);
            }
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).FanName,
                                            FirstHVACIteration,
                                            state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index,
                                            _,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                HVACFan::fanObjs[state.dataUnitVentilators->UnitVent(UnitVentNum).Fan_Index]->simulate(_, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilPresent) {

                CalcMdotCCoilCycFan(mdot, QCoilReq, state.dataUnitVentilators->QZnReq, UnitVentNum, PartLoadRatio);
                SetComponentFlowRate(mdot,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).ColdControlNode,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).ColdCoilOutNodeNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWLoopSide,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWBranchNum,
                                     state.dataUnitVentilators->UnitVent(UnitVentNum).CWCompNum);

                if (state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilType == state.dataUnitVentilators->Cooling_CoilHXAssisted) {
                    SimHXAssistedCoolingCoil(
                        state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName, FirstHVACIteration, state.dataUnitVentilators->On, PartLoadRatio, state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index, FanOpMode);
                } else {
                    SimulateWaterCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).CCoilName,
                                                FirstHVACIteration,
                                                state.dataUnitVentilators->UnitVent(UnitVentNum).CCoil_Index,
                                                QCoilReq,
                                                FanOpMode,
                                                PartLoadRatio);
                }
            }

            if (state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilPresent) {

                {
                    auto const SELECT_CASE_var(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilType);

                    if (SELECT_CASE_var == state.dataUnitVentilators->Heating_WaterCoilType) {
                        if (!state.dataUnitVentilators->HCoilOn) {
                            QCoilReq = 0.0;
                            mdot = 0.0;
                        } else {
                            HCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
                            CpAirZn = PsyCpAirFnW(Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).HumRat);
                            QCoilReq = state.dataUnitVentilators->QZnReq - Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                                    (Node(HCoilInAirNode).Temp - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Temp);
                            mdot = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotWaterFlow * PartLoadRatio;
                        }

                        if (QCoilReq < 0.0) QCoilReq = 0.0; // a heating coil can only heat, not cool
                        SetComponentFlowRate(mdot,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum);
                        SimulateWaterCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                    FirstHVACIteration,
                                                    state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index,
                                                    QCoilReq,
                                                    FanOpMode,
                                                    PartLoadRatio);

                    } else if (SELECT_CASE_var == state.dataUnitVentilators->Heating_SteamCoilType) {

                        if (!state.dataUnitVentilators->HCoilOn) {
                            QCoilReq = 0.0;
                            mdot = 0.0;
                        } else {
                            HCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
                            CpAirZn = PsyCpAirFnW(Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).HumRat);
                            QCoilReq = state.dataUnitVentilators->QZnReq - Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                                    (Node(HCoilInAirNode).Temp - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Temp);
                            mdot = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxHotSteamFlow * PartLoadFrac;
                        }

                        if (QCoilReq < 0.0) QCoilReq = 0.0;
                        SetComponentFlowRate(mdot,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HotControlNode,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HotCoilOutNodeNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWLoopSide,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWBranchNum,
                                             state.dataUnitVentilators->UnitVent(UnitVentNum).HWCompNum);
                        SimulateSteamCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                    FirstHVACIteration,
                                                    state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index,
                                                    QCoilReq,
                                                    _,
                                                    FanOpMode,
                                                    PartLoadRatio);

                    } else if ((SELECT_CASE_var == state.dataUnitVentilators->Heating_ElectricCoilType) || (SELECT_CASE_var == state.dataUnitVentilators->Heating_GasCoilType)) {

                        if (!state.dataUnitVentilators->HCoilOn) {
                            QCoilReq = 0.0;
                        } else {
                            HCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
                            CpAirZn = PsyCpAirFnW(Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).HumRat);
                            QCoilReq = state.dataUnitVentilators->QZnReq - Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                                    (Node(HCoilInAirNode).Temp - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Temp);
                        }
                        if (QCoilReq < 0.0) QCoilReq = 0.0;
                        SimulateHeatingCoilComponents(state.dataUnitVentilators->UnitVent(UnitVentNum).HCoilName,
                                                      FirstHVACIteration,
                                                      QCoilReq,
                                                      state.dataUnitVentilators->UnitVent(UnitVentNum).HCoil_Index,
                                                      _,
                                                      _,
                                                      FanOpMode,
                                                      PartLoadRatio);
                    }
                }
            }
        }
        AirMassFlow = Node(OutletNode).MassFlowRate;
        // calculate delivered load
        if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
            if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_SupplySide) {
                // set the primary air inlet mass flow rate
                Node(ATMixerPriNode).MassFlowRate = min(Node(ATMixerPriNode).MassFlowRateMaxAvail, state.dataUnitVentilators->OAMassFlowRate);
                // now calculate the the mixer outlet conditions (and the secondary air inlet flow rate)
                SimATMixer(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerName, FirstHVACIteration, state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerIndex);
                SpecHumMin = min(Node(ATMixOutNode).HumRat, Node(InletNode).HumRat);
                LoadMet = Node(ATMixOutNode).MassFlowRate *
                          (PsyHFnTdbW(Node(ATMixOutNode).Temp, SpecHumMin) - PsyHFnTdbW(Node(InletNode).Temp, SpecHumMin));
            } else {
                // ATM Mixer on inlet side
                LoadMet =
                    AirMassFlow * (PsyHFnTdbW(Node(OutletNode).Temp, Node(ZoneNode).HumRat) - PsyHFnTdbW(Node(ZoneNode).Temp, Node(ZoneNode).HumRat));
            }
        } else {
            LoadMet =
                AirMassFlow * (PsyHFnTdbW(Node(OutletNode).Temp, Node(InletNode).HumRat) - PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));
        }
    }

    void SimUnitVentOAMixer(int const UnitVentNum, // Unit index in unit ventilator array
                            int const FanOpMode    // unit ventilator fan operating mode
    )
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This responsibility of this subroutine is to set the air flow rates
        // through the mixing box portion of the unit ventilator and then perform
        // an energy balance to arrive at outlet conditions which then would
        // serve as inlet conditions to the coils (or outlet conditions for
        // the device).  There is some question as to whether this needs to be
        // called every time the coils and fan are called since how the fans and
        // coil operate won't presumable change how the mixer operates.  The
        // method in which this routine is called is slightly cleaner though
        // from a code readability standpoint though less efficient.

        // METHODOLOGY EMPLOYED:
        // The OAMassFlowRate has already been calculated in the main control
        // algorithm.  Use this flow rate to establish all of the other flow
        // rates and perform an energy balance on the mixing of the return and
        // outdoor air streams.

        int AirRelNode;            // relief air node number in unit ventilator loop
        int InletNode;             // inlet node number for unit ventilator loop
        Real64 OAFraction;         // Outside air fraction of inlet air
        int OAMixOutNode;          // outside air mixer outlet node for unit ventilator loop
        int OutsideAirNode;        // outside air node number in unit ventilator loop
        Real64 OutAirMassFlowRate; // Outside air mass flow rate capped for cycling fan

        // FLOW:
        AirRelNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirReliefNode;
        InletNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        OAMixOutNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode;
        OutsideAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode;
        OutAirMassFlowRate = state.dataUnitVentilators->OAMassFlowRate;

        // Limit the outdoor air mass flow rate if cycling fan
        if (FanOpMode == CycFanCycCoil) {
            OutAirMassFlowRate = min(state.dataUnitVentilators->OAMassFlowRate, Node(InletNode).MassFlowRate);
        }

        // "Resolve" the air flow rates...
        Node(OutsideAirNode).MassFlowRate = OutAirMassFlowRate;
        Node(OutsideAirNode).MassFlowRateMinAvail = OutAirMassFlowRate;
        Node(OutsideAirNode).MassFlowRateMaxAvail = OutAirMassFlowRate;

        Node(AirRelNode).MassFlowRate = OutAirMassFlowRate;
        Node(AirRelNode).MassFlowRateMinAvail = OutAirMassFlowRate;
        Node(AirRelNode).MassFlowRateMaxAvail = OutAirMassFlowRate;

        Node(OAMixOutNode).MassFlowRate = Node(InletNode).MassFlowRate;
        Node(OAMixOutNode).MassFlowRateMinAvail = Node(InletNode).MassFlowRate;
        Node(OAMixOutNode).MassFlowRateMaxAvail = Node(InletNode).MassFlowRate;

        // "Inlet" conditions for InletNode and OutsideAirNode have already
        // been set elsewhere so we just need to set the "outlet" conditions
        Node(AirRelNode).Temp = Node(InletNode).Temp;
        Node(AirRelNode).Press = Node(InletNode).Press;
        Node(AirRelNode).HumRat = Node(InletNode).HumRat;
        Node(AirRelNode).Enthalpy = Node(InletNode).Enthalpy;

        if (Node(InletNode).MassFlowRate > 0.0) {
            OAFraction = Node(OutsideAirNode).MassFlowRate / Node(InletNode).MassFlowRate;
        } else {
            OAFraction = 0.0;
        }

        // Perform an energy and moisture mass balance on the mixing portion of the unit ventilator
        Node(OAMixOutNode).Enthalpy = OAFraction * Node(OutsideAirNode).Enthalpy + (1.0 - OAFraction) * Node(InletNode).Enthalpy;
        Node(OAMixOutNode).HumRat = OAFraction * Node(OutsideAirNode).HumRat + (1.0 - OAFraction) * Node(InletNode).HumRat;

        // Find the other key state points based on calculated conditions
        Node(OAMixOutNode).Temp = PsyTdbFnHW(Node(OAMixOutNode).Enthalpy, Node(OAMixOutNode).HumRat);
        Node(OAMixOutNode).Press = Node(InletNode).Press;

        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            Node(AirRelNode).CO2 = Node(InletNode).CO2;
            Node(OAMixOutNode).CO2 = OAFraction * Node(OutsideAirNode).CO2 + (1.0 - OAFraction) * Node(InletNode).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            Node(AirRelNode).GenContam = Node(InletNode).GenContam;
            Node(OAMixOutNode).GenContam = OAFraction * Node(OutsideAirNode).GenContam + (1.0 - OAFraction) * Node(InletNode).GenContam;
        }
    }

    // SUBROUTINE UpdateUnitVentilator

    // No update routine needed in this module since all of the updates happen on
    // the Node derived type directly and these updates are done by other routines.

    // END SUBROUTINE UpdateUnitVentilator

    void ReportUnitVentilator(int const UnitVentNum) // Unit index in unit ventilator array
    {
EnergyPlusData & state = getCurrentState();
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine needs a description.

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // Using/Aliasing
        using DataHVACGlobals::TimeStepSys;

        state.dataUnitVentilators->UnitVent(UnitVentNum).HeatEnergy = state.dataUnitVentilators->UnitVent(UnitVentNum).HeatPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataUnitVentilators->UnitVent(UnitVentNum).SensCoolEnergy = state.dataUnitVentilators->UnitVent(UnitVentNum).SensCoolPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataUnitVentilators->UnitVent(UnitVentNum).TotCoolEnergy = state.dataUnitVentilators->UnitVent(UnitVentNum).TotCoolPower * TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataUnitVentilators->UnitVent(UnitVentNum).ElecEnergy = state.dataUnitVentilators->UnitVent(UnitVentNum).ElecPower * TimeStepSys * DataGlobalConstants::SecInHour;

        if (state.dataUnitVentilators->UnitVent(UnitVentNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(DataSizing::CurZoneEqNum, 0, state.dataUnitVentilators->UnitVent(UnitVentNum).FirstPass);
            }
        }
    }

    int GetUnitVentilatorOutAirNode(int const UnitVentNum)
    {
EnergyPlusData & state = getCurrentState();
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node

        int GetUnitVentilatorOutAirNode;

        if (state.dataUnitVentilators->GetUnitVentilatorInputFlag) {
            GetUnitVentilatorInput();
            state.dataUnitVentilators->GetUnitVentilatorInputFlag = false;
        }

        GetUnitVentilatorOutAirNode = 0;
        if (UnitVentNum > 0 && UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents) {
            GetUnitVentilatorOutAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OutsideAirNode;
        }

        return GetUnitVentilatorOutAirNode;
    }

    int GetUnitVentilatorZoneInletAirNode(int const UnitVentNum)
    {
EnergyPlusData & state = getCurrentState();
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for zone air inlet node

        int GetUnitVentilatorZoneInletAirNode;

        if (state.dataUnitVentilators->GetUnitVentilatorInputFlag) {
            GetUnitVentilatorInput();
            state.dataUnitVentilators->GetUnitVentilatorInputFlag = false;
        }

        GetUnitVentilatorZoneInletAirNode = 0;
        if (UnitVentNum > 0 && UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents) {
            GetUnitVentilatorZoneInletAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirOutNode;
        }

        return GetUnitVentilatorZoneInletAirNode;
    }

    int GetUnitVentilatorMixedAirNode(int const UnitVentNum)
    {
EnergyPlusData & state = getCurrentState();
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for mixed air node

        int GetUnitVentilatorMixedAirNode;

        if (state.dataUnitVentilators->GetUnitVentilatorInputFlag) {
            GetUnitVentilatorInput();
            state.dataUnitVentilators->GetUnitVentilatorInputFlag = false;
        }

        GetUnitVentilatorMixedAirNode = 0;
        if (UnitVentNum > 0 && UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents) {
            GetUnitVentilatorMixedAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode;
        }

        return GetUnitVentilatorMixedAirNode;
    }

    int GetUnitVentilatorReturnAirNode(int const UnitVentNum)
    {
EnergyPlusData & state = getCurrentState();
        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for return air node into "mixer"

        int GetUnitVentilatorReturnAirNode;

        if (state.dataUnitVentilators->GetUnitVentilatorInputFlag) {
            GetUnitVentilatorInput();
            state.dataUnitVentilators->GetUnitVentilatorInputFlag = false;
        }

        GetUnitVentilatorReturnAirNode = 0;
        if (UnitVentNum > 0 && UnitVentNum <= state.dataUnitVentilators->NumOfUnitVents) {
            GetUnitVentilatorReturnAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        }

        return GetUnitVentilatorReturnAirNode;
    }

    Real64 CalcUnitVentilatorResidual(Real64 const PartLoadRatio, // Coil Part Load Ratio
                                      Array1D<Real64> const &Par  // Function parameters
    )
    {
        EnergyPlusData & state = getCurrentState();
        // FUNCTION INFORMATION:
        //       AUTHOR         Bereket Nigusse, FSEC
        //       DATE WRITTEN   October 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na
        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the part-load ratio for the unit ventilator.
        // METHODOLOGY EMPLOYED:
        // Use SolveRoot to call this Function to converge on a solution

        // Return value
        Real64 Residuum(0.0); // Result (force to 0)

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:
        //  Parameter description:
        //  Par(1)  = REAL(UnitVentNum,r64)   ! Index to Unit Ventilator
        //  Par(2)  = 0.0                     ! FirstHVACIteration FLAG, IF 1.0 then TRUE, if 0.0 then FALSE
        //  Par(3)  = REAL(OpMode,r64)        ! Fan control, IF 1.0 then cycling fan, if 0.0 then continuous fan

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int UnitVentNum;         // Index to this UnitHeater
        bool FirstHVACIteration; // FirstHVACIteration flag
        int OpMode;              // Cycling fan or constant fan
        Real64 QUnitOut;         // heating/Cooling provided by unit ventilator [watts]

        // Convert parameters to usable variables
        UnitVentNum = int(Par(1));
        FirstHVACIteration = (Par(2) == 1.0);
        OpMode = int(Par(3));
        CalcUnitVentilatorComponents(UnitVentNum, FirstHVACIteration, QUnitOut, OpMode, PartLoadRatio);
        // Calculate residual based on output calculation flag
        if (state.dataUnitVentilators->QZnReq != 0.0) {
            Residuum = (QUnitOut - state.dataUnitVentilators->QZnReq) / state.dataUnitVentilators->QZnReq;
        }
        return Residuum;
    }

    Real64 SetOAMassFlowRateForCoolingVariablePercent(int const UnitVentNum,     // Unit Ventilator index
                                                      Real64 const MinOAFrac,    // Minimum Outside Air Fraction
                                                      Real64 const MassFlowRate, // Design Outside Air Mass Flow Rate
                                                      Real64 const MaxOAFrac,    // Maximum Outside Air Fraction
                                                      Real64 const Tinlet,       // Inlet Temperature to Unit or Zone Temperature
                                                      Real64 const Toutdoor      // Outdoor Air Temperature
    )
    {
EnergyPlusData & state = getCurrentState();
        Real64 ActualOAMassFlowRate(0.0); // Result or return value

        if (Tinlet <= Toutdoor) {

            ActualOAMassFlowRate = MinOAFrac * MassFlowRate;

        } else { // Tinlet > Toutdoor
            // Use cooler outside air to provide "free" cooling without over-cooling.
            // First, use a simple load equals mass flow times Cp time Delta T equation to find OA Mass Flow Rate.
            // This must include the enthalpy difference across the fan.  Otherwise, this will potentially put a
            // small load on the cooling coil (if it exists) or will leave a small load that is not met when it could be.
            // Then, limit the OA Mass Flow Rate between the MinOA flow and the MaxOA flow.

            Real64 EnthDiffAcrossFan(0.0); // Temperature difference across the fan
            if (!state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerExists) {
                EnthDiffAcrossFan = Node(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode).Enthalpy - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).OAMixerOutNode).Enthalpy;
            } else {
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_InletSide) {
                    EnthDiffAcrossFan = Node(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode).Enthalpy - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerOutNode).Enthalpy;
                }
                if (state.dataUnitVentilators->UnitVent(UnitVentNum).ATMixerType == ATMixer_SupplySide) {
                    EnthDiffAcrossFan = Node(state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode).Enthalpy - Node(state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode).Enthalpy;
                }
            }

            ActualOAMassFlowRate =
                (std::abs(state.dataUnitVentilators->QZnReq) + (MassFlowRate * std::abs(EnthDiffAcrossFan))) / (PsyCpAirFnW(state.dataEnvrn->OutHumRat) * (Tinlet - Toutdoor));

            ActualOAMassFlowRate = max(ActualOAMassFlowRate, (MinOAFrac * MassFlowRate));
            ActualOAMassFlowRate = min(ActualOAMassFlowRate, (MaxOAFrac * MassFlowRate));
        }

        return ActualOAMassFlowRate;
    }

    void CalcMdotCCoilCycFan(Real64 &mdot,              // mass flow rate
                             Real64 &QCoilReq,          // Remaining load to cooling coil
                             Real64 const QZnReq,       // Zone load to setpoint
                             int const UnitVentNum,     // Unit Ventilator index
                             Real64 const PartLoadRatio // Part load ratio for unit ventilator
    )
    {
EnergyPlusData & state = getCurrentState();
        if (QZnReq >= 0.0) { // Heating requested so no cooling coil needed
            mdot = 0.0;
        } else { // Cooling so set first guess at flow rate
            mdot = state.dataUnitVentilators->UnitVent(UnitVentNum).MaxColdWaterFlow * PartLoadRatio;
        }

        // Check to see what outside air will do, "turn off" cooling coil if OA can handle the load
        int CCoilInAirNode = state.dataUnitVentilators->UnitVent(UnitVentNum).FanOutletNode;
        int AirInNode = state.dataUnitVentilators->UnitVent(UnitVentNum).AirInNode;
        Real64 const SmallLoad = -1.0; // Watts
        Real64 CpAirZn = PsyCpAirFnW(Node(AirInNode).HumRat);
        QCoilReq = QZnReq - Node(CCoilInAirNode).MassFlowRate * CpAirZn * (Node(CCoilInAirNode).Temp - Node(AirInNode).Temp);
        if (QCoilReq > SmallLoad) {
            QCoilReq = 0.0;
            mdot = 0.0;
        }
    }

} // namespace UnitVentilator

} // namespace EnergyPlus
