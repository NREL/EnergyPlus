// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus {

namespace OutdoorAirUnit {
    // Module containing the routines dealing with the outdoor air unit

    // MODULE INFORMATION:
    //       AUTHOR         Young Tae Chae, Rick Strand
    //       DATE WRITTEN   AUG. 2009
    //       MODIFIED
    //                      Feb 2013 Bereket Nigusse, FSEC
    //                        Added DX Coil Model For 100% OA systems
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // Simulate zone outdoor air unit.

    // METHODOLOGY EMPLOYED:
    // Systems are modeled as a collection of components:
    // fan, heat recovery, dehumidifier, heating coil and/or cooling coil plus an integrated control
    // algorithm that adjusts the hot or cold water flow to meet the setpoint
    // condition.

    // Using/Aliasing
    using namespace DataLoopNode;
    using DataHVACGlobals::BlowThru;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::DrawThru;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using namespace ScheduleManager;
    using namespace Psychrometrics;
    using namespace FluidProperties;

    // component types addressed by this module
    constexpr static std::string_view ZoneHVACOAUnit = {"ZoneHVAC:OutdoorAirUnit"};
    constexpr static std::string_view ZoneHVACEqList = {"ZoneHVAC:OutdoorAirUnit:EquipmentList"};

    void SimOutdoorAirUnit(EnergyPlusData &state,
                           std::string_view CompName,     // name of the outdoor air unit
                           int const ZoneNum,             // number of zone being served
                           bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           Real64 &PowerMet,              // Sensible power supplied (W)
                           Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                           int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED
        // This is re-engineered by Rick Strand and Young T. Chae for OutdoorAirUnit (July, 2009)

        // PURPOSE OF THIS SUBROUTINE:
        // This is the main driver subroutine for the outdoor air control unit simulation.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int OAUnitNum; // index of outdoor air unit being simulated

        if (state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag) {
            GetOutdoorAirUnitInputs(state);
            state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;
        }

        // Find the correct Outdoor Air Unit

        if (CompIndex == 0) {
            OAUnitNum = UtilityRoutines::FindItemInList(CompName, state.dataOutdoorAirUnit->OutAirUnit);
            if (OAUnitNum == 0) {
                ShowFatalError(state, "ZoneHVAC:OutdoorAirUnit not found=" + std::string{CompName});
            }
            CompIndex = OAUnitNum;
        } else {
            OAUnitNum = CompIndex;
            if (OAUnitNum > state.dataOutdoorAirUnit->NumOfOAUnits || OAUnitNum < 1) {
                ShowFatalError(state,
                               format("SimOutdoorAirUnit:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      OAUnitNum,
                                      state.dataOutdoorAirUnit->NumOfOAUnits,
                                      CompName));
            }
            if (state.dataOutdoorAirUnit->CheckEquipName(OAUnitNum)) {
                if (CompName != state.dataOutdoorAirUnit->OutAirUnit(OAUnitNum).Name) {
                    ShowFatalError(state,
                                   format("SimOutdoorAirUnit: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          OAUnitNum,
                                          CompName,
                                          state.dataOutdoorAirUnit->OutAirUnit(OAUnitNum).Name));
                }
                state.dataOutdoorAirUnit->CheckEquipName(OAUnitNum) = false;
            }
        }

        state.dataSize->ZoneEqOutdoorAirUnit = true;

        if (state.dataGlobal->ZoneSizingCalc || state.dataGlobal->SysSizingCalc) return;

        InitOutdoorAirUnit(state, OAUnitNum, ZoneNum, FirstHVACIteration);

        CalcOutdoorAirUnit(state, OAUnitNum, ZoneNum, FirstHVACIteration, PowerMet, LatOutputProvided);

        ReportOutdoorAirUnit(state, OAUnitNum);

        state.dataSize->ZoneEqOutdoorAirUnit = false;
    }

    void GetOutdoorAirUnitInputs(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   July 2009
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine obtains the input for the outdoor air control unit and sets
        // up the appropriate derived type.

        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // REFERENCES:
        // Fred Buhl's fan coil module (FanCoilUnits.cc)
        // Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.cc)
        // Young Tae Chae's Ventilated Slab System (VentilatedSlab.cc)
        // Mixed Air.cc

        // Using/Aliasing
        using BranchNodeConnections::SetUpCompSets;
        using BranchNodeConnections::TestCompSet;
        using FluidProperties::FindRefrigerant;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetScheduleIndex;
        using SteamCoils::GetCoilAirInletNode;
        using SteamCoils::GetCoilAirOutletNode;
        using SteamCoils::GetCoilMaxSteamFlowRate;
        using SteamCoils::GetCoilSteamInletNode;
        using SteamCoils::GetCoilSteamOutletNode;
        using SteamCoils::GetSteamCoilIndex;
        using namespace DataLoopNode;
        using OutAirNodeManager::CheckAndAddAirNodeNumber;
        using WaterCoils::GetCoilWaterInletNode;
        using WaterCoils::GetWaterCoilIndex;
        auto &GetWCoilInletNode(WaterCoils::GetCoilInletNode);
        auto &GetWCoilOutletNode(WaterCoils::GetCoilOutletNode);
        using HeatingCoils::GetCoilInletNode;
        using HeatingCoils::GetCoilOutletNode;
        using WaterCoils::GetCoilWaterOutletNode;
        auto &GetHeatingCoilIndex(HeatingCoils::GetCoilIndex);
        auto &GetElecCoilInletNode(HeatingCoils::GetCoilInletNode);
        auto &GetElecCoilOutletNode(HeatingCoils::GetCoilOutletNode);
        auto &GetHXAssistedCoilFlowRate(HVACHXAssistedCoolingCoil::GetCoilMaxWaterFlowRate);
        auto &GetWHXCoilInletNode(HVACHXAssistedCoolingCoil::GetCoilInletNode);
        auto &GetWHXCoilOutletNode(HVACHXAssistedCoolingCoil::GetCoilOutletNode);
        using DataHVACGlobals::cFanTypes;

        using Fans::GetFanAvailSchPtr;
        using Fans::GetFanDesignVolumeFlowRate;
        using Fans::GetFanIndex;
        using Fans::GetFanType;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetOutdoorAirUnitInputs: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int NumNums;   // Number of real numbers returned by GetObjectItem
        int NumAlphas; // Number of alphanumerics returned by GetObjectItem
        int IOStat;
        int OAUnitNum;
        int CompNum;
        std::string ComponentListName;
        int NumInList;
        int InListNum;
        int ListNum;
        bool ErrorsFound(false);
        int MaxNums(0);                  // Maximum number of numeric input fields
        int MaxAlphas(0);                // Maximum number of alpha input fields
        int TotalArgs(0);                // Total number of alpha and numeric arguments (max) for a
        bool IsValid;                    // Set for outside air node check
        Array1D_string cAlphaArgs;       // Alpha input items for object
        std::string CurrentModuleObject; // Object type for getting and messages
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        Array1D<Real64> NumArray;
        Array1D_string AlphArray;
        bool errFlag(false);

        // Figure out how many outdoor air units there are in the input file

        if (!state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag) return;

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ZoneHVACOAUnit, TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, ZoneHVACEqList, TotalArgs, NumAlphas, NumNums);
        MaxNums = max(MaxNums, NumNums);
        MaxAlphas = max(MaxAlphas, NumAlphas);

        AlphArray.allocate(MaxAlphas);
        cAlphaFields.allocate(MaxAlphas);
        NumArray.dimension(MaxNums, 0.0);
        cNumericFields.allocate(MaxNums);
        lAlphaBlanks.dimension(MaxAlphas, true);
        lNumericBlanks.dimension(MaxNums, true);
        cAlphaArgs.allocate(NumAlphas);

        CurrentModuleObject = ZoneHVACOAUnit;
        state.dataOutdoorAirUnit->NumOfOAUnits = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        state.dataOutdoorAirUnit->OutAirUnit.allocate(state.dataOutdoorAirUnit->NumOfOAUnits);
        state.dataOutdoorAirUnit->SupplyFanUniqueNames.reserve(static_cast<unsigned>(state.dataOutdoorAirUnit->NumOfOAUnits));
        state.dataOutdoorAirUnit->ExhaustFanUniqueNames.reserve(static_cast<unsigned>(state.dataOutdoorAirUnit->NumOfOAUnits));
        state.dataOutdoorAirUnit->ComponentListUniqueNames.reserve(static_cast<unsigned>(state.dataOutdoorAirUnit->NumOfOAUnits));
        state.dataOutdoorAirUnit->MyOneTimeErrorFlag.dimension(state.dataOutdoorAirUnit->NumOfOAUnits, true);
        state.dataOutdoorAirUnit->CheckEquipName.dimension(state.dataOutdoorAirUnit->NumOfOAUnits, true);

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);

        for (OAUnitNum = 1; OAUnitNum <= state.dataOutdoorAirUnit->NumOfOAUnits; ++OAUnitNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     OAUnitNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), CurrentModuleObject, ErrorsFound);

            // A1
            OutAirUnit(OAUnitNum).Name = state.dataIPShortCut->cAlphaArgs(1);

            // A2
            OutAirUnit(OAUnitNum).SchedName = state.dataIPShortCut->cAlphaArgs(2);
            if (lAlphaBlanks(2)) {
                OutAirUnit(OAUnitNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                OutAirUnit(OAUnitNum).SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
                if (OutAirUnit(OAUnitNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaArgs(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // A3
            OutAirUnit(OAUnitNum).ZoneName = state.dataIPShortCut->cAlphaArgs(3);
            OutAirUnit(OAUnitNum).ZonePtr = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);

            if (OutAirUnit(OAUnitNum).ZonePtr == 0) {
                if (lAlphaBlanks(3)) {
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaArgs(3) + " is required but input is blank.");
                } else {
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        state.dataIPShortCut->cAlphaArgs(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\" not found.");
                }
                ErrorsFound = true;
            }
            OutAirUnit(OAUnitNum).ZoneNodeNum = state.dataHeatBal->Zone(OutAirUnit(OAUnitNum).ZonePtr).SystemZoneNodeNumber;
            // Outside air information:
            // N1
            OutAirUnit(OAUnitNum).OutAirVolFlow = NumArray(1);
            // A4
            OutAirUnit(OAUnitNum).OutAirSchedName = state.dataIPShortCut->cAlphaArgs(4);
            // convert schedule name to pointer
            OutAirUnit(OAUnitNum).OutAirSchedPtr = GetScheduleIndex(state, OutAirUnit(OAUnitNum).OutAirSchedName);
            if (OutAirUnit(OAUnitNum).OutAirSchedPtr == 0) {
                ShowSevereError(state,
                                std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(4) +
                                    "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\" not found.");
                ErrorsFound = true;
            }

            // A5
            OutAirUnit(OAUnitNum).SFanName = state.dataIPShortCut->cAlphaArgs(5);
            GlobalNames::IntraObjUniquenessCheck(state,
                                                 state.dataIPShortCut->cAlphaArgs(5),
                                                 CurrentModuleObject,
                                                 cAlphaFields(5),
                                                 state.dataOutdoorAirUnit->SupplyFanUniqueNames,
                                                 ErrorsFound);
            errFlag = false;
            if (HVACFan::checkIfFanNameIsAFanSystem(state, OutAirUnit(OAUnitNum).SFanName)) { // no object type in input, so check if Fan:SystemModel
                OutAirUnit(OAUnitNum).SFanType = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, OutAirUnit(OAUnitNum).SFanName)); // call constructor
                OutAirUnit(OAUnitNum).SFan_Index = HVACFan::getFanObjectVectorIndex(state, OutAirUnit(OAUnitNum).SFanName);
                OutAirUnit(OAUnitNum).SFanMaxAirVolFlow = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->designAirVolFlowRate;
                OutAirUnit(OAUnitNum).SFanAvailSchedPtr = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->availSchedIndex;
            } else {
                GetFanType(
                    state, OutAirUnit(OAUnitNum).SFanName, OutAirUnit(OAUnitNum).SFanType, errFlag, CurrentModuleObject, OutAirUnit(OAUnitNum).Name);

                OutAirUnit(OAUnitNum).SFanMaxAirVolFlow =
                    GetFanDesignVolumeFlowRate(state, cFanTypes(OutAirUnit(OAUnitNum).SFanType), OutAirUnit(OAUnitNum).SFanName, errFlag);
                if (!errFlag) {
                    OutAirUnit(OAUnitNum).SFanAvailSchedPtr =
                        GetFanAvailSchPtr(state, cFanTypes(OutAirUnit(OAUnitNum).SFanType), OutAirUnit(OAUnitNum).SFanName, errFlag);
                    // get fan index
                    GetFanIndex(state, OutAirUnit(OAUnitNum).SFanName, OutAirUnit(OAUnitNum).SFan_Index, ErrorsFound);
                } else {
                    ErrorsFound = true;
                }
            }
            // A6 :Fan Place
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "BlowThrough")) OutAirUnit(OAUnitNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "DrawThrough")) OutAirUnit(OAUnitNum).FanPlace = DrawThru;
            if (OutAirUnit(OAUnitNum).FanPlace == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFields(6) + " = " + state.dataIPShortCut->cAlphaArgs(6));
                ShowContinueError(state, "Occurs in " + std::string{CurrentModuleObject} + " = " + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }

            // A7

            if (lAlphaBlanks(7)) {
                OutAirUnit(OAUnitNum).ExtFan = false;
                if (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance) {
                    ShowWarningError(state,
                                     std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " + cAlphaFields(7) +
                                         " is blank.");
                    ShowContinueError(state,
                                      "Unbalanced mass flow rates between supply from outdoor air and exhaust from zone air will be introduced.");
                }
            } else if (!lAlphaBlanks(7)) {
                OutAirUnit(OAUnitNum).ExtFanName = state.dataIPShortCut->cAlphaArgs(7);
                GlobalNames::IntraObjUniquenessCheck(state,
                                                     state.dataIPShortCut->cAlphaArgs(7),
                                                     CurrentModuleObject,
                                                     cAlphaFields(7),
                                                     state.dataOutdoorAirUnit->ExhaustFanUniqueNames,
                                                     ErrorsFound);
                errFlag = false;
                if (HVACFan::checkIfFanNameIsAFanSystem(state,
                                                        OutAirUnit(OAUnitNum).ExtFanName)) { // no object type in input, so check if Fan:SystemModel
                    OutAirUnit(OAUnitNum).ExtFanType = DataHVACGlobals::FanType_SystemModelObject;
                    state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, OutAirUnit(OAUnitNum).ExtFanName)); // call constructor
                    OutAirUnit(OAUnitNum).ExtFan_Index = HVACFan::getFanObjectVectorIndex(state, OutAirUnit(OAUnitNum).ExtFanName);
                    OutAirUnit(OAUnitNum).EFanMaxAirVolFlow = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->designAirVolFlowRate;
                    OutAirUnit(OAUnitNum).ExtFanAvailSchedPtr = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->availSchedIndex;
                } else {
                    GetFanType(state,
                               OutAirUnit(OAUnitNum).ExtFanName,
                               OutAirUnit(OAUnitNum).ExtFanType,
                               errFlag,
                               CurrentModuleObject,
                               OutAirUnit(OAUnitNum).Name);
                    OutAirUnit(OAUnitNum).EFanMaxAirVolFlow =
                        GetFanDesignVolumeFlowRate(state, cFanTypes(OutAirUnit(OAUnitNum).ExtFanType), OutAirUnit(OAUnitNum).ExtFanName, errFlag);
                    if (!errFlag) {
                        OutAirUnit(OAUnitNum).ExtFanAvailSchedPtr =
                            GetFanAvailSchPtr(state, cFanTypes(OutAirUnit(OAUnitNum).ExtFanType), OutAirUnit(OAUnitNum).ExtFanName, errFlag);
                        // get fan index
                        GetFanIndex(state, OutAirUnit(OAUnitNum).ExtFanName, OutAirUnit(OAUnitNum).ExtFan_Index, ErrorsFound);
                    } else {
                        ErrorsFound = true;
                    }
                }
                OutAirUnit(OAUnitNum).ExtFan = true;
            }

            // N2
            OutAirUnit(OAUnitNum).ExtAirVolFlow = NumArray(2);
            if ((OutAirUnit(OAUnitNum).ExtFan) && (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance)) {
                if (NumArray(2) != NumArray(1)) {
                    ShowWarningError(state,
                                     std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", " + cNumericFields(1) +
                                         " and " + cNumericFields(2) + " are not equal. This may cause unbalanced flow.");
                    ShowContinueError(state, format("{}={:.3R}= and {}{:.3R}", cNumericFields(1), NumArray(1), cNumericFields(2), NumArray(2)));
                }
            }
            // A8
            OutAirUnit(OAUnitNum).ExtAirSchedName = state.dataIPShortCut->cAlphaArgs(8);
            // convert schedule name to pointer
            OutAirUnit(OAUnitNum).ExtOutAirSchedPtr = GetScheduleIndex(state, OutAirUnit(OAUnitNum).ExtAirSchedName);
            if (OutAirUnit(OAUnitNum).ExtFan) {
                if ((OutAirUnit(OAUnitNum).ExtOutAirSchedPtr == 0) || (lNumericBlanks(2))) {
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(8) +
                                        "=\"" + state.dataIPShortCut->cAlphaArgs(8) + "\" not found.");
                    ErrorsFound = true;
                } else {
                    if ((OutAirUnit(OAUnitNum).ExtOutAirSchedPtr != OutAirUnit(OAUnitNum).OutAirSchedPtr) &&
                        (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance)) {
                        ShowWarningError(
                            state,
                            std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                "\", different schedule inputs for outdoor air and exhaust air schedules may cause unbalanced mass flow.");
                        ShowContinueError(state,
                                          cAlphaFields(4) + "=" + state.dataIPShortCut->cAlphaArgs(4) + " and " + cAlphaFields(8) + "=" +
                                              state.dataIPShortCut->cAlphaArgs(8));
                    }
                }
            }

            if (OutAirUnit(OAUnitNum).ExtFan) {
                SetUpCompSets(state,
                              CurrentModuleObject,
                              OutAirUnit(OAUnitNum).Name,
                              "UNDEFINED",
                              state.dataIPShortCut->cAlphaArgs(7),
                              "UNDEFINED",
                              "UNDEFINED");
            }

            // Process the unit control type
            if (!lAlphaBlanks(9)) {
                constexpr std::array<std::string_view, static_cast<int>(OAUnitCtrlType::Num)> ctrlTypeNamesUC = {
                    "NEUTRALCONTROL", "INVALID-UNCONDITIONED", "TEMPERATURECONTROL"};
                auto tmpCtrlType = static_cast<OAUnitCtrlType>(getEnumerationValue(ctrlTypeNamesUC, state.dataIPShortCut->cAlphaArgs(9)));
                switch (tmpCtrlType) {
                case OAUnitCtrlType::Neutral:
                case OAUnitCtrlType::Temperature:
                    OutAirUnit(OAUnitNum).controlType = tmpCtrlType;
                    break;
                default:
                    break; // just leave it alone, nothing was done here
                }
            } else {
                ShowSevereError(state,
                                std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(9) +
                                    "=\"" + state.dataIPShortCut->cAlphaArgs(9) + "\".");
                ShowContinueError(state, "Control reset to Unconditioned Control.");
                OutAirUnit(OAUnitNum).controlType = OAUnitCtrlType::Neutral;
            }

            // A10:High Control Temp :
            OutAirUnit(OAUnitNum).HiCtrlTempSched = state.dataIPShortCut->cAlphaArgs(10);
            OutAirUnit(OAUnitNum).HiCtrlTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(10));
            if ((OutAirUnit(OAUnitNum).HiCtrlTempSchedPtr == 0) && (!lAlphaBlanks(10))) {
                ShowSevereError(state,
                                std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(10) +
                                    "=\"" + state.dataIPShortCut->cAlphaArgs(9) + "\" not found.");
                ErrorsFound = true;
            }

            // A11:Low Control Temp :
            OutAirUnit(OAUnitNum).LoCtrlTempSched = state.dataIPShortCut->cAlphaArgs(11);
            OutAirUnit(OAUnitNum).LoCtrlTempSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(11));
            if ((OutAirUnit(OAUnitNum).LoCtrlTempSchedPtr == 0) && (!lAlphaBlanks(11))) {
                ShowSevereError(state,
                                std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(11) +
                                    "=\"" + state.dataIPShortCut->cAlphaArgs(10) + "\" not found.");
                ErrorsFound = true;
            }

            OutAirUnit(OAUnitNum).CompOutSetTemp = 0.0;

            // A12~A15 : Node Condition

            // Main air nodes (except outside air node):

            OutAirUnit(OAUnitNum).AirOutletNode = GetOnlySingleNode(state,
                                                                    state.dataIPShortCut->cAlphaArgs(13),
                                                                    ErrorsFound,
                                                                    DataLoopNode::ConnectionObjectType::ZoneHVACOutdoorAirUnit,
                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                    DataLoopNode::NodeFluidType::Air,
                                                                    DataLoopNode::ConnectionType::Outlet,
                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                    ObjectIsParent);
            if (!lAlphaBlanks(14)) {
                OutAirUnit(OAUnitNum).AirInletNode = GetOnlySingleNode(state,
                                                                       state.dataIPShortCut->cAlphaArgs(14),
                                                                       ErrorsFound,
                                                                       DataLoopNode::ConnectionObjectType::ZoneHVACOutdoorAirUnit,
                                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                                       DataLoopNode::NodeFluidType::Air,
                                                                       DataLoopNode::ConnectionType::Inlet,
                                                                       NodeInputManager::CompFluidStream::Primary,
                                                                       ObjectIsParent);
            } else {
                if (OutAirUnit(OAUnitNum).ExtFan) {
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        cAlphaFields(14) + " cannot be blank when there is an exhaust fan.");
                    ErrorsFound = true;
                }
            }

            OutAirUnit(OAUnitNum).SFanOutletNode = GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(15),
                                                                     ErrorsFound,
                                                                     DataLoopNode::ConnectionObjectType::ZoneHVACOutdoorAirUnit,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Air,
                                                                     DataLoopNode::ConnectionType::Internal,
                                                                     NodeInputManager::CompFluidStream::Primary,
                                                                     ObjectIsNotParent);

            //  Set connection type to 'OutdoorAir', because this is hardwired to OA conditions
            OutAirUnit(OAUnitNum).OutsideAirNode = GetOnlySingleNode(state,
                                                                     state.dataIPShortCut->cAlphaArgs(12),
                                                                     ErrorsFound,
                                                                     DataLoopNode::ConnectionObjectType::ZoneHVACOutdoorAirUnit,
                                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                                     DataLoopNode::NodeFluidType::Air,
                                                                     DataLoopNode::ConnectionType::OutsideAirReference,
                                                                     NodeInputManager::CompFluidStream::Primary,
                                                                     ObjectIsNotParent);

            if (!lAlphaBlanks(12)) {
                CheckAndAddAirNodeNumber(state, OutAirUnit(OAUnitNum).OutsideAirNode, IsValid);
                if (!IsValid) {
                    ShowWarningError(state,
                                     std::string{CurrentModuleObject} + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                         "\", Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(12));
                }
            }

            // When the fan position is "BlowThru", Each node is set up

            if (OutAirUnit(OAUnitNum).FanPlace == BlowThru) {
                SetUpCompSets(state,
                              CurrentModuleObject,
                              OutAirUnit(OAUnitNum).Name,
                              "UNDEFINED",
                              state.dataIPShortCut->cAlphaArgs(5),
                              state.dataIPShortCut->cAlphaArgs(12),
                              state.dataIPShortCut->cAlphaArgs(15));
            }

            // A16 : component list

            GlobalNames::IntraObjUniquenessCheck(state,
                                                 state.dataIPShortCut->cAlphaArgs(16),
                                                 CurrentModuleObject,
                                                 cAlphaFields(16),
                                                 state.dataOutdoorAirUnit->ComponentListUniqueNames,
                                                 ErrorsFound);
            ComponentListName = state.dataIPShortCut->cAlphaArgs(16);
            OutAirUnit(OAUnitNum).ComponentListName = ComponentListName;
            if (!lAlphaBlanks(16)) {
                ListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, ZoneHVACEqList, ComponentListName);
                if (ListNum > 0) {
                    state.dataInputProcessing->inputProcessor->getObjectItem(
                        state, ZoneHVACEqList, ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat);
                    NumInList = (NumAlphas - 1) / 2; // potential problem if puts in type but not name
                    if (mod(NumAlphas - 1, 2) != 0) ++NumInList;
                    OutAirUnit(OAUnitNum).NumComponents = NumInList;
                    OutAirUnit(OAUnitNum).OAEquip.allocate(NumInList);

                    // Get information of component
                    for (InListNum = 1; InListNum <= NumInList; ++InListNum) {
                        OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName = AlphArray(InListNum * 2 + 1);

                        OutAirUnit(OAUnitNum).OAEquip(InListNum).Type =
                            static_cast<CompType>(getEnumerationValue(CompTypeNamesUC, UtilityRoutines::MakeUPPERCase(AlphArray(InListNum * 2))));

                        CompNum = InListNum;

                        // Coil Types
                        switch (OutAirUnit(OAUnitNum).OAEquip(InListNum).Type) {
                        case CompType::WaterCoil_Cooling: {
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilType = DataPlant::PlantEquipmentType::CoilWaterCooling;
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex =
                                GetWaterCoilIndex(state,
                                                  CompTypeNamesUC[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetWCoilInletNode(state,
                                                  CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetWCoilOutletNode(state,
                                                   CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                   OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                   ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterInletNode =
                                GetCoilWaterInletNode(state,
                                                      CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterOutletNode =
                                GetCoilWaterOutletNode(state,
                                                       CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                       OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                       ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate(state,
                                                                    CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                                    OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                                    ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MinVolWaterFlow = 0.0;
                            break;
                        }
                        case CompType::WaterCoil_SimpleHeat: {
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex =
                                GetWaterCoilIndex(state,
                                                  CompTypeNamesUC[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetWCoilInletNode(state,
                                                  CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetWCoilOutletNode(state, "Coil:Heating:Water", OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName, ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterInletNode =
                                GetCoilWaterInletNode(state,
                                                      CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterOutletNode =
                                GetCoilWaterOutletNode(state,
                                                       CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                       OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                       ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow = WaterCoils::GetCoilMaxWaterFlowRate(
                                state, "Coil:Heating:Water", OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName, ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MinVolWaterFlow = 0.0;
                            break;
                        }
                        case CompType::SteamCoil_AirHeat: {
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilType = DataPlant::PlantEquipmentType::CoilSteamAirHeating;
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex =
                                GetSteamCoilIndex(state,
                                                  CompTypeNamesUC[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetCoilAirInletNode(state,
                                                    OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                    OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                    ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetCoilAirOutletNode(state,
                                                     OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                     OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                     ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterInletNode =
                                GetCoilSteamInletNode(state,
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterOutletNode =
                                GetCoilSteamOutletNode(state,
                                                       CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                       OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                       ErrorsFound);

                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow =
                                GetCoilMaxSteamFlowRate(state, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex, ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MinVolWaterFlow = 0.0;
                            // below: no extra error needed if steam properties not in input
                            // file because getting the steam coil will have done that.
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).FluidIndex = FindRefrigerant(state, "Steam");
                            break;
                        }
                        case CompType::WaterCoil_DetailedCool: {
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex =
                                GetWaterCoilIndex(state,
                                                  CompTypeNamesUC[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilType = DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling;
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetWCoilInletNode(state,
                                                  CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetWCoilOutletNode(state,
                                                   CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                   OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                   ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterInletNode =
                                GetCoilWaterInletNode(state,
                                                      CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterOutletNode =
                                GetCoilWaterOutletNode(state,
                                                       CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                       OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                       ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate(state,
                                                                    CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                                    OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                                    ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MinVolWaterFlow = 0.0;
                            break;
                        }
                        case CompType::WaterCoil_CoolingHXAsst: {
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetWHXCoilInletNode(state,
                                                    CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                    OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                    ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetWHXCoilOutletNode(state,
                                                     CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                     OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                     ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterInletNode =
                                GetCoilWaterInletNode(state,
                                                      CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilWaterOutletNode =
                                GetCoilWaterOutletNode(state,
                                                       CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                       OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                       ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow =
                                GetHXAssistedCoilFlowRate(state,
                                                          CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                          OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                          ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).MinVolWaterFlow = 0.0;
                            break;
                        }
                        case CompType::Coil_ElectricHeat: {
                            // Get OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, 2 types of mining functions to choose from
                            GetHeatingCoilIndex(state,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetElecCoilInletNode(state,
                                                     CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                     OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                     ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetElecCoilOutletNode(state,
                                                      CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                      OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                      ErrorsFound);
                            break;
                        }
                        case CompType::Coil_GasHeat: {
                            // Get OutAirUnit( OAUnitNum ).OAEquip( CompNum ).ComponentIndex, 2 types of mining functions to choose from
                            GetHeatingCoilIndex(state,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirInletNode =
                                GetCoilInletNode(state,
                                                 CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                 OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                 ErrorsFound);
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).CoilAirOutletNode =
                                GetCoilOutletNode(state,
                                                  CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)],
                                                  OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                  ErrorsFound);
                            break;
                        }
                        case CompType::DXSystem: {
                            // set the data for 100% DOAS DX cooling coil
                            // is a different function call needed here? similar to one in HVACDXSystem
                            // CheckDXCoolingCoilInOASysExists(state, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName);
                            break;
                        }
                        case CompType::DXHeatPumpSystem: {
                            break;
                        }
                        case CompType::UnitarySystemModel: {
                            UnitarySystems::UnitarySys thisSys;
                            OutAirUnit(OAUnitNum).OAEquip(CompNum).compPointer = thisSys.factory(state,
                                                                                                 DataHVACGlobals::UnitarySys_AnyCoilType,
                                                                                                 OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                                                                 false,
                                                                                                 OAUnitNum);
                            UnitarySystems::UnitarySys::checkUnitarySysCoilInOASysExists(
                                state, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName, OAUnitNum);

                            // Heat recovery
                            break;
                        }
                        case CompType::HeatXchngrFP:
                        case CompType::HeatXchngrSL: {
                            //        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
                            //          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%Type= CompType::HeatXchngr

                            // Desiccant Dehumidifier
                            break;
                        }
                        case CompType::Desiccant: {
                            // Futher Enhancement
                            //        CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
                            //          OutAirUnit(OAUnitNum)%OAEquip(CompNum)%Type= CompType::Desiccant
                            break;
                        }
                        default: {
                            ShowSevereError(state,
                                            format("{}= \"{}\" invalid Outside Air Component=\"{}\".",
                                                   CurrentModuleObject,
                                                   AlphArray(1),
                                                   CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(CompNum).Type)]));
                            ErrorsFound = true;
                        }
                        }

                        // Add equipment to component sets array
                        // Node set up
                        if (OutAirUnit(OAUnitNum).FanPlace == BlowThru) {
                            if (InListNum == 1) { // the component is the first one
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              state.dataIPShortCut->cAlphaArgs(15),
                                              "UNDEFINED");
                            } else if (InListNum != NumInList) { // the component is placed in b/w components
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              "UNDEFINED",
                                              "UNDEFINED");
                            } else if (InListNum == NumInList) { // the component is the last one
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              "UNDEFINED",
                                              state.dataIPShortCut->cAlphaArgs(13));
                            }
                            // If fan is on the end of equipment.
                        } else if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
                            if (InListNum == 1) {
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              state.dataIPShortCut->cAlphaArgs(12),
                                              "UNDEFINED");
                            } else if (InListNum != NumInList) {
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              "UNDEFINED",
                                              "UNDEFINED");
                            } else if (InListNum == NumInList) {
                                SetUpCompSets(state,
                                              "ZoneHVAC:OutdoorAirUnit",
                                              OutAirUnit(OAUnitNum).Name,
                                              CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)],
                                              OutAirUnit(OAUnitNum).OAEquip(InListNum).ComponentName,
                                              "UNDEFINED",
                                              "UNDEFINED");
                            }
                        }
                        // Must call after SetUpCompSets since this will add another CoilSystem:Cooling:DX object in CompSets
                        if (CompTypeNamesUC[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(InListNum).Type)] == "COILSYSTEM:COOLING:DX") {
                            UnitarySystems::UnitarySys::checkUnitarySysCoilInOASysExists(
                                state, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName, OAUnitNum);
                        }
                    } // End Inlist

                    // In case of draw through, the last component is linked with the zone air supply node
                    if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
                        SetUpCompSets(state,
                                      CurrentModuleObject,
                                      OutAirUnit(OAUnitNum).Name,
                                      "UNDEFINED",
                                      state.dataIPShortCut->cAlphaArgs(5),
                                      "UNDEFINED",
                                      state.dataIPShortCut->cAlphaArgs(13));
                    }

                } else { // when ListNum<0
                    ShowSevereError(state,
                                    std::string{CurrentModuleObject} + " = \"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " +
                                        cAlphaFields(16) + "=\"" + state.dataIPShortCut->cAlphaArgs(16) + "\" not found.");
                    ErrorsFound = true;
                }
            } else { // when Equipment list is left blanked
                ShowSevereError(state,
                                std::string{CurrentModuleObject} + " = \"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid " + cAlphaFields(16) +
                                    " is blank and must be entered.");
                ErrorsFound = true;
            }
            if (!lAlphaBlanks(17)) {
                OutAirUnit(OAUnitNum).AvailManagerListName = state.dataIPShortCut->cAlphaArgs(17);
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, std::string{RoutineName} + "Errors found in getting " + std::string{CurrentModuleObject} + '.');
        }

        AlphArray.deallocate();
        cAlphaFields.deallocate();
        NumArray.deallocate();
        cNumericFields.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;

        // Setup Report variables for the zone outdoor air unit CurrentModuleObject='ZoneHVAC:OutdoorAirUnit'
        for (OAUnitNum = 1; OAUnitNum <= state.dataOutdoorAirUnit->NumOfOAUnits; ++OAUnitNum) {
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Total Heating Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).TotHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Total Heating Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).TotHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Sensible Heating Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).SensHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Sensible Heating Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).SensHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Latent Heating Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).LatHeatingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Latent Heating Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).LatHeatingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).TotCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).TotCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).SensCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).SensCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).LatCoolingRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).LatCoolingEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                OutAirUnit(OAUnitNum).AirMassFlow,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Fan Electricity Rate",
                                OutputProcessor::Unit::W,
                                OutAirUnit(OAUnitNum).ElecFanRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Fan Electricity Energy",
                                OutputProcessor::Unit::J,
                                OutAirUnit(OAUnitNum).ElecFanEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                OutAirUnit(OAUnitNum).Name);
            SetupOutputVariable(state,
                                "Zone Outdoor Air Unit Fan Availability Status",
                                OutputProcessor::Unit::None,
                                OutAirUnit(OAUnitNum).AvailStatus,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                OutAirUnit(OAUnitNum).Name);
            //! Note that the outdoor air unit fan electric is NOT metered because this value is already metered through the fan component
        }
    }

    void InitOutdoorAirUnit(EnergyPlusData &state,
                            int const OAUnitNum,          // index for the current outdoor air unit
                            int const ZoneNum,            // number of zone being served
                            bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   July 2009
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes all of the data elements which are necessary
        // to simulate a zone outdoor air control unit.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // REFERENCES:
        // na

        // Using/Aliasing
        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

        using DataZoneEquipment::CheckZoneEquipmentList;
        using FluidProperties::GetDensityGlycol;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;
        using ScheduleManager::GetCurrentScheduleValue;
        using SteamCoils::GetCoilMaxSteamFlowRate;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view CurrentModuleObject("ZoneHVAC:OutdoorAirUnit");
        static constexpr std::string_view RoutineName("SizeOutdoorAirUnit");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Loop;
        auto &MyEnvrnFlag = state.dataOutdoorAirUnit->MyEnvrnFlag;
        auto &MyPlantScanFlag = state.dataOutdoorAirUnit->MyPlantScanFlag;
        auto &MyZoneEqFlag = state.dataOutdoorAirUnit->MyZoneEqFlag; // used to set up zone equipment availability managers
        int InNode;                                                  // inlet node number in outdoor air unit
        int OutNode;                                                 // outlet node number in outdoor air unit
        int OutsideAirNode;                                          // outside air node number outdoor air unit
        Real64 OAFrac;                                               // possible outside air fraction
        Real64 EAFrac;                                               // possible exhaust air fraction
        Real64 RhoAir;                                               // air density at InNode
        int compLoop;                                                // local do loop index
        Real64 rho;
        bool errFlag;

        // Do the one time initializations

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);

        if (state.dataOutdoorAirUnit->MyOneTimeFlag) {

            MyEnvrnFlag.allocate(state.dataOutdoorAirUnit->NumOfOAUnits);
            state.dataOutdoorAirUnit->MySizeFlag.allocate(state.dataOutdoorAirUnit->NumOfOAUnits);
            MyPlantScanFlag.allocate(state.dataOutdoorAirUnit->NumOfOAUnits);
            MyZoneEqFlag.allocate(state.dataOutdoorAirUnit->NumOfOAUnits);
            MyEnvrnFlag = true;
            state.dataOutdoorAirUnit->MySizeFlag = true;
            MyPlantScanFlag = true;
            MyZoneEqFlag = true;
            state.dataOutdoorAirUnit->MyOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (MyZoneEqFlag(OAUnitNum)) { // initialize the name of each availability manager list and zone number
                ZoneComp(DataZoneEquipment::ZoneEquip::OutdoorAirUnit).ZoneCompAvailMgrs(OAUnitNum).AvailManagerListName =
                    OutAirUnit(OAUnitNum).AvailManagerListName;
                ZoneComp(DataZoneEquipment::ZoneEquip::OutdoorAirUnit).ZoneCompAvailMgrs(OAUnitNum).ZoneNum = ZoneNum;
                MyZoneEqFlag(OAUnitNum) = false;
            }
            OutAirUnit(OAUnitNum).AvailStatus = ZoneComp(DataZoneEquipment::ZoneEquip::OutdoorAirUnit).ZoneCompAvailMgrs(OAUnitNum).AvailStatus;
        }

        if (MyPlantScanFlag(OAUnitNum) && allocated(state.dataPlnt->PlantLoop)) {
            for (compLoop = 1; compLoop <= OutAirUnit(OAUnitNum).NumComponents; ++compLoop) {

                CompType Type = OutAirUnit(OAUnitNum).OAEquip(compLoop).Type;

                switch (Type) {
                case CompType::WaterCoil_Cooling:
                case CompType::WaterCoil_DetailedCool:
                case CompType::WaterCoil_SimpleHeat:
                case CompType::SteamCoil_AirHeat:

                {
                    errFlag = false;
                    ScanPlantLoopsForObject(state,
                                            OutAirUnit(OAUnitNum).OAEquip(compLoop).ComponentName,
                                            OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilType,
                                            OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc,
                                            errFlag,
                                            _,
                                            _,
                                            _,
                                            _,
                                            _);
                    if (errFlag) {
                        ShowFatalError(state, "InitOutdoorAirUnit: Program terminated for previous conditions.");
                    }
                    break;
                }
                default:
                    break;
                }
            }

            MyPlantScanFlag(OAUnitNum) = false;
        } else if (MyPlantScanFlag(OAUnitNum) && !state.dataGlobal->AnyPlantInModel) {
            MyPlantScanFlag(OAUnitNum) = false;
        }

        // need to check all zone outdoor air control units to see if they are on Zone Equipment List or issue warning
        if (!state.dataOutdoorAirUnit->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataOutdoorAirUnit->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataOutdoorAirUnit->NumOfOAUnits; ++Loop) {
                if (CheckZoneEquipmentList(state, CurrentModuleObject, OutAirUnit(Loop).Name)) continue;
                ShowSevereError(state,
                                "InitOutdoorAirUnit: Zone Outdoor Air Unit=[" + std::string{CurrentModuleObject} + ',' + OutAirUnit(Loop).Name +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataOutdoorAirUnit->MySizeFlag(OAUnitNum) && !MyPlantScanFlag(OAUnitNum)) {

            SizeOutdoorAirUnit(state, OAUnitNum);

            state.dataOutdoorAirUnit->MySizeFlag(OAUnitNum) = false;
        }

        // Do the one time initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(OAUnitNum)) {
            // Node Conditions

            OutNode = OutAirUnit(OAUnitNum).AirOutletNode;
            OutsideAirNode = OutAirUnit(OAUnitNum).OutsideAirNode;
            // Outdoor Air flow rate conditions
            RhoAir = state.dataEnvrn->StdRhoAir;
            OAFrac = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).OutAirSchedPtr);
            OutAirUnit(OAUnitNum).OutAirMassFlow = RhoAir * OAFrac * OutAirUnit(OAUnitNum).OutAirVolFlow;
            OutAirUnit(OAUnitNum).SMaxAirMassFlow = RhoAir * OAFrac * OutAirUnit(OAUnitNum).SFanMaxAirVolFlow;

            if (OutAirUnit(OAUnitNum).ExtFan) {
                InNode = OutAirUnit(OAUnitNum).AirInletNode;
                // set the exhaust air mass flow rate from input
                if (OutAirUnit(OAUnitNum).ExtFan) {
                    EAFrac = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).ExtOutAirSchedPtr);
                    OutAirUnit(OAUnitNum).ExtAirMassFlow = RhoAir * EAFrac * OutAirUnit(OAUnitNum).ExtAirVolFlow;
                    OutAirUnit(OAUnitNum).EMaxAirMassFlow = RhoAir * EAFrac * OutAirUnit(OAUnitNum).EFanMaxAirVolFlow;
                } else if (!OutAirUnit(OAUnitNum).ExtFan) {
                    OutAirUnit(OAUnitNum).ExtAirMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;
                    OutAirUnit(OAUnitNum).EMaxAirMassFlow = OutAirUnit(OAUnitNum).SMaxAirMassFlow;
                }
                state.dataLoopNodes->Node(InNode).MassFlowRateMax = OutAirUnit(OAUnitNum).EMaxAirMassFlow;
                state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
            }
            // set the node max and min mass flow rates
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax = OutAirUnit(OAUnitNum).SMaxAirMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(OutNode).MassFlowRate = OutAirUnit(OAUnitNum).EMaxAirMassFlow;

            if (!MyPlantScanFlag(OAUnitNum)) {
                for (compLoop = 1; compLoop <= OutAirUnit(OAUnitNum).NumComponents; ++compLoop) {
                    if ((OutAirUnit(OAUnitNum).OAEquip(compLoop).Type == CompType::WaterCoil_Cooling) ||
                        (OutAirUnit(OAUnitNum).OAEquip(compLoop).Type == CompType::WaterCoil_DetailedCool)) {
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow =
                            WaterCoils::GetCoilMaxWaterFlowRate(state,
                                                                CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(compLoop).Type)],
                                                                OutAirUnit(OAUnitNum).OAEquip(compLoop).ComponentName,
                                                                errFlag);
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidName,
                                               DataGlobalConstants::CWInitConvTemp,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidIndex,
                                               RoutineName);
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow;
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MinVolWaterFlow;
                        InitComponentNodes(state,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterInletNode,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterOutletNode);
                    }

                    if (OutAirUnit(OAUnitNum).OAEquip(compLoop).Type == CompType::WaterCoil_SimpleHeat) {
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow =
                            WaterCoils::GetCoilMaxWaterFlowRate(state,
                                                                CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(compLoop).Type)],
                                                                OutAirUnit(OAUnitNum).OAEquip(compLoop).ComponentName,
                                                                errFlag);
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidName,
                                               DataGlobalConstants::HWInitConvTemp,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidIndex,
                                               RoutineName);
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow;
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MinVolWaterFlow;
                        InitComponentNodes(state,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterInletNode,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterOutletNode);
                    }
                    if (OutAirUnit(OAUnitNum).OAEquip(compLoop).Type == CompType::SteamCoil_AirHeat) {
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow =
                            GetCoilMaxSteamFlowRate(state, OutAirUnit(OAUnitNum).OAEquip(compLoop).ComponentIndex, errFlag);
                        Real64 rho =
                            GetSatDensityRefrig(state,
                                                state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidName,
                                                DataGlobalConstants::SteamInitConvTemp,
                                                1.0,
                                                state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidIndex,
                                                RoutineName);
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow;
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MinVolWaterFlow;
                        InitComponentNodes(state,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterInletNode,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterOutletNode);
                    }
                    if (OutAirUnit(OAUnitNum).OAEquip(compLoop).Type == CompType::WaterCoil_CoolingHXAsst) {
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow =
                            WaterCoils::GetCoilMaxWaterFlowRate(state,
                                                                CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(compLoop).Type)],
                                                                OutAirUnit(OAUnitNum).OAEquip(compLoop).ComponentName,
                                                                errFlag);
                        rho = GetDensityGlycol(state,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidName,
                                               DataGlobalConstants::CWInitConvTemp,
                                               state.dataPlnt->PlantLoop(OutAirUnit(OAUnitNum).OAEquip(compLoop).plantLoc.loopNum).FluidIndex,
                                               RoutineName);
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxVolWaterFlow;
                        OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow = rho * OutAirUnit(OAUnitNum).OAEquip(compLoop).MinVolWaterFlow;
                        InitComponentNodes(state,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MinWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).MaxWaterMassFlow,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterInletNode,
                                           OutAirUnit(OAUnitNum).OAEquip(compLoop).CoilWaterOutletNode);
                    }
                }
            }
            MyEnvrnFlag(OAUnitNum) = false;

        } // ...end start of environment inits

        if (!state.dataGlobal->BeginEnvrnFlag) MyEnvrnFlag(OAUnitNum) = true;

        // These initializations are done every iteration...
        // Set all the output variable
        OutAirUnit(OAUnitNum).TotHeatingRate = 0.0;
        OutAirUnit(OAUnitNum).SensHeatingRate = 0.0;
        OutAirUnit(OAUnitNum).LatHeatingRate = 0.0;
        OutAirUnit(OAUnitNum).TotCoolingRate = 0.0;
        OutAirUnit(OAUnitNum).SensCoolingRate = 0.0;
        OutAirUnit(OAUnitNum).LatCoolingRate = 0.0;
        OutAirUnit(OAUnitNum).AirMassFlow = 0.0;
        OutAirUnit(OAUnitNum).ElecFanRate = 0.0;
        // Node Set

        OutNode = OutAirUnit(OAUnitNum).AirOutletNode;
        OutsideAirNode = OutAirUnit(OAUnitNum).OutsideAirNode;
        RhoAir = state.dataEnvrn->StdRhoAir;
        OAFrac = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).OutAirSchedPtr);

        // set the mass flow rates from the input volume flow rates
        if (OAFrac > 0.0 || (ZoneCompTurnFansOn && !ZoneCompTurnFansOff)) { // fan is available
            OutAirUnit(OAUnitNum).OutAirMassFlow = RhoAir * OAFrac * OutAirUnit(OAUnitNum).OutAirVolFlow;
        } else {
            OutAirUnit(OAUnitNum).OutAirMassFlow = 0.0;
        }

        // set the exhaust air mass flow rate from input
        if (OutAirUnit(OAUnitNum).ExtFan) {
            InNode = OutAirUnit(OAUnitNum).AirInletNode;
            EAFrac = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).ExtOutAirSchedPtr);
            if (OutAirUnit(OAUnitNum).ExtFanAvailSchedPtr > 0.0) {
                OutAirUnit(OAUnitNum).ExtAirMassFlow = RhoAir * EAFrac * OutAirUnit(OAUnitNum).ExtAirVolFlow;
            } else {
                OutAirUnit(OAUnitNum).ExtAirMassFlow = 0.0;
            }
            state.dataLoopNodes->Node(InNode).MassFlowRate = OutAirUnit(OAUnitNum).ExtAirMassFlow;
            state.dataLoopNodes->Node(InNode).MassFlowRateMaxAvail = OutAirUnit(OAUnitNum).ExtAirMassFlow;
            state.dataLoopNodes->Node(InNode).MassFlowRateMinAvail = 0.0;
        } else if (!OutAirUnit(OAUnitNum).ExtFan) {
            OutAirUnit(OAUnitNum).ExtAirMassFlow = 0.0;
        }

        // First, set the flow conditions up so that there is flow through the unit

        state.dataLoopNodes->Node(OutNode).MassFlowRate = OutAirUnit(OAUnitNum).OutAirMassFlow;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMaxAvail = OutAirUnit(OAUnitNum).OutAirMassFlow;
        state.dataLoopNodes->Node(OutNode).MassFlowRateMinAvail = 0.0;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = OutAirUnit(OAUnitNum).OutAirMassFlow;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = OutAirUnit(OAUnitNum).OutAirMassFlow;
        state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;

        // Just in case the system is off and conditions do not get sent through
        // the system for some reason, set the outlet conditions equal to the inlet
        // conditions of the zone outdoor air control unit
        if (OutAirUnit(OAUnitNum).ExtFan) {
            state.dataLoopNodes->Node(OutNode).Temp = state.dataLoopNodes->Node(InNode).Temp;
            state.dataLoopNodes->Node(OutNode).Press = state.dataLoopNodes->Node(InNode).Press;
            state.dataLoopNodes->Node(OutNode).HumRat = state.dataLoopNodes->Node(InNode).HumRat;
            state.dataLoopNodes->Node(OutNode).Enthalpy = state.dataLoopNodes->Node(InNode).Enthalpy;
        } else {
            state.dataLoopNodes->Node(OutNode).Temp = state.dataLoopNodes->Node(OutsideAirNode).Temp;
            state.dataLoopNodes->Node(OutNode).Press = state.dataLoopNodes->Node(OutsideAirNode).Press;
            state.dataLoopNodes->Node(OutNode).HumRat = state.dataLoopNodes->Node(OutsideAirNode).HumRat;
            state.dataLoopNodes->Node(OutNode).Enthalpy = state.dataLoopNodes->Node(OutsideAirNode).Enthalpy;
        }
        // These initializations only need to be done once at the start of the iterations...
        if (FirstHVACIteration || state.dataHVACGlobal->ShortenTimeStepSys) {
            // Initialize the outside air conditions...
            state.dataLoopNodes->Node(OutsideAirNode).Temp = state.dataLoopNodes->Node(OutsideAirNode).OutAirDryBulb;
            state.dataLoopNodes->Node(OutsideAirNode).HumRat = state.dataEnvrn->OutHumRat;
            state.dataLoopNodes->Node(OutsideAirNode).Press = state.dataEnvrn->OutBaroPress;
        }
    }

    void SizeOutdoorAirUnit(EnergyPlusData &state, int const OAUnitNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   July 2009
        //       MODIFIED       Brent Griffith, March 2010, autosize OA flow rate
        //                      August 2013 Daeho Kang, add component sizing table entries
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing zoen outdoor air control unit components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data.

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::cFanTypes;

        using Fans::GetFanDesignVolumeFlowRate;

        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using PlantUtilities::MyPlantSizingIndex;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizHeatNum; // index of plant sizing object for 1st heating loop
        int PltSizCoolNum; // index of plant sizing object for 1st cooling loop
        bool ErrorsFound;
        Real64 RhoAir;
        int CompNum;
        bool IsAutoSize;            // Indicator to autosize
        Real64 OutAirVolFlowDes;    // Autosized outdoor air flow for reporting
        Real64 OutAirVolFlowUser;   // Hardsized outdoor air flow for reporting
        Real64 ExtAirVolFlowDes;    // Autosized exhaust air flow for reporting
        Real64 ExtAirVolFlowUser;   // Hardsized exhaust air flow for reporting
        Real64 MaxVolWaterFlowDes;  // Autosized maximum water flow for reporting
        Real64 MaxVolWaterFlowUser; // Hardsized maximum water flow for reporting

        PltSizCoolNum = 0;
        PltSizHeatNum = 0;
        ErrorsFound = false;
        RhoAir = state.dataEnvrn->StdRhoAir;
        IsAutoSize = false;
        OutAirVolFlowDes = 0.0;
        OutAirVolFlowUser = 0.0;
        ExtAirVolFlowDes = 0.0;
        ExtAirVolFlowUser = 0.0;
        MaxVolWaterFlowDes = 0.0;
        MaxVolWaterFlowUser = 0.0;

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);
        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &DataFanEnumType(state.dataSize->DataFanEnumType);

        if (OutAirUnit(OAUnitNum).SFanType == DataHVACGlobals::FanType_SystemModelObject) {
            DataFanEnumType = DataAirSystems::ObjectVectorOOFanSystemModel;
        } else {
            DataFanEnumType = DataAirSystems::StructArrayLegacyFanModels;
        }
        state.dataSize->DataFanIndex = OutAirUnit(OAUnitNum).SFan_Index;
        if (OutAirUnit(OAUnitNum).FanPlace == BlowThru) {
            state.dataSize->DataFanPlacement = DataSizing::ZoneFanPlacement::BlowThru;
        } else if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
            state.dataSize->DataFanPlacement = DataSizing::ZoneFanPlacement::DrawThru;
        }

        if (OutAirUnit(OAUnitNum).OutAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }

        if (state.dataSize->CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (OutAirUnit(OAUnitNum).OutAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 ZoneHVACOAUnit,
                                                 OutAirUnit(OAUnitNum).Name,
                                                 "User-Specified Outdoor Air Flow Rate [m3/s]",
                                                 OutAirUnit(OAUnitNum).OutAirVolFlow);
                }
            } else {
                CheckZoneSizing(state, std::string(ZoneHVACOAUnit), OutAirUnit(OAUnitNum).Name);
                OutAirVolFlowDes = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
                if (OutAirVolFlowDes < SmallAirVolFlow) {
                    OutAirVolFlowDes = 0.0;
                }
                if (IsAutoSize) {
                    OutAirUnit(OAUnitNum).OutAirVolFlow = OutAirVolFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "Design Size Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes);
                } else {
                    if (OutAirUnit(OAUnitNum).OutAirVolFlow > 0.0 && OutAirVolFlowDes > 0.0) {
                        OutAirVolFlowUser = OutAirUnit(OAUnitNum).OutAirVolFlow;
                        BaseSizer::reportSizerOutput(
                            state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "User-Specified Outdoor Air Flow Rate [m3/s]", OutAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(OutAirVolFlowDes - OutAirVolFlowUser) / OutAirVolFlowUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                BaseSizer::reportSizerOutput(
                                    state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "Design Size Outdoor Air Flow Rate [m3/s]", OutAirVolFlowDes);
                                ShowMessage(state,
                                            "SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit " +
                                                OutAirUnit(OAUnitNum).Name);
                                ShowContinueError(state, format("User-Specified Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowUser));
                                ShowContinueError(state, format("differs from Design Size Outdoor Air Flow Rate of {:.5R} [m3/s]", OutAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (OutAirUnit(OAUnitNum).ExtAirVolFlow == AutoSize) {
            IsAutoSize = true;
        }
        if (state.dataSize->CurZoneEqNum > 0) {
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (OutAirUnit(OAUnitNum).ExtAirVolFlow > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 ZoneHVACOAUnit,
                                                 OutAirUnit(OAUnitNum).Name,
                                                 "User-Specified Exhaust Air Flow Rate [m3/s]",
                                                 OutAirUnit(OAUnitNum).ExtAirVolFlow);
                }
            } else {
                // set exhaust flow equal to the oa inlet flow
                ExtAirVolFlowDes = OutAirUnit(OAUnitNum).OutAirVolFlow;
                if (IsAutoSize) {
                    OutAirUnit(OAUnitNum).ExtAirVolFlow = ExtAirVolFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "Design Size Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowDes);
                } else {
                    if (OutAirUnit(OAUnitNum).ExtAirVolFlow > 0.0 && ExtAirVolFlowDes > 0.0) {
                        ExtAirVolFlowUser = OutAirUnit(OAUnitNum).ExtAirVolFlow;
                        BaseSizer::reportSizerOutput(
                            state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "User-Specified Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(ExtAirVolFlowDes - ExtAirVolFlowUser) / ExtAirVolFlowUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                BaseSizer::reportSizerOutput(
                                    state, ZoneHVACOAUnit, OutAirUnit(OAUnitNum).Name, "Design Size Exhaust Air Flow Rate [m3/s]", ExtAirVolFlowDes);
                                ShowMessage(state,
                                            "SizeOutdoorAirUnit: Potential issue with equipment sizing for ZoneHVAC:OutdoorAirUnit " +
                                                OutAirUnit(OAUnitNum).Name);
                                ShowContinueError(state, format("User-Specified Exhaust Air Flow Rate of {:.5R} [m3/s]", ExtAirVolFlowUser));
                                ShowContinueError(state, format("differs from Design Size Exhaust Air Flow Rate of {:.5R} [m3/s]", ExtAirVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }

        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirFlow = true;
        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirFlow = true;
        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingAirVolFlow = OutAirUnit(OAUnitNum).OutAirVolFlow;
        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingAirVolFlow = OutAirUnit(OAUnitNum).OutAirVolFlow;
        ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = OutAirUnit(OAUnitNum).OutAirVolFlow;

        if (OutAirUnit(OAUnitNum).SFanMaxAirVolFlow == AutoSize) {
            if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state, OutAirUnit(OAUnitNum).SFanName, true, OutAirUnit(OAUnitNum).SFan_Index, _, false, false);
                OutAirUnit(OAUnitNum).SFanMaxAirVolFlow =
                    GetFanDesignVolumeFlowRate(state, cFanTypes(OutAirUnit(OAUnitNum).SFanType), OutAirUnit(OAUnitNum).SFanName, ErrorsFound);

            } else {
                state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, _, _, _);
                OutAirUnit(OAUnitNum).SFanMaxAirVolFlow = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->designAirVolFlowRate;
            }
        }
        if (OutAirUnit(OAUnitNum).ExtFan) {
            if (OutAirUnit(OAUnitNum).EFanMaxAirVolFlow == AutoSize) {
                if (OutAirUnit(OAUnitNum).ExtFanType != DataHVACGlobals::FanType_SystemModelObject) {

                    Fans::SimulateFanComponents(state, OutAirUnit(OAUnitNum).ExtFanName, true, OutAirUnit(OAUnitNum).ExtFan_Index);
                    OutAirUnit(OAUnitNum).EFanMaxAirVolFlow =
                        GetFanDesignVolumeFlowRate(state, cFanTypes(OutAirUnit(OAUnitNum).ExtFanType), OutAirUnit(OAUnitNum).ExtFanName, ErrorsFound);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->simulate(state, _, _, _, _);
                    OutAirUnit(OAUnitNum).EFanMaxAirVolFlow = state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->designAirVolFlowRate;
                }
            }
        }

        for (CompNum = 1; CompNum <= OutAirUnit(OAUnitNum).NumComponents; ++CompNum) {
            if ((OutAirUnit(OAUnitNum).OAEquip(CompNum).Type == CompType::WaterCoil_Cooling) ||
                (OutAirUnit(OAUnitNum).OAEquip(CompNum).Type == CompType::WaterCoil_DetailedCool)) {
                if (OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow == AutoSize) {
                    SimulateWaterCoilComponents(state,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                true,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                _,
                                                1,
                                                0.0);
                }
            }
            if (OutAirUnit(OAUnitNum).OAEquip(CompNum).Type == CompType::WaterCoil_SimpleHeat) {
                if (OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow == AutoSize) {
                    SimulateWaterCoilComponents(state,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                                true,
                                                OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                                _,
                                                1,
                                                0.0);
                }
            }
            if (OutAirUnit(OAUnitNum).OAEquip(CompNum).Type == CompType::SteamCoil_AirHeat) {
                if (OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow == AutoSize) {
                    SimulateSteamCoilComponents(
                        state, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName, true, OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex);
                }
            }
            if (OutAirUnit(OAUnitNum).OAEquip(CompNum).Type == CompType::WaterCoil_CoolingHXAsst) {
                if (OutAirUnit(OAUnitNum).OAEquip(CompNum).MaxVolWaterFlow == AutoSize) {
                    SimHXAssistedCoolingCoil(state,
                                             OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentName,
                                             true,
                                             DataHVACGlobals::CompressorOperation::On,
                                             0.0,
                                             OutAirUnit(OAUnitNum).OAEquip(CompNum).ComponentIndex,
                                             ContFanCycCoil);
                }
            }
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void CalcOutdoorAirUnit(EnergyPlusData &state,
                            int &OAUnitNum,                // number of the current unit being simulated
                            int const ZoneNum,             // number of zone being served
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            Real64 &PowerMet,              // power supplied
                            Real64 &LatOutputProvided      // Latent power supplied (kg/s), negative = dehumidification
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine mainly controls the action of the outdoor air unit
        // (or more exactly, it controls the coil outlet temperature of the unit)
        // based on the user input for controls and the defined controls
        // algorithms.

        // METHODOLOGY EMPLOYED:
        // Outdoor air unit is controlled based on user input and what is happening in the
        // simulation.
        // Note: controls are strictly temperature based and do not factor
        // humidity into the equation (not an enthalpy economy cycle but rather
        // a simple return air cycle).

        // REFERENCES:
        // ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

        // USE STATEMENTS:

        // Using/Aliasing
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;
        using HeatingCoils::CheckHeatingCoilSchedule;
        using HVACHXAssistedCoolingCoil::CheckHXAssistedCoolingCoilSchedule;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // INTERFACE BLOCK SPECIFICATIONS

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 DesOATemp;    // Design OA Temp degree C
        Real64 AirMassFlow;  // air mass flow rate [kg/s]
        int ControlNode;     // the hot water or cold water inlet node
        int InletNode;       // Unit air inlet node
        int SFanOutletNode;  // Unit supply fan outlet node
        int OutletNode;      // air outlet node
        int OutsideAirNode;  // outside air node
        Real64 QTotUnitOut;  // total unit output [watts]
        Real64 QUnitOut;     // heating or sens. cooling provided by fan coil unit [watts]
        Real64 LatLoadMet;   // heating or sens. cooling provided by fan coil unit [watts]
        Real64 MinHumRat;    // desired temperature after mixing inlet and outdoor air [degrees C]
        Real64 SetPointTemp; // temperature that will be used to control the radiant system [Celsius]
        Real64 HiCtrlTemp;   // Current high point in setpoint temperature range
        Real64 LoCtrlTemp;   // Current low point in setpoint temperature range
        Real64 AirInEnt;     // RE-calcualte the Enthalpy of supply air
        Real64 AirOutletTemp;
        Operation OperatingMode;
        Real64 ZoneSupAirEnt; // Specific humidity ratio of inlet air (kg moisture / kg moist air)
        // Latent output
        Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
        Real64 SpecHumOut;   // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn;    // Specific humidity ratio of inlet air (kg moisture / kg moist air)
        Real64 ZoneAirEnt;   // zone air enthalphy J/kg

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);

        // initialize local variables
        ControlNode = 0;
        QUnitOut = 0.0;
        if (OutAirUnit(OAUnitNum).ExtFan) InletNode = OutAirUnit(OAUnitNum).AirInletNode;
        SFanOutletNode = OutAirUnit(OAUnitNum).SFanOutletNode;
        OutletNode = OutAirUnit(OAUnitNum).AirOutletNode;
        OutsideAirNode = OutAirUnit(OAUnitNum).OutsideAirNode;
        OperatingMode = OutAirUnit(OAUnitNum).OperatingMode;
        OAUnitCtrlType UnitControlType = OutAirUnit(OAUnitNum).controlType;
        AirOutletTemp = 0.0;
        OutAirUnit(OAUnitNum).CompOutSetTemp = 0.0;
        OutAirUnit(OAUnitNum).FanEffect = false;

        if ((GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).SchedPtr) <= 0) ||
            (GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).OutAirSchedPtr) <= 0) ||
            ((GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).SFanAvailSchedPtr) <= 0) && !ZoneCompTurnFansOn) || ZoneCompTurnFansOff) {
            // System is off or has no load upon the unit; set the flow rates to zero and then
            // simulate the components with the no flow conditions
            if (OutAirUnit(OAUnitNum).ExtFan) state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
            if (OutAirUnit(OAUnitNum).ExtFan) state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
            if (OutAirUnit(OAUnitNum).ExtFan) state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(SFanOutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(SFanOutletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(SFanOutletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
            AirMassFlow = state.dataLoopNodes->Node(SFanOutletNode).MassFlowRate;

            // Node condition
            if (OutAirUnit(OAUnitNum).ExtFan) {
                state.dataLoopNodes->Node(InletNode).Temp = state.dataHeatBalFanSys->MAT(ZoneNum);
                state.dataLoopNodes->Node(SFanOutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
            } else {
                state.dataLoopNodes->Node(SFanOutletNode).Temp = state.dataHeatBalFanSys->MAT(ZoneNum);
            }
            state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(SFanOutletNode).Temp;

            if (OutAirUnit(OAUnitNum).FanPlace == BlowThru) {
                if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).SFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).SFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }

                SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                if (OutAirUnit(OAUnitNum).ExtFan) {
                    if (OutAirUnit(OAUnitNum).ExtFanType != DataHVACGlobals::FanType_SystemModelObject) {
                        Fans::SimulateFanComponents(state,
                                                    OutAirUnit(OAUnitNum).ExtFanName,
                                                    FirstHVACIteration,
                                                    OutAirUnit(OAUnitNum).ExtFan_Index,
                                                    _,
                                                    ZoneCompTurnFansOn,
                                                    ZoneCompTurnFansOff); // why not turn on/off flags here?
                    } else {
                        state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->simulate(
                            state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                    }
                }

            } else if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
                SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).SFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).SFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
                if (OutAirUnit(OAUnitNum).ExtFan) {
                    if (OutAirUnit(OAUnitNum).ExtFanType != DataHVACGlobals::FanType_SystemModelObject) {
                        Fans::SimulateFanComponents(state,
                                                    OutAirUnit(OAUnitNum).ExtFanName,
                                                    FirstHVACIteration,
                                                    OutAirUnit(OAUnitNum).ExtFan_Index,
                                                    _,
                                                    ZoneCompTurnFansOn,
                                                    ZoneCompTurnFansOff);
                    } else {
                        state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->simulate(
                            state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                    }
                }
            }

        } else { // System On

            // Flowrate Check
            if (state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate > 0.0) {
                state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = OutAirUnit(OAUnitNum).OutAirMassFlow;
            }

            // Fan Positioning Check

            if (OutAirUnit(OAUnitNum).ExtFan) {
                state.dataLoopNodes->Node(InletNode).MassFlowRate = OutAirUnit(OAUnitNum).ExtAirMassFlow;
            }

            // Air mass balance check
            if ((std::abs(OutAirUnit(OAUnitNum).ExtAirMassFlow - OutAirUnit(OAUnitNum).OutAirMassFlow) > 0.001) &&
                (!state.dataHeatBal->ZoneAirMassFlow.EnforceZoneMassBalance)) {
                if (!OutAirUnit(OAUnitNum).FlowError) {
                    ShowWarningError(state, "Air mass flow between zone supply and exhaust is not balanced. Only the first occurrence is reported.");
                    ShowContinueError(state, "Occurs in ZoneHVAC:OutdoorAirUnit Object= " + OutAirUnit(OAUnitNum).Name);
                    ShowContinueError(state,
                                      "Air mass balance is required by other outdoor air units: Fan:ZoneExhaust, ZoneMixing, ZoneCrossMixing, or "
                                      "other air flow control inputs.");
                    ShowContinueErrorTimeStamp(state,
                                               format("The outdoor mass flow rate = {:.3R} and the exhaust mass flow rate = {:.3R}.",
                                                      OutAirUnit(OAUnitNum).OutAirMassFlow,
                                                      OutAirUnit(OAUnitNum).ExtAirMassFlow));
                    OutAirUnit(OAUnitNum).FlowError = true;
                }
            }

            if (OutAirUnit(OAUnitNum).FanPlace == BlowThru) {
                if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).SFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).SFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
                DesOATemp = state.dataLoopNodes->Node(SFanOutletNode).Temp;
            } else if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
                DesOATemp = state.dataLoopNodes->Node(OutsideAirNode).Temp;
            }

            // Control type check
            switch (UnitControlType) {
            case OAUnitCtrlType::Neutral: {
                SetPointTemp = state.dataHeatBalFanSys->MAT(ZoneNum);
                // Neutral Control Condition
                if (DesOATemp == SetPointTemp) {
                    OutAirUnit(OAUnitNum).OperatingMode = Operation::NeutralMode;
                    AirOutletTemp = DesOATemp;
                    OutAirUnit(OAUnitNum).CompOutSetTemp = DesOATemp;
                    SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                } else {
                    if (DesOATemp < SetPointTemp) { // Heating MODE
                        OutAirUnit(OAUnitNum).OperatingMode = Operation::HeatingMode;
                        AirOutletTemp = SetPointTemp;
                        OutAirUnit(OAUnitNum).CompOutSetTemp = AirOutletTemp;
                        SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                    } else if (DesOATemp > SetPointTemp) { // Cooling Mode
                        OutAirUnit(OAUnitNum).OperatingMode = Operation::CoolingMode;
                        AirOutletTemp = SetPointTemp;
                        OutAirUnit(OAUnitNum).CompOutSetTemp = AirOutletTemp;
                        SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                    }
                }
                // SetPoint Temperature Condition
            } break;
            case OAUnitCtrlType::Temperature: {
                SetPointTemp = DesOATemp;
                HiCtrlTemp = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).HiCtrlTempSchedPtr);
                LoCtrlTemp = GetCurrentScheduleValue(state, OutAirUnit(OAUnitNum).LoCtrlTempSchedPtr);
                if ((DesOATemp <= HiCtrlTemp) && (DesOATemp >= LoCtrlTemp)) {
                    OutAirUnit(OAUnitNum).OperatingMode = Operation::NeutralMode;
                    AirOutletTemp = DesOATemp;
                    OutAirUnit(OAUnitNum).CompOutSetTemp = DesOATemp;
                    SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                } else {
                    if (SetPointTemp < LoCtrlTemp) {
                        OutAirUnit(OAUnitNum).OperatingMode = Operation::HeatingMode;
                        AirOutletTemp = LoCtrlTemp;
                        OutAirUnit(OAUnitNum).CompOutSetTemp = AirOutletTemp;
                        SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                    } else if (SetPointTemp > HiCtrlTemp) {
                        OutAirUnit(OAUnitNum).OperatingMode = Operation::CoolingMode;
                        AirOutletTemp = HiCtrlTemp;
                        OutAirUnit(OAUnitNum).CompOutSetTemp = AirOutletTemp;
                        SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                    }
                }
            } break;
            default:
                break;
            }

            // Fan positioning
            if (OutAirUnit(OAUnitNum).FanPlace == DrawThru) {
                if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).SFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).SFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }

                OutAirUnit(OAUnitNum).FanEffect = true; // RE-Simulation to take over the supply fan effect
                OutAirUnit(OAUnitNum).FanCorTemp = (state.dataLoopNodes->Node(OutletNode).Temp - OutAirUnit(OAUnitNum).CompOutSetTemp);
                SimZoneOutAirUnitComps(state, OAUnitNum, FirstHVACIteration);
                if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).SFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).SFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
                OutAirUnit(OAUnitNum).FanEffect = false;
            }
            if (OutAirUnit(OAUnitNum).ExtFan) {
                if (OutAirUnit(OAUnitNum).ExtFanType != DataHVACGlobals::FanType_SystemModelObject) {
                    Fans::SimulateFanComponents(state,
                                                OutAirUnit(OAUnitNum).ExtFanName,
                                                FirstHVACIteration,
                                                OutAirUnit(OAUnitNum).ExtFan_Index,
                                                _,
                                                ZoneCompTurnFansOn,
                                                ZoneCompTurnFansOff);
                } else {
                    state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
                }
            }
        } // ...end of system ON/OFF IF-THEN block

        AirMassFlow = state.dataLoopNodes->Node(OutletNode).MassFlowRate;
        MinHumRat = min(state.dataLoopNodes->Node(OutletNode).HumRat, state.dataLoopNodes->Node(OutAirUnit(OAUnitNum).ZoneNodeNum).HumRat);

        AirInEnt = PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat);                          // zone supply air node enthalpy
        ZoneAirEnt = PsyHFnTdbW(state.dataLoopNodes->Node(OutAirUnit(OAUnitNum).ZoneNodeNum).Temp, MinHumRat); // zone air enthalpy
        QUnitOut = AirMassFlow * (AirInEnt - ZoneAirEnt);                                                      // Senscooling

        // CR9155 Remove specific humidity calculations
        SpecHumOut = state.dataLoopNodes->Node(OutletNode).HumRat;
        SpecHumIn = state.dataLoopNodes->Node(OutAirUnit(OAUnitNum).ZoneNodeNum).HumRat;
        LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative

        ZoneAirEnt = PsyHFnTdbW(state.dataLoopNodes->Node(OutAirUnit(OAUnitNum).ZoneNodeNum).Temp,
                                state.dataLoopNodes->Node(OutAirUnit(OAUnitNum).ZoneNodeNum).HumRat);

        ZoneSupAirEnt = PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(OutletNode).HumRat);
        QTotUnitOut = AirMassFlow * (ZoneSupAirEnt - ZoneAirEnt);
        LatLoadMet = QTotUnitOut - QUnitOut; // watts

        // Report variables...

        if (QUnitOut < 0.0) {
            OutAirUnit(OAUnitNum).SensCoolingRate = std::abs(QUnitOut);
            OutAirUnit(OAUnitNum).SensHeatingRate = 0.0;
        } else {
            OutAirUnit(OAUnitNum).SensCoolingRate = 0.0;
            OutAirUnit(OAUnitNum).SensHeatingRate = QUnitOut;
        }

        if (QTotUnitOut < 0.0) {
            OutAirUnit(OAUnitNum).TotCoolingRate = std::abs(QTotUnitOut);
            OutAirUnit(OAUnitNum).TotHeatingRate = 0.0;
        } else {
            OutAirUnit(OAUnitNum).TotCoolingRate = 0.0;
            OutAirUnit(OAUnitNum).TotHeatingRate = QTotUnitOut;
        }

        if (LatLoadMet < 0.0) {
            OutAirUnit(OAUnitNum).LatCoolingRate = std::abs(LatLoadMet);
            OutAirUnit(OAUnitNum).LatHeatingRate = 0.0;
        } else {
            OutAirUnit(OAUnitNum).LatCoolingRate = 0.0;
            OutAirUnit(OAUnitNum).LatHeatingRate = LatLoadMet;
        }

        // OutAirUnit( OAUnitNum ).ElecFanRate = FanElecPower;  //Issue #5524 this would only get the last fan called, not both if there are two
        OutAirUnit(OAUnitNum).ElecFanRate = 0.0;
        if (OutAirUnit(OAUnitNum).SFanType != DataHVACGlobals::FanType_SystemModelObject) {
            OutAirUnit(OAUnitNum).ElecFanRate += Fans::GetFanPower(state, OutAirUnit(OAUnitNum).SFan_Index);
        } else {
            OutAirUnit(OAUnitNum).ElecFanRate += state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).SFan_Index]->fanPower();
        }
        if (OutAirUnit(OAUnitNum).ExtFan) {
            if (OutAirUnit(OAUnitNum).ExtFanType != DataHVACGlobals::FanType_SystemModelObject) {
                OutAirUnit(OAUnitNum).ElecFanRate += Fans::GetFanPower(state, OutAirUnit(OAUnitNum).ExtFan_Index);
            } else {
                OutAirUnit(OAUnitNum).ElecFanRate += state.dataHVACFan->fanObjs[OutAirUnit(OAUnitNum).ExtFan_Index]->fanPower();
            }
        }

        PowerMet = QUnitOut;
        LatOutputProvided = LatentOutput;
    }

    void SimZoneOutAirUnitComps(EnergyPlusData &state, int const OAUnitNum, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   Oct 1998
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Simulate the controllers and components in the outside air system.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int EquipNum;
        int CurOAUnitNum;
        std::string EquipType;
        std::string EquipName;
        bool FatalErrorFlag;
        bool Sim;

        FatalErrorFlag = false;
        CurOAUnitNum = OAUnitNum;
        Sim = true;
        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);
        for (EquipNum = 1; EquipNum <= OutAirUnit(OAUnitNum).NumComponents; ++EquipNum) {
            EquipName = OutAirUnit(OAUnitNum).OAEquip(EquipNum).ComponentName;
            SimOutdoorAirEquipComps(state,
                                    OAUnitNum,
                                    CompTypeNames[static_cast<int>(OutAirUnit(OAUnitNum).OAEquip(EquipNum).Type)],
                                    EquipName,
                                    EquipNum,
                                    OutAirUnit(OAUnitNum).OAEquip(EquipNum).Type,
                                    FirstHVACIteration,
                                    OutAirUnit(OAUnitNum).OAEquip(EquipNum).ComponentIndex,
                                    Sim);
        }

        CurOAUnitNum = 0;
    }

    void SimOutdoorAirEquipComps(EnergyPlusData &state,
                                 int const OAUnitNum,          // actual outdoor air unit num
                                 std::string_view EquipType,   // the component type
                                 std::string const &EquipName, // the component Name
                                 int const EquipNum,
                                 [[maybe_unused]] CompType const CompTypeNum, // Component Type -- Integerized for this module
                                 bool const FirstHVACIteration,
                                 int &CompIndex,
                                 bool const Sim // if TRUE, simulate component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2008
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Outdoor air unit has various coil options. This subroutine defines the coil loads and execute
        // to simulate each components
        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using DesiccantDehumidifiers::SimDesiccantDehumidifier;
        using HeatRecovery::SimHeatRecovery;
        using HVACDXHeatPumpSystem::SimDXHeatPumpSystem;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using ScheduleManager::GetCurrentScheduleValue;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE LOCAL VARIABLE DEFINITIONS
        Real64 OAMassFlow;
        Real64 QCompReq;
        int UnitNum;
        Real64 MaxWaterFlow;
        Real64 MinWaterFlow;
        int ControlNode;
        Real64 CpAirZn;
        int SimCompNum;
        CompType EquipTypeNum;
        int WCCoilInletNode;
        int WCCoilOutletNode;
        int WHCoilInletNode;
        int WHCoilOutletNode;
        Real64 QUnitOut;
        int DXSystemIndex(0);
        Real64 CompAirOutTemp;
        Real64 FanEffect;
        bool DrawFan; // fan position If .True., the temperature increasing by fan operating is considered
        Real64 Dxsystemouttemp;
        auto &HeatActive = state.dataOutdoorAirUnit->HeatActive;
        auto &CoolActive = state.dataOutdoorAirUnit->CoolActive;

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);

        UnitNum = OAUnitNum;
        CompAirOutTemp = OutAirUnit(OAUnitNum).CompOutSetTemp;
        Operation OpMode = OutAirUnit(OAUnitNum).OperatingMode;
        SimCompNum = EquipNum;
        EquipTypeNum = OutAirUnit(OAUnitNum).OAEquip(SimCompNum).Type;
        OAMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;
        DrawFan = OutAirUnit(OAUnitNum).FanEffect;
        DXSystemIndex = 0;

        // check the fan positioning
        if (DrawFan) {
            FanEffect = OutAirUnit(OAUnitNum).FanCorTemp; // Heat effect by fan
        } else {
            FanEffect = 0.0;
        }

        // checking equipment index

        {
            switch (EquipTypeNum) {
            // Heat recovery
            case CompType::HeatXchngrFP: // 'HeatExchanger:AirToAir:FlatPlate',
            case CompType::HeatXchngrSL: // 'HeatExchanger:AirToAir:SensibleAndLatent',
                                         // 'HeatExchanger:Desiccant:BalancedFlow' - unused
            {

                if (Sim) {
                    SimHeatRecovery(state, EquipName, FirstHVACIteration, CompIndex, ContFanCycCoil, _, _, _, _, false, false);
                }
            } break;
            // Desiccant Dehumidifier
            case CompType::Desiccant: { // 'Dehumidifier:Desiccant:NoFans'
                if (Sim) {
                    SimDesiccantDehumidifier(state, EquipName, FirstHVACIteration, CompIndex);
                }

            } break;
            case CompType::WaterCoil_SimpleHeat: { // ('Coil:Heating:Water')

                if (Sim) {
                    ControlNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilWaterInletNode;
                    MaxWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MaxWaterMassFlow;
                    MinWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MinWaterMassFlow;
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if ((!FirstHVACIteration) && (ControlNode > 0)) {
                        MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                        MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                    }
                    WHCoilInletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirInletNode;
                    WHCoilOutletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirOutletNode;

                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(WHCoilInletNode).HumRat);

                    if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::CoolingMode) ||
                        (state.dataLoopNodes->Node(WHCoilInletNode).Temp > CompAirOutTemp)) {
                        QCompReq = 0.0;
                    } else {
                        QCompReq = CpAirZn * OAMassFlow * ((CompAirOutTemp - state.dataLoopNodes->Node(WHCoilInletNode).Temp) - FanEffect);
                        if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                        if (QCompReq < 0.0) QCompReq = 0.0; // coil can heat only
                    }

                    ControlCompOutput(state,
                                      OutAirUnit(OAUnitNum).Name,
                                      std::string(ZoneHVACOAUnit),
                                      UnitNum,
                                      FirstHVACIteration,
                                      QCompReq,
                                      ControlNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      0.0001,
                                      OutAirUnit(OAUnitNum).ControlCompTypeNum,
                                      OutAirUnit(OAUnitNum).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      2,
                                      SimCompNum,
                                      OutAirUnit(OAUnitNum).OAEquip(EquipNum).plantLoc);
                }
            } break;
            case CompType::SteamCoil_AirHeat: { // 'Coil:Heating:Steam'
                if (Sim) {
                    CalcOAUnitCoilComps(state, UnitNum, FirstHVACIteration, SimCompNum, QUnitOut);
                }
            } break;
            case CompType::Coil_ElectricHeat: // 'Coil:Heating:Electric'
            case CompType::Coil_GasHeat: {    // 'Coil:Heating:Fuel'
                if (Sim) {
                    //     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
                    CalcOAUnitCoilComps(state, UnitNum, FirstHVACIteration, SimCompNum, QUnitOut);
                }
            } break;
                // water cooling coil Types
            case CompType::WaterCoil_Cooling: { // 'Coil:Cooling:Water'
                if (Sim) {
                    ControlNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilWaterInletNode;
                    MaxWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MaxWaterMassFlow;
                    MinWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MinWaterMassFlow;
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if ((!FirstHVACIteration) && (ControlNode > 0)) {
                        MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                        MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                    }
                    WCCoilInletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirInletNode;
                    WCCoilOutletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirOutletNode;

                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(WCCoilInletNode).HumRat);
                    OAMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;
                    if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::HeatingMode) ||
                        (state.dataLoopNodes->Node(WCCoilInletNode).Temp < CompAirOutTemp)) {
                        QCompReq = 0.0;
                        state.dataLoopNodes->Node(WCCoilOutletNode).Temp = state.dataLoopNodes->Node(WCCoilInletNode).Temp;
                        state.dataLoopNodes->Node(WCCoilOutletNode).HumRat = state.dataLoopNodes->Node(WCCoilInletNode).HumRat;
                        state.dataLoopNodes->Node(WCCoilOutletNode).MassFlowRate = state.dataLoopNodes->Node(WCCoilInletNode).MassFlowRate;

                    } else {

                        QCompReq = CpAirZn * OAMassFlow * ((CompAirOutTemp - state.dataLoopNodes->Node(WCCoilInletNode).Temp) - FanEffect);
                        if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                        if (QCompReq > 0.0) QCompReq = 0.0; // coil can cool only
                    }

                    ControlCompOutput(state,
                                      OutAirUnit(OAUnitNum).Name,
                                      std::string(ZoneHVACOAUnit),
                                      UnitNum,
                                      FirstHVACIteration,
                                      QCompReq,
                                      ControlNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      0.001,
                                      OutAirUnit(OAUnitNum).ControlCompTypeNum,
                                      OutAirUnit(OAUnitNum).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      1,
                                      SimCompNum,
                                      OutAirUnit(OAUnitNum).OAEquip(EquipNum).plantLoc);
                }
            } break;
            case CompType::WaterCoil_DetailedCool: { // 'Coil:Cooling:Water:DetailedGeometry'
                if (Sim) {
                    ControlNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilWaterInletNode;
                    MaxWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MaxWaterMassFlow;
                    MinWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MinWaterMassFlow;
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if ((!FirstHVACIteration) && (ControlNode > 0)) {
                        MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                        MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                    }
                    WCCoilInletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirInletNode;
                    WCCoilOutletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirOutletNode;

                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(WCCoilInletNode).HumRat);
                    OAMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;

                    if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::HeatingMode) ||
                        (state.dataLoopNodes->Node(WCCoilInletNode).Temp < CompAirOutTemp)) {
                        QCompReq = 0.0;
                    } else {

                        QCompReq = CpAirZn * OAMassFlow * ((CompAirOutTemp - state.dataLoopNodes->Node(WCCoilInletNode).Temp) - FanEffect);
                        if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                        if (QCompReq > 0.0) QCompReq = 0.0; // coil can cool only
                    }

                    ControlCompOutput(state,
                                      OutAirUnit(OAUnitNum).Name,
                                      "ZONEHVAC:OUTDOORAIRUNIT",
                                      UnitNum,
                                      FirstHVACIteration,
                                      QCompReq,
                                      ControlNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      0.001,
                                      OutAirUnit(OAUnitNum).ControlCompTypeNum,
                                      OutAirUnit(OAUnitNum).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      1,
                                      SimCompNum,
                                      OutAirUnit(OAUnitNum).OAEquip(EquipNum).plantLoc);
                }
            } break;
            case CompType::WaterCoil_CoolingHXAsst: { // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
                if (Sim) {
                    ControlNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilWaterInletNode;
                    MaxWaterFlow = OutAirUnit(OAUnitNum).OAEquip(EquipNum).MaxWaterMassFlow;
                    MinWaterFlow = 0.0;
                    // On the first HVAC iteration the system values are given to the controller, but after that
                    // the demand limits are in place and there needs to be feedback to the Zone Equipment
                    if ((!FirstHVACIteration) && (ControlNode > 0)) {
                        MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
                        MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
                    }
                    WCCoilInletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirInletNode;
                    WCCoilOutletNode = OutAirUnit(OAUnitNum).OAEquip(EquipNum).CoilAirOutletNode;
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(WCCoilInletNode).HumRat);
                    OAMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;
                    if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::HeatingMode) ||
                        (state.dataLoopNodes->Node(WCCoilInletNode).Temp < CompAirOutTemp)) {
                        QCompReq = 0.0;
                    } else {
                        QCompReq = CpAirZn * OAMassFlow * ((CompAirOutTemp - state.dataLoopNodes->Node(WCCoilInletNode).Temp) - FanEffect);
                        if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                        if (QCompReq > 0.0) QCompReq = 0.0; // coil can cool only
                    }
                    ControlCompOutput(state,
                                      OutAirUnit(OAUnitNum).Name,
                                      "ZONEHVAC:OUTDOORAIRUNIT",
                                      UnitNum,
                                      FirstHVACIteration,
                                      QCompReq,
                                      ControlNode,
                                      MaxWaterFlow,
                                      MinWaterFlow,
                                      0.001,
                                      OutAirUnit(OAUnitNum).ControlCompTypeNum,
                                      OutAirUnit(OAUnitNum).CompErrIndex,
                                      _,
                                      _,
                                      _,
                                      1,
                                      SimCompNum,
                                      OutAirUnit(OAUnitNum).OAEquip(EquipNum).plantLoc);
                }
            } break;
            case CompType::DXSystem: { // CoilSystem:Cooling:DX  old 'CompType:UnitaryCoolOnly'
                if (Sim) {
                    if (OutAirUnit(OAUnitNum).OAEquip(SimCompNum).compPointer == nullptr) {
                        UnitarySystems::UnitarySys thisSys;
                        OutAirUnit(OAUnitNum).OAEquip(SimCompNum).compPointer =
                            thisSys.factory(state,
                                            DataHVACGlobals::UnitarySys_AnyCoilType,
                                            OutAirUnit(OAUnitNum).OAEquip(SimCompNum).ComponentName,
                                            false,
                                            OAUnitNum);
                        UnitarySystems::UnitarySys::checkUnitarySysCoilInOASysExists(
                            state, OutAirUnit(OAUnitNum).OAEquip(SimCompNum).ComponentName, OAUnitNum);
                    }
                    if (((OpMode == Operation::NeutralMode) && (OutAirUnit(OAUnitNum).controlType == OAUnitCtrlType::Temperature)) ||
                        (OpMode == Operation::HeatingMode)) {
                        Dxsystemouttemp = 100.0; // There is no cooling demand for the DX system.
                    } else {
                        Dxsystemouttemp = CompAirOutTemp - FanEffect;
                    }
                    Real64 sensOut = 0.0;
                    Real64 latOut = 0.0;
                    OutAirUnit(OAUnitNum)
                        .OAEquip(SimCompNum)
                        .compPointer->simulate(state,
                                               EquipName,
                                               FirstHVACIteration,
                                               -1,
                                               DXSystemIndex,
                                               HeatActive,
                                               CoolActive,
                                               UnitNum,
                                               Dxsystemouttemp,
                                               false,
                                               sensOut,
                                               latOut);
                }
            } break;
            case CompType::DXHeatPumpSystem: {
                if (Sim) {
                    if (((OpMode == Operation::NeutralMode) && (OutAirUnit(OAUnitNum).controlType == OAUnitCtrlType::Temperature)) ||
                        (OpMode == Operation::CoolingMode)) {
                        Dxsystemouttemp = -20.0; // There is no heating demand for the DX system.
                    } else {
                        Dxsystemouttemp = CompAirOutTemp - FanEffect;
                    }
                    SimDXHeatPumpSystem(state, EquipName, FirstHVACIteration, -1, DXSystemIndex, UnitNum, Dxsystemouttemp);
                }
            } break;
                // RAR need new CompType:UnitarySystem object here
            case CompType::UnitarySystemModel: { // 'CompType:UnitarySystem'
                if (Sim) {
                    // This may have to be done in the unitary system object since there can be both cooling and heating
                    if (((OpMode == Operation::NeutralMode) && (OutAirUnit(OAUnitNum).controlType == OAUnitCtrlType::Temperature)) &&
                        (OpMode == Operation::HeatingMode)) {
                        Dxsystemouttemp = 100.0; // There is no cooling demand.
                    } else if (((OpMode == Operation::NeutralMode) && (OutAirUnit(OAUnitNum).controlType == OAUnitCtrlType::Temperature)) &&
                               (OpMode == Operation::CoolingMode)) {
                        Dxsystemouttemp = -20.0; // There is no heating demand.
                    } else {
                        Dxsystemouttemp = CompAirOutTemp - FanEffect;
                    }
                    Real64 sensOut = 0.0;
                    Real64 latOut = 0.0;
                    OutAirUnit(OAUnitNum)
                        .OAEquip(SimCompNum)
                        .compPointer->simulate(state,
                                               EquipName,
                                               FirstHVACIteration,
                                               -1,
                                               DXSystemIndex,
                                               HeatActive,
                                               CoolActive,
                                               UnitNum,
                                               Dxsystemouttemp,
                                               false,
                                               sensOut,
                                               latOut);
                }
            } break;
            default: {
                ShowFatalError(state, format("Invalid Outdoor Air Unit Component={}", EquipType)); // validate
            } break;
            }
        }
    }

    void CalcOAUnitCoilComps(EnergyPlusData &state,
                             int const CompNum, // actual outdoor air unit num
                             bool const FirstHVACIteration,
                             int const EquipIndex, // Component Type -- Integerized for this module
                             Real64 &LoadMet)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young Tae Chae, Rick Strand
        //       DATE WRITTEN   June 2009
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine mainly controls the action of water components in the unit

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using SteamCoils::SimulateSteamCoilComponents;
        using WaterCoils::SimulateWaterCoilComponents;

        // SUBROUTINE ARGUMENT DEFINITIONS:

        // Locals
        // SUBROUTINE LOCAL VARIABLE DEFINITIONS
        int OAUnitNum;
        Real64 CpAirZn;
        int CoilIndex;
        Operation OpMode;
        Real64 AirMassFlow;
        Real64 FanEffect;
        bool DrawFan; // Fan Flag
        int InletNode;
        int OutletNode;
        Real64 QCompReq; // Actual equipment load
        CompType CoilTypeNum;
        Real64 CoilAirOutTemp;
        int CompoNum;

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);

        CoilIndex = 0;
        OAUnitNum = CompNum;
        CompoNum = EquipIndex;
        CoilTypeNum = OutAirUnit(OAUnitNum).OAEquip(CompoNum).Type;
        OpMode = OutAirUnit(OAUnitNum).OperatingMode;
        CoilAirOutTemp = OutAirUnit(OAUnitNum).CompOutSetTemp;
        DrawFan = OutAirUnit(OAUnitNum).FanEffect;
        if (DrawFan) {
            FanEffect = OutAirUnit(OAUnitNum).FanCorTemp;
        } else {
            FanEffect = 0.0;
        }

        {
            switch (CoilTypeNum) {
            case CompType::Coil_ElectricHeat: {
                InletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirInletNode;
                OutletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirOutletNode;
                if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::CoolingMode) ||
                    (state.dataLoopNodes->Node(InletNode).Temp > CoilAirOutTemp)) {
                    QCompReq = 0.0;
                } else {
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(InletNode).HumRat);
                    QCompReq = state.dataLoopNodes->Node(InletNode).MassFlowRate * CpAirZn *
                               ((CoilAirOutTemp - state.dataLoopNodes->Node(InletNode).Temp) - FanEffect);
                    if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                }

                if (QCompReq <= 0.0) {
                    QCompReq = 0.0; // a heating coil can only heat, not cool
                    state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
                    state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                }
                HeatingCoils::SimulateHeatingCoilComponents(
                    state, OutAirUnit(OAUnitNum).OAEquip(CompoNum).ComponentName, FirstHVACIteration, QCompReq, CoilIndex);

                AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                         PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));

            } break;
            case CompType::Coil_GasHeat: { // 'Coil:Heating:Steam'
                InletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirInletNode;
                OutletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirOutletNode;
                if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::CoolingMode) ||
                    (state.dataLoopNodes->Node(InletNode).Temp > CoilAirOutTemp)) {
                    QCompReq = 0.0;
                } else {
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(InletNode).HumRat);
                    QCompReq = state.dataLoopNodes->Node(InletNode).MassFlowRate * CpAirZn *
                               ((CoilAirOutTemp - state.dataLoopNodes->Node(InletNode).Temp) - FanEffect);
                    if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                }
                if (QCompReq <= 0.0) {
                    QCompReq = 0.0; // a heating coil can only heat, not cool
                    state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
                    state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                }
                HeatingCoils::SimulateHeatingCoilComponents(
                    state, OutAirUnit(OAUnitNum).OAEquip(CompoNum).ComponentName, FirstHVACIteration, QCompReq, CoilIndex);

                AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                         PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));

            } break;
            case CompType::SteamCoil_AirHeat: { // 'Coil:Heating:Steam'
                InletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirInletNode;
                OutletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirOutletNode;
                if ((OpMode == Operation::NeutralMode) || (OpMode == Operation::CoolingMode) ||
                    (state.dataLoopNodes->Node(InletNode).Temp > CoilAirOutTemp)) {
                    QCompReq = 0.0;
                } else {
                    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(InletNode).HumRat);
                    QCompReq = state.dataLoopNodes->Node(InletNode).MassFlowRate * CpAirZn *
                               ((CoilAirOutTemp - state.dataLoopNodes->Node(InletNode).Temp) - FanEffect);
                    if (std::abs(QCompReq) < SmallLoad) QCompReq = 0.0;
                }
                if (QCompReq <= 0.0) {
                    QCompReq = 0.0; // a heating coil can only heat, not cool
                    state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
                    state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;
                    state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                }
                SimulateSteamCoilComponents(state, OutAirUnit(OAUnitNum).OAEquip(CompoNum).ComponentName, FirstHVACIteration, CoilIndex, QCompReq);
                AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                         PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));

            } break;
            case CompType::WaterCoil_SimpleHeat: // 'Coil:Heating:Water')
            case CompType::WaterCoil_Cooling:    // 'Coil:Cooling:Water'
            case CompType::WaterCoil_DetailedCool: {
                SimulateWaterCoilComponents(state, OutAirUnit(OAUnitNum).OAEquip(CompoNum).ComponentName, FirstHVACIteration, CoilIndex);
                InletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirInletNode;
                OutletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirOutletNode;
                AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                         PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));

            } break;
            case CompType::WaterCoil_CoolingHXAsst: {
                SimHXAssistedCoolingCoil(state,
                                         OutAirUnit(OAUnitNum).OAEquip(CompoNum).ComponentName,
                                         FirstHVACIteration,
                                         DataHVACGlobals::CompressorOperation::On,
                                         0.0,
                                         CoilIndex,
                                         ContFanCycCoil);
                InletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirInletNode;
                OutletNode = OutAirUnit(OAUnitNum).OAEquip(CompoNum).CoilAirOutletNode;
                AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat) -
                                         PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, state.dataLoopNodes->Node(InletNode).HumRat));
            } break;
            default:
                ShowFatalError(state, format("Invalid Coil Type = {}", CoilTypeNum)); // validate
                break;
            }
        }
    }

    // SUBROUTINE UpdateOutdoorAirUnit

    // No update routine needed in this module since all of the updates happen on
    // the Node derived type directly and these updates are done by other routines.

    // END SUBROUTINE UpdateOutdoorAirUnit

    void ReportOutdoorAirUnit(EnergyPlusData &state,
                              int const OAUnitNum) // Index for the outdoor air unit under consideration within the derived types
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Young T. Chae
        //       DATE WRITTEN   Oct. 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simply produces output for the outdoor air unit.
        // METHODOLOGY EMPLOYED:
        // Standard EnergyPlus methodology.

        // Using/Aliasing
        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        auto &OutAirUnit(state.dataOutdoorAirUnit->OutAirUnit);
        OutAirUnit(OAUnitNum).TotHeatingEnergy = OutAirUnit(OAUnitNum).TotHeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).SensHeatingEnergy = OutAirUnit(OAUnitNum).SensHeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).LatHeatingEnergy = OutAirUnit(OAUnitNum).LatHeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).SensCoolingEnergy = OutAirUnit(OAUnitNum).SensCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).LatCoolingEnergy = OutAirUnit(OAUnitNum).LatCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).TotCoolingEnergy = OutAirUnit(OAUnitNum).TotCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
        OutAirUnit(OAUnitNum).AirMassFlow = OutAirUnit(OAUnitNum).OutAirMassFlow;
        OutAirUnit(OAUnitNum).ElecFanEnergy = OutAirUnit(OAUnitNum).ElecFanRate * TimeStepSys * DataGlobalConstants::SecInHour;

        if (OutAirUnit(OAUnitNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(state, state.dataSize->CurZoneEqNum, 0, OutAirUnit(OAUnitNum).FirstPass);
            }
        }
    }

    int GetOutdoorAirUnitOutAirNode(EnergyPlusData &state, int const OAUnitNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node

        // Return value
        int GetOutdoorAirUnitOutAirNode;

        if (state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag) {
            OutdoorAirUnit::GetOutdoorAirUnitInputs(state);
            state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;
        }

        GetOutdoorAirUnitOutAirNode = 0;
        if (OAUnitNum > 0 && OAUnitNum <= state.dataOutdoorAirUnit->NumOfOAUnits) {
            GetOutdoorAirUnitOutAirNode = state.dataOutdoorAirUnit->OutAirUnit(OAUnitNum).OutsideAirNode;
        }

        return GetOutdoorAirUnitOutAirNode;
    }

    int GetOutdoorAirUnitZoneInletNode(EnergyPlusData &state, int const OAUnitNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node

        // Return value
        int GetOutdoorAirUnitZoneInletNode;

        if (state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag) {
            OutdoorAirUnit::GetOutdoorAirUnitInputs(state);
            state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;
        }

        GetOutdoorAirUnitZoneInletNode = 0;
        if (OAUnitNum > 0 && OAUnitNum <= state.dataOutdoorAirUnit->NumOfOAUnits) {
            GetOutdoorAirUnitZoneInletNode = state.dataOutdoorAirUnit->OutAirUnit(OAUnitNum).AirOutletNode;
        }

        return GetOutdoorAirUnitZoneInletNode;
    }

    int GetOutdoorAirUnitReturnAirNode(EnergyPlusData &state, int const OAUnitNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node

        // Return value
        int GetOutdoorAirUnitReturnAirNode;

        if (state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag) {
            OutdoorAirUnit::GetOutdoorAirUnitInputs(state);
            state.dataOutdoorAirUnit->GetOutdoorAirUnitInputFlag = false;
        }

        GetOutdoorAirUnitReturnAirNode = 0;
        if (OAUnitNum > 0 && OAUnitNum <= state.dataOutdoorAirUnit->NumOfOAUnits) {
            GetOutdoorAirUnitReturnAirNode = state.dataOutdoorAirUnit->OutAirUnit(OAUnitNum).AirInletNode;
        }

        return GetOutdoorAirUnitReturnAirNode;
    }

} // namespace OutdoorAirUnit

} // namespace EnergyPlus
