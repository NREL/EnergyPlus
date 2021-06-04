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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/AirTerminalUnit.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DualDuct.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACCooledBeam.hh>
#include <EnergyPlus/HVACFourPipeBeam.hh>
#include <EnergyPlus/HVACSingleDuctInduc.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

namespace EnergyPlus {

namespace ZoneAirLoopEquipmentManager {
    // Module containing the routines dealing with the ZoneAirLoopEquipmentManager

    // MODULE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997

    using namespace DataDefineEquip;

    void ManageZoneAirLoopEquipment(EnergyPlusData &state,
                                    std::string const &ZoneAirLoopEquipName,
                                    bool const FirstHVACIteration,
                                    Real64 &SysOutputProvided,
                                    Real64 &NonAirSysOutput,
                                    Real64 &LatOutputProvided, // Latent add/removal supplied by air dist unit (kg/s), dehumid = negative
                                    int const ActualZoneNum,
                                    int &ControlledZoneNum,
                                    int &CompIndex)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   May 1997
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)

        // PURPOSE OF THIS SUBROUTINE:
        // Calls the zone thermal control simulations and the interfaces
        // (water-air, refrigerant-air, steam-air, electric-electric,
        // water-water, etc)

        int AirDistUnitNum;

        // Beginning of Code

        GetZoneAirLoopEquipment(state);

        // Find the correct Zone Air Distribution Unit Equipment
        if (CompIndex == 0) {
            AirDistUnitNum = UtilityRoutines::FindItemInList(ZoneAirLoopEquipName, state.dataDefineEquipment->AirDistUnit);
            if (AirDistUnitNum == 0) {
                ShowFatalError(state, "ManageZoneAirLoopEquipment: Unit not found=" + ZoneAirLoopEquipName);
            }
            CompIndex = AirDistUnitNum;
        } else {
            AirDistUnitNum = CompIndex;
            if (AirDistUnitNum > state.dataDefineEquipment->NumAirDistUnits || AirDistUnitNum < 1) {
                ShowFatalError(state,
                               format("ManageZoneAirLoopEquipment:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      AirDistUnitNum,
                                      state.dataDefineEquipment->NumAirDistUnits,
                                      ZoneAirLoopEquipName));
            }
            if (ZoneAirLoopEquipName != state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name) {
                ShowFatalError(state,
                               format("ManageZoneAirLoopEquipment: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      AirDistUnitNum,
                                      ZoneAirLoopEquipName,
                                      state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name));
            }
        }
        state.dataSize->CurTermUnitSizingNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).TermUnitSizingNum;
        InitZoneAirLoopEquipment(state, AirDistUnitNum, ControlledZoneNum, ActualZoneNum);
        InitZoneAirLoopEquipmentTimeStep(state, AirDistUnitNum);

        SimZoneAirLoopEquipment(
            state, AirDistUnitNum, SysOutputProvided, NonAirSysOutput, LatOutputProvided, FirstHVACIteration, ControlledZoneNum, ActualZoneNum);

        // Call one-time init to fill termunit sizing and other data for the ADU - can't do this until the actual terminal unit nodes have been
        // matched to zone euqip config nodes
        InitZoneAirLoopEquipment(state, AirDistUnitNum, ControlledZoneNum, ActualZoneNum);
    }

    void GetZoneAirLoopEquipment(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   June 1997
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Get all the system related equipment which may be attached to
        // a zone

        // METHODOLOGY EMPLOYED:
        // Needs description, as appropriate.

        // REFERENCES:
        // na

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;
        using BranchNodeConnections::SetUpCompSets;
        using DualDuct::GetDualDuctOutdoorAirRecircUse;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetZoneAirLoopEquipment: ");            // include trailing blank space
        static std::string const CurrentModuleObject("ZoneHVAC:AirDistributionUnit"); // Object type for getting and error messages

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int AirDistUnitNum;
        int AirDistCompUnitNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input
        bool IsNotOK;            // Flag to verify name
        Array1D_string AlphArray(5);
        Array1D<Real64> NumArray(2);
        Array1D_string cAlphaFields(5);   // Alpha field names
        Array1D_string cNumericFields(2); // Numeric field names
        Array1D_bool lAlphaBlanks(5);     // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks(2);   // Logical array, numeric field input BLANK = .TRUE.
        bool DualDuctRecircIsUsed;        // local temporary for deciding if recirc side used by dual duct terminal

        // make sure the input data is read in only once
        if (!state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag) {
            return;
        } else {
            state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag = false;
        }

        state.dataDefineEquipment->NumAirDistUnits = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

        state.dataDefineEquipment->AirDistUnit.allocate(state.dataDefineEquipment->NumAirDistUnits);
        state.dataZoneAirLoopEquipmentManager->EachOnceFlag.allocate(state.dataDefineEquipment->NumAirDistUnits);
        state.dataZoneAirLoopEquipmentManager->EachOnceFlag = true;

        if (state.dataDefineEquipment->NumAirDistUnits > 0) {

            for (AirDistUnitNum = 1; AirDistUnitNum <= state.dataDefineEquipment->NumAirDistUnits; ++AirDistUnitNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         CurrentModuleObject,
                                                                         AirDistUnitNum,
                                                                         AlphArray,
                                                                         NumAlphas,
                                                                         NumArray,
                                                                         NumNums,
                                                                         IOStat,
                                                                         lNumericBlanks,
                                                                         lAlphaBlanks,
                                                                         cAlphaFields,
                                                                         cNumericFields); //  data for one zone
                UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name = AlphArray(1);
                // Input Outlet Node Num
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).OutletNodeNum = GetOnlySingleNode(state,
                                                                                                         AlphArray(2),
                                                                                                         ErrorsFound,
                                                                                                         CurrentModuleObject,
                                                                                                         AlphArray(1),
                                                                                                         DataLoopNode::NodeFluidType::Air,
                                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                                         NodeInputManager::compFluidStream::Primary,
                                                                                                         ObjectIsParent);
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).InletNodeNum = 0;
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).NumComponents = 1;
                AirDistCompUnitNum = 1;
                // Load the air Distribution Unit Equip and Name
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) = AlphArray(3);
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum) = AlphArray(4);
                ValidateComponent(state, AlphArray(3), AlphArray(4), IsNotOK, CurrentModuleObject);
                if (IsNotOK) {
                    ShowContinueError(state, "In " + CurrentModuleObject + " = " + AlphArray(1));
                    ErrorsFound = true;
                }
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeakFrac = NumArray(1);
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac = NumArray(2);
                if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac <= 0.0) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).LeakLoadMult = 1.0;
                } else if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac < 1.0 &&
                           state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac > 0.0) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).LeakLoadMult =
                        1.0 / (1.0 - state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac);
                } else {
                    ShowSevereError(state,
                                    "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError(state, cNumericFields(2) + " must be less than 1.0");
                    ErrorsFound = true;
                }
                if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeakFrac > 0.0) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak = true;
                } else {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak = false;
                }
                if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac > 0.0) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak = true;
                } else {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak = false;
                }

                // DesignSpecification:AirTerminal:Sizing name
                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex = 0;
                if (!lAlphaBlanks(5)) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex =
                        UtilityRoutines::FindItemInList(AlphArray(5), state.dataSize->AirTerminalSizingSpec);
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).AirTerminalSizingSpecIndex == 0) {
                        ShowSevereError(state, cAlphaFields(5) + " = " + AlphArray(5) + " not found.");
                        ShowContinueError(state,
                                          "Occurs in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ErrorsFound = true;
                    }
                }
                // Validate EquipType for Air Distribution Unit
                if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                "AirTerminal:DualDuct:ConstantVolume")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::DualDuctConstVolume;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:DualDuct:VAV")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::DualDuctVAV;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:DualDuct:VAV:OutdoorAir")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::DualDuctVAVOutdoorAir;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:Reheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:NoReheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolNoReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:Reheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:NoReheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVNoReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVNoReheat;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:SeriesPIU:Reheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ParallelPIU:Reheat")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ConstVol_4PipeInduc;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheatVSFan;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:CooledBeam")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolCooledBeam;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolFourPipeBeam;
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).airTerminalPtr = FourPipeBeam::HVACFourPipeBeam::fourPipeBeamFactory(
                        state, state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(1));
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:UserDefined")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctUserDefined;
                } else if (UtilityRoutines::SameString(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                       "AirTerminal:SingleDuct:Mixer")) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) =
                        DataDefineEquip::iZnAirLoopEquipType::SingleDuctATMixer;
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) {
                        ShowSevereError(
                            state, "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                        ShowContinueError(state,
                                          "Simple duct leakage model not available for " + cAlphaFields(3) + " = " +
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    "Error found in " + CurrentModuleObject + " = " + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError(state,
                                      "Invalid " + cAlphaFields(3) + " = " +
                                          state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum));
                    ErrorsFound = true;
                }

                // Set up component set for air terminal unit
                if ((state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) ==
                     DataDefineEquip::iZnAirLoopEquipType::DualDuctConstVolume) ||
                    (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) ==
                     DataDefineEquip::iZnAirLoopEquipType::DualDuctVAV)) {
                    //  For dual duct units, set up two component sets, one for heat and one for cool
                    SetUpCompSets(state,
                                  CurrentModuleObject,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":HEAT",
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    SetUpCompSets(state,
                                  CurrentModuleObject,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":COOL",
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    //  For dual duct units with decoupled OA and RA, set up two component sets, one for OA (Outdoor Air)
                    //  and one for RA (Recirculated Air)
                } else if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompUnitNum) ==
                           DataDefineEquip::iZnAirLoopEquipType::DualDuctVAVOutdoorAir) {
                    SetUpCompSets(state,
                                  CurrentModuleObject,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":OutdoorAir",
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                    GetDualDuctOutdoorAirRecircUse(state,
                                                   state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                                   state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                                   DualDuctRecircIsUsed);
                    if (DualDuctRecircIsUsed) {
                        SetUpCompSets(state,
                                      CurrentModuleObject,
                                      state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name,
                                      state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum) + ":RecirculatedAir",
                                      state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                      "UNDEFINED",
                                      AlphArray(2));
                    }
                } else {
                    SetUpCompSets(state,
                                  CurrentModuleObject,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name,
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompUnitNum),
                                  state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompUnitNum),
                                  "UNDEFINED",
                                  AlphArray(2));
                }

            } // End of Air Dist Do Loop
            for (AirDistUnitNum = 1; AirDistUnitNum <= state.dataDefineEquipment->NumAirDistUnits; ++AirDistUnitNum) {
                SetupOutputVariable(state,
                                    "Zone Air Terminal Sensible Heating Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).HeatGain,
                                    "System",
                                    "Sum",
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Sensible Cooling Energy",
                                    OutputProcessor::Unit::J,
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).CoolGain,
                                    "System",
                                    "Sum",
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Sensible Heating Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).HeatRate,
                                    "System",
                                    "Average",
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                SetupOutputVariable(state,
                                    "Zone Air Terminal Sensible Cooling Rate",
                                    OutputProcessor::Unit::W,
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).CoolRate,
                                    "System",
                                    "Average",
                                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
            }
        }
        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors found in getting " + CurrentModuleObject + " Input");
        }
    }

    void InitZoneAirLoopEquipment(EnergyPlusData &state, int const AirDistUnitNum, int const ControlledZoneNum, int const ActualZoneNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is left for Module format consistency -- not needed in this module.

        // Do the Begin Simulation initializations
        if (!state.dataZoneAirLoopEquipmentManager->InitAirDistUnitsFlag) {
            return;
        }
        if (state.dataZoneAirLoopEquipmentManager->EachOnceFlag(AirDistUnitNum) &&
            (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).TermUnitSizingNum > 0)) {

            {
                auto &thisADU(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum));
                {
                    auto &thisZoneEqConfig(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum));
                    thisADU.ZoneNum = ActualZoneNum;
                    for (int inletNum = 1; inletNum <= thisZoneEqConfig.NumInletNodes; ++inletNum) {
                        if (thisZoneEqConfig.InletNode(inletNum) == thisADU.OutletNodeNum)
                            thisZoneEqConfig.InletNodeADUNum(inletNum) = AirDistUnitNum;
                    }
                }

                // Fill TermUnitSizing with specs from DesignSpecification:AirTerminal:Sizing
                {
                    auto &thisTermUnitSizingData(state.dataSize->TermUnitSizing(thisADU.TermUnitSizingNum));
                    thisTermUnitSizingData.ADUName = thisADU.Name;
                    if (thisADU.AirTerminalSizingSpecIndex > 0) {
                        {
                            auto const &thisAirTermSizingSpec(state.dataSize->AirTerminalSizingSpec(thisADU.AirTerminalSizingSpecIndex));
                            thisTermUnitSizingData.SpecDesCoolSATRatio = thisAirTermSizingSpec.DesCoolSATRatio;
                            thisTermUnitSizingData.SpecDesHeatSATRatio = thisAirTermSizingSpec.DesHeatSATRatio;
                            thisTermUnitSizingData.SpecDesSensCoolingFrac = thisAirTermSizingSpec.DesSensCoolingFrac;
                            thisTermUnitSizingData.SpecDesSensHeatingFrac = thisAirTermSizingSpec.DesSensHeatingFrac;
                            thisTermUnitSizingData.SpecMinOAFrac = thisAirTermSizingSpec.MinOAFrac;
                        }
                    }
                }
            }

            if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).ZoneNum != 0 &&
                state.dataHeatBal->Zone(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).ZoneNum).HasAdjustedReturnTempByITE) {
                for (int AirDistCompNum = 1; AirDistCompNum <= state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).NumComponents;
                     ++AirDistCompNum) {
                    if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum) !=
                            DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheat &&
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum) !=
                            DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVNoReheat) {
                        ShowSevereError(state,
                                        "The FlowControlWithApproachTemperatures only works with ITE zones with single duct VAV terminal unit.");
                        ShowContinueError(state, "The return air temperature of the ITE will not be overwritten.");
                        ShowFatalError(state, "Preceding condition causes termination.");
                    }
                }
            }
            state.dataZoneAirLoopEquipmentManager->EachOnceFlag(AirDistUnitNum) = false;
            ++state.dataZoneAirLoopEquipmentManager->numADUInitialized;
            if (state.dataZoneAirLoopEquipmentManager->numADUInitialized == state.dataDefineEquipment->NumAirDistUnits) {
                // If all ADUs are initialized, set InitAirDistUnitsFlag to false
                state.dataZoneAirLoopEquipmentManager->InitAirDistUnitsFlag = false;
            }
        }
    }

    void InitZoneAirLoopEquipmentTimeStep(EnergyPlusData &state, int const AirDistUnitNum)
    {
        // every time step
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateZSup = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateSup = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).HeatRate = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).CoolRate = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).HeatGain = 0.0;
        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).CoolGain = 0.0;
    }

    void SimZoneAirLoopEquipment(EnergyPlusData &state,
                                 int const AirDistUnitNum,
                                 Real64 &SysOutputProvided,
                                 Real64 &NonAirSysOutput,
                                 Real64 &LatOutputProvided, // Latent add/removal provided by this unit (kg/s), dehumidify = negative
                                 bool const FirstHVACIteration,
                                 int const ControlledZoneNum,
                                 int const ActualZoneNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   May 1997
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)

        // PURPOSE OF THIS SUBROUTINE:
        // Simulates primary system air supplied to a zone and calculates
        // airflow requirements

        using DualDuct::SimulateDualDuct;
        using HVACCooledBeam::SimCoolBeam;
        using HVACSingleDuctInduc::SimIndUnit;
        using PoweredInductionUnits::SimPIU;
        using Psychrometrics::PsyCpAirFnW;
        using SingleDuct::GetATMixers;
        using SingleDuct::SimulateSingleDuct;
        using UserDefinedComponents::SimAirTerminalUserDefined;

        bool ProvideSysOutput;
        int AirDistCompNum;
        int InNodeNum;                      // air distribution unit inlet node
        int OutNodeNum;                     // air distribution unit outlet node
        int AirLoopNum(0);                  // index of air loop
        Real64 MassFlowRateMaxAvail;        // max avail mass flow rate excluding leaks [kg/s]
        Real64 MassFlowRateMinAvail;        // min avail mass flow rate excluding leaks [kg/s]
        Real64 MassFlowRateUpStreamLeakMax; // max upstream leak flow rate [kg/s]
        Real64 DesFlowRatio(0.0);           // ratio of system to sum of zones design flow rate
        Real64 SpecHumOut(0.0);             // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn(0.0);              // Specific humidity ratio of inlet air (kg moisture / kg moist air)

        ProvideSysOutput = true;
        for (AirDistCompNum = 1; AirDistCompNum <= state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).NumComponents; ++AirDistCompNum) {
            NonAirSysOutput = 0.0;
            InNodeNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).InletNodeNum;
            OutNodeNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).OutletNodeNum;
            MassFlowRateMaxAvail = 0.0;
            MassFlowRateMinAvail = 0.0;
            // check for no plenum
            // set the max and min avail flow rates taking into acount the upstream leak
            if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak) {
                if (InNodeNum > 0) {
                    MassFlowRateMaxAvail = state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail;
                    MassFlowRateMinAvail = state.dataLoopNodes->Node(InNodeNum).MassFlowRateMinAvail;
                    AirLoopNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).AirLoopNum;
                    if (AirLoopNum > 0) {
                        DesFlowRatio = state.dataAirLoop->AirLoopFlow(AirLoopNum).SysToZoneDesFlowRatio;
                    } else {
                        DesFlowRatio = 1.0;
                    }
                    MassFlowRateUpStreamLeakMax = max(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeakFrac *
                                                          state.dataLoopNodes->Node(InNodeNum).MassFlowRateMax * DesFlowRatio,
                                                      0.0);
                    if (MassFlowRateMaxAvail > MassFlowRateUpStreamLeakMax) {
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = MassFlowRateUpStreamLeakMax;
                        state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail = MassFlowRateMaxAvail - MassFlowRateUpStreamLeakMax;
                    } else {
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk = MassFlowRateMaxAvail;
                        state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail = 0.0;
                    }
                    state.dataLoopNodes->Node(InNodeNum).MassFlowRateMinAvail =
                        max(0.0, MassFlowRateMinAvail - state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                }
            }

            {
                auto const SELECT_CASE_var(state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum));

                if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::DualDuctConstVolume) {
                    SimulateDualDuct(state,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::DualDuctVAV) {
                    SimulateDualDuct(state,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::DualDuctVAVOutdoorAir) {
                    SimulateDualDuct(state,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                     FirstHVACIteration,
                                     ActualZoneNum,
                                     state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVNoReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVNoReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolNoReheat) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat) {
                    SimPIU(state,
                           state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                           FirstHVACIteration,
                           ActualZoneNum,
                           state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                           state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat) {
                    SimPIU(state,
                           state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                           FirstHVACIteration,
                           ActualZoneNum,
                           state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                           state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ConstVol_4PipeInduc) {
                    SimIndUnit(state,
                               state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                               FirstHVACIteration,
                               ActualZoneNum,
                               state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                               state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheatVSFan) {
                    SimulateSingleDuct(state,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                       FirstHVACIteration,
                                       ActualZoneNum,
                                       state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                       state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolCooledBeam) {
                    SimCoolBeam(state,
                                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                FirstHVACIteration,
                                ActualZoneNum,
                                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum),
                                NonAirSysOutput);

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolFourPipeBeam) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).airTerminalPtr->simulate(state, FirstHVACIteration, NonAirSysOutput);

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctUserDefined) {
                    SimAirTerminalUserDefined(state,
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipName(AirDistCompNum),
                                              FirstHVACIteration,
                                              ActualZoneNum,
                                              state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode,
                                              state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipIndex(AirDistCompNum));

                } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuctATMixer) {
                    GetATMixers(state); // Needed here if mixer used only with unitarysystem which gets its input late
                    ProvideSysOutput = false;

                } else {
                    ShowSevereError(state,
                                    "Error found in ZoneHVAC:AirDistributionUnit=" + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).Name);
                    ShowContinueError(state, "Invalid Component=" + state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType(AirDistCompNum));
                    ShowFatalError(state, "Preceding condition causes termination.");
                }
            }

            // do leak mass flow calcs
            if (InNodeNum > 0) { // InNodeNum is not always known when this is called, eg FPIU
                InNodeNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).InletNodeNum;
                if (state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak) {
                    state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail = MassFlowRateMaxAvail;
                    state.dataLoopNodes->Node(InNodeNum).MassFlowRateMinAvail = MassFlowRateMinAvail;
                }
                if ((state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).UpStreamLeak ||
                     state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeak) &&
                    MassFlowRateMaxAvail > 0.0) {
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU = state.dataLoopNodes->Node(InNodeNum).MassFlowRate;
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateZSup =
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU *
                        (1.0 - state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac);
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk =
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU *
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).DownStreamLeakFrac;
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateSup =
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU +
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk;
                    state.dataLoopNodes->Node(InNodeNum).MassFlowRate = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateSup;
                    state.dataLoopNodes->Node(OutNodeNum).MassFlowRate = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateZSup;
                    state.dataLoopNodes->Node(OutNodeNum).MassFlowRateMaxAvail =
                        max(0.0,
                            MassFlowRateMaxAvail - state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk -
                                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                    state.dataLoopNodes->Node(OutNodeNum).MassFlowRateMinAvail =
                        max(0.0,
                            MassFlowRateMinAvail - state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateDnStrLk -
                                state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateUpStrLk);
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MaxAvailDelta =
                        MassFlowRateMaxAvail - state.dataLoopNodes->Node(OutNodeNum).MassFlowRateMaxAvail;
                    state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MinAvailDelta =
                        MassFlowRateMinAvail - state.dataLoopNodes->Node(OutNodeNum).MassFlowRateMinAvail;
                } else {
                    // if no leaks, or a terminal unit type not supported for leaks
                    DataDefineEquip::iZnAirLoopEquipType termUnitType =
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).EquipType_Num(AirDistCompNum);
                    if ((termUnitType == DataDefineEquip::iZnAirLoopEquipType::DualDuctConstVolume) ||
                        (termUnitType == DataDefineEquip::iZnAirLoopEquipType::DualDuctVAV) ||
                        (termUnitType == DataDefineEquip::iZnAirLoopEquipType::DualDuctVAVOutdoorAir)) {
                        // Use ADU outlet node flow for dual duct terminal units (which don't support leaks)
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU = state.dataLoopNodes->Node(OutNodeNum).MassFlowRate;
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateZSup = state.dataLoopNodes->Node(OutNodeNum).MassFlowRate;
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateSup = state.dataLoopNodes->Node(OutNodeNum).MassFlowRate;
                    } else {
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateTU = state.dataLoopNodes->Node(InNodeNum).MassFlowRate;
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateZSup = state.dataLoopNodes->Node(InNodeNum).MassFlowRate;
                        state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).MassFlowRateSup = state.dataLoopNodes->Node(InNodeNum).MassFlowRate;
                    }
                }
            }
        }
        if (ProvideSysOutput) {
            int OutletNodeNum = state.dataDefineEquipment->AirDistUnit(AirDistUnitNum).OutletNodeNum;
            int ZoneAirNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
            SpecHumOut = state.dataLoopNodes->Node(OutletNodeNum).HumRat;
            SpecHumIn = state.dataLoopNodes->Node(ZoneAirNode).HumRat;
            // Sign convention: SysOutputProvided <0 Zone is cooled
            //                  SysOutputProvided >0 Zone is heated
            SysOutputProvided = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate *
                                Psychrometrics::PsyDeltaHSenFnTdb2W2Tdb1W1(state.dataLoopNodes->Node(OutletNodeNum).Temp,
                                                                           SpecHumOut,
                                                                           state.dataLoopNodes->Node(ZoneAirNode).Temp,
                                                                           SpecHumIn); // sensible {W};
            // Sign convention: LatOutputProvided <0 Zone is dehumidified
            //                  LatOutputProvided >0 Zone is humidified
            // CR9155 Remove specific humidity calculations
            LatOutputProvided =
                state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate * (SpecHumOut - SpecHumIn); // Latent rate (kg/s), dehumid = negative
        } else {
            SysOutputProvided = 0.0;
            LatOutputProvided = 0.0;
        }
    }

} // namespace ZoneAirLoopEquipmentManager

} // namespace EnergyPlus
