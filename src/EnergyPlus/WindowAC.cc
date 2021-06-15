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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/SystemAirFlowSizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/VariableSpeedCoils.hh>
#include <EnergyPlus/WindowAC.hh>

namespace EnergyPlus {

namespace WindowAC {

    // Module containing the routines dealing window air conditioner units

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2000
    //       MODIFIED       Richard Raustad, FSEC Oct 2003
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To encapsulate the data and algorithms needed to simulate window air
    // conditioner units.

    // METHODOLOGY EMPLOYED:
    // Units are modeled as a collection of components: outside air mixer,
    // fan and DX coil. Control is by means of cycling: either continuous
    // air flow with the DX compressor cycling on/off or the entire unit -
    // fan and compressor cycling on/off. Cycling behavior is not explicitly
    // modeled - instead cycling inefficiencies must be included in the efficiency
    // curves of the DX module.

    using namespace DataLoopNode;
    using namespace DataSizing;
    using DataHVACGlobals::BlowThru;
    using DataHVACGlobals::CoilDX_CoolingHXAssisted;
    using DataHVACGlobals::CoilDX_CoolingSingleSpeed;
    using DataHVACGlobals::ContFanCycCoil;
    using DataHVACGlobals::CycFanCycCoil;
    using DataHVACGlobals::DrawThru;
    using DataHVACGlobals::SingleHeatingSetPoint;
    using DataHVACGlobals::SmallAirVolFlow;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SmallMassFlow;
    using namespace ScheduleManager;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    void SimWindowAC(EnergyPlusData &state,
                     std::string_view CompName,   // name of the window AC unit
                     int const ZoneNum,             // number of zone being served
                     bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                     Real64 &PowerMet,              // Sensible power supplied by window AC (W)
                     Real64 &LatOutputProvided,     // Latent add/removal supplied by window AC (kg/s), dehumid = negative
                     int &CompIndex                 // component index
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manages the simulation of a window AC unit. Called from SimZone Equipment

        int WindACNum;                     // index of window AC unit being simulated
        Real64 QZnReq;                     // zone load (W)
        Real64 RemainingOutputToCoolingSP; // - remaining load to cooling setpoint (W)

        // First time SimWindowAC is called, get the input for all the window AC units
        if (state.dataWindowAC->GetWindowACInputFlag) {
            GetWindowAC(state);
            state.dataWindowAC->GetWindowACInputFlag = false;
        }

        // Find the correct Window AC Equipment
        if (CompIndex == 0) {
            WindACNum = UtilityRoutines::FindItemInList(CompName, state.dataWindowAC->WindAC);
            if (WindACNum == 0) {
                ShowFatalError(state, "SimWindowAC: Unit not found=" + std::string{CompName});
            }
            CompIndex = WindACNum;
        } else {
            WindACNum = CompIndex;
            if (WindACNum > state.dataWindowAC->NumWindAC || WindACNum < 1) {
                ShowFatalError(state,
                               format("SimWindowAC:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      WindACNum,
                                      state.dataWindowAC->NumWindAC,
                                      CompName));
            }
            if (state.dataWindowAC->CheckEquipName(WindACNum)) {
                if (CompName != state.dataWindowAC->WindAC(WindACNum).Name) {
                    ShowFatalError(state,
                                   format("SimWindowAC: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          WindACNum,
                                          CompName,
                                          state.dataWindowAC->WindAC(WindACNum).Name));
                }
                state.dataWindowAC->CheckEquipName(WindACNum) = false;
            }
        }

        RemainingOutputToCoolingSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;

        if (RemainingOutputToCoolingSP < 0.0 && state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) {
            QZnReq = RemainingOutputToCoolingSP;
        } else {
            QZnReq = 0.0;
        }

        state.dataSize->ZoneEqDXCoil = true;
        state.dataSize->ZoneCoolingOnlyFan = true;

        // Initialize the window AC unit
        InitWindowAC(state, WindACNum, QZnReq, ZoneNum, FirstHVACIteration);

        SimCyclingWindowAC(state, WindACNum, ZoneNum, FirstHVACIteration, PowerMet, QZnReq, LatOutputProvided);

        // Report the result of the simulation
        ReportWindowAC(state, WindACNum);

        state.dataSize->ZoneEqDXCoil = false;
        state.dataSize->ZoneCoolingOnlyFan = false;
    }

    void GetWindowAC(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
        //                      Bereket Nigusse, FSEC, April 2011: eliminated input node names,
        //                                                         added OA Mixer object type
        //                                                         and fan object type
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for window AC units and stores it in window AC data structures

        // METHODOLOGY EMPLOYED:
        // Uses "Get" routines to read in data.

        using BranchNodeConnections::SetUpCompSets;
        using Fans::GetFanAvailSchPtr;
        using Fans::GetFanIndex;
        using Fans::GetFanType;
        using Fans::GetFanVolFlow;

        using NodeInputManager::GetOnlySingleNode;
        auto &GetDXCoilOutletNode(DXCoils::GetCoilOutletNode);
        auto &GetDXHXAsstdCoilOutletNode(HVACHXAssistedCoolingCoil::GetCoilOutletNode);
        using DataHVACGlobals::cFanTypes;
        using DataHVACGlobals::FanType_SimpleConstVolume;
        using DataHVACGlobals::FanType_SimpleOnOff;
        using MixedAir::GetOAMixerIndex;
        using MixedAir::GetOAMixerNodeNumbers;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetWindowAC: "); // include trailing blank space

        int WindACIndex; // loop index
        int WindACNum;   // current window AC number
        std::string CompSetFanInlet;
        std::string CompSetCoolInlet;
        std::string CompSetFanOutlet;
        std::string CompSetCoolOutlet;
        int NumAlphas;                   // Number of Alphas for each GetObjectItem call
        int NumNumbers;                  // Number of Numbers for each GetObjectItem call
        Array1D_int OANodeNums(4);       // Node numbers of Outdoor air mixer (OA, EA, RA, MA)
        int IOStatus;                    // Used in GetObjectItem
        bool ErrorsFound(false);         // Set to true if errors in input, fatal at end of routine
        bool errFlag(false);             // Local error flag for GetOAMixerNodeNums
        bool FanErrFlag(false);          // Error flag used in GetFanIndex call
        Real64 FanVolFlow;               // Fan volumetric flow rate
        bool CoilNodeErrFlag;            // Used in error messages for mining coil outlet node number
        std::string CurrentModuleObject; // Object type for getting and error messages
        Array1D_string Alphas;           // Alpha input items for object
        Array1D_string cAlphaFields;     // Alpha field names
        Array1D_string cNumericFields;   // Numeric field names
        Array1D<Real64> Numbers;         // Numeric input items for object
        Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
        Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
        int TotalArgs(0);                // Total number of alpha and numeric arguments (max) for a
        int CtrlZone;                    // index to loop counter
        int NodeNum;                     // index to loop counter
        bool ZoneNodeNotFound;           // used in error checking

        // find the number of each type of window AC unit
        CurrentModuleObject = "ZoneHVAC:WindowAirConditioner";

        state.dataWindowAC->NumWindACCyc = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        state.dataWindowAC->NumWindAC = state.dataWindowAC->NumWindACCyc;
        // allocate the data structures
        state.dataWindowAC->WindAC.allocate(state.dataWindowAC->NumWindAC);
        state.dataWindowAC->CheckEquipName.dimension(state.dataWindowAC->NumWindAC, true);
        state.dataWindowAC->WindACNumericFields.allocate(state.dataWindowAC->NumWindAC);

        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, TotalArgs, NumAlphas, NumNumbers);

        Alphas.allocate(NumAlphas);
        cAlphaFields.allocate(NumAlphas);
        cNumericFields.allocate(NumNumbers);
        Numbers.dimension(NumNumbers, 0.0);
        lAlphaBlanks.dimension(NumAlphas, true);
        lNumericBlanks.dimension(NumNumbers, true);

        // loop over window AC units; get and load the input data
        for (WindACIndex = 1; WindACIndex <= state.dataWindowAC->NumWindACCyc; ++WindACIndex) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     WindACIndex,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            WindACNum = WindACIndex;

            state.dataWindowAC->WindACNumericFields(WindACNum).FieldNames.allocate(NumNumbers);
            state.dataWindowAC->WindACNumericFields(WindACNum).FieldNames = "";
            state.dataWindowAC->WindACNumericFields(WindACNum).FieldNames = cNumericFields;
            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            state.dataWindowAC->WindAC(WindACNum).Name = Alphas(1);
            state.dataWindowAC->WindAC(WindACNum).UnitType = state.dataWindowAC->WindowAC_UnitType; // 'ZoneHVAC:WindowAirConditioner'
            state.dataWindowAC->WindAC(WindACNum).Sched = Alphas(2);
            if (lAlphaBlanks(2)) {
                state.dataWindowAC->WindAC(WindACNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataWindowAC->WindAC(WindACNum).SchedPtr = GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer
                if (state.dataWindowAC->WindAC(WindACNum).SchedPtr == 0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + state.dataWindowAC->WindAC(WindACNum).Name + "\" invalid data.");
                    ShowContinueError(state, "invalid-not found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                    ErrorsFound = true;
                }
            }
            state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow = Numbers(1);
            state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow = Numbers(2);

            state.dataWindowAC->WindAC(WindACNum).AirInNode = GetOnlySingleNode(state,
                                                                                Alphas(3),
                                                                                ErrorsFound,
                                                                                CurrentModuleObject,
                                                                                Alphas(1),
                                                                                DataLoopNode::NodeFluidType::Air,
                                                                                DataLoopNode::NodeConnectionType::Inlet,
                                                                                1,
                                                                                ObjectIsParent);

            state.dataWindowAC->WindAC(WindACNum).AirOutNode = GetOnlySingleNode(state,
                                                                                 Alphas(4),
                                                                                 ErrorsFound,
                                                                                 CurrentModuleObject,
                                                                                 Alphas(1),
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Outlet,
                                                                                 1,
                                                                                 ObjectIsParent);

            state.dataWindowAC->WindAC(WindACNum).OAMixType = Alphas(5);
            state.dataWindowAC->WindAC(WindACNum).OAMixName = Alphas(6);
            // Get outdoor air mixer node numbers
            errFlag = false;
            ValidateComponent(state,
                              state.dataWindowAC->WindAC(WindACNum).OAMixType,
                              state.dataWindowAC->WindAC(WindACNum).OAMixName,
                              errFlag,
                              CurrentModuleObject);
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\".");
                ErrorsFound = true;
            } else {
                // Get outdoor air mixer node numbers
                OANodeNums = GetOAMixerNodeNumbers(state, state.dataWindowAC->WindAC(WindACNum).OAMixName, errFlag);
                if (errFlag) {
                    ShowContinueError(state,
                                      "that was specified in " + CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\"");
                    ShowContinueError(state, "..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.");
                    ErrorsFound = true;
                } else {
                    state.dataWindowAC->WindAC(WindACNum).OutsideAirNode = OANodeNums(1);
                    state.dataWindowAC->WindAC(WindACNum).AirReliefNode = OANodeNums(2);
                    state.dataWindowAC->WindAC(WindACNum).MixedAirNode = OANodeNums(4);
                }
            }

            state.dataWindowAC->WindAC(WindACNum).FanType = Alphas(7);
            state.dataWindowAC->WindAC(WindACNum).FanName = Alphas(8);

            FanErrFlag = false;
            ValidateComponent(
                state, state.dataWindowAC->WindAC(WindACNum).FanType, state.dataWindowAC->WindAC(WindACNum).FanName, FanErrFlag, CurrentModuleObject);
            if (FanErrFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\".");
                ErrorsFound = true;
            } else {
                if (UtilityRoutines::SameString(state.dataWindowAC->WindAC(WindACNum).FanType, "Fan:SystemModel")) {
                    state.dataWindowAC->WindAC(WindACNum).FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                    state.dataHVACFan->fanObjs.emplace_back(
                        new HVACFan::FanSystem(state, state.dataWindowAC->WindAC(WindACNum).FanName)); // call constructor
                    state.dataWindowAC->WindAC(WindACNum).FanIndex =
                        HVACFan::getFanObjectVectorIndex(state, state.dataWindowAC->WindAC(WindACNum).FanName);
                    FanVolFlow = state.dataHVACFan->fanObjs[state.dataWindowAC->WindAC(WindACNum).FanIndex]->designAirVolFlowRate;
                    if (FanVolFlow != AutoSize) {
                        if (FanVolFlow < state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow) {
                            ShowWarningError(state,
                                             format("Air flow rate = {:.7T} in fan object {} is less than the maximum supply air flow rate ({:.7T}) "
                                                    "in the {} object.",
                                                    FanVolFlow,
                                                    state.dataWindowAC->WindAC(WindACNum).FanName,
                                                    state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow,
                                                    CurrentModuleObject));
                            ShowContinueError(
                                state, " The fan flow rate must be >= to the " + cNumericFields(1) + " in the " + CurrentModuleObject + " object.");
                            ShowContinueError(state, " Occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                            ErrorsFound = true;
                        }
                    }
                    state.dataWindowAC->WindAC(WindACNum).FanAvailSchedPtr =
                        state.dataHVACFan->fanObjs[state.dataWindowAC->WindAC(WindACNum).FanIndex]->availSchedIndex;
                } else {

                    GetFanType(state,
                               state.dataWindowAC->WindAC(WindACNum).FanName,
                               state.dataWindowAC->WindAC(WindACNum).FanType_Num,
                               FanErrFlag,
                               CurrentModuleObject,
                               state.dataWindowAC->WindAC(WindACNum).Name);
                    {
                        auto const SELECT_CASE_var(state.dataWindowAC->WindAC(WindACNum).FanType_Num);
                        if ((SELECT_CASE_var == FanType_SimpleOnOff) || (SELECT_CASE_var == FanType_SimpleConstVolume)) {
                            GetFanIndex(state,
                                        state.dataWindowAC->WindAC(WindACNum).FanName,
                                        state.dataWindowAC->WindAC(WindACNum).FanIndex,
                                        FanErrFlag,
                                        CurrentModuleObject);
                            if (FanErrFlag) {
                                ShowContinueError(
                                    state, " specified in " + CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\".");
                                ErrorsFound = true;
                            } else {
                                GetFanVolFlow(state, state.dataWindowAC->WindAC(WindACNum).FanIndex, FanVolFlow);
                                if (FanVolFlow != AutoSize) {
                                    if (FanVolFlow < state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow) {
                                        ShowWarningError(state,
                                                         format("Air flow rate = {:.7T} in fan object {} is less than the maximum supply air flow "
                                                                "rate ({:.7T}) in the {} object.",
                                                                FanVolFlow,
                                                                state.dataWindowAC->WindAC(WindACNum).FanName,
                                                                state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow,
                                                                CurrentModuleObject));
                                        ShowContinueError(state,
                                                          " The fan flow rate must be >= to the " + cNumericFields(1) + " in the " +
                                                              CurrentModuleObject + " object.");
                                        ShowContinueError(state,
                                                          " Occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                                        ErrorsFound = true;
                                    }
                                }
                            }
                        } else {
                            ShowSevereError(state, CurrentModuleObject + " = \"" + Alphas(1) + "\".");
                            ShowContinueError(state, "Fan Type must be Fan:OnOff, or Fan:ConstantVolume.");
                            ErrorsFound = true;
                        }
                    }
                    // Get the fan's availability schedule
                    state.dataWindowAC->WindAC(WindACNum).FanAvailSchedPtr = GetFanAvailSchPtr(
                        state, state.dataWindowAC->WindAC(WindACNum).FanType, state.dataWindowAC->WindAC(WindACNum).FanName, FanErrFlag);
                }
                if (FanErrFlag) {
                    ShowContinueError(state, "...occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                    ErrorsFound = true;
                }
            }

            state.dataWindowAC->WindAC(WindACNum).DXCoilName = Alphas(10);

            if (UtilityRoutines::SameString(Alphas(9), "Coil:Cooling:DX:SingleSpeed") ||
                UtilityRoutines::SameString(Alphas(9), "CoilSystem:Cooling:DX:HeatExchangerAssisted") ||
                UtilityRoutines::SameString(Alphas(9), "Coil:Cooling:DX:VariableSpeed")) {
                state.dataWindowAC->WindAC(WindACNum).DXCoilType = Alphas(9);
                CoilNodeErrFlag = false;
                if (UtilityRoutines::SameString(Alphas(9), "Coil:Cooling:DX:SingleSpeed")) {
                    state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num = CoilDX_CoolingSingleSpeed;
                    state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum = GetDXCoilOutletNode(
                        state, state.dataWindowAC->WindAC(WindACNum).DXCoilType, state.dataWindowAC->WindAC(WindACNum).DXCoilName, CoilNodeErrFlag);
                } else if (UtilityRoutines::SameString(Alphas(9), "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                    state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num = CoilDX_CoolingHXAssisted;
                    state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum = GetDXHXAsstdCoilOutletNode(
                        state, state.dataWindowAC->WindAC(WindACNum).DXCoilType, state.dataWindowAC->WindAC(WindACNum).DXCoilName, CoilNodeErrFlag);
                } else if (UtilityRoutines::SameString(Alphas(9), "Coil:Cooling:DX:VariableSpeed")) {
                    state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                    state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(
                        state, state.dataWindowAC->WindAC(WindACNum).DXCoilType, state.dataWindowAC->WindAC(WindACNum).DXCoilName, CoilNodeErrFlag);
                    state.dataWindowAC->WindAC(WindACNum).DXCoilNumOfSpeeds =
                        VariableSpeedCoils::GetVSCoilNumOfSpeeds(state, state.dataWindowAC->WindAC(WindACNum).DXCoilName, ErrorsFound);
                }
                if (CoilNodeErrFlag) {
                    ShowContinueError(state,
                                      " that was specified in " + CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\".");
                    ErrorsFound = true;
                }
            } else {
                ShowWarningError(state, "Invalid " + cAlphaFields(9) + " = " + Alphas(9));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                ErrorsFound = true;
            }

            state.dataWindowAC->WindAC(WindACNum).FanSchedPtr = GetScheduleIndex(state, Alphas(11));

            // Default to cycling fan when fan mode schedule is not present
            if (!lAlphaBlanks(11) && state.dataWindowAC->WindAC(WindACNum).FanSchedPtr == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + " \"" + state.dataWindowAC->WindAC(WindACNum).Name + "\" " + cAlphaFields(11) +
                                    " not found: " + Alphas(11));
                ErrorsFound = true;
            } else if (lAlphaBlanks(11)) {
                state.dataWindowAC->WindAC(WindACNum).OpMode = CycFanCycCoil;
            }

            if (UtilityRoutines::SameString(Alphas(12), "BlowThrough")) state.dataWindowAC->WindAC(WindACNum).FanPlace = BlowThru;
            if (UtilityRoutines::SameString(Alphas(12), "DrawThrough")) state.dataWindowAC->WindAC(WindACNum).FanPlace = DrawThru;
            if (state.dataWindowAC->WindAC(WindACNum).FanPlace == 0) {
                ShowSevereError(state, "Invalid " + cAlphaFields(12) + " = " + Alphas(12));
                ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                ErrorsFound = true;
            }

            state.dataWindowAC->WindAC(WindACNum).ConvergenceTol = Numbers(3);

            if (!lAlphaBlanks(13)) {
                state.dataWindowAC->WindAC(WindACNum).AvailManagerListName = Alphas(13);
            }

            state.dataWindowAC->WindAC(WindACNum).HVACSizingIndex = 0;
            if (!lAlphaBlanks(14)) {
                state.dataWindowAC->WindAC(WindACNum).HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(14), state.dataSize->ZoneHVACSizing);
                if (state.dataWindowAC->WindAC(WindACNum).HVACSizingIndex == 0) {
                    ShowSevereError(state, cAlphaFields(14) + " = " + Alphas(14) + " not found.");
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + state.dataWindowAC->WindAC(WindACNum).Name);
                    ErrorsFound = true;
                }
            }

            // Add fan to component sets array
            if (state.dataWindowAC->WindAC(WindACNum).FanPlace == BlowThru) {

                // Window AC air inlet node must be the same as a zone exhaust node and the OA Mixer return node
                // check that Window AC air inlet node is the same as a zone exhaust node.
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                        if (state.dataWindowAC->WindAC(WindACNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                        "\". Window AC air inlet node name must be the same as a zone exhaust node name.");
                    ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(
                        state, "..Window AC air inlet node name = " + state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirInNode));
                    ErrorsFound = true;
                }
                // check that Window AC air outlet node is a zone inlet node.
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                        if (state.dataWindowAC->WindAC(WindACNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                            state.dataWindowAC->WindAC(WindACNum).ZonePtr = CtrlZone;
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                        "\". Window AC air outlet node name must be the same as a zone inlet node name.");
                    ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(
                        state, "..Window AC air outlet node name = " + state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirOutNode));
                    ErrorsFound = true;
                }
                CompSetFanInlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).MixedAirNode);
                CompSetFanOutlet = "UNDEFINED";
                CompSetCoolInlet = "UNDEFINED";
                CompSetCoolOutlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirOutNode);
            } else { // draw through fan from IF (WindAC(WindACNum)%FanPlace == BlowThru) THEN
                // check that Window AC air inlet node is the same as a zone exhaust node.
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                        if (state.dataWindowAC->WindAC(WindACNum).AirInNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                        "\"."
                                        " Window AC air inlet node name must be the same as a zone exhaust node name.");
                    ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(
                        state, "..Window AC inlet node name = " + state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirInNode));
                    ErrorsFound = true;
                }
                // check that Window AC air outlet node is the same as a zone inlet node.
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                        if (state.dataWindowAC->WindAC(WindACNum).AirOutNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                            state.dataWindowAC->WindAC(WindACNum).ZonePtr = CtrlZone;
                            ZoneNodeNotFound = false;
                            break;
                        }
                    }
                }
                if (ZoneNodeNotFound) {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                        "\". Window AC air outlet node name must be the same as a zone inlet node name.");
                    ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(
                        state, "..Window AC outlet node name = " + state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirOutNode));
                    ErrorsFound = true;
                }
                CompSetFanInlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum);
                CompSetFanOutlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).AirOutNode);
                CompSetCoolInlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).MixedAirNode);
                CompSetCoolOutlet = state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum);
            }
            // Add fan to component sets array
            SetUpCompSets(state,
                          state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(WindACNum).UnitType),
                          state.dataWindowAC->WindAC(WindACNum).Name,
                          state.dataWindowAC->WindAC(WindACNum).FanType,
                          state.dataWindowAC->WindAC(WindACNum).FanName,
                          CompSetFanInlet,
                          CompSetFanOutlet);

            // Add cooling coil to component sets array
            SetUpCompSets(state,
                          state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(WindACNum).UnitType),
                          state.dataWindowAC->WindAC(WindACNum).Name,
                          state.dataWindowAC->WindAC(WindACNum).DXCoilType,
                          state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                          CompSetCoolInlet,
                          CompSetCoolOutlet);

            // Set up component set for OA mixer - use OA node and Mixed air node
            SetUpCompSets(state,
                          state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(WindACNum).UnitType),
                          state.dataWindowAC->WindAC(WindACNum).Name,
                          state.dataWindowAC->WindAC(WindACNum).OAMixType,
                          state.dataWindowAC->WindAC(WindACNum).OAMixName,
                          state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).OutsideAirNode),
                          state.dataLoopNodes->NodeID(state.dataWindowAC->WindAC(WindACNum).MixedAirNode));
        }

        Alphas.deallocate();
        cAlphaFields.deallocate();
        cNumericFields.deallocate();
        Numbers.deallocate();
        lAlphaBlanks.deallocate();
        lNumericBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state,
                           std::string{RoutineName} + "Errors found in getting " + CurrentModuleObject + " input.  Preceding condition causes termination.");
        }

        for (WindACNum = 1; WindACNum <= state.dataWindowAC->NumWindAC; ++WindACNum) {
            // Setup Report variables for the Fan Coils
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Total Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataWindowAC->WindAC(WindACNum).TotCoolEnergyRate,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Total Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWindowAC->WindAC(WindACNum).TotCoolEnergy,
                                "System",
                                "Sum",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Sensible Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Sensible Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWindowAC->WindAC(WindACNum).SensCoolEnergy,
                                "System",
                                "Sum",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Latent Cooling Rate",
                                OutputProcessor::Unit::W,
                                state.dataWindowAC->WindAC(WindACNum).LatCoolEnergyRate,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Latent Cooling Energy",
                                OutputProcessor::Unit::J,
                                state.dataWindowAC->WindAC(WindACNum).LatCoolEnergy,
                                "System",
                                "Sum",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Electricity Rate",
                                OutputProcessor::Unit::W,
                                state.dataWindowAC->WindAC(WindACNum).ElecPower,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Electricity Energy",
                                OutputProcessor::Unit::J,
                                state.dataWindowAC->WindAC(WindACNum).ElecConsumption,
                                "System",
                                "Sum",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Fan Part Load Ratio",
                                OutputProcessor::Unit::None,
                                state.dataWindowAC->WindAC(WindACNum).FanPartLoadRatio,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Compressor Part Load Ratio",
                                OutputProcessor::Unit::None,
                                state.dataWindowAC->WindAC(WindACNum).CompPartLoadRatio,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            SetupOutputVariable(state,
                                "Zone Window Air Conditioner Fan Availability Status",
                                OutputProcessor::Unit::None,
                                state.dataWindowAC->WindAC(WindACNum).AvailStatus,
                                "System",
                                "Average",
                                state.dataWindowAC->WindAC(WindACNum).Name);
            if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                SetupEMSActuator(state,
                                 "Window Air Conditioner",
                                 state.dataWindowAC->WindAC(WindACNum).Name,
                                 "Part Load Ratio",
                                 "[fraction]",
                                 state.dataWindowAC->WindAC(WindACNum).EMSOverridePartLoadFrac,
                                 state.dataWindowAC->WindAC(WindACNum).EMSValueForPartLoadFrac);
            }
        }
        for (WindACNum = 1; WindACNum <= state.dataWindowAC->NumWindAC; ++WindACNum) {
            if (state.dataWindowAC->WindAC(WindACNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                                                                                         state.dataWindowAC->WindAC(WindACNum).DXCoilType,
                                                                                         state.dataWindowAC->WindAC(WindACNum).FanName,
                                                                                         DataAirSystems::objectVectorOOFanSystemModel,
                                                                                         state.dataWindowAC->WindAC(WindACNum).FanIndex);
            } else {
                state.dataRptCoilSelection->coilSelectionReportObj->setCoilSupplyFanInfo(state,
                                                                                         state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                                                                                         state.dataWindowAC->WindAC(WindACNum).DXCoilType,
                                                                                         state.dataWindowAC->WindAC(WindACNum).FanName,
                                                                                         DataAirSystems::structArrayLegacyFanModels,
                                                                                         state.dataWindowAC->WindAC(WindACNum).FanIndex);
            }
        }
    }

    void InitWindowAC(EnergyPlusData &state,
                      int const WindACNum,          // number of the current window AC unit being simulated
                      Real64 &QZnReq,               // zone load (modified as needed) (W)
                      int const ZoneNum,            // index to zone
                      bool const FirstHVACIteration // TRUE when first HVAC iteration
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Window AC Components.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        using DataHVACGlobals::SmallLoad;
        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;
        using DataZoneEquipment::CheckZoneEquipmentList;
        using DataZoneEquipment::WindowAC_Num;

        int InNode;          // inlet node number in window AC loop
        int OutNode;         // outlet node number in window AC loop
        int InletNode;       // inlet node number for window AC WindACNum
        int OutsideAirNode;  // outside air node number in window AC loop
        int AirRelNode;      // relief air node number in window AC loop
        Real64 RhoAir;       // air density at InNode
        int Loop;            // loop counter
        Real64 QToCoolSetPt; // sensible load to cooling setpoint (W)
        Real64 NoCompOutput; // sensible load delivered with compressor off (W)

        // Do the one time initializations
        if (state.dataWindowAC->MyOneTimeFlag) {

            state.dataWindowAC->MyEnvrnFlag.allocate(state.dataWindowAC->NumWindAC);
            state.dataWindowAC->MySizeFlag.allocate(state.dataWindowAC->NumWindAC);
            state.dataWindowAC->MyZoneEqFlag.allocate(state.dataWindowAC->NumWindAC);
            state.dataWindowAC->MyEnvrnFlag = true;
            state.dataWindowAC->MySizeFlag = true;
            state.dataWindowAC->MyZoneEqFlag = true;
            state.dataWindowAC->MyOneTimeFlag = false;
        }

        if (allocated(ZoneComp)) {
            if (state.dataWindowAC->MyZoneEqFlag(WindACNum)) { // initialize the name of each availability manager list and zone number
                ZoneComp(WindowAC_Num).ZoneCompAvailMgrs(WindACNum).AvailManagerListName = state.dataWindowAC->WindAC(WindACNum).AvailManagerListName;
                ZoneComp(WindowAC_Num).ZoneCompAvailMgrs(WindACNum).ZoneNum = ZoneNum;
                state.dataWindowAC->MyZoneEqFlag(WindACNum) = false;
            }
            state.dataWindowAC->WindAC(WindACNum).AvailStatus = ZoneComp(WindowAC_Num).ZoneCompAvailMgrs(WindACNum).AvailStatus;
        }

        // need to check all Window AC units to see if they are on Zone Equipment List or issue warning
        if (!state.dataWindowAC->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataWindowAC->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataWindowAC->NumWindAC; ++Loop) {
                if (CheckZoneEquipmentList(state,
                                           state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(Loop).UnitType),
                                           state.dataWindowAC->WindAC(Loop).Name))
                    continue;
                ShowSevereError(state,
                                "InitWindowAC: Window AC Unit=[" +
                                    state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(Loop).UnitType) + ',' +
                                    state.dataWindowAC->WindAC(Loop).Name + "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataWindowAC->MySizeFlag(WindACNum)) {

            SizeWindowAC(state, WindACNum);

            state.dataWindowAC->MySizeFlag(WindACNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && state.dataWindowAC->MyEnvrnFlag(WindACNum)) {
            InNode = state.dataWindowAC->WindAC(WindACNum).AirInNode;
            OutNode = state.dataWindowAC->WindAC(WindACNum).AirOutNode;
            OutsideAirNode = state.dataWindowAC->WindAC(WindACNum).OutsideAirNode;
            RhoAir = state.dataEnvrn->StdRhoAir;
            // set the mass flow rates from the input volume flow rates
            state.dataWindowAC->WindAC(WindACNum).MaxAirMassFlow = RhoAir * state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow;
            state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow = RhoAir * state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow;
            // set the node max and min mass flow rates
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax = state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMax = state.dataWindowAC->WindAC(WindACNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(OutNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(InNode).MassFlowRateMax = state.dataWindowAC->WindAC(WindACNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InNode).MassFlowRateMin = 0.0;
            state.dataWindowAC->MyEnvrnFlag(WindACNum) = false;
        } // end one time inits

        if (!state.dataGlobal->BeginEnvrnFlag) {
            state.dataWindowAC->MyEnvrnFlag(WindACNum) = true;
        }

        if (state.dataWindowAC->WindAC(WindACNum).FanSchedPtr > 0) {
            if (GetCurrentScheduleValue(state, state.dataWindowAC->WindAC(WindACNum).FanSchedPtr) == 0.0) {
                state.dataWindowAC->WindAC(WindACNum).OpMode = CycFanCycCoil;
            } else {
                state.dataWindowAC->WindAC(WindACNum).OpMode = ContFanCycCoil;
            }
        }

        // These initializations are done every iteration
        InletNode = state.dataWindowAC->WindAC(WindACNum).AirInNode;
        OutsideAirNode = state.dataWindowAC->WindAC(WindACNum).OutsideAirNode;
        AirRelNode = state.dataWindowAC->WindAC(WindACNum).AirReliefNode;
        // Set the inlet node mass flow rate
        if (GetCurrentScheduleValue(state, state.dataWindowAC->WindAC(WindACNum).SchedPtr) <= 0.0 ||
            (GetCurrentScheduleValue(state, state.dataWindowAC->WindAC(WindACNum).FanAvailSchedPtr) <= 0.0 && !ZoneCompTurnFansOn) ||
            ZoneCompTurnFansOff) {
            state.dataWindowAC->WindAC(WindACNum).PartLoadFrac = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = 0.0;
        } else {
            state.dataWindowAC->WindAC(WindACNum).PartLoadFrac = 1.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataWindowAC->WindAC(WindACNum).MaxAirMassFlow;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(InletNode).MassFlowRate;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate = state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMaxAvail = state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow;
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMaxAvail = state.dataWindowAC->WindAC(WindACNum).OutAirMassFlow;
            state.dataLoopNodes->Node(AirRelNode).MassFlowRateMinAvail = 0.0;
        }

        // Original thermostat control logic (works only for cycling fan systems)
        if (QZnReq < (-1.0 * SmallLoad) && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) &&
            state.dataWindowAC->WindAC(WindACNum).PartLoadFrac > 0.0) {
            state.dataWindowAC->CoolingLoad = true;
        } else {
            state.dataWindowAC->CoolingLoad = false;
        }

        // Constant fan systems are tested for ventilation load to determine if load to be met changes.
        if (state.dataWindowAC->WindAC(WindACNum).OpMode == ContFanCycCoil && state.dataWindowAC->WindAC(WindACNum).PartLoadFrac > 0.0 &&
            (GetCurrentScheduleValue(state, state.dataWindowAC->WindAC(WindACNum).FanAvailSchedPtr) > 0.0 || ZoneCompTurnFansOn) &&
            !ZoneCompTurnFansOn) {

            CalcWindowACOutput(state, WindACNum, FirstHVACIteration, state.dataWindowAC->WindAC(WindACNum).OpMode, 0.0, false, NoCompOutput);

            QToCoolSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;

            // If the unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
            if (NoCompOutput > (-1.0 * SmallLoad) && QToCoolSetPt > (-1.0 * SmallLoad) && state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
                if (NoCompOutput > QToCoolSetPt) {
                    QZnReq = QToCoolSetPt;
                    state.dataWindowAC->CoolingLoad = true;
                }
            }
        }
    }

    void SizeWindowAC(EnergyPlusData &state, int const WindACNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   January 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      July 2014, B. Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing Window AC  Unit components for which flow rates have not been
        // specified in the input.

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone or system sizing arrays

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::CoolingCapacitySizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeWindowAC: "); // include trailing blank space

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 MaxAirVolFlowDes;  // Autosized maximum air flow for reporting
        Real64 MaxAirVolFlowUser; // Hardsized maximum air flow for reporting
        Real64 OutAirVolFlowDes;  // Autosized outdoor air flow for reporting
        Real64 OutAirVolFlowUser; // Hardsized outdoor ari flow for reporting
        bool IsAutoSize;          // Indicator to autosize
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

        IsAutoSize = false;
        MaxAirVolFlowDes = 0.0;
        MaxAirVolFlowUser = 0.0;
        OutAirVolFlowDes = 0.0;
        OutAirVolFlowUser = 0.0;
        state.dataSize->DataFracOfAutosizedCoolingAirflow = 1.0;
        state.dataSize->DataFracOfAutosizedHeatingAirflow = 1.0;
        state.dataSize->DataFracOfAutosizedCoolingCapacity = 1.0;
        state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
        state.dataSize->DataScalableSizingON = false;
        state.dataSize->ZoneHeatingOnlyFan = false;
        state.dataSize->ZoneCoolingOnlyFan = true;
        state.dataSize->DataScalableCapSizingON = false;
        CompType = "ZoneHVAC:WindowAirConditioner";
        CompName = state.dataWindowAC->WindAC(WindACNum).Name;
        state.dataSize->DataZoneNumber = state.dataWindowAC->WindAC(WindACNum).ZonePtr;
        if (state.dataWindowAC->WindAC(WindACNum).FanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataSize->DataFanEnumType = DataAirSystems::objectVectorOOFanSystemModel;
        } else {
            state.dataSize->DataFanEnumType = DataAirSystems::structArrayLegacyFanModels;
        }
        state.dataSize->DataFanIndex = state.dataWindowAC->WindAC(WindACNum).FanIndex;
        if (state.dataWindowAC->WindAC(WindACNum).FanPlace == BlowThru) {
            state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneBlowThru;
        } else if (state.dataWindowAC->WindAC(WindACNum).FanPlace == DrawThru) {
            state.dataSize->DataFanPlacement = DataSizing::zoneFanPlacement::zoneDrawThru;
        }

        if (state.dataSize->CurZoneEqNum > 0) {
            if (state.dataWindowAC->WindAC(WindACNum).HVACSizingIndex > 0) {
                zoneHVACIndex = state.dataWindowAC->WindAC(WindACNum).HVACSizingIndex;
                // N1 , \field Maximum Supply Air Flow Rate
                SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
                PrintFlag = true;
                SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                if (SAFMethod == None || SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea ||
                    SAFMethod == FractionOfAutosizedCoolingAirflow) {
                    if (SAFMethod == SupplyAirFlowRate) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                        }
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    } else if (SAFMethod == FlowPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                        state.dataSize->DataScalableSizingON = true;
                    } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                        state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                    } else {
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    }
                    bool errorsFound = false;
                    CoolingAirFlowSizer sizingCoolingAirFlow;
                    std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                    if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                    sizingCoolingAirFlow.overrideSizingString(stringOverride);
                    // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow = sizingCoolingAirFlow.size(state, TempSize, errorsFound);

                } else if (SAFMethod == FlowPerCoolingCapacity) {
                    SizingMethod = CoolingCapacitySizing;
                    TempSize = AutoSize;
                    PrintFlag = false;
                    state.dataSize->DataScalableSizingON = true;
                    state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow;
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod == FractionOfAutosizedCoolingCapacity) {
                        state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                    }
                    bool errorsFound = false;
                    CoolingCapacitySizer sizerCoolingCapacity;
                    sizerCoolingCapacity.overrideSizingString(SizingString);
                    sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    state.dataSize->DataCapacityUsedForSizing = sizerCoolingCapacity.size(state, TempSize, errorsFound);
                    state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    PrintFlag = true;
                    TempSize = AutoSize;
                    errorsFound = false;
                    CoolingAirFlowSizer sizingCoolingAirFlow;
                    std::string stringOverride = "Maximum Supply Air Flow Rate [m3/s]";
                    if (state.dataGlobal->isEpJSON) stringOverride = "maximum_supply_air_flow_rate [m3/s]";
                    sizingCoolingAirFlow.overrideSizingString(stringOverride);
                    // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
                }
                // DataScalableSizingON = false;

                // initialize capacity sizing variables: cooling
                CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                    CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                    if (CapSizingMethod == HeatingDesignCapacity) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                        }
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity *
                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        state.dataSize->DataScalableCapSizingON = true;
                    } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                        state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                        state.dataSize->DataScalableCapSizingON = true;
                    }
                }
            } else {
                // no scalble sizing method has been specified. Sizing proceeds using the method
                // specified in the zoneHVAC object
                // N1 , \field Maximum Supply Air Flow Rate
                FieldNum = 1;
                PrintFlag = true;
                SizingString = state.dataWindowAC->WindACNumericFields(WindACNum).FieldNames(FieldNum) + " [m3/s]";
                TempSize = state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow;
                bool errorsFound = false;
                SystemAirFlowSizer sizerSystemAirFlow;
                sizerSystemAirFlow.overrideSizingString(SizingString);
                // sizerSystemAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizerSystemAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow = sizerSystemAirFlow.size(state, TempSize, errorsFound);
            }
        }

        if (state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow == AutoSize) {

            if (state.dataSize->CurZoneEqNum > 0) {

                CheckZoneSizing(state,
                                state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(WindACNum).UnitType),
                                state.dataWindowAC->WindAC(WindACNum).Name);
                state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow =
                    min(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA, state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow);
                if (state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow < SmallAirVolFlow) {
                    state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow = 0.0;
                }
                BaseSizer::reportSizerOutput(state,
                                             state.dataWindowAC->cWindowAC_UnitTypes(state.dataWindowAC->WindAC(WindACNum).UnitType),
                                             state.dataWindowAC->WindAC(WindACNum).Name,
                                             "Maximum Outdoor Air Flow Rate [m3/s]",
                                             state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow);
            }
        }

        if (state.dataSize->CurZoneEqNum > 0) {
            ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataWindowAC->WindAC(WindACNum).OutAirVolFlow;
            ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataWindowAC->WindAC(WindACNum).MaxAirVolFlow;
        }

        state.dataSize->DataScalableCapSizingON = false;
    }

    void SimCyclingWindowAC(EnergyPlusData &state,
                            int const WindACNum,                // number of the current window AC unit being simulated
                            [[maybe_unused]] int const ZoneNum, // number of zone being served !unused1208
                            bool const FirstHVACIteration,      // TRUE if 1st HVAC simulation of system timestep
                            Real64 &PowerMet,                   // Sensible power supplied (W)
                            Real64 const QZnReq,                // Sensible load to be met (W)
                            Real64 &LatOutputProvided           // Latent power supplied (kg/s), negative = dehumidification
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Buhl/Shirey Mar 2001, Shirey Aug 2009 (LatOutputProvided)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate a cycling window air conditioner unit; adjust its output to match the
        // remaining zone load.

        // METHODOLOGY EMPLOYED:
        // If unit is on, calls ControlWindACOutput to obtain the desired unit output

        Real64 PartLoadFrac; // unit part load fraction
        Real64 QUnitOut;     // Dry air sens. cooling provided by AC unit [watts]
        Real64 SensCoolOut;  // Moist air sensible cooling rate [W]
        Real64 LatentOutput; // Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
        bool UnitOn;         // TRUE if unit is on
        bool CoilOn;         // TRUE if coil is on
        int OutletNode;      // unit air outlet node
        int InletNode;       // unit air inlet node
        Real64 QTotUnitOut;  // total unit output [watts]
        Real64 AirMassFlow;  // air mass flow rate [kg/sec]
        Real64 CpAir;        // inlet air specific heat [J/kg-C]
        Real64 Test;
        int OpMode;        // operating mode (fan cycling or continious; DX coil always cycles)
        Real64 MinHumRat;  // minimum of inlet & outlet humidity ratio
        bool HXUnitOn;     // Used to control HX heat recovery as needed
        Real64 SpecHumOut; // Specific humidity ratio of outlet air (kg moisture / kg moist air)
        Real64 SpecHumIn;  // Specific humidity ratio of inlet air (kg moisture / kg moist air)

        // zero the DX coil electricity consumption

        state.dataHVACGlobal->DXElecCoolingPower = 0.0;
        // initialize local variables
        UnitOn = true;
        CoilOn = true;
        QUnitOut = 0.0;
        LatentOutput = 0.0;
        OutletNode = state.dataWindowAC->WindAC(WindACNum).AirOutNode;
        InletNode = state.dataWindowAC->WindAC(WindACNum).AirInNode;
        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        Test = AirMassFlow;
        CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(InletNode).HumRat);
        OpMode = state.dataWindowAC->WindAC(WindACNum).OpMode;

        // set the on/off flags
        if (state.dataWindowAC->WindAC(WindACNum).OpMode == CycFanCycCoil) {
            // cycling unit: only runs if there is a load.
            if (!state.dataWindowAC->CoolingLoad || AirMassFlow < SmallMassFlow) {
                UnitOn = false;
                CoilOn = false;
            }
        } else if (state.dataWindowAC->WindAC(WindACNum).OpMode == ContFanCycCoil) {
            // continuous unit: fan runs if scheduled on; coil runs only if cooling load
            if (AirMassFlow < SmallMassFlow) {
                UnitOn = false;
                CoilOn = false;
            } else if (!state.dataWindowAC->CoolingLoad) {
                CoilOn = false;
            }
        }

        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;

        if (UnitOn && CoilOn) {
            HXUnitOn = false;
            ControlCycWindACOutput(state, WindACNum, FirstHVACIteration, OpMode, QZnReq, PartLoadFrac, HXUnitOn);
        } else {
            PartLoadFrac = 0.0;
            HXUnitOn = false;
        }

        state.dataWindowAC->WindAC(WindACNum).PartLoadFrac = PartLoadFrac;

        CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, QUnitOut);

        // Reseting AirMassFlow to inlet node mass flow rate since inlet mass flow rate may be getting
        // manipulated in subroutine CalcWindowACOutput

        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        MinHumRat = min(state.dataLoopNodes->Node(InletNode).HumRat, state.dataLoopNodes->Node(OutletNode).HumRat);
        QUnitOut = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                  PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));

        SensCoolOut = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                     PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));

        // CR9155 Remove specific humidity calculations
        SpecHumOut = state.dataLoopNodes->Node(OutletNode).HumRat;
        SpecHumIn = state.dataLoopNodes->Node(InletNode).HumRat;
        LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn); // Latent rate, kg/s

        QTotUnitOut = AirMassFlow * (state.dataLoopNodes->Node(OutletNode).Enthalpy - state.dataLoopNodes->Node(InletNode).Enthalpy);

        // report variables
        state.dataWindowAC->WindAC(WindACNum).CompPartLoadRatio = state.dataWindowAC->WindAC(WindACNum).PartLoadFrac;
        if (state.dataWindowAC->WindAC(WindACNum).OpMode == CycFanCycCoil) {
            state.dataWindowAC->WindAC(WindACNum).FanPartLoadRatio = state.dataWindowAC->WindAC(WindACNum).PartLoadFrac;
        } else {
            if (UnitOn) {
                state.dataWindowAC->WindAC(WindACNum).FanPartLoadRatio = 1.0;
            } else {
                state.dataWindowAC->WindAC(WindACNum).FanPartLoadRatio = 0.0;
            }
        }
        state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate = std::abs(min(0.0, SensCoolOut));
        state.dataWindowAC->WindAC(WindACNum).TotCoolEnergyRate = std::abs(min(0.0, QTotUnitOut));
        state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate =
            min(state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate, state.dataWindowAC->WindAC(WindACNum).TotCoolEnergyRate);
        state.dataWindowAC->WindAC(WindACNum).LatCoolEnergyRate =
            state.dataWindowAC->WindAC(WindACNum).TotCoolEnergyRate - state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate;
        Real64 locFanElecPower = 0.0;
        if (state.dataWindowAC->WindAC(WindACNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            locFanElecPower = Fans::GetFanPower(state, state.dataWindowAC->WindAC(WindACNum).FanIndex);
        } else {
            locFanElecPower = state.dataHVACFan->fanObjs[state.dataWindowAC->WindAC(WindACNum).FanIndex]->fanPower();
        }
        state.dataWindowAC->WindAC(WindACNum).ElecPower = locFanElecPower + state.dataHVACGlobal->DXElecCoolingPower;

        PowerMet = QUnitOut;
        LatOutputProvided = LatentOutput;
    }

    void ReportWindowAC(EnergyPlusData &state, int const WindACNum) // number of the current AC unit being simulated
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Fills some of the report variables for the window AC units

        auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

        Real64 ReportingConstant;

        ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

        state.dataWindowAC->WindAC(WindACNum).SensCoolEnergy = state.dataWindowAC->WindAC(WindACNum).SensCoolEnergyRate * ReportingConstant;
        state.dataWindowAC->WindAC(WindACNum).TotCoolEnergy = state.dataWindowAC->WindAC(WindACNum).TotCoolEnergyRate * ReportingConstant;
        state.dataWindowAC->WindAC(WindACNum).LatCoolEnergy = state.dataWindowAC->WindAC(WindACNum).LatCoolEnergyRate * ReportingConstant;
        state.dataWindowAC->WindAC(WindACNum).ElecConsumption = state.dataWindowAC->WindAC(WindACNum).ElecPower * ReportingConstant;

        if (state.dataWindowAC->WindAC(WindACNum).FirstPass) { // reset sizing flags so other zone equipment can size normally
            if (!state.dataGlobal->SysSizingCalc) {
                DataSizing::resetHVACSizingGlobals(state, state.dataSize->CurZoneEqNum, 0, state.dataWindowAC->WindAC(WindACNum).FirstPass);
            }
        }
    }

    void CalcWindowACOutput(EnergyPlusData &state,
                            int const WindACNum,           // Unit index in fan coil array
                            bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                            int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                            Real64 const PartLoadFrac,     // unit part load fraction
                            bool const HXUnitOn,           // Flag to toggle HX heat recovery as needed
                            Real64 &LoadMet                // load met by unit (watts)
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Simulate the components making up the cycling window AC unit.

        // METHODOLOGY EMPLOYED:
        // Simulates the unit components sequentially in the air flow direction.

        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;
        using DXCoils::SimDXCoil;
        using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
        using MixedAir::SimOAMixer;

        int OutletNode;     // unit air outlet node
        int InletNode;      // unit air inlet node
        int OutsideAirNode; // outside air node number in window AC loop
        int AirRelNode;     // relief air node number in window AC loop
        Real64 AirMassFlow; // total mass flow through the unit
        Real64 MinHumRat;   // minimum of inlet & outlet humidity ratio

        OutletNode = state.dataWindowAC->WindAC(WindACNum).AirOutNode;
        InletNode = state.dataWindowAC->WindAC(WindACNum).AirInNode;
        OutsideAirNode = state.dataWindowAC->WindAC(WindACNum).OutsideAirNode;
        AirRelNode = state.dataWindowAC->WindAC(WindACNum).AirReliefNode;
        // for cycling fans, pretend we have VAV
        if (OpMode == CycFanCycCoil) {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRateMax * PartLoadFrac;
            // Don't let the outside air flow be > supply air flow
            state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate =
                min(state.dataLoopNodes->Node(OutsideAirNode).MassFlowRateMax, state.dataLoopNodes->Node(InletNode).MassFlowRate);
            state.dataLoopNodes->Node(AirRelNode).MassFlowRate = state.dataLoopNodes->Node(OutsideAirNode).MassFlowRate;
        }
        AirMassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        SimOAMixer(state, state.dataWindowAC->WindAC(WindACNum).OAMixName, FirstHVACIteration, state.dataWindowAC->WindAC(WindACNum).OAMixIndex);

        // if blow through, simulate fan then coil. For draw through, simulate coil then fan.
        if (state.dataWindowAC->WindAC(WindACNum).FanPlace == BlowThru) {
            if (state.dataWindowAC->WindAC(WindACNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state,
                                            state.dataWindowAC->WindAC(WindACNum).FanName,
                                            FirstHVACIteration,
                                            state.dataWindowAC->WindAC(WindACNum).FanIndex,
                                            PartLoadFrac,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                state.dataHVACFan->fanObjs[state.dataWindowAC->WindAC(WindACNum).FanIndex]->simulate(
                    state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }
        }

        if (state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num == CoilDX_CoolingHXAssisted) {
            SimHXAssistedCoolingCoil(state,
                                     state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                                     FirstHVACIteration,
                                     state.dataWindowAC->On,
                                     PartLoadFrac,
                                     state.dataWindowAC->WindAC(WindACNum).DXCoilIndex,
                                     state.dataWindowAC->WindAC(WindACNum).OpMode,
                                     HXUnitOn);
        } else if (state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
            Real64 QZnReq(-1.0);               // Zone load (W), input to variable-speed DX coil
            Real64 QLatReq(0.0);               // Zone latent load, input to variable-speed DX coil
            Real64 MaxONOFFCyclesperHour(4.0); // Maximum cycling rate of heat pump [cycles/hr]
            Real64 HPTimeConstant(0.0);        // Heat pump time constant [s]
            Real64 FanDelayTime(0.0);          // Fan delay time, time delay for the HP's fan to
            Real64 OnOffAirFlowRatio(1.0);     // ratio of compressor on flow to average flow over time step

            VariableSpeedCoils::SimVariableSpeedCoils(state,
                                                      state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                                                      state.dataWindowAC->WindAC(WindACNum).DXCoilIndex,
                                                      state.dataWindowAC->WindAC(WindACNum).OpMode,
                                                      MaxONOFFCyclesperHour,
                                                      HPTimeConstant,
                                                      FanDelayTime,
                                                      1.0,
                                                      PartLoadFrac,
                                                      state.dataWindowAC->WindAC(WindACNum).DXCoilNumOfSpeeds,
                                                      1.0,
                                                      QZnReq,
                                                      QLatReq,
                                                      OnOffAirFlowRatio);

        } else {
            SimDXCoil(state,
                      state.dataWindowAC->WindAC(WindACNum).DXCoilName,
                      state.dataWindowAC->On,
                      FirstHVACIteration,
                      state.dataWindowAC->WindAC(WindACNum).DXCoilIndex,
                      state.dataWindowAC->WindAC(WindACNum).OpMode,
                      PartLoadFrac);
        }

        if (state.dataWindowAC->WindAC(WindACNum).FanPlace == DrawThru) {
            if (state.dataWindowAC->WindAC(WindACNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::SimulateFanComponents(state,
                                            state.dataWindowAC->WindAC(WindACNum).FanName,
                                            FirstHVACIteration,
                                            state.dataWindowAC->WindAC(WindACNum).FanIndex,
                                            PartLoadFrac,
                                            ZoneCompTurnFansOn,
                                            ZoneCompTurnFansOff);
            } else {
                state.dataHVACFan->fanObjs[state.dataWindowAC->WindAC(WindACNum).FanIndex]->simulate(
                    state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            }
        }

        MinHumRat = min(state.dataLoopNodes->Node(InletNode).HumRat, state.dataLoopNodes->Node(OutletNode).HumRat);
        LoadMet = AirMassFlow * (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, MinHumRat) -
                                 PsyHFnTdbW(state.dataLoopNodes->Node(InletNode).Temp, MinHumRat));
    }

    void ControlCycWindACOutput(EnergyPlusData &state,
                                int const WindACNum,           // Unit index in fan coil array
                                bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                                int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                                Real64 const QZnReq,           // cooling output needed by zone [W]
                                Real64 &PartLoadFrac,          // unit part load fraction
                                bool &HXUnitOn                 // Used to control HX heat recovery as needed
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   May 2000
        //       MODIFIED       Shirey, May 2001
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Determine the part load fraction of the air conditioner for this time step

        // METHODOLOGY EMPLOYED:
        // Linear interpolation between max and min outputs

        int const MaxIter(50);    // maximum number of iterations
        Real64 const MinPLF(0.0); // minimum part load factor allowed

        Real64 FullOutput;   // unit full output [W]
        Real64 NoCoolOutput; // output when no active cooling [W]
        Real64 ActualOutput; // output at current partloadfrac [W]
        Real64 Error;        // error between QznReq and ActualOutput [W]
        Real64 ErrorToler;   // error tolerance
        int Iter;            // iteration counter
        // CHARACTER(len=20) :: ErrNum
        // INTEGER,SAVE :: ErrCount=0
        Real64 DelPLF;
        Real64 Relax;

        // DX Cooling HX assisted coils can cycle the heat exchanger, see if coil ON, HX OFF can meet humidity setpoint if one exists
        if (state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num == CoilDX_CoolingHXAssisted) {
            // Check for a setpoint at the HX outlet node, if it doesn't exist always run the HX
            if (state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRatMax == SensedNodeFlagValue) {
                HXUnitOn = true;
            } else {
                HXUnitOn = false;
            }
        } else {
            HXUnitOn = false;
        }

        if (state.dataWindowAC->WindAC(WindACNum).EMSOverridePartLoadFrac) {

            PartLoadFrac = state.dataWindowAC->WindAC(WindACNum).EMSValueForPartLoadFrac;
        }

        // Get result when DX coil is off
        CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, 0.0, HXUnitOn, NoCoolOutput);

        // If NoCoolOutput < QZnReq, the coil needs to be off
        if (NoCoolOutput < QZnReq) {
            PartLoadFrac = 0.0;
            return;
        }

        // Get full load result
        CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, 1.0, HXUnitOn, FullOutput);

        // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
        // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
        if (FullOutput >= 0.0 || FullOutput >= NoCoolOutput) {
            PartLoadFrac = 0.0;
            return;
        }

        // If the QZnReq <= FullOutput the unit needs to run full out
        if (QZnReq <= FullOutput && state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num != CoilDX_CoolingHXAssisted) {
            PartLoadFrac = 1.0;
            return;
        }

        // If the QZnReq <= FullOutput and a HXAssisted coil is used, check the node setpoint for a maximum humidity ratio set point
        // HumRatMax will be equal to -999 if no setpoint exists or some set point managers may still use 0 as a no moisture load indicator
        if (QZnReq <= FullOutput && state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num == CoilDX_CoolingHXAssisted &&
            state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRatMax <= 0.0) {
            PartLoadFrac = 1.0;
            return;
        }

        // QZnReq should now be greater than FullOutput and less than NoCoolOutput)
        // Calculate the part load fraction

        PartLoadFrac = max(MinPLF, std::abs(QZnReq - NoCoolOutput) / std::abs(FullOutput - NoCoolOutput));

        ErrorToler = state.dataWindowAC->WindAC(WindACNum).ConvergenceTol; // Error tolerance for convergence from input deck
        Error = 1.0;                                                       // initialize error value for comparison against tolerance
        Iter = 0;                                                          // initialize iteration counter
        Relax = 1.0;

        while ((std::abs(Error) > ErrorToler) && (Iter <= MaxIter) && PartLoadFrac > MinPLF) {
            // Get result when DX coil is operating at partloadfrac
            CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, ActualOutput);
            Error = (QZnReq - ActualOutput) / QZnReq;
            DelPLF = (QZnReq - ActualOutput) / FullOutput;
            PartLoadFrac += Relax * DelPLF;
            PartLoadFrac = max(MinPLF, min(1.0, PartLoadFrac));
            ++Iter;
            if (Iter == 16) {
                Relax = 0.5;
            }
        }
        if (Iter > MaxIter) {
            if (state.dataWindowAC->WindAC(WindACNum).MaxIterIndex1 == 0) {
                ShowWarningMessage(state,
                                   "ZoneHVAC:WindowAirConditioner=\"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                       "\" -- Exceeded max iterations while adjusting compressor sensible runtime to meet the zone load within the "
                                       "cooling convergence tolerance.");
                ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIter));
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "ZoneHVAC:WindowAirConditioner=\"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                               "\"  -- Exceeded max iterations error (sensible runtime) continues...",
                                           state.dataWindowAC->WindAC(WindACNum).MaxIterIndex1);
        }

        // HX is off up until this point where the outlet air humidity ratio is tested to see if HX needs to be turned on
        if (state.dataWindowAC->WindAC(WindACNum).DXCoilType_Num == CoilDX_CoolingHXAssisted &&
            state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRatMax <
                state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRat &&
            state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRatMax > 0.0) {

            //   Run the HX to recovery energy and improve latent performance
            HXUnitOn = true;

            //   Get full load result
            CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, 1.0, HXUnitOn, FullOutput);

            if (state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRatMax <
                    state.dataLoopNodes->Node(state.dataWindowAC->WindAC(WindACNum).CoilOutletNodeNum).HumRat ||
                QZnReq <= FullOutput) {
                PartLoadFrac = 1.0;
                return;
            }

            Error = 1.0; // initialize error value for comparison against tolerance
            Iter = 0;    // initialize iteration counter
            Relax = 1.0;

            while ((std::abs(Error) > ErrorToler) && (Iter <= MaxIter) && PartLoadFrac > MinPLF) {
                // Get result when DX coil is operating at partloadfrac
                CalcWindowACOutput(state, WindACNum, FirstHVACIteration, OpMode, PartLoadFrac, HXUnitOn, ActualOutput);
                Error = (QZnReq - ActualOutput) / QZnReq;
                DelPLF = (QZnReq - ActualOutput) / FullOutput;
                PartLoadFrac += Relax * DelPLF;
                PartLoadFrac = max(MinPLF, min(1.0, PartLoadFrac));
                ++Iter;
                if (Iter == 16) {
                    Relax = 0.5;
                }
            }
            if (Iter > MaxIter) {
                if (state.dataWindowAC->WindAC(WindACNum).MaxIterIndex2 == 0) {
                    ShowWarningMessage(state,
                                       "ZoneHVAC:WindowAirConditioner=\"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                           "\" -- Exceeded max iterations while adjusting compressor latent runtime to meet the zone load within the "
                                           "cooling convergence tolerance.");
                    ShowContinueErrorTimeStamp(state, format("Iterations={}", MaxIter));
                }
                ShowRecurringWarningErrorAtEnd(state,
                                               "ZoneHVAC:WindowAirConditioner=\"" + state.dataWindowAC->WindAC(WindACNum).Name +
                                                   "\"  -- Exceeded max iterations error (latent runtime) continues...",
                                               state.dataWindowAC->WindAC(WindACNum).MaxIterIndex2);
            }

        } // WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted && *
    }

    int GetWindowACZoneInletAirNode(EnergyPlusData &state, int const WindACNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for zone inlet node

        // Return value
        int GetWindowACZoneInletAirNode;

        if (state.dataWindowAC->GetWindowACInputFlag) {
            GetWindowAC(state);
            state.dataWindowAC->GetWindowACInputFlag = false;
        }

        GetWindowACZoneInletAirNode = state.dataWindowAC->WindAC(WindACNum).AirOutNode;

        return GetWindowACZoneInletAirNode;
    }

    int GetWindowACOutAirNode(EnergyPlusData &state, int const WindACNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for OA inlet node

        // Return value
        int GetWindowACOutAirNode;

        if (state.dataWindowAC->GetWindowACInputFlag) {
            GetWindowAC(state);
            state.dataWindowAC->GetWindowACInputFlag = false;
        }

        GetWindowACOutAirNode = state.dataWindowAC->WindAC(WindACNum).OutsideAirNode;

        return GetWindowACOutAirNode;
    }

    int GetWindowACReturnAirNode(EnergyPlusData &state, int const WindACNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for mixer return air node for ventilation load reporting

        using MixedAir::GetOAMixerReturnNodeNumber;

        // Return value
        int GetWindowACReturnAirNode;

        if (state.dataWindowAC->GetWindowACInputFlag) {
            GetWindowAC(state);
            state.dataWindowAC->GetWindowACInputFlag = false;
        }

        if (WindACNum > 0 && WindACNum <= state.dataWindowAC->NumWindAC) {
            if (state.dataWindowAC->WindAC(WindACNum).OAMixIndex > 0) {
                GetWindowACReturnAirNode = GetOAMixerReturnNodeNumber(state, state.dataWindowAC->WindAC(WindACNum).OAMixIndex);
            } else {
                GetWindowACReturnAirNode = 0;
            }
        } else {
            GetWindowACReturnAirNode = 0;
        }

        return GetWindowACReturnAirNode;
    }

    int GetWindowACMixedAirNode(EnergyPlusData &state, int const WindACNum)
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         B Griffith
        //       DATE WRITTEN   Dec  2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // lookup function for mixed air node for ventilation rate reporting

        using MixedAir::GetOAMixerMixedNodeNumber;

        // Return value
        int GetWindowACMixedAirNode;

        if (state.dataWindowAC->GetWindowACInputFlag) {
            GetWindowAC(state);
            state.dataWindowAC->GetWindowACInputFlag = false;
        }

        if (WindACNum > 0 && WindACNum <= state.dataWindowAC->NumWindAC) {
            if (state.dataWindowAC->WindAC(WindACNum).OAMixIndex > 0) {
                GetWindowACMixedAirNode = GetOAMixerMixedNodeNumber(state, state.dataWindowAC->WindAC(WindACNum).OAMixIndex);
            } else {
                GetWindowACMixedAirNode = 0;
            }
        } else {
            GetWindowACMixedAirNode = 0;
        }

        return GetWindowACMixedAirNode;
    }

} // namespace WindowAC

} // namespace EnergyPlus
