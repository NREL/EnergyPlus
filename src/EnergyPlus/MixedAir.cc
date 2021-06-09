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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/TranspiredCollector.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus::MixedAir {

// Module containing the routines dealing with the mixed air portion
// of the HVAC air loop.

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   October 1998
//       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Jan 2004
//                      Lawrie, March 2006 - Module order (per template)
//                      Craig Wray 22Aug2010 - Added Fan ComponentModel
//                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
//                           to enhance CO2 based DCV control
//                      Feb 2013 Bereket Nigusse, FSEC
//                        Added DX Coil Model For 100% OA systems
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to
// simulate the mixed air portion of the EPlus air loop.

// METHODOLOGY EMPLOYED:
// An algorithmic controller will be employed - there is no attempt to
// simulate real controllers for the economizer. The mixed air controller
// will sense various node conditions and set some node flow rates.  Mixed
// air components will operate with predetermined flow rates.

// Using/Aliasing
using namespace DataLoopNode;
using namespace DataAirLoop;
using namespace DataEnvironment;
using namespace DataHVACGlobals;
using namespace ScheduleManager;
using namespace DataSizing;
using namespace FaultsManager;

Array1D_string const CurrentModuleObjects(8,
                                          {"AirLoopHVAC:OutdoorAirSystem",
                                           "AirLoopHVAC:OutdoorAirSystem:EquipmentList",
                                           "AirLoopHVAC:ControllerList",
                                           "AvailabilityManagerAssignmentList",
                                           "Controller:OutdoorAir",
                                           "ZoneHVAC:EnergyRecoveryVentilator:Controller",
                                           "Controller:MechanicalVentilation",
                                           "OutdoorAir:Mixer"});

Real64 OAGetFlowRate(EnergyPlusData &state, int OAPtr)
{
    Real64 FlowRate(0);
    if ((OAPtr > 0) && (OAPtr <= state.dataMixedAir->NumOAControllers) && (state.dataEnvrn->StdRhoAir != 0)) {
        FlowRate = state.dataMixedAir->OAController(OAPtr).OAMassFlow / state.dataEnvrn->StdRhoAir;
    }
    return FlowRate;
}
Real64 OAGetMinFlowRate(EnergyPlusData &state, int OAPtr)
{
    Real64 MinFlowRate(0);
    if ((OAPtr > 0) && (OAPtr <= state.dataMixedAir->NumOAControllers)) {
        MinFlowRate = state.dataMixedAir->OAController(OAPtr).MinOA;
    }
    return MinFlowRate;
}
void OASetDemandManagerVentilationState(EnergyPlusData &state, int OAPtr, bool aState)
{
    if ((OAPtr > 0) && (OAPtr <= state.dataMixedAir->NumOAControllers)) {
        state.dataMixedAir->OAController(OAPtr).ManageDemand = aState;
    }
}
void OASetDemandManagerVentilationFlow(EnergyPlusData &state, int OAPtr, Real64 aFlow)
{
    if ((OAPtr > 0) && (OAPtr <= state.dataMixedAir->NumOAControllers)) {
        state.dataMixedAir->OAController(OAPtr).DemandLimitFlowRate = aFlow * state.dataEnvrn->StdRhoAir;
    }
}
int GetOAController(EnergyPlusData &state, std::string const &OAName)
{
    int CurrentOAController(0);
    for (int i = 1; i <= state.dataMixedAir->NumOAControllers; i++) {
        if (OAName == state.dataMixedAir->OAController(i).Name) {
            CurrentOAController = i;
            break;
        }
    }
    return CurrentOAController;
}

void ManageOutsideAirSystem(EnergyPlusData &state, std::string const &OASysName, bool const FirstHVACIteration, int const AirLoopNum, int &OASysNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Manage the outside air system

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    if (OASysNum == 0) {
        OASysNum = UtilityRoutines::FindItemInList(OASysName, state.dataAirLoop->OutsideAirSys);
        if (OASysNum == 0) {
            ShowFatalError(state, "ManageOutsideAirSystem: AirLoopHVAC:OutdoorAirSystem not found=" + OASysName);
        }
    }

    InitOutsideAirSys(state, OASysNum, FirstHVACIteration, AirLoopNum);

    SimOutsideAirSys(state, OASysNum, FirstHVACIteration, AirLoopNum);
}

void SimOASysComponents(EnergyPlusData &state, int const OASysNum, bool const FirstHVACIteration, int const AirLoopNum)
{
    int CompNum;
    auto &CompType = state.dataMixedAir->CompType;
    auto &CompName = state.dataMixedAir->CompName;
    bool ReSim(false);
    bool Sim(true);
    bool OAHeatCoil(false);
    bool OACoolCoil(false);
    bool OAHX(false);

    for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents; ++CompNum) {
        CompType = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(CompNum);
        CompName = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum);
        SimOAComponent(state,
                       CompType,
                       CompName,
                       static_cast<MixedAir::ComponentType>(state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum)),
                       FirstHVACIteration,
                       state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex(CompNum),
                       AirLoopNum,
                       Sim,
                       OASysNum,
                       OAHeatCoil,
                       OACoolCoil,
                       OAHX);
        if (OAHX) ReSim = true;
    }
    // if there were heat exchangers and/or desiccant wheel in the OA path, need to simulate again
    // in reverse order to propagate the air flow and conditions out the relief air path to the relief air
    // exit node
    if (ReSim) {
        for (CompNum = state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents - 1; CompNum >= 1; --CompNum) {
            CompType = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(CompNum);
            CompName = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum);
            SimOAComponent(state,
                           CompType,
                           CompName,
                           static_cast<MixedAir::ComponentType>(state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum)),
                           FirstHVACIteration,
                           state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex(CompNum),
                           AirLoopNum,
                           Sim,
                           OASysNum,
                           OAHeatCoil,
                           OACoolCoil,
                           OAHX);
        }
        // now simulate again propagate current temps back through OA system
        for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents; ++CompNum) {
            CompType = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(CompNum);
            CompName = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum);
            SimOAComponent(state,
                           CompType,
                           CompName,
                           static_cast<MixedAir::ComponentType>(state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum)),
                           FirstHVACIteration,
                           state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex(CompNum),
                           AirLoopNum,
                           Sim,
                           OASysNum,
                           OAHeatCoil,
                           OACoolCoil,
                           OAHX);
        }
    }
}

void SimOutsideAirSys(EnergyPlusData &state, int const OASysNum, bool const FirstHVACIteration, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Simulate the controllers and components in the outside air system.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CompNum;
    // INTEGER :: CtrlNum
    int OAMixerNum;
    int OAControllerNum;                           // OA controller index in OAController
    auto &CompType = state.dataMixedAir->CompType; // Tuned Made static
    auto &CompName = state.dataMixedAir->CompName; // Tuned Made static
    bool FatalErrorFlag(false);

    // SimOutsideAirSys can handle only 1 controller right now.  This must be
    // an Outside Air Controller.  This is because of the lack of iteration
    // and convergence control in the following code.
    //  DO CtrlNum=1,OutsideAirSys(OASysNum)%NumControllers
    //    CtrlName = OutsideAirSys(OASysNum)%ControllerName(CtrlNum)
    //    CALL SimOAController(CtrlName,FirstHVACIteration)
    //  END DO
    state.dataSize->CurOASysNum = OASysNum;
    auto &CurrentOASystem(state.dataAirLoop->OutsideAirSys(OASysNum));
    if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum == -1) {
        SimOAController(state, CurrentOASystem.OAControllerName, CurrentOASystem.OAControllerIndex, FirstHVACIteration, AirLoopNum);
    }
    SimOASysComponents(state, OASysNum, FirstHVACIteration, AirLoopNum);

    if (state.dataMixedAir->MyOneTimeErrorFlag(OASysNum)) {
        if (CurrentOASystem.NumControllers - CurrentOASystem.NumSimpleControllers > 1) {
            ShowWarningError(
                state, "AirLoopHVAC:OutdoorAirSystem " + CurrentOASystem.Name + " has more than 1 outside air controller; only the 1st will be used");
        }
        for (CompNum = 1; CompNum <= CurrentOASystem.NumComponents; ++CompNum) {
            CompType = CurrentOASystem.ComponentType(CompNum);
            CompName = CurrentOASystem.ComponentName(CompNum);
            if (UtilityRoutines::SameString(CompType, "OutdoorAir:Mixer")) {
                OAMixerNum = UtilityRoutines::FindItemInList(CompName, state.dataMixedAir->OAMixer);
                OAControllerNum = CurrentOASystem.OAControllerIndex;
                if (state.dataMixedAir->OAController(OAControllerNum).MixNode != state.dataMixedAir->OAMixer(OAMixerNum).MixNode) {
                    ShowSevereError(state,
                                    "The mixed air node of Controller:OutdoorAir=\"" + state.dataMixedAir->OAController(OAControllerNum).Name + "\"");
                    ShowContinueError(state,
                                      "should be the same node as the mixed air node of OutdoorAir:Mixer=\"" +
                                          state.dataMixedAir->OAMixer(OAMixerNum).Name + "\".");
                    ShowContinueError(state,
                                      "Controller:OutdoorAir mixed air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAController(OAControllerNum).MixNode) + "\".");
                    ShowContinueError(state,
                                      "OutdoorAir:Mixer mixed air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OAMixerNum).MixNode) + "\".");
                    FatalErrorFlag = true;
                }
                if (state.dataMixedAir->OAController(OAControllerNum).RelNode != state.dataMixedAir->OAMixer(OAMixerNum).RelNode) {
                    ShowSevereError(
                        state, "The relief air node of Controller:OutdoorAir=\"" + state.dataMixedAir->OAController(OAControllerNum).Name + "\"");
                    ShowContinueError(state,
                                      "should be the same node as the relief air node of OutdoorAir:Mixer=\"" +
                                          state.dataMixedAir->OAMixer(OAMixerNum).Name + "\".");
                    ShowContinueError(state,
                                      "Controller:OutdoorAir relief air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAController(OAControllerNum).RelNode) + "\".");
                    ShowContinueError(state,
                                      "OutdoorAir:Mixer relief air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OAMixerNum).RelNode) + "\".");
                    FatalErrorFlag = true;
                }
                if (state.dataMixedAir->OAController(OAControllerNum).RetNode != state.dataMixedAir->OAMixer(OAMixerNum).RetNode) {
                    ShowSevereError(
                        state, "The return air node of Controller:OutdoorAir=\"" + state.dataMixedAir->OAController(OAControllerNum).Name + "\"");
                    ShowContinueError(state,
                                      "should be the same node as the return air node of OutdoorAir:Mixer=\"" +
                                          state.dataMixedAir->OAMixer(OAMixerNum).Name + "\".");
                    ShowContinueError(state,
                                      "Controller:OutdoorAir return air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAController(OAControllerNum).RetNode) + "\".");
                    ShowContinueError(state,
                                      "OutdoorAir:Mixer return air node=\"" +
                                          state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OAMixerNum).RetNode) + "\".");
                    FatalErrorFlag = true;
                }
            }
        }
        state.dataMixedAir->MyOneTimeErrorFlag(OASysNum) = false;
        if (FatalErrorFlag) ShowFatalError(state, "Previous severe error(s) cause program termination");
    }

    state.dataSize->CurOASysNum = 0;
    if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum == -1) {
        state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysComponentsSimulated = true;
    }
}

void SimOAComponent(EnergyPlusData &state,
                    std::string const &CompType,               // the component type
                    std::string const &CompName,               // the component Name
                    MixedAir::ComponentType const CompTypeNum, // Component Type -- Integerized for this module
                    bool const FirstHVACIteration,
                    int &CompIndex,
                    int const AirLoopNum, // air loop index for economizer lockout coordination
                    bool const Sim,       // if TRUE, simulate component; if FALSE, just set the coil exisitence flags
                    int const OASysNum,   // index to outside air system
                    bool &OAHeatingCoil,  // TRUE indicates a heating coil has been found
                    bool &OACoolingCoil,  // TRUE indicates a cooling coil has been found
                    bool &OAHX            // TRUE indicates a heat exchanger has been found
)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
    //       DATE WRITTEN:  Oct 1997
    //           MODIFIED:  Dec 1997 Fred Buhl, D Shirey Feb/Sept 2003
    //                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
    //                        Add DXSystem:AirLoop as valid OA system equipment
    //                        Work supported by ASHRAE research project 1254-RP
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // Calls the individual air loop component simulation routines

    // METHODOLOGY EMPLOYED: None

    // REFERENCES: None

    // USE Statements
    // Using/Aliasing
    using DesiccantDehumidifiers::SimDesiccantDehumidifier;
    using EvaporativeCoolers::SimEvapCooler;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using HeatRecovery::SimHeatRecovery;
    using Humidifiers::SimHumidifier;
    using HVACDXHeatPumpSystem::SimDXHeatPumpSystem;
    using HVACDXSystem::SimDXCoolingSystem;
    using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
    using SimAirServingZones::SolveWaterCoilController;
    using SteamCoils::SimulateSteamCoilComponents;
    using TranspiredCollector::SimTranspiredCollector;
    using UserDefinedComponents::SimCoilUserDefined;
    // Locals
    // SUBROUTINE ARGUMENTS:

    // SUBROUTINE PARAMETER DEFINITIONS: None

    // INTERFACE BLOCK DEFINITIONS: None

    // DERIVED TYPE DEFINITIONS: None

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS

    OAHeatingCoil = false;
    OACoolingCoil = false;
    OAHX = false;
    Real64 AirloopPLR;
    int FanOpMode;

    if (CompTypeNum == ComponentType::OAMixer_Num) { // 'OutdoorAir:Mixer'
        if (Sim) {
            SimOAMixer(state, CompName, FirstHVACIteration, CompIndex);
        }

        // Fan Types
    } else if (CompTypeNum == ComponentType::Fan_Simple_CV) { // 'Fan:ConstantVolume'
        if (Sim) {
            Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);
        }
    } else if (CompTypeNum == ComponentType::Fan_Simple_VAV) { // 'Fan:VariableVolume'
        if (Sim) {
            Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);
        }

    } else if (CompTypeNum == ComponentType::Fan_System_Object) {              // 'Fan:SystemModel'
        if (CompIndex == 0) {                                                  // 0 means has not been filled because of 1-based arrays in old fortran
            CompIndex = HVACFan::getFanObjectVectorIndex(state, CompName) + 1; // + 1 for shift from zero-based vector to 1-based compIndex
        }
        if (Sim) {
            state.dataHVACFan->fanObjs[CompIndex - 1]->simulate(state, _, _, _, _); // vector is 0 based, but CompIndex is 1 based so shift
        }
    } else if (CompTypeNum == ComponentType::Fan_ComponentModel) { // 'Fan:ComponentModel'
        if (Sim) {
            Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);
        }

        // Coil Types
    } else if (CompTypeNum == ComponentType::WaterCoil_Cooling) { // 'Coil:Cooling:Water'
        if (Sim) {
            // get water coil and controller data if not called previously
            if (CompIndex == 0) WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex);
            // iterate on OA sys controller and water coil at the same time
            SolveWaterCoilController(state,
                                     FirstHVACIteration,
                                     AirLoopNum,
                                     CompName,
                                     CompIndex,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerName,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex,
                                     false);
            // set flag to tell HVAC controller it will be simulated only in SolveWaterCoilController()
            state.dataHVACControllers->ControllerProps(state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex).BypassControllerCalc = true;
        }
        OACoolingCoil = true;
    } else if (CompTypeNum == ComponentType::WaterCoil_SimpleHeat) { // 'Coil:Heating:Water')
        if (Sim) {
            // get water coil and controller data if not called previously
            if (CompIndex == 0) WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex);
            // iterate on OA sys controller and water coil at the same time
            SolveWaterCoilController(state,
                                     FirstHVACIteration,
                                     AirLoopNum,
                                     CompName,
                                     CompIndex,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerName,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex,
                                     false);
            // set flag to tell HVAC controller it will be simulated only in SolveWaterCoilController()
            state.dataHVACControllers->ControllerProps(state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex).BypassControllerCalc = true;
        }
        OAHeatingCoil = true;
    } else if (CompTypeNum == ComponentType::SteamCoil_AirHeat) { // 'Coil:Heating:Steam'
        if (Sim) {
            SimulateSteamCoilComponents(state, CompName, FirstHVACIteration, CompIndex, 0.0);
        }
        OAHeatingCoil = true;
    } else if (CompTypeNum == ComponentType::WaterCoil_DetailedCool) { // 'Coil:Cooling:Water:DetailedGeometry'
        if (Sim) {
            // get water coil and controller data if not called previously
            if (CompIndex == 0) WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex);
            // iterate on OA sys controller and water coil at the same time
            SolveWaterCoilController(state,
                                     FirstHVACIteration,
                                     AirLoopNum,
                                     CompName,
                                     CompIndex,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerName,
                                     state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex,
                                     false);
            // set flag to tell HVAC controller it will be simulated only in SolveWaterCoilController()
            state.dataHVACControllers->ControllerProps(state.dataWaterCoils->WaterCoil(CompIndex).ControllerIndex).BypassControllerCalc = true;
        }
        OACoolingCoil = true;
    } else if (CompTypeNum == ComponentType::Coil_ElectricHeat) { // 'Coil:Heating:Electric'
        if (Sim) {
            //     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
            SimulateHeatingCoilComponents(state, CompName, FirstHVACIteration, _, CompIndex);
        }
        OAHeatingCoil = true;
    } else if (CompTypeNum == ComponentType::Coil_GasHeat) { // 'Coil:Heating:Fuel'
        if (Sim) {
            //     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
            SimulateHeatingCoilComponents(state, CompName, FirstHVACIteration, _, CompIndex);
        }
        OAHeatingCoil = true;
    } else if (CompTypeNum == ComponentType::WaterCoil_CoolingHXAsst) { // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
        if (Sim) {
            // get water coil and controller data if not called previously
            if (CompIndex == 0) SimHXAssistedCoolingCoil(state, CompName, FirstHVACIteration, On, 0.0, CompIndex, ContFanCycCoil);
            // iterate on OA sys controller and water coil at the same time
            SolveWaterCoilController(state,
                                     FirstHVACIteration,
                                     AirLoopNum,
                                     CompName,
                                     CompIndex,
                                     state.dataHVACAssistedCC->HXAssistedCoil(CompIndex).ControllerName,
                                     state.dataHVACAssistedCC->HXAssistedCoil(CompIndex).ControllerIndex,
                                     true);
            // set flag to tell HVAC controller it will be simulated only in SolveWaterCoilController()
            state.dataHVACControllers->ControllerProps(state.dataHVACAssistedCC->HXAssistedCoil(CompIndex).ControllerIndex).BypassControllerCalc =
                true;
        }
        OACoolingCoil = true;
    } else if (CompTypeNum == ComponentType::DXSystem) { // CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
        if (Sim) {
            SimDXCoolingSystem(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
        }
        OACoolingCoil = true;
    } else if (CompTypeNum == ComponentType::UnitarySystemModel) { // AirLoopHVAC:UnitarySystem
        if (Sim) {
            bool HeatingActive = false;
            bool CoolingActive = false;
            Real64 OAUCoilOutTemp = 0.0;
            bool ZoneEquipFlag = false;
            Real64 sensOut = 0.0;
            Real64 latOut = 0.0;
            state.dataAirLoop->OutsideAirSys(OASysNum).compPointer[CompIndex]->simulate(state,
                                                                                        CompName,
                                                                                        FirstHVACIteration,
                                                                                        AirLoopNum,
                                                                                        CompIndex,
                                                                                        HeatingActive,
                                                                                        CoolingActive,
                                                                                        CompIndex,
                                                                                        OAUCoilOutTemp,
                                                                                        ZoneEquipFlag,
                                                                                        sensOut,
                                                                                        latOut);
        }
        if (state.dataMixedAir->MyOneTimeCheckUnitarySysFlag(OASysNum)) {
            UnitarySystems::UnitarySys::getUnitarySysHeatCoolCoil(state, CompName, OACoolingCoil, OAHeatingCoil, 0);
            UnitarySystems::UnitarySys::checkUnitarySysCoilInOASysExists(state, CompName, 0);
            if (Sim) state.dataMixedAir->MyOneTimeCheckUnitarySysFlag(OASysNum) = false;
        }
    } else if (CompTypeNum == ComponentType::DXHeatPumpSystem) {
        if (Sim) {
            SimDXHeatPumpSystem(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
        }
        OAHeatingCoil = true;
    } else if (CompTypeNum == ComponentType::Coil_UserDefined) {
        if (Sim) {
            SimCoilUserDefined(state, CompName, CompIndex, AirLoopNum, OAHeatingCoil, OACoolingCoil);
        }
        // Heat recovery
    } else if (CompTypeNum == ComponentType::HeatXchngr) { // 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent',
        // 'HeatExchanger:Desiccant:BalancedFlow'
        if (Sim) {
            if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum > -1) {
                AirloopPLR = 1.0;
                FanOpMode = DataHVACGlobals::ContFanCycCoil;
            } else {
                if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    FanOpMode = DataHVACGlobals::CycFanCycCoil;
                } else {
                    FanOpMode = DataHVACGlobals::ContFanCycCoil;
                }
                if (FanOpMode == DataHVACGlobals::CycFanCycCoil) {
                    // HX's in the OA system can be troublesome given that the OA flow rate is not necessarily proportional to air loop PLR
                    // adding that user input for branch flow rate, HX nominal flow rate, OA system min/max flow rate will not necessarily be
                    // perfectly input, a compromise is used for OA sys HX's as the ratio of flow to max. Issue #4298.
                    //                    AirloopPLR = AirLoopFlow( AirLoopNum ).FanPLR;
                    AirloopPLR = state.dataMixedAir->OAController(OASysNum).OAMassFlow / state.dataMixedAir->OAController(OASysNum).MaxOAMassFlowRate;
                } else {
                    AirloopPLR = 1.0;
                }
            }
            if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum > -1) {
                SimHeatRecovery(state, CompName, FirstHVACIteration, CompIndex, FanOpMode, AirloopPLR, _, _, _, _, _);
            } else {
                SimHeatRecovery(state,
                                CompName,
                                FirstHVACIteration,
                                CompIndex,
                                FanOpMode,
                                AirloopPLR,
                                _,
                                _,
                                _,
                                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass,
                                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HighHumCtrlActive);
            }
        }
        OAHX = true;

        // Desiccant Dehumidifier
    } else if (CompTypeNum == ComponentType::Desiccant) { // 'Dehumidifier:Desiccant:NoFans'
        // 'Dehumidifier:Desiccant:System'
        if (Sim) {
            SimDesiccantDehumidifier(state, CompName, FirstHVACIteration, CompIndex);
        }
        OAHX = true;

        // Humidifiers
    } else if (CompTypeNum == ComponentType::Humidifier) { // 'Humidifier:Steam:Electric'
        // 'Humidifier:Steam:Gas'
        if (Sim) {
            SimHumidifier(state, CompName, FirstHVACIteration, CompIndex);
        }

        // Unglazed Transpired Solar Collector
    } else if (CompTypeNum == ComponentType::Unglazed_SolarCollector) { // 'SolarCollector:UnglazedTranspired'
        if (Sim) {
            SimTranspiredCollector(state, CompName, CompIndex);
        }

        // Air-based Photovoltaic-thermal flat plate collector
    } else if (CompTypeNum == ComponentType::PVT_AirBased) { // 'SolarCollector:FlatPlate:PhotovoltaicThermal'
        if (Sim) {
            if (CompIndex == 0) {
                CompIndex = PhotovoltaicThermalCollectors::getPVTindexFromName(state, CompName);
            }
            PhotovoltaicThermalCollectors::simPVTfromOASys(state, CompIndex, FirstHVACIteration);
        }

        // Evaporative Cooler Types
    } else if (CompTypeNum == ComponentType::EvapCooler) { // 'EvaporativeCooler:Direct:CelDekPad','EvaporativeCooler:Indirect:CelDekPad'
        // 'EvaporativeCooler:Indirect:WetCoil','EvaporativeCooler:Indirect:ResearchSpecial'
        if (Sim) {
            SimEvapCooler(state, CompName, CompIndex);
        }

    } else if (CompTypeNum == ComponentType::VRFTerminalUnit) { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
        if (Sim) {
            int ControlledZoneNum = 0;
            bool HeatingActive = false;
            bool CoolingActive = false;
            int const OAUnitNum = 0;
            Real64 const OAUCoilOutTemp = 0.0;
            bool const ZoneEquipment = false;
            Real64 sysOut = 0.0;
            Real64 latOut = 0.0;
            HVACVariableRefrigerantFlow::SimulateVRF(state,
                                                     CompName,
                                                     FirstHVACIteration,
                                                     ControlledZoneNum,
                                                     CompIndex,
                                                     HeatingActive,
                                                     CoolingActive,
                                                     OAUnitNum,
                                                     OAUCoilOutTemp,
                                                     ZoneEquipment,
                                                     sysOut,
                                                     latOut);
        } else {
            HVACVariableRefrigerantFlow::isVRFCoilPresent(state, CompName, OACoolingCoil, OAHeatingCoil);
        }

    } else {
        ShowFatalError(state, "Invalid Outside Air Component=" + CompType);
    }
}

void SimOAMixer(EnergyPlusData &state, std::string const &CompName, bool const FirstHVACIteration, int &CompIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Simulate an Outside Air Mixer component

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OAMixerNum;

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    if (CompIndex == 0) {
        OAMixerNum = UtilityRoutines::FindItemInList(CompName, state.dataMixedAir->OAMixer);
        CompIndex = OAMixerNum;
        if (OAMixerNum == 0) {
            ShowFatalError(state, "SimOAMixer: OutdoorAir:Mixer not found=" + CompName);
        }
    } else {
        OAMixerNum = CompIndex;
    }

    InitOAMixer(state, OAMixerNum, FirstHVACIteration);

    CalcOAMixer(state, OAMixerNum);

    UpdateOAMixer(state, OAMixerNum);

    ReportOAMixer(OAMixerNum);
}

void SimOAController(EnergyPlusData &state, std::string const &CtrlName, int &CtrlIndex, bool const FirstHVACIteration, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Simulate an Outside Air Controller component

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OAControllerNum;

    if ((state.dataMixedAir->GetOAControllerInputFlag) &&
        (AirLoopNum > 0)) { // Gets input for object  first time Sim routine is called from an airloop
        GetOAControllerInputs(state);
        state.dataMixedAir->GetOAControllerInputFlag = false;
    }

    if (CtrlIndex == 0) {
        if (state.dataMixedAir->NumOAControllers > 0) {
            OAControllerNum = UtilityRoutines::FindItemInList(CtrlName, state.dataMixedAir->OAController);
        } else {
            OAControllerNum = 0;
        }
        CtrlIndex = OAControllerNum;
        if (OAControllerNum == 0) {
            ShowFatalError(state, "SimOAController: Outside Air Controller not found=" + CtrlName);
        }
    } else {
        OAControllerNum = CtrlIndex;
    }

    InitOAController(state, OAControllerNum, FirstHVACIteration, AirLoopNum);

    state.dataMixedAir->OAController(OAControllerNum).CalcOAController(state, AirLoopNum, FirstHVACIteration);
    state.dataMixedAir->OAController(OAControllerNum).UpdateOAController(state);
}

// Get Input Section of the Module
//******************************************************************************

void GetOutsideAirSysInputs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Input the Outside Air System data and store it in the OutsideAirSys array.

    // METHODOLOGY EMPLOYED:
    // Use the Get routines from the InputProcessor module.

    // Using/Aliasing
    using BranchNodeConnections::SetUpCompSets;
    using BranchNodeConnections::TestCompSet;
    using HVACDXSystem::CheckDXCoolingCoilInOASysExists;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetOutsideAirSysInputs: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int NumNums;   // Number of real numbers returned by GetObjectItem
    int NumAlphas; // Number of alphanumerics returned by GetObjectItem
    int IOStat;
    Array1D<Real64> NumArray;
    Array1D_string AlphArray;
    int OASysNum;
    int CompNum;
    int Item;
    // unused0909INTEGER :: NumComponents
    int AlphaNum;
    std::string ComponentListName;
    std::string ControllerListName;
    std::string AvailManagerListName;
    int NumInList;
    int InListNum;
    int ListNum;
    int NumSimpControllers; // number of Controller:Simple objects in an OA System
    bool ErrorsFound(false);
    std::string CurrentModuleObject; // Object type for getting and messages
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    int MaxNums(0);                  // Maximum number of numeric input fields
    int MaxAlphas(0);                // Maximum number of alpha input fields
    int TotalArgs(0);                // Total number of alpha and numeric arguments (max) for a
    //  certain object in the input file

    if (!state.dataMixedAir->GetOASysInputFlag) return;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::OASystem)), TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::AirLoopEqList)), TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::ControllerList)), TotalArgs, NumAlphas, NumNums);
    MaxNums = max(MaxNums, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);

    AlphArray.allocate(MaxAlphas);
    cAlphaFields.allocate(MaxAlphas);
    NumArray.dimension(MaxNums, 0.0);
    cNumericFields.allocate(MaxNums);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNums, true);

    CurrentModuleObject = CurrentModuleObjects(static_cast<int>(CMO::ControllerList));
    state.dataMixedAir->NumControllerLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

    state.dataMixedAir->ControllerLists.allocate(state.dataMixedAir->NumControllerLists);

    for (Item = 1; Item <= state.dataMixedAir->NumControllerLists; ++Item) {

        // create a reference for convenience
        auto &thisControllerList(state.dataMixedAir->ControllerLists(Item));
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 Item,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);
        thisControllerList.Name = AlphArray(1);
        thisControllerList.NumControllers = (NumAlphas - 1) / 2;
        thisControllerList.ControllerType.allocate(thisControllerList.NumControllers);
        thisControllerList.ControllerName.allocate(thisControllerList.NumControllers);
        AlphaNum = 2;
        for (CompNum = 1; CompNum <= thisControllerList.NumControllers; ++CompNum) {
            if (UtilityRoutines::SameString(AlphArray(AlphaNum), "Controller:WaterCoil") ||
                UtilityRoutines::SameString(AlphArray(AlphaNum), "Controller:OutdoorAir")) {
                thisControllerList.ControllerType(CompNum) = AlphArray(AlphaNum);
                thisControllerList.ControllerName(CompNum) = AlphArray(AlphaNum + 1);
                // loop over all previous controller lists to check if this controllers is also present on previous controllers
                for (int previousListNum = 1; previousListNum < Item; ++previousListNum) {
                    // loop over each of the controllers listed for this list
                    auto &previousList(state.dataMixedAir->ControllerLists(previousListNum));
                    for (int PreviousListControllerNum = 1; PreviousListControllerNum <= previousList.NumControllers; ++PreviousListControllerNum) {
                        if ((previousList.ControllerType(PreviousListControllerNum) == thisControllerList.ControllerType(CompNum)) &&
                            (previousList.ControllerName(PreviousListControllerNum) == thisControllerList.ControllerName(CompNum))) {
                            ShowSevereError(state, "Controller instance repeated in multiple " + CurrentModuleObject + " objects");
                            ShowContinueError(state, "Found in " + CurrentModuleObject + " = " + thisControllerList.Name);
                            ShowContinueError(state, "Also found in " + CurrentModuleObject + " = " + previousList.Name);
                            ErrorsFound = true;
                        }
                    }
                }
            } else {
                ShowSevereError(state, "For " + CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(AlphaNum));
                ShowContinueError(state, "...entered=\"" + AlphArray(AlphaNum) + "\", should be Controller:WaterCoil or Controller:OutdoorAir.");
                ErrorsFound = true;
            }
            AlphaNum += 2;
        }
    }

    CurrentModuleObject = CurrentModuleObjects(static_cast<int>(CMO::OASystem));

    state.dataAirLoop->NumOASystems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

    state.dataAirLoop->OutsideAirSys.allocate(state.dataAirLoop->NumOASystems);
    state.dataSize->OASysEqSizing.allocate(state.dataAirLoop->NumOASystems);
    state.dataMixedAir->ControllerListUniqueNames.reserve(static_cast<unsigned>(state.dataAirLoop->NumOASystems));
    state.dataMixedAir->MyOneTimeErrorFlag.dimension(state.dataAirLoop->NumOASystems, true);
    state.dataMixedAir->MyOneTimeCheckUnitarySysFlag.dimension(state.dataAirLoop->NumOASystems, true);
    state.dataMixedAir->initOASysFlag.dimension(state.dataAirLoop->NumOASystems, true);

    for (OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; ++OASysNum) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 OASysNum,
                                                                 AlphArray,
                                                                 NumAlphas,
                                                                 NumArray,
                                                                 NumNums,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);
        UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);
        state.dataAirLoop->OutsideAirSys(OASysNum).Name = AlphArray(1);
        if (!AlphArray(2).empty()) {
            GlobalNames::IntraObjUniquenessCheck(
                state, AlphArray(2), CurrentModuleObject, cAlphaFields(2), state.dataMixedAir->ControllerListUniqueNames, ErrorsFound);
        }
        ControllerListName = AlphArray(2);
        state.dataAirLoop->OutsideAirSys(OASysNum).ControllerListName = AlphArray(2);
        ComponentListName = AlphArray(3);
        state.dataAirLoop->OutsideAirSys(OASysNum).ComponentListName = AlphArray(3);
        AvailManagerListName = AlphArray(4);

        TestCompSet(state, CurrentModuleObject, AlphArray(1), "UNDEFINED", "UNDEFINED", "Air Nodes");

        if (!lAlphaBlanks(3)) {
            ListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(
                state, CurrentModuleObjects(static_cast<int>(CMO::AirLoopEqList)), ComponentListName);
            if (ListNum > 0) {
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, CurrentModuleObjects(static_cast<int>(CMO::AirLoopEqList)), ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat);
                NumInList = (NumAlphas - 1) / 2;
                state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents = NumInList;
                state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName.allocate(NumInList);
                state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType.allocate(NumInList);
                state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num.dimension(NumInList, 0);
                state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex.dimension(NumInList, 0);
                state.dataAirLoop->OutsideAirSys(OASysNum).InletNodeNum.dimension(NumInList, 0);
                state.dataAirLoop->OutsideAirSys(OASysNum).OutletNodeNum.dimension(NumInList, 0);
                state.dataAirLoop->OutsideAirSys(OASysNum).compPointer.resize(NumInList + 1, nullptr);
                for (InListNum = 1; InListNum <= NumInList; ++InListNum) {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(InListNum) = AlphArray(InListNum * 2 + 1);
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(InListNum) = AlphArray(InListNum * 2);

                    // Add equipment to component sets array
                    SetUpCompSets(state,
                                  CurrentModuleObject,
                                  state.dataAirLoop->OutsideAirSys(OASysNum).Name,
                                  state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(InListNum),
                                  state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(InListNum),
                                  "UNDEFINED",
                                  "UNDEFINED");
                }
            } else {
                ShowSevereError(
                    state, CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid " + cAlphaFields(3) + "=\"" + AlphArray(3) + "\" not found.");
                ErrorsFound = true;
            }
        } else {
            ShowSevereError(state, CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid " + cAlphaFields(3) + " is blank and must be entered.");
            ErrorsFound = true;
        }

        ListNum = 0;
        NumSimpControllers = 0;
        if (!lAlphaBlanks(2)) {
            ListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(
                state, CurrentModuleObjects(static_cast<int>(CMO::ControllerList)), ControllerListName);
            if (ListNum > 0) {
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, CurrentModuleObjects(static_cast<int>(CMO::ControllerList)), ListNum, AlphArray, NumAlphas, NumArray, NumNums, IOStat);
                NumInList = (NumAlphas - 1) / 2;
                state.dataAirLoop->OutsideAirSys(OASysNum).NumControllers = NumInList;
                state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName.allocate(NumInList);
                state.dataAirLoop->OutsideAirSys(OASysNum).ControllerType.allocate(NumInList);
                state.dataAirLoop->OutsideAirSys(OASysNum).ControllerIndex.dimension(NumInList, 0);
                for (InListNum = 1; InListNum <= NumInList; ++InListNum) {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName(InListNum) = AlphArray(InListNum * 2 + 1);
                    state.dataAirLoop->OutsideAirSys(OASysNum).ControllerType(InListNum) = AlphArray(InListNum * 2);
                    if (!UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerType(InListNum),
                                                     CurrentModuleObjects(static_cast<int>(CMO::OAController)))) {
                        ++NumSimpControllers;
                    }
                }
            } else {
                ShowSevereError(
                    state, CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid " + cAlphaFields(2) + "=\"" + AlphArray(2) + "\" not found.");
                ErrorsFound = true;
            }
        } else {
            if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:DedicatedOutdoorAirSystem") == 0) {
                ShowSevereError(state,
                                CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid " + cAlphaFields(2) + " is blank and must be entered.");
                ErrorsFound = true;
            } else {
                ShowWarningError(state,
                                 CurrentModuleObject + " = \"" + AlphArray(1) + "\": blank " + cAlphaFields(2) +
                                     " must be used with AirLoopHVAC:DedicatedOutdoorAirSystem.");
            }
        }
        state.dataAirLoop->OutsideAirSys(OASysNum).ControllerListNum = ListNum;
        state.dataAirLoop->OutsideAirSys(OASysNum).NumSimpleControllers = NumSimpControllers;

        if (!lAlphaBlanks(4)) {
            ListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(
                state, CurrentModuleObjects(static_cast<int>(CMO::SysAvailMgrList)), AvailManagerListName);
            if (ListNum <= 0) {
                ShowSevereError(
                    state, CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid " + cAlphaFields(4) + "=\"" + AlphArray(4) + "\" not found.");
                ErrorsFound = true;
            }
        }
    }

    for (OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; ++OASysNum) {
        for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents; ++CompNum) {

            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(CompNum)));

                if (SELECT_CASE_var == "OUTDOORAIR:MIXER") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::OAMixer_Num);

                    // Fan Types
                } else if (SELECT_CASE_var == "FAN:CONSTANTVOLUME") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Fan_Simple_CV);
                } else if (SELECT_CASE_var == "FAN:VARIABLEVOLUME") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Fan_Simple_VAV);
                } else if (SELECT_CASE_var == "FAN:SYSTEMMODEL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Fan_System_Object);
                    // construct fan object
                    state.dataHVACFan->fanObjs.emplace_back(
                        new HVACFan::FanSystem(state, state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum)));
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex(CompNum) = state.dataHVACFan->fanObjs.size();
                } else if (SELECT_CASE_var == "FAN:COMPONENTMODEL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Fan_ComponentModel);

                    // Coil Types
                } else if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::WaterCoil_Cooling);
                } else if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::WaterCoil_SimpleHeat);
                } else if (SELECT_CASE_var == "COIL:HEATING:STEAM") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::SteamCoil_AirHeat);
                } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::WaterCoil_DetailedCool);
                } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Coil_ElectricHeat);
                } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Coil_GasHeat);
                } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::WaterCoil_CoolingHXAsst);
                } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:DX") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::DXSystem);
                    // set the data for 100% DOAS DX cooling coil
                    CheckDXCoolingCoilInOASysExists(state, state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum));
                } else if (SELECT_CASE_var == "COILSYSTEM:HEATING:DX") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::DXHeatPumpSystem);
                } else if (SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::UnitarySystemModel);
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentIndex(CompNum) = CompNum;
                    UnitarySystems::UnitarySys thisSys;
                    state.dataAirLoop->OutsideAirSys(OASysNum).compPointer[CompNum] = thisSys.factory(
                        state, DataHVACGlobals::UnitarySys_AnyCoilType, state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(CompNum), false, 0);
                } else if (SELECT_CASE_var == "COIL:USERDEFINED") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Coil_UserDefined);
                    // Heat recovery
                } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::HeatXchngr);
                } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::HeatXchngr);
                } else if (SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::HeatXchngr);

                    // Desiccant Dehumidifier
                } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:NOFANS") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Desiccant);
                } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:SYSTEM") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Desiccant);
                    // Humidifiers: Humidifier:Steam:Electric and Humidifier:Steam:Gas
                } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:ELECTRIC") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Humidifier);
                } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:GAS") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Humidifier);

                    // Unglazed Transpired Solar Collector
                } else if (SELECT_CASE_var == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::Unglazed_SolarCollector);

                    // PVT air heater
                } else if (SELECT_CASE_var == "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::PVT_AirBased);
                    // Evaporative Cooler Types
                } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::EvapCooler);
                } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::EvapCooler);
                } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::EvapCooler);
                } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::EvapCooler);
                } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::EvapCooler);
                } else if (SELECT_CASE_var == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW") {
                    state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(CompNum) = static_cast<int>(ComponentType::VRFTerminalUnit);
                } else {
                    ShowSevereError(state,
                                    CurrentModuleObject + " = \"" + AlphArray(1) + "\" invalid Outside Air Component=\"" +
                                        state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(CompNum) + "\".");
                    ErrorsFound = true;
                }
            }
        }

        // loop through the controllers in the controller list for OA system and save the pointer to the OA controller index
        for (int OAControllerNum = 1; OAControllerNum <= state.dataAirLoop->OutsideAirSys(OASysNum).NumControllers; ++OAControllerNum) {
            if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerType(OAControllerNum),
                                            CurrentModuleObjects(static_cast<int>(CMO::OAController)))) {
                state.dataAirLoop->OutsideAirSys(OASysNum).OAControllerName =
                    state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName(OAControllerNum);
                break;
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, RoutineName + "Errors found in getting " + CurrentModuleObject + '.');
    }

    AlphArray.deallocate();
    cAlphaFields.deallocate();
    NumArray.deallocate();
    cNumericFields.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    state.dataMixedAir->GetOASysInputFlag = false;
}

void GetOAControllerInputs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       Shirey/Raustad FSEC, June 2003, Jan 2004
    //                      Mangesh Basarkar, 06/2011: Getting zone OA specifications from Design Specification Object
    //                      Tianzhen Hong, 3/2012: getting zone air distribution effectiveness and secondary recirculation
    //                       from DesignSpecification:ZoneAirDistribution objects

    // PURPOSE OF THIS SUBROUTINE
    // Input the OAController data and store it in the OAController array.
    // Input the Ventilation:Mechanical data and store it in the VentilationMechanical array.
    //  Condense Ventilation:Mechanical data array to include only unique zones specified for each instance of this object.

    // METHODOLOGY EMPLOYED:
    // Use the Get routines from the InputProcessor module.

    // Using/Aliasing
    using namespace DataDefineEquip;
    using CurveManager::GetCurveIndex;

    using NodeInputManager::GetOnlySingleNode;
    using namespace OutputReportPredefined;

    using OutAirNodeManager::CheckOutAirNodeNumber;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetOAControllerInputs: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int ZoneNum;         // zone number attached to a given air loop
    int NumNums;         // Number of real numbers returned by GetObjectItem
    int NumAlphas;       // Number of alphanumerics returned by GetObjectItem
    int OutAirNum;       // Number of Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
    int OAControllerNum; // Index to Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
    int VentMechNum;     // Number of VENTILATION:MECHANICAL objects
    int groupNum;        // Index to group in extensible VENTILATION:MECHANICAL object
    int IOStat;          // Status of GetObjectItem call
    Array1D<Real64> NumArray;
    Array1D_string AlphArray;
    std::string CurrentModuleObject; // Object type for getting and messages
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    bool ErrorsFound(false);         // Flag identifying errors found during get input
    int ZoneListNum;                 // Index to Zone List
    int MechVentZoneCount;           // Index counter for zones with mechanical ventilation
    int NumArg;                      // Number of arguments from GetObjectDefMaxArgs call
    int MaxAlphas;                   // Maximum alphas in multiple objects
    int MaxNums;                     // Maximum numbers in multiple objects

    int NumGroups; // Number of extensible input groups of the VentilationMechanical object
    int ObjIndex(0);
    int EquipListIndex(0);
    int EquipNum(0);
    int EquipListNum(0);
    int ADUNum(0);
    int jZone;
    int i;

    // Formats

    // First, call other get input routines in this module to make sure data is filled during this routine.
    if (state.dataMixedAir->GetOASysInputFlag) { // Gets input for object  first time Sim routine is called
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }
    if (state.dataMixedAir->GetOAMixerInputFlag) { // Gets input for object  first time Sim routine is called
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    FaultsManager::CheckAndReadFaults(state);

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::OAController)), NumArg, NumAlphas, NumNums);
    MaxAlphas = NumAlphas;
    MaxNums = NumNums;
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::ERVController)), NumArg, NumAlphas, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNums = max(MaxNums, NumNums);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)), NumArg, NumAlphas, NumNums);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNums = max(MaxNums, NumNums);

    AlphArray.allocate(MaxAlphas);
    NumArray.dimension(MaxNums, 0.0);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNums, true);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNums);

    // Count OAcontrollers and ERVcontrollers and allocate arrays
    AllocateOAControllers(state);

    // If there are ERV controllers, they have been filled before now NumOAControllers includes the count of NumERVControllers
    if (state.dataMixedAir->NumOAControllers > state.dataMixedAir->NumERVControllers) {
        CurrentModuleObject = CurrentModuleObjects(static_cast<int>(CMO::OAController));
        int currentOAControllerNum = 0;
        for (OutAirNum = state.dataMixedAir->NumERVControllers + 1; OutAirNum <= state.dataMixedAir->NumOAControllers; ++OutAirNum) {
            ++currentOAControllerNum;
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     currentOAControllerNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            GlobalNames::VerifyUniqueInterObjectName(
                state, state.dataMixedAir->OAControllerUniqueNames, AlphArray(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);

            ProcessOAControllerInputs(state,
                                      CurrentModuleObject,
                                      OutAirNum,
                                      AlphArray,
                                      NumAlphas,
                                      NumArray,
                                      NumNums,
                                      lNumericBlanks,
                                      lAlphaBlanks,
                                      cAlphaFields,
                                      cNumericFields,
                                      ErrorsFound);

            // add applicable faults identifier to avoid string comparison at each time step
            //  loop through each fault for each OA controller and determine economizer faultys
            for (i = 1; i <= state.dataFaultsMgr->NumFaultyEconomizer; ++i) {
                if (state.dataFaultsMgr->FaultsEconomizer(i).ControllerTypeEnum != iController_AirEconomizer) continue;
                if (UtilityRoutines::SameString(state.dataMixedAir->OAController(OutAirNum).Name,
                                                state.dataFaultsMgr->FaultsEconomizer(i).ControllerName)) {
                    state.dataFaultsMgr->FaultsEconomizer(i).ControllerID = OutAirNum;
                    ++state.dataMixedAir->OAController(OutAirNum).NumFaultyEconomizer;
                }
            }
            //  loop through each fault for each OA controller to determine faulty counts
            state.dataMixedAir->OAController(OutAirNum).EconmizerFaultNum.allocate(state.dataMixedAir->OAController(OutAirNum).NumFaultyEconomizer);
            if (state.dataMixedAir->OAController(OutAirNum).NumFaultyEconomizer > 0) {
                for (int j = 0, i = 1; i <= state.dataFaultsMgr->NumFaultyEconomizer; ++i) {
                    if (state.dataFaultsMgr->FaultsEconomizer(i).ControllerTypeEnum != iController_AirEconomizer) continue;
                    if (UtilityRoutines::SameString(state.dataMixedAir->OAController(OutAirNum).Name,
                                                    state.dataFaultsMgr->FaultsEconomizer(i).ControllerName)) {
                        state.dataMixedAir->OAController(OutAirNum).EconmizerFaultNum(++j) = i;
                    }
                }
            }
        } // LOOP FOR OutAirNum

        if (ErrorsFound) {
            AlphArray.deallocate();
            NumArray.deallocate();
            lNumericBlanks.deallocate();
            lAlphaBlanks.deallocate();
            cAlphaFields.deallocate();
            cNumericFields.deallocate();
            ShowFatalError(state, RoutineName + "Errors found in getting " + CurrentModuleObject + " inputs.");
        }
    }

    state.dataMixedAir->GetOAControllerInputFlag = false;

    // Process Controller:MechanicalVentilation objects
    CurrentModuleObject = CurrentModuleObjects(static_cast<int>(CMO::MechVentilation));
    state.dataMixedAir->NumVentMechControllers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    if (state.dataMixedAir->NumVentMechControllers > 0) {
        state.dataMixedAir->VentilationMechanical.allocate(state.dataMixedAir->NumVentMechControllers);
        for (VentMechNum = 1; VentMechNum <= state.dataMixedAir->NumVentMechControllers; ++VentMechNum) {
            auto &thisVentilationMechanical(state.dataMixedAir->VentilationMechanical(VentMechNum));
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     VentMechNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            MechVentZoneCount = 0;

            NumGroups = (NumAlphas + NumNums - 5) / 3;
            if (mod((NumAlphas + NumNums - 5), 3) != 0) ++NumGroups;
            thisVentilationMechanical.Name = AlphArray(1);

            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

            thisVentilationMechanical.SchName = AlphArray(2);
            if (lAlphaBlanks(2)) {
                thisVentilationMechanical.SchPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                thisVentilationMechanical.SchPtr = GetScheduleIndex(state, AlphArray(2)); // convert schedule name to pointer
                if (thisVentilationMechanical.SchPtr == 0) {
                    ShowSevereError(
                        state, CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(2) + "=\"" + AlphArray(2) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Adding new flag for DCV
            if (UtilityRoutines::SameString(AlphArray(3), "Yes")) {
                thisVentilationMechanical.DCVFlag = true;
            } else if (UtilityRoutines::SameString(AlphArray(3), "No") || lAlphaBlanks(3)) {
                thisVentilationMechanical.DCVFlag = false;
            } else {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid value " + cAlphaFields(3) + "=\"" + AlphArray(3) + "\".");
                ShowContinueError(state, "...Valid values are \"Yes\" or \"No\".");
                ErrorsFound = true;
            }

            // System outdoor air method
            {
                auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(AlphArray(4)));
                if (SELECT_CASE_var == "ZONESUM") { // Simplify sum the zone OA flow rates
                    thisVentilationMechanical.SystemOAMethod = SOAM_ZoneSum;
                } else if ((SELECT_CASE_var == "VENTILATIONRATEPROCEDURE")) { // Ventilation Rate Procedure based on ASHRAE Standard 62.1-2007
                    thisVentilationMechanical.SystemOAMethod = SOAM_VRP;
                } else if ((SELECT_CASE_var == "INDOORAIRQUALITYPROCEDURE")) { // Indoor Air Quality Procedure based on ASHRAE Standard 62.1-2007
                    thisVentilationMechanical.SystemOAMethod = SOAM_IAQP;
                    if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires CO2 simulation.");
                        ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var ==
                           "PROPORTIONALCONTROLBASEDONOCCUPANCYSCHEDULE") { // Proportional Control based on ASHRAE Standard 62.1-2004
                    thisVentilationMechanical.SystemOAMethod = SOAM_ProportionalControlSchOcc;
                    if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires CO2 simulation.");
                        ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var ==
                           "PROPORTIONALCONTROLBASEDONDESIGNOCCUPANCY") { // Proportional Control based on ASHRAE Standard 62.1-2004
                    thisVentilationMechanical.SystemOAMethod = SOAM_ProportionalControlDesOcc;
                    if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires CO2 simulation.");
                        ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "PROPORTIONALCONTROLBASEDONDESIGNOARATE") { // Proportional Control based on design OA rate
                    thisVentilationMechanical.SystemOAMethod = SOAM_ProportionalControlDesOARate;
                    if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires CO2 simulation.");
                        ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var ==
                           "INDOORAIRQUALITYPROCEDUREGENERICCONTAMINANT") { // Indoor Air Quality Procedure based on generic contaminant setpoint
                    thisVentilationMechanical.SystemOAMethod = SOAM_IAQPGC;
                    if (!state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires generic contaminant simulation.");
                        ShowContinueError(state,
                                          "The choice must be Yes for the field Generic Contaminant Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else if (SELECT_CASE_var == "INDOORAIRQUALITYPROCEDURECOMBINED") { // Indoor Air Quality Procedure based on both generic
                                                                                     // contaminant and CO2 setpoint
                    thisVentilationMechanical.SystemOAMethod = SOAM_IAQPCOM;
                    if (!state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires generic contaminant simulation.");
                        ShowContinueError(state,
                                          "The choice must be Yes for the field Generic Contaminant Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                    if (!state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        ShowSevereError(state,
                                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" valid " + cAlphaFields(2) + "=\"" + AlphArray(2) +
                                            "\" requires CO2 simulation.");
                        ShowContinueError(state, "The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance");
                        ErrorsFound = true;
                    }
                } else { // If specified incorrectly, show errors
                    thisVentilationMechanical.SystemOAMethod = SOAM_ZoneSum;
                    ShowWarningError(state,
                                     CurrentModuleObject + "=\"" + AlphArray(1) + "\" incorrect specification for " + cAlphaFields(4) +
                                         ", the ZoneSum method will be used.");
                    // ErrorsFound=.TRUE.
                }
            }

            // Zone maximum outdoor air fraction
            thisVentilationMechanical.ZoneMaxOAFraction = NumArray(1);

            state.dataMixedAir->VentMechZoneOrListName.allocate(NumGroups);
            state.dataMixedAir->DesignSpecOAObjName.allocate(NumGroups);
            state.dataMixedAir->DesignSpecOAObjIndex.dimension(NumGroups, 0);
            state.dataMixedAir->DesignSpecZoneADObjName.allocate(NumGroups);
            state.dataMixedAir->DesignSpecZoneADObjIndex.dimension(NumGroups, 0);

            //   First time through find the total number of zones requiring mechanical ventilation
            //   May include duplicate zones. Will check for duplicate zones further down in this subroutine.
            for (groupNum = 1; groupNum <= NumGroups; ++groupNum) {
                state.dataMixedAir->VentMechZoneOrListName(groupNum) = AlphArray((groupNum - 1) * 3 + 5);

                //     Getting OA details from design specification OA object
                if (!lAlphaBlanks((groupNum - 1) * 3 + 6)) {
                    state.dataMixedAir->DesignSpecOAObjName(groupNum) = AlphArray((groupNum - 1) * 3 + 6);
                    ObjIndex = UtilityRoutines::FindItemInList(state.dataMixedAir->DesignSpecOAObjName(groupNum), state.dataSize->OARequirements);
                    state.dataMixedAir->DesignSpecOAObjIndex(groupNum) = ObjIndex;

                    if (ObjIndex == 0) {
                        ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", invalid");
                        ShowContinueError(state,
                                          "... not found " + cAlphaFields((groupNum - 1) * 3 + 6) + "=\"" +
                                              state.dataMixedAir->DesignSpecOAObjName(groupNum) + "\".");
                        ErrorsFound = true;
                    }
                }

                // Get zone air distribution details from design specification Zone Air Distribution object
                if (!lAlphaBlanks((groupNum - 1) * 3 + 7)) {
                    state.dataMixedAir->DesignSpecZoneADObjName(groupNum) = AlphArray((groupNum - 1) * 3 + 7);
                    ObjIndex =
                        UtilityRoutines::FindItemInList(state.dataMixedAir->DesignSpecZoneADObjName(groupNum), state.dataSize->ZoneAirDistribution);
                    state.dataMixedAir->DesignSpecZoneADObjIndex(groupNum) = ObjIndex;

                    if (ObjIndex == 0) {
                        // Cannot find the design specification Zone Air Distribution object
                        ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", invalid");
                        ShowContinueError(state,
                                          "... not found " + cAlphaFields((groupNum - 1) * 3 + 7) + "=\"" +
                                              state.dataMixedAir->DesignSpecZoneADObjName(groupNum) + "\".");
                        ErrorsFound = true;
                    }
                }

                ZoneNum = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum), state.dataHeatBal->Zone);
                if (ZoneNum > 0) {
                    ++MechVentZoneCount;
                } else {
                    ZoneListNum = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum), state.dataHeatBal->ZoneList);
                    if (ZoneListNum > 0) {
                        MechVentZoneCount += state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                    } else {
                        ShowWarningError(
                            state, CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields((groupNum - 1) * 3 + 5) + " not found.");
                        ShowContinueError(
                            state, "Missing " + cAlphaFields((groupNum - 1) * 3 + 5) + " = " + state.dataMixedAir->VentMechZoneOrListName(groupNum));
                        ErrorsFound = true;
                    }
                }
            }

            thisVentilationMechanical.NumofVentMechZones = MechVentZoneCount;

            // Now allocate and store unique zone and associated ventilation rate information
            thisVentilationMechanical.VentMechZone.dimension(MechVentZoneCount, 0);
            thisVentilationMechanical.VentMechZoneName.dimension(MechVentZoneCount);
            thisVentilationMechanical.ZoneDesignSpecOAObjName.dimension(MechVentZoneCount);
            thisVentilationMechanical.ZoneDesignSpecOAObjIndex.dimension(MechVentZoneCount, 0);
            thisVentilationMechanical.ZoneOAAreaRate.dimension(MechVentZoneCount, 0.0);
            thisVentilationMechanical.ZoneOAPeopleRate.dimension(MechVentZoneCount, 0.0);
            thisVentilationMechanical.ZoneOAFlowRate.dimension(MechVentZoneCount, 0.0);
            thisVentilationMechanical.ZoneOAACHRate.dimension(MechVentZoneCount, 0.0);
            thisVentilationMechanical.ZoneOAFlowMethod.dimension(MechVentZoneCount, 0);
            thisVentilationMechanical.ZoneOASchPtr.dimension(MechVentZoneCount, 0);
            thisVentilationMechanical.OAPropCtlMinRateSchPtr.dimension(MechVentZoneCount, 0);

            // added for new DCV, 2/12/2009
            thisVentilationMechanical.ZoneADEffCooling.dimension(MechVentZoneCount, 1.0);
            // Zone air distribution effectiveness in heating mode
            thisVentilationMechanical.ZoneADEffHeating.dimension(MechVentZoneCount, 1.0);
            // Indices to the zone air distribution effectiveness schedules
            thisVentilationMechanical.ZoneADEffSchPtr.dimension(MechVentZoneCount, 0);
            // Zone air secondary recirculation ratio, added 3/2012
            thisVentilationMechanical.ZoneSecondaryRecirculation.dimension(MechVentZoneCount, 0.0);
            thisVentilationMechanical.ZoneDesignSpecADObjName.allocate(MechVentZoneCount);
            thisVentilationMechanical.ZoneDesignSpecADObjIndex.dimension(MechVentZoneCount, 0);

            MechVentZoneCount = 0;

            //   Loop through zone names and list of zone names, remove duplicate zones, and store designspec names and indexes
            for (groupNum = 1; groupNum <= NumGroups; ++groupNum) {
                ZoneNum = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum), state.dataHeatBal->Zone);
                if (ZoneNum > 0) {
                    if (any_eq(thisVentilationMechanical.VentMechZone, ZoneNum)) {
                        //          Disregard duplicate zone names, show warning and do not store data for this zone
                        ShowWarningError(state,
                                         "Zone name = " + state.dataMixedAir->VentMechZoneOrListName(groupNum) + " for " + CurrentModuleObject +
                                             " object = " + thisVentilationMechanical.Name);
                        ShowContinueError(state, "is specified more than once. The first ventilation values specified for this zone will be used");
                        ShowContinueError(state, "and the rest will be ignored. Simulation will continue..");
                    } else {
                        //          Store unique zone names
                        ++MechVentZoneCount;
                        thisVentilationMechanical.VentMechZone(MechVentZoneCount) = ZoneNum;
                        thisVentilationMechanical.VentMechZoneName(MechVentZoneCount) = state.dataHeatBal->Zone(ZoneNum).Name;

                        // Populating new temp array to hold design spec OA object for each zone
                        if (state.dataMixedAir->DesignSpecOAObjIndex(groupNum) > 0) {
                            thisVentilationMechanical.ZoneDesignSpecOAObjName(MechVentZoneCount) = state.dataMixedAir->DesignSpecOAObjName(groupNum);
                            thisVentilationMechanical.ZoneDesignSpecOAObjIndex(MechVentZoneCount) =
                                state.dataMixedAir->DesignSpecOAObjIndex(groupNum);
                        } else {
                            if (state.dataGlobal->DoZoneSizing) {
                                ObjIndex = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum),
                                                                           state.dataSize->ZoneSizingInput,
                                                                           &ZoneSizingInputData::ZoneName);
                                if (ObjIndex > 0) {
                                    thisVentilationMechanical.ZoneDesignSpecOAObjName(MechVentZoneCount) =
                                        state.dataSize->ZoneSizingInput(ObjIndex).DesignSpecOAObjName;
                                    thisVentilationMechanical.ZoneDesignSpecOAObjIndex(MechVentZoneCount) =
                                        state.dataSize->ZoneSizingInput(ObjIndex).ZoneDesignSpecOAIndex;
                                }
                            }
                        }
                        // Zone Air Distribution inputs
                        if (state.dataMixedAir->DesignSpecZoneADObjIndex(groupNum) > 0) {
                            // new DCV inputs
                            thisVentilationMechanical.ZoneDesignSpecADObjName(MechVentZoneCount) =
                                state.dataMixedAir->DesignSpecZoneADObjName(groupNum);
                            thisVentilationMechanical.ZoneDesignSpecADObjIndex(MechVentZoneCount) =
                                state.dataMixedAir->DesignSpecZoneADObjIndex(groupNum);
                        } else {
                            if (state.dataGlobal->DoZoneSizing) {
                                ObjIndex = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum),
                                                                           state.dataSize->ZoneSizingInput,
                                                                           &ZoneSizingInputData::ZoneName);
                                if (ObjIndex > 0) {
                                    thisVentilationMechanical.ZoneDesignSpecADObjName(MechVentZoneCount) =
                                        state.dataSize->ZoneSizingInput(ObjIndex).ZoneAirDistEffObjName;
                                    thisVentilationMechanical.ZoneDesignSpecADObjIndex(MechVentZoneCount) =
                                        state.dataSize->ZoneSizingInput(ObjIndex).ZoneAirDistributionIndex;
                                }
                            }
                        }
                    }
                } else {
                    //       Not a zone name, must be a zone list
                    ZoneListNum = UtilityRoutines::FindItemInList(state.dataMixedAir->VentMechZoneOrListName(groupNum), state.dataHeatBal->ZoneList);
                    if (ZoneListNum > 0) {
                        for (int ScanZoneListNum = 1; ScanZoneListNum <= state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones; ++ScanZoneListNum) {
                            ObjIndex = 0;
                            // check to make sure zone name is unique (not listed more than once)...
                            ZoneNum = state.dataHeatBal->ZoneList(ZoneListNum).Zone(ScanZoneListNum);
                            if (any_eq(thisVentilationMechanical.VentMechZone, ZoneNum)) {
                                //             Disregard duplicate zone names, show warning and do not store data for this zone
                                ShowWarningError(state,
                                                 "Zone name = " + state.dataHeatBal->Zone(ZoneNum).Name +
                                                     " in ZoneList = " + state.dataMixedAir->VentMechZoneOrListName(groupNum) + " for " +
                                                     CurrentModuleObject + " object = " + thisVentilationMechanical.Name);
                                ShowContinueError(state, "is a duplicate. The first ventilation values specified for this zone will be used ");
                                ShowContinueError(state, "and the rest will be ignored. The simulation will continue...");
                            } else {
                                //           Store data for each zone name from zone list (duplicate zone names accounted for in
                                //           HeatBalanceManager)
                                ++MechVentZoneCount;
                                thisVentilationMechanical.VentMechZone(MechVentZoneCount) = ZoneNum;
                                thisVentilationMechanical.VentMechZoneName(MechVentZoneCount) = state.dataHeatBal->Zone(ZoneNum).Name;
                                // Populating new temp array to hold design spec OA object for each zone
                                if (state.dataMixedAir->DesignSpecOAObjIndex(groupNum) > 0) {
                                    thisVentilationMechanical.ZoneDesignSpecOAObjName(MechVentZoneCount) =
                                        state.dataMixedAir->DesignSpecOAObjName(groupNum);
                                    thisVentilationMechanical.ZoneDesignSpecOAObjIndex(MechVentZoneCount) =
                                        state.dataMixedAir->DesignSpecOAObjIndex(groupNum);
                                } else {
                                    if (state.dataGlobal->DoZoneSizing) {
                                        ObjIndex = UtilityRoutines::FindItemInList(
                                            state.dataHeatBal->Zone(ZoneNum).Name, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
                                        if (ObjIndex > 0) {
                                            thisVentilationMechanical.ZoneDesignSpecOAObjName(MechVentZoneCount) =
                                                state.dataSize->ZoneSizingInput(ObjIndex).DesignSpecOAObjName;
                                            thisVentilationMechanical.ZoneDesignSpecOAObjIndex(MechVentZoneCount) =
                                                state.dataSize->ZoneSizingInput(ObjIndex).ZoneDesignSpecOAIndex;
                                        }
                                    }
                                }

                                if (state.dataMixedAir->DesignSpecZoneADObjIndex(groupNum) > 0) {
                                    // new DCV inputs
                                    thisVentilationMechanical.ZoneDesignSpecADObjName(MechVentZoneCount) =
                                        state.dataMixedAir->DesignSpecZoneADObjName(groupNum);
                                    thisVentilationMechanical.ZoneDesignSpecADObjIndex(MechVentZoneCount) =
                                        state.dataMixedAir->DesignSpecZoneADObjIndex(groupNum);
                                } else {
                                    if (state.dataGlobal->DoZoneSizing) {
                                        ObjIndex = UtilityRoutines::FindItemInList(
                                            state.dataHeatBal->Zone(ZoneNum).Name, state.dataSize->ZoneSizingInput, &ZoneSizingInputData::ZoneName);
                                        if (ObjIndex > 0) {
                                            thisVentilationMechanical.ZoneDesignSpecADObjName(MechVentZoneCount) =
                                                state.dataSize->ZoneSizingInput(ObjIndex).ZoneAirDistEffObjName;
                                            thisVentilationMechanical.ZoneDesignSpecADObjIndex(MechVentZoneCount) =
                                                state.dataSize->ZoneSizingInput(ObjIndex).ZoneAirDistributionIndex;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            //   Overwrite previous number of zones with number that does not include duplicates
            thisVentilationMechanical.NumofVentMechZones = MechVentZoneCount;

            // Loop over zones and fill OA and AD specs, if none were found, use defaults
            for (int ventMechZoneNum = 1; ventMechZoneNum <= MechVentZoneCount; ++ventMechZoneNum) {
                int zoneOAReqObjIndex = thisVentilationMechanical.ZoneDesignSpecOAObjIndex(ventMechZoneNum);
                if (zoneOAReqObjIndex > 0) {
                    auto const &curOARequirements(state.dataSize->OARequirements(zoneOAReqObjIndex));
                    thisVentilationMechanical.ZoneOAAreaRate(ventMechZoneNum) = curOARequirements.OAFlowPerArea;
                    thisVentilationMechanical.ZoneOAPeopleRate(ventMechZoneNum) = curOARequirements.OAFlowPerPerson;
                    thisVentilationMechanical.ZoneOAFlowRate(ventMechZoneNum) = curOARequirements.OAFlowPerZone;
                    thisVentilationMechanical.ZoneOAACHRate(ventMechZoneNum) = curOARequirements.OAFlowACH;
                    thisVentilationMechanical.ZoneOAFlowMethod(ventMechZoneNum) = curOARequirements.OAFlowMethod;
                    thisVentilationMechanical.ZoneOASchPtr(ventMechZoneNum) = curOARequirements.OAFlowFracSchPtr;
                    thisVentilationMechanical.OAPropCtlMinRateSchPtr(ventMechZoneNum) = curOARequirements.OAPropCtlMinRateSchPtr;
                    if (thisVentilationMechanical.SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                        if (thisVentilationMechanical.ZoneOAPeopleRate(ventMechZoneNum) == 0.0 &&
                            thisVentilationMechanical.ZoneOAAreaRate(ventMechZoneNum) == 0.0) {
                            ShowSevereError(state,
                                            RoutineName + CurrentModuleObject + "=\"" + thisVentilationMechanical.Name +
                                                "\", invalid input with System Outdoor Air Method = ProportionalControlBasedOnDesignOARate.");
                            ShowContinueError(state,
                                              " The values of Outdoor Air Flow per Person and Outdoor Air Flow per Zone Floor Area in the same "
                                              "object can not be zero.");
                            ErrorsFound = true;
                        }
                    }
                } else { // use defaults
                    thisVentilationMechanical.ZoneOAAreaRate(ventMechZoneNum) = 0.0;
                    // since this is case with no DesSpcOA object, cannot determine the method and default would be Flow/Person which should
                    // default to this flow rate
                    thisVentilationMechanical.ZoneOAPeopleRate(ventMechZoneNum) = 0.00944;
                    thisVentilationMechanical.ZoneOAFlowRate(ventMechZoneNum) = 0.0;
                    thisVentilationMechanical.ZoneOAACHRate = 0.0;
                    thisVentilationMechanical.ZoneOAFlowMethod(ventMechZoneNum) = OAFlowPPer;
                    thisVentilationMechanical.ZoneOASchPtr(ventMechZoneNum) = DataGlobalConstants::ScheduleAlwaysOn;
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + thisVentilationMechanical.Name);
                    ShowContinueError(state,
                                      "Cannot locate a matching DesignSpecification:OutdoorAir object for Zone=\"" +
                                          thisVentilationMechanical.VentMechZoneName(ventMechZoneNum) + "\".");
                    ShowContinueError(state, "Using default OA of 0.00944 m3/s-person and 0.0 m3/s-m2.");
                }
                int zoneAirDistObjIndex = thisVentilationMechanical.ZoneDesignSpecADObjIndex(ventMechZoneNum);
                if (zoneAirDistObjIndex > 0) {
                    auto const &curZoneAirDistribution(state.dataSize->ZoneAirDistribution(zoneAirDistObjIndex));
                    thisVentilationMechanical.ZoneADEffCooling(ventMechZoneNum) = curZoneAirDistribution.ZoneADEffCooling;
                    thisVentilationMechanical.ZoneADEffHeating(ventMechZoneNum) = curZoneAirDistribution.ZoneADEffHeating;
                    thisVentilationMechanical.ZoneADEffSchPtr(ventMechZoneNum) = curZoneAirDistribution.ZoneADEffSchPtr;
                    thisVentilationMechanical.ZoneSecondaryRecirculation(ventMechZoneNum) = curZoneAirDistribution.ZoneSecondaryRecirculation;
                } else { // use defaults
                    thisVentilationMechanical.ZoneADEffCooling(ventMechZoneNum) = 1.0;
                    thisVentilationMechanical.ZoneADEffHeating(ventMechZoneNum) = 1.0;
                    thisVentilationMechanical.ZoneSecondaryRecirculation(ventMechZoneNum) = 0.0;
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + thisVentilationMechanical.Name);
                    ShowContinueError(state,
                                      "Cannot locate a matching DesignSpecification:ZoneAirDistribution object for Zone=\"" +
                                          thisVentilationMechanical.VentMechZoneName(ventMechZoneNum) + "\".");
                    ShowContinueError(state, "Using default zone air distribution effectiveness of 1.0 for heating and cooling.");
                }
            }
            state.dataMixedAir->VentMechZoneOrListName.deallocate();
            state.dataMixedAir->DesignSpecOAObjName.deallocate();
            state.dataMixedAir->DesignSpecOAObjIndex.deallocate();
            state.dataMixedAir->DesignSpecZoneADObjName.deallocate();
            state.dataMixedAir->DesignSpecZoneADObjIndex.deallocate();
        }

        for (VentMechNum = 1; VentMechNum <= state.dataMixedAir->NumVentMechControllers; ++VentMechNum) {
            auto &thisVentilationMechanical(state.dataMixedAir->VentilationMechanical(VentMechNum));
            for (jZone = 1; jZone <= thisVentilationMechanical.NumofVentMechZones; ++jZone) {
                if (thisVentilationMechanical.SystemOAMethod == SOAM_ProportionalControlSchOcc) {
                    if (thisVentilationMechanical.ZoneOAACHRate(jZone) > 0.0 || thisVentilationMechanical.ZoneOAFlowRate(jZone) > 0.0) {
                        ShowWarningError(state,
                                         CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", inappropriate outdoor air method");
                        ShowContinueError(state,
                                          "Inappropriate method for Design Specification Outdoor Air Object Name=\"" +
                                              thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone) + "\".");
                        ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                        ShowContinueError(state,
                                          "Since System Outdoor Air Method= ProportionalControlBasedOnOccupancySchedule\", AirChanges/Hour or "
                                          "Flow/Zone outdoor air methods are not valid. Simulation continues.... ");
                    }
                }
                if (thisVentilationMechanical.SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                    if (thisVentilationMechanical.ZoneOAACHRate(jZone) > 0.0 || thisVentilationMechanical.ZoneOAFlowRate(jZone) > 0.0) {
                        ShowWarningError(state,
                                         CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", inappropriate outdoor air method");
                        ShowContinueError(state,
                                          "Inappropriate method for Design Specification Outdoor Air Object Name=\"" +
                                              thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone) + "\".");
                        ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                        ShowContinueError(state,
                                          "Since System Outdoor Air Method= ProportionalControlBasedOnDesignOccupancy\", AirChanges/Hour or "
                                          "Flow/Zone outdoor air methods are not valid. Simulation continues.... ");
                    }
                }

                // Error check to see if a single duct air terminal is assigned to a zone that has zone secondary recirculation
                if (thisVentilationMechanical.ZoneSecondaryRecirculation(jZone) > 0.0) {
                    ZoneNum = thisVentilationMechanical.VentMechZone(jZone);
                    if (ZoneNum > 0) {
                        EquipListIndex = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).EquipListIndex;
                        if (EquipListIndex > 0) {
                            for (EquipListNum = 1; EquipListNum <= state.dataZoneEquip->NumOfZoneEquipLists; ++EquipListNum) {
                                if (EquipListNum == EquipListIndex) {
                                    for (EquipNum = 1; EquipNum <= state.dataZoneEquip->ZoneEquipList(EquipListNum).NumOfEquipTypes; ++EquipNum) {
                                        if (UtilityRoutines::SameString(state.dataZoneEquip->ZoneEquipList(EquipListNum).EquipType(EquipNum),
                                                                        "ZONEHVAC:AIRDISTRIBUTIONUNIT")) {
                                            for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
                                                if (UtilityRoutines::SameString(state.dataZoneEquip->ZoneEquipList(EquipListNum).EquipName(EquipNum),
                                                                                state.dataDefineEquipment->AirDistUnit(ADUNum).Name)) {
                                                    if ((state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolNoReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVNoReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctVAVReheatVSFan) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctCBVAVNoReheat) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolCooledBeam) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::SingleDuctConstVolFourPipeBeam) ||
                                                        (state.dataDefineEquipment->AirDistUnit(ADUNum).EquipType_Num(EquipNum) ==
                                                         DataDefineEquip::iZnAirLoopEquipType::DualDuctVAVOutdoorAir)) {
                                                        ShowWarningError(state,
                                                                         CurrentModuleObject + "=\"" + thisVentilationMechanical.Name +
                                                                             "\", inappropriate use of Zone secondary recirculation");
                                                        ShowContinueError(state,
                                                                          "A zone secondary recirculation fraction is specified for zone served by ");
                                                        ShowContinueError(state,
                                                                          "...terminal unit \"" +
                                                                              state.dataDefineEquipment->AirDistUnit(ADUNum).Name +
                                                                              "\" , that indicates a single path system");
                                                        ShowContinueError(state,
                                                                          "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                                                        ShowContinueError(state, "...The zone secondary recirculation for that zone was set to 0.0");
                                                        thisVentilationMechanical.ZoneSecondaryRecirculation(jZone) = 0.0;
                                                    }
                                                    goto EquipLoop_exit;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        EquipLoop_exit:;
                        }
                    }
                }
                if (thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone).empty()) {
                    ShowSevereError(state,
                                    CurrentModuleObject + "=\"" + thisVentilationMechanical.Name +
                                        "\", Design Specification Outdoor Air Object Name blank");
                    ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                    ShowContinueError(state, "This field either needs to be filled in in this object or Sizing:Zone object.");
                    ShowContinueError(state, "For this run, default values for these fields will be used.");
                }
                if (thisVentilationMechanical.ZoneOAPeopleRate(jZone) <= 0.0 && thisVentilationMechanical.DCVFlag) {
                    ShowWarningError(state, CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", Zone OA/person rate");
                    ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                    ShowContinueError(state,
                                      "Zone outside air per person rate not set in Design Specification Outdoor Air Object=\"" +
                                          thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone) + "\".");
                }

                if (thisVentilationMechanical.ZoneOAAreaRate(jZone) < 0.0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", invalid Outdoor Air flow per area");
                    ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                    ShowContinueError(state,
                                      "invalid Outdoor Air flow per area specified in object=\"" +
                                          thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone) + "\". Value must be >= 0.0.");
                    ErrorsFound = true;
                }
                if (thisVentilationMechanical.ZoneOAPeopleRate(jZone) < 0.0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisVentilationMechanical.Name + "\", invalid Outdoor Air flow per person");
                    ShowContinueError(state, "For Zone=\"" + thisVentilationMechanical.VentMechZoneName(jZone) + "\".");
                    ShowContinueError(state,
                                      "invalid Outdoor Air flow per person specified in object \"" +
                                          thisVentilationMechanical.ZoneDesignSpecOAObjName(jZone) + "\". Value must be >= 0.0.");
                    ErrorsFound = true;
                }
            }
        }

        // Link OA controller object with mechanical ventilation object
        for (OAControllerNum = 1; OAControllerNum <= state.dataMixedAir->NumOAControllers; ++OAControllerNum) {
            state.dataMixedAir->OAController(OAControllerNum).VentMechObjectNum = UtilityRoutines::FindItemInList(
                state.dataMixedAir->OAController(OAControllerNum).VentilationMechanicalName, state.dataMixedAir->VentilationMechanical);
            if (state.dataMixedAir->OAController(OAControllerNum).VentMechObjectNum == 0 &&
                !state.dataMixedAir->OAController(OAControllerNum).VentilationMechanicalName.empty()) {
                ShowSevereError(state,
                                CurrentModuleObject + "=\"" + state.dataMixedAir->OAController(OAControllerNum).VentilationMechanicalName +
                                    "\", non-match to Controller:OutdoorAir");
                ShowContinueError(state,
                                  "Invalid specified in Controller:OutdoorAir object = " + state.dataMixedAir->OAController(OAControllerNum).Name);
                ShowContinueError(state,
                                  CurrentModuleObject + " object name must match the " + CurrentModuleObject +
                                      " object name specified in Controller:OutdoorAir.");
                ErrorsFound = true;
            }
        }

        // write to .eio file
        static constexpr auto Format_700("!<Controller:MechanicalVentilation>,Name,Availability Schedule Name,Demand Controlled Ventilation "
                                         "{Yes/No},System Outdoor Air Method,Zone Maximum Outdoor Air Fraction,Number of Zones,Zone Name,DSOA "
                                         "Name,DSZAD Name");
        print(state.files.eio, "{}\n", Format_700);
        for (VentMechNum = 1; VentMechNum <= state.dataMixedAir->NumVentMechControllers; ++VentMechNum) {
            print(state.files.eio,
                  " Controller:MechanicalVentilation,{},{},",
                  state.dataMixedAir->VentilationMechanical(VentMechNum).Name,
                  state.dataMixedAir->VentilationMechanical(VentMechNum).SchName);

            if (state.dataMixedAir->VentilationMechanical(VentMechNum).DCVFlag) {
                print(state.files.eio, "Yes,");
            } else {
                print(state.files.eio, "No,");
            }

            if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_ZoneSum) {
                print(state.files.eio, "ZoneSum,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_VRP) {
                print(state.files.eio, "VentilationRateProcedure,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_IAQP) {
                print(state.files.eio, "IndoorAirQualityProcedure,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_ProportionalControlSchOcc) {
                print(state.files.eio, "ProportionalControlBasedOnOccupancySchedule,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                print(state.files.eio, "ProportionalControlBasedOnDesignOccupancy,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                print(state.files.eio, "ProportionalControlBasedOnDesignOARate,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_IAQPGC) {
                print(state.files.eio, "IndoorAirQualityGenericContaminant,");
            } else if (state.dataMixedAir->VentilationMechanical(VentMechNum).SystemOAMethod == SOAM_IAQPCOM) {
                print(state.files.eio, "IndoorAirQualityProcedureCombined,");
            } else {
                print(state.files.eio, "Invalid/Unknown,");
            }

            print(state.files.eio, "{:.2R},", state.dataMixedAir->VentilationMechanical(VentMechNum).ZoneMaxOAFraction);
            print(state.files.eio, "{},", state.dataMixedAir->VentilationMechanical(VentMechNum).NumofVentMechZones);

            for (jZone = 1; jZone <= state.dataMixedAir->VentilationMechanical(VentMechNum).NumofVentMechZones; ++jZone) {
                if (jZone < state.dataMixedAir->VentilationMechanical(VentMechNum).NumofVentMechZones) {
                    print(state.files.eio,
                          "{},{},{},",
                          state.dataHeatBal->Zone(state.dataMixedAir->VentilationMechanical(VentMechNum).VentMechZone(jZone)).Name,
                          state.dataMixedAir->VentilationMechanical(VentMechNum).ZoneDesignSpecOAObjName(jZone),
                          state.dataMixedAir->VentilationMechanical(VentMechNum).ZoneDesignSpecADObjName(jZone));
                } else {
                    print(state.files.eio,
                          "{},{},{}\n",
                          state.dataHeatBal->Zone(state.dataMixedAir->VentilationMechanical(VentMechNum).VentMechZone(jZone)).Name,
                          state.dataMixedAir->VentilationMechanical(VentMechNum).ZoneDesignSpecOAObjName(jZone),
                          state.dataMixedAir->VentilationMechanical(VentMechNum).ZoneDesignSpecADObjName(jZone));
                }
            }
        }

    } // Number of Mechanical Ventilation Objects > 0

    AlphArray.deallocate();
    NumArray.deallocate();
    lNumericBlanks.deallocate();
    lAlphaBlanks.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, RoutineName + "Errors found when getting " + CurrentModuleObject + " inputs.");
    }
}

void AllocateOAControllers(EnergyPlusData &state)
{

    // PURPOSE OF THIS SUBROUTINE:
    // Allocate the OA controller arrays which are shared by Controller:OutdoorAir and ZoneHVAC:EnergyRecoveryVentilator:Controller

    if (state.dataMixedAir->AllocateOAControllersFlag) {
        state.dataMixedAir->NumOAControllers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObjects(static_cast<int>(CMO::OAController)));
        state.dataMixedAir->NumERVControllers =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObjects(static_cast<int>(CMO::ERVController)));
        state.dataMixedAir->NumOAControllers += state.dataMixedAir->NumERVControllers;
        state.dataMixedAir->OAController.allocate(state.dataMixedAir->NumOAControllers);
        state.dataMixedAir->OAControllerUniqueNames.reserve(static_cast<unsigned>(state.dataMixedAir->NumOAControllers));
        state.dataMixedAir->AllocateOAControllersFlag = false;
    }
}

void GetOAMixerInputs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Input the OAMixer data and store it in the OAMixer array.

    // METHODOLOGY EMPLOYED:
    // Use the Get routines from the InputProcessor module.

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using NodeInputManager::GetOnlySingleNode;

    // Locals
    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetOAMixerInputs: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int NumNums;   // Number of REAL(r64) numbers returned by GetObjectItem
    int NumAlphas; // Number of alphanumerics returned by GetObjectItem
    int NumArg;    // Number of arguments from GetObjectDefMaxArgs call
    int OutAirNum;
    int IOStat;
    Array1D<Real64> NumArray;        // array that holds numeric input values
    Array1D_string AlphArray;        // array that holds alpha input values
    std::string CurrentModuleObject; // Object type for getting and messages
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    bool ErrorsFound(false);

    if (!state.dataMixedAir->GetOAMixerInputFlag) return;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(
        state, CurrentModuleObjects(static_cast<int>(CMO::OAMixer)), NumArg, NumAlphas, NumNums);

    AlphArray.allocate(NumAlphas);
    NumArray.dimension(NumNums, 0.0);
    lNumericBlanks.dimension(NumNums, true);
    lAlphaBlanks.dimension(NumAlphas, true);
    cAlphaFields.allocate(NumAlphas);
    cNumericFields.allocate(NumNums);

    CurrentModuleObject = CurrentModuleObjects(static_cast<int>(CMO::OAMixer));

    state.dataMixedAir->NumOAMixers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);

    if (state.dataMixedAir->NumOAMixers > 0) {

        state.dataMixedAir->OAMixer.allocate(state.dataMixedAir->NumOAMixers);

        for (OutAirNum = 1; OutAirNum <= state.dataMixedAir->NumOAMixers; ++OutAirNum) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
                                                                     OutAirNum,
                                                                     AlphArray,
                                                                     NumAlphas,
                                                                     NumArray,
                                                                     NumNums,
                                                                     IOStat,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);
            UtilityRoutines::IsNameEmpty(state, AlphArray(1), CurrentModuleObject, ErrorsFound);

            state.dataMixedAir->OAMixer(OutAirNum).Name = AlphArray(1);
            state.dataMixedAir->OAMixer(OutAirNum).MixNode = GetOnlySingleNode(state,
                                                                               AlphArray(2),
                                                                               ErrorsFound,
                                                                               CurrentModuleObject,
                                                                               AlphArray(1),
                                                                               DataLoopNode::NodeFluidType::Air,
                                                                               DataLoopNode::NodeConnectionType::Outlet,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               ObjectIsNotParent);
            //  Set connection type to 'Inlet', because this is not necessarily directly from
            //  outside air.  Outside Air Inlet Node List will set the connection to outside air
            state.dataMixedAir->OAMixer(OutAirNum).InletNode = GetOnlySingleNode(state,
                                                                                 AlphArray(3),
                                                                                 ErrorsFound,
                                                                                 CurrentModuleObject,
                                                                                 AlphArray(1),
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::NodeConnectionType::Inlet,
                                                                                 NodeInputManager::compFluidStream::Primary,
                                                                                 ObjectIsNotParent);
            state.dataMixedAir->OAMixer(OutAirNum).RelNode = GetOnlySingleNode(state,
                                                                               AlphArray(4),
                                                                               ErrorsFound,
                                                                               CurrentModuleObject,
                                                                               AlphArray(1),
                                                                               DataLoopNode::NodeFluidType::Air,
                                                                               DataLoopNode::NodeConnectionType::ReliefAir,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               ObjectIsNotParent);
            state.dataMixedAir->OAMixer(OutAirNum).RetNode = GetOnlySingleNode(state,
                                                                               AlphArray(5),
                                                                               ErrorsFound,
                                                                               CurrentModuleObject,
                                                                               AlphArray(1),
                                                                               DataLoopNode::NodeFluidType::Air,
                                                                               DataLoopNode::NodeConnectionType::Inlet,
                                                                               NodeInputManager::compFluidStream::Primary,
                                                                               ObjectIsNotParent);
            // Check for dupes in the four nodes.
            if (state.dataMixedAir->OAMixer(OutAirNum).MixNode == state.dataMixedAir->OAMixer(OutAirNum).InletNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(3) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).InletNode) + " duplicates the " +
                                    cAlphaFields(2) + '.');
                ErrorsFound = true;
            } else if (state.dataMixedAir->OAMixer(OutAirNum).MixNode == state.dataMixedAir->OAMixer(OutAirNum).RelNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(4) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).RelNode) + " duplicates the " +
                                    cAlphaFields(2) + '.');
                ErrorsFound = true;
            } else if (state.dataMixedAir->OAMixer(OutAirNum).MixNode == state.dataMixedAir->OAMixer(OutAirNum).RetNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(5) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).RetNode) + " duplicates the " +
                                    cAlphaFields(2) + '.');
                ErrorsFound = true;
            }

            if (state.dataMixedAir->OAMixer(OutAirNum).InletNode == state.dataMixedAir->OAMixer(OutAirNum).RelNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(4) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).RelNode) + " duplicates the " +
                                    cAlphaFields(3) + '.');
                ErrorsFound = true;
            } else if (state.dataMixedAir->OAMixer(OutAirNum).InletNode == state.dataMixedAir->OAMixer(OutAirNum).RetNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(5) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).RetNode) + " duplicates the " +
                                    cAlphaFields(3) + '.');
                ErrorsFound = true;
            }

            if (state.dataMixedAir->OAMixer(OutAirNum).RelNode == state.dataMixedAir->OAMixer(OutAirNum).RetNode) {
                ShowSevereError(state,
                                CurrentModuleObject + " = " + state.dataMixedAir->OAMixer(OutAirNum).Name + ' ' + cAlphaFields(5) + " = " +
                                    state.dataLoopNodes->NodeID(state.dataMixedAir->OAMixer(OutAirNum).RetNode) + " duplicates the " +
                                    cAlphaFields(4) + '.');
                ErrorsFound = true;
            }

            TestCompSet(state, CurrentModuleObject, state.dataMixedAir->OAMixer(OutAirNum).Name, AlphArray(3), AlphArray(2), "Air Nodes");
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, RoutineName + "Errors found in getting " + CurrentModuleObject);
    }

    state.dataMixedAir->GetOAMixerInputFlag = false;
}

void ProcessOAControllerInputs(EnergyPlusData &state,
                               std::string const &CurrentModuleObject,
                               int const OutAirNum,
                               Array1D_string const &AlphArray,
                               int &NumAlphas,
                               Array1D<Real64> const &NumArray,
                               int &NumNums,
                               Array1D_bool const &lNumericBlanks, // Unused
                               Array1D_bool const &lAlphaBlanks,
                               Array1D_string const &cAlphaFields,
                               Array1D_string const &cNumericFields, // Unused
                               bool &ErrorsFound                     // If errors found in input
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       Shirey/Raustad FSEC, June 2003, Jan 2004
    //                      Mangesh Basarkar, 06/2011: Getting zone OA specifications from Design Specification Object
    //                      Tianzhen Hong, 3/2012: getting zone air distribution effectiveness and secondary recirculation
    //                       from DesignSpecification:ZoneAirDistribution objects
    //       RE-ENGINEERED  MJW: Split out processing controller:outdoorair input to facilitate unit testing, Feb 2015

    // PURPOSE OF THIS SUBROUTINE
    // Input the OAController data and store it in the OAController array.

    // METHODOLOGY EMPLOYED:

    // Using/Aliasing
    using namespace DataDefineEquip;
    using CurveManager::GetCurveIndex;
    using NodeInputManager::GetOnlySingleNode;
    using namespace OutputReportPredefined;

    using OutAirNodeManager::CheckOutAirNodeNumber;

    using SetPointManager::GetMixedAirNumWithCoilFreezingCheck;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetOAControllerInputs: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int OAControllerNum;   // Index to Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
    int ControlledZoneNum; // Index to controlled zones
    bool AirNodeFound;     // Used to determine if control zone is valid
    bool AirLoopFound;     // Used to determine if control zone is served by furnace air loop
    int BranchNum;         // Used to determine if control zone is served by furnace air loop
    int CompNum;           // Used to determine if control zone is served by furnace air loop
    int HStatZoneNum;      // Used to determine if control zone has a humidistat object
    int OASysNum;          // Used to find OA System index for OA Controller
    int OASysIndex;        // Index to OA System
    bool OASysFound;       // OA Controller found OA System index
    Real64 OAFlowRatio;    // Ratio of minimum OA flow rate to maximum OA flow rate

    state.dataMixedAir->OAController(OutAirNum).Name = AlphArray(1);
    state.dataMixedAir->OAController(OutAirNum).ControllerType = CurrentModuleObject;
    state.dataMixedAir->OAController(OutAirNum).ControllerType_Num = iControllerType::ControllerOutsideAir;
    state.dataMixedAir->OAController(OutAirNum).MaxOA = NumArray(2);
    state.dataMixedAir->OAController(OutAirNum).MinOA = NumArray(1);
    state.dataMixedAir->OAController(OutAirNum).MixNode = GetOnlySingleNode(state,
                                                                            AlphArray(4),
                                                                            ErrorsFound,
                                                                            CurrentModuleObject,
                                                                            AlphArray(1),
                                                                            DataLoopNode::NodeFluidType::Air,
                                                                            DataLoopNode::NodeConnectionType::Sensor,
                                                                            NodeInputManager::compFluidStream::Primary,
                                                                            ObjectIsNotParent);
    state.dataMixedAir->OAController(OutAirNum).OANode = GetOnlySingleNode(state,
                                                                           AlphArray(5),
                                                                           ErrorsFound,
                                                                           CurrentModuleObject,
                                                                           AlphArray(1),
                                                                           DataLoopNode::NodeFluidType::Air,
                                                                           DataLoopNode::NodeConnectionType::Actuator,
                                                                           NodeInputManager::compFluidStream::Primary,
                                                                           ObjectIsNotParent);
    if (!CheckOutAirNodeNumber(state, state.dataMixedAir->OAController(OutAirNum).OANode)) {
        ShowWarningError(
            state, CurrentModuleObject + "=\"" + AlphArray(1) + "\": " + cAlphaFields(5) + "=\"" + AlphArray(5) + "\" is not an OutdoorAir:Node.");
        ShowContinueError(state, "Confirm that this is the intended source for the outdoor air stream.");
    }
    if (UtilityRoutines::SameString(AlphArray(6), "NoEconomizer")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::NoEconomizer;
    } else if (UtilityRoutines::SameString(AlphArray(6), "FixedDryBulb")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::FixedDryBulb;
    } else if (UtilityRoutines::SameString(AlphArray(6), "FixedEnthalpy")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::FixedEnthalpy;
    } else if (UtilityRoutines::SameString(AlphArray(6), "FixedDewPointAndDryBulb")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::FixedDewPointAndDryBulb;
    } else if (UtilityRoutines::SameString(AlphArray(6), "DifferentialDryBulb")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::DifferentialDryBulb;
    } else if (UtilityRoutines::SameString(AlphArray(6), "DifferentialEnthalpy")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::DifferentialEnthalpy;
    } else if (UtilityRoutines::SameString(AlphArray(6), "DifferentialDryBulbAndEnthalpy")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::DifferentialDryBulbAndEnthalpy;
    } else if (UtilityRoutines::SameString(AlphArray(6), "ElectronicEnthalpy")) {
        state.dataMixedAir->OAController(OutAirNum).Econo = iEconoOp::ElectronicEnthalpy;
    } else {
        ShowSevereError(state, CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(6) + "=\"" + AlphArray(6) + "\" value.");
        ErrorsFound = true;
    }
    // Bypass choice - Added by Amit for new feature implementation
    if (UtilityRoutines::SameString(AlphArray(7), "ModulateFlow")) {
        state.dataMixedAir->OAController(OutAirNum).EconBypass = false;
    } else if (UtilityRoutines::SameString(AlphArray(7), "MinimumFlowWithBypass")) {
        state.dataMixedAir->OAController(OutAirNum).EconBypass = true;
    } else {
        ShowSevereError(state, CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(7) + "=\"" + AlphArray(7) + "\" value.");
        ErrorsFound = true;
    }

    if (UtilityRoutines::SameString(AlphArray(9), "NoLockout")) {
        state.dataMixedAir->OAController(OutAirNum).Lockout = iLockoutType::NoLockoutPossible;
    } else if (UtilityRoutines::SameString(AlphArray(9), "LockoutWithHeating")) {
        state.dataMixedAir->OAController(OutAirNum).Lockout = iLockoutType::LockoutWithHeatingPossible;
    } else if (UtilityRoutines::SameString(AlphArray(9), "LockoutWithCompressor")) {
        state.dataMixedAir->OAController(OutAirNum).Lockout = iLockoutType::LockoutWithCompressorPossible;
    } else {
        ShowSevereError(state, CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(9) + "=\"" + AlphArray(9) + "\" value.");
        ErrorsFound = true;
    }
    if (UtilityRoutines::SameString(AlphArray(10), "FixedMinimum")) {
        state.dataMixedAir->OAController(OutAirNum).FixedMin = true;
    } else {
        state.dataMixedAir->OAController(OutAirNum).FixedMin = false;
    }
    if (lNumericBlanks(3)) {
        state.dataMixedAir->OAController(OutAirNum).TempLim = BlankNumeric;
    } else {
        state.dataMixedAir->OAController(OutAirNum).TempLim = NumArray(3);
    }

    if (lNumericBlanks(4)) {
        state.dataMixedAir->OAController(OutAirNum).EnthLim = BlankNumeric;
    } else {
        state.dataMixedAir->OAController(OutAirNum).EnthLim = NumArray(4);
    }
    if (lNumericBlanks(5)) {
        state.dataMixedAir->OAController(OutAirNum).DPTempLim = BlankNumeric;
    } else {
        state.dataMixedAir->OAController(OutAirNum).DPTempLim = NumArray(5);
    }

    if (lNumericBlanks(6)) {
        state.dataMixedAir->OAController(OutAirNum).TempLowLim = BlankNumeric;
    } else {
        state.dataMixedAir->OAController(OutAirNum).TempLowLim = NumArray(6);
    }

    if (!lAlphaBlanks(8)) {
        state.dataMixedAir->OAController(OutAirNum).EnthalpyCurvePtr = GetCurveIndex(state, AlphArray(8)); // convert curve name to number
        if (state.dataMixedAir->OAController(OutAirNum).EnthalpyCurvePtr == 0) {
            ShowSevereError(state,
                            CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(8) + "=\"" + AlphArray(8) + "\" not found.");
            ErrorsFound = true;
        } else {
            // Verify Curve Object, only legal types are Quadratic and Cubic
            ErrorsFound |= CurveManager::CheckCurveDims(state,
                                                        state.dataMixedAir->OAController(OutAirNum).EnthalpyCurvePtr, // Curve index
                                                        {1},                                                          // Valid dimensions
                                                        RoutineName,                                                  // Routine name
                                                        CurrentModuleObject,                                          // Object Type
                                                        state.dataMixedAir->OAController(OutAirNum).Name,             // Object Name
                                                        cAlphaFields(8));                                             // Field Name
        }
    }

    state.dataMixedAir->OAController(OutAirNum).RelNode = GetOnlySingleNode(state,
                                                                            AlphArray(2),
                                                                            ErrorsFound,
                                                                            CurrentModuleObject,
                                                                            AlphArray(1),
                                                                            DataLoopNode::NodeFluidType::Air,
                                                                            DataLoopNode::NodeConnectionType::Actuator,
                                                                            NodeInputManager::compFluidStream::Primary,
                                                                            ObjectIsNotParent);
    state.dataMixedAir->OAController(OutAirNum).RetNode = GetOnlySingleNode(state,
                                                                            AlphArray(3),
                                                                            ErrorsFound,
                                                                            CurrentModuleObject,
                                                                            AlphArray(1),
                                                                            DataLoopNode::NodeFluidType::Air,
                                                                            DataLoopNode::NodeConnectionType::Sensor,
                                                                            NodeInputManager::compFluidStream::Primary,
                                                                            ObjectIsNotParent);
    state.dataMixedAir->OAController(OutAirNum).MinOASch = AlphArray(11);
    state.dataMixedAir->OAController(OutAirNum).MinOASchPtr = GetScheduleIndex(state, AlphArray(11));
    if (state.dataMixedAir->OAController(OutAirNum).MinOASchPtr == 0 && (!lAlphaBlanks(11))) {
        ShowSevereError(state,
                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(11) + "=\"" + AlphArray(11) + "\" not found.");
        ErrorsFound = true;
    }

    // Changed by Amit for new feature implementation
    state.dataMixedAir->OAController(OutAirNum).MinOAflowSch = AlphArray(12);
    state.dataMixedAir->OAController(OutAirNum).MinOAflowSchPtr = GetScheduleIndex(state, AlphArray(12));
    if (state.dataMixedAir->OAController(OutAirNum).MinOAflowSchPtr == 0 && (!lAlphaBlanks(12))) {
        ShowSevereError(state,
                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(12) + "=\"" + AlphArray(12) + "\" not found.");
        ErrorsFound = true;
    }

    state.dataMixedAir->OAController(OutAirNum).MaxOAflowSch = AlphArray(13);
    state.dataMixedAir->OAController(OutAirNum).MaxOAflowSchPtr = GetScheduleIndex(state, AlphArray(13));
    if (state.dataMixedAir->OAController(OutAirNum).MaxOAflowSchPtr == 0 && (!lAlphaBlanks(13))) {
        ShowSevereError(state,
                        CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(13) + "=\"" + AlphArray(13) + "\" not found.");
        ErrorsFound = true;
    }
    state.dataMixedAir->OAController(OutAirNum).VentilationMechanicalName = AlphArray(14);

    //   Check for a time of day economizer control schedule
    state.dataMixedAir->OAController(OutAirNum).EconomizerOASchedPtr = GetScheduleIndex(state, AlphArray(15));

    //   High humidity control option can be used with any economizer flag
    if (UtilityRoutines::SameString(AlphArray(16), "Yes")) {

        state.dataMixedAir->OAController(OutAirNum).HumidistatZoneNum = UtilityRoutines::FindItemInList(AlphArray(17), state.dataHeatBal->Zone);

        // Get the node number for the zone with the humidistat
        if (state.dataMixedAir->OAController(OutAirNum).HumidistatZoneNum > 0) {
            AirNodeFound = false;
            AirLoopFound = false;
            OASysFound = false;
            for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum !=
                    state.dataMixedAir->OAController(OutAirNum).HumidistatZoneNum)
                    continue;
                //           Find the controlled zone number for the specified humidistat location
                state.dataMixedAir->OAController(OutAirNum).NodeNumofHumidistatZone =
                    state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                //           Determine which OA System uses this OA Controller
                OASysIndex = 0;
                for (OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; ++OASysNum) {
                    for (OAControllerNum = 1; OAControllerNum <= state.dataAirLoop->OutsideAirSys(OASysNum).NumControllers; ++OAControllerNum) {
                        if (!UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerType(OAControllerNum),
                                                         CurrentModuleObject) ||
                            !UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName(OAControllerNum),
                                                         state.dataMixedAir->OAController(OutAirNum).Name))
                            continue;
                        OASysIndex = OASysNum;
                        OASysFound = true;
                        break;
                    }
                    if (OASysFound) break;
                }
                //           Determine if controller is on air loop served by the humidistat location specified
                for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                    int AirLoopNumber = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode);
                    if (AirLoopNumber > 0 && OASysIndex > 0) {
                        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).NumBranches; ++BranchNum) {
                            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                if (!UtilityRoutines::SameString(
                                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).Comp(CompNum).Name,
                                        state.dataAirLoop->OutsideAirSys(OASysIndex).Name) ||
                                    !UtilityRoutines::SameString(
                                        state.dataAirSystemsData->PrimaryAirSystems(AirLoopNumber).Branch(BranchNum).Comp(CompNum).TypeOf,
                                        "AirLoopHVAC:OutdoorAirSystem"))
                                    continue;
                                AirLoopFound = true;
                                break;
                            }
                            if (AirLoopFound) break;
                        }
                        for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                            if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum !=
                                state.dataMixedAir->OAController(OutAirNum).HumidistatZoneNum)
                                continue;
                            AirNodeFound = true;
                            break;
                        }
                    } else {
                        if (OASysIndex == 0) {
                            ShowSevereError(state,
                                            "Did not find an AirLoopHVAC:OutdoorAirSystem for " +
                                                state.dataMixedAir->OAController(OutAirNum).ControllerType + " = \"" +
                                                state.dataMixedAir->OAController(OutAirNum).Name + "\"");
                            ErrorsFound = true;
                        }
                    }
                }
            }
            if (!AirNodeFound) {
                ShowSevereError(state,
                                "Did not find Air Node (Zone with Humidistat), " + state.dataMixedAir->OAController(OutAirNum).ControllerType +
                                    " = \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\"");
                ShowContinueError(state, "Specified " + cAlphaFields(17) + " = " + AlphArray(17));
                ShowContinueError(state,
                                  "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object must be specified for this zone.");
                ErrorsFound = true;
            }
            if (!AirLoopFound) {
                ShowSevereError(state,
                                "Did not find correct Primary Air Loop for " + state.dataMixedAir->OAController(OutAirNum).ControllerType + " = \"" +
                                    state.dataMixedAir->OAController(OutAirNum).Name + "\"");
                ShowContinueError(state, cAlphaFields(17) + " = " + AlphArray(17) + " is not served by this Primary Air Loop equipment.");
                ErrorsFound = true;
            }
        } else {
            ShowSevereError(state,
                            "Did not find Air Node (Zone with Humidistat), " + state.dataMixedAir->OAController(OutAirNum).ControllerType + " = \"" +
                                state.dataMixedAir->OAController(OutAirNum).Name + "\"");
            ShowContinueError(state, "Specified " + cAlphaFields(17) + " = " + AlphArray(17));
            ShowContinueError(state,
                              "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object must be specified for this zone.");
            ErrorsFound = true;
        }

        state.dataMixedAir->OAController(OutAirNum).HighRHOAFlowRatio = NumArray(7);
        if (state.dataMixedAir->OAController(OutAirNum).HighRHOAFlowRatio <= 0.0 && NumNums > 6) {
            ShowWarningError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\"");
            ShowContinueError(state, ' ' + cNumericFields(7) + " must be greater than 0.");
            ShowContinueError(state, ' ' + cNumericFields(7) + " is reset to 1 and the simulation continues.");
            state.dataMixedAir->OAController(OutAirNum).HighRHOAFlowRatio = 1.0;
        }

        if (UtilityRoutines::SameString(AlphArray(16), "Yes") && state.dataMixedAir->OAController(OutAirNum).FixedMin) {
            if (state.dataMixedAir->OAController(OutAirNum).MaxOA > 0.0 && state.dataMixedAir->OAController(OutAirNum).MinOA != AutoSize) {
                OAFlowRatio = state.dataMixedAir->OAController(OutAirNum).MinOA / state.dataMixedAir->OAController(OutAirNum).MaxOA;
                if (state.dataMixedAir->OAController(OutAirNum).HighRHOAFlowRatio < OAFlowRatio) {
                    ShowWarningError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\"");
                    ShowContinueError(state, "... A fixed minimum outside air flow rate and high humidity control have been specified.");
                    ShowContinueError(state,
                                      "... The " + cNumericFields(7) +
                                          " is less than the ratio of the outside air controllers minimum to maximum outside air flow rate.");
                    ShowContinueError(
                        state, format("... Controller {} = {:.4T} m3/s.", cNumericFields(1), state.dataMixedAir->OAController(OutAirNum).MinOA));
                    ShowContinueError(
                        state, format("... Controller {} = {:.4T} m3/s.", cNumericFields(2), state.dataMixedAir->OAController(OutAirNum).MaxOA));
                    ShowContinueError(state, format("... Controller minimum to maximum flow ratio = {:.4T}.", OAFlowRatio));
                    ShowContinueError(state,
                                      format("... {} = {:.4T}.", cNumericFields(7), state.dataMixedAir->OAController(OutAirNum).HighRHOAFlowRatio));
                }
            }
        }

        if (NumAlphas >= 18) {
            if (UtilityRoutines::SameString(AlphArray(18), "Yes")) {
                state.dataMixedAir->OAController(OutAirNum).ModifyDuringHighOAMoisture = false;
            } else if (UtilityRoutines::SameString(AlphArray(18), "No")) {
                state.dataMixedAir->OAController(OutAirNum).ModifyDuringHighOAMoisture = true;
            } else {
                ShowSevereError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\", invalid field value");
                ShowContinueError(state, "..." + cAlphaFields(18) + "=\"" + AlphArray(18) + "\" - valid values are \"Yes\" or \"No\".");
                ErrorsFound = true;
            }
        } else {
            if (state.dataMixedAir->OAController(OutAirNum).Econo == iEconoOp::NoEconomizer) {
                state.dataMixedAir->OAController(OutAirNum).ModifyDuringHighOAMoisture = true;
            } else {
                state.dataMixedAir->OAController(OutAirNum).ModifyDuringHighOAMoisture = false;
                ShowWarningError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\", missing field value");
                ShowContinueError(state, "..." + cAlphaFields(18) + " will default to Yes when " + cAlphaFields(16) + "= \"Yes\"");
            }
        }

    } else if (UtilityRoutines::SameString(AlphArray(16), "No") || lAlphaBlanks(16)) {
        if (NumAlphas >= 18) {
            if (!UtilityRoutines::SameString(AlphArray(18), "Yes") && !UtilityRoutines::SameString(AlphArray(18), "No")) {
                ShowSevereError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\", invalid field value");
                ShowContinueError(state, "..." + cAlphaFields(18) + "=\"" + AlphArray(18) + "\" - valid values are \"Yes\" or \"No\".");
                ErrorsFound = true;
            }
        }
    } else { // Invalid field 16
        ShowSevereError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\", invalid field value");
        ShowContinueError(state, "..." + cAlphaFields(16) + "=\"" + AlphArray(16) + "\" - valid values are \"Yes\" or \"No\".");
        ErrorsFound = true;
        if (NumAlphas >= 18) {
            if (!UtilityRoutines::SameString(AlphArray(18), "Yes") && !UtilityRoutines::SameString(AlphArray(18), "No")) {
                ShowSevereError(state, CurrentModuleObject + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\", invalid field value");
                ShowContinueError(state, "..." + cAlphaFields(18) + "=\"" + AlphArray(18) + "\" - valid values are \"Yes\" or \"No\".");
                ErrorsFound = true;
            }
        }
    }

    if (NumAlphas > 18) {
        if (!lAlphaBlanks(19)) {
            if (UtilityRoutines::SameString(AlphArray(19), "BypassWhenWithinEconomizerLimits")) {
                state.dataMixedAir->OAController(OutAirNum).HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits;
            } else if (UtilityRoutines::SameString(AlphArray(19), "BypassWhenOAFlowGreaterThanMinimum")) {
                state.dataMixedAir->OAController(OutAirNum).HeatRecoveryBypassControlType = BypassWhenOAFlowGreaterThanMinimum;
            } else {
                ShowWarningError(state,
                                 CurrentModuleObject + "=\"" + AlphArray(1) + "\" invalid " + cAlphaFields(19) + "=\"" + AlphArray(19) + "\".");
                ShowContinueError(state, "...assuming \"BypassWhenWithinEconomizerLimits\" and the simulation continues.");
                state.dataMixedAir->OAController(OutAirNum).HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits;
            }
        }
    }

    if (UtilityRoutines::SameString(AlphArray(16), "Yes") && state.dataMixedAir->OAController(OutAirNum).Econo == iEconoOp::NoEconomizer) {
        ShowWarningError(
            state, state.dataMixedAir->OAController(OutAirNum).ControllerType + " \"" + state.dataMixedAir->OAController(OutAirNum).Name + "\"");
        ShowContinueError(state, "...Economizer operation must be enabled when " + cAlphaFields(16) + " is set to YES.");
        ShowContinueError(state, "...The high humidity control option will be disabled and the simulation continues.");
    }

    state.dataMixedAir->OAController(OutAirNum).MixedAirSPMNum =
        GetMixedAirNumWithCoilFreezingCheck(state, state.dataMixedAir->OAController(OutAirNum).MixNode);
}

// End of Get Input subroutines for the Module
//******************************************************************************

// Beginning Initialization Section of the Module
//******************************************************************************

void InitOutsideAirSys(EnergyPlusData &state, int const(OASysNum), bool const FirstHVACIteration, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Initialize the OutsideAirSys data structure

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // Using/Aliasing
    using namespace DataLoopNode;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    //        if ( BeginEnvrnFlag && FirstHVACIteration ) {
    //        }

    //        if ( BeginDayFlag ) {
    //        }

    if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum > -1) return;

    if (state.dataMixedAir->initOASysFlag(OASysNum)) {
        state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysNum = OASysNum;
        state.dataMixedAir->initOASysFlag(OASysNum) = false;
    }

    // Each time step
    if (FirstHVACIteration) {
    }

    // Each iteration
}

void InitOAController(EnergyPlusData &state, int const OAControllerNum, bool const FirstHVACIteration, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Feb 2004
    //                      Tianzhen Hong, Feb 2009 for DCV
    //                      Tianzhen Hong, Aug 2013 for economizer faults

    // PURPOSE OF THIS SUBROUTINE
    // Initialize the OAController data structure with input node data

    using namespace DataLoopNode;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    using namespace OutputReportPredefined;
    using EMSManager::CheckIfNodeSetPointManagedByEMS;

    auto &OAControllerMyOneTimeFlag = state.dataMixedAir->OAControllerMyOneTimeFlag; // One-time initialization flag
    auto &OAControllerMyEnvrnFlag = state.dataMixedAir->OAControllerMyEnvrnFlag;     // One-time initialization flag
    auto &OAControllerMySizeFlag = state.dataMixedAir->OAControllerMySizeFlag;       // One-time initialization flag
    auto &MechVentCheckFlag = state.dataMixedAir->MechVentCheckFlag;                 // One-time initialization flag
    bool FoundZone;               // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
    bool FoundAreaZone;           // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
    bool FoundPeopleZone;         // Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
    bool OASysFound;              // Logical determines if OA system found
    bool AirLoopFound;            // Logical determines if primary air loop found
    bool ErrorsFound;             // Errors found getting input
    Real64 RhoAirStdInit;         // Standard air density
    Real64 TotalPeopleOAFlow;     // Total outside air required for PEOPLE objects served by this OA controller
    int MixedAirNode;             // Controller:OutdoorAir mixed air node
    int AirLoopZoneInfoZoneNum;   // Index to AirLoopZoneInfo structure
    int NumZone;                  // Zone number in AirLoopZoneInfo structure
    int PeopleNum;                // Index to PEOPLE objects
    int NumMechVentZone;          // Index to number of zones in VentilationMechanical structure
    int TempMechVentArrayCounter; // Temporary array counter
    int thisOASys;                // Temporary array counter
    int thisNumForMixer;          // Temporary array counter
    int thisMixerIndex;           // Temporary array counter
    int OASysNum;                 // Temporary array counter
    int found;                    // Temporary index to equipment
    int OANode;                   // OA node index
    int VentMechObjectNum;        // Temporary variable
    int OAControllerLoop;         // Index to OA controller in an OA system
    int OAControllerLoop2;        // Index to OA controller in an OA system
    int thisAirLoop;              // Temporary array counter
    int BranchNum;                // Temporary array counter
    int CompNum;                  // Temporary array counter
    std::string equipName;        // Temporary equipment name
    std::string airloopName;      // Temporary equipment name
    std::string zoneName;
    int jZone;

    Real64 rSchVal;
    Real64 rOffset;
    int i;
    iEconoOp iEco;

    ErrorsFound = false;
    OANode = 0;

    auto &thisOAController(state.dataMixedAir->OAController(OAControllerNum));

    if (state.dataMixedAir->InitOAControllerOneTimeFlag) {
        OAControllerMyOneTimeFlag.dimension(state.dataMixedAir->NumOAControllers, true);
        OAControllerMyEnvrnFlag.dimension(state.dataMixedAir->NumOAControllers, true);
        OAControllerMySizeFlag.dimension(state.dataMixedAir->NumOAControllers, true);
        MechVentCheckFlag.dimension(state.dataMixedAir->NumOAControllers, true);
        state.dataMixedAir->InitOAControllerSetPointCheckFlag.dimension(state.dataMixedAir->NumOAControllers, true);
        state.dataMixedAir->InitOAControllerOneTimeFlag = false;
    }
    if (OAControllerMyOneTimeFlag(OAControllerNum)) {
        // Determine Inlet node index for OAController, not a user input for controller, but is obtained from OutsideAirSys and OAMixer
        {
            auto const SELECT_CASE_var(thisOAController.ControllerType_Num);

            if (SELECT_CASE_var == iControllerType::ControllerOutsideAir) {
                thisOASys = 0;
                for (OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; ++OASysNum) {
                    // find which OAsys has this controller
                    found = UtilityRoutines::FindItemInList(thisOAController.Name,
                                                            state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName,
                                                            isize(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName));
                    if (found != 0) {
                        thisOASys = OASysNum;
                        state.dataAirLoop->OutsideAirSys(thisOASys).OAControllerIndex = GetOAController(state, thisOAController.Name);
                        break; // we found it
                    }
                }
                if (thisOASys == 0) {
                    ShowSevereError(state, "InitOAController: Did not find OAController=\"" + thisOAController.Name + "\".");
                    ShowContinueError(state, "in list of valid OA Controllers.");
                    ErrorsFound = true;
                }
                thisNumForMixer = UtilityRoutines::FindItem(CurrentModuleObjects(static_cast<int>(CMO::OAMixer)),
                                                            state.dataAirLoop->OutsideAirSys(thisOASys).ComponentType,
                                                            isize(state.dataAirLoop->OutsideAirSys(thisOASys).ComponentType));
                if (thisNumForMixer != 0) {
                    equipName = state.dataAirLoop->OutsideAirSys(thisOASys).ComponentName(thisNumForMixer);
                    thisMixerIndex = UtilityRoutines::FindItemInList(equipName, state.dataMixedAir->OAMixer);
                    if (thisMixerIndex != 0) {
                        thisOAController.InletNode = state.dataMixedAir->OAMixer(thisMixerIndex).InletNode;
                    } else {
                        ShowSevereError(state, "InitOAController: Did not find OAMixer=\"" + equipName + "\".");
                        ShowContinueError(state, "in list of valid OA Mixers.");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, "InitOAController: Did not find OutdoorAir:Mixer Component=\"OutdoorAir:Mixer\".");
                    ShowContinueError(state, "in list of valid OA Components.");
                    ErrorsFound = true;
                }

                if (thisOAController.InletNode == 0) { // throw an error
                    ShowSevereError(
                        state, "InitOAController: Failed to find proper inlet node for OutdoorAir:Mixer and Controller = " + thisOAController.Name);
                    ErrorsFound = true;
                }

            } else if (SELECT_CASE_var == iControllerType::ControllerStandAloneERV) {
                // set the inlet node to also equal the OA node because this is a special controller for economizing stand alone ERV
                // with the assumption that equipment is bypassed....

                thisOAController.InletNode = thisOAController.OANode;

            } else {
                ShowSevereError(state, "InitOAController: Failed to find ControllerType: " + thisOAController.ControllerType);
                ErrorsFound = true;
            }
        }

        OAControllerMyOneTimeFlag(OAControllerNum) = false;
    }

    if (!state.dataGlobal->SysSizingCalc && state.dataMixedAir->InitOAControllerSetPointCheckFlag(OAControllerNum) &&
        state.dataHVACGlobal->DoSetPointTest && !FirstHVACIteration) {
        MixedAirNode = thisOAController.MixNode;
        if (MixedAirNode > 0) {
            //      IF (OAController(OAControllerNum)%Econo == 1 .AND. .NOT. AirLoopControlInfo(AirLoopNum)%CyclingFan) THEN
            if (thisOAController.Econo > iEconoOp::NoEconomizer && state.dataAirLoop->AirLoopControlInfo(AirLoopNum).AnyContFan) {
                if (state.dataLoopNodes->Node(MixedAirNode).TempSetPoint == SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state, "MixedAir: Missing temperature setpoint for economizer controller " + thisOAController.Name);
                        ShowSevereError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(MixedAirNode));
                        ShowContinueError(
                            state, "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the mixed air node.");
                        state.dataHVACGlobal->SetPointErrorFlag = true;
                    } else {
                        // add call to check node in EMS
                        CheckIfNodeSetPointManagedByEMS(
                            state, MixedAirNode, EMSManager::SPControlType::iTemperatureSetPoint, state.dataHVACGlobal->SetPointErrorFlag);
                        if (state.dataHVACGlobal->SetPointErrorFlag) {
                            ShowSevereError(state, "MixedAir: Missing temperature setpoint for economizer controller " + thisOAController.Name);
                            ShowSevereError(state, "Node Referenced (by Controller)=" + state.dataLoopNodes->NodeID(MixedAirNode));
                            ShowContinueError(state,
                                              "  use a Setpoint Manager with Control Variable = \"Temperature\" to establish a setpoint at the "
                                              "mixed air node.");
                            ShowContinueError(state, "Or add EMS Actuator to provide temperature setpoint at this node");
                        }
                    }
                }
            }
        }

        state.dataMixedAir->InitOAControllerSetPointCheckFlag(OAControllerNum) = false;
    }

    if (!state.dataGlobal->SysSizingCalc && OAControllerMySizeFlag(OAControllerNum)) {
        thisOAController.SizeOAController(state);
        if (AirLoopNum > 0) {
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OACtrlNum = OAControllerNum;
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OACtrlName = thisOAController.Name;
            if (thisOAController.Lockout == iLockoutType::LockoutWithHeatingPossible) {
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating = true;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor = false;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanNotLockoutEcono = false;
            } else if (thisOAController.Lockout == iLockoutType::LockoutWithCompressorPossible) {
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating = false;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor = true;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanNotLockoutEcono = false;
            } else {
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating = false;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor = false;
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CanNotLockoutEcono = true;
            }
        }
        if ((thisOAController.MaxOA - thisOAController.MinOA) < -SmallAirVolFlow) {
            ShowSevereError(state, "For Controller:OutdoorAir: " + thisOAController.Name);
            ShowContinueError(state,
                              format("  maximum outdoor air flow rate ({:.4R}) < minimum outdoor air flow rate ({:.4R})",
                                     thisOAController.MaxOA,
                                     thisOAController.MinOA));
            ShowContinueError(state,
                              "  To set the minimum outside air flow rate use the \"Design (minimum) outdoor air flow rate\" field in the "
                              "Sizing:System object");
            ErrorsFound = true;
        }

        if (AirLoopNum > 0) {
            Real64 DesSupplyVolFlowRate = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply / state.dataEnvrn->StdRhoAir;
            if ((thisOAController.MinOA - DesSupplyVolFlowRate) > 0.0001) {
                ShowWarningError(state,
                                 "InitOAController: Minimum Outdoor Air Flow Rate for Controller:OutdoorAir=" + thisOAController.Name +
                                     " is greater than Design Supply Air Flow Rate for AirLoopHVAC=" +
                                     state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name + ".");
                ShowContinueError(state,
                                  format("...Minimum Outdoor Air Flow Rate={:.6R} will be reset to loop Design Supply Air Flow Rate={:.6R}",
                                         thisOAController.MinOA,
                                         DesSupplyVolFlowRate));
                thisOAController.MinOA = DesSupplyVolFlowRate;
            } else if ((thisOAController.MinOA - DesSupplyVolFlowRate) > 0.0) {
                // If difference is tiny, reset silently
                thisOAController.MinOA = DesSupplyVolFlowRate;
            }
            if ((thisOAController.MaxOA - DesSupplyVolFlowRate) > 0.0001) {
                ShowWarningError(state,
                                 "InitOAController: Maximum Outdoor Air Flow Rate for Controller:OutdoorAir=" + thisOAController.Name +
                                     " is greater than Design Supply Air Flow Rate for AirLoopHVAC=" +
                                     state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name + ".");
                ShowContinueError(state,
                                  format("...Maximum Outdoor Air Flow Rate={:.6R} will be reset to loop Design Supply Air Flow Rate={:.6R}",
                                         thisOAController.MaxOA,
                                         DesSupplyVolFlowRate));
                thisOAController.MaxOA = DesSupplyVolFlowRate;
            } else if ((thisOAController.MaxOA - DesSupplyVolFlowRate) > 0.0) {
                // If difference is tiny, reset silently
                thisOAController.MaxOA = DesSupplyVolFlowRate;
            }
        }

        OAControllerMySizeFlag(OAControllerNum) = false;
    }

    if (state.dataGlobal->BeginEnvrnFlag && OAControllerMyEnvrnFlag(OAControllerNum)) {
        OANode = thisOAController.OANode;
        RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        thisOAController.MinOAMassFlowRate = thisOAController.MinOA * RhoAirStdInit;
        thisOAController.MaxOAMassFlowRate = thisOAController.MaxOA * RhoAirStdInit;
        OAControllerMyEnvrnFlag(OAControllerNum) = false;
        state.dataLoopNodes->Node(OANode).MassFlowRateMax = thisOAController.MaxOAMassFlowRate;

        // predefined reporting
        if (thisOAController.Econo > iEconoOp::NoEconomizer) {
            equipName = thisOAController.Name;
            // 90.1 descriptor for economizer controls
            // Changed by Amit for New Feature implementation
            if (thisOAController.Econo == iEconoOp::DifferentialEnthalpy) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoKind, equipName, "DifferentialEnthalpy");
            } else if (thisOAController.Econo == iEconoOp::DifferentialDryBulb) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoKind, equipName, "DifferentialDryBulb");
            } else if (thisOAController.Econo == iEconoOp::FixedEnthalpy) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoKind, equipName, "FixedEnthalpy");
            } else if (thisOAController.Econo == iEconoOp::FixedDryBulb) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoKind, equipName, "FixedDryBulb");
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoKind, equipName, "Other");
            }

            PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoMinOA, equipName, thisOAController.MinOA);
            PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoMaxOA, equipName, thisOAController.MaxOA);
            // EnergyPlus input echos for economizer controls
            // Chnged by Amit for new feature implementation
            if (thisOAController.Econo == iEconoOp::DifferentialDryBulb) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "Yes");
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "No");
            }
            if (thisOAController.Econo == iEconoOp::DifferentialEnthalpy) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "Yes");
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "No");
            }
            if (thisOAController.Econo == iEconoOp::FixedDryBulb) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, thisOAController.TempLim);
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "-");
            }
            if (thisOAController.Econo == iEconoOp::FixedEnthalpy) {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, thisOAController.EnthLim);
            } else {
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchEcoRetTemp, equipName, "-");
            }
        }
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        OAControllerMyEnvrnFlag(OAControllerNum) = true;
    }

    VentMechObjectNum = thisOAController.VentMechObjectNum;
    if (MechVentCheckFlag(OAControllerNum)) {
        // Make these checks only once at the beginning of the simulation

        // Make sure all air loop zones and air loop zones with people objects are covered by mechanical ventilation
        // Issue a warning only if the zone is not accounted for in the associated mechanical ventilation object
        if (VentMechObjectNum > 0) {
            auto &vent_mech(state.dataMixedAir->VentilationMechanical(VentMechObjectNum));

            // Make sure all zones with mechanical ventilation are on the correct air loop
            TempMechVentArrayCounter = 0;
            for (NumMechVentZone = 1; NumMechVentZone <= vent_mech.NumofVentMechZones; ++NumMechVentZone) {
                int ZoneNum = vent_mech.VentMechZone(NumMechVentZone);
                auto const &zone(state.dataHeatBal->Zone(ZoneNum));
                FoundZone = false;

                for (AirLoopZoneInfoZoneNum = 1; AirLoopZoneInfoZoneNum <= state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones;
                     ++AirLoopZoneInfoZoneNum) {
                    NumZone = state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).ActualZoneNumber(AirLoopZoneInfoZoneNum);
                    if (ZoneNum == NumZone) {
                        FoundZone = true;
                        ++TempMechVentArrayCounter;
                        if (TempMechVentArrayCounter < NumMechVentZone) { // Copy to lower index
                            vent_mech.VentMechZone(TempMechVentArrayCounter) = vent_mech.VentMechZone(NumMechVentZone);
                            vent_mech.ZoneOAAreaRate(TempMechVentArrayCounter) = vent_mech.ZoneOAAreaRate(NumMechVentZone);
                            vent_mech.ZoneOAPeopleRate(TempMechVentArrayCounter) = vent_mech.ZoneOAPeopleRate(NumMechVentZone);
                            vent_mech.ZoneOAFlowRate(TempMechVentArrayCounter) = vent_mech.ZoneOAFlowRate(NumMechVentZone);
                            vent_mech.ZoneOAACHRate(TempMechVentArrayCounter) = vent_mech.ZoneOAACHRate(NumMechVentZone);
                            vent_mech.ZoneOAFlowMethod(TempMechVentArrayCounter) = vent_mech.ZoneOAFlowMethod(NumMechVentZone);
                            vent_mech.ZoneOASchPtr(TempMechVentArrayCounter) = vent_mech.ZoneOASchPtr(NumMechVentZone);
                            vent_mech.ZoneDesignSpecOAObjIndex(TempMechVentArrayCounter) = vent_mech.ZoneDesignSpecOAObjIndex(NumMechVentZone);
                            vent_mech.ZoneDesignSpecOAObjName(TempMechVentArrayCounter) = vent_mech.ZoneDesignSpecOAObjName(NumMechVentZone);

                            // new DCV
                            vent_mech.ZoneADEffCooling(TempMechVentArrayCounter) = vent_mech.ZoneADEffCooling(NumMechVentZone);
                            vent_mech.ZoneADEffHeating(TempMechVentArrayCounter) = vent_mech.ZoneADEffHeating(NumMechVentZone);
                            vent_mech.ZoneADEffSchPtr(TempMechVentArrayCounter) = vent_mech.ZoneADEffSchPtr(NumMechVentZone);
                        }

                        // Sum outside air per unit floor area for each mechanical ventilation object only once per simulation
                        vent_mech.TotAreaOAFlow += zone.FloorArea * zone.Multiplier * zone.ListMultiplier * vent_mech.ZoneOAAreaRate(NumMechVentZone);
                        vent_mech.TotZoneOAFlow += zone.Multiplier * zone.ListMultiplier * vent_mech.ZoneOAFlowRate(NumMechVentZone);
                        vent_mech.TotZoneOAACH +=
                            zone.Multiplier * zone.ListMultiplier * (vent_mech.ZoneOAACHRate(NumMechVentZone) * zone.Volume / 3600.0);
                        break;
                    }
                }
                if (!FoundZone) {
                    ShowWarningError(state,
                                     "Zone name = " + zone.Name + " in " + CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) +
                                         " object name = " + thisOAController.VentilationMechanicalName +
                                         " is not on the same air loop as Controller:OutdoorAir = " + thisOAController.Name);
                    ShowContinueError(state, "This zone will not be used and the simulation will continue...");
                }
            }

            // Shrink final arrays to conserve environment space
            if (TempMechVentArrayCounter < vent_mech.NumofVentMechZones) {
                vent_mech.VentMechZone.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOAAreaRate.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOAPeopleRate.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOAFlowRate.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOAACHRate.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOAFlowMethod.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneOASchPtr.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneDesignSpecOAObjIndex.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneDesignSpecOAObjName.redimension(TempMechVentArrayCounter);

                vent_mech.ZoneADEffCooling.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneADEffHeating.redimension(TempMechVentArrayCounter);
                vent_mech.ZoneADEffSchPtr.redimension(TempMechVentArrayCounter);

                vent_mech.NumofVentMechZones = TempMechVentArrayCounter;
            }

            // predefined report
            for (jZone = 1; jZone <= vent_mech.NumofVentMechZones; ++jZone) {
                zoneName = state.dataHeatBal->Zone(vent_mech.VentMechZone(jZone)).Name;
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVventMechName, zoneName, vent_mech.Name);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVperPerson, zoneName, vent_mech.ZoneOAPeopleRate(jZone), 6);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVperArea, zoneName, vent_mech.ZoneOAAreaRate(jZone), 6);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVperZone, zoneName, vent_mech.ZoneOAFlowRate(jZone), 6);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVperACH, zoneName, vent_mech.ZoneOAACHRate(jZone), 6);
                PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVMethod, zoneName, cOAFlowMethodTypes(vent_mech.ZoneOAFlowMethod(jZone)));
                if (vent_mech.ZoneOASchPtr(jZone) > 0) {
                    PreDefTableEntry(
                        state, state.dataOutRptPredefined->pdchDCVOASchName, zoneName, GetScheduleName(state, vent_mech.ZoneOASchPtr(jZone)));
                } else {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVOASchName, zoneName, "");
                }

                // added for new DCV inputs
                if (vent_mech.ZoneADEffSchPtr(jZone) > 0) {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVZoneADEffCooling, zoneName, "");
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVZoneADEffHeating, zoneName, "");
                    PreDefTableEntry(state,
                                     state.dataOutRptPredefined->pdchDCVZoneADEffSchName,
                                     zoneName,
                                     GetScheduleName(state, vent_mech.ZoneADEffSchPtr(jZone)));
                } else {
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVZoneADEffCooling, zoneName, vent_mech.ZoneADEffCooling(jZone), 2);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVZoneADEffHeating, zoneName, vent_mech.ZoneADEffHeating(jZone), 2);
                    PreDefTableEntry(state, state.dataOutRptPredefined->pdchDCVZoneADEffSchName, zoneName, "");
                }
            }

            // Check to see if any zones on an air loop are not accounted for by a mechanical ventilation object
            for (AirLoopZoneInfoZoneNum = 1; AirLoopZoneInfoZoneNum <= state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).NumZones;
                 ++AirLoopZoneInfoZoneNum) {
                NumZone = state.dataAirLoop->AirLoopZoneInfo(AirLoopNum).ActualZoneNumber(AirLoopZoneInfoZoneNum);
                FoundAreaZone = false;
                FoundPeopleZone = false;
                for (NumMechVentZone = 1; NumMechVentZone <= vent_mech.NumofVentMechZones; ++NumMechVentZone) {
                    int ZoneNum = vent_mech.VentMechZone(NumMechVentZone);
                    if (ZoneNum == NumZone) {
                        FoundAreaZone = true;
                        if (vent_mech.ZoneOAPeopleRate(NumMechVentZone) > 0.0) {
                            FoundPeopleZone = true;
                        }
                        break;
                    }
                }
                if (!FoundAreaZone) {
                    ShowWarningError(state,
                                     "Zone name = " + state.dataHeatBal->Zone(NumZone).Name + " is not accounted for by " +
                                         CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) +
                                         " object name = " + thisOAController.VentilationMechanicalName);
                    ShowContinueError(state, "Ventilation per unit floor area has not been specified for this zone, which is connected to");
                    ShowContinueError(state,
                                      "the air loop served by Controller:OutdoorAir = " + thisOAController.Name + ". Simulation will continue...");
                }
                if (!FoundPeopleZone) {
                    // Loop through people objects to see if this zone has a people object and only then show a warning
                    for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                        if (state.dataHeatBal->People(PeopleNum).ZonePtr == NumZone) {
                            if (!FoundAreaZone) {
                                ShowWarningError(state,
                                                 "PEOPLE object for zone = " + state.dataHeatBal->Zone(NumZone).Name + " is not accounted for by " +
                                                     CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) +
                                                     " object name = " + thisOAController.VentilationMechanicalName);
                                ShowContinueError(state,
                                                  "A \"PEOPLE\" object has been specified in the idf for this zone, but it is not included in this " +
                                                      CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) + " Object.");
                                ShowContinueError(state,
                                                  "Check " + CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) +
                                                      " object. Simulation will continue.");
                            }
                        }
                    }
                } else { // People > 0, check to make sure there is a people statement in the zone
                    FoundAreaZone = false;
                    for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                        if (state.dataHeatBal->People(PeopleNum).ZonePtr != NumZone) continue;
                        FoundAreaZone = true;
                        break;
                    }
                    if (!FoundAreaZone) {
                        ShowWarningError(state,
                                         CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)) + " = \"" +
                                             thisOAController.VentilationMechanicalName + "\", Zone=\"" + state.dataHeatBal->Zone(NumZone).Name +
                                             "\".");
                        ShowContinueError(state,
                                          "No \"PEOPLE\" object has been specified in the idf for this zone, but the ventilation rate is > 0 in "
                                          "this Controller:MechanicalVentilation Object.");
                        ShowContinueError(state, "Check ventilation rate in Controller:MechanicalVentilation object.  Simulation will continue.");
                    }
                }
            }
        }

        MechVentCheckFlag(OAControllerNum) = false;
    }
    //****

    // Perform a one time initialization of AirloopHVAC OA System report variables
    // If AirloopHVAC objects are used, NumPrimaryAirSys > 0 and the initialization proceeds and then sets
    // SetUpAirLoopHVACVariables to .FALSE. so this is never done again and only the first IF is checked
    // each time through Init. If for some reason the primary air system have not yet been read in, this
    // code waits for the air loop data to be available before performing the report variable initialization.
    // If AirloopHVAC objects are not used, NumPrimaryAirSys is always equal to 0 and only these
    // two IF statements are checked each time through Init (e.g., if StandAloneERV controllers are used
    // without AirloopHVAC objects).
    if (state.dataMixedAir->InitOAControllerSetUpAirLoopHVACVariables) {
        if (AirLoopNum > 0) {
            // Added code to report (TH, 10/20/2008):
            //   air economizer status (1 = on, 0 = off or does not exist), and
            //   actual and minimum outside air fraction (0 to 1)
            for (OAControllerLoop = 1; OAControllerLoop <= state.dataMixedAir->NumOAControllers; ++OAControllerLoop) {
                auto &loopOAController(state.dataMixedAir->OAController(OAControllerLoop));

                // Find the outside air system that has the OA controller
                if (loopOAController.ControllerType_Num == iControllerType::ControllerStandAloneERV) continue; // ERV controller not on airloop
                OASysFound = false;
                thisOASys = 0;
                for (OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; ++OASysNum) {
                    for (OAControllerLoop2 = 1; OAControllerLoop2 <= state.dataAirLoop->OutsideAirSys(OASysNum).NumControllers; ++OAControllerLoop2) {
                        if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerName(OAControllerLoop2),
                                                        loopOAController.Name)) {
                            thisOASys = OASysNum;
                            OASysFound = true;
                            break;
                        }
                    }
                    if (OASysFound) break;
                }

                if (thisOASys <= 0) {
                    // Check outside air system name
                    ShowWarningError(state, "Cannot find the AirLoopHVAC:OutdoorAirSystem for the OA Controller: " + thisOAController.Name);
                    AirLoopFound = false;
                } else {
                    // Find the primary air loop that has the outside air system
                    AirLoopFound = false;
                    for (thisAirLoop = 1; thisAirLoop <= state.dataHVACGlobal->NumPrimaryAirSys; ++thisAirLoop) {
                        for (BranchNum = 1; BranchNum <= state.dataAirSystemsData->PrimaryAirSystems(thisAirLoop).NumBranches; ++BranchNum) {
                            for (CompNum = 1; CompNum <= state.dataAirSystemsData->PrimaryAirSystems(thisAirLoop).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                if (!UtilityRoutines::SameString(
                                        state.dataAirSystemsData->PrimaryAirSystems(thisAirLoop).Branch(BranchNum).Comp(CompNum).Name,
                                        state.dataAirLoop->OutsideAirSys(thisOASys).Name) ||
                                    !UtilityRoutines::SameString(
                                        state.dataAirSystemsData->PrimaryAirSystems(thisAirLoop).Branch(BranchNum).Comp(CompNum).TypeOf,
                                        "AirLoopHVAC:OutdoorAirSystem"))
                                    continue;
                                AirLoopFound = true;
                                break;
                            }
                            if (AirLoopFound) break;
                        }
                        if (AirLoopFound) break;
                    }
                }
                // Check primary air loop name
                if (AirLoopFound && thisAirLoop > 0) {
                    airloopName = state.dataAirSystemsData->PrimaryAirSystems(thisAirLoop).Name; // OutsideAirSys(OASysIndex)%Name
                } else {
                    ShowWarningError(state, "Cannot find the primary air loop for the OA Controller: " + thisOAController.Name);
                    airloopName = "AirLoop not found";
                }

                //    Note use of OAControllerLoop here to keep DO Loop index separate from InitOAController local variable
                // CurrentModuleObject='AirLoopHVAC'
                SetupOutputVariable(state,
                                    "Air System Outdoor Air Economizer Status",
                                    OutputProcessor::Unit::None,
                                    loopOAController.EconomizerStatus,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Heat Recovery Bypass Status",
                                    OutputProcessor::Unit::None,
                                    loopOAController.HeatRecoveryBypassStatus,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status",
                                    OutputProcessor::Unit::None,
                                    loopOAController.HRHeatingCoilActive,
                                    "System",
                                    "Average",
                                    airloopName);
                SetupOutputVariable(state,
                                    "Air System Outdoor Air Heat Recovery Bypass Minimum Outdoor Air Mixed Air Temperature",
                                    OutputProcessor::Unit::C,
                                    loopOAController.MixedAirTempAtMinOAFlow,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air High Humidity Control Status",
                                    OutputProcessor::Unit::None,
                                    loopOAController.HighHumCtrlStatus,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Limiting Factor",
                                    OutputProcessor::Unit::None,
                                    loopOAController.OALimitingFactor,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Flow Fraction",
                                    OutputProcessor::Unit::None,
                                    loopOAController.OAFractionRpt,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Minimum Flow Fraction",
                                    OutputProcessor::Unit::None,
                                    loopOAController.MinOAFracLimit,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Outdoor Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    loopOAController.OAMassFlow,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Mixed Air Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    loopOAController.MixMassFlow,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Relief Air Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    loopOAController.RelTotalLossRate,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Relief Air Sensible Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    loopOAController.RelSensiLossRate,
                                    "System",
                                    "Average",
                                    airloopName);

                SetupOutputVariable(state,
                                    "Air System Relief Air Latent Heat Transfer Rate",
                                    OutputProcessor::Unit::W,
                                    loopOAController.RelLatentLossRate,
                                    "System",
                                    "Average",
                                    airloopName);

                if (loopOAController.MixedAirSPMNum > 0) {
                    SetupOutputVariable(state,
                                        "Air System Outdoor Air Maximum Flow Fraction",
                                        OutputProcessor::Unit::None,
                                        loopOAController.MaxOAFracBySetPoint,
                                        "System",
                                        "Average",
                                        airloopName);
                }

                if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
                    SetupEMSInternalVariable(
                        state, "Outdoor Air Controller Maximum Mass Flow Rate", loopOAController.Name, "[kg/s]", loopOAController.MaxOAMassFlowRate);
                    SetupEMSInternalVariable(
                        state, "Outdoor Air Controller Minimum Mass Flow Rate", loopOAController.Name, "[kg/s]", loopOAController.MinOAMassFlowRate);
                    SetupEMSActuator(state,
                                     "Outdoor Air Controller",
                                     loopOAController.Name,
                                     "Air Mass Flow Rate",
                                     "[kg/s]",
                                     loopOAController.EMSOverrideOARate,
                                     loopOAController.EMSOARateValue);
                }

                VentMechObjectNum = loopOAController.VentMechObjectNum;
                if (VentMechObjectNum > 0 && thisAirLoop > 0) {
                    SetupOutputVariable(state,
                                        "Air System Outdoor Air Mechanical Ventilation Requested Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        loopOAController.MechVentOAMassFlowRequest,
                                        "System",
                                        "Average",
                                        airloopName);
                    if (!state.dataMixedAir->VentilationMechanical(VentMechObjectNum).DCVFlag) {
                        state.dataAirLoop->AirLoopControlInfo(thisAirLoop).AirLoopDCVFlag = false;
                    }
                }
            }

            state.dataMixedAir->InitOAControllerSetUpAirLoopHVACVariables = false;
        }
    }

    // Each time step
    if (FirstHVACIteration) {
        // Mixed air setpoint. Set by a setpoint manager.
        if (thisOAController.ControllerType_Num == iControllerType::ControllerOutsideAir) {
            if (state.dataLoopNodes->Node(thisOAController.MixNode).TempSetPoint > SensedNodeFlagValue) {
                thisOAController.MixSetTemp = state.dataLoopNodes->Node(thisOAController.MixNode).TempSetPoint;
            } else {
                thisOAController.MixSetTemp = thisOAController.TempLowLim;
            }

            TotalPeopleOAFlow = 0.0;
            if (VentMechObjectNum != 0) {
                auto &vent_mech(state.dataMixedAir->VentilationMechanical(VentMechObjectNum));
                for (int ZoneIndex = 1; ZoneIndex <= vent_mech.NumofVentMechZones; ++ZoneIndex) {
                    int ZoneNum = vent_mech.VentMechZone(ZoneIndex);

                    // ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
                    int OAFlowMethod = vent_mech.ZoneOAFlowMethod(ZoneIndex);
                    if (OAFlowMethod == OAFlowPPer || OAFlowMethod == OAFlowSum || OAFlowMethod == OAFlowMax) {
                        TotalPeopleOAFlow += state.dataHeatBal->ZoneIntGain(ZoneNum).NOFOCC * state.dataHeatBal->Zone(ZoneNum).Multiplier *
                                             state.dataHeatBal->Zone(ZoneNum).ListMultiplier * vent_mech.ZoneOAPeopleRate(ZoneIndex) *
                                             GetCurrentScheduleValue(state, vent_mech.ZoneOASchPtr(ZoneIndex));
                    }
                }
                vent_mech.TotPeopleOAFlow = TotalPeopleOAFlow;
            }
        } else {
            // Stand Alone ERV does not require a termperature setpoint schedule, make setpoint equal to lower economizer limit
            thisOAController.MixSetTemp = thisOAController.TempLowLim;
        }
    }

    // Each iteration

    if (thisOAController.ControllerType_Num == iControllerType::ControllerOutsideAir) {
        // zone exhaust mass flow is saved in AirLoopFlow%ZoneExhaust
        // the zone exhaust mass flow that is said to be balanced by simple air flows is saved in AirLoopFlow%ZoneExhaustBalanced
        if (AirLoopNum > 0) {
            thisOAController.ExhMassFlow =
                max(0.0, state.dataAirLoop->AirLoopFlow(AirLoopNum).SupFlow - state.dataAirLoop->AirLoopFlow(AirLoopNum).SysRetFlow);
            state.dataAirLoop->AirLoopControlInfo(AirLoopNum).ZoneExhMassFlow = thisOAController.ExhMassFlow;
            if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).LoopFlowRateSet && !FirstHVACIteration) {
                // if flow rate has been specified by a manager, set it to the specified value
                thisOAController.MixMassFlow =
                    state.dataAirLoop->AirLoopFlow(AirLoopNum).ReqSupplyFrac * state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
            } else {
                thisOAController.MixMassFlow = state.dataLoopNodes->Node(thisOAController.RetNode).MassFlowRate + thisOAController.ExhMassFlow;

                // The following was commented out after discussion on PR 7382, it can be reopened for discussion anytime
                // found this equation results in flow that exceeds the design flow rate when there is exhaust flow rate is greater than
                // the design supply air flow rate. Capped the mixed air flow rate at design supply air flow rate, issue #77379
                // thisOAController.MixMassFlow = Node(thisOAController.RetNode).MassFlowRate + thisOAController.ExhMassFlow;
                // thisOAController.MixMassFlow =
                //     min(Node(thisOAController.RetNode).MassFlowRate + thisOAController.ExhMassFlow, AirLoopFlow(AirLoopNum).DesSupply);
            }
        } else {
            thisOAController.ExhMassFlow = 0.0;
            thisOAController.MixMassFlow = state.dataLoopNodes->Node(thisOAController.RetNode).MassFlowRate;
        }
        if (state.dataLoopNodes->Node(thisOAController.MixNode).MassFlowRateMaxAvail <= 0.0) {
            thisOAController.MixMassFlow = 0.0;
        }
    } else {
        // Mixed and exhaust flow rates are passed through to model CONTROLLER:STAND ALONE ERV in SimOAController
        thisOAController.OAMassFlow = thisOAController.MaxOAMassFlowRate;
        thisOAController.MixMassFlow = thisOAController.MaxOAMassFlowRate;
        thisOAController.ExhMassFlow = state.dataLoopNodes->Node(thisOAController.RetNode).MassFlowRate;
    }
    thisOAController.ExhMassFlow = max(thisOAController.ExhMassFlow, 0.0);

    // Outside air values
    thisOAController.OATemp = state.dataLoopNodes->Node(thisOAController.OANode).Temp;
    thisOAController.OAEnth = state.dataLoopNodes->Node(thisOAController.OANode).Enthalpy;
    thisOAController.OAPress = state.dataLoopNodes->Node(thisOAController.OANode).Press;
    thisOAController.OAHumRat = state.dataLoopNodes->Node(thisOAController.OANode).HumRat;

    // Inlet air values (on OA input side)
    thisOAController.InletTemp = state.dataLoopNodes->Node(thisOAController.InletNode).Temp;
    thisOAController.InletEnth = state.dataLoopNodes->Node(thisOAController.InletNode).Enthalpy;
    thisOAController.InletPress = state.dataLoopNodes->Node(thisOAController.InletNode).Press;
    thisOAController.InletHumRat = state.dataLoopNodes->Node(thisOAController.InletNode).HumRat;

    // Return air values
    thisOAController.RetTemp = state.dataLoopNodes->Node(thisOAController.RetNode).Temp;
    thisOAController.RetEnth = state.dataLoopNodes->Node(thisOAController.RetNode).Enthalpy;

    // Check sensors faults for the air economizer
    iEco = thisOAController.Econo;
    if (state.dataFaultsMgr->AnyFaultsInModel && (iEco != iEconoOp::NoEconomizer)) {
        int j; // index to economizer faults
        for (i = 1; i <= thisOAController.NumFaultyEconomizer; ++i) {
            j = thisOAController.EconmizerFaultNum(i);
            if (GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsEconomizer(j).AvaiSchedPtr) > 0.0) {
                rSchVal = 1.0;
                if (state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr > 0) {
                    rSchVal = GetCurrentScheduleValue(state, state.dataFaultsMgr->FaultsEconomizer(j).SeveritySchedPtr);
                }
            } else {
                // no fault
                continue;
            }

            rOffset = rSchVal * state.dataFaultsMgr->FaultsEconomizer(j).Offset;

            if (std::abs(rOffset) < 0.000000001) continue;

            // ECONOMIZER - outdoor air dry-bulb temperature sensor offset
            {
                auto const SELECT_CASE_var(iEco);
                if ((SELECT_CASE_var == iEconoOp::FixedDryBulb) || (SELECT_CASE_var == iEconoOp::DifferentialDryBulb) ||
                    (SELECT_CASE_var == iEconoOp::FixedDewPointAndDryBulb) || (SELECT_CASE_var == iEconoOp::ElectronicEnthalpy) ||
                    (SELECT_CASE_var == iEconoOp::DifferentialDryBulbAndEnthalpy)) {
                    if (state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum == Fault::TemperatureSensorOffset_OutdoorAir) {
                        // FaultModel:TemperatureSensorOffset:OutdoorAir
                        thisOAController.OATemp += rOffset;
                        thisOAController.InletTemp += rOffset;
                    }
                } else {
                }
            }

            // ECONOMIZER - outdoor air humidity ratio sensor offset. really needed ???
            {
                auto const SELECT_CASE_var(iEco);
                if ((SELECT_CASE_var == iEconoOp::FixedDewPointAndDryBulb) || (SELECT_CASE_var == iEconoOp::ElectronicEnthalpy)) {
                    if (state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum == Fault::HumiditySensorOffset_OutdoorAir) {
                        // FaultModel:HumiditySensorOffset:OutdoorAir
                        thisOAController.OAHumRat += rOffset;
                        thisOAController.InletHumRat += rOffset;
                    }
                } else {
                }
            }

            // ECONOMIZER - outdoor air enthalpy sensor offset
            {
                auto const SELECT_CASE_var(iEco);
                if ((SELECT_CASE_var == iEconoOp::FixedEnthalpy) || (SELECT_CASE_var == iEconoOp::ElectronicEnthalpy) ||
                    (SELECT_CASE_var == iEconoOp::DifferentialDryBulbAndEnthalpy)) {
                    if (state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum == Fault::EnthalpySensorOffset_OutdoorAir) {
                        // FaultModel:EnthalpySensorOffset:OutdoorAir
                        thisOAController.OAEnth += rOffset;
                        thisOAController.InletEnth += rOffset;
                    }
                } else {
                }
            }

            // ECONOMIZER - return air dry-bulb temperature sensor offset
            {
                auto const SELECT_CASE_var(iEco);
                if ((SELECT_CASE_var == iEconoOp::DifferentialDryBulb) || (SELECT_CASE_var == iEconoOp::DifferentialDryBulbAndEnthalpy)) {
                    if (state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum == Fault::TemperatureSensorOffset_ReturnAir) {
                        // FaultModel:TemperatureSensorOffset:ReturnAir
                        thisOAController.RetTemp += rOffset;
                    }
                } else {
                }
            }

            // ECONOMIZER - return air enthalpy sensor offset
            {
                auto const SELECT_CASE_var(iEco);
                if ((SELECT_CASE_var == iEconoOp::ElectronicEnthalpy) || (SELECT_CASE_var == iEconoOp::DifferentialDryBulbAndEnthalpy)) {
                    if (state.dataFaultsMgr->FaultsEconomizer(j).FaultTypeEnum == Fault::EnthalpySensorOffset_ReturnAir) {
                        // FaultModel:EnthalpySensorOffset:ReturnAir
                        thisOAController.RetEnth += rOffset;
                    }
                } else {
                }
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Error in " + CurrentModuleObjects(static_cast<int>(CMO::OAController)) + "; program terminated");
    }
} // namespace MixedAir

void InitOAMixer(EnergyPlusData &state, int const OAMixerNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Initialize the OAMixer data structure with input node data

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // Using/Aliasing
    using namespace DataLoopNode;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RetNode;
    int InletNode;
    int RelNode;

    RetNode = state.dataMixedAir->OAMixer(OAMixerNum).RetNode;
    InletNode = state.dataMixedAir->OAMixer(OAMixerNum).InletNode;
    RelNode = state.dataMixedAir->OAMixer(OAMixerNum).RelNode;

    if (state.dataGlobal->BeginEnvrnFlag && FirstHVACIteration) {
    }

    if (state.dataGlobal->BeginDayFlag) {
    }

    if (FirstHVACIteration) {
    }

    // Each iteration

    // Return air stream data
    state.dataMixedAir->OAMixer(OAMixerNum).RetTemp = state.dataLoopNodes->Node(RetNode).Temp;
    state.dataMixedAir->OAMixer(OAMixerNum).RetHumRat = state.dataLoopNodes->Node(RetNode).HumRat;
    state.dataMixedAir->OAMixer(OAMixerNum).RetEnthalpy = state.dataLoopNodes->Node(RetNode).Enthalpy;
    state.dataMixedAir->OAMixer(OAMixerNum).RetPressure = state.dataLoopNodes->Node(RetNode).Press;
    state.dataMixedAir->OAMixer(OAMixerNum).RetMassFlowRate = state.dataLoopNodes->Node(RetNode).MassFlowRate;
    // Outside air stream data
    state.dataMixedAir->OAMixer(OAMixerNum).OATemp = state.dataLoopNodes->Node(InletNode).Temp;
    state.dataMixedAir->OAMixer(OAMixerNum).OAHumRat = state.dataLoopNodes->Node(InletNode).HumRat;
    state.dataMixedAir->OAMixer(OAMixerNum).OAEnthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;
    state.dataMixedAir->OAMixer(OAMixerNum).OAPressure = state.dataLoopNodes->Node(InletNode).Press;
    state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
    // Relief air data
    state.dataMixedAir->OAMixer(OAMixerNum).RelMassFlowRate = state.dataLoopNodes->Node(RelNode).MassFlowRate;
}

// End of Initialization Section of the Module
//******************************************************************************

// Beginning Calculation Section of the Module
//******************************************************************************

void OAControllerProps::CalcOAController(EnergyPlusData &state, int const AirLoopNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       Shirey/Raustad FSEC, June 2003
    //                      Tianzhen Hong, Feb 2009 for new DCV
    //                      Brent Griffith ,EMS override of OA rate
    //                      Mangesh Basarkar, 06/2011: Modifying outside air calculation based on DCV flag
    //                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
    //                           to enhance CO2 based DCV control
    //                      Tianzhen Hong, March 2012, zone maximum OA fraction - a TRACE feature
    //                      Tianzhen Hong, March 2012, multi-path VRP based on ASHRAE 62.1-2010
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Determine the outside air flow

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // DOE-2.1E Supplement pages 3.97 - 3.100
    // BLAST User Reference pages 183 - 186
    // ASHRAE Standard 62.1-2010

    // Using/Aliasing
    using CurveManager::CurveValue;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CalcOAController: ");
    static std::string const CurrentModuleObject(CurrentModuleObjects(static_cast<int>(CMO::OAController)));

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OutAirMinFrac; // Local variable used to calculate min OA fraction

    Real64 MechVentOutsideAirMinFrac;     // fraction of OA specified by mechanical ventilation object
    Real64 MechVentOAMassFlow;            // outside air mass flow rate calculated by mechanical ventilation object [kg/s]
    Real64 MinOASchedVal;                 // value of the minimum outside air schedule
    Real64 OASignal;                      // Outside air flow rate fraction (0.0 to 1.0)
    bool AirLoopCyclingFan;               // Type of air loop fan (TRUE if Fan:OnOff)
    bool HighHumidityOperationFlag;       // TRUE if zone humidistat senses a high humidity condition
    Real64 RecircTemp;                    // - return air temp, used for custom economizer control calculation
    Real64 MixedAirTempAtMinOAFlow;       // - mixed air temperature at min flow rate, used for custom economizer control calculation
    Real64 RecircMassFlowRateAtMinOAFlow; // recirc air mass flow rate at min OA, used for custom economizer control calculation
    Real64 ReliefMassFlowAtMinOA;         // relief air mass flow rate at min OA, used for custom economizer control calculation
    Real64 SysSA(0.0);                    // System supply air mass flow rate [kg/s]
    MinOASchedVal = 1.0;

    if (AirLoopNum > 0) {
        AirLoopCyclingFan = (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).FanOpMode == CycFanCycCoil);
    } else {
        AirLoopCyclingFan = false;
    }

    this->OALimitingFactor = limitFactorNone; // oa controller limiting factor

    // Check for no flow
    if (this->MixMassFlow <= SmallMassFlow) {

        this->OAMassFlow = 0.0;     // outside air mass flow rate
        this->RelMassFlow = 0.0;    // relief air mass flow rate
        this->MixMassFlow = 0.0;    // mixed air mass flow rate
        this->MinOAFracLimit = 0.0; // minimum OA fraction limit

        this->EconomizerStatus = 0;                                                    // economizer status for reporting
        this->HeatRecoveryBypassStatus = 0;                                            // HR bypass status for reporting
        this->HRHeatingCoilActive = 0;                                                 // resets report variable
        this->MixedAirTempAtMinOAFlow = state.dataLoopNodes->Node(this->RetNode).Temp; // track return T
        this->HighHumCtrlStatus = 0;                                                   // high humdity control status for reporting
        this->OAFractionRpt = 0.0;                                                     // actual OA fraction for reporting

        this->EconoActive = false;       // DataAirLoop variable (OA Controllers)
        this->HighHumCtrlActive = false; // DataAirLoop variable (OA Controllers)

        // also reset air loop data for use by other routines
        if (AirLoopNum > 0) {
            auto &curAirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo(AirLoopNum));
            auto &curAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));

            curAirLoopControlInfo.EconoActive = false;        // DataAirLoop variable (AirloopHVAC)
            curAirLoopControlInfo.HeatRecoveryBypass = false; // DataAirLoop variable (AirloopHVAC)
            curAirLoopControlInfo.HighHumCtrlActive = false;  // DataAirLoop variable (AirloopHVAC)
            curAirLoopControlInfo.ResimAirLoopFlag = false;   // DataAirLoop variable (AirloopHVAC)
            curAirLoopFlow.OAFrac = 0.0;                      // DataAirLoop variable (AirloopHVAC)
            curAirLoopFlow.OAMinFrac = 0.0;                   // DataAirLoop variable (AirloopHVAC)
            curAirLoopFlow.MinOutAir = 0.0;
            curAirLoopFlow.OAFlow = 0.0;
        }

        return;
    }

    // set OutAirMinFrac
    if (AirLoopNum > 0) {
        auto &curAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));

        if (curAirLoopFlow.DesSupply >= SmallAirVolFlow) {
            OutAirMinFrac = this->MinOAMassFlowRate / curAirLoopFlow.DesSupply;
        } else {
            OutAirMinFrac = 0.0;
        }
    } else {
        if (this->MaxOA >= SmallAirVolFlow) {
            OutAirMinFrac = this->MinOA / this->MaxOA;
        } else {
            OutAirMinFrac = 0.0;
        }
    }
    if (this->MinOASchPtr > 0) {
        MinOASchedVal = GetCurrentScheduleValue(state, this->MinOASchPtr);
        MinOASchedVal = min(max(MinOASchedVal, 0.0), 1.0);
        OutAirMinFrac *= MinOASchedVal;
        this->OALimitingFactor = limitFactorLimits;
    }

    // Get mechanical ventilation
    MechVentOAMassFlow = 0.0;
    MechVentOutsideAirMinFrac = 0.0;
    if (AirLoopNum > 0 && this->VentMechObjectNum != 0) {
        auto &curAirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo(AirLoopNum));
        auto &curAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));

        // Get system supply air flow rate
        if (curAirLoopControlInfo.LoopFlowRateSet) {
            // if flow rate has been specified by a manager, set it to the specified value
            // DesSupply and SupFlow are mass flow rate in kg/s
            SysSA = curAirLoopFlow.ReqSupplyFrac * curAirLoopFlow.DesSupply;
        } else {
            SysSA = curAirLoopFlow.SupFlow;
        }
        state.dataMixedAir->VentilationMechanical(this->VentMechObjectNum).CalcMechVentController(state, SysSA, MechVentOAMassFlow);
        MechVentOutsideAirMinFrac = MechVentOAMassFlow / curAirLoopFlow.DesSupply;
        if (curAirLoopFlow.FanPLR > 0.0) {
            MechVentOutsideAirMinFrac *= curAirLoopFlow.FanPLR;
            MechVentOAMassFlow *= curAirLoopFlow.FanPLR;
        }
    }
    this->MechVentOAMassFlowRequest = MechVentOAMassFlow;
    //****** use greater of Mechanical Ventilation Outside Air fraction and OutAirMinFrac
    if ((MechVentOutsideAirMinFrac > 0.0) && (OutAirMinFrac > MechVentOutsideAirMinFrac)) {
        if (!state.dataGlobal->WarmupFlag) {
            if (this->CountMechVentFrac == 0) {
                ++this->CountMechVentFrac;
                ShowWarningError(state,
                                 RoutineName + "Minimum OA fraction > Mechanical Ventilation Controller request for Controller:OutdoorAir=" +
                                     this->Name + ", Min OA fraction is used.");
                ShowContinueError(state,
                                  "This may be overriding desired ventilation controls. Check inputs for Minimum Outdoor Air Flow Rate, Minimum "
                                  "Outdoor Air Schedule Name and Controller:MechanicalVentilation");
                ShowContinueErrorTimeStamp(
                    state, format("Minimum OA fraction = {:.4R}, Mech Vent OA fraction = {:.4R}", OutAirMinFrac, MechVentOutsideAirMinFrac));
            } else {
                ShowRecurringWarningErrorAtEnd(state,
                                               "Controller:OutdoorAir=\"" + this->Name +
                                                   "\": Min OA fraction > Mechanical ventilation OA fraction, continues...",
                                               this->IndexMechVentFrac,
                                               OutAirMinFrac,
                                               OutAirMinFrac);
            }
        }
    }
    if (MechVentOutsideAirMinFrac > OutAirMinFrac) {
        OutAirMinFrac = MechVentOutsideAirMinFrac;
        this->OALimitingFactor = limitFactorDCV;
    }

    OutAirMinFrac = min(max(OutAirMinFrac, 0.0), 1.0);

    // At this point, OutAirMinFrac is still based on AirLoopFlow.DesSupply
    if (AirLoopNum > 0) {
        auto &curAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));

        curAirLoopFlow.MinOutAir = OutAirMinFrac * curAirLoopFlow.DesSupply;

        // calculate mixed air temp at min OA flow rate
        ReliefMassFlowAtMinOA = max(curAirLoopFlow.MinOutAir - this->ExhMassFlow, 0.0);
        RecircMassFlowRateAtMinOAFlow = max(state.dataLoopNodes->Node(this->RetNode).MassFlowRate - ReliefMassFlowAtMinOA, 0.0);
        if ((RecircMassFlowRateAtMinOAFlow + curAirLoopFlow.MinOutAir) > 0.0) {
            RecircTemp = state.dataLoopNodes->Node(this->RetNode).Temp;
            MixedAirTempAtMinOAFlow =
                (RecircMassFlowRateAtMinOAFlow * RecircTemp + curAirLoopFlow.MinOutAir * state.dataLoopNodes->Node(this->OANode).Temp) /
                (RecircMassFlowRateAtMinOAFlow + curAirLoopFlow.MinOutAir);
        } else {
            MixedAirTempAtMinOAFlow = state.dataLoopNodes->Node(this->RetNode).Temp;
        }
        this->MixedAirTempAtMinOAFlow = MixedAirTempAtMinOAFlow;
    }

    // Economizer
    this->CalcOAEconomizer(state, AirLoopNum, OutAirMinFrac, OASignal, HighHumidityOperationFlag, FirstHVACIteration);

    this->OAMassFlow = OASignal * this->MixMassFlow;

    // Do not allow OA to be below Ventilation:Mechanical flow rate or above mixed mass flow rate
    if (AirLoopNum > 0 && VentMechObjectNum != 0) {
        if (MechVentOAMassFlow > this->OAMassFlow) {
            this->OAMassFlow = min(MechVentOAMassFlow, this->MixMassFlow);
        }
    }

    // Do not allow OA to be below Exh for controller:outside air
    if (this->ControllerType_Num == iControllerType::ControllerOutsideAir) {
        if (this->ExhMassFlow > this->OAMassFlow) {
            this->OAMassFlow = this->ExhMassFlow;
            this->OALimitingFactor = limitFactorExhaust;
        }
    }

    // if fixed minimum, don't let go below min OA
    if (this->FixedMin) {
        // cycling fans allow "average" min OA to be below minimum
        if (!AirLoopCyclingFan) {
            Real64 minOASchedMassFlowRate = this->MinOAMassFlowRate * MinOASchedVal;
            if (minOASchedMassFlowRate > this->OAMassFlow) {
                this->OAMassFlow = minOASchedMassFlowRate;
                this->OALimitingFactor = limitFactorLimits;
            }
        }
    }

    // Apply Minimum Fraction of Outdoor Air Schedule
    if (this->MinOAflowSchPtr > 0) {
        Real64 MinOAflowfracVal = GetCurrentScheduleValue(state, this->MinOAflowSchPtr);
        MinOAflowfracVal = min(max(MinOAflowfracVal, 0.0), 1.0);
        OutAirMinFrac = max(MinOAflowfracVal, OutAirMinFrac);
        Real64 minOAFracMassFlowRate = this->MixMassFlow * MinOAflowfracVal;
        if (minOAFracMassFlowRate > this->OAMassFlow) {
            this->OAMassFlow = minOAFracMassFlowRate;
            this->OALimitingFactor = limitFactorLimits;
        }
    }

    // Apply Maximum Fraction of Outdoor Air Schedule
    Real64 currentMaxOAMassFlowRate = this->MaxOAMassFlowRate;
    if (this->MaxOAflowSchPtr > 0) {
        Real64 MaxOAflowfracVal = GetCurrentScheduleValue(state, this->MaxOAflowSchPtr);
        MaxOAflowfracVal = min(max(MaxOAflowfracVal, 0.0), 1.0);
        currentMaxOAMassFlowRate = min(this->MaxOAMassFlowRate, this->MixMassFlow * MaxOAflowfracVal);
        OutAirMinFrac = min(MaxOAflowfracVal, OutAirMinFrac);
        if (currentMaxOAMassFlowRate < this->OAMassFlow) {
            this->OAMassFlow = currentMaxOAMassFlowRate;
            this->OALimitingFactor = limitFactorLimits;
        }
    }

    // Don't let the OA flow be > than the max OA limit. OA for high humidity control is allowed to be greater than max OA.
    // Night Ventilation has priority and may override an OASignal > 1 high humidity condition with OASignal = 1
    if (HighHumidityOperationFlag) {
        Real64 maxOAMassFlow = this->MaxOAMassFlowRate * max(1.0, OASignal);
        if (maxOAMassFlow < this->OAMassFlow) {
            this->OAMassFlow = maxOAMassFlow;
            this->OALimitingFactor = limitFactorLimits;
        }
    } else {
        if (this->MaxOAMassFlowRate < this->OAMassFlow) {
            this->OAMassFlow = this->MaxOAMassFlowRate;
            this->OALimitingFactor = limitFactorLimits;
        }
    }

    if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && (this->ManageDemand) && (this->OAMassFlow > this->DemandLimitFlowRate)) {
        this->OAMassFlow = this->DemandLimitFlowRate;
        this->OALimitingFactor = limitFactorDemandLimit;
    }
    if (this->EMSOverrideOARate) {
        this->OAMassFlow = this->EMSOARateValue;
        this->OALimitingFactor = limitFactorEMS;
    }

    // Don't let OA flow be > mixed air flow.
    // Seems if RAB (return air bypass) that this should be don't let OA flow be > design supply flow but that causes other issues
    if (this->MixMassFlow < this->OAMassFlow) {
        this->OAMassFlow = this->MixMassFlow;
        this->OALimitingFactor = limitFactorMixedAir;
    }

    // save the min outside air flow fraction and max outside air mass flow rate
    if (AirLoopNum > 0) {
        auto &curAirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo(AirLoopNum));
        auto &curAirLoopFlow(state.dataAirLoop->AirLoopFlow(AirLoopNum));

        curAirLoopFlow.OAMinFrac = OutAirMinFrac;
        if (this->FixedMin) {
            curAirLoopFlow.MinOutAir = min(OutAirMinFrac * curAirLoopFlow.DesSupply, this->MixMassFlow);
        } else {
            curAirLoopFlow.MinOutAir = OutAirMinFrac * this->MixMassFlow;
        }
        if (this->MixMassFlow > 0.0) {
            curAirLoopFlow.OAFrac = this->OAMassFlow / this->MixMassFlow;
            curAirLoopFlow.OAFlow = this->OAMassFlow;
        } else {
            curAirLoopFlow.OAFrac = 0.0;
            curAirLoopFlow.OAFlow = 0.0;
        }
        this->MinOAFracLimit = OutAirMinFrac;
        if (HighHumidityOperationFlag && OASignal > 1.0) {
            curAirLoopFlow.MaxOutAir = this->MaxOAMassFlowRate * OASignal;
        } else {
            curAirLoopFlow.MaxOutAir = currentMaxOAMassFlowRate;
        }

        // MJW - Not sure if this is necessary but keeping it for now
        if (curAirLoopControlInfo.HeatingActiveFlag && curAirLoopControlInfo.EconomizerFlowLocked) {
            // The airloop needs to be simulated again so that the heating coil & HX can be resimulated
            if (curAirLoopControlInfo.HeatRecoveryResimFlag && curAirLoopControlInfo.OASysComponentsSimulated) {
                curAirLoopControlInfo.ResimAirLoopFlag = true;
                curAirLoopControlInfo.HeatRecoveryResimFlag = false;
                curAirLoopControlInfo.HeatRecoveryResimFlag2 = true;
                // on the first iteration, air loop heating coils have not be simulated so HeatingCoilActive=FALSE
                // on the second iteration, the heating coils could have been on, but logic tests here could deactivate heating coil
                // reset heating coil active status and HX since logic tests may turn off heating coil
                // the ResimAirLoopFlag will force another iteration and things should line up on subsequent iterations
                curAirLoopControlInfo.HeatingActiveFlag = false;
                this->HRHeatingCoilActive = 0;
                curAirLoopControlInfo.HeatRecoveryBypass = true;
                this->HeatRecoveryBypassStatus = 1;
            } else if (curAirLoopControlInfo.HeatRecoveryResimFlag2) {
                curAirLoopControlInfo.ResimAirLoopFlag = true;
                curAirLoopControlInfo.HeatRecoveryResimFlag2 = false;
            } else {
                curAirLoopControlInfo.ResimAirLoopFlag = false;
            }
        } else if (curAirLoopControlInfo.HeatingActiveFlag) {
            this->HRHeatingCoilActive = 1;
        } else {
            this->HRHeatingCoilActive = 0;
        }

    } // if (AirLoopNum > 0)

    // Set the relief air flow rate (must be done last to account for changes in OAMassFlow
    this->RelMassFlow = max(this->OAMassFlow - this->ExhMassFlow, 0.0);

    // Save OA fraction for reporting
    if (this->MixMassFlow > 0) {
        this->OAFractionRpt = this->OAMassFlow / this->MixMassFlow;
    } else {
        if (this->OAMassFlow > 0) {
            this->OAFractionRpt = OASignal;
        } else {
            this->OAFractionRpt = 0.0;
        }
    }
    this->RelTemp = this->RetTemp;
    this->RelEnth = this->RetEnth;
    this->RelSensiLossRate =
        this->RelMassFlow * Psychrometrics::PsyCpAirFnW(state.dataEnvrn->OutHumRat) * (this->RelTemp - state.dataEnvrn->OutDryBulbTemp);
    this->RelTotalLossRate = this->RelMassFlow * (this->RelEnth - state.dataEnvrn->OutEnthalpy);
    this->RelLatentLossRate = this->RelTotalLossRate - this->RelSensiLossRate;
}

void VentilationMechanicalProps::CalcMechVentController(
    EnergyPlusData &state,
    Real64 &SysSA,             // System supply air mass flow rate [kg/s]
    Real64 &MechVentOAMassFlow // outside air mass flow rate calculated by mechanical ventilation object [kg/s]
)
{
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    static std::string const RoutineName("CalcMechVentController: ");
    static std::string const CurrentModuleObject(CurrentModuleObjects(static_cast<int>(CMO::MechVentilation)));

    // new local variables for DCV
    Real64 ZoneOAPeople; // Zone OA flow rate based on number of occupants [m3/s]
    Real64 ZoneOAArea;   // Zone OA flow rate based on space floor area [m3/s]
    Real64 ZoneOAFlow;   // Zone OA flow rate based on simple flow [m3/s]
    Real64 ZoneOAACH;    // Zone OA flow rate based on air changes per hour [m3/s]
    Real64 ZoneOABZ;     // Zone breathing-zone OA flow rate [m3/s]
    Real64 ZoneOAMin;    // Minimum Zone OA flow rate when the zone is unoccupied (i.e. ZoneOAPeople = 0)
    // used for "ProportionalControl" System outdoor air method
    Real64 ZoneOAMax; // Maximum Zone OA flow rate (ZoneOAPeople + ZoneOAArea)
    // used for "ProportionalControl" System outdoor air method
    Real64 ZoneOA;        // Zone OA flow rate [m3/s]
    Real64 ZoneOAFrac;    // Zone OA fraction (as a fraction of actual supply air flow rate)
    Real64 ZoneEz;        // Zone air distribution effectiveness
    Real64 ZoneSA;        // Zone supply air flow rate
    Real64 ZonePA;        // Zone primary air flow rate
    Real64 SysOAuc;       // System uncorrected OA flow rate
    Real64 SysOA;         // System supply OA volume flow rate [m3/s]
    Real64 SysOAMassFlow; // System supply OA mass flow rate [kg/s]
    Real64 SysEv;         // System ventilation efficiency
    Real64 NodeTemp;      // node temperature
    Real64 NodeHumRat;    // node humidity ratio
    Real64 MassFlowRate;  // Temporary variable
    Real64 ZoneLoad;      // Zone loads
    std::string ZoneName; // Zone name
    int OAIndex;          // index to design specification outdoor air objects
    int PeopleNum;
    Real64 ZoneMaxCO2;                // Breathing-zone CO2 concentartion
    Real64 ZoneMinCO2;                // Minimum CO2 concentration in zone
    Real64 ZoneContamControllerSched; // Schedule value for ZoneControl:ContaminantController
    Real64 CO2PeopleGeneration;       // CO2 generation from people at design level

    int PriNode;   // primary node of zone terminal unit
    int InletNode; // outlet node of zone terminal unit

    ZoneMaxCO2 = 0.0;
    ZoneMinCO2 = 0.0;
    ZoneOAMin = 0.0;
    ZoneOAMax = 0.0;
    ZoneContamControllerSched = 0.0;
    MechVentOAMassFlow = 0.0;

    // Apply mechanical ventilation only when it is available/allowed
    if (GetCurrentScheduleValue(state, this->SchPtr) > 0) {
        if (this->SystemOAMethod == SOAM_IAQP) {
            // IAQP for CO2 control
            SysOAMassFlow = 0.0;
            for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                int ZoneNum = this->VentMechZone(ZoneIndex);
                SysOAMassFlow += state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP *
                                 GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));
            }
            MechVentOAMassFlow = SysOAMassFlow;
        } else if (this->SystemOAMethod == SOAM_IAQPGC) {
            // IAQP for generic contaminant control
            SysOAMassFlow = 0.0;
            for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                int ZoneNum = this->VentMechZone(ZoneIndex);
                SysOAMassFlow += state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP *
                                 GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));
            }
            MechVentOAMassFlow = SysOAMassFlow;
        } else if (this->SystemOAMethod == SOAM_IAQPCOM) {
            // IAQP for both CO2 and generic contaminant control
            SysOAMassFlow = 0.0;
            for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                int ZoneNum = this->VentMechZone(ZoneIndex);
                SysOAMassFlow += state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToCO2SP *
                                 GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));
            }
            MechVentOAMassFlow = SysOAMassFlow;
            SysOAMassFlow = 0.0;
            for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                int ZoneNum = this->VentMechZone(ZoneIndex);
                SysOAMassFlow += state.dataContaminantBalance->ZoneSysContDemand(ZoneNum).OutputRequiredToGCSP *
                                 GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));
            }
            MechVentOAMassFlow = max(SysOAMassFlow, MechVentOAMassFlow);
        } else {
            // for system OA methods: Zone_Sum, VRP, CO2 methods
            // new code for DCV method complying with the VRP defined in ASHRAE Standard 62.1-2010

            // Loop through each zone first to calc uncorrected system OA flow rate
            SysOAuc = 0.0;
            SysOA = 0.0;
            for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                int ZoneNum = this->VentMechZone(ZoneIndex);
                auto const &curZone(state.dataHeatBal->Zone(ZoneNum));
                Real64 curZoneOASchValue = GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));

                // Calc the zone OA flow rate based on the people component
                // ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
                //  Checking DCV flag before calculating zone OA per person
                if (this->DCVFlag && this->SystemOAMethod != SOAM_ProportionalControlDesOcc) {
                    ZoneOAPeople = state.dataHeatBal->ZoneIntGain(ZoneNum).NOFOCC * curZone.Multiplier * curZone.ListMultiplier *
                                   this->ZoneOAPeopleRate(ZoneIndex) * curZoneOASchValue;
                } else {
                    ZoneOAPeople =
                        curZone.TotOccupants * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate(ZoneIndex) * curZoneOASchValue;
                }

                // Calc the zone OA flow rate based on the floor area component
                ZoneOAArea = curZone.FloorArea * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAAreaRate(ZoneIndex) * curZoneOASchValue;
                ZoneOAFlow = curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAFlowRate(ZoneIndex) * curZoneOASchValue;
                ZoneOAACH =
                    curZone.Multiplier * curZone.ListMultiplier * (this->ZoneOAACHRate(ZoneIndex) * curZone.Volume) * curZoneOASchValue / 3600.0;

                // Calc the breathing-zone OA flow rate
                OAIndex = this->ZoneDesignSpecOAObjIndex(ZoneIndex);
                if (OAIndex > 0) {
                    {
                        auto const SELECT_CASE_var(state.dataSize->OARequirements(OAIndex).OAFlowMethod);
                        if (SELECT_CASE_var == OAFlowPPer) {
                            ZoneOABZ = ZoneOAPeople;
                        } else if (SELECT_CASE_var == OAFlow) {
                            ZoneOABZ = ZoneOAFlow;
                        } else if (SELECT_CASE_var == OAFlowPerArea) {
                            ZoneOABZ = ZoneOAArea;
                        } else if (SELECT_CASE_var == OAFlowACH) {
                            ZoneOABZ = ZoneOAACH;
                        } else if (SELECT_CASE_var == OAFlowSum) {
                            ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH;
                        } else if (SELECT_CASE_var == OAFlowMax) {
                            ZoneOABZ = max(ZoneOAPeople, ZoneOAArea, ZoneOAFlow, ZoneOAACH);
                        } else {
                            ZoneOABZ = 0.0;
                        }
                    }
                } else {
                    ZoneOABZ = ZoneOAPeople;
                }

                if (this->SystemOAMethod == SOAM_ZoneSum) {
                    // Sum the zone OA flow rates and done
                    SysOA += ZoneOABZ;
                } else {
                    // Calc the uncorrected system OA flow rate - VRP and ProportionalControl
                    SysOAuc += ZoneOABZ;
                }
            }

            // get system supply air flow rate
            if (this->SystemOAMethod == SOAM_VRP || this->SystemOAMethod == SOAM_ProportionalControlSchOcc ||
                this->SystemOAMethod == SOAM_ProportionalControlDesOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOARate) {

                // System supply air flow rate is always greater than or equal the system outdoor air flow rate
                if ((SysSA > 0.0) && (SysSA < (SysOAuc * state.dataEnvrn->StdRhoAir))) SysSA = SysOAuc * state.dataEnvrn->StdRhoAir;

                // calc Xs - average outdoor air fraction
                if (SysSA > 0.0) {
                    Xs = (SysOAuc * state.dataEnvrn->StdRhoAir) / SysSA;
                } else {
                    Xs = 0.0;
                }

                // Loop through each zone again
                SysEv = 2.0; // starting with a big fraction
                for (int ZoneIndex = 1; ZoneIndex <= this->NumofVentMechZones; ++ZoneIndex) {
                    int ZoneNum = this->VentMechZone(ZoneIndex);
                    int ZoneEquipConfigNum = ZoneNum; // correspondence - 1:1 of ZoneEquipConfig to Zone index
                    ZoneEz = 0.0;

                    // Assign references
                    auto &curZone(state.dataHeatBal->Zone(ZoneNum));
                    auto &curZoneEquipConfig(state.dataZoneEquip->ZoneEquipConfig(ZoneEquipConfigNum));
                    auto &curZoneSysEnergyDemand(state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneEquipConfigNum));
                    ZoneName = curZone.Name;
                    Real64 curZoneOASchValue = GetCurrentScheduleValue(state, this->ZoneOASchPtr(ZoneIndex));

                    // Calc the zone OA flow rate based on the people component
                    // ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
                    //  Checking DCV flag before calculating zone OA per person
                    if (this->DCVFlag && this->SystemOAMethod != SOAM_ProportionalControlDesOcc) {
                        ZoneOAPeople = state.dataHeatBal->ZoneIntGain(ZoneNum).NOFOCC * curZone.Multiplier * curZone.ListMultiplier *
                                       this->ZoneOAPeopleRate(ZoneIndex) * curZoneOASchValue;
                    } else {
                        ZoneOAPeople = curZone.TotOccupants * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAPeopleRate(ZoneIndex) *
                                       curZoneOASchValue;
                        CO2PeopleGeneration = 0.0;
                        if (this->SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                            // Accumulate CO2 generation from people at design occupancy and current activity level
                            for (PeopleNum = 1; PeopleNum <= state.dataHeatBal->TotPeople; ++PeopleNum) {
                                if (state.dataHeatBal->People(PeopleNum).ZonePtr != ZoneNum) continue;
                                CO2PeopleGeneration += state.dataHeatBal->People(PeopleNum).NumberOfPeople *
                                                       state.dataHeatBal->People(PeopleNum).CO2RateFactor *
                                                       GetCurrentScheduleValue(state, state.dataHeatBal->People(PeopleNum).ActivityLevelPtr);
                            }
                        }
                    }

                    // Calc the zone OA flow rate based on the floor area component
                    ZoneOAArea =
                        curZone.FloorArea * curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAAreaRate(ZoneIndex) * curZoneOASchValue;
                    ZoneOAFlow = curZone.Multiplier * curZone.ListMultiplier * this->ZoneOAFlowRate(ZoneIndex) * curZoneOASchValue;
                    ZoneOAACH = curZone.Multiplier * curZone.ListMultiplier *
                                (this->ZoneOAACHRate(ZoneIndex) * state.dataHeatBal->Zone(ZoneIndex).Volume) * curZoneOASchValue / 3600.0;

                    // Calc the breathing-zone OA flow rate
                    OAIndex = this->ZoneDesignSpecOAObjIndex(ZoneIndex);
                    if (OAIndex > 0) {
                        {
                            auto const SELECT_CASE_var(state.dataSize->OARequirements(OAIndex).OAFlowMethod);
                            if (SELECT_CASE_var == OAFlowPPer) {
                                ZoneOABZ = ZoneOAPeople;
                            } else if (SELECT_CASE_var == OAFlow) {
                                ZoneOABZ = ZoneOAFlow;
                            } else if (SELECT_CASE_var == OAFlowPerArea) {
                                ZoneOABZ = ZoneOAArea;
                            } else if (SELECT_CASE_var == OAFlowACH) {
                                ZoneOABZ = ZoneOAACH;
                            } else if (SELECT_CASE_var == OAFlowSum) {
                                ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH;
                            } else if (SELECT_CASE_var == OAFlowMax) {
                                ZoneOABZ = max(ZoneOAPeople, ZoneOAArea, ZoneOAFlow, ZoneOAACH);
                            } else {
                                ZoneOABZ = 0.0;
                            }
                        }
                    }

                    // use the ventilation rate procedure in ASHRAE Standard 62.1-2007
                    // Calc the zone supplied OA flow rate counting the zone air distribution effectiveness
                    //  First check whether the zone air distribution effectiveness schedule exists, if yes uses it;
                    //   otherwise uses the inputs of zone distribution effectiveness in cooling mode or heating mode
                    int ADEffSchPtr = this->ZoneADEffSchPtr(ZoneIndex);
                    if (ADEffSchPtr > 0) {
                        // Get schedule value for the zone air distribution effectiveness
                        ZoneEz = GetCurrentScheduleValue(state, ADEffSchPtr);
                    } else {
                        ZoneLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(curZoneEquipConfig.ActualZoneNum).TotalOutputRequired;

                        // Zone in cooling mode
                        if (ZoneLoad < 0.0) ZoneEz = this->ZoneADEffCooling(ZoneIndex);

                        // Zone in heating mode
                        if (ZoneLoad > 0.0) ZoneEz = this->ZoneADEffHeating(ZoneIndex);
                    }
                    if (ZoneEz <= 0.0) {
                        // Enforce defaults
                        ZoneEz = 1.0;
                    }

                    // Calc zone supply OA flow rate
                    if (this->SystemOAMethod == SOAM_VRP) {
                        // the VRP case
                        ZoneOA = ZoneOABZ / ZoneEz;

                    } else if (this->SystemOAMethod == SOAM_ProportionalControlSchOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOcc ||
                               this->SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                        // Check whether "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController is specified
                        if (curZone.ZoneContamControllerSchedIndex > 0.0) {
                            // Check the availability schedule value for ZoneControl:ContaminantController
                            ZoneContamControllerSched = GetCurrentScheduleValue(state, curZone.ZoneContamControllerSchedIndex);
                            if (ZoneContamControllerSched > 0.0) {
                                ZoneOAMin = ZoneOAArea / ZoneEz;
                                ZoneOAMax = (ZoneOAArea + ZoneOAPeople) / ZoneEz;
                                if (this->SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                                    ZoneOAMax = ZoneOABZ / ZoneEz;
                                    if (this->OAPropCtlMinRateSchPtr(ZoneIndex) > 0) {
                                        ZoneOAMin = ZoneOAMax * GetCurrentScheduleValue(state, this->OAPropCtlMinRateSchPtr(ZoneIndex));
                                    } else {
                                        ZoneOAMin = ZoneOAMax;
                                    }
                                    if (ZoneOAMax < ZoneOAMin) {
                                        ZoneOAMin = ZoneOAMax;
                                        ++this->OAMaxMinLimitErrorCount;
                                        if (this->OAMaxMinLimitErrorCount < 2) {
                                            ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                            ShowContinueError(
                                                state,
                                                format("For System Outdoor Air Method = ProportionalControlBasedOnDesignOARate, maximum zone "
                                                       "outdoor air rate ({:.4R}), is not greater than minimum zone outdoor air rate ({:.4R}).",
                                                       ZoneOAMax,
                                                       ZoneOAMin));
                                            ShowContinueError(state,
                                                              " The minimum zone outdoor air rate is set to the maximum zone outdoor air rate. "
                                                              "Simulation continues...");
                                            ShowContinueErrorTimeStamp(state, "");
                                        } else {
                                            ShowRecurringWarningErrorAtEnd(
                                                state,
                                                CurrentModuleObject + " = \"" + this->Name +
                                                    "\", For System Outdoor Air Method = ProportionalControlBasedOnDesignOARate, maximum zone "
                                                    "outdoor air rate is not greater than minimum zone outdoor air rate. Error continues...",
                                                this->OAMaxMinLimitErrorIndex);
                                        }
                                    }
                                }

                                if (ZoneOAPeople > 0.0) {
                                    if (state.dataContaminantBalance->ZoneCO2GainFromPeople(ZoneNum) > 0.0) {
                                        if (curZone.ZoneMinCO2SchedIndex > 0.0) {
                                            // Take the schedule value of "Minimum Carbon Dioxide Concentration Schedule Name"
                                            // in the ZoneControl:ContaminantController
                                            ZoneMinCO2 = GetCurrentScheduleValue(state, curZone.ZoneMinCO2SchedIndex);
                                        } else {
                                            ZoneMinCO2 = state.dataContaminantBalance->OutdoorCO2;
                                        }

                                        // Calculate zone maximum target CO2 concentration in PPM
                                        if (this->SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                                            ZoneMaxCO2 = state.dataContaminantBalance->OutdoorCO2 +
                                                         (CO2PeopleGeneration * curZone.Multiplier * curZone.ListMultiplier * 1.0e6) / ZoneOAMax;
                                        } else if (curZone.ZoneMaxCO2SchedIndex > 0.0) {
                                            ZoneMaxCO2 = GetCurrentScheduleValue(state, curZone.ZoneMaxCO2SchedIndex);
                                        } else {
                                            ZoneMaxCO2 = state.dataContaminantBalance->OutdoorCO2 +
                                                         (state.dataContaminantBalance->ZoneCO2GainFromPeople(ZoneNum) * curZone.Multiplier *
                                                          curZone.ListMultiplier * 1.0e6) /
                                                             ZoneOAMax;
                                        }

                                        if (ZoneMaxCO2 <= ZoneMinCO2) {
                                            ++this->CO2MaxMinLimitErrorCount;
                                            if (this->SystemOAMethod == SOAM_ProportionalControlSchOcc) {
                                                if (this->CO2MaxMinLimitErrorCount < 2) {
                                                    ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                                    ShowContinueError(
                                                        state,
                                                        format("For System Outdoor Air Method = ProportionalControlBasedOnOccupancySchedule, "
                                                               "maximum target CO2 concentration ({:.2R}), is not greater than minimum target "
                                                               "CO2 concentration ({:.2R}).",
                                                               ZoneMaxCO2,
                                                               ZoneMinCO2));
                                                    ShowContinueError(state,
                                                                      "\"ProportionalControlBasedOnOccupancySchedule\" will not be modeled. "
                                                                      "Default \"VentilationRateProcedure\" will be modeled. Simulation "
                                                                      "continues...");
                                                    ShowContinueErrorTimeStamp(state, "");
                                                } else {
                                                    ShowRecurringWarningErrorAtEnd(state,
                                                                                   CurrentModuleObject + " = \"" + this->Name +
                                                                                       "\", For System Outdoor Air Method = "
                                                                                       "ProportionalControlBasedOnOccupancySchedule, maximum "
                                                                                       "target CO2 concentration is not greater than minimum "
                                                                                       "target CO2 concentration. Error continues...",
                                                                                   this->CO2MaxMinLimitErrorIndex);
                                                }
                                            }
                                            if (this->SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                                                if (this->CO2MaxMinLimitErrorCount < 2) {
                                                    ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                                    ShowContinueError(
                                                        state,
                                                        format("For System Outdoor Air Method = ProportionalControlBasedOnDesignOccupancy, "
                                                               "maximum target CO2 concentration ({:.2R}), is not greater than minimum target "
                                                               "CO2 concentration ({:.2R}).",
                                                               ZoneMaxCO2,
                                                               ZoneMinCO2));
                                                    ShowContinueError(state,
                                                                      "\"ProportionalControlBasedOnDesignOccupancy\" will not be modeled. "
                                                                      "Default \"VentilationRateProcedure\" will be modeled. Simulation "
                                                                      "continues...");
                                                    ShowContinueErrorTimeStamp(state, "");
                                                } else {
                                                    ShowRecurringWarningErrorAtEnd(state,
                                                                                   CurrentModuleObject + " = \"" + this->Name +
                                                                                       "\", For System Outdoor Air Method = "
                                                                                       "ProportionalControlBasedOnDesignOccupancy, maximum "
                                                                                       "target CO2 concentration is not greater than minimum "
                                                                                       "target CO2 concentration. Error continues...",
                                                                                   this->CO2MaxMinLimitErrorIndex);
                                                }
                                            }
                                            if (this->SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                                                if (this->CO2MaxMinLimitErrorCount < 2) {
                                                    ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                                    ShowContinueError(
                                                        state,
                                                        format("For System Outdoor Air Method = ProportionalControlBasedOnDesignOARate, maximum "
                                                               "target CO2 concentration ({:.2R}), is not greater than minimum target CO2 "
                                                               "concentration ({:.2R}).",
                                                               ZoneMaxCO2,
                                                               ZoneMinCO2));
                                                    ShowContinueError(state,
                                                                      "\"ProportionalControlBasedOnDesignOARate\" will not be modeled. Default "
                                                                      "\"VentilationRateProcedure\" will be modeled. Simulation continues...");
                                                    ShowContinueErrorTimeStamp(state, "");
                                                } else {
                                                    ShowRecurringWarningErrorAtEnd(state,
                                                                                   CurrentModuleObject + " = \"" + this->Name +
                                                                                       "\", For System Outdoor Air Method = "
                                                                                       "ProportionalControlBasedOnDesignOARate, maximum target "
                                                                                       "CO2 concentration is not greater than minimum target CO2 "
                                                                                       "concentration. Error continues...",
                                                                                   this->CO2MaxMinLimitErrorIndex);
                                                }
                                            }

                                            ZoneOA = ZoneOABZ / ZoneEz;
                                        } else {

                                            if (state.dataContaminantBalance->ZoneAirCO2(ZoneNum) <= ZoneMinCO2) {
                                                // Zone air CO2 concentration is less than minimum zone CO2 concentration, set the Zone OA flow
                                                // rate to minimum Zone OA flow rate when the zone is unoccupied
                                                ZoneOA = ZoneOAMin;
                                            } else if (state.dataContaminantBalance->ZoneAirCO2(ZoneNum) >= ZoneMaxCO2) {
                                                // Zone air CO2 concentration is greater than maximum zone CO2 concentration, set the Zone OA flow
                                                // rate to maximum Zone OA flow rate (i.e. ZoneOAArea + ZoneOAPeople)
                                                ZoneOA = ZoneOAMax;
                                            } else {
                                                // Zone air CO2 concentration is between maximum and minimum limits of zone CO2 concentration,
                                                // set Zone OA flow rate by proportionally adjusting between ZoneOAMin and ZoneOAMax
                                                ZoneOA = ZoneOAMin +
                                                         (ZoneOAMax - ZoneOAMin) * ((state.dataContaminantBalance->ZoneAirCO2(ZoneNum) - ZoneMinCO2) /
                                                                                    (ZoneMaxCO2 - ZoneMinCO2));
                                            }
                                        }
                                    } else {
                                        if (state.dataGlobal->DisplayExtraWarnings) {
                                            ++this->CO2GainErrorCount;
                                            if (this->SystemOAMethod == SOAM_ProportionalControlSchOcc) {
                                                if (this->CO2GainErrorCount < 2) {
                                                    ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                                    ShowContinueError(state,
                                                                      "For System Outdoor Air Method = "
                                                                      "ProportionalControlBasedOnOccupancySchedule, CO2 generation from people "
                                                                      "is not greater than zero. Occurs in Zone =\"" +
                                                                          curZone.Name + "\". ");
                                                    ShowContinueError(state,
                                                                      "\"ProportionalControlBasedOnOccupancySchedule\" will not be modeled. "
                                                                      "Default \"VentilationRateProcedure\" will be modeled. Simulation "
                                                                      "continues...");
                                                    ShowContinueErrorTimeStamp(state, "");
                                                } else {
                                                    ShowRecurringWarningErrorAtEnd(
                                                        state,
                                                        CurrentModuleObject + " = \"" + this->Name +
                                                            "\", For System Outdoor Air Method = ProportionalControlBasedOnOccupancySchedule, "
                                                            "CO2 generation from people is not greater than zero. Error continues...",
                                                        this->CO2GainErrorIndex);
                                                }
                                            }
                                            if (this->SystemOAMethod == SOAM_ProportionalControlDesOcc) {
                                                if (this->CO2GainErrorCount < 2) {
                                                    ShowSevereError(state, RoutineName + CurrentModuleObject + " = \"" + this->Name + "\".");
                                                    ShowContinueError(state,
                                                                      "For System Outdoor Air Method = "
                                                                      "ProportionalControlBasedOnDesignOccupancy, CO2 generation from people is "
                                                                      "not greater than zero. Occurs in Zone =\"" +
                                                                          curZone.Name + "\". ");
                                                    ShowContinueError(state,
                                                                      "\"ProportionalControlBasedOnDesignOccupancy\" will not be modeled. "
                                                                      "Default \"VentilationRateProcedure\" will be modeled. Simulation "
                                                                      "continues...");
                                                    ShowContinueErrorTimeStamp(state, "");
                                                } else {
                                                    ShowRecurringWarningErrorAtEnd(
                                                        state,
                                                        CurrentModuleObject + " = \"" + this->Name +
                                                            "\", For System Outdoor Air Method = ProportionalControlBasedOnDesignOccupancy, CO2 "
                                                            "generation from people is not greater than zero. Error continues...",
                                                        this->CO2GainErrorIndex);
                                                }
                                            }
                                        }
                                        ZoneOA = ZoneOABZ / ZoneEz;
                                    }
                                } else {
                                    // ZoneOAPeople is less than or equal to zero
                                    ZoneOA = ZoneOABZ / ZoneEz;
                                }
                            } else {
                                // ZoneControl:ContaminantController is scheduled off (not available)
                                ZoneOA = ZoneOABZ / ZoneEz;
                            }
                        } else {
                            // "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController not found
                            ZoneOA = ZoneOABZ / ZoneEz;
                        }
                        SysOA = SysOA + ZoneOA;
                    }

                    // Get the zone supply air flow rate
                    ZoneSA = 0.0;
                    ZonePA = 0.0;
                    Ep = 1.0;
                    if (ZoneEquipConfigNum > 0) {
                        for (int InNodeIndex = 1; InNodeIndex <= curZoneEquipConfig.NumInletNodes; ++InNodeIndex) {
                            // Assume primary air is always stored at the AirDistUnitCool (cooling deck if dual duct)
                            PriNode = curZoneEquipConfig.AirDistUnitCool(InNodeIndex).InNode;
                            if (PriNode > 0) {
                                NodeTemp = state.dataLoopNodes->Node(PriNode).Temp;
                                NodeHumRat = state.dataLoopNodes->Node(PriNode).HumRat;
                                MassFlowRate = state.dataLoopNodes->Node(PriNode).MassFlowRate;
                            } else {
                                MassFlowRate = 0.0;
                            }
                            // total primary air to terminal units of the zone
                            if (MassFlowRate > 0.0)
                                ZonePA += MassFlowRate / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, NodeTemp, NodeHumRat);

                            // or InletNode = ZoneEquipConfig(ZoneEquipConfigNum)%AirDistUnitCool(InNodeIndex)%OutNode
                            InletNode = curZoneEquipConfig.InletNode(InNodeIndex);
                            if (InletNode > 0) {
                                NodeTemp = state.dataLoopNodes->Node(InletNode).Temp;
                                NodeHumRat = state.dataLoopNodes->Node(InletNode).HumRat; // ZoneAirHumRat(ZoneNum)
                                MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
                            } else {
                                MassFlowRate = 0.0;
                            }
                            // total supply air to the zone
                            if (MassFlowRate > 0.0)
                                ZoneSA += MassFlowRate / PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, NodeTemp, NodeHumRat);
                        }

                        // calc zone primary air fraction
                        if (ZoneSA > 0.0) Ep = ZonePA / ZoneSA;
                        if (Ep > 1.0) Ep = 1.0;
                    }

                    // Calc the zone OA fraction = Zone OA flow rate / Zone supply air flow rate
                    if (ZoneSA > 0.0) {
                        ZoneOAFrac = ZoneOA / ZoneSA;
                        // Zone OA fraction cannot be more than 1
                        if (ZoneOAFrac > 1.0) ZoneOAFrac = 1.0;
                    } else {
                        ZoneOAFrac = 0.0;
                    }

                    // added for TRACE - zone maximum OA fraction - calculate the adjustment factor for the TU/zone supply air flow
                    // only for VRP system OA method
                    curZoneSysEnergyDemand.SupplyAirAdjustFactor = 1.0;

                    if (this->SystemOAMethod == SOAM_VRP) {
                        if (ZoneOAFrac > this->ZoneMaxOAFraction) {
                            if (this->ZoneMaxOAFraction > 0.0) {
                                curZoneSysEnergyDemand.SupplyAirAdjustFactor = ZoneOAFrac / this->ZoneMaxOAFraction;
                            } else {
                                curZoneSysEnergyDemand.SupplyAirAdjustFactor = 1.0;
                            }

                            // cap zone OA fraction at the maximum specified
                            ZoneOAFrac = this->ZoneMaxOAFraction;
                        }
                    }

                    // Zone air secondary recirculation fraction
                    Er = this->ZoneSecondaryRecirculation(ZoneIndex);
                    if (Er > 0.0) {
                        // multi-path ventilation system using VRP
                        Fa = Ep + (1.0 - Ep) * Er;
                        Fb = Ep;
                        Fc = 1.0 - (1.0 - ZoneEz) * (1.0 - Er) * (1.0 - Ep);

                        // Calc zone ventilation efficiency
                        if (Fa > 0.0) {
                            Evz = 1.0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa;
                        } else {
                            Evz = 1.0;
                        }
                    } else {
                        // single-path ventilation system
                        Evz = 1.0 + Xs - ZoneOAFrac;
                    }

                    // calc system ventilation efficiency = Minimum of zone ventilation efficiency
                    if (Evz < 0.0) Evz = 0.0;
                    if (Evz < SysEv) SysEv = Evz;

                } // zone loop

                // Calc the system supply OA flow rate counting the system ventilation efficiency
                if (SysEv <= 0.0) SysEv = 1.0;

                // Calc system outdoor air requirement
                if (this->SystemOAMethod == SOAM_ProportionalControlSchOcc || this->SystemOAMethod == SOAM_ProportionalControlDesOcc ||
                    this->SystemOAMethod == SOAM_ProportionalControlDesOARate) {
                    SysOA = SysOA / SysEv;
                } else {
                    SysOA = SysOAuc / SysEv;
                }
            }

            // Finally calc the system supply OA mass flow rate
            MechVentOAMassFlow = SysOA * state.dataEnvrn->StdRhoAir;
        }

    } else {
        MechVentOAMassFlow = 0.0;
    }
}

void OAControllerProps::CalcOAEconomizer(EnergyPlusData &state,
                                         int const AirLoopNum,
                                         Real64 const OutAirMinFrac,
                                         Real64 &OASignal,
                                         bool &HighHumidityOperationFlag,
                                         bool const FirstHVACIteration)
{
    using General::SolveRoot;
    using SetPointManager::GetCoilFreezingCheckFlag;

    static std::string const RoutineName("CalcOAEconomizer: ");
    static std::string const CurrentModuleObject(CurrentModuleObjects(static_cast<int>(CMO::OAController)));
    int const MaxIte(500);                 // Maximum number of iterations
    Real64 const Acc(0.0001);              // Accuracy of result
    bool AirLoopEconoLockout;              // Economizer lockout flag
    bool AirLoopNightVent;                 // Night Ventilation flag for air loop
    bool EconomizerOperationFlag;          // TRUE if OA economizer is active
    Real64 EconomizerAirFlowScheduleValue; // value of economizer operation schedule (push-button type control schedule)
    Real64 MaximumOAFracBySetPoint;        // The maximum OA fraction due to freezing cooling coil check
    Real64 OutAirSignal;                   // Used to set OA mass flow rate
    auto &Par = state.dataMixedAir->Par;   // Par(1) = mixed air node number //Tuned Made static
                                           // Par(2) = return air node number
                                           // Par(3) = outside air node number
                                           // Par(4) = mixed air mass flow rate
                                           // Par(5) = FirstHVACIteration
                                           // Par(6) = AirLoopNum
    int SolFla;                            // Flag of solver
    Real64 lowFlowResiduum;                // result of low OA flow calculation (Tmixedair_sp - Tmixedair)
    Real64 highFlowResiduum;               // result of high OA flow calculation (Tmixedair_sp - Tmixedair)
    Real64 minOAFrac;

    if (AirLoopNum > 0) {
        // Check lockout with heating for any airloop - will lockout economizer even on airloops without a unitary system
        if (this->Lockout == iLockoutType::LockoutWithHeatingPossible) {
            // For all system types (even ones that don't set AirLoopEconoLockout) lock out economizer if unfavorable for heating
            if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CheckHeatRecoveryBypassStatus &&
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysComponentsSimulated) {

                if (this->MixedAirTempAtMinOAFlow <= state.dataLoopNodes->Node(this->MixNode).TempSetPoint) {
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconomizerFlowLocked = true;
                    // this->OAMassFlow = AirLoopFlow( AirLoopNum ).MinOutAir;
                    // AirLoopFlow( AirLoopNum ).OAFrac = this->OAMassFlow / this->MixMassFlow;
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoLockout = true;
                    EconomizerOperationFlag = false;
                } else {
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconomizerFlowLocked = false;
                    this->HRHeatingCoilActive = 0;
                }
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).CheckHeatRecoveryBypassStatus = false;
            }
        }
    }

    if (AirLoopNum > 0) {
        AirLoopEconoLockout = state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoLockout;
        AirLoopNightVent = state.dataAirLoop->AirLoopControlInfo(AirLoopNum).NightVent;
    } else {
        AirLoopEconoLockout = false;
        AirLoopNightVent = false;
    }

    // Define an outside air signal
    if (this->MixedAirSPMNum > 0) {
        this->CoolCoilFreezeCheck = GetCoilFreezingCheckFlag(state, this->MixedAirSPMNum);
    } else {
        this->CoolCoilFreezeCheck = false;
    }

    if (std::abs(this->RetTemp - this->InletTemp) > SmallTempDiff) {
        OutAirSignal = (this->RetTemp - this->MixSetTemp) / (this->RetTemp - this->InletTemp);
        if (this->CoolCoilFreezeCheck) {
            this->MaxOAFracBySetPoint = 0.0;
            MaximumOAFracBySetPoint = OutAirSignal;
        }
    } else {
        if (this->RetTemp - this->MixSetTemp < 0.0) {
            if (this->RetTemp - this->InletTemp >= 0.0) {
                OutAirSignal = -1.0;
            } else {
                OutAirSignal = 1.0;
            }
        } else {
            if (this->RetTemp - this->InletTemp >= 0.0) {
                OutAirSignal = 1.0;
            } else {
                OutAirSignal = -1.0;
            }
        }
    }
    OutAirSignal = min(max(OutAirSignal, OutAirMinFrac), 1.0);

    // If no economizer, set to minimum and disable economizer and high humidity control
    if (this->Econo == iEconoOp::NoEconomizer) {
        OutAirSignal = OutAirMinFrac;
        EconomizerOperationFlag = false;
        EconomizerAirFlowScheduleValue = 0.0;
        HighHumidityOperationFlag = false;
    } else if (this->MaxOA < SmallAirVolFlow) {
        OutAirSignal = OutAirMinFrac;
        EconomizerOperationFlag = false;
        EconomizerAirFlowScheduleValue = 0.0;
        HighHumidityOperationFlag = false;
    } else if (AirLoopEconoLockout) {
        OutAirSignal = OutAirMinFrac;
        EconomizerOperationFlag = false;
        EconomizerAirFlowScheduleValue = 0.0;
        HighHumidityOperationFlag = false;
    } else {
        // Changed by Amit for new implementation
        // Otherwise do the limit checks
        EconomizerOperationFlag = true;
        // Outside air temp greater than mix air setpoint
        if (this->InletTemp > this->MixSetTemp) {
            OutAirSignal = 1.0;
        }
        // Return air temp limit
        if (this->Econo == iEconoOp::DifferentialDryBulb) {
            if (this->InletTemp > this->RetTemp) {
                OutAirSignal = OutAirMinFrac;
                EconomizerOperationFlag = false;
            }
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // Return air enthalpy limit
        if (this->Econo == iEconoOp::DifferentialEnthalpy) {
            if (this->InletEnth > this->RetEnth) {
                OutAirSignal = OutAirMinFrac;
                EconomizerOperationFlag = false;
            }
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // Outside air temperature limit
        if (this->Econo == iEconoOp::FixedDryBulb) {
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // Fixed Enthalpy limit
        if (this->Econo == iEconoOp::FixedEnthalpy) {
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // FIXED DEW POINT AND DRY BULB TEMPERATURE STRATEGY
        if (this->Econo == iEconoOp::FixedDewPointAndDryBulb) {
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // ELECRONIC ENTHALPY, HUMIDITY RATIO CURVE
        if (this->Econo == iEconoOp::ElectronicEnthalpy) {
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }
        // Differential dry bulb and enthalpy strategy
        if (this->Econo == iEconoOp::DifferentialDryBulbAndEnthalpy) {
            if (this->InletTemp > this->RetTemp) {
                OutAirSignal = OutAirMinFrac;
                EconomizerOperationFlag = false;
            }
            if (this->InletEnth > this->RetEnth) {
                OutAirSignal = OutAirMinFrac;
                EconomizerOperationFlag = false;
            }
            this->Checksetpoints(state, OutAirMinFrac, OutAirSignal, EconomizerOperationFlag);
        }

        if (this->TempLowLim != BlankNumeric && this->OATemp < this->TempLowLim) {
            OutAirSignal = OutAirMinFrac;
            EconomizerOperationFlag = false;
        }
        // Increase air flow for humidity control
        // (HumidistatZoneNum is greater than 0 IF High Humidity Control Flag = YES, checked in GetInput)
        if (this->HumidistatZoneNum > 0) {
            //   IF humidistat senses a moisture load check to see if modifying air flow is appropriate, otherwise disable modified air flow
            if (state.dataZoneEnergyDemand->ZoneSysMoistureDemand(this->HumidistatZoneNum).TotalOutputRequired < 0.0) {
                //     IF OAController is not allowed to modify air flow during high outdoor humrat condition, then disable modified air flow
                //     if indoor humrat is less than or equal to outdoor humrat
                if (!this->ModifyDuringHighOAMoisture &&
                    (state.dataLoopNodes->Node(this->NodeNumofHumidistatZone).HumRat - this->OAHumRat) <= DataHVACGlobals::SmallHumRatDiff) {
                    HighHumidityOperationFlag = false;
                } else {
                    HighHumidityOperationFlag = true;
                }
            } else {
                HighHumidityOperationFlag = false;
            }
        } else {
            HighHumidityOperationFlag = false;
        }

        // Check time of day economizer schedule, enable economizer if schedule value > 0
        EconomizerAirFlowScheduleValue = 0.0;
        if (this->EconomizerOASchedPtr > 0) {
            EconomizerAirFlowScheduleValue = GetCurrentScheduleValue(state, this->EconomizerOASchedPtr);
            if (EconomizerAirFlowScheduleValue > 0.0) {
                EconomizerOperationFlag = true;
                OutAirSignal = 1.0;
            }
        }
    }

    // OutAirSignal will not give exactly the correct mixed air temperature (equal to the setpoint) since
    // it was calculated using the approximate method of sensible energy balance. Now we have to get the
    // accurate result using a full mass, enthalpy and moisture balance and iteration.
    if (OutAirSignal > OutAirMinFrac && OutAirSignal < 1.0 && this->MixMassFlow > VerySmallMassFlow &&
        this->ControllerType_Num == iControllerType::ControllerOutsideAir && !AirLoopNightVent) {

        if (AirLoopNum > 0) {

            if (state.dataAirLoop->OutsideAirSys(state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysNum).NumComponents == 1) {
                // no need to simulate OA System if only a mixer is used in the OutsideAirSystem

                Par(1) = this->MixNode;
                Par(2) = this->RetNode;
                Par(3) = this->InletNode;
                Par(4) = this->MixMassFlow;
                SolveRoot(state, Acc, MaxIte, SolFla, OASignal, MixedAirControlTempResidual, OutAirMinFrac, 1.0, Par);
                if (SolFla < 0) {
                    OASignal = OutAirSignal;
                }

            } else {

                // simulate OA System if equipment exists other than the mixer (e.g., heating/cooling coil, HX, ect.)

                // 1 - check min OA flow result
                if (this->FixedMin) {
                    state.dataLoopNodes->Node(this->OANode).MassFlowRate =
                        min(max(this->ExhMassFlow, OutAirMinFrac * state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply),
                            state.dataLoopNodes->Node(this->MixNode).MassFlowRate);
                    state.dataLoopNodes->Node(this->RelNode).MassFlowRate =
                        max(state.dataLoopNodes->Node(this->OANode).MassFlowRate - this->ExhMassFlow, 0.0);
                    // save actual OA flow frac for use as min value for RegulaFalsi call
                    minOAFrac = max(OutAirMinFrac, state.dataLoopNodes->Node(this->OANode).MassFlowRate / this->MixMassFlow);
                } else {
                    state.dataLoopNodes->Node(this->OANode).MassFlowRate =
                        max(this->ExhMassFlow, OutAirMinFrac * state.dataLoopNodes->Node(this->MixNode).MassFlowRate);
                    state.dataLoopNodes->Node(this->RelNode).MassFlowRate =
                        max(state.dataLoopNodes->Node(this->OANode).MassFlowRate - this->ExhMassFlow, 0.0);
                    // save actual OA flow frac for use as min value for RegulaFalsi call
                    minOAFrac = max(OutAirMinFrac, state.dataLoopNodes->Node(this->OANode).MassFlowRate / this->MixMassFlow);
                }
                SimOASysComponents(state, state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysNum, FirstHVACIteration, AirLoopNum);
                lowFlowResiduum = state.dataLoopNodes->Node(this->MixNode).TempSetPoint - state.dataLoopNodes->Node(this->MixNode).Temp;

                // 2 - check max OA flow result
                state.dataLoopNodes->Node(this->OANode).MassFlowRate = max(this->ExhMassFlow, state.dataLoopNodes->Node(this->MixNode).MassFlowRate);
                state.dataLoopNodes->Node(this->RelNode).MassFlowRate =
                    max(state.dataLoopNodes->Node(this->OANode).MassFlowRate - this->ExhMassFlow, 0.0);
                SimOASysComponents(state, state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OASysNum, FirstHVACIteration, AirLoopNum);
                highFlowResiduum = state.dataLoopNodes->Node(this->MixNode).TempSetPoint - state.dataLoopNodes->Node(this->MixNode).Temp;

                // 3 - test to ensure RegulaFalsi can find an answer
                if ((sign(lowFlowResiduum) == sign(highFlowResiduum))) {
                    OASignal = OutAirSignal;
                } else {
                    // 4 - find result
                    Par(1) = this->MixNode;
                    Par(2) = this->RelNode;
                    Par(3) = this->OANode;
                    Par(4) = this->MixMassFlow;
                    Par(5) = 0.0;
                    if (FirstHVACIteration) Par(5) = 1.0;
                    Par(6) = double(AirLoopNum);

                    SolveRoot(state, (Acc / 10.0), MaxIte, SolFla, OASignal, MultiCompControlTempResidual, minOAFrac, 1.0, Par);
                    if (SolFla < 0) { // if RegulaFalsi fails to find a solution, returns -1 or -2, set to existing OutAirSignal
                        OASignal = OutAirSignal;
                    }
                }
            }

        } else {

            Par(1) = this->MixNode;
            Par(2) = this->RetNode;
            Par(3) = this->InletNode;
            Par(4) = this->MixMassFlow;
            SolveRoot(state, Acc, MaxIte, SolFla, OASignal, MixedAirControlTempResidual, OutAirMinFrac, 1.0, Par);
            if (SolFla < 0) {
                OASignal = OutAirSignal;
            }
        }

    } else {
        OASignal = OutAirSignal;
    }

    // Economizer choice "Bypass" forces minimum OA except when high humidity air flow is active based on indoor RH
    if (this->EconBypass && EconomizerAirFlowScheduleValue == 0.0) {
        OASignal = OutAirMinFrac;
    }

    // Set outdoor air signal based on OA flow ratio if high humidity air flow is enabled
    if (HighHumidityOperationFlag) {
        if (this->MixMassFlow > 0.0) {
            //   calculate the actual ratio of outside air to mixed air so the magnitude of OA during high humidity control is correct
            OASignal = max(OutAirMinFrac, (this->HighRHOAFlowRatio * this->MaxOAMassFlowRate / this->MixMassFlow));
            this->OALimitingFactor = limitFactorHighHum;
        }
    }

    if (this->CoolCoilFreezeCheck) {
        MaximumOAFracBySetPoint = min(max(MaximumOAFracBySetPoint, 0.0), 1.0);
        this->MaxOAFracBySetPoint = MaximumOAFracBySetPoint;

        // This should not be messing with OutAirMinFrac, freeze protection should only limit economizer operation
        // if (MaximumOAFracBySetPoint < OutAirMinFrac) {
        // OutAirMinFrac = MaximumOAFracBySetPoint;
        //    if (AirLoopNum > 0) AirLoopFlow(AirLoopNum).MinOutAir = OutAirMinFrac * this->MixMassFlow;
        //}
        if (MaximumOAFracBySetPoint < OASignal) {
            OASignal = MaximumOAFracBySetPoint;
            this->OALimitingFactor = limitFactorLimits;
        }
        if (OutAirMinFrac > OASignal) {
            OASignal = OutAirMinFrac;
            this->OALimitingFactor = limitFactorLimits;
        }
    }

    if (AirLoopNum > 0) {

        // Set the air loop economizer and high humidity control flags.
        state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconoActive = EconomizerOperationFlag;
        state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HighHumCtrlActive = HighHumidityOperationFlag;
        if (state.dataAirLoop->AirLoopControlInfo(AirLoopNum).EconomizerFlowLocked) {
            this->OAMassFlow = state.dataAirLoop->AirLoopFlow(AirLoopNum).MinOutAir;
            state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFrac = this->OAMassFlow / this->MixMassFlow;
            state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFlow = this->OAMassFlow;
        }

        // Check heat exchanger bypass control
        state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass = false;
        this->HeatRecoveryBypassStatus = 0;
        if (EconomizerOperationFlag) {
            if (this->HeatRecoveryBypassControlType == BypassWhenWithinEconomizerLimits) {
                state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass = true;
                this->HeatRecoveryBypassStatus = 1;
            } else if (this->HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum) {
                Real64 OAMassFlowMin = OutAirMinFrac * state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
                Real64 OAMassFlowActual = OASignal * this->MixMassFlow;
                Real64 reasonablySmallMassFlow = 1e-6;
                if (OAMassFlowActual > (OAMassFlowMin + reasonablySmallMassFlow)) {
                    state.dataAirLoop->AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass = true;
                    this->HeatRecoveryBypassStatus = 1;
                }
            }
        }
    }

    // Set economizer report variable and status flag
    if (this->Econo == iEconoOp::NoEconomizer) {
        // No economizer
        this->EconomizerStatus = 0;
        this->EconoActive = false;
    } else {
        // With economizer.
        if (EconomizerOperationFlag) {
            // Economizer is enabled
            this->EconomizerStatus = 1;
            this->EconoActive = true;
            if ((OASignal > OutAirMinFrac) && !HighHumidityOperationFlag) {
                this->OALimitingFactor = limitFactorEconomizer;
            }
        } else {
            // Economizer is disabled
            this->EconomizerStatus = 0;
            this->EconoActive = false;
        }
    }

    // Night ventilation control overrides economizer and high humidity control.
    if (AirLoopNightVent) {
        OASignal = 1.0;
        this->OALimitingFactor = limitFactorNightVent;
    }

    // Set high humidity control report variable and status flag
    if (HighHumidityOperationFlag) {
        this->HighHumCtrlStatus = 1;
        this->HighHumCtrlActive = true;
    } else {
        this->HighHumCtrlStatus = 0;
        this->HighHumCtrlActive = false;
    }
}
void CalcOAMixer(EnergyPlusData &state, int const OAMixerNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Calculate the mixed air flow and conditions

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // Using/Aliasing
    using Psychrometrics::PsyTdbFnHW;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 RecircMassFlowRate;
    Real64 RecircPressure;
    Real64 RecircEnthalpy;
    Real64 RecircHumRat;

    // Define a recirculation mass flow rate
    RecircMassFlowRate = state.dataMixedAir->OAMixer(OAMixerNum).RetMassFlowRate - state.dataMixedAir->OAMixer(OAMixerNum).RelMassFlowRate;
    // In certain low flow conditions the return air mass flow rate can be below the outside air value established
    //  by the user.  This check will ensure that this condition does not result in unphysical air properties.
    if (RecircMassFlowRate < 0.0) {
        RecircMassFlowRate = 0.0;
        state.dataMixedAir->OAMixer(OAMixerNum).RelMassFlowRate = state.dataMixedAir->OAMixer(OAMixerNum).RetMassFlowRate;
    }

    // Pass through the return air conditions to the relief air stream.  The return air is "split" to
    // the relief air and the recirculation air.
    state.dataMixedAir->OAMixer(OAMixerNum).RelTemp = state.dataMixedAir->OAMixer(OAMixerNum).RetTemp;
    state.dataMixedAir->OAMixer(OAMixerNum).RelHumRat = state.dataMixedAir->OAMixer(OAMixerNum).RetHumRat;
    state.dataMixedAir->OAMixer(OAMixerNum).RelEnthalpy = state.dataMixedAir->OAMixer(OAMixerNum).RetEnthalpy;
    state.dataMixedAir->OAMixer(OAMixerNum).RelPressure = state.dataMixedAir->OAMixer(OAMixerNum).RetPressure;
    RecircPressure = state.dataMixedAir->OAMixer(OAMixerNum).RetPressure;
    RecircEnthalpy = state.dataMixedAir->OAMixer(OAMixerNum).RetEnthalpy;
    RecircHumRat = state.dataMixedAir->OAMixer(OAMixerNum).RetHumRat;
    // The recirculation air and the outside air are mixed to form the mixed air stream
    state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate = state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate + RecircMassFlowRate;
    // Check for zero flow
    if (state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate <= VerySmallMassFlow) {
        state.dataMixedAir->OAMixer(OAMixerNum).MixEnthalpy = state.dataMixedAir->OAMixer(OAMixerNum).RetEnthalpy;
        state.dataMixedAir->OAMixer(OAMixerNum).MixHumRat = state.dataMixedAir->OAMixer(OAMixerNum).RetHumRat;
        state.dataMixedAir->OAMixer(OAMixerNum).MixPressure = state.dataMixedAir->OAMixer(OAMixerNum).RetPressure;
        state.dataMixedAir->OAMixer(OAMixerNum).MixTemp = state.dataMixedAir->OAMixer(OAMixerNum).RetTemp;
        return;
    }

    state.dataMixedAir->OAMixer(OAMixerNum).MixEnthalpy =
        (RecircMassFlowRate * RecircEnthalpy +
         state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate * state.dataMixedAir->OAMixer(OAMixerNum).OAEnthalpy) /
        state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
    state.dataMixedAir->OAMixer(OAMixerNum).MixHumRat = (RecircMassFlowRate * RecircHumRat + state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate *
                                                                                                 state.dataMixedAir->OAMixer(OAMixerNum).OAHumRat) /
                                                        state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
    state.dataMixedAir->OAMixer(OAMixerNum).MixPressure =
        (RecircMassFlowRate * RecircPressure +
         state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate * state.dataMixedAir->OAMixer(OAMixerNum).OAPressure) /
        state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
    // Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
    state.dataMixedAir->OAMixer(OAMixerNum).MixTemp =
        PsyTdbFnHW(state.dataMixedAir->OAMixer(OAMixerNum).MixEnthalpy, state.dataMixedAir->OAMixer(OAMixerNum).MixHumRat);
}

// End of Calculation/Simulation Section of the Module
//******************************************************************************

// Beginning Sizing Section of the Module
//******************************************************************************

void OAControllerProps::SizeOAController(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing OAController Components for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays.

    // Using/Aliasing

    using HVACHXAssistedCoolingCoil::GetHXCoilType;
    using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
    using WaterCoils::SetCoilDesFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const CurrentModuleObject(CurrentModuleObjects(static_cast<int>(CMO::OAController)));

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OAFlowRatio;   // Used for error checking
    std::string CompType; // Component type
    std::string CompName; // Component name
    std::string CoilName;
    std::string CoilType;
    int CompNum;
    bool ErrorsFound;

    ErrorsFound = false;
    if (this->MaxOA == AutoSize) {

        if (state.dataSize->CurSysNum > 0) {

            {
                auto const SELECT_CASE_var(this->ControllerType_Num);

                if (SELECT_CASE_var == iControllerType::ControllerOutsideAir) {

                    CheckSysSizing(state, CurrentModuleObject, this->Name);

                    {
                        auto const SELECT_CASE_var1(state.dataSize->CurDuctType);
                        if (SELECT_CASE_var1 == Main) {
                            this->MaxOA = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        } else if (SELECT_CASE_var1 == Cooling) {
                            this->MaxOA = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesCoolVolFlow;
                        } else if (SELECT_CASE_var1 == Heating) {
                            this->MaxOA = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesHeatVolFlow;
                        } else if (SELECT_CASE_var1 == Other) {
                            this->MaxOA = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        } else {
                            this->MaxOA = state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesMainVolFlow;
                        }
                    }

                } else if (SELECT_CASE_var == iControllerType::ControllerStandAloneERV) {

                } else {
                }
            }

        } else if (state.dataSize->CurZoneEqNum > 0) {

            {
                auto const SELECT_CASE_var(this->ControllerType_Num);

                if (SELECT_CASE_var == iControllerType::ControllerOutsideAir) {

                    CheckZoneSizing(state, CurrentModuleObject, this->Name);
                    this->MaxOA = max(state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolVolFlow,
                                      state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesHeatVolFlow);

                } else if (SELECT_CASE_var == iControllerType::ControllerStandAloneERV) {

                } else {
                }
            }
        }

        if (this->MaxOA < SmallAirVolFlow) {
            this->MaxOA = 0.0;
        }

        BaseSizer::reportSizerOutput(state, CurrentModuleObject, this->Name, "Maximum Outdoor Air Flow Rate [m3/s]", this->MaxOA);
    }

    if (this->MinOA == AutoSize) {

        if (state.dataSize->CurSysNum > 0) {

            CheckSysSizing(state, CurrentModuleObject, this->Name);
            if (state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow >= SmallAirVolFlow) {
                this->MinOA = min(state.dataSize->FinalSysSizing(state.dataSize->CurSysNum).DesOutAirVolFlow, this->MaxOA);
            } else {
                this->MinOA = 0.0;
            }
        }

        BaseSizer::reportSizerOutput(state, CurrentModuleObject, this->Name, "Minimum Outdoor Air Flow Rate [m3/s]", this->MinOA);

        if (this->HumidistatZoneNum > 0 && this->FixedMin) {
            if (this->MaxOA > 0.0) {
                OAFlowRatio = this->MinOA / this->MaxOA;
                if (this->HighRHOAFlowRatio < OAFlowRatio) {
                    ShowWarningError(state, CurrentModuleObject + " \"" + this->Name + "\"");
                    ShowContinueError(state, "... A fixed minimum outdoor air flow rate and high humidity control have been specified.");
                    ShowContinueError(state,
                                      "... The High Humidity Outdoor Air Flow Ratio is less than the ratio of the outdoor air controllers "
                                      "minimum to maximum outside air flow rate.");
                    ShowContinueError(state, format("... Controller minimum flow rate = {:.4T} m3/s.", this->MinOA));
                    ShowContinueError(state, format("... Controller maximum flow rate = {:.4T} m3/s.", this->MaxOA));
                    ShowContinueError(state, format("... Controller minimum to maximum flow ratio = {:.4T}.", OAFlowRatio));
                    ShowContinueError(state, format("... High humidity control flow ratio = {:.4T}.", this->HighRHOAFlowRatio));
                }
            }
        }
    }
    // If there is an outside air system, loop over components in the OA system; pass the design air flow rate
    // to the coil components that don't have design air flow as an input.
    if (state.dataSize->CurOASysNum > 0) {
        for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(state.dataSize->CurOASysNum).NumComponents; ++CompNum) {
            CompType = state.dataAirLoop->OutsideAirSys(state.dataSize->CurOASysNum).ComponentType(CompNum);
            CompName = state.dataAirLoop->OutsideAirSys(state.dataSize->CurOASysNum).ComponentName(CompNum);
            if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY") ||
                UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER") ||
                UtilityRoutines::SameString(CompType, "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED")) {
                if (UtilityRoutines::SameString(CompType, "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED")) {
                    CoilName = GetHXDXCoilName(state, CompType, CompName, ErrorsFound);
                    CoilType = GetHXCoilType(state, CompType, CompName, ErrorsFound);
                } else {
                    CoilName = CompName;
                    CoilType = CompType;
                }
                SetCoilDesFlow(state, CoilType, CoilName, this->MinOA, ErrorsFound);
            }
        } // End of component loop
    }
    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

// End of Sizing Section of the Module
//******************************************************************************

// Beginning Update/Reporting Section of the Module
//******************************************************************************

void OAControllerProps::UpdateOAController(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       Shirey/Raustad FSEC, June 2003
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Move the results of CalcOAController to the affected nodes

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // Using/Aliasing
    using namespace DataLoopNode;
    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutAirNodeNum;
    int InletAirNodeNum;
    int RelAirNodeNum;
    int RetAirNodeNum;

    OutAirNodeNum = this->OANode;
    InletAirNodeNum = this->InletNode;
    RelAirNodeNum = this->RelNode;
    RetAirNodeNum = this->RetNode;

    if (this->ControllerType_Num == iControllerType::ControllerOutsideAir) {
        // The outside air controller sets the outside air flow rate and the relief air flow rate
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && (this->ManageDemand) &&
            (this->OAMassFlow > this->DemandLimitFlowRate)) {
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRate = this->DemandLimitFlowRate;
            state.dataLoopNodes->Node(InletAirNodeNum).MassFlowRate = this->DemandLimitFlowRate;
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRateMaxAvail = this->DemandLimitFlowRate;
        } else {
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRate = this->OAMassFlow;
            state.dataLoopNodes->Node(InletAirNodeNum).MassFlowRate = this->OAMassFlow;
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRateMaxAvail = this->OAMassFlow;
        }
        state.dataLoopNodes->Node(RelAirNodeNum).MassFlowRate = this->RelMassFlow;
    } else {
        // The ERV controller sets the supply and secondary inlet node information for the Stand Alone ERV
        // Currently, the Stand Alone ERV only has constant air flows (supply and exhaust), and these are
        // already set in HVACStandAloneERV.cc (subroutine init). Therefore, these flow assignments below are
        // currently redundant but may be useful in the future as mass flow rates can vary based on the controller signal.
        if (!state.dataGlobal->WarmupFlag && !state.dataGlobal->DoingSizing && (this->ManageDemand) &&
            (this->OAMassFlow > this->DemandLimitFlowRate)) {
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRate = this->DemandLimitFlowRate;
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRateMaxAvail = this->DemandLimitFlowRate;
        } else {
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRate = this->OAMassFlow;
            state.dataLoopNodes->Node(OutAirNodeNum).MassFlowRateMaxAvail = this->OAMassFlow;
        }
        state.dataLoopNodes->Node(RetAirNodeNum).MassFlowRate = state.dataLoopNodes->Node(this->RetNode).MassFlowRate;
        state.dataLoopNodes->Node(RetAirNodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(this->RetNode).MassFlowRate;
    }
}

void UpdateOAMixer(EnergyPlusData &state, int const OAMixerNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   Oct 1998
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Move the results of CalcOAMixer to the affected nodes

    // METHODOLOGY EMPLOYED:

    // REFERENCES:

    // Using/Aliasing
    using namespace DataLoopNode;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int MixNode;
    int RelNode;
    int RetNode;

    MixNode = state.dataMixedAir->OAMixer(OAMixerNum).MixNode;
    RelNode = state.dataMixedAir->OAMixer(OAMixerNum).RelNode;
    RetNode = state.dataMixedAir->OAMixer(OAMixerNum).RetNode;
    // Move mixed air data to the mixed air node
    state.dataLoopNodes->Node(MixNode).MassFlowRate = state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
    state.dataLoopNodes->Node(MixNode).Temp = state.dataMixedAir->OAMixer(OAMixerNum).MixTemp;
    state.dataLoopNodes->Node(MixNode).HumRat = state.dataMixedAir->OAMixer(OAMixerNum).MixHumRat;
    state.dataLoopNodes->Node(MixNode).Enthalpy = state.dataMixedAir->OAMixer(OAMixerNum).MixEnthalpy;
    state.dataLoopNodes->Node(MixNode).Press = state.dataMixedAir->OAMixer(OAMixerNum).MixPressure;
    state.dataLoopNodes->Node(MixNode).MassFlowRateMaxAvail = state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
    // Move the relief air data to the relief air node
    state.dataLoopNodes->Node(RelNode).MassFlowRate = state.dataMixedAir->OAMixer(OAMixerNum).RelMassFlowRate;
    state.dataLoopNodes->Node(RelNode).Temp = state.dataMixedAir->OAMixer(OAMixerNum).RelTemp;
    state.dataLoopNodes->Node(RelNode).HumRat = state.dataMixedAir->OAMixer(OAMixerNum).RelHumRat;
    state.dataLoopNodes->Node(RelNode).Enthalpy = state.dataMixedAir->OAMixer(OAMixerNum).RelEnthalpy;
    state.dataLoopNodes->Node(RelNode).Press = state.dataMixedAir->OAMixer(OAMixerNum).RelPressure;
    state.dataLoopNodes->Node(RelNode).MassFlowRateMaxAvail = state.dataMixedAir->OAMixer(OAMixerNum).RelMassFlowRate;

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataLoopNodes->Node(RelNode).CO2 = state.dataLoopNodes->Node(RetNode).CO2;
        if (state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate <= VerySmallMassFlow) {
            state.dataLoopNodes->Node(MixNode).CO2 = state.dataLoopNodes->Node(RetNode).CO2;
        } else {
            state.dataLoopNodes->Node(MixNode).CO2 =
                ((state.dataLoopNodes->Node(RetNode).MassFlowRate - state.dataLoopNodes->Node(RelNode).MassFlowRate) *
                     state.dataLoopNodes->Node(RetNode).CO2 +
                 state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate * state.dataContaminantBalance->OutdoorCO2) /
                state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
        }
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataLoopNodes->Node(RelNode).GenContam = state.dataLoopNodes->Node(RetNode).GenContam;
        if (state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate <= VerySmallMassFlow) {
            state.dataLoopNodes->Node(MixNode).GenContam = state.dataLoopNodes->Node(RetNode).GenContam;
        } else {
            state.dataLoopNodes->Node(MixNode).GenContam =
                ((state.dataLoopNodes->Node(RetNode).MassFlowRate - state.dataLoopNodes->Node(RelNode).MassFlowRate) *
                     state.dataLoopNodes->Node(RetNode).GenContam +
                 state.dataMixedAir->OAMixer(OAMixerNum).OAMassFlowRate * state.dataContaminantBalance->OutdoorGC) /
                state.dataMixedAir->OAMixer(OAMixerNum).MixMassFlowRate;
        }
    }
}

void ReportOAMixer([[maybe_unused]] int const OAMixerNum) // unused1208
{

    // SUBROUTINE ARGUMENT DEFINITIONS
}

// End of Sizing Section of the Module
//******************************************************************************

// Beginning Utility Section of the Module
//******************************************************************************

Real64 MixedAirControlTempResidual(EnergyPlusData &state,
                                   Real64 const OASignal,     // Relative outside air flow rate (0 to 1)
                                   Array1D<Real64> const &Par // par(1) = mixed node number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April, 2003
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function TMixSetPoint - TMix.
    // Economizer damper position (OASignal) is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Using a mass and energy balance at the mixed air node, calculates the
    // mixed air temperature given the outside air damper position.

    // REFERENCES:

    // Using/Aliasing
    using Psychrometrics::PsyTdbFnHW;

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // par(2) = return node number
    // par(3) = outside air node number
    // par(4) = mixed air flow rate

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int MixNode;               // mixed air node number
    int RetNode;               // return air node number
    int OANode;                // outside air node number
    Real64 MixMassFlowRate;    // mixed air mass flow rare [kg/s]
    Real64 OAMassFlowRate;     // outside air mass flow rate [kg/s]
    Real64 RecircMassFlowRate; // recirculated air mass flow rate [kg/s]
    Real64 RecircEnth;         // recirculated air specific enthalpy [J/kg]
    Real64 RecircHumRat;       // recirculated air humidity ratio [kg water/kg dry air]
    Real64 MixEnth;            // mixed air specific enthalpy [J/kg]
    Real64 MixHumRat;          // mixed air humidity ratio [kg water/kg dry air]
    Real64 MixTemp;            // mixed air temperature [C]

    MixNode = int(Par(1));
    RetNode = int(Par(2));
    OANode = int(Par(3));
    MixMassFlowRate = Par(4);

    OAMassFlowRate = OASignal * MixMassFlowRate;
    RecircMassFlowRate = max(MixMassFlowRate - OAMassFlowRate, 0.0);
    RecircEnth = state.dataLoopNodes->Node(RetNode).Enthalpy;
    RecircHumRat = state.dataLoopNodes->Node(RetNode).HumRat;
    MixEnth = (RecircMassFlowRate * RecircEnth + OAMassFlowRate * state.dataLoopNodes->Node(OANode).Enthalpy) / MixMassFlowRate;
    MixHumRat = (RecircMassFlowRate * RecircHumRat + OAMassFlowRate * state.dataLoopNodes->Node(OANode).HumRat) / MixMassFlowRate;
    MixTemp = PsyTdbFnHW(MixEnth, MixHumRat);
    Residuum = state.dataLoopNodes->Node(MixNode).TempSetPoint - MixTemp;

    return Residuum;
}

Real64 MultiCompControlTempResidual(EnergyPlusData &state,
                                    Real64 const OASignal,     // Relative outside air flow rate (0 to 1)
                                    Array1D<Real64> const &Par // par(1) = mixed node number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   Nov, 2016
    //       MODIFIED
    //       RE-ENGINEERED

    // PURPOSE OF THIS FUNCTION:
    // Calculates residual function TMixSetPoint - TMix.
    // Economizer damper position (OASignal) is being varied to zero the residual.

    // METHODOLOGY EMPLOYED:
    // Simulate the OA System to determine actual mixed air condition, calculates the
    // mixed air temperature given the outside air damper position.

    // REFERENCES:

    // Using/Aliasing
    using Psychrometrics::PsyTdbFnHW;

    // Return value
    Real64 Residuum; // residual to be minimized to zero

    // Argument array dimensioning

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // pao(1) = mixed air node number
    // par(2) = relief air node number
    // par(3) = outside air node number
    // par(4) = mixed air flow rate
    // par(5) = FirstHVACIteration
    // par(6) = AirLoopNum index

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int MixNode;            // mixed air node number
    int RelNode;            // return air node number
    int OANode;             // outside air node number
    Real64 MixMassFlowRate; // mixed air mass flow rare [kg/s]
    Real64 OAMassFlowRate;  // outside air mass flow rate [kg/s]
    Real64 ExhMassFlow;
    bool FirstHVACIteration;
    int AirloopNum;
    int OASysNum;

    MixNode = int(Par(1));
    RelNode = int(Par(2));
    OANode = int(Par(3));
    MixMassFlowRate = Par(4);
    FirstHVACIteration = (Par(5) == 1.0);
    AirloopNum = int(Par(6));
    OASysNum = state.dataAirLoop->AirLoopControlInfo(AirloopNum).OASysNum;
    ExhMassFlow = state.dataAirLoop->AirLoopControlInfo(AirloopNum).ZoneExhMassFlow;

    OAMassFlowRate = max(ExhMassFlow, OASignal * MixMassFlowRate);
    state.dataLoopNodes->Node(OANode).MassFlowRate = OAMassFlowRate; // set OA node mass flow rate
    state.dataLoopNodes->Node(RelNode).MassFlowRate =
        max(OAMassFlowRate - ExhMassFlow, 0.0); // set relief node mass flow rate to maintain mixer continuity calcs

    SimOASysComponents(state, OASysNum, FirstHVACIteration, AirloopNum);

    Residuum = state.dataLoopNodes->Node(MixNode).TempSetPoint - state.dataLoopNodes->Node(MixNode).Temp;

    return Residuum;
}

Array1D_int GetOAMixerNodeNumbers(EnergyPlusData &state,
                                  std::string const &OAMixerName, // must match OA mixer names for the OA mixer type
                                  bool &ErrorsFound               // set to true if problem
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Richard Raustad
    //       DATE WRITTEN   June 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given OA mixer and returns the node numbers.  If
    // incorrect OA mixer name is given, ErrorsFound is returned as true
    // as zero.

    // Return value
    Array1D_int OANodeNumbers(4); // return OA mixer nodes

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int WhichOAMixer;

    // Obtains and Allocates OA mixer related parameters from input file
    if (state.dataMixedAir->GetOAMixerInputFlag) { // First time subroutine has been entered
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    WhichOAMixer = UtilityRoutines::FindItemInList(OAMixerName, state.dataMixedAir->OAMixer);
    if (WhichOAMixer != 0) {
        OANodeNumbers(1) = state.dataMixedAir->OAMixer(WhichOAMixer).InletNode;
        OANodeNumbers(2) = state.dataMixedAir->OAMixer(WhichOAMixer).RelNode;
        OANodeNumbers(3) = state.dataMixedAir->OAMixer(WhichOAMixer).RetNode;
        OANodeNumbers(4) = state.dataMixedAir->OAMixer(WhichOAMixer).MixNode;
    }

    if (WhichOAMixer == 0) {
        ShowSevereError(state, "GetOAMixerNodeNumbers: Could not find OA Mixer = \"" + OAMixerName + "\"");
        ErrorsFound = true;
        OANodeNumbers = 0;
    }

    return OANodeNumbers;
}

int GetNumOAMixers(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of OA mixers is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int NumberOfOAMixers;

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

    if (state.dataMixedAir->GetOAMixerInputFlag) { // First time subroutine has been entered
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    NumberOfOAMixers = state.dataMixedAir->NumOAMixers;

    return NumberOfOAMixers;
}

int GetNumOAControllers(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of OA Controllers is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int NumberOfOAControllers;

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

    if (state.dataMixedAir->AllocateOAControllersFlag) {
        // Make sure OAControllers are allocated
        AllocateOAControllers(state);
    }

    NumberOfOAControllers = state.dataMixedAir->NumOAControllers;

    return NumberOfOAControllers;
}

int GetOAMixerReliefNodeNumber(EnergyPlusData &state, int const OAMixerNum) // Which Mixer
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the relief node number of indicated
    // mixer is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing

    // Return value
    int ReliefNodeNumber; // Relief Node Number

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

    if (state.dataMixedAir->GetOAMixerInputFlag) { // First time subroutine has been entered
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    if (OAMixerNum > state.dataMixedAir->NumOAMixers) {
        ShowFatalError(state,
                       format("GetOAMixerReliefNodeNumber: Requested Mixer #={}, which is > number of OA Mixers={}",
                              OAMixerNum,
                              state.dataMixedAir->NumOAMixers));
    }

    ReliefNodeNumber = state.dataMixedAir->OAMixer(OAMixerNum).RelNode;

    return ReliefNodeNumber;
}

int GetOASysControllerListIndex(EnergyPlusData &state, int const OASysNumber) // OA Sys Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the Controller List index of the indicated
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int OASysControllerListNum; // OA Sys Controller List index

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

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OASysControllerListNum = state.dataAirLoop->OutsideAirSys(OASysNumber).ControllerListNum;

    return OASysControllerListNum;
}

int GetOASysNumSimpControllers(EnergyPlusData &state, int const OASysNumber) // OA Sys Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of Controller:Simple objects in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int OASysNumSimpControllers; // number of Controller:Simple objects in this OA System

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

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OASysNumSimpControllers = state.dataAirLoop->OutsideAirSys(OASysNumber).NumSimpleControllers;

    return OASysNumSimpControllers;
}

int GetOASysNumHeatingCoils(EnergyPlusData &state, int const OASysNumber) // OA Sys Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of heating coils in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int NumHeatingCoils; // number of heating coils in this OA System

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    std::string CompType;
    std::string CompName;
    bool Sim(false);
    bool FirstHVACIteration(false);
    bool OAHeatingCoil(false);
    bool OACoolingCoil(false);
    int CompNum;
    int AirLoopNum(0);
    bool OAHX(false);

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumHeatingCoils = 0;
    for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(OASysNumber).NumComponents; ++CompNum) {
        CompType = state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType(CompNum);
        CompName = state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentName(CompNum);
        SimOAComponent(state,
                       CompType,
                       CompName,
                       static_cast<MixedAir::ComponentType>(state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType_Num(CompNum)),
                       FirstHVACIteration,
                       state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentIndex(CompNum),
                       AirLoopNum,
                       Sim,
                       OASysNumber,
                       OAHeatingCoil,
                       OACoolingCoil,
                       OAHX);
        if (OAHeatingCoil) {
            ++NumHeatingCoils;
        }
    }

    return NumHeatingCoils;
}

int GetOASysNumHXs(EnergyPlusData &state, int const OASysNumber)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl, Rongpeng Zhang
    //       DATE WRITTEN   Oct. 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of heat recovery exchangers in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int NumHX; // number of heat exchangers in this OA System

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int CompNum;
    int CompNum_end;

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumHX = 0;

    auto const &componentType_Num = state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType_Num;
    for (CompNum = 1, CompNum_end = state.dataAirLoop->OutsideAirSys(OASysNumber).NumComponents; CompNum <= CompNum_end; ++CompNum) {
        int const componentTypeNum = componentType_Num(CompNum);
        if (static_cast<int>(ComponentType::HeatXchngr) == componentTypeNum || static_cast<int>(ComponentType::Desiccant) == componentTypeNum) {
            ++NumHX;
        }
    }

    return NumHX;
}

int GetOASysNumCoolingCoils(EnergyPlusData &state, int const OASysNumber) // OA Sys Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   May 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of cooling coils in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int NumCoolingCoils; // number of cooling coils in this OA System

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:

    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    std::string CompType;
    std::string CompName;
    bool Sim(false);
    bool FirstHVACIteration(false);
    bool OAHeatingCoil(false);
    bool OACoolingCoil(false);
    int CompNum;
    int AirLoopNum(0);
    bool OAHX(false);

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumCoolingCoils = 0;
    for (CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(OASysNumber).NumComponents; ++CompNum) {
        CompType = state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType(CompNum);
        CompName = state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentName(CompNum);
        SimOAComponent(state,
                       CompType,
                       CompName,
                       static_cast<MixedAir::ComponentType>(state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType_Num(CompNum)),
                       FirstHVACIteration,
                       state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentIndex(CompNum),
                       AirLoopNum,
                       Sim,
                       OASysNumber,
                       OAHeatingCoil,
                       OACoolingCoil,
                       OAHX);
        if (OACoolingCoil) {
            ++NumCoolingCoils;
        }
    }

    return NumCoolingCoils;
}

int GetOASystemNumber(EnergyPlusData &state, std::string const &OASysName) // OA Sys Name
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the OA System number of indicated
    // OA System is returned.

    // Return value
    int OASysNumber; // OA Sys Number

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OASysNumber = UtilityRoutines::FindItemInList(OASysName, state.dataAirLoop->OutsideAirSys);

    return OASysNumber;
}

int FindOAMixerMatchForOASystem(EnergyPlusData &state, int const OASysNumber) // Which OA System
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the matched mixer number is found.
    // Note -- only the first is looked at for an Outside Air System.

    // Return value
    int OAMixerNumber; // Mixer Number

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int OACompNum;

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    OAMixerNumber = 0;
    if (OASysNumber > 0 && OASysNumber <= state.dataAirLoop->NumOASystems) {
        for (OACompNum = 1; OACompNum <= state.dataAirLoop->OutsideAirSys(OASysNumber).NumComponents; ++OACompNum) {
            if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentType(OACompNum), "OUTDOORAIR:MIXER")) {
                OAMixerNumber = UtilityRoutines::FindItemInList(state.dataAirLoop->OutsideAirSys(OASysNumber).ComponentName(OACompNum),
                                                                state.dataMixedAir->OAMixer);
                break;
            }
        }
    }

    return OAMixerNumber;
}

int GetOAMixerIndex(EnergyPlusData &state, std::string const &OAMixerName) // Which Mixer
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   December 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the mixer index of indicated
    // mixer is returned.

    // Return value
    int OAMixerIndex; // Mixer Index

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    OAMixerIndex = UtilityRoutines::FindItem(OAMixerName, state.dataMixedAir->OAMixer);

    if (OAMixerIndex == 0) {
        ShowSevereError(state, "GetOAMixerIndex: Could not find OutdoorAir:Mixer, Name=\"" + OAMixerName + "\"");
    }

    return OAMixerIndex;
}

int GetOAMixerInletNodeNumber(EnergyPlusData &state, int const OAMixerNumber) // Which Mixer
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the mixer inlet node number of indicated
    // mixer is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int OAMixerInletNodeNumber; // Mixer Inlet Node Number

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

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    OAMixerInletNodeNumber = 0;
    if (OAMixerNumber > 0 && OAMixerNumber <= state.dataMixedAir->NumOAMixers) {
        OAMixerInletNodeNumber = state.dataMixedAir->OAMixer(OAMixerNumber).InletNode;
    }

    return OAMixerInletNodeNumber;
}

int GetOAMixerReturnNodeNumber(EnergyPlusData &state, int const OAMixerNumber) // Which Mixer
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   December 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the mixer return node number of indicated
    // mixer is returned.

    // METHODOLOGY EMPLOYED:
    // followed Linda Lawrie's GetOAMixerInletNodeNumber

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int OAMixerReturnNodeNumber; // Mixer Inlet Node Number

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

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    OAMixerReturnNodeNumber = 0;
    if (OAMixerNumber > 0 && OAMixerNumber <= state.dataMixedAir->NumOAMixers) {
        OAMixerReturnNodeNumber = state.dataMixedAir->OAMixer(OAMixerNumber).RetNode;
    }

    return OAMixerReturnNodeNumber;
}

int GetOAMixerMixedNodeNumber(EnergyPlusData &state, int const OAMixerNumber) // Which Mixer
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Brent Griffith
    //       DATE WRITTEN   December 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the mixer mixed air node number of indicated
    // mixer is returned.

    // METHODOLOGY EMPLOYED:
    // followed Linda Lawrie's GetOAMixerInletNodeNumber

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int OAMixerMixedNodeNumber; // Mixer Inlet Node Number

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

    if (state.dataMixedAir->GetOAMixerInputFlag) {
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    OAMixerMixedNodeNumber = 0;
    if (OAMixerNumber > 0 && OAMixerNumber <= state.dataMixedAir->NumOAMixers) {
        OAMixerMixedNodeNumber = state.dataMixedAir->OAMixer(OAMixerNumber).MixNode;
    }

    return OAMixerMixedNodeNumber;
}

bool CheckForControllerWaterCoil(EnergyPlusData &state,
                                 std::string const &ControllerType, // should be passed in as UPPERCASE
                                 std::string const &ControllerName  // should be passed in as UPPERCASE
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine checks the controller list for existance of the
    // reference coil.

    // Return value
    bool OnControllerList; // true if found on controller list

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Num;
    int CompNum;

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OnControllerList = false;

    for (Num = 1; Num <= state.dataMixedAir->NumControllerLists; ++Num) {
        for (CompNum = 1; CompNum <= state.dataMixedAir->ControllerLists(Num).NumControllers; ++CompNum) {

            if (!UtilityRoutines::SameString(state.dataMixedAir->ControllerLists(Num).ControllerType(CompNum), ControllerType)) continue;
            if (!UtilityRoutines::SameString(state.dataMixedAir->ControllerLists(Num).ControllerName(CompNum), ControllerName)) continue;
            OnControllerList = true;
            break;
        }
    }

    return OnControllerList;
}

void CheckControllerLists(EnergyPlusData &state, bool &ErrFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine checks for a "dangling" controller list (AirLoopHVAC:ControllerList).
    // It must be either found on a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem.

    // Using/Aliasing

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CheckControllerLists");
    static std::string const CurrentModuleObject("AirLoopHVAC:ControllerList");
    static std::string const AirLoopObject("AirLoopHVAC");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;
    int NumNumbers;
    int NumControllers;
    int NumAirLoop;
    std::string ControllerListName;
    int Item;
    int IOStat;
    int Found;
    int Count;
    int Loop;
    std::string AirLoopName;

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumControllers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    NumAirLoop = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, AirLoopObject);
    AirLoopName = "";

    for (Item = 1; Item <= NumControllers; ++Item) {

        state.dataInputProcessing->inputProcessor->getObjectItem(
            state, CurrentModuleObject, Item, state.dataIPShortCut->cAlphaArgs, NumAlphas, state.dataIPShortCut->rNumericArgs, NumNumbers, IOStat);
        ControllerListName = state.dataIPShortCut->cAlphaArgs(1);
        Count = 0;

        // Check AirLoopHVAC -- brute force, get each AirLoopHVAC

        for (Loop = 1; Loop <= NumAirLoop; ++Loop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state, AirLoopObject, Loop, state.dataIPShortCut->cAlphaArgs, NumAlphas, state.dataIPShortCut->rNumericArgs, NumNumbers, IOStat);
            if (state.dataIPShortCut->cAlphaArgs(2) != ControllerListName) continue;
            ++Count;
            if (Count == 1) AirLoopName = state.dataIPShortCut->cAlphaArgs(1);
        }

        //  Now check AirLoopHVAC and AirLoopHVAC:OutdoorAirSystem
        Found = 0;
        if (state.dataAirLoop->NumOASystems > 0) {
            Found = UtilityRoutines::FindItemInList(ControllerListName, state.dataAirLoop->OutsideAirSys, &OutsideAirSysProps::ControllerListName);
            if (Found > 0) ++Count;
        }

        if (Count == 0) {
            ShowSevereError(state,
                            CurrentModuleObject + "=\"" + ControllerListName +
                                "\" is not referenced on a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem object.");
            ErrFound = true;
        } else if (Count > 1) {
            ShowSevereError(state,
                            CurrentModuleObject + "=\"" + ControllerListName +
                                "\" has too many references on AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem objects.");
            if (Found > 0) {
                ShowContinueError(state, "...AirLoopHVAC:OutdoorAirSystem=\"" + state.dataAirLoop->OutsideAirSys(Found).Name + "\".");
            }
            ShowContinueError(state, "...also on AirLoopHVAC=\"" + AirLoopName + "\".");
            ErrFound = true;
        }
    }
}

void CheckOAControllerName(
    EnergyPlusData &state, std::string &OAControllerName, std::string const &ObjectType, std::string const &FieldName, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   October 2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // When OA Controller data is gotten from other routines, must check to make sure
    // new name doesn't duplicate.  (Essentially a pass through to call Verify Name)
    // Currently, this is only called from HVACStandAlongERV::GetStandaloneERV()

    if (state.dataMixedAir->AllocateOAControllersFlag) {
        // Make sure OAControllers are allocated
        AllocateOAControllers(state);
    }

    GlobalNames::VerifyUniqueInterObjectName(
        state, state.dataMixedAir->OAControllerUniqueNames, OAControllerName, ObjectType, FieldName, ErrorsFound);
}

void OAControllerProps::Checksetpoints(EnergyPlusData &state,
                                       Real64 const OutAirMinFrac,   // Local variable used to calculate min OA fraction
                                       Real64 &OutAirSignal,         // Used to set OA mass flow rate
                                       bool &EconomizerOperationFlag // logical used to show economizer status
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Amit bhansali
    //       DATE WRITTEN   August 2008?
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks the setpoints of the upper limits of temperatures, limit enthalpy
    // Limit dew point, Enthalpy curve

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // Using/Aliasing
    using CurveManager::CurveValue;
    using Psychrometrics::PsyTdpFnWPb;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OADPTemp; // Dew Point Temperature calculation

    if (this->TempLim != BlankNumeric && this->OATemp > this->TempLim) {
        OutAirSignal = OutAirMinFrac;
        EconomizerOperationFlag = false;
    }
    // Outside air enthalpy limit
    if (this->EnthLim != BlankNumeric && this->OAEnth > this->EnthLim) {
        OutAirSignal = OutAirMinFrac;
        EconomizerOperationFlag = false;
    }

    if (this->DPTempLim != BlankNumeric) {
        OADPTemp = PsyTdpFnWPb(state, this->OAHumRat, this->OAPress);
        if (OADPTemp > this->DPTempLim) {
            OutAirSignal = OutAirMinFrac;
            EconomizerOperationFlag = false;
        }
    }

    if (this->EnthalpyCurvePtr > 0) {
        if (this->OAHumRat > CurveValue(state, this->EnthalpyCurvePtr, this->OATemp)) {
            OutAirSignal = OutAirMinFrac;
            EconomizerOperationFlag = false;
        }
    }
}

int GetNumOASystems(EnergyPlusData &state)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Get Number of OA Systems, After making sure get input is done

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:
    // na

    // Return value
    int NumberOfOASystems; // Number of OA Systems

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

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumberOfOASystems = state.dataAirLoop->NumOASystems;

    return NumberOfOASystems;
}

int GetOACompListNumber(EnergyPlusData &state, int const OASysNum) // OA Sys Number
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Heejin Cho
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the OA System number of indicated
    // OA System is returned.

    // Return value
    int NumOACompList; // OA Comp Number

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    NumOACompList = state.dataAirLoop->OutsideAirSys(OASysNum).NumComponents;

    return NumOACompList;
}

std::string GetOACompName(EnergyPlusData &state,
                          int const OASysNum, // OA Sys Number
                          int const InListNum // In-list Number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Heejin Cho
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of heating coils in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    std::string OACompName;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:
    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OACompName = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentName(InListNum);

    return OACompName;
}

std::string GetOACompType(EnergyPlusData &state,
                          int const OASysNum, // OA Sys Number
                          int const InListNum // In-list Number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Heejin Cho
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of heating coils in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    std::string OACompType;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:
    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OACompType = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType(InListNum);

    return OACompType;
}

int GetOACompTypeNum(EnergyPlusData &state,
                     int const OASysNum, // OA Sys Number
                     int const InListNum // In-list Number
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Heejin Cho
    //       DATE WRITTEN   November 2010
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // After making sure get input is done, the number of heating coils in the
    // OA System is returned.

    // METHODOLOGY EMPLOYED:
    // na

    // REFERENCES:
    // na

    // USE STATEMENTS:

    // Return value
    int OACompTypeNum;

    // Locals
    // FUNCTION ARGUMENT DEFINITIONS:
    // FUNCTION PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS:
    // na

    // DERIVED TYPE DEFINITIONS:
    // na

    // FUNCTION LOCAL VARIABLE DECLARATIONS:

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }

    OACompTypeNum = state.dataAirLoop->OutsideAirSys(OASysNum).ComponentType_Num(InListNum);

    return OACompTypeNum;
}

int GetOAMixerNumber(EnergyPlusData &state, std::string const &OAMixerName // must match OA mixer names for the OA mixer type
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   Feb. 2018

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given OA mixer and returns the OAMixer number.  If
    // incorrect OA mixer name is given, ErrorsFound is returned as true

    int WhichOAMixer;

    // Obtains and Allocates OA mixer related parameters from input file
    if (state.dataMixedAir->GetOAMixerInputFlag) { // First time subroutine has been entered
        GetOAMixerInputs(state);
        state.dataMixedAir->GetOAMixerInputFlag = false;
    }

    WhichOAMixer = UtilityRoutines::FindItemInList(OAMixerName, state.dataMixedAir->OAMixer);

    return WhichOAMixer;
}
// End of Utility Section of the Module
//******************************************************************************

} // namespace EnergyPlus::MixedAir
