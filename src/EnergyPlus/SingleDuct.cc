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
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ReportCoilSelection.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SingleDuct.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

namespace EnergyPlus::SingleDuct {

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
using namespace DataLoopNode;
using BranchNodeConnections::SetUpCompSets;
using BranchNodeConnections::TestCompSet;
using DataHVACGlobals::ATMixer_InletSide;
using DataHVACGlobals::ATMixer_SupplySide;
using DataHVACGlobals::DualSetPointWithDeadBand;
using DataHVACGlobals::SingleCoolingSetPoint;
using DataHVACGlobals::SingleHeatCoolSetPoint;
using DataHVACGlobals::SingleHeatingSetPoint;
using DataHVACGlobals::SmallAirVolFlow;
using DataHVACGlobals::SmallLoad;
using DataHVACGlobals::SmallMassFlow;
using namespace DataSizing;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using namespace FluidProperties;
using namespace ScheduleManager;
using namespace SteamCoils;

static constexpr std::string_view fluidNameSteam("STEAM");
static constexpr std::string_view fluidNameWater("WATER");

void SimulateSingleDuct(
    EnergyPlusData &state, std::string_view CompName, bool const FirstHVACIteration, int const ZoneNum, int const ZoneNodeNum, int &CompIndex)
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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int SysNum; // The Sys that you are currently loading input into

    // Obtains and Allocates Sys related parameters from input file
    if (state.dataSingleDuct->GetInputFlag) { // First time subroutine has been entered
        GetSysInput(state);
        state.dataSingleDuct->GetInputFlag = false;
    }

    // Find the correct SysNumber with the Component Name
    if (CompIndex == 0) {
        SysNum = UtilityRoutines::FindItemInList(CompName, state.dataSingleDuct->sd_airterminal, &SingleDuctAirTerminal::SysName);
        if (SysNum == 0) {
            ShowFatalError(state, "SimulateSingleDuct: System not found=" + std::string{CompName});
        }
        CompIndex = SysNum;
    } else {
        SysNum = CompIndex;
        if (SysNum > state.dataSingleDuct->NumSDAirTerminal || SysNum < 1) {
            ShowFatalError(state,
                           format("SimulateSingleDuct: Invalid CompIndex passed={}, Number of Systems={}, System name={}",
                                  CompIndex,
                                  state.dataSingleDuct->NumSDAirTerminal,
                                  CompName));
        }
        if (state.dataSingleDuct->CheckEquipName(SysNum)) {
            if (CompName != state.dataSingleDuct->sd_airterminal(SysNum).SysName) {
                ShowFatalError(state,
                               format("SimulateSingleDuct: Invalid CompIndex passed={}, System name={}, stored System Name for that index={}",
                                      CompIndex,
                                      CompName,
                                      state.dataSingleDuct->sd_airterminal(SysNum).SysName));
            }
            state.dataSingleDuct->CheckEquipName(SysNum) = false;
        }
    }

    auto &thisATU(state.dataSingleDuct->sd_airterminal(SysNum));

    state.dataSize->TermUnitSingDuct = true;
    state.dataSize->CurTermUnitSizingNum = state.dataDefineEquipment->AirDistUnit(thisATU.ADUNum).TermUnitSizingNum;

    // With the correct SysNum Initialize the system
    thisATU.InitSys(state, FirstHVACIteration); // Initialize all Sys related parameters

    // Calculate the Correct Sys Model with the current SysNum
    switch (thisATU.SysType_Num) {
    case SysType::SingleDuctConstVolReheat: // AirTerminal:SingleDuct:ConstantVolume:Reheat
        thisATU.SimConstVol(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);
        break;
    case SysType::SingleDuctConstVolNoReheat: // AirTerminal:SingleDuct:ConstantVolume:NoReheat
        thisATU.SimConstVolNoReheat(state);
        break;
    case SysType::SingleDuctVAVReheat:   // SINGLE DUCT:VAV:REHEAT
    case SysType::SingleDuctVAVNoReheat: // SINGLE DUCT:VAV:NOREHEAT
        thisATU.SimVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);
        break;
    case SysType::SingleDuctVAVReheatVSFan: // SINGLE DUCT:VAV:REHEAT:VS FAN
        thisATU.SimVAVVS(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);
        break;
    case SysType::SingleDuctCBVAVReheat:   // SINGLE DUCT:VAVHEATANDCOOL:REHEAT
    case SysType::SingleDuctCBVAVNoReheat: // SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT
        thisATU.SimCBVAV(state, FirstHVACIteration, ZoneNum, ZoneNodeNum);
        break;
    default:
        // assert(false);
        break;
    }

    // Report the current Sys
    thisATU.ReportSys(state);

    state.dataSize->TermUnitSingDuct = false;
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
    using namespace DataHeatBalance;
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using DataPlant::TypeOf_CoilWaterSimpleHeating;
    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetSysInput: "); // include trailing blank

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    int NumZoneSiz;
    int ZoneSizIndex;
    int IOStat;
    bool ErrorsFound(false);         // If errors detected in input
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

    //  certain object in the input file
    std::string AirTermSysInletNodeName;  // air terminal single duct system inlet node name
    std::string AirTermSysOutletNodeName; // air terminal single duct system outlet node name

    state.dataSingleDuct->NumVAVSysGSI = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:VAV:Reheat");
    state.dataSingleDuct->NumNoRHVAVSysGSI =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:VAV:NoReheat");
    state.dataSingleDuct->NumConstVolSys =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:ConstantVolume:Reheat");
    state.dataSingleDuct->NumCVNoReheatSysGSI =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:ConstantVolume:NoReheat");
    state.dataSingleDuct->NumVAVVSGSI =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan");
    state.dataSingleDuct->NumCBVAVSysGSI =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat");
    state.dataSingleDuct->NumNoRHCBVAVSysGSI =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat");
    state.dataSingleDuct->NumSDAirTerminal = state.dataSingleDuct->NumVAVSysGSI + state.dataSingleDuct->NumConstVolSys +
                                             state.dataSingleDuct->NumCVNoReheatSysGSI + state.dataSingleDuct->NumNoRHVAVSysGSI +
                                             state.dataSingleDuct->NumVAVVSGSI + state.dataSingleDuct->NumCBVAVSysGSI +
                                             state.dataSingleDuct->NumNoRHCBVAVSysGSI;

    state.dataSingleDuct->sd_airterminal.allocate(state.dataSingleDuct->NumSDAirTerminal);
    state.dataSingleDuct->SysUniqueNames.reserve(static_cast<unsigned>(state.dataSingleDuct->NumSDAirTerminal));
    state.dataSingleDuct->CheckEquipName.dimension(state.dataSingleDuct->NumSDAirTerminal, true);

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:VAV:Reheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:VAV:NoReheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:ConstantVolume:Reheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state,
                                                                   "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat",
                                                                   state.dataSingleDuct->TotalArgsGSI,
                                                                   state.dataSingleDuct->NumAlphasGSI,
                                                                   state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxNumsGSI = max(state.dataSingleDuct->MaxNumsGSI, state.dataSingleDuct->NumNumsGSI);
    state.dataSingleDuct->MaxAlphasGSI = max(state.dataSingleDuct->MaxAlphasGSI, state.dataSingleDuct->NumAlphasGSI);

    Alphas.allocate(state.dataSingleDuct->MaxAlphasGSI);
    cAlphaFields.allocate(state.dataSingleDuct->MaxAlphasGSI);
    cNumericFields.allocate(state.dataSingleDuct->MaxNumsGSI);
    Numbers.dimension(state.dataSingleDuct->MaxNumsGSI, 0.0);
    lAlphaBlanks.dimension(state.dataSingleDuct->MaxAlphasGSI, true);
    lNumericBlanks.dimension(state.dataSingleDuct->MaxNumsGSI, true);

    // Start Loading the System Input
    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumVAVSysGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctVAVReheat;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = Alphas(7);
        if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp, "Coil:Heating:Fuel")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Electric")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Electric;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Water")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SimpleHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Steam")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SteamAirHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
        } else if (!state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp.empty()) {
            ShowSevereError(
                state, "Illegal " + cAlphaFields(8) + " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp + '.');
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = Alphas(8);
        ValidateComponent(state,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                          IsNotOK,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        // For node connections, this object is both a parent and a non-parent, because the
        // VAV damper is not called out as a separate component, its nodes must be connected
        // as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(4));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);

        if (UtilityRoutines::SameString(Alphas(5), "Constant")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Constant;
        } else if (UtilityRoutines::SameString(Alphas(5), "FixedFlowRate")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Fixed;
        } else if (UtilityRoutines::SameString(Alphas(5), "Scheduled")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Scheduled;
        } else {
            ShowSevereError(state, cAlphaFields(5) + " = " + Alphas(5) + " not found.");
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(2);
        if (lNumericBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ConstantMinAirFracSetByUser = false;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignMinAirFrac = 0.0;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ConstantMinAirFracSetByUser = true;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignMinAirFrac = Numbers(2);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Fixed) {
                ShowWarningError(state, "Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(2) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
            }
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFixedMinAir = Numbers(3);
        if (lNumericBlanks(3)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FixedMinAirSetByUser = false;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignMinAirFrac = 0.0;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FixedMinAirSetByUser = true;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignMinAirFrac = Numbers(3);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Constant) {
                ShowWarningError(state, "Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(3) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFixedMinAir = 0.0;
            }
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr = GetScheduleIndex(state, Alphas(6));
        if ((state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr == 0) &&
            (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Scheduled)) {
            ShowSevereError(state, cAlphaFields(6) + " = " + Alphas(6) + " not found.");
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, "A valid schedule is required");
            ErrorsFound = true;
        } else if ((state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr > 0) &&
                   (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Scheduled)) {
            // check range of values in schedule
            if (!CheckScheduleValueMinMax(
                    state, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0)) {
                ShowSevereError(state, "Error found in " + cAlphaFields(6) + " = " + Alphas(6));
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
            }
        }

        // The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
        // electric or gas reheat.
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num != HeatingCoilType::Gas &&
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num != HeatingCoilType::Electric) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilSteamInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
            } else {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilWaterInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            GetOnlySingleNode(state,
                              Alphas(9),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsParent,
                              cAlphaFields(9));
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = Numbers(4);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = Numbers(5);
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = Numbers(4);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = Numbers(5);
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = Numbers(6);
        // Set default convergence tolerance
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset <= 0.0) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.001;
        }
        if (UtilityRoutines::SameString(Alphas(10), "Reverse")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::ReverseAction;
        } else if (UtilityRoutines::SameString(Alphas(10), "Normal")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::Normal;
        } else if (UtilityRoutines::SameString(Alphas(10), "ReverseWithLimits")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::ReverseActionWithLimits;
        } else {
            ShowSevereError(state, cAlphaFields(10) + " = " + Alphas(10) + " not found.");
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(state,
                                              state.dataLoopNodes->NodeID(
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode) +
                                                  " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }

                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }
        if (Numbers(7) == DataGlobalConstants::AutoCalculate) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRateDuringReheat = Numbers(7);
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRateDuringReheat =
                Numbers(7) * state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFractionDuringReheat = Numbers(8);

        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction != Action::ReverseActionWithLimits) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRateDuringReheat > 0.0) {
                ShowWarningError(state, "Since " + cAlphaFields(10) + " = " + Alphas(10) + ", input for " + cNumericFields(7) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            }
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFractionDuringReheat > 0.0) {
                ShowWarningError(state, "Since " + cAlphaFields(10) + " = " + Alphas(10) + ", input for " + cNumericFields(8) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            }
        }

        // Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
        if (!lNumericBlanks(9)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTemp = Numbers(9);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = true;
        } else {
            // user does not specify maximum supply air temperature
            // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = false;
        }

        if (!lAlphaBlanks(11)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr =
                UtilityRoutines::FindItemInList(Alphas(11), state.dataSize->OARequirements);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr == 0) {
                ShowSevereError(state, cAlphaFields(11) + " = " + Alphas(11) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            } else {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).NoOAFlowInputFromUser = false;
            }
        }

        if (lAlphaBlanks(12)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFrac = 1.0;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = false;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, Alphas(12));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr == 0) {
                ShowSevereError(state, cAlphaFields(12) + " = " + Alphas(12) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = true;
        }

        ValidateComponent(state, Alphas(7), Alphas(8), IsNotOK, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                      Alphas(7),
                      Alphas(8),
                      Alphas(3),
                      Alphas(9));

        // Setup the Average damper Position output variable
        SetupOutputVariable(state,
                            "Zone Air Terminal VAV Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
        SetupOutputVariable(state,
                            "Zone Air Terminal Minimum Air Flow Fraction",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracReport,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);

    } // end Number of Sys Loop

    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumCBVAVSysGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctCBVAVReheat;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = Alphas(5);
        if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp, "Coil:Heating:Fuel")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Electric")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Electric;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Water")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SimpleHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Steam")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SteamAirHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
        } else if (!state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp.empty()) {
            ShowSevereError(
                state, "Illegal " + cAlphaFields(5) + " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp + '.');
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = Alphas(6);
        ValidateComponent(state,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                          IsNotOK,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        // For node connections, this object is both a parent and a non-parent, because the
        // VAV damper is not called out as a separate component, its nodes must be connected
        // as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(4));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(2);
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes < 0.0) {
            ShowWarningError(state,
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " \"" +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "\"");
            ShowContinueError(state, cNumericFields(2) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues.");
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
        }
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes > 1.0) {
            ShowWarningError(state,
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " \"" +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "\"");
            ShowContinueError(state, cNumericFields(2) + " must be less than or equal to 1. Resetting to 1 and the simulation continues.");
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 1.0;
        }
        // The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
        // electric or gas reheat.
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Gas ||
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Electric) {
            //          IF(.NOT. lAlphaBlanks(5)) THEN
            //            CALL ShowWarningError(state, 'In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' //TRIM(sd_airterminal(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
            //            CALL ShowContinueError(state, '  It is used for hot water and steam reheat coils only.')
            //          END IF
        } else {
            //          IF(lAlphaBlanks(5)) THEN
            //            CALL ShowSevereError(state, 'In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' //TRIM(sd_airterminal(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
            //            ErrorsFound=.TRUE.
            //          ELSE
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilSteamInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
                //                GetOnlySingleNode(state, Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                //                              DataLoopNode::NodeFluidType::Steam,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            } else {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilWaterInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
                //                GetOnlySingleNode(state, Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                //                              DataLoopNode::NodeFluidType::Water,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            }
            //  END IF
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            GetOnlySingleNode(state,
                              Alphas(7),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsParent,
                              cAlphaFields(7));
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = Numbers(3);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = Numbers(4);
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = Numbers(3);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = Numbers(4);
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = Numbers(5);
        // Set default convergence tolerance
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset <= 0.0) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.001;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::ReverseAction;

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(state,
                                              state.dataLoopNodes->NodeID(
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode) +
                                                  " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }
        if (!lNumericBlanks(6)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTemp = Numbers(6);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = true;
        } else {
            // user does not specify maximum supply air temperature
            // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = false;
        }

        ValidateComponent(state, Alphas(5), Alphas(6), IsNotOK, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        if (lAlphaBlanks(8)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFrac = 1.0;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = false;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, Alphas(8));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr == 0) {
                ShowSevereError(state, cAlphaFields(8) + " = " + Alphas(8) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = true;
        }

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                      Alphas(5),
                      Alphas(6),
                      Alphas(3),
                      Alphas(7));

        // Setup the Average damper Position output variable
        SetupOutputVariable(state,
                            "Zone Air Terminal VAV Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);

    } // end Number of VAVHeatandCool Sys Loop

    CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:Reheat";

    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumConstVolSys;
         ++state.dataSingleDuct->SysIndexGSI) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI =
            state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI + state.dataSingleDuct->NumCBVAVSysGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctConstVolReheat;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = Alphas(5);
        if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp, "Coil:Heating:Fuel")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Electric")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Electric;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Water")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SimpleHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Steam")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SteamAirHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
        } else {
            ShowSevereError(
                state, "Illegal " + cAlphaFields(5) + " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp + '.');
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = Alphas(6);
        ValidateComponent(state,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                          IsNotOK,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsParent,
                              cAlphaFields(4));
        // The reheat coil control node is necessary for hot water reheat, but not necessary for
        // electric or gas reheat.
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Gas ||
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Electric) {
            //          IF(.NOT. lAlphaBlanks(5)) THEN
            //            CALL ShowWarningError(state, 'In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
            //            CALL ShowContinueError(state, '  It is used for hot water reheat coils only.')
            //          END IF
        } else {
            //          IF(lAlphaBlanks(5)) THEN
            //            CALL ShowSevereError(state, 'In '//TRIM(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
            //            ErrorsFound=.TRUE.
            //          END IF
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilSteamInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
                //                 GetOnlySingleNode(state, Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                //                               DataLoopNode::NodeFluidType::Steam,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            } else {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilWaterInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                }
                //                 GetOnlySingleNode(state, Alphas(5),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                //                               DataLoopNode::NodeFluidType::Water,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::MinFracNotUsed;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::HeatingActionNotUsed;
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = Numbers(2);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = Numbers(3);
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = Numbers(2);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = Numbers(3);
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = Numbers(4);
        // Set default convergence tolerance
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset <= 0.0) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.001;
        }

        // Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
        if (!lNumericBlanks(5)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTemp = Numbers(5);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = true;
        } else {
            // user does not specify maximum supply air temperature
            // sd_airterminal(SysNum)%MaxReheatTemp = 35.0D0 !C
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatTempSetByUser = false;
        }
        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(
                                state,
                                state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum) +
                                    " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }

        ValidateComponent(state, Alphas(5), Alphas(6), IsNotOK, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                      Alphas(5),
                      Alphas(6),
                      Alphas(4),
                      Alphas(3));

        // Setup the Average damper Position output variable
        // BG removed 9-10-2009 during work on CR 7770, constant volume has no damper
        //  CALL SetupOutputVariable(state, 'Damper Position', Sys(SysNum)%DamperPosition, &
        //                        'System','Average',Sys(SysNum)%SysName)

    } // End Number of Sys Loop

    CurrentModuleObject = "AirTerminal:SingleDuct:ConstantVolume:NoReheat";

    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumCVNoReheatSysGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI +
                                          state.dataSingleDuct->NumCBVAVSysGSI + state.dataSingleDuct->NumConstVolSys;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctConstVolNoReheat;

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(4));

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::MinFracNotUsed;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::HeatingActionNotUsed;

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode = 0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.000001;

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(state,
                              "...should have outlet node = " +
                                  state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(
                                state,
                                state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum) +
                                    " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }

        if (lAlphaBlanks(5)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).NoOAFlowInputFromUser = true;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr =
                UtilityRoutines::FindItemInList(Alphas(5), state.dataSize->OARequirements);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr == 0) {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError(state, "..invalid " + cAlphaFields(5) + "=\"" + Alphas(5) + "\".");
                ErrorsFound = true;
            } else {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).NoOAFlowInputFromUser = false;
            }
        }

        if (lAlphaBlanks(6)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
        } else {
            if (Alphas(6) == "CURRENTOCCUPANCY") {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
            } else if (Alphas(6) == "DESIGNOCCUPANCY") {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OAPerPersonMode = DataZoneEquipment::PerPersonByDesignLevel;
            } else {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                ShowWarningError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid data.");
                ShowContinueError(state,
                                  "..invalid " + cAlphaFields(6) + "=\"" + Alphas(6) + "\". The default input of CurrentOccupancy is assigned");
            }
        }

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            // model results related actuators
            SetupEMSActuator(state,
                             "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                             "Mass Flow Rate",
                             "[kg/s]",
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).EMSOverrideAirFlow,
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).EMSMassFlowRateValue);
            // model input related internal variables
            SetupEMSInternalVariable(state,
                                     "AirTerminal:SingleDuct:ConstantVolume:NoReheat Maximum Mass Flow Rate",
                                     state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                                     "[kg/s]",
                                     state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).AirMassFlowRateMax);
        }

    } // End Number of Sys Loop

    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumNoRHVAVSysGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        CurrentModuleObject = "AirTerminal:SingleDuct:VAV:NoReheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI +
                                          state.dataSingleDuct->NumCBVAVSysGSI + state.dataSingleDuct->NumConstVolSys +
                                          state.dataSingleDuct->NumCVNoReheatSysGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctVAVNoReheat;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = "";
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = "";
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(4));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);

        if (UtilityRoutines::SameString(Alphas(5), "Constant")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Constant;
        } else if (UtilityRoutines::SameString(Alphas(5), "FixedFlowRate")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Fixed;
        } else if (UtilityRoutines::SameString(Alphas(5), "Scheduled")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod = MinFlowFraction::Scheduled;
        } else {
            ShowSevereError(state, cAlphaFields(5) + " = " + Alphas(5) + " not found.");
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(2);
        if (lNumericBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ConstantMinAirFracSetByUser = false;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ConstantMinAirFracSetByUser = true;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(2);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Fixed) {
                ShowWarningError(state, "Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(2) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
            }
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFixedMinAir = Numbers(3);
        if (lNumericBlanks(3)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FixedMinAirSetByUser = false;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignFixedMinAir = 0.0;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FixedMinAirSetByUser = true;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DesignFixedMinAir = Numbers(3);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Constant) {
                ShowWarningError(state, "Since " + cAlphaFields(5) + " = " + Alphas(5) + ", input for " + cNumericFields(3) + " will be ignored.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFixedMinAir = 0.0;
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr = GetScheduleIndex(state, Alphas(6));
        if ((state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr == 0) &&
            (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Scheduled)) {
            ShowSevereError(state, cAlphaFields(6) + " = " + Alphas(6) + " not found.");
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, "A valid schedule is required");
            ErrorsFound = true;
        } else if ((state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr > 0) &&
                   (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracMethod == MinFlowFraction::Scheduled)) {
            // check range of values in schedule
            if (!CheckScheduleValueMinMax(
                    state, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracSchPtr, ">=", 0.0, "<=", 1.0)) {
                ShowSevereError(state, "Error found in " + cAlphaFields(6) + " = " + Alphas(6));
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ShowContinueError(state, "Schedule values must be (>=0., <=1.)");
            }
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode = 0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.000001;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::HeatingActionNotUsed;

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(state,
                                              state.dataLoopNodes->NodeID(
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode) +
                                                  " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }

                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }
        if (!lAlphaBlanks(7)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr =
                UtilityRoutines::FindItemInList(Alphas(7), state.dataSize->OARequirements);
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OARequirementsPtr == 0) {
                ShowSevereError(state, cAlphaFields(7) + " = " + Alphas(7) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            } else {
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).NoOAFlowInputFromUser = false;
            }
        }

        if (lAlphaBlanks(8)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFrac = 1.0;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = false;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, Alphas(8));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr == 0) {
                ShowSevereError(state, cAlphaFields(8) + " = " + Alphas(8) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = true;
        }

        // Setup the Average damper Position output variable
        SetupOutputVariable(state,
                            "Zone Air Terminal VAV Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
        SetupOutputVariable(state,
                            "Zone Air Terminal Minimum Air Flow Fraction",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracReport,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);

    } // end Number of Sys Loop

    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumNoRHCBVAVSysGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        CurrentModuleObject = "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI +
                                          state.dataSingleDuct->NumCBVAVSysGSI + state.dataSingleDuct->NumConstVolSys +
                                          state.dataSingleDuct->NumCVNoReheatSysGSI + state.dataSingleDuct->NumNoRHVAVSysGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctCBVAVNoReheat;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = "";
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = "";
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(3),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Outlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(3));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
            GetOnlySingleNode(state,
                              Alphas(4),
                              ErrorsFound,
                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                              Alphas(1),
                              DataLoopNode::NodeFluidType::Air,
                              DataLoopNode::NodeConnectionType::Inlet,
                              NodeInputManager::compFluidStream::Primary,
                              ObjectIsNotParent,
                              cAlphaFields(4));
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(2);
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes < 0.0) {
            ShowWarningError(state,
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = \"" +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, cNumericFields(2) + " must be greater than or equal to 0. Resetting to 0 and the simulation continues.");
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 0.0;
        }
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes > 1.0) {
            ShowWarningError(state,
                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = \"" +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, cNumericFields(2) + " must be less than or equal to 1. Resetting to 1 and the simulation continues.");
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = 1.0;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode = 0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = 0.0;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.000001;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::HeatingActionNotUsed;

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(state,
                                              state.dataLoopNodes->NodeID(
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode) +
                                                  " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }

        if (lAlphaBlanks(5)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFrac = 1.0;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = false;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, Alphas(5));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr == 0) {
                ShowSevereError(state, cAlphaFields(5) + " = " + Alphas(5) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = true;
        }

        // Setup the Average damper Position output variable
        SetupOutputVariable(state,
                            "Zone Air Terminal VAV Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);

    } // end Number of VAVHeatandCool:NoReheat Sys Loop

    // read in the SINGLE DUCT:VAV:REHEAT:VS FAN data
    for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumVAVVSGSI;
         ++state.dataSingleDuct->SysIndexGSI) {

        CurrentModuleObject = "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 state.dataSingleDuct->SysIndexGSI,
                                                                 Alphas,
                                                                 state.dataSingleDuct->NumAlphasGSI,
                                                                 Numbers,
                                                                 state.dataSingleDuct->NumNumsGSI,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields);

        state.dataSingleDuct->SysNumGSI = state.dataSingleDuct->SysIndexGSI + state.dataSingleDuct->NumVAVSysGSI +
                                          state.dataSingleDuct->NumCBVAVSysGSI + state.dataSingleDuct->NumConstVolSys +
                                          state.dataSingleDuct->NumCVNoReheatSysGSI + state.dataSingleDuct->NumNoRHVAVSysGSI +
                                          state.dataSingleDuct->NumNoRHCBVAVSysGSI;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysNum = state.dataSingleDuct->SysNumGSI;
        GlobalNames::VerifyUniqueInterObjectName(
            state, state.dataSingleDuct->SysUniqueNames, Alphas(1), CurrentModuleObject, cAlphaFields(1), ErrorsFound);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName = Alphas(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType = CurrentModuleObject;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType_Num = SysType::SingleDuctVAVReheatVSFan;
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp = Alphas(7);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName = Alphas(8);
        IsNotOK = false;
        if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp, "Coil:Heating:Fuel")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Gas;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
                GetHeatingCoilOutletNode(state,
                                         state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                         state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                         IsNotOK);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatCoilMaxCapacity =
                GetHeatingCoilCapacity(state,
                                       state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                       state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                       IsNotOK);
            if (IsNotOK)
                ShowContinueError(state,
                                  "Occurs for terminal unit " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType +
                                      " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Electric")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::Electric;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
                GetHeatingCoilOutletNode(state,
                                         state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                         state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                         IsNotOK);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatCoilMaxCapacity =
                GetHeatingCoilCapacity(state,
                                       state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                       state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                       IsNotOK);
            if (IsNotOK)
                ShowContinueError(state,
                                  "Occurs for terminal unit " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType +
                                      " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Water")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SimpleHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                               "Coil:Heating:Steam")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num = HeatingCoilType::SteamAirHeating;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_PlantType = TypeOf_CoilSteamAirHeating;
        } else if (!state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp.empty()) {
            ShowSevereError(
                state, "Illegal " + cAlphaFields(7) + " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp + '.');
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        ValidateComponent(state,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                          IsNotOK,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType = Alphas(5);
        if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType, "Fan:VariableVolume")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Num = DataHVACGlobals::FanType_SimpleVAV;
        } else if (UtilityRoutines::SameString(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType, "Fan:SystemModel")) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
        } else if (!state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType.empty()) {
            ShowSevereError(
                state, "Illegal " + cAlphaFields(5) + " = " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType + '.');
            ShowContinueError(state,
                              "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanName = Alphas(6);
        ValidateComponent(state,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanName,
                          IsNotOK,
                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType);
        if (IsNotOK) {
            ShowContinueError(state,
                              "In " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ErrorsFound = true;
        }
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs.emplace_back(
                new HVACFan::FanSystem(state,
                                       state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI)
                                           .FanName)); // call constructor, safe here because get input is not using DataIPShortCuts.
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Index =
                HVACFan::getFanObjectVectorIndex(state, state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanName);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
                state.dataHVACFan->fanObjs[state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Index]->outletNodeNum;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
                state.dataHVACFan->fanObjs[state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Index]->inletNodeNum;
            state.dataHVACFan->fanObjs[state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Index]->fanIsSecondaryDriver = true;
        } else if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Fan_Num == DataHVACGlobals::FanType_SimpleVAV) {
            IsNotOK = false;

            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum =
                GetFanOutletNode(state,
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType,
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanName,
                                 IsNotOK);
            if (IsNotOK) {
                ShowContinueError(state,
                                  "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }

            IsNotOK = false;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum =
                GetFanInletNode(state,
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanType,
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).FanName,
                                IsNotOK);
            if (IsNotOK) {
                ShowContinueError(state,
                                  "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).Schedule = Alphas(2);
        if (lAlphaBlanks(2)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr = GetScheduleIndex(state, Alphas(2));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SchedPtr == 0) {
                ShowSevereError(state, cAlphaFields(2) + " = " + Alphas(2) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
        }

        AirTermSysInletNodeName = state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum);
        if (!UtilityRoutines::SameString(Alphas(3), AirTermSysInletNodeName)) {
            ShowWarningError(state,
                             std::string{RoutineName} + "Invalid air terminal object air inlet node name in " +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, " Specified air inlet node name is = " + Alphas(3) + ".");
            ShowContinueError(state, " Expected air inlet node name is = " + AirTermSysInletNodeName + ".");
            // ErrorsFound = true;
        }

        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxAirVolFlowRate = Numbers(1);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxHeatAirVolFlowRate = Numbers(2);
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneMinAirFracDes = Numbers(3);
        // The reheat coil control node is necessary for hot water reheat, but not necessary for
        // electric or gas reheat.
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Gas ||
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::Electric) {
            //          IF(.NOT. lAlphaBlanks(6)) THEN
            //            CALL ShowWarningError(state, 'In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' // TRIM(sd_airterminal(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(6))//' is not needed and will be ignored.')
            //            CALL ShowContinueError(state, '  It is used for hot water reheat coils only.')
            //          END IF
        } else {
            //          IF(lAlphaBlanks(6)) THEN
            //            CALL ShowSevereError(state, 'In '//TRIM(sd_airterminal(SysNum)%SysType)//' = ' // TRIM(sd_airterminal(SysNum)%SysName) &
            //                                 // ' the '//TRIM(cAlphaFields(6))//' is undefined')
            //            ErrorsFound=.TRUE.
            //          END IF
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilSteamInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                } else {
                    //  A4,     \field Unit supply air outlet node
                    //          \note same as heating coil air outlet node
                    //          \note same as zone inlet node
                    //          \type alpha
                    IsNotOK = false;
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
                        GetCoilAirOutletNode(state,
                                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                             state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                             IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError(state,
                                          "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                        ErrorsFound = true;
                    }
                }
                //               GetOnlySingleNode(state, Alphas(6),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                //                                DataLoopNode::NodeFluidType::Steam,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            } else {
                IsNotOK = false;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatControlNode =
                    GetCoilWaterInletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                if (IsNotOK) {
                    ShowContinueError(state,
                                      "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                    ErrorsFound = true;
                } else {
                    //  A4,     \field Unit supply air outlet node
                    //          \note same as heating coil air outlet node
                    //          \note same as zone inlet node
                    //          \type alpha
                    IsNotOK = false;
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode =
                        GetCoilOutletNode(state,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp,
                                          state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatName,
                                          IsNotOK);
                    if (IsNotOK) {
                        ShowContinueError(state,
                                          "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                              state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                        ErrorsFound = true;
                    }
                }
                //               GetOnlySingleNode(state, Alphas(6),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
                //                                DataLoopNode::NodeFluidType::Water,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
            }
        }
        //  A4,     \field Unit supply air outlet node
        //          \note same as heating coil air outlet node
        //          \note same as zone inlet node
        //          \type alpha
        //        sd_airterminal(SysNum)%ReheatAirOutletNode  = &
        //               GetOnlySingleNode(state, Alphas(4),ErrorsFound,sd_airterminal(SysNum)%SysType,Alphas(1), &
        //                            DataLoopNode::NodeFluidType::Air,DataLoopNode::NodeConnectionType::Outlet,1,ObjectIsParent)
        AirTermSysOutletNodeName =
            state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode);
        if (!UtilityRoutines::SameString(Alphas(4), AirTermSysOutletNodeName)) {
            ShowWarningError(state,
                             std::string{RoutineName} + "Invalid air terminal object air outlet node name in " +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                 state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
            ShowContinueError(state, " Specified air outlet node name is = " + Alphas(4) + ".");
            ShowContinueError(state, " Expected air outlet node name is = " + AirTermSysOutletNodeName + ".");
            // ErrorsFound = true;
        }

        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatSteamVolFlow = Numbers(4);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatSteamVolFlow = Numbers(5);
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MaxReheatWaterVolFlow = Numbers(4);
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).MinReheatWaterVolFlow = Numbers(5);
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = Numbers(6);
        // Set default convergence tolerance
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset <= 0.0) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ControllerOffset = 0.001;
        }
        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperHeatingAction = Action::HeatingActionNotUsed;

        // Register component set data
        TestCompSet(state,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode),
                    "Air Nodes");

        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum =
                    state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + ',' +
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName + "].");
            ShowContinueError(
                state,
                "...should have outlet node = " +
                    state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the inlet node number of this unit.
            // what if not found?  error?
            IsNotOK = true;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode ==
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        IsNotOK = false;
                        if (state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode > 0) {
                            ShowSevereError(state, "Error in connecting a terminal unit to a zone");
                            ShowContinueError(state,
                                              state.dataLoopNodes->NodeID(
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode) +
                                                  " already connects to another zone");
                            ShowContinueError(state,
                                              "Occurs for terminal unit " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                            ShowContinueError(state, "Check terminal unit node names for errors");
                            ErrorsFound = true;
                        } else {
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum;
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                                state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .TermUnitSizingNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ADUNum)
                                .ZoneEqNum = CtrlZone;
                        }
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneNum = CtrlZone;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).CtrlZoneInNodeIndex = SupAirIn;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneFloorArea =
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).FloorArea *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum).Multiplier *
                            state.dataHeatBal->Zone(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ActualZoneNum)
                                .ListMultiplier;
                    }
                }
            }
        }
        if (IsNotOK) {
            ShowWarningError(state, "Did not Match Supply Air Outlet Node to any Zone Node");
            ShowContinueError(state,
                              "..Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
        }

        if (lAlphaBlanks(9)) {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFrac = 1.0;
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = false;
        } else {
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr = GetScheduleIndex(state, Alphas(9));
            if (state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchPtr == 0) {
                ShowSevereError(state, cAlphaFields(9) + " = " + Alphas(9) + " not found.");
                ShowContinueError(state,
                                  "Occurs in " + state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType + " = " +
                                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
                ErrorsFound = true;
            }
            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ZoneTurndownMinAirFracSchExist = true;
        }

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                      Alphas(7),
                      Alphas(8),
                      state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).ReheatAirOutletNode));
        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysType,
                      state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName,
                      Alphas(5),
                      Alphas(6),
                      state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).InletNodeNum),
                      state.dataLoopNodes->NodeID(state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).OutletNodeNum));

        // Setup the Average damper Position output variable
        SetupOutputVariable(state,
                            "Zone Air Terminal VAV Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).DamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysNumGSI).SysName);
    }

    // common report variable for all single duct air terminals
    for (int sdIndex = 1; sdIndex <= state.dataSingleDuct->NumSDAirTerminal; ++sdIndex) {
        SetupOutputVariable(state,
                            "Zone Air Terminal Outdoor Air Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataSingleDuct->sd_airterminal(sdIndex).OutdoorAirFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            state.dataSingleDuct->sd_airterminal(sdIndex).SysName);
    }

    // Error check to see if a single duct air terminal is assigned to zone that has zone secondary recirculation
    // specified in the Sizing:Zone object

    NumZoneSiz = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "Sizing:Zone");
    if (NumZoneSiz > 0) {
        for (state.dataSingleDuct->SysIndexGSI = 1; state.dataSingleDuct->SysIndexGSI <= state.dataSingleDuct->NumSDAirTerminal;
             ++state.dataSingleDuct->SysIndexGSI) {
            for (ZoneSizIndex = 1; ZoneSizIndex <= NumZoneSiz; ++ZoneSizIndex) {
                if (state.dataGlobal->DoZoneSizing) {
                    if (state.dataSize->FinalZoneSizing(ZoneSizIndex).ActualZoneNum ==
                        state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysIndexGSI).ActualZoneNum) {
                        if (state.dataSize->FinalZoneSizing(ZoneSizIndex).ZoneSecondaryRecirculation > 0.0) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + "A zone secondary recirculation fraction is specified for zone served by ");
                            ShowContinueError(state,
                                              "...terminal unit \"" +
                                                  state.dataSingleDuct->sd_airterminal(state.dataSingleDuct->SysIndexGSI).SysName +
                                                  "\" , that indicates a single path system");
                            ShowContinueError(state, "...The zone secondary recirculation for that zone was set to 0.0");
                            state.dataSize->FinalZoneSizing(ZoneSizIndex).ZoneSecondaryRecirculation = 0.0;
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
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Preceding condition(s) cause termination.");
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
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using DataPlant::TypeOf_CoilWaterSimpleHeating;
    using DataZoneEquipment::CheckZoneEquipmentList;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;
    auto &GetHeatingCoilCapacity(HeatingCoils::GetCoilCapacity);

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InitSys");
    static constexpr std::string_view RoutineNameFull("InitHVACSingleDuct");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InletNode;
    int OutletNode;
    int SysIndex;
    // static Array1D_bool MyEnvrnFlag;
    // static Array1D_bool MySizeFlag;
    // static Array1D_bool GetGasElecHeatCoilCap; // Gets autosized value of coil capacity
    Real64 SteamTemp;
    Real64 SteamDensity;
    Real64 rho;
    bool errFlag;

    // static Array1D_bool PlantLoopScanFlag;

    // Do the Begin Simulation initializations
    if (state.dataSingleDuct->InitSysFlag) {

        // MyEnvrnFlag.allocate(NumSDAirTerminal);
        // MySizeFlag.allocate(NumSDAirTerminal);
        // PlantLoopScanFlag.allocate(NumSDAirTerminal);
        // GetGasElecHeatCoilCap.allocate(NumSDAirTerminal);
        // MyEnvrnFlag = true;
        // MySizeFlag = true;
        // PlantLoopScanFlag = true;
        // GetGasElecHeatCoilCap = true;
        state.dataSingleDuct->InitSysFlag = false;
    }

    if (this->PlantLoopScanFlag && allocated(state.dataPlnt->PlantLoop)) {
        if ((this->ReheatComp_PlantType == TypeOf_CoilWaterSimpleHeating) || (this->ReheatComp_PlantType == TypeOf_CoilSteamAirHeating)) {
            // setup plant topology indices for plant fed heating coils
            errFlag = false;
            ScanPlantLoopsForObject(state,
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
                ShowContinueError(state, "Reference Unit=\"" + this->SysName + "\", type=" + this->SysType);
                ShowFatalError(state, "InitSys: Program terminated for previous conditions.");
            }

            this->ReheatCoilOutletNode =
                state.dataPlnt->PlantLoop(this->HWLoopNum).LoopSide(this->HWLoopSide).Branch(this->HWBranchIndex).Comp(this->HWCompIndex).NodeNumOut;

            this->PlantLoopScanFlag = false;
        } else {
            this->PlantLoopScanFlag = false;
        }
    } else if (this->PlantLoopScanFlag && !state.dataGlobal->AnyPlantInModel) {
        this->PlantLoopScanFlag = false;
    }

    if (!state.dataSingleDuct->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataSingleDuct->ZoneEquipmentListChecked = true;
        // Check to see if there is a Air Distribution Unit on the Zone Equipment List
        for (SysIndex = 1; SysIndex <= state.dataSingleDuct->NumSDAirTerminal; ++SysIndex) {
            if (state.dataSingleDuct->sd_airterminal(SysIndex).ADUNum == 0) continue;
            if (CheckZoneEquipmentList(state,
                                       "ZoneHVAC:AirDistributionUnit",
                                       state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(SysIndex).ADUNum).Name))
                continue;
            ShowSevereError(state,
                            "InitSingleDuctSystems: ADU=[Air Distribution Unit," +
                                state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->sd_airterminal(SysIndex).ADUNum).Name +
                                "] is not on any ZoneHVAC:EquipmentList.");
            ShowContinueError(state,
                              "...System=[" + state.dataSingleDuct->sd_airterminal(SysIndex).SysType + ',' +
                                  state.dataSingleDuct->sd_airterminal(SysIndex).SysName + "] will not be simulated.");
        }
    }

    // get current time step air terminal box turndown minimum flow fraction
    if (this->ZoneTurndownMinAirFracSchExist) {
        this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(state, this->ZoneTurndownMinAirFracSchPtr);
    } else {
        this->ZoneTurndownMinAirFrac = 1.0;
    }

    if (!state.dataGlobal->SysSizingCalc && this->MySizeFlag) {

        this->SizeSys(state);

        this->MySizeFlag = false;
    }

    if (this->GetGasElecHeatCoilCap) {
        if (this->ReheatComp_Num == HeatingCoilType::Electric || this->ReheatComp_Num == HeatingCoilType::Gas) {
            if (this->ReheatCoilMaxCapacity == AutoSize) {
                errFlag = false;
                this->ReheatCoilMaxCapacity = GetHeatingCoilCapacity(state, this->ReheatComp, this->ReheatName, errFlag);
                if (errFlag) ShowContinueError(state, "Occurs for terminal unit " + this->SysType + " = " + this->SysName);
            }
            if (this->ReheatCoilMaxCapacity != AutoSize) {
                this->GetGasElecHeatCoilCap = false;
            }
        } else {
            this->GetGasElecHeatCoilCap = false;
        }
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {

        // Set the outlet node max mass flow rate to the Max Air Flow specified for the Sys
        OutletNode = this->OutletNodeNum;
        InletNode = this->InletNodeNum;
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = this->MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;
        this->AirMassFlowRateMax = this->MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;
        this->HeatAirMassFlowRateMax = this->MaxHeatAirVolFlowRate * state.dataEnvrn->StdRhoAir;
        state.dataLoopNodes->Node(InletNode).MassFlowRateMax = this->MaxAirVolFlowRate * state.dataEnvrn->StdRhoAir;
        this->MassFlowDiff = 1.0e-10 * this->AirMassFlowRateMax;

        if (this->HWLoopNum > 0 && this->ReheatComp_Num != HeatingCoilType::SteamAirHeating) { // protect early calls before plant is setup
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                   DataGlobalConstants::HWInitConvTemp,
                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                   RoutineName);
        } else {
            rho = 1000.0;
        }

        this->MaxReheatWaterFlow = rho * this->MaxReheatWaterVolFlow;
        this->MinReheatWaterFlow = rho * this->MinReheatWaterVolFlow;

        this->AirMassFlowDuringReheatMax = this->MaxAirVolFlowRateDuringReheat * state.dataEnvrn->StdRhoAir;

        // set the upstream leakage flowrate - remove from here - done in ZoneAirLoopEquipmentManager::SimZoneAirLoopEquipment

        if (this->ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
            SteamTemp = 100.0;
            SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, SteamTemp, 1.0, this->FluidIndex, RoutineNameFull);
            this->MaxReheatSteamFlow = SteamDensity * this->MaxReheatSteamVolFlow;
            this->MinReheatSteamFlow = SteamDensity * this->MinReheatSteamVolFlow;
        }

        // get current environment air terminal box turndown minimum flow fraction
        Real64 CurrentEnvZoneTurndownMinAirFrac = 1.0;
        if (this->ZoneTurndownMinAirFracSchExist) {
            CurrentEnvZoneTurndownMinAirFrac = ScheduleManager::GetScheduleMinValue(state, this->ZoneTurndownMinAirFracSchPtr);
        }
        if ((this->SysType_Num == SysType::SingleDuctVAVReheat || this->SysType_Num == SysType::SingleDuctCBVAVReheat) ||
            (this->SysType_Num == SysType::SingleDuctCBVAVNoReheat)) {
            // need the lowest schedule value
            if (this->ZoneMinAirFracMethod == MinFlowFraction::Scheduled) {
                this->ZoneMinAirFracDes = GetScheduleMinValue(state, this->ZoneMinAirFracSchPtr);
            }
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMin =
                state.dataLoopNodes->Node(OutletNode).MassFlowRateMax * this->ZoneMinAirFracDes * CurrentEnvZoneTurndownMinAirFrac;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMin =
                state.dataLoopNodes->Node(InletNode).MassFlowRateMax * this->ZoneMinAirFracDes * CurrentEnvZoneTurndownMinAirFrac;
        } else {
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRateMin = 0.0;
        }
        if ((this->ReheatControlNode > 0) && !this->PlantLoopScanFlag) {
            if (this->ReheatComp_Num == HeatingCoilType::SteamAirHeating) {
                InitComponentNodes(state,
                                   this->MinReheatSteamFlow,
                                   this->MaxReheatSteamFlow,
                                   this->ReheatControlNode,
                                   this->ReheatCoilOutletNode,
                                   this->HWLoopNum,
                                   this->HWLoopSide,
                                   this->HWBranchIndex,
                                   this->HWCompIndex);
            } else {
                InitComponentNodes(state,
                                   this->MinReheatWaterFlow,
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
            this->AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(this->CtrlZoneNum).InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
            state.dataDefineEquipment->AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
        }

        this->MyEnvrnFlag = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        this->MyEnvrnFlag = true;
    }

    // Initialize the Inlet Nodes of the air side of air terminal
    InletNode = this->InletNodeNum;
    OutletNode = this->OutletNodeNum;

    Real64 mDotFromOARequirement(0.0);

    if (this->SysType_Num == SysType::SingleDuctConstVolNoReheat) {
        if (!this->NoOAFlowInputFromUser) {
            mDotFromOARequirement = this->AirMassFlowRateMax;
            int airLoopNum(0);
            Real64 airLoopOAFrac(0.0);
            airLoopNum = this->AirLoopNum;
            if (airLoopNum > 0) {
                airLoopOAFrac = state.dataAirLoop->AirLoopFlow(airLoopNum).OAFrac;
                bool UseOccSchFlag = false;
                if (this->OAPerPersonMode == DataZoneEquipment::PerPersonDCVByCurrentLevel) UseOccSchFlag = true;
                if (airLoopOAFrac > 0.0) {
                    Real64 vDotOAReq =
                        DataSizing::calcDesignSpecificationOutdoorAir(state, this->OARequirementsPtr, this->CtrlZoneNum, UseOccSchFlag, true);
                    mDotFromOARequirement = vDotOAReq * state.dataEnvrn->StdRhoAir / airLoopOAFrac;
                    mDotFromOARequirement = min(mDotFromOARequirement, this->AirMassFlowRateMax);
                } else {
                    mDotFromOARequirement = this->AirMassFlowRateMax;
                }
            }
        }
    }

    if (this->ZoneMinAirFracMethod == MinFlowFraction::Scheduled) {
        this->ZoneMinAirFracDes = GetCurrentScheduleValue(state, this->ZoneMinAirFracSchPtr);
        // now reset inlet node min avail
        state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = this->AirMassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
    }

    if (FirstHVACIteration) {
        // The first time through set the mass flow rate to the Max
        if ((state.dataLoopNodes->Node(InletNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
            if (!(state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                  state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
                state.dataLoopNodes->Node(InletNode).MassFlowRate = this->AirMassFlowRateMax;
            }
        } else {
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
        }
        if ((state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
            if (!(state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                  state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
                if (this->SysType_Num == SysType::SingleDuctConstVolNoReheat) {
                    if (this->NoOAFlowInputFromUser) {
                        state.dataLoopNodes->Node(InletNode).MassFlowRate = this->AirMassFlowRateMax;
                        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = this->AirMassFlowRateMax;
                    } else {
                        state.dataLoopNodes->Node(InletNode).MassFlowRate = mDotFromOARequirement;
                        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = mDotFromOARequirement;
                    }
                    if (this->EMSOverrideAirFlow) {
                        state.dataLoopNodes->Node(InletNode).MassFlowRate = this->EMSMassFlowRateValue;
                        state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = this->EMSMassFlowRateValue;
                    }
                } else {
                    state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = this->AirMassFlowRateMax;
                }
            }
        } else {
            state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
        }

        if ((state.dataLoopNodes->Node(InletNode).MassFlowRate > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
            if (!(state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
                  state.dataAirflowNetwork->AirflowNetworkFanActivated)) {
                state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail =
                    this->AirMassFlowRateMax * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
            }
        } else {
            state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
        }
        // reset the mass flow rate histories
        this->MassFlow1 = 0.0;
        this->MassFlow2 = 0.0;
        this->MassFlow3 = 0.0;
        this->MassFlow3 = 0.0;

    } else {
        if (this->SysType_Num == SysType::SingleDuctConstVolNoReheat) {
            if (!this->EMSOverrideAirFlow) {
                if ((state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail > 0.0) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
                    if (this->NoOAFlowInputFromUser) {
                        if (state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail < state.dataLoopNodes->Node(InletNode).MassFlowRateMax) {
                            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
                        } else if (state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail > state.dataLoopNodes->Node(InletNode).MassFlowRateMin) {
                            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;
                        } else {
                            state.dataLoopNodes->Node(InletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
                        }
                    } else {
                        state.dataLoopNodes->Node(InletNode).MassFlowRate = mDotFromOARequirement;
                        // but also apply constraints
                        state.dataLoopNodes->Node(InletNode).MassFlowRate =
                            min(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail);
                        state.dataLoopNodes->Node(InletNode).MassFlowRate =
                            min(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMax);
                        state.dataLoopNodes->Node(InletNode).MassFlowRate =
                            max(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail);
                        state.dataLoopNodes->Node(InletNode).MassFlowRate =
                            max(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMin);
                    }
                } else {
                    state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
                    state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail = 0.0;
                    state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail = 0.0;
                }
            } else { // EMS override on
                state.dataLoopNodes->Node(InletNode).MassFlowRate = this->EMSMassFlowRateValue;
                // but also apply constraints
                state.dataLoopNodes->Node(InletNode).MassFlowRate =
                    min(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail);
                state.dataLoopNodes->Node(InletNode).MassFlowRate =
                    min(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMax);
                state.dataLoopNodes->Node(InletNode).MassFlowRate =
                    max(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail);
                state.dataLoopNodes->Node(InletNode).MassFlowRate =
                    max(state.dataLoopNodes->Node(InletNode).MassFlowRate, state.dataLoopNodes->Node(InletNode).MassFlowRateMin);
            }
        }
    }

    // Do a check and make sure that the max and min available(control) flow is
    //  between the physical max and min while operating.
    this->sd_airterminalInlet.AirMassFlowRateMaxAvail = min(this->AirMassFlowRateMax, state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail);
    this->sd_airterminalInlet.AirMassFlowRateMinAvail =
        min(max(state.dataLoopNodes->Node(OutletNode).MassFlowRateMin, state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail),
            this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.
    // Load the node data in this section for the component simulation
    this->sd_airterminalInlet.AirMassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
    this->sd_airterminalInlet.AirTemp = state.dataLoopNodes->Node(InletNode).Temp;
    this->sd_airterminalInlet.AirHumRat = state.dataLoopNodes->Node(InletNode).HumRat;
    this->sd_airterminalInlet.AirEnthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;

    // update to the current minimum air flow fraction
    this->ZoneMinAirFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
}

void SingleDuctAirTerminal::SizeSys(EnergyPlusData &state)
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
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using General::SafeDivide;
    using PlantUtilities::MyPlantSizingIndex;
    using SteamCoils::GetCoilSteamInletNode;
    using SteamCoils::GetCoilSteamOutletNode;
    using WaterCoils::GetCoilWaterInletNode;
    using WaterCoils::GetCoilWaterOutletNode;
    using WaterCoils::SetCoilDesFlow;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeSys");
    static constexpr std::string_view RoutineNameFull("SizeHVACSingleDuct");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PltSizHeatNum; // index of plant sizing object for 1st heating loop
    Real64 DesMassFlow;
    Real64 TempSteamIn;
    Real64 EnthSteamOutWet;
    Real64 EnthSteamInDry;
    Real64 LatentHeatSteam;
    Real64 SteamDensity;

    bool ErrorsFound;
    bool PlantSizingErrorsFound;
    Real64 rho; // local fluid density
    Real64 Cp;  // local fluid specific heat
    bool IsAutoSize;
    bool IsMaxFlowAutoSize; // Indicate if the maximum terminal flow is autosize
    int ZoneNum(0);
    int AirLoopNum;                           // Air loop number
    int SysSizNum;                            // System sizing number
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
    IsMaxFlowAutoSize = false;
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
    AirLoopNum = 0;
    SysSizNum = 0;

    ZoneNum = this->ActualZoneNum;

    auto &TermUnitSizing(state.dataSize->TermUnitSizing);

    if (this->MaxAirVolFlowRate == AutoSize) {
        IsAutoSize = true;
    }

    if (state.dataSize->CurTermUnitSizingNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // simulation continue
            if (this->MaxAirVolFlowRate > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, this->SysType, this->SysName, "User-Specified Maximum Air Flow Rate [m3/s]", this->MaxAirVolFlowRate);
            }
        } else { // Autosize or hard-size with sizing run

            CheckZoneSizing(state, this->SysType, this->SysName);

            MaxAirVolFlowRateDes = max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesCoolVolFlow,
                                       state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatVolFlow);

            if (MaxAirVolFlowRateDes < SmallAirVolFlow) {
                MaxAirVolFlowRateDes = 0.0;
            }
            if (IsAutoSize) {
                this->MaxAirVolFlowRate = MaxAirVolFlowRateDes;
                IsMaxFlowAutoSize = true;
                BaseSizer::reportSizerOutput(state, this->SysType, this->SysName, "Design Size Maximum Air Flow Rate [m3/s]", MaxAirVolFlowRateDes);
            } else { // Hard-size with sizing data
                if (this->MaxAirVolFlowRate > 0.0 && MaxAirVolFlowRateDes > 0.0) {
                    MaxAirVolFlowRateUser = this->MaxAirVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 this->SysType,
                                                 this->SysName,
                                                 "Design Size Maximum Air Flow Rate [m3/s]",
                                                 MaxAirVolFlowRateDes,
                                                 "User-Specified Maximum Air Flow Rate [m3/s]",
                                                 MaxAirVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxAirVolFlowRateDes - MaxAirVolFlowRateUser) / MaxAirVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName +
                                            "\".");
                            ShowContinueError(state, format("User-Specified Maximum Air Flow Rate of {:.5R} [m3/s]", MaxAirVolFlowRateUser));
                            ShowContinueError(state, format("differs from Design Size Maximum Air Flow Rate of {:.5R} [m3/s]", MaxAirVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
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
    if (state.dataSize->CurTermUnitSizingNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // simulation should continue
            state.dataSingleDuct->UserInputMaxHeatAirVolFlowRateSS = this->MaxHeatAirVolFlowRate;
            if (this->MaxHeatAirVolFlowRate > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, this->SysType, this->SysName, "User-Specified Maximum Heating Air Flow Rate [m3/s]", this->MaxHeatAirVolFlowRate);
            }
        } else {
            CheckZoneSizing(state, this->SysType, this->SysName);
            MaxHeatAirVolFlowRateDes = state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatVolFlow;
            if (MaxHeatAirVolFlowRateDes < SmallAirVolFlow) {
                MaxHeatAirVolFlowRateDes = 0.0;
            }
            if (IsAutoSize) {
                this->MaxHeatAirVolFlowRate = MaxHeatAirVolFlowRateDes;
                state.dataSingleDuct->UserInputMaxHeatAirVolFlowRateSS = 0.0;
                BaseSizer::reportSizerOutput(
                    state, this->SysType, this->SysName, "Design Size Maximum Heating Air Flow Rate [m3/s]", MaxHeatAirVolFlowRateDes);
            } else { // Hard-size with sizing data
                if (this->MaxHeatAirVolFlowRate > 0.0 && MaxHeatAirVolFlowRateDes > 0.0) {
                    MaxHeatAirVolFlowRateUser = this->MaxHeatAirVolFlowRate;
                    state.dataSingleDuct->UserInputMaxHeatAirVolFlowRateSS = this->MaxHeatAirVolFlowRate;
                    BaseSizer::reportSizerOutput(state,
                                                 this->SysType,
                                                 this->SysName,
                                                 "Design Size Maximum Heating Air Flow Rate [m3/s]",
                                                 MaxHeatAirVolFlowRateDes,
                                                 "User-Specified Maximum Heating Air Flow Rate [m3/s]",
                                                 MaxHeatAirVolFlowRateUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxHeatAirVolFlowRateDes - MaxHeatAirVolFlowRateUser) / MaxHeatAirVolFlowRateUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName +
                                            "\".");
                            ShowContinueError(state,
                                              format("User-Specified Maximum Heating Air Flow Rate of {:.5R} [m3/s]", MaxHeatAirVolFlowRateUser));
                            ShowContinueError(
                                state, format("differs from Design Size Maximum Heating Air Flow Rate of {:.5R} [m3/s]", MaxHeatAirVolFlowRateDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    // get design day terminal unit turndown minimum flow fraction
    if (this->ZoneTurndownMinAirFracSchExist) {
        this->ZoneTurndownMinAirFrac = ScheduleManager::GetCurrentScheduleValue(state, this->ZoneTurndownMinAirFracSchPtr);
    } else {
        this->ZoneTurndownMinAirFrac = 1.0;
    }

    // if a sizing run has been done, check if system sizing has been done for this system
    bool SizingDesRunThisAirSys = false;
    if (state.dataSize->SysSizingRunDone) {
        AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(this->CtrlZoneNum).InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
        if (AirLoopNum > 0) {
            CheckThisAirSystemForSizing(state, AirLoopNum, SizingDesRunThisAirSys);
        }

        // get system sizing id if a sizing run has been done for this system
        if (SizingDesRunThisAirSys) {
            SysSizNum = UtilityRoutines::FindItemInList(
                state.dataSize->FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
            if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
        }
    }

    IsAutoSize = false;
    if (this->ZoneMinAirFracDes == AutoSize) {
        IsAutoSize = true;
    }
    if (this->ZoneMinAirFracMethod == MinFlowFraction::Constant) {
        if (state.dataSize->ZoneSizingRunDone) {
            if (state.dataSize->CurTermUnitSizingNum > 0) {
                // use the combined defaults or other user inputs stored in DesCoolVolFlowMin
                if (this->MaxAirVolFlowRate > 0.0) {
                    MinAirFlowFracDes = min(1.0,
                                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesCoolVolFlowMin /
                                                this->MaxAirVolFlowRate);
                } else {
                    MinAirFlowFracDes = 0.0;
                }
            }
        } else {
            // if no zone sizing values available; use max of min frac = 0.2 and 0.000762 [m3/s-m2]
            if (this->MaxAirVolFlowRate > 0.0) {
                MinMinFlowRatio = (0.000762 * state.dataHeatBal->Zone(ZoneNum).FloorArea * state.dataHeatBal->Zone(ZoneNum).Multiplier *
                                   state.dataHeatBal->Zone(ZoneNum).ListMultiplier) /
                                  this->MaxAirVolFlowRate;
                MinAirFlowFracDes = max(0.2, MinMinFlowRatio);
            } else {
                MinAirFlowFracDes = 0.0;
            }
        }
        if (SizingDesRunThisAirSys) {
            if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                if (this->MaxAirVolFlowRate > 0.0) {
                    MinAirFlowFracDes = 1.5 *
                                        max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozClgByZone,
                                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozHtgByZone) /
                                        this->MaxAirVolFlowRate;

                    // adjust maximum flow rate
                    if (MinAirFlowFracDes > 1.0 && IsMaxFlowAutoSize) {
                        this->MaxAirVolFlowRate *= MinAirFlowFracDes;
                        MinAirFlowFracDes = 1.0;
                        ShowWarningError(state,
                                         "SingleDuctSystem:SizeSys: Autosized maximum air flow rate for " + this->SysName +
                                             " was increased to meet the zone primary air flow determined according to the ASHRAE Standard 62.1 "
                                             "Simplified Procedure.");
                    } else if (MinAirFlowFracDes > 1.0) {
                        ShowWarningError(state, "SingleDuctSystem:SizeSys: Maximum air flow rate for " + this->SysName + " is potentially too low.");
                        ShowContinueError(
                            state,
                            "The flow is lower than the minimum flow rate calculated following the ASHRAE Standard 62.1 Simplified Procedure:");
                        ShowContinueError(state, format(" User-specified maximum air flow rate: {:.3R} m3/s.", this->MaxAirVolFlowRate));
                        ShowContinueError(state,
                                          format(" Calculated minimum air flow rate: {:.3R} m3/s.", this->MaxAirVolFlowRate * MinAirFlowFracDes));
                        MinAirFlowFracDes = 1.0;
                    }
                }
            }
        }
        if (IsAutoSize) {
            // report out autosized result and save value in Sys array
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Constant Minimum Air Flow Fraction",
                                         MinAirFlowFracDes * this->ZoneTurndownMinAirFrac);
            if (SizingDesRunThisAirSys) {
                if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) {
                    state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VpzMinByZoneSPSized = true;
                }
            }
            this->ZoneMinAirFracDes = MinAirFlowFracDes;
        } else {
            // report out hard (user set) value and issue warning if appropriate
            MinAirFlowFracUser = this->ZoneMinAirFracDes;
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Constant Minimum Air Flow Fraction",
                                         MinAirFlowFracDes * this->ZoneTurndownMinAirFrac,
                                         "User-Specified Constant Minimum Air Flow Fraction",
                                         MinAirFlowFracUser * this->ZoneTurndownMinAirFrac);
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(MinAirFlowFracDes - MinAirFlowFracUser) / MinAirFlowFracUser) > state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state, format("User-Specified Minimum Cooling Air Flow Fraction of {:.5R} [m3/s]", MinAirFlowFracUser));
                    ShowContinueError(state,
                                      format("differs from Design Size Minimum Cooling Air Flow Fraction of {:.5R} [m3/s]", MinAirFlowFracDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
        }
        // report out the min air flow rate set by min air flow frac
        BaseSizer::reportSizerOutput(state,
                                     this->SysType,
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
    if (this->ZoneMinAirFracMethod == MinFlowFraction::Fixed) {
        if (state.dataSize->ZoneSizingRunDone) {
            if (state.dataSize->CurTermUnitSizingNum > 0) {
                // use the combined defaults or other user inputs stored in DesCoolVolFlowMin
                if (this->MaxAirVolFlowRate > 0.0) {
                    FixedMinAirDes = state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesCoolVolFlowMin;
                } else {
                    MinAirFlowFracDes = 0.0;
                }
            }
        } else {
            // if no zone sizing values available; use max of min frac = 0.2 and 0.000762 [m3/s-m2]
            if (this->MaxAirVolFlowRate > 0.0) {
                FixedMinAirDes = max(0.2 * this->MaxAirVolFlowRate,
                                     0.000762 * state.dataHeatBal->Zone(ZoneNum).FloorArea * state.dataHeatBal->Zone(ZoneNum).Multiplier *
                                         state.dataHeatBal->Zone(ZoneNum).ListMultiplier);
            } else {
                MinAirFlowFracDes = 0.0;
            }
        }
        if (SizingDesRunThisAirSys) {
            if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                if (this->MaxAirVolFlowRate > 0.0) {
                    FixedMinAirDes = 1.5 * max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozClgByZone,
                                               state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozHtgByZone);

                    // adjust maximum flow rate
                    if (FixedMinAirDes > this->MaxAirVolFlowRate && IsMaxFlowAutoSize) {
                        this->MaxAirVolFlowRate = FixedMinAirDes;
                        ShowWarningError(state,
                                         "SingleDuctSystem:SizeSys: Autosized maximum air flow rate for " + this->SysName +
                                             " was increased to meet the zone primary air flow determined according to the ASHRAE Standard 62.1 "
                                             "Simplified Procedure.");
                    } else if (FixedMinAirDes > this->MaxAirVolFlowRate) {
                        ShowWarningError(state, "SingleDuctSystem:SizeSys: Maximum air flow rate for " + this->SysName + " is potentially too low.");
                        ShowContinueError(
                            state,
                            "The flow is lower than the minimum flow rate calculated following the ASHRAE Standard 62.1 Simplified Procedure:");
                        ShowContinueError(state, format(" User-specified maximum air flow rate: {:.3R} m3/s.", this->MaxAirVolFlowRate));
                        ShowContinueError(state, format(" Calculated minimum air flow rate: {:.3R} m3/s.", FixedMinAirDes));
                        FixedMinAirDes = this->MaxAirVolFlowRate;
                    }
                }
            }
        }
        if (IsAutoSize) {
            // report out autosized result and save value in Sys array
            BaseSizer::reportSizerOutput(
                state, this->SysType, this->SysName, "Design Size Fixed Minimum Air Flow Rate [m3/s]", FixedMinAirDes * this->ZoneTurndownMinAirFrac);
            if (SizingDesRunThisAirSys) {
                if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) {
                    state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VpzMinByZoneSPSized = true;
                }
            }
            this->ZoneFixedMinAir = FixedMinAirDes;
        } else {
            // report out hard (user set) value and issue warning if appropriate
            FixedMinAirUser = this->ZoneFixedMinAir;
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Fixed Minimum Air Flow Rate [m3/s]",
                                         FixedMinAirDes * this->ZoneTurndownMinAirFrac,
                                         "User-Specified Fixed Minimum Air Flow Rate [m3/s]",
                                         FixedMinAirUser * this->ZoneTurndownMinAirFrac);
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(FixedMinAirDes - FixedMinAirUser) / FixedMinAirUser) > state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state, format("User-Specified Minimum Cooling Air Flow Rate of {:.5R} [m3/s]", FixedMinAirUser));
                    ShowContinueError(state, format("differs from Design Size Minimum Cooling Air Flow Rate of {:.5R} [m3/s]", FixedMinAirDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
        }
        // report out the min air flow frac set by the min air flow rate
        if (this->MaxAirVolFlowRate > 0.0) {
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Minimum Air Flow Fraction [m3/s]",
                                         this->ZoneFixedMinAir * this->ZoneTurndownMinAirFrac / this->MaxAirVolFlowRate);
        }
    } else {
        if (IsAutoSize) {
            this->ZoneFixedMinAir = 0.0;
        }
    }

    if (this->ZoneMinAirFracMethod == MinFlowFraction::Scheduled) {
        // need a value for sizing.
        if (this->ConstantMinAirFracSetByUser) {
            this->ZoneMinAirFracDes = this->DesignMinAirFrac;
            // if both inputs are defined, use the max
            if (this->FixedMinAirSetByUser) {
                this->ZoneMinAirFracDes = min(1.0, max(this->ZoneMinAirFracDes, SafeDivide(this->DesignFixedMinAir, this->MaxAirVolFlowRate)));
            }
            // if only fixed is defined, use the value
        } else if (this->FixedMinAirSetByUser) {
            this->ZoneMinAirFracDes = min(1.0, SafeDivide(this->DesignFixedMinAir, this->MaxAirVolFlowRate));
        } else {
            // use an average of min and max in schedule
            this->ZoneMinAirFracDes =
                (GetScheduleMinValue(state, this->ZoneMinAirFracSchPtr) + GetScheduleMaxValue(state, this->ZoneMinAirFracSchPtr)) / 2.0;
        }
    }

    if (this->ZoneMinAirFracMethod == MinFlowFraction::Fixed) {
        // need a value for sizing.
        this->ZoneMinAirFracDes = min(1.0, SafeDivide(this->ZoneFixedMinAir, this->MaxAirVolFlowRate));
    }

    if (this->DamperHeatingAction == Action::ReverseActionWithLimits) {
        if (state.dataSize->ZoneSizingRunDone) {
            if (state.dataSize->CurTermUnitSizingNum > 0) {
                // if zone sizing run done, set the design max reheat air flow to the value from the design calcs
                MaxAirVolFlowRateDuringReheatDes = state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatVolFlowMax;
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
        if (this->MaxAirVolFlowRateDuringReheat == DataGlobalConstants::AutoCalculate &&
            this->MaxAirVolFractionDuringReheat == DataGlobalConstants::AutoCalculate) {
            // if both inputs are autosize (the default) report both out and save in the Sys array.
            BaseSizer::reportSizerOutput(
                state, this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes);
            if (this->ZoneFloorArea > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             this->SysType,
                                             this->SysName,
                                             "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea);
            }
            this->MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes;
            this->MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes;
        } else if (this->MaxAirVolFlowRateDuringReheat == DataGlobalConstants::AutoCalculate &&
                   this->MaxAirVolFractionDuringReheat != DataGlobalConstants::AutoCalculate) {
            // if max reheat flow fraction was input, set the max reheat flow design value correspondingly, report both out.
            // Check for optional caution message that user input value is not within 10% of the design value.
            MaxAirVolFlowRateDuringReheatDes = this->MaxAirVolFractionDuringReheat * this->MaxAirVolFlowRate;
            MaxAirVolFractionDuringReheatUser = this->MaxAirVolFractionDuringReheat;
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Maximum Flow Fraction during Reheat []",
                                         MaxAirVolFractionDuringReheatDes,
                                         "User-Specified Maximum Flow Fraction during Reheat []",
                                         MaxAirVolFractionDuringReheatUser);
            if (this->ZoneFloorArea > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             this->SysType,
                                             this->SysName,
                                             "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea);
            }
            this->MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes;
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser) / MaxAirVolFractionDuringReheatUser) >
                    state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state,
                                      format("User-Specified Maximum Flow Fraction during Reheat of {:.5R} []", MaxAirVolFractionDuringReheatUser));
                    ShowContinueError(
                        state, format("differs from Design Size Maximum Flow Fraction during Reheat of {:.5R} []", MaxAirVolFractionDuringReheatDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
        } else if (this->MaxAirVolFlowRateDuringReheat != DataGlobalConstants::AutoCalculate &&
                   this->MaxAirVolFractionDuringReheat == DataGlobalConstants::AutoCalculate) {
            // if max reheat flow was input set the design max reheat flow frac to the corresponding value, report both out, save the design value
            // of the flow frac in Sys. Check for optional caution message that user input value is not within 10% of the design value.
            if (this->MaxAirVolFlowRate > 0.0) {
                MaxAirVolFractionDuringReheatDes = MaxAirVolFlowRateDuringReheatDes / this->MaxAirVolFlowRate;
            } else {
                MaxAirVolFractionDuringReheatDes = 0.0;
            }
            MaxAirVolFlowRateDuringReheatUser = this->MaxAirVolFlowRateDuringReheat;
            BaseSizer::reportSizerOutput(
                state, this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", MaxAirVolFractionDuringReheatDes);
            if (this->ZoneFloorArea > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             this->SysType,
                                             this->SysName,
                                             "Design Size Maximum Flow per Zone Floor Area during Reheat [ m3/s-m2 ]",
                                             MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea,
                                             "User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatUser / this->ZoneFloorArea);
            }
            this->MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes;
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser) / MaxAirVolFlowRateDuringReheatUser) >
                    state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state,
                                      format("User-Specified Maximum Flow per Zone Floor Area during Reheat of {:.5R} [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatUser));
                    ShowContinueError(state,
                                      format("differs from Design Size Maximum Flow per Zone Floor Area during Reheat of {:.5R} [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
        } else {
            // both fields have user input. Report both out, use the larger of the 2 values. Note that only sd_airterminal( SysNum
            // ).MaxAirVolFlowRateDuringReheat is used subsequently. Check both inputs for optional caution message that user input value is not
            // within 10% of the design value.
            MaxAirVolFlowRateDuringReheatUser = this->MaxAirVolFlowRateDuringReheat;
            MaxAirVolFractionDuringReheatUser = this->MaxAirVolFractionDuringReheat;
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Maximum Flow Fraction during Reheat []",
                                         MaxAirVolFractionDuringReheatDes,
                                         "User-Specified Maximum Flow Fraction during Reheat []",
                                         MaxAirVolFractionDuringReheatUser);
            if (this->ZoneFloorArea > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             this->SysType,
                                             this->SysName,
                                             "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatDes / this->ZoneFloorArea,
                                             "User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatUser / this->ZoneFloorArea);
            }
            this->MaxAirVolFlowRateDuringReheat =
                max(this->MaxAirVolFlowRateDuringReheat, this->MaxAirVolFractionDuringReheat * this->MaxAirVolFlowRate);
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser) / MaxAirVolFractionDuringReheatUser) >
                    state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state,
                                      format("User-Specified Maximum Flow Fraction during Reheat of {:.5R} []", MaxAirVolFractionDuringReheatUser));
                    ShowContinueError(
                        state, format("differs from Design Size Maximum Flow Fraction during Reheat of {:.5R} []", MaxAirVolFractionDuringReheatDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
            if (state.dataGlobal->DisplayExtraWarnings) {
                if ((std::abs(MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser) / MaxAirVolFlowRateDuringReheatUser) >
                    state.dataSize->AutoVsHardSizingThreshold) {
                    ShowMessage(state,
                                "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" + this->SysName + "\".");
                    ShowContinueError(state,
                                      format("User-Specified Maximum Flow per Zone Floor Area during Reheat of {:.5R} [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatUser));
                    ShowContinueError(state,
                                      format("differs from Design Size Maximum Flow per Zone Floor Area during Reheat of {:.5R} [m3/s-m2]",
                                             MaxAirVolFlowRateDuringReheatDes));
                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                }
            }
        }
        // check that MaxAirVolFlowRateDuringReheat is greater than the min and less than the max
        this->MaxAirVolFlowRateDuringReheat = min(MaxAirVolFlowRateDuringReheatDes, this->MaxAirVolFlowRate);
        this->MaxAirVolFlowRateDuringReheat = max(MaxAirVolFlowRateDuringReheatDes, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes));
    } else if (this->DamperHeatingAction == Action::Normal) {
        // for Normal action, max reheat flow is equal to the minimum. Report it.
        if (this->ZoneFloorArea > 0.0) {
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                         (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes) / this->ZoneFloorArea);
        }
        BaseSizer::reportSizerOutput(
            state, this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", this->ZoneMinAirFracDes);
        // zero the ReverseActioWithLimits inputs
        this->MaxAirVolFlowRateDuringReheat = max(this->MaxAirVolFlowRateDuringReheat, 0.0);
        this->MaxAirVolFractionDuringReheat = max(this->MaxAirVolFractionDuringReheat, 0.0);
    } else if (this->DamperHeatingAction == Action::ReverseAction) {
        // for ReverseAction, max reheat flow is equal to the maximum. Report it.
        if (this->ZoneFloorArea > 0.0) {
            BaseSizer::reportSizerOutput(state,
                                         this->SysType,
                                         this->SysName,
                                         "Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]",
                                         this->MaxAirVolFlowRate / this->ZoneFloorArea);
        }
        BaseSizer::reportSizerOutput(state, this->SysType, this->SysName, "Design Size Maximum Flow Fraction during Reheat []", 1.0);
        // zero the ReverseActioWithLimits inputs
        this->MaxAirVolFlowRateDuringReheat = max(this->MaxAirVolFlowRateDuringReheat, 0.0);
        this->MaxAirVolFractionDuringReheat = max(this->MaxAirVolFractionDuringReheat, 0.0);
    }

    if (state.dataSize->CurTermUnitSizingNum > 0) {
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult = 1.0;
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult = 1.0;
        if (state.dataSize->ZoneSizingRunDone) {
            if (this->SysType_Num == SysType::SingleDuctVAVReheatVSFan) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow =
                    max(state.dataSingleDuct->UserInputMaxHeatAirVolFlowRateSS,
                        state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).NonAirSysDesHeatVolFlow,
                        this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
            } else {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow =
                    max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).NonAirSysDesHeatVolFlow,
                        this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
            }
        } else {
            if (this->SysType_Num == SysType::SingleDuctVAVReheatVSFan) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow =
                    max(this->MaxHeatAirVolFlowRate, this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac);
            } else if (this->SysType_Num == SysType::SingleDuctConstVolReheat || this->SysType_Num == SysType::SingleDuctConstVolNoReheat) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow = this->MaxAirVolFlowRate;
            } else {
                if (this->DamperHeatingAction == Action::ReverseAction) {
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow = this->MaxAirVolFlowRate;
                } else if (this->DamperHeatingAction == Action::ReverseActionWithLimits) {
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow =
                        max(this->MaxAirVolFlowRateDuringReheat, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac));
                } else {
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow =
                        this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
                }
            }
        }

        if (TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow > SmallAirVolFlow) {
            if (this->DamperHeatingAction == Action::ReverseActionWithLimits) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                    min(this->MaxAirVolFlowRateDuringReheat, this->MaxAirVolFlowRate) /
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult =
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult;
            } else if (this->DamperHeatingAction == Action::ReverseAction) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                    this->MaxAirVolFlowRate / TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult =
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult;
            } else if (this->DamperHeatingAction == Action::Normal && this->MaxAirVolFlowRateDuringReheat > 0.0) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                    min(this->MaxAirVolFlowRateDuringReheat, (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac)) /
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult = 1.0;
            } else if (this->DamperHeatingAction == Action::Normal && this->MaxAirVolFlowRateDuringReheat == 0.0) {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                    (this->MaxAirVolFlowRate * this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac) /
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult = 1.0;
            } else {
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                    this->MaxAirVolFlowRate / TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult =
                    TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult;
            }
            TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult =
                max(1.0, TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult);
            TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult =
                max(1.0, TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult);
        } else {
            TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatAirFlowMult = 1.0;
            TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult = 1.0;
        }
        if (this->ReheatComp_Index > 0) {
            state.dataRptCoilSelection->coilSelectionReportObj->setCoilReheatMultiplier(
                state, this->ReheatName, this->ReheatComp, TermUnitSizing(state.dataSize->CurTermUnitSizingNum).ReheatLoadMult);
        }
    }

    IsAutoSize = false;
    if (this->MaxReheatWaterVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurTermUnitSizingNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
            if (this->MaxReheatWaterVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, this->SysType, this->SysName, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", this->MaxReheatWaterVolFlow);
            }
        } else {
            CheckZoneSizing(state, this->SysType, this->SysName);
            if (UtilityRoutines::SameString(this->ReheatComp, "Coil:Heating:Water")) {
                state.dataSingleDuct->CoilWaterInletNodeSS = GetCoilWaterInletNode(state, "Coil:Heating:Water", this->ReheatName, ErrorsFound);
                state.dataSingleDuct->CoilWaterOutletNodeSS = GetCoilWaterOutletNode(state, "Coil:Heating:Water", this->ReheatName, ErrorsFound);
                if (IsAutoSize) {
                    PlantSizingErrorsFound = false;
                    PltSizHeatNum = MyPlantSizingIndex(state,
                                                       "Coil:Heating:Water",
                                                       this->ReheatName,
                                                       state.dataSingleDuct->CoilWaterInletNodeSS,
                                                       state.dataSingleDuct->CoilWaterOutletNodeSS,
                                                       PlantSizingErrorsFound);
                    if (PlantSizingErrorsFound) {
                        ShowContinueError(state, "...Occurs in " + this->SysType + ':' + this->SysName);
                        ErrorsFound = true;
                    }
                    if (PltSizHeatNum > 0) {
                        state.dataSingleDuct->CoilInTempSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatCoilInTempTU;
                        DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                        state.dataSingleDuct->DesZoneHeatLoadSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).NonAirSysDesHeatLoad;
                        state.dataSingleDuct->ZoneDesTempSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                        state.dataSingleDuct->ZoneDesHumRatSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                        // the coil load is the zone design heating load plus (or minus!) the reheat load
                        state.dataSingleDuct->DesCoilLoadSS =
                            state.dataSingleDuct->DesZoneHeatLoadSS + PsyCpAirFnW(state.dataSingleDuct->ZoneDesHumRatSS) * DesMassFlow *
                                                                          (state.dataSingleDuct->ZoneDesTempSS - state.dataSingleDuct->CoilInTempSS);
                        if (state.dataSingleDuct->DesCoilLoadSS >= SmallLoad) {

                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                   RoutineName);

                            Cp = GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                       DataGlobalConstants::HWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                       RoutineName);

                            MaxReheatWaterVolFlowDes =
                                state.dataSingleDuct->DesCoilLoadSS / (state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                        } else {
                            MaxReheatWaterVolFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of water flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state, "Occurs in AirTerminal Object=" + this->SysName);
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    this->MaxReheatWaterVolFlow = MaxReheatWaterVolFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, this->SysType, this->SysName, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxReheatWaterVolFlowDes);
                    BaseSizer::reportSizerOutput(state,
                                                 this->SysType,
                                                 this->SysName,
                                                 "Design Size Reheat Coil Sizing Air Volume Flow Rate [m3/s]",
                                                 TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow);
                    BaseSizer::reportSizerOutput(state,
                                                 this->SysType,
                                                 this->SysName,
                                                 "Design Size Reheat Coil Sizing Inlet Air Temperature [C]",
                                                 state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatCoilInTempTU);
                    BaseSizer::reportSizerOutput(state,
                                                 this->SysType,
                                                 this->SysName,
                                                 "Design Size Reheat Coil Sizing Inlet Air Humidity Ratio [kgWater/kgDryAir]",
                                                 state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatCoilInHumRatTU);
                } else { // Hard-size with sizing data
                    if (this->MaxReheatWaterVolFlow > 0.0 && MaxReheatWaterVolFlowDes > 0.0) {
                        MaxReheatWaterVolFlowUser = this->MaxReheatWaterVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     this->SysType,
                                                     this->SysName,
                                                     "Design Size Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxReheatWaterVolFlowDes,
                                                     "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxReheatWaterVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxReheatWaterVolFlowDes - MaxReheatWaterVolFlowUser) / MaxReheatWaterVolFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                                this->SysName + "\".");
                                ShowContinueError(
                                    state, format("User-Specified Maximum Reheat Water Flow Rate of {:.5R} [m3/s]", MaxReheatWaterVolFlowUser));
                                ShowContinueError(
                                    state,
                                    format("differs from Design Size Maximum Reheat Water Flow Rate of {:.5R} [m3/s]", MaxReheatWaterVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
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
    if (state.dataSize->CurTermUnitSizingNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
            if (this->MaxReheatSteamVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, this->SysType, this->SysName, "User-Specified Maximum Reheat Steam Flow Rate [m3/s]", this->MaxReheatSteamVolFlow);
            }
        } else {
            CheckZoneSizing(state, this->SysType, this->SysName);
            if (UtilityRoutines::SameString(this->ReheatComp, "Coil:Heating:Steam")) {
                state.dataSingleDuct->CoilSteamInletNodeSS = GetCoilSteamInletNode(state, "Coil:Heating:Steam", this->ReheatName, ErrorsFound);
                state.dataSingleDuct->CoilSteamOutletNodeSS = GetCoilSteamOutletNode(state, "Coil:Heating:Steam", this->ReheatName, ErrorsFound);
                if (IsAutoSize) {
                    PlantSizingErrorsFound = false;
                    PltSizHeatNum = MyPlantSizingIndex(state,
                                                       "Coil:Heating:Steam",
                                                       this->ReheatName,
                                                       state.dataSingleDuct->CoilSteamInletNodeSS,
                                                       state.dataSingleDuct->CoilSteamOutletNodeSS,
                                                       PlantSizingErrorsFound);
                    if (PlantSizingErrorsFound) {
                        ShowContinueError(state, "...Occurs in " + this->SysType + ':' + this->SysName);
                        ErrorsFound = true;
                    }
                    if (PltSizHeatNum > 0) {
                        state.dataSingleDuct->CoilInTempSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).DesHeatCoilInTempTU;
                        DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow;
                        state.dataSingleDuct->DesZoneHeatLoadSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).NonAirSysDesHeatLoad;
                        state.dataSingleDuct->ZoneDesTempSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).ZoneTempAtHeatPeak;
                        state.dataSingleDuct->ZoneDesHumRatSS =
                            state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).ZoneHumRatAtHeatPeak;
                        // the coil load is the zone design heating load plus (or minus!) the reheat load
                        state.dataSingleDuct->DesCoilLoadSS =
                            state.dataSingleDuct->DesZoneHeatLoadSS + PsyCpAirFnW(state.dataSingleDuct->ZoneDesHumRatSS) * DesMassFlow *
                                                                          (state.dataSingleDuct->ZoneDesTempSS - state.dataSingleDuct->CoilInTempSS);
                        if (state.dataSingleDuct->DesCoilLoadSS >= SmallLoad) {
                            TempSteamIn = 100.00;
                            EnthSteamInDry = GetSatEnthalpyRefrig(state, fluidNameSteam, TempSteamIn, 1.0, this->FluidIndex, RoutineNameFull);
                            EnthSteamOutWet = GetSatEnthalpyRefrig(state, fluidNameSteam, TempSteamIn, 0.0, this->FluidIndex, RoutineNameFull);
                            LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                            SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, TempSteamIn, 1.0, this->FluidIndex, RoutineNameFull);

                            Cp = GetSpecificHeatGlycol(state,
                                                       fluidNameWater,
                                                       state.dataSize->PlantSizData(PltSizHeatNum).ExitTemp,
                                                       state.dataSingleDuct->DummyWaterIndexSS,
                                                       RoutineName);
                            MaxReheatSteamVolFlowDes = state.dataSingleDuct->DesCoilLoadSS /
                                                       (SteamDensity * (LatentHeatSteam + state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp));
                        } else {
                            MaxReheatSteamVolFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state, "Occurs in AirTerminal:SingleDuct:ConstantVolume:Reheat Object=" + this->SysName);
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    this->MaxReheatSteamVolFlow = MaxReheatSteamVolFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, this->SysType, this->SysName, "Design Size Maximum Reheat Steam Flow Rate [m3/s]", MaxReheatSteamVolFlowDes);
                } else {
                    if (this->MaxReheatSteamVolFlow > 0.0 && MaxReheatSteamVolFlowDes > 0.0) {
                        MaxReheatSteamVolFlowUser = this->MaxReheatSteamVolFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     this->SysType,
                                                     this->SysName,
                                                     "Design Size Maximum Reheat Steam Flow Rate [m3/s]",
                                                     MaxReheatSteamVolFlowDes,
                                                     "User-Specified Maximum Reheat Steam Flow Rate [m3/s]",
                                                     MaxReheatSteamVolFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxReheatSteamVolFlowDes - MaxReheatSteamVolFlowUser) / MaxReheatSteamVolFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizeHVACSingleDuct: Potential issue with equipment sizing for " + this->SysType + " = \"" +
                                                this->SysName + "\".");
                                ShowContinueError(
                                    state, format("User-Specified Maximum Reheat Steam Flow Rate of {:.5R} [m3/s]", MaxReheatSteamVolFlowUser));
                                ShowContinueError(
                                    state,
                                    format("differs from Design Size Maximum Reheat Steam Flow Rate of {:.5R} [m3/s]", MaxReheatSteamVolFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }
        }
    } else {
        this->MaxReheatSteamVolFlow = 0.0;
    }

    if (state.dataSize->CurTermUnitSizingNum > 0) {
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).MinFlowFrac = this->ZoneMinAirFracDes * this->ZoneTurndownMinAirFrac;
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).MaxHWVolFlow = this->MaxReheatWaterVolFlow;
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).MaxSTVolFlow = this->MaxReheatSteamVolFlow;
        TermUnitSizing(state.dataSize->CurTermUnitSizingNum).DesHeatingLoad = state.dataSingleDuct->DesCoilLoadSS; // Coil Summary report
        if (this->ReheatComp_Num == HeatingCoilType::SimpleHeating) {
            if (this->DamperHeatingAction == Action::Normal) {
                SetCoilDesFlow(state, this->ReheatComp, this->ReheatName, this->ZoneMinAirFracDes * this->MaxAirVolFlowRate, ErrorsFound);
            } else {
                SetCoilDesFlow(
                    state, this->ReheatComp, this->ReheatName, TermUnitSizing(state.dataSize->CurTermUnitSizingNum).AirVolFlow, ErrorsFound);
            }
        }
    }

    if (this->MaxAirVolFlowRateDuringReheat > 0.0) {
        // check for inconsistent dual max input
        if (this->MaxAirVolFlowRateDuringReheat < (this->ZoneMinAirFracDes * this->MaxAirVolFlowRate)) {
            // Only warn when really out of bounds
            if ((this->ZoneMinAirFracDes * this->MaxAirVolFlowRate) - this->MaxAirVolFlowRateDuringReheat > 1.e-8) {
                ShowWarningError(state,
                                 "SingleDuctSystem:SizeSys: Air Terminal Unit flow limits are not consistent, minimum flow limit is larger than "
                                 "reheat maximum");
                ShowContinueError(state, "Air Terminal Unit name = " + this->SysName);
                ShowContinueError(state,
                                  format("Maximum terminal flow during reheat = {:.6R} [m3/s] or flow fraction = {:.4R}",
                                         this->MaxAirVolFlowRateDuringReheat,
                                         (this->MaxAirVolFlowRateDuringReheat / this->MaxAirVolFlowRate)));
                ShowContinueError(state,
                                  format("Minimum terminal flow = {:.6R} [m3/s] or flow fraction = {:.4R}",
                                         (this->ZoneMinAirFracDes * this->MaxAirVolFlowRate),
                                         this->ZoneMinAirFracDes));
                ShowContinueError(state, "The reheat maximum flow limit will be replaced by the minimum limit, and the simulation continues");
            }
            this->MaxAirVolFlowRateDuringReheat = (this->ZoneMinAirFracDes * this->MaxAirVolFlowRate);
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
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
    using DataHVACGlobals::SmallLoad;
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
    Real64 QTotLoad;     // [Watts] Remaining load required for this zone
    Real64 QZnReq;       // [Watts] Load calculated for heating coil
    Real64 QToHeatSetPt; // [W]  remaining load to heating setpoint
    Real64 CpAirAvg;
    Real64 DeltaTemp;
    int SysOutletNode;        // The node number of the terminal unit outlet node
    int SysInletNode;         // the node number of the terminal unit inlet node
    int WaterControlNode;     // This is the Actuated Reheat Control Node
    Real64 MaxFlowWater;      // This is the value passed to the Controller depending if FirstHVACIteration or not
    Real64 MinFlowWater;      // This is the value passed to the Controller depending if FirstHVACIteration or not
    Real64 QActualHeating;    // the heating load seen by the reheat coil
    Real64 QHeatingDelivered; // the actual output from heating coil
    Real64 LeakLoadMult;      // load multiplier to adjust for downstream leaks
    Real64 MinFlowFrac;       // minimum flow fraction (and minimum damper position)
    Real64 MassFlowBasedOnOA; // supply air mass flow rate based on zone OA requirements
    Real64 AirLoopOAFrac;     // fraction of outside air entering air loop
    Real64 DummyMdot;         // temporary mass flow rate argument

    // Note to the perplexed
    // The SINGLE DUCT:VAV:REHEAT terminal unit originally contained 2 components: a damper
    // and a reheat coil. The damper has become a virtual component - it consists only of
    // an air inlet node and an air outlet node. The damper is upstream of the heating coil.
    // sd_airterminal(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
    // sd_airterminal(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
    // sd_airterminal(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

    // The calculated load from the Heat Balance
    LeakLoadMult = state.dataDefineEquipment->AirDistUnit(this->ADUNum).LeakLoadMult;
    QTotLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired * LeakLoadMult;
    QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP * LeakLoadMult;
    SysOutletNode = this->ReheatAirOutletNode;
    SysInletNode = this->InletNodeNum;
    CpAirAvg = PsyCpAirFnW(0.5 * (state.dataLoopNodes->Node(ZoneNodeNum).HumRat + this->sd_airterminalInlet.AirHumRat));
    MinFlowFrac = this->ZoneMinAirFrac;
    MassFlowBasedOnOA = 0.0;
    state.dataSingleDuct->ZoneTempSDAT = state.dataLoopNodes->Node(ZoneNodeNum).Temp;
    state.dataSingleDuct->MinMassAirFlowSDAT = MinFlowFrac * state.dataEnvrn->StdRhoAir * this->MaxAirVolFlowRate;

    // Then depending on if the Load is for heating or cooling it is handled differently.  First
    // the massflow rate for cooling is determined to meet the entire load.  Then
    // if the massflow is below the minimum or greater than the Max it is set to either the Min
    // or the Max as specified for the VAV model.
    if ((QTotLoad < 0.0) && (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) &&
        (state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleHeatingSetPoint) && (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
        // Calculate the flow required for cooling

        DeltaTemp = CpAirAvg * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSDAT);

        // Need to check DeltaTemp and ensure that it is not zero
        if (DeltaTemp != 0.0) {
            MassFlow = QTotLoad / DeltaTemp;
        } else {
            MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        }

        // Apply the zone maximum outdoor air fraction FOR VAV boxes - a TRACE feature
        if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
            MassFlow *= state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
        }

        // calculate supply air flow rate based on user specified OA requirement
        this->CalcOAMassFlow(state, MassFlowBasedOnOA, AirLoopOAFrac);
        MassFlow = max(MassFlow, MassFlowBasedOnOA);

        // used for normal acting damper
        state.dataSingleDuct->MinMassAirFlowSDAT = max(state.dataSingleDuct->MinMassAirFlowSDAT, MassFlowBasedOnOA);
        state.dataSingleDuct->MinMassAirFlowSDAT = max(state.dataSingleDuct->MinMassAirFlowSDAT, this->sd_airterminalInlet.AirMassFlowRateMinAvail);
        state.dataSingleDuct->MinMassAirFlowSDAT = min(state.dataSingleDuct->MinMassAirFlowSDAT, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

        // limit the OA based supply air flow rate based on optional user input
        // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
        MassFlow = max(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);
        MassFlow = min(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
            state.dataAirflowNetwork->AirflowNetworkFanActivated && state.dataAirflowNetwork->VAVTerminalRatio > 0.0) {
            MassFlow *= state.dataAirflowNetwork->VAVTerminalRatio;
            if (MassFlow > state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate) {
                MassFlow = state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate;
            }
        }

    } else if ((this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) &&
               (QTotLoad >= 0.0 || state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleHeatingSetPoint) &&
               (GetCurrentScheduleValue(state, this->SchedPtr) > 0.0)) {
        //     IF (sd_airterminal(SysNum)%DamperHeatingAction .EQ. ReverseAction .AND. this->sd_airterminalInlet%AirMassFlowRateMinAvail <=
        //     SmallMassFlow) THEN
        // special case for heating: reverse action and damper allowed to close - set the minimum flow rate to a small but nonzero value
        //       MassFlow = 0.01d0*this->sd_airterminalInlet%AirMassFlowRateMaxAvail
        //     ELSE
        // usual case for heating: set the air mass flow rate to the minimum
        MassFlow = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
        //     END IF

        // Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
        if (state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor > 1.0) {
            MassFlow *= state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).SupplyAirAdjustFactor;
        }

        // calculate supply air flow rate based on user specified OA requirement
        this->CalcOAMassFlow(state, MassFlowBasedOnOA, AirLoopOAFrac);
        MassFlow = max(MassFlow, MassFlowBasedOnOA);

        // Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
        if (MassFlow <= this->sd_airterminalInlet.AirMassFlowRateMinAvail) {
            MassFlow = this->sd_airterminalInlet.AirMassFlowRateMinAvail;
        } else if (MassFlow >= this->sd_airterminalInlet.AirMassFlowRateMaxAvail) {
            MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
        }

        // the AirflowNetwork model overrids the mass flow rate value
        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
            state.dataAirflowNetwork->AirflowNetworkFanActivated && state.dataAirflowNetwork->VAVTerminalRatio > 0.0) {
            MassFlow *= state.dataAirflowNetwork->VAVTerminalRatio;
            if (MassFlow > state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate) {
                MassFlow = state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate;
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
    //                                   (this->sd_airterminalInlet%AirMassFlowRateMaxAvail-this->sd_airterminalInlet%AirMassFlowRateMinAvail)) *
    //                                   &
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
    this->UpdateSys(state);

    // At the current air mass flow rate, calculate heating coil load
    QActualHeating = QToHeatSetPt - MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSDAT); // reheat needed

    // do the reheat calculation if there's some air nass flow (or the damper action is "reverse action"), the flow is <= minimum ,
    // there's a heating requirement, and there's a thermostat with a heating setpoint
    // Reverse damper option is working only for water coils for now.
    if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint)) {
        // At this point we know that there is a heating requirement: i.e., the heating coil needs to
        // be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
        // situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
        // the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
        // temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
        // max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
        // and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
        // max we are in condition 2.

        state.dataSingleDuct->QZoneMax2SDAT = QToHeatSetPt;

        // fill dual-max reheat flow limit, if any
        if (this->DamperHeatingAction == Action::ReverseAction) {
            state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT = this->AirMassFlowRateMax;
        } else if (this->DamperHeatingAction == Action::ReverseActionWithLimits) {
            state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT = this->AirMassFlowDuringReheatMax;
        } else if (this->DamperHeatingAction == Action::Normal) {
            state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT = this->ZoneMinAirFrac * this->AirMassFlowRateMax;
        } else {
            // used for AIRTERMINAL_SINGLEDUCT_VAV_NOREHEAT or SingleDuctVAVNoReheat
            state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT = this->AirMassFlowRateMax;
        }

        // determine flow based on leaving reheat temperature limit
        if (this->MaxReheatTempSetByUser) {

            state.dataSingleDuct->MaxHeatTempSDAT = this->MaxReheatTemp;
            if (QToHeatSetPt > SmallLoad) { // zone has a positive load to heating setpoint
                state.dataSingleDuct->MassFlowReqToLimitLeavingTempSDAT =
                    QToHeatSetPt / (CpAirAvg * (state.dataSingleDuct->MaxHeatTempSDAT - state.dataSingleDuct->ZoneTempSDAT));
            } else {
                state.dataSingleDuct->MassFlowReqToLimitLeavingTempSDAT = 0.0;
            }
        }

        // (re)apply limits to find air mass flow
        MassFlow = max(MassFlow, state.dataSingleDuct->MassFlowReqToLimitLeavingTempSDAT);
        MassFlow = min(MassFlow, state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT);
        MassFlow = max(MassFlow, MassFlowBasedOnOA);
        MassFlow = min(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
        MassFlow = max(MassFlow, this->sd_airterminalInlet.AirMassFlowRateMinAvail);

        if (state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone &&
            state.dataAirflowNetwork->AirflowNetworkFanActivated && state.dataAirflowNetwork->VAVTerminalRatio > 0.0) {
            MassFlow *= state.dataAirflowNetwork->VAVTerminalRatio;
            if (MassFlow > state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate) {
                MassFlow = state.dataLoopNodes->Node(this->InletNodeNum).MassFlowRate;
            }
        }

        // now make any corrections to heating coil loads
        if (this->MaxReheatTempSetByUser) {
            state.dataSingleDuct->QZoneMaxRHTempLimitSDAT =
                MassFlow * CpAirAvg * (state.dataSingleDuct->MaxHeatTempSDAT - state.dataSingleDuct->ZoneTempSDAT);
            state.dataSingleDuct->QZoneMax2SDAT = min(state.dataSingleDuct->QZoneMaxRHTempLimitSDAT, QToHeatSetPt);
        }

        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;

        this->UpdateSys(state);

        // Now do the heating coil calculation for each heating coil type
        {
            auto const SELECT_CASE_var(this->ReheatComp_Num); // Reverse damper option is working only for water coils for now.

            // hot water heating coil
            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Determine the load required to pass to the Component controller
                // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                // and is working as-is, temperature setpoints are maintained as expected.
                QZnReq = state.dataSingleDuct->QZoneMax2SDAT + MassFlow * CpAirAvg * state.dataSingleDuct->ZoneTempSDAT;

                // Initialize hot water flow rate to zero.
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (FirstHVACIteration) {
                    MaxFlowWater = this->MaxReheatWaterFlow;
                    MinFlowWater = this->MinReheatWaterFlow;
                } else {
                    WaterControlNode = this->ReheatControlNode;
                    MaxFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMaxAvail;
                    MinFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMinAvail;
                }

                // Simulate the reheat coil at constant air flow. Control by varying the
                // hot water flow rate.
                // FB use QActualHeating, change ControlCompOutput to use new
                ControlCompOutput(state,
                                  this->ReheatName,
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
                if (this->DamperHeatingAction == Action::ReverseAction || this->DamperHeatingAction == Action::ReverseActionWithLimits) {
                    if (state.dataLoopNodes->Node(this->ReheatControlNode).MassFlowRate == MaxFlowWater) {
                        // fill limits for air flow for controller
                        state.dataSingleDuct->MinAirMassFlowRevActSVAV = this->AirMassFlowRateMax * this->ZoneMinAirFrac;
                        state.dataSingleDuct->MinAirMassFlowRevActSVAV =
                            min(state.dataSingleDuct->MinAirMassFlowRevActSVAV, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
                        state.dataSingleDuct->MinAirMassFlowRevActSVAV =
                            max(state.dataSingleDuct->MinAirMassFlowRevActSVAV, this->sd_airterminalInlet.AirMassFlowRateMinAvail);

                        state.dataSingleDuct->MaxAirMassFlowRevActSVAV = this->AirMassFlowRateMax;
                        state.dataSingleDuct->MaxAirMassFlowRevActSVAV =
                            min(state.dataSingleDuct->MaxAirMassFlowRevActSVAV, state.dataSingleDuct->MaxDeviceAirMassFlowReheatSDAT);
                        state.dataSingleDuct->MaxAirMassFlowRevActSVAV =
                            max(state.dataSingleDuct->MaxAirMassFlowRevActSVAV, state.dataSingleDuct->MinAirMassFlowRevActSVAV);
                        state.dataSingleDuct->MaxAirMassFlowRevActSVAV =
                            min(state.dataSingleDuct->MaxAirMassFlowRevActSVAV, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);

                        state.dataLoopNodes->Node(this->OutletNodeNum).MassFlowRateMaxAvail =
                            state.dataSingleDuct->MaxAirMassFlowRevActSVAV; // suspect, check how/if used in ControlCompOutput
                        ControlCompOutput(state,
                                          this->ReheatName,
                                          this->ReheatComp,
                                          this->ReheatComp_Index,
                                          FirstHVACIteration,
                                          state.dataSingleDuct->QZoneMax2SDAT,
                                          this->OutletNodeNum,
                                          state.dataSingleDuct->MaxAirMassFlowRevActSVAV,
                                          state.dataSingleDuct->MinAirMassFlowRevActSVAV,
                                          this->ControllerOffset,
                                          this->ControlCompTypeNum,
                                          this->CompErrIndex,
                                          ZoneNodeNum,
                                          SysOutletNode); // why not QZnReq  ?
                        // air flow controller, not on plant, don't pass plant topology info
                        // reset terminal unit inlet air mass flow to new value.
                        state.dataLoopNodes->Node(this->OutletNodeNum).MassFlowRateMaxAvail = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
                        MassFlow = state.dataLoopNodes->Node(SysOutletNode).MassFlowRate;

                        //         ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
                        //         ! equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
                        if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) ||
                             (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
                            (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
                            if (MassFlow > 0.0) MassFlow = this->MassFlow1;
                            this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                            this->UpdateSys(state);

                            // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                            // and is working as-is, temperature setpoints are maintained as expected.
                            QZnReq = state.dataSingleDuct->QZoneMax2SDAT + MassFlow * CpAirAvg * state.dataSingleDuct->ZoneTempSDAT;
                            ControlCompOutput(state,
                                              this->ReheatName,
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
                        this->UpdateSys(state);
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

            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // ! COIL:STEAM:AIRHEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QZoneMax2SDAT -
                         MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSDAT);

                // Simulate reheat coil for the VAV system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);

            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QZoneMax2SDAT -
                         MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSDAT);

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QZoneMax2SDAT -
                         MassFlow * CpAirAvg * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSDAT);

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index, QHeatingDelivered);

            } else if (SELECT_CASE_var == HeatingCoilType::None) { // blank
                                                                   // I no reheat is defined then assume that the damper is the only component.
                // If something else is there that is not a reheat coil or a blank then give the error message

            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
            }
        }

        // the COIL is OFF the properties are calculated for this special case.
    } else {
        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);

            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Simulate reheat coil for the Const Volume system
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                // call the reheat coil with the NO FLOW condition to make sure that the Node values
                // are passed through to the coil outlet correctly
                SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // COIL:STEAM:AIRHEATING
                // Simulate reheat coil for the VAV system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::None) { // blank
                                                                   // If no reheat is defined then assume that the damper is the only component.
                // If something else is that is not a reheat coil or a blank then give the error message

            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
            }
        }
    }

    // push the flow rate history
    this->MassFlow3 = this->MassFlow2;
    this->MassFlow2 = this->MassFlow1;
    this->MassFlow1 = MassFlow;
}

void SingleDuctAirTerminal::CalcOAMassFlow(EnergyPlusData &state,
                                           Real64 &SAMassFlow,   // outside air based on optional user input
                                           Real64 &AirLoopOAFrac // outside air based on optional user input
) const
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
        AirLoopOAFrac = state.dataAirLoop->AirLoopFlow(AirLoopNum).OAFrac;
        // If no additional input from user, RETURN from subroutine
        if (this->NoOAFlowInputFromUser) return;
        // Calculate outdoor air flow rate, zone multipliers are applied in GetInput
        if (AirLoopOAFrac > 0.0) {
            OAVolumeFlowRate = DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                             this->OARequirementsPtr,
                                                                             this->ActualZoneNum,
                                                                             state.dataAirLoop->AirLoopControlInfo(AirLoopNum).AirLoopDCVFlag,
                                                                             UseMinOASchFlag);
            OAMassFlow = OAVolumeFlowRate * state.dataEnvrn->StdRhoAir;

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
    Real64 MinFlowFrac; // minimum flow fraction (and minimum damper position)

    // sd_airterminal(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
    // sd_airterminal(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
    // sd_airterminal(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

    // The calculated load from the Heat Balance
    LeakLoadMult = state.dataDefineEquipment->AirDistUnit(this->ADUNum).LeakLoadMult;
    QTotLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired * LeakLoadMult;
    QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP * LeakLoadMult;
    SysOutletNode = this->ReheatAirOutletNode;
    SysInletNode = this->InletNodeNum;
    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
    MinFlowFrac = this->ZoneMinAirFrac;
    state.dataSingleDuct->MinMassAirFlowSCBVAV = MinFlowFrac * state.dataEnvrn->StdRhoAir * this->MaxAirVolFlowRate;
    state.dataSingleDuct->ZoneTempSCBVAV = state.dataLoopNodes->Node(ZoneNodeNum).Temp;

    // Then depending on if the Load is for heating or cooling it is handled differently.  First
    // the massflow rate for cooling is determined to meet the entire load.  Then
    // if the massflow is below the minimum or greater than the Max it is set to either the Min
    // or the Max as specified for the VAV model.
    if (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) {
        // Calculate the flow required for cooling
        CpAirSysIn = PsyCpAirFnW(this->sd_airterminalInlet.AirHumRat);
        DeltaTemp = CpAirSysIn * this->sd_airterminalInlet.AirTemp - CpAirZn * state.dataSingleDuct->ZoneTempSCBVAV;

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
    this->UpdateSys(state);

    QActualHeating = QToHeatSetPt - MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCBVAV);

    if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint)) {
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

        state.dataSingleDuct->QZoneMax2SCBVAV = QToHeatSetPt;

        if (this->MaxReheatTempSetByUser) {

            state.dataSingleDuct->MaxHeatTempSCBVAV = this->MaxReheatTemp;
            if (QToHeatSetPt > SmallLoad) { // zone has a positive load to heating setpoint
                state.dataSingleDuct->MassFlowReqSCBVAV =
                    QToHeatSetPt / (CpAirZn * (state.dataSingleDuct->MaxHeatTempSCBVAV - state.dataSingleDuct->ZoneTempSCBVAV));
            } else {
                state.dataSingleDuct->MassFlowReqSCBVAV = MassFlow;
            }

            state.dataSingleDuct->QZoneMax3SCBVAV =
                CpAirZn * (state.dataSingleDuct->MaxHeatTempSCBVAV - state.dataSingleDuct->ZoneTempSCBVAV) * MassFlow;

            state.dataSingleDuct->MassFlowActualSCBVAV = MassFlow;

            if (state.dataSingleDuct->QZoneMax3SCBVAV < QToHeatSetPt) {
                state.dataSingleDuct->MassFlowActualSCBVAV = state.dataSingleDuct->MassFlowReqSCBVAV;
                // QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlowActual
            }

            if (state.dataSingleDuct->MassFlowActualSCBVAV <= state.dataSingleDuct->MinMassAirFlowSCBVAV) {
                state.dataSingleDuct->MassFlowActualSCBVAV = state.dataSingleDuct->MinMassAirFlowSCBVAV;
            } else if (state.dataSingleDuct->MassFlowActualSCBVAV >= this->AirMassFlowRateMax) {
                state.dataSingleDuct->MassFlowActualSCBVAV = this->AirMassFlowRateMax;
            }

            state.dataSingleDuct->QZoneMaxSCBVAV = CpAirZn * state.dataSingleDuct->MassFlowActualSCBVAV *
                                                   (state.dataSingleDuct->MaxHeatTempSCBVAV - state.dataSingleDuct->ZoneTempSCBVAV);

            // temporary variable
            state.dataSingleDuct->QZoneMax2SCBVAV = min(state.dataSingleDuct->QZoneMaxSCBVAV, QToHeatSetPt);

            MassFlow = state.dataSingleDuct->MassFlowActualSCBVAV;

        } // IF (sd_airterminal(SysNum)%MaxReheatTempSetByUser) THEN

        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;

        this->UpdateSys(state);

        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);

            // hot water heating coil
            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Determine the load required to pass to the Component controller
                // Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
                // and is working as-is, temperature setpoints are maintained as expected.
                QZnReq = state.dataSingleDuct->QZoneMax2SCBVAV + MassFlow * CpAirZn * state.dataLoopNodes->Node(ZoneNodeNum).Temp;
                if (QZnReq < SmallLoad) QZnReq = 0.0;

                // Initialize hot water flow rate to zero.
                // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);
                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (FirstHVACIteration) {
                    MaxFlowWater = this->MaxReheatWaterFlow;
                    MinFlowWater = this->MinReheatWaterFlow;
                } else {
                    WaterControlNode = this->ReheatControlNode;
                    MaxFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMaxAvail;
                    MinFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMinAvail;
                }

                // Simulate the reheat coil at constant air flow. Control by varying the
                // hot water flow rate.
                ControlCompOutput(state,
                                  this->ReheatName,
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
                if (this->DamperHeatingAction == Action::ReverseAction) {
                    if (state.dataLoopNodes->Node(this->ReheatControlNode).MassFlowRate == this->MaxReheatWaterFlow) {
                        ControlCompOutput(state,
                                          this->ReheatName,
                                          this->ReheatComp,
                                          this->ReheatComp_Index,
                                          FirstHVACIteration,
                                          state.dataSingleDuct->QZoneMax2SCBVAV,
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
                        MassFlow = state.dataLoopNodes->Node(SysOutletNode).MassFlowRate;
                        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                        this->UpdateSys(state);
                    }
                    // look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
                    // equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
                    if (((std::abs(MassFlow - this->MassFlow2) < this->MassFlowDiff) ||
                         (std::abs(MassFlow - this->MassFlow3) < this->MassFlowDiff)) &&
                        (std::abs(MassFlow - this->MassFlow1) >= this->MassFlowDiff)) {
                        MassFlow = this->MassFlow1;
                        this->sd_airterminalOutlet.AirMassFlowRate = MassFlow;
                        this->UpdateSys(state);
                        ControlCompOutput(state,
                                          this->ReheatName,
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
            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // ! COIL:STEAM:AIRHEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QZoneMax2SCBVAV -
                         MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCBVAV);
                if (QZnReq < SmallLoad) QZnReq = 0.0;

                // Simulate reheat coil for the VAV system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);

            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Determine the load required to pass to the Component controller
                QSupplyAir = MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCBVAV);
                QZnReq = state.dataSingleDuct->QZoneMax2SCBVAV - QSupplyAir;
                if (QZnReq < SmallLoad) QZnReq = 0.0;

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QZoneMax2SCBVAV -
                         MassFlow * CpAirZn * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCBVAV);
                if (QZnReq < SmallLoad) QZnReq = 0.0;

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::None) { // blank
                                                                   // If no reheat is defined then assume that the damper is the only component.
                // If something else is there that is not a reheat coil then give the error message below.

            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
            }
        }

        // the COIL is OFF the properties are calculated for this special case.
    } else {
        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);

            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Simulate reheat coil for the Const Volume system
                // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                // Initialize hot water flow rate to zero.
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                // call the reheat coil with the NO FLOW condition to make sure that the Node values
                // are passed through to the coil outlet correctly
                SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // COIL:STEAM:AIRHEATING
                // Simulate reheat coil for the VAV system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::None) { // blank
                                                                   // If no reheat is defined then assume that the damper is the only component.
                                                                   // If something else is there that is not a reheat coil then give the error message

            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
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

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;
    using General::SolveRoot;
    using SteamCoils::GetCoilCapacity;

    // SUBROUTINE PARAMETER DEFINITIONS:
    Real64 const BigLoad(1.0e+20);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MassFlow = 0; // [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
    Real64 QTotLoad;     // [Watts]
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
    HeatingCoilType HCType; // heating coil type
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
    QTotLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
    SysOutletNode = this->ReheatAirOutletNode;
    SysInletNode = this->InletNodeNum;
    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
    HCType = this->ReheatComp_Num;
    FanType = this->Fan_Num;
    MaxCoolMassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;
    MaxHeatMassFlow = min(this->HeatAirMassFlowRateMax, this->sd_airterminalInlet.AirMassFlowRateMaxAvail);
    MinMassFlow = MaxCoolMassFlow * this->ZoneMinAirFrac;
    UnitFlowToler = 0.001 * DataConvergParams::HVACFlowRateToler;
    QDelivered = 0.0;
    HWFlow = 0.0;
    if (this->sd_airterminalInlet.AirMassFlowRateMaxAvail <= 0.0 || state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
        MassFlow = 0.0;
        FanOp = 0;
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered);
        return;
    }

    if (HCType == HeatingCoilType::SimpleHeating) {
        WaterControlNode = this->ReheatControlNode;
        HCLoad = 0.0;
        if (FirstHVACIteration) {
            MaxFlowWater = this->MaxReheatWaterFlow;
            MinFlowWater = this->MinReheatWaterFlow;
        } else {
            WaterControlNode = this->ReheatControlNode;
            MaxFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMaxAvail;
            MinFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMinAvail;
        }
    } else {
        WaterControlNode = 0;
        HCLoad = BigLoad;
        MaxFlowWater = 0.0;
        MinFlowWater = 0.0;
    }

    if (HCType == HeatingCoilType::SteamAirHeating) {
        SteamControlNode = this->ReheatControlNode;
        HCLoad = 0.0;
        if (FirstHVACIteration) {
            MaxFlowSteam = this->MaxReheatSteamFlow;
            MinFlowSteam = this->MinReheatSteamFlow;
        } else {
            MaxFlowSteam = state.dataLoopNodes->Node(SteamControlNode).MassFlowRateMaxAvail;
            MinFlowSteam = state.dataLoopNodes->Node(SteamControlNode).MassFlowRateMinAvail;
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
    if (HCType == HeatingCoilType::SteamAirHeating) {
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowSteam, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin);
        // region 2: active heating with fan on
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowSteam, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax);
        MaxSteamCap = GetCoilCapacity(state, this->ReheatComp, this->ReheatName, ErrorsFound);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin);
        // region 3: active heating with fan off
        FanOp = 0;
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowSteam, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowSteam, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff);
    } else {
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowWater, 0.0, FanType, MaxCoolMassFlow, FanOp, QCoolFanOnMax);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QCoolFanOnMin);
        // region 2: active heating with fan on
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, BigLoad, FanType, MaxHeatMassFlow, FanOp, QHeatFanOnMax);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, 0.0, FanType, MinMassFlow, FanOp, QHeatFanOnMin);
        // region 3: active heating with fan off
        FanOp = 0;
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, BigLoad, FanType, MinMassFlow, FanOp, QHeatFanOffMax);
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowWater, 0.0, FanType, MinMassFlow, FanOp, QNoHeatFanOff);
    }

    // Active cooling with fix for issue #5592
    if (QTotLoad < (-1.0 * SmallLoad) && QTotLoad < QCoolFanOnMin - SmallLoad && this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 &&
        !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
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
            if (HCType == HeatingCoilType::SteamAirHeating) {
                Par(5) = MinFlowSteam;
            } else {
                Par(5) = MinFlowWater;
            }
            Par(6) = double(FanType);
            Par(7) = double(FanOp);
            Par(8) = QTotLoad;
            SolveRoot(state,
                      UnitFlowToler,
                      50,
                      SolFlag,
                      MassFlow,
                      EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSCoolingResidual,
                      MinMassFlow,
                      MaxCoolMassFlow,
                      Par);
            if (SolFlag == -1) {
                if (this->IterationLimit == 0) {
                    ShowWarningError(state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                    ShowContinueError(state, "  Iteration limit exceeded in calculating air flow rate");
                }
                ShowRecurringWarningErrorAtEnd(
                    state, "Supply air flow Iteration limit exceeded in VS VAV terminal unit " + this->SysName, this->IterationLimit);
            } else if (SolFlag == -2) {
                if (this->IterationFailed == 0) {
                    ShowWarningError(state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                    ShowContinueError(state, "  Bad air flow limits");
                }
                ShowRecurringWarningErrorAtEnd(
                    state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName, this->IterationFailed);
            }

        } else {

            MassFlow = MaxCoolMassFlow;

            if (HCType == HeatingCoilType::SteamAirHeating) {
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowSteam, 0.0, FanType, MassFlow, FanOp, QDelivered);
            } else {
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
            }
        }

        // no active heating or cooling
    } else if ((QTotLoad >= QCoolFanOnMin - SmallLoad && QTotLoad <= QNoHeatFanOff + SmallLoad &&
                this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0) ||
               (this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 && state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum))) {
        MassFlow = MinMassFlow;
        FanOp = 0;
        if (HCType == HeatingCoilType::SteamAirHeating) {
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowSteam, QTotLoad, FanType, MassFlow, FanOp, QNoHeatFanOff);
        } else {
            this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MinFlowWater, 0.0, FanType, MassFlow, FanOp, QNoHeatFanOff);
        }

        // active heating
    } else if (QTotLoad > QNoHeatFanOff + SmallLoad && this->sd_airterminalInlet.AirMassFlowRateMaxAvail > 0.0 &&
               !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum)) {
        // hot water coil
        if (HCType == HeatingCoilType::SimpleHeating) {
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
                Par(4) = double(static_cast<int>(HCType));
                Par(5) = MassFlow;
                Par(6) = double(FanType);
                Par(7) = double(FanOp);
                Par(8) = QTotLoad;
                ErrTolerance = this->ControllerOffset;
                SolveRoot(state,
                          ErrTolerance,
                          500,
                          SolFlag,
                          HWFlow,
                          EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSHWNoFanResidual,
                          MinFlowWater,
                          MaxFlowWater,
                          Par);
                if (SolFlag == -1) {
                    ShowRecurringWarningErrorAtEnd(state, "Hot Water flow control failed in VS VAV terminal unit " + this->SysName, this->ErrCount1);
                    ShowRecurringContinueErrorAtEnd(
                        state, "...Iteration limit (500) exceeded in calculating the hot water flow rate", this->ErrCount1c);
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered);
                } else if (SolFlag == -2) {
                    ShowRecurringWarningErrorAtEnd(
                        state, "Hot Water flow control failed (bad air flow limits) in VS VAV terminal unit " + this->SysName, this->ErrCount2);
                }
            } else if (QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad) {
                MassFlow = MinMassFlow;
                FanOp = 0;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
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
                SolveRoot(state,
                          UnitFlowToler,
                          50,
                          SolFlag,
                          MassFlow,
                          EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSHWFanOnResidual,
                          MinMassFlow,
                          MaxHeatMassFlow,
                          Par);
                if (SolFlag == -1) {
                    if (this->IterationLimit == 0) {
                        ShowWarningError(state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Iteration limit exceeded in calculating air flow rate");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Supply air flow Iteration limit exceeded in VS VAV terminal unit " + this->SysName, this->IterationLimit);
                } else if (SolFlag == -2) {
                    if (this->IterationFailed == 0) {
                        ShowWarningError(state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Bad air flow limits");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Supply air flow control failed in VS VAV terminal unit " + this->SysName, this->IterationFailed);
                }
            } else {
                MassFlow = MaxHeatMassFlow;
                FanOp = 1;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
            }
        } else if (HCType == HeatingCoilType::SteamAirHeating) {
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
                Par(4) = double(static_cast<int>(HCType));
                Par(5) = MassFlow;
                Par(6) = double(FanType);
                Par(7) = double(FanOp);
                Par(8) = QTotLoad;
                Par(9) = MinFlowSteam;
                Par(10) = MaxFlowSteam;
                Par(11) = MaxSteamCap;
                ErrTolerance = this->ControllerOffset;
                SolveRoot(state,
                          ErrTolerance,
                          500,
                          SolFlag,
                          HWFlow,
                          EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSHWNoFanResidual,
                          MinFlowSteam,
                          MaxFlowSteam,
                          Par);
                if (SolFlag == -1) {
                    ShowRecurringWarningErrorAtEnd(state, "Steam flow control failed in VS VAV terminal unit " + this->SysName, this->ErrCount1);
                    ShowRecurringContinueErrorAtEnd(
                        state, "...Iteration limit (500) exceeded in calculating the hot water flow rate", this->ErrCount1c);
                    this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, HWFlow, 0.0, FanType, MassFlow, FanOp, QDelivered);
                } else if (SolFlag == -2) {
                    ShowRecurringWarningErrorAtEnd(
                        state, "Steam flow control failed (bad air flow limits) in VS VAV terminal unit " + this->SysName, this->ErrCount2);
                }
            } else if (QTotLoad >= QHeatFanOffMax - SmallLoad && QTotLoad <= QHeatFanOnMin + SmallLoad) {
                MassFlow = MinMassFlow;
                FanOp = 0;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, MaxFlowWater, 0.0, FanType, MassFlow, FanOp, QDelivered);
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
                SolveRoot(state,
                          UnitFlowToler,
                          50,
                          SolFlag,
                          MassFlow,
                          EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSHWFanOnResidual,
                          MinMassFlow,
                          MaxHeatMassFlow,
                          Par);
                if (SolFlag == -1) {
                    if (this->IterationLimit == 0) {
                        ShowWarningError(state, "Steam heating coil control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Iteration limit exceeded in calculating air flow rate");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Steam heating coil iteration limit exceeded in VS VAV terminal unit " + this->SysName, this->IterationLimit);
                } else if (SolFlag == -2) {
                    if (this->IterationFailed == 0) {
                        ShowWarningError(state, "Steam heating coil control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Bad air flow limits");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Steam heating coil control failed in VS VAV terminal unit " + this->SysName, this->IterationFailed);
                }
            } else {
                MassFlow = MaxHeatMassFlow;
                FanOp = 1;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, QTotLoad, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
            }
        } else if (HCType == HeatingCoilType::Gas || HCType == HeatingCoilType::Electric) {
            if (QTotLoad <= QHeatFanOnMin + SmallLoad) {
                // vary heating coil power, leave mass flow at minimum
                MassFlow = MinMassFlow;
                FanOp = 0;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
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
                SolveRoot(state,
                          UnitFlowToler,
                          50,
                          SolFlag,
                          FracDelivered,
                          EnergyPlus::SingleDuct::SingleDuctAirTerminal::VAVVSHCFanOnResidual,
                          0.0,
                          1.0,
                          Par);
                MassFlow = state.dataLoopNodes->Node(SysInletNode).MassFlowRate;
                if (SolFlag == -1) {
                    if (this->IterationLimit == 0) {
                        ShowWarningError(state, "Heating coil control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Iteration limit exceeded in calculating air flow rate");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Heating coil control iteration limit exceeded in VS VAV terminal unit " + this->SysName, this->IterationLimit);
                } else if (SolFlag == -2) {
                    if (this->IterationFailed == 0) {
                        ShowWarningError(state, "Heating coil control failed in VS VAV terminal unit " + this->SysName);
                        ShowContinueError(state, "  Bad air flow limits");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state, "Heating coil control failed in VS VAV terminal unit " + this->SysName, this->IterationFailed);
                }
            } else {
                MassFlow = MaxHeatMassFlow;
                FanOp = 1;
                this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, 0.0, QTotLoad, FanType, MassFlow, FanOp, QDelivered);
            }
        } else {
            ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
        }

    } else {

        MassFlow = 0.0;
        FanOp = 0;
        this->CalcVAVVS(state, FirstHVACIteration, ZoneNodeNum, 0.0, 0.0, FanType, MassFlow, FanOp, QDelivered);
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
    this->UpdateSys(state);
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
    int WaterControlNode;  // This is the Actuated Reheat Control Node
    Real64 MaxFlowWater;   // This is the value passed to the Controller depending if FirstHVACIteration or not
    Real64 MinFlowWater;   // This is the value passed to the Controller depending if FirstHVACIteration or not
    Real64 QActualHeating; // the heating load seen by the reheat coil
    Real64 DummyMdot;      // local fluid mass flow rate

    QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP; // The calculated load from the Heat Balance
    MassFlow = this->sd_airterminalInlet.AirMassFlowRateMaxAvail;                                       // System massflow is set to the Available
    state.dataSingleDuct->QMax2SCV = QToHeatSetPt;
    state.dataSingleDuct->ZoneTempSCV = state.dataLoopNodes->Node(ZoneNodeNum).Temp;
    CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNodeNum).HumRat); // zone air specific heat
    if (this->MaxReheatTempSetByUser) {
        state.dataSingleDuct->TAirMaxSCV = this->MaxReheatTemp;
        state.dataSingleDuct->QMaxSCV = CpAir * MassFlow * (state.dataSingleDuct->TAirMaxSCV - state.dataSingleDuct->ZoneTempSCV);
        state.dataSingleDuct->QMax2SCV = min(QToHeatSetPt, state.dataSingleDuct->QMaxSCV);
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
    this->UpdateSys(state);

    QActualHeating = QToHeatSetPt - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCV); // reheat needed
    // Now the massflow for reheating has been determined. If it is zero, or in SetBack, or the
    // system scheduled OFF then not operational and shut the system down.
    if ((MassFlow > SmallMassFlow) && (QActualHeating > 0.0) && (state.dataHeatBalFanSys->TempControlType(ZoneNum) != SingleCoolingSetPoint)) {

        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);

            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QMax2SCV + MassFlow * CpAir * state.dataSingleDuct->ZoneTempSCV;

                // Before Iterating through the Reheat Coil and Controller set the flags for the
                // Do Loop to initialized conditions.
                // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                // Initialize hot water flow rate to zero.
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (FirstHVACIteration) {
                    MaxFlowWater = this->MaxReheatWaterFlow;
                    MinFlowWater = this->MinReheatWaterFlow;
                } else {
                    WaterControlNode = this->ReheatControlNode;
                    MaxFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMaxAvail;
                    MinFlowWater = state.dataLoopNodes->Node(WaterControlNode).MassFlowRateMinAvail;
                }

                // Simulate reheat coil for the Const Volume system
                // Set Converged to True & when controller is not converged it will set to False.
                ControlCompOutput(state,
                                  this->ReheatName,
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

            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // COIL:STEAM:STEAMAIRHEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QMax2SCV - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCV);

                // Simulate reheat coil for the VAV system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, QZnReq);
            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QMax2SCV - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCV);

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Determine the load required to pass to the Component controller
                QZnReq = state.dataSingleDuct->QMax2SCV - MassFlow * CpAir * (this->sd_airterminalInlet.AirTemp - state.dataSingleDuct->ZoneTempSCV);

                // Simulate reheat coil for the VAV system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, QZnReq, this->ReheatComp_Index);
            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
            }
        }

        // the COIL is OFF the properties are calculated for this special case.
    } else {
        {
            auto const SELECT_CASE_var(this->ReheatComp_Num);

            if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
                // Simulate reheat coil for the Const Volume system
                // Node(sd_airterminal(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
                // Initialize hot water flow rate to zero.
                DummyMdot = 0.0;
                SetActuatedBranchFlowRate(state, DummyMdot, this->ReheatControlNode, this->HWLoopNum, this->HWLoopSide, this->HWBranchIndex, true);

                // call the reheat coil with the NO FLOW condition to make sure that the Node values
                // are passed through to the coil outlet correctly
                SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
            } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // COIL:STEAM:AIRHEATING
                // Simulate reheat coil for the Const Volume system
                SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, 0.0);

            } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
                // Simulate reheat coil for the Const Volume system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);

            } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
                // Simulate reheat coil for the Const Volume system
                SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, 0.0, this->ReheatComp_Index);
            } else {
                ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
            }
        }
    }
}

void SingleDuctAirTerminal::SimConstVolNoReheat(EnergyPlusData &state)
{

    // PURPOSE OF THIS SUBROUTINE:
    // Sets outlet flow rate and conditions for singleduct constantvolume with no reheat air terminal.

    this->sd_airterminalOutlet = this->sd_airterminalInlet;

    // update the air terminal outlet node data
    this->UpdateSys(state);
}

void SingleDuctAirTerminal::CalcVAVVS(EnergyPlusData &state,
                                      bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
                                      int const ZoneNode,            // zone node number
                                      Real64 const HWFlow,           // hot water flow (kg/s)
                                      Real64 const HCoilReq,         // gas or elec coil demand requested
                                      int const FanType,             // type of fan
                                      Real64 const AirFlow,          // air flow rate (kg/s)
                                      int const FanOn,               // 1 means fan is on
                                      Real64 &LoadMet                // load met by unit (watts)
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

    TurnFansOffSav = state.dataHVACGlobal->TurnFansOff;
    FanInNode = this->InletNodeNum;
    FanOutNode = this->OutletNodeNum;
    HCOutNode = this->ReheatAirOutletNode;
    HotControlNode = this->ReheatControlNode;
    AirMassFlow = AirFlow;
    state.dataLoopNodes->Node(FanInNode).MassFlowRate = AirMassFlow;
    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
    if (FanType == DataHVACGlobals::FanType_SimpleVAV && FanOn == 1) {
        Fans::SimulateFanComponents(state, this->FanName, FirstHVACIteration, this->Fan_Index);
    } else if (FanType == DataHVACGlobals::FanType_SystemModelObject && FanOn == 1) {
        state.dataHVACFan->fanObjs[this->Fan_Index]->simulate(state, _, _, _, _);

    } else { // pass through conditions
        state.dataHVACGlobal->TurnFansOff = true;
        if (FanType == DataHVACGlobals::FanType_SimpleVAV) {
            Fans::SimulateFanComponents(state, this->FanName, FirstHVACIteration, this->Fan_Index);
        } else if (FanType == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACFan->fanObjs[this->Fan_Index]->simulate(state, _, _, state.dataHVACGlobal->TurnFansOff, _);
        }
        state.dataHVACGlobal->TurnFansOff = TurnFansOffSav;
        state.dataLoopNodes->Node(FanOutNode).MassFlowRate = state.dataLoopNodes->Node(FanInNode).MassFlowRate;
        state.dataLoopNodes->Node(FanOutNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(FanInNode).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(FanOutNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(FanInNode).MassFlowRateMinAvail;
    }
    {
        auto const SELECT_CASE_var(this->ReheatComp_Num);
        if (SELECT_CASE_var == HeatingCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
            mdot = HWFlow;
            if (this->HWLoopNum > 0) {
                SetComponentFlowRate(state,
                                     mdot,
                                     this->ReheatControlNode,
                                     this->ReheatCoilOutletNode,
                                     this->HWLoopNum,
                                     this->HWLoopSide,
                                     this->HWBranchIndex,
                                     this->HWCompIndex);
            }

            SimulateWaterCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index);
        } else if (SELECT_CASE_var == HeatingCoilType::SteamAirHeating) { // HW Flow is steam mass flow here
            mdot = HWFlow;
            if (this->HWLoopNum > 0) {
                SetComponentFlowRate(state,
                                     mdot,
                                     this->ReheatControlNode,
                                     this->ReheatCoilOutletNode,
                                     this->HWLoopNum,
                                     this->HWLoopSide,
                                     this->HWBranchIndex,
                                     this->HWCompIndex);
            }
            SimulateSteamCoilComponents(state, this->ReheatName, FirstHVACIteration, this->ReheatComp_Index, HCoilReq);
        } else if (SELECT_CASE_var == HeatingCoilType::Electric) { // COIL:ELECTRIC:HEATING
            SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, HCoilReq, this->ReheatComp_Index);
        } else if (SELECT_CASE_var == HeatingCoilType::Gas) { // COIL:GAS:HEATING
            SimulateHeatingCoilComponents(state, this->ReheatName, FirstHVACIteration, HCoilReq, this->ReheatComp_Index);
        } else {
            ShowFatalError(state, "Invalid Reheat Component=" + this->ReheatComp);
        }
    }

    LoadMet = AirMassFlow * CpAirZn * (state.dataLoopNodes->Node(HCOutNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
}

Real64 SingleDuctAirTerminal::VAVVSCoolingResidual(EnergyPlusData &state,
                                                   Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
                                                   Array1D<Real64> const &Par      // Par(1) = REAL(SysNum)
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
    int FanType;       // fan type (as an integer)
    int FanOp;         // fan operation; 0=off, 1=on.
    Real64 UnitOutput; // cooling output [W] (cooling is negative)

    UnitIndex = int(Par(1));
    FirstHVACSoln = (Par(2) > 0.0);
    ZoneNodeIndex = int(Par(3));
    MinHWFlow = Par(5);
    FanType = int(Par(6));
    FanOp = int(Par(7));
    state.dataSingleDuct->sd_airterminal(UnitIndex).CalcVAVVS(
        state, FirstHVACSoln, ZoneNodeIndex, MinHWFlow, 0.0, FanType, SupplyAirMassFlow, FanOp, UnitOutput);

    Residuum = (Par(8) - UnitOutput) / Par(8);

    return Residuum;
}

Real64 SingleDuctAirTerminal::VAVVSHWNoFanResidual(EnergyPlusData &state,
                                                   Real64 const HWMassFlow,   // hot water mass flow rate [kg/s]
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
    Real64 AirMassFlow;     // supply air mass flow rate [kg/s]
    HeatingCoilType HCType; // heating coil type (integer)
    int FanType;            // fan type (as an integer)
    int FanOp;              // fan operation; 0=off, 1=on.
    Real64 UnitOutput;      // heating output [W]
    Real64 QSteamLoad;      // proportional load to calculate steam flow [W]
    Real64 MinSteamFlow;
    Real64 MaxSteamFlow;
    Real64 MaxSteamCoilCapacity;

    UnitIndex = int(Par(1));
    FirstHVACSoln = (Par(2) > 0.0);
    ZoneNodeIndex = int(Par(3));
    HCType = static_cast<HeatingCoilType>(int(Par(4)));
    AirMassFlow = Par(5);
    FanType = int(Par(6));
    FanOp = int(Par(7));
    QSteamLoad = 0.0;
    // vary the load to be met by the steam coil to converge on a steam flow rate to meet the load
    if (HCType == HeatingCoilType::SteamAirHeating) {
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
    state.dataSingleDuct->sd_airterminal(UnitIndex).CalcVAVVS(
        state, FirstHVACSoln, ZoneNodeIndex, HWMassFlow, QSteamLoad, FanType, AirMassFlow, FanOp, UnitOutput);

    Residuum = (Par(8) - UnitOutput) / Par(8);

    return Residuum;
}

Real64 SingleDuctAirTerminal::VAVVSHWFanOnResidual(EnergyPlusData &state,
                                                   Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
                                                   Array1D<Real64> const &Par      // Par(1) = REAL(SysNum)
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
    state.dataSingleDuct->sd_airterminal(UnitIndex).CalcVAVVS(
        state, FirstHVACSoln, ZoneNodeIndex, HWMassFlow, Par(8), FanType, SupplyAirMassFlow, FanOp, UnitOutput);

    Residuum = (Par(8) - UnitOutput) / Par(8);

    return Residuum;
}

Real64 SingleDuctAirTerminal::VAVVSHCFanOnResidual(EnergyPlusData &state,
                                                   Real64 const HeatingFrac,  // fraction of maximum heating output
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
    AirMassFlowRate = max(HeatingFrac * state.dataSingleDuct->sd_airterminal(UnitIndex).HeatAirMassFlowRateMax,
                          state.dataSingleDuct->sd_airterminal(UnitIndex).sd_airterminalInlet.AirMassFlowRateMaxAvail *
                              state.dataSingleDuct->sd_airterminal(UnitIndex).ZoneMinAirFrac);

    state.dataSingleDuct->sd_airterminal(UnitIndex).CalcVAVVS(
        state, FirstHVACSoln, ZoneNodeIndex, 0.0, HeatOut, FanType, AirMassFlowRate, FanOp, UnitOutput);

    Residuum = (Par(8) - UnitOutput) / Par(8);

    return Residuum;
}

// End Algorithm Section of the Module
// *****************************************************************************

// Beginning of Update subroutines for the Sys Module
// *****************************************************************************

void SingleDuctAirTerminal::UpdateSys(EnergyPlusData &state) const
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   january 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine updates the Syss.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int OutletNode;
    int InletNode;

    OutletNode = this->OutletNodeNum;
    InletNode = this->InletNodeNum;

    if (this->SysType_Num == SysType::SingleDuctVAVReheat || this->SysType_Num == SysType::SingleDuctCBVAVReheat ||
        this->SysType_Num == SysType::SingleDuctCBVAVNoReheat || this->SysType_Num == SysType::SingleDuctVAVNoReheat ||
        this->SysType_Num == SysType::SingleDuctConstVolNoReheat) {
        // Set the outlet air nodes of the Sys
        state.dataLoopNodes->Node(OutletNode).MassFlowRate = this->sd_airterminalOutlet.AirMassFlowRate;
        state.dataLoopNodes->Node(OutletNode).Temp = this->sd_airterminalOutlet.AirTemp;
        state.dataLoopNodes->Node(OutletNode).HumRat = this->sd_airterminalOutlet.AirHumRat;
        state.dataLoopNodes->Node(OutletNode).Enthalpy = this->sd_airterminalOutlet.AirEnthalpy;
        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
        state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;
    }

    // After all of the Outlets are updated the mass flow information needs to be
    // passed back to the system inlet.
    state.dataLoopNodes->Node(InletNode).MassFlowRate = this->sd_airterminalOutlet.AirMassFlowRate;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail =
        min(this->sd_airterminalOutlet.AirMassFlowRateMaxAvail, state.dataLoopNodes->Node(OutletNode).MassFlowRateMax);
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = this->sd_airterminalOutlet.AirMassFlowRateMinAvail;

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        state.dataLoopNodes->Node(OutletNode).CO2 = state.dataLoopNodes->Node(InletNode).CO2;
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        state.dataLoopNodes->Node(OutletNode).GenContam = state.dataLoopNodes->Node(InletNode).GenContam;
    }
}

//        End of Update subroutines for the Sys Module
// *****************************************************************************

// Beginning of Reporting subroutines for the Sys Module
// *****************************************************************************

void SingleDuctAirTerminal::ReportSys(EnergyPlusData &state) // unused1208
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
    this->CalcOutdoorAirVolumeFlowRate(state);
}

void GetHVACSingleDuctSysIndex(EnergyPlusData &state,
                               std::string const &SDSName,
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

    if (state.dataSingleDuct->GetInputFlag) { // First time subroutine has been entered
        GetSysInput(state);
        state.dataSingleDuct->GetInputFlag = false;
    }

    SDSIndex = UtilityRoutines::FindItemInList(SDSName, state.dataSingleDuct->sd_airterminal, &SingleDuctAirTerminal::SysName);
    if (SDSIndex == 0) {
        if (present(ThisObjectType)) {
            ShowSevereError(state, ThisObjectType() + ", GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName);
        } else {
            ShowSevereError(state, "GetHVACSingleDuctSysIndex: Single duct system not found=" + SDSName);
        }
        ErrorsFound = true;
    } else {
        if ((state.dataSingleDuct->sd_airterminal(SDSIndex).SysType_Num != SysType::SingleDuctConstVolReheat) &&
            (state.dataSingleDuct->sd_airterminal(SDSIndex).SysType_Num != SysType::SingleDuctVAVReheat)) {
            ShowSevereError(state, ThisObjectType() + ", GetHVACSingleDuctSysIndex: Could not find allowed types=" + SDSName);
            ShowContinueError(state, "The allowed types are: AirTerminal:SingleDuct:ConstantVolume:Reheat and AirTerminal:SingleDuct:VAV:Reheat");
            ErrorsFound = true;
        }
        if (state.dataSingleDuct->sd_airterminal(SDSIndex).SysType_Num == SysType::SingleDuctVAVReheat) {
            if (present(DamperInletNode)) DamperInletNode = state.dataSingleDuct->sd_airterminal(SDSIndex).InletNodeNum;
            if (present(DamperOutletNode)) DamperOutletNode = state.dataSingleDuct->sd_airterminal(SDSIndex).OutletNodeNum;
        }
    }
}

void SimATMixer(EnergyPlusData &state, std::string const &SysName, bool const FirstHVACIteration, int &SysIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR
    //       DATE WRITTEN   March 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE
    // Simulate an Air Terminal Mixer component

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    if (state.dataSingleDuct->GetATMixerFlag) {
        state.dataSingleDuct->GetATMixerFlag = false;
    }

    if (SysIndex == 0) {
        state.dataSingleDuct->SysNumSATM = UtilityRoutines::FindItemInList(SysName, state.dataSingleDuct->SysATMixer);
        SysIndex = state.dataSingleDuct->SysNumSATM;
        if (state.dataSingleDuct->SysNumSATM == 0) {
            ShowFatalError(state, "Object " + SysName + " not found");
        }
    } else {
        state.dataSingleDuct->SysNumSATM = SysIndex;
    }

    state.dataSingleDuct->SysATMixer(state.dataSingleDuct->SysNumSATM).InitATMixer(state, FirstHVACIteration);

    CalcATMixer(state, state.dataSingleDuct->SysNumSATM);

    UpdateATMixer(state, state.dataSingleDuct->SysNumSATM);
}

void GetATMixers(EnergyPlusData &state)
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
    using NodeInputManager::GetOnlySingleNode;
    using namespace DataLoopNode;
    using BranchNodeConnections::SetUpCompSets;
    using BranchNodeConnections::TestCompSet;
    using DataHVACGlobals::ATMixer_InletSide;
    using DataHVACGlobals::ATMixer_SupplySide;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumNums;    // Number of REAL(r64) numbers returned by GetObjectItem
    int NumAlphas;  // Number of alphanumerics returned by GetObjectItem
    int ATMixerNum; // Index of inlet side mixer air terminal unit
    int IOStat;
    static constexpr std::string_view RoutineName("GetATMixers: "); // include trailing blank space
    bool ErrorsFound(false);                                        // Error flag
    int NodeNum;                                                    // Index to node number
    int CtrlZone;                                                   // Index to control zone
    bool ZoneNodeNotFound;                                          // Flag for error checking
    bool errFlag;                                                   // error flag from component validation

    if (!state.dataSingleDuct->GetATMixerFlag) {
        return;
    }
    state.dataSingleDuct->GetATMixerFlag = false;
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "AirTerminal:SingleDuct:Mixer";
    state.dataSingleDuct->NumATMixers = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataSingleDuct->SysATMixer.allocate(state.dataSingleDuct->NumATMixers);

    // make sure the input data is read in only once
    if (state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag) {
        // Need air distribution units first
        ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);
        state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag = false;
    }

    for (ATMixerNum = 1; ATMixerNum <= state.dataSingleDuct->NumATMixers; ++ATMixerNum) {
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 ATMixerNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
        state.dataSingleDuct->SysATMixer(ATMixerNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        if (state.dataIPShortCut->cAlphaArgs(7) == "INLETSIDE") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).MixerType = ATMixer_InletSide; // inlet side mixer
        } else if (state.dataIPShortCut->cAlphaArgs(7) == "SUPPLYSIDE") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).MixerType = ATMixer_SupplySide; // supply side mixer
        }
        if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:WATERTOAIRHEATPUMP") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 1;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:FOURPIPEFANCOIL") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 2;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 3;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:PACKAGEDTERMINALHEATPUMP") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 4;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:VARIABLEREFRIGERANTFLOW") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 5;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "AIRLOOPHVAC:UNITARYSYSTEM") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 6;
        } else if (state.dataIPShortCut->cAlphaArgs(2) == "ZONEHVAC:UNITVENTILATOR") {
            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitType = 7;
        }

        state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitName = state.dataIPShortCut->cAlphaArgs(3);

        ValidateComponent(
            state, state.dataIPShortCut->cAlphaArgs(2), state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneHVACUnitName, errFlag, cCurrentModuleObject);

        state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode = GetOnlySingleNode(state,
                                                                                         state.dataIPShortCut->cAlphaArgs(4),
                                                                                         ErrorsFound,
                                                                                         cCurrentModuleObject,
                                                                                         state.dataIPShortCut->cAlphaArgs(1),
                                                                                         DataLoopNode::NodeFluidType::Air,
                                                                                         DataLoopNode::NodeConnectionType::Outlet,
                                                                                         NodeInputManager::compFluidStream::Primary,
                                                                                         ObjectIsNotParent,
                                                                                         state.dataIPShortCut->cAlphaFieldNames(4));

        state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode = GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(5),
                                                                                   ErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Primary,
                                                                                   ObjectIsNotParent,
                                                                                   state.dataIPShortCut->cAlphaFieldNames(5));
        state.dataSingleDuct->SysATMixer(ATMixerNum).SecInNode = GetOnlySingleNode(state,
                                                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                                                   ErrorsFound,
                                                                                   cCurrentModuleObject,
                                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                                   DataLoopNode::NodeFluidType::Air,
                                                                                   DataLoopNode::NodeConnectionType::Inlet,
                                                                                   NodeInputManager::compFluidStream::Primary,
                                                                                   ObjectIsNotParent,
                                                                                   state.dataIPShortCut->cAlphaFieldNames(6));

        if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser = true;
        } else {
            state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(8), state.dataSize->OARequirements);
            if (state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                ShowContinueError(state,
                                  "..invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) + "\".");
                ErrorsFound = true;
            } else {
                state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
            }
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
            state.dataSingleDuct->SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
        } else {
            if (state.dataIPShortCut->cAlphaArgs(9) == "CURRENTOCCUPANCY") {
                state.dataSingleDuct->SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
            } else if (state.dataIPShortCut->cAlphaArgs(9) == "DESIGNOCCUPANCY") {
                state.dataSingleDuct->SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonByDesignLevel;
            } else {
                state.dataSingleDuct->SysATMixer(ATMixerNum).OAPerPersonMode = DataZoneEquipment::PerPersonDCVByCurrentLevel;
                ShowWarningError(state,
                                 std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid data.");
                ShowContinueError(state,
                                  "..invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" + state.dataIPShortCut->cAlphaArgs(9) +
                                      "\". The default input of CurrentOccupancy is assigned");
            }
        }

        // Check for dupes in the three nodes.
        if (state.dataSingleDuct->SysATMixer(ATMixerNum).SecInNode == state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = " + state.dataSingleDuct->SysATMixer(ATMixerNum).Name + ' ' +
                                state.dataIPShortCut->cAlphaArgs(5) + " = " +
                                state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode) + " duplicates the " +
                                state.dataIPShortCut->cAlphaArgs(4) + '.');
            ErrorsFound = true;
        } else if (state.dataSingleDuct->SysATMixer(ATMixerNum).SecInNode == state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = " + state.dataSingleDuct->SysATMixer(ATMixerNum).Name + ' ' +
                                state.dataIPShortCut->cAlphaArgs(6) + " = " +
                                state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode) + " duplicates the " +
                                state.dataIPShortCut->cAlphaArgs(4) + '.');
            ErrorsFound = true;
        }

        if (state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode == state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode) {
            ShowSevereError(state,
                            cCurrentModuleObject + " = " + state.dataSingleDuct->SysATMixer(ATMixerNum).Name + ' ' +
                                state.dataIPShortCut->cAlphaArgs(6) + " = " +
                                state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode) + " duplicates the " +
                                state.dataIPShortCut->cAlphaArgs(5) + '.');
            ErrorsFound = true;
        }

        for (int ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode;
                state.dataSingleDuct->SysATMixer(ATMixerNum).ADUNum = ADUNum;
                break;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataSingleDuct->SysATMixer(ATMixerNum).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for System = [" + cCurrentModuleObject + ',' +
                                state.dataSingleDuct->SysATMixer(ATMixerNum).Name + "].");
            ShowContinueError(
                state, "...should have outlet node = " + state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode));
            ErrorsFound = true;
        } else {

            if (state.dataSingleDuct->SysATMixer(ATMixerNum).MixerType == ATMixer_InletSide) {
                // Air Terminal inlet node must be the same as a zone exhaust node
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumExhaustNodes; ++NodeNum) {
                        if (state.dataSingleDuct->SysATMixer(ATMixerNum).SecInNode ==
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ExhaustNode(NodeNum)) {
                            ZoneNodeNotFound = false;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->SysATMixer(ATMixerNum).ADUNum).ZoneEqNum = CtrlZone;
                            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneEqNum = CtrlZone;
                            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            // Must wait until InitATMixer to fill other zone equip config data because ultimate zone inlet node is not known yet
                            // for inlet side mixers
                            if (!state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser) {
                                bool UseOccSchFlag = false;
                                bool UseMinOASchFlag = false;
                                state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate =
                                    DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                                  state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr,
                                                                                  state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum,
                                                                                  UseOccSchFlag,
                                                                                  UseMinOASchFlag);
                            }
                            goto ControlledZoneLoop_exit;
                        }
                    }
                }
            ControlledZoneLoop_exit:;
                if (ZoneNodeNotFound) {
                    bool ZoneNodeFoundAgain = false;
                    for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                        for (int Num = 1; Num <= state.dataZoneEquip->ZoneEquipList(CtrlZone).NumOfEquipTypes; ++Num) {
                            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(3),
                                                            state.dataZoneEquip->ZoneEquipList(CtrlZone).EquipName(Num)) &&
                                UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(2),
                                                            state.dataZoneEquip->ZoneEquipList(CtrlZone).EquipType(Num))) {
                                state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->SysATMixer(ATMixerNum).ADUNum).ZoneEqNum = CtrlZone;
                                state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneEqNum = CtrlZone;
                                state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                                // Must wait until InitATMixer to fill other zone equip config data because ultimate zone inlet node is not known yet
                                // for inlet side mixers
                                if (!state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser) {
                                    bool UseOccSchFlag = false;
                                    bool UseMinOASchFlag = false;
                                    state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate =
                                        DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                                      state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr,
                                                                                      state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum,
                                                                                      UseOccSchFlag,
                                                                                      UseMinOASchFlag);
                                }
                                ZoneNodeFoundAgain = true;
                                break;
                            }
                        }
                        if (ZoneNodeFoundAgain) break;
                    }
                    if (!ZoneNodeFoundAgain) {
                        ShowSevereError(state,
                                        cCurrentModuleObject + " = \"" + state.dataSingleDuct->SysATMixer(ATMixerNum).Name +
                                            "\". Inlet Side Air Terminal Mixer air inlet node name must be the same as either a zone exhaust node "
                                            "name or an induced "
                                            "air node in ZonePlenum.");
                        ShowContinueError(state, "..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.");
                        ShowContinueError(state, "..Induced Air Outlet Node name is specified in AirLoopHVAC:ReturnPlenum object.");
                        ShowContinueError(state,
                                          "..Inlet Side CONNECTED Air Terminal Mixer inlet node name = " +
                                              state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).SecInNode));
                        ErrorsFound = true;
                    }
                }
            }

            if (state.dataSingleDuct->SysATMixer(ATMixerNum).MixerType == ATMixer_SupplySide) {
                ZoneNodeNotFound = true;
                for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                    if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                    for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                        if (state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode ==
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                            ZoneNodeNotFound = false;
                            state.dataDefineEquipment->AirDistUnit(state.dataSingleDuct->SysATMixer(ATMixerNum).ADUNum).ZoneEqNum = CtrlZone;
                            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneEqNum = CtrlZone;
                            state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrlZone).ActualZoneNum;
                            // Wait until InitATMixer to fill other zone equip config data

                            if (!state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser) {
                                bool UseOccSchFlag = false;
                                bool UseMinOASchFlag = false;
                                state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate =
                                    DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                                  state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr,
                                                                                  state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum,
                                                                                  UseOccSchFlag,
                                                                                  UseMinOASchFlag);
                            }
                            goto ControlZoneLoop_exit;
                        }
                    }
                }
            ControlZoneLoop_exit:;
                if (ZoneNodeNotFound) {
                    ShowSevereError(state,
                                    cCurrentModuleObject + " = \"" + state.dataSingleDuct->SysATMixer(ATMixerNum).Name +
                                        "\". Supply Side Air Terminal Mixer air outlet node name must be the same as a zone inlet node name.");
                    ShowContinueError(state, "..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.");
                    ShowContinueError(state,
                                      "..Supply Side connected Air Terminal Mixer outlet node name = " +
                                          state.dataLoopNodes->NodeID(state.dataSingleDuct->SysATMixer(ATMixerNum).MixedAirOutNode));
                    ErrorsFound = true;
                }
            }
        }
        TestCompSet(state,
                    cCurrentModuleObject,
                    state.dataSingleDuct->SysATMixer(ATMixerNum).Name,
                    state.dataIPShortCut->cAlphaArgs(5),
                    state.dataIPShortCut->cAlphaArgs(4),
                    "Air Nodes");

        if (state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr == 0) {
            if (state.dataSize->ZoneSizingInput.allocated()) {
                for (int SizingInputNum = 1; SizingInputNum <= state.dataSize->NumZoneSizingInput; ++SizingInputNum) {
                    if (state.dataSize->ZoneSizingInput(SizingInputNum).ZoneNum == state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum) {
                        if (state.dataSize->ZoneSizingInput(SizingInputNum).ZoneDesignSpecOAIndex == 0) {
                            ShowWarningError(state,
                                             std::string{RoutineName} + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                                 "\", invalid data.");
                            ShowContinueError(state,
                                              state.dataIPShortCut->cAlphaFieldNames(8) +
                                                  " is blank in both the mixer and the Sizing:Zone object for the same zone.");
                            ShowContinueError(state, "The mixer outdoor airflow rate is set to zero.");
                            state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = 0.0;
                        } else {
                            state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr =
                                state.dataSize->ZoneSizingInput(SizingInputNum).ZoneDesignSpecOAIndex;
                            state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate =
                                DataSizing::calcDesignSpecificationOutdoorAir(state,
                                                                              state.dataSingleDuct->SysATMixer(ATMixerNum).OARequirementsPtr,
                                                                              state.dataSingleDuct->SysATMixer(ATMixerNum).ZoneNum,
                                                                              false,
                                                                              false);
                            state.dataSingleDuct->SysATMixer(ATMixerNum).NoOAFlowInputFromUser = false;
                        }
                    }
                }
            } else {
                ShowWarningError(state,
                                 state.dataIPShortCut->cAlphaFieldNames(8) +
                                     "is blank and there is no Sizing:Zone for the same zone. The mixer outdoor airflow rate is set to zero.");
                state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate = 0.0;
            }
        }
        state.dataSingleDuct->SysATMixer(ATMixerNum).MassFlowRateMaxAvail =
            state.dataSingleDuct->SysATMixer(ATMixerNum).DesignPrimaryAirVolRate * state.dataEnvrn->StdRhoAir;
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in input.  Program terminates.");
    }
}

void AirTerminalMixerData::InitATMixer(EnergyPlusData &state, bool const FirstHVACIteration)
{
    // Purpose: Initialize the AirTerminalMixers data structure with node data
    if (this->OneTimeInitFlag) {
        {
            auto &thisADU(state.dataDefineEquipment->AirDistUnit(this->ADUNum));
            {
                auto &thisZoneEqConfig(state.dataZoneEquip->ZoneEquipConfig(thisADU.ZoneEqNum));
                for (int SupAirIn = 1; SupAirIn <= thisZoneEqConfig.NumInletNodes; ++SupAirIn) {
                    if (this->ZoneInletNode == thisZoneEqConfig.InletNode(SupAirIn)) {
                        thisZoneEqConfig.AirDistUnitCool(SupAirIn).InNode = this->PriInNode;
                        thisZoneEqConfig.AirDistUnitCool(SupAirIn).OutNode = this->MixedAirOutNode;
                        thisZoneEqConfig.AirDistUnitHeat(SupAirIn).InNode = this->PriInNode;
                        thisZoneEqConfig.AirDistUnitHeat(SupAirIn).OutNode = this->MixedAirOutNode;
                        thisADU.TermUnitSizingNum = thisZoneEqConfig.AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                        this->CtrlZoneInNodeIndex = SupAirIn;
                        {
                            auto &thisTermUnitSizingData(state.dataSize->TermUnitSizing(thisADU.TermUnitSizingNum));
                            thisTermUnitSizingData.ADUName = thisADU.Name;
                            // Fill TermUnitSizing with specs from DesignSpecification:AirTerminal:Sizing if there is one attached to this
                            // terminal unit
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
                }
            }
        }
        this->OneTimeInitFlag = false;
    }

    // Keep trying until we find it, the airloopnum, that is
    if (this->OneTimeInitFlag2) {
        this->AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(state.dataDefineEquipment->AirDistUnit(this->ADUNum).ZoneEqNum)
                               .InletNodeAirLoopNum(this->CtrlZoneInNodeIndex);
        state.dataDefineEquipment->AirDistUnit(this->ADUNum).AirLoopNum = this->AirLoopNum;
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
            airLoopOAFrac = state.dataAirLoop->AirLoopFlow(this->AirLoopNum).OAFrac;
            if (airLoopOAFrac > 0.0) {
                vDotOAReq = DataSizing::calcDesignSpecificationOutdoorAir(state, this->OARequirementsPtr, this->ZoneNum, UseOccSchFlag, true);
                mDotFromOARequirement = vDotOAReq * state.dataEnvrn->StdRhoAir / airLoopOAFrac;
            } else {
                mDotFromOARequirement = state.dataLoopNodes->Node(this->PriInNode).MassFlowRate;
            }
        }
        if (FirstHVACIteration) {
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRate = mDotFromOARequirement;
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRateMaxAvail = this->MassFlowRateMaxAvail;
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRateMinAvail = 0.0;
        } else {
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRate = mDotFromOARequirement;

            state.dataLoopNodes->Node(this->PriInNode).MassFlowRate =
                min(state.dataLoopNodes->Node(this->PriInNode).MassFlowRate, state.dataLoopNodes->Node(this->PriInNode).MassFlowRateMaxAvail);
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRate =
                max(state.dataLoopNodes->Node(this->PriInNode).MassFlowRate, state.dataLoopNodes->Node(this->PriInNode).MassFlowRateMinAvail);
            state.dataLoopNodes->Node(this->PriInNode).MassFlowRate =
                max(state.dataLoopNodes->Node(this->PriInNode).MassFlowRate, state.dataLoopNodes->Node(this->PriInNode).MassFlowRateMin);
        }
    }
    if (this->MixerType == ATMixer_InletSide) {
        state.dataLoopNodes->Node(this->PriInNode).MassFlowRate =
            min(state.dataLoopNodes->Node(this->PriInNode).MassFlowRate, state.dataLoopNodes->Node(this->MixedAirOutNode).MassFlowRate);
    }
}

void CalcATMixer(EnergyPlusData &state, int const SysNum)
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
    using Psychrometrics::PsyTdbFnHW;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    state.dataSingleDuct->PriEnthalpyCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).PriInNode).Enthalpy;
    state.dataSingleDuct->PriHumRatCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).PriInNode).HumRat;
    state.dataSingleDuct->PriTempCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).PriInNode).Temp;
    state.dataSingleDuct->PriMassFlowRateCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).PriInNode).MassFlowRate;

    state.dataSingleDuct->SecAirMassFlowRateCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).SecInNode).MassFlowRate;
    state.dataSingleDuct->SecAirEnthalpyCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).SecInNode).Enthalpy;
    state.dataSingleDuct->SecAirHumRatCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).SecInNode).HumRat;
    state.dataSingleDuct->SecAirTempCATM = state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).SecInNode).Temp;

    if (state.dataSingleDuct->SysATMixer(SysNum).MixerType == ATMixer_SupplySide) {
        state.dataSingleDuct->MixedAirMassFlowRateCATM = state.dataSingleDuct->SecAirMassFlowRateCATM + state.dataSingleDuct->PriMassFlowRateCATM;
    } else {
        // for inlet side mixer, the mixed air flow has been set, but we don't know the secondary flow
        state.dataSingleDuct->MixedAirMassFlowRateCATM =
            state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).MixedAirOutNode).MassFlowRate;
        state.dataSingleDuct->SecAirMassFlowRateCATM =
            max(state.dataSingleDuct->MixedAirMassFlowRateCATM - state.dataSingleDuct->PriMassFlowRateCATM, 0.0);
        state.dataLoopNodes->Node(state.dataSingleDuct->SysATMixer(SysNum).SecInNode).MassFlowRate = state.dataSingleDuct->SecAirMassFlowRateCATM;
        if (std::abs(state.dataSingleDuct->PriMassFlowRateCATM + state.dataSingleDuct->SecAirMassFlowRateCATM -
                     state.dataSingleDuct->MixedAirMassFlowRateCATM) > SmallMassFlow) {
            ShowSevereError(state,
                            "CalcATMixer: Invalid mass flow rates in AirTerminal:SingleDuct:Mixer=" + state.dataSingleDuct->SysATMixer(SysNum).Name);
            ShowContinueErrorTimeStamp(state,
                                       format("Primary mass flow rate={:.6R}Secondary mass flow rate={:.6R}Mixed mass flow rate={:.6R}",
                                              state.dataSingleDuct->PriMassFlowRateCATM,
                                              state.dataSingleDuct->SecAirMassFlowRateCATM,
                                              state.dataSingleDuct->MixedAirMassFlowRateCATM));
            ShowFatalError(state, "Simulation terminates.");
        }
    }
    // now calculate the mixed (outlet) conditions
    if (state.dataSingleDuct->MixedAirMassFlowRateCATM > 0.0) {
        state.dataSingleDuct->MixedAirEnthalpyCATM = (state.dataSingleDuct->SecAirMassFlowRateCATM * state.dataSingleDuct->SecAirEnthalpyCATM +
                                                      state.dataSingleDuct->PriMassFlowRateCATM * state.dataSingleDuct->PriEnthalpyCATM) /
                                                     state.dataSingleDuct->MixedAirMassFlowRateCATM;
        state.dataSingleDuct->MixedAirHumRatCATM = (state.dataSingleDuct->SecAirMassFlowRateCATM * state.dataSingleDuct->SecAirHumRatCATM +
                                                    state.dataSingleDuct->PriMassFlowRateCATM * state.dataSingleDuct->PriHumRatCATM) /
                                                   state.dataSingleDuct->MixedAirMassFlowRateCATM;
        // Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
        state.dataSingleDuct->MixedAirTempCATM = PsyTdbFnHW(state.dataSingleDuct->MixedAirEnthalpyCATM, state.dataSingleDuct->MixedAirHumRatCATM);
    }

    state.dataSingleDuct->SysATMixer(SysNum).MixedAirMassFlowRate = state.dataSingleDuct->MixedAirMassFlowRateCATM;
    state.dataSingleDuct->SysATMixer(SysNum).MixedAirEnthalpy = state.dataSingleDuct->MixedAirEnthalpyCATM;
    state.dataSingleDuct->SysATMixer(SysNum).MixedAirHumRat = state.dataSingleDuct->MixedAirHumRatCATM;
    state.dataSingleDuct->SysATMixer(SysNum).MixedAirTemp = state.dataSingleDuct->MixedAirTempCATM;
}

void UpdateATMixer(EnergyPlusData &state, int const SysNum)
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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PriInNode = state.dataSingleDuct->SysATMixer(SysNum).PriInNode;
    int SecInNode = state.dataSingleDuct->SysATMixer(SysNum).SecInNode;
    int MixedAirOutNode = state.dataSingleDuct->SysATMixer(SysNum).MixedAirOutNode;

    // mixed air data
    state.dataLoopNodes->Node(MixedAirOutNode).Temp = state.dataSingleDuct->SysATMixer(SysNum).MixedAirTemp;
    state.dataLoopNodes->Node(MixedAirOutNode).HumRat = state.dataSingleDuct->SysATMixer(SysNum).MixedAirHumRat;
    state.dataLoopNodes->Node(MixedAirOutNode).Enthalpy = state.dataSingleDuct->SysATMixer(SysNum).MixedAirEnthalpy;
    state.dataLoopNodes->Node(MixedAirOutNode).Press = state.dataSingleDuct->SysATMixer(SysNum).MixedAirPressure;
    state.dataLoopNodes->Node(MixedAirOutNode).MassFlowRate = state.dataSingleDuct->SysATMixer(SysNum).MixedAirMassFlowRate;

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        if (state.dataSingleDuct->SysATMixer(SysNum).MixedAirMassFlowRate <= DataHVACGlobals::VerySmallMassFlow) {
            state.dataLoopNodes->Node(MixedAirOutNode).CO2 = state.dataLoopNodes->Node(PriInNode).CO2;
        } else {
            state.dataLoopNodes->Node(MixedAirOutNode).CO2 =
                (state.dataLoopNodes->Node(SecInNode).MassFlowRate * state.dataLoopNodes->Node(SecInNode).CO2 +
                 state.dataLoopNodes->Node(PriInNode).MassFlowRate * state.dataLoopNodes->Node(PriInNode).CO2) /
                state.dataLoopNodes->Node(MixedAirOutNode).MassFlowRate;
        }
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        if (state.dataSingleDuct->SysATMixer(SysNum).MixedAirMassFlowRate <= DataHVACGlobals::VerySmallMassFlow) {
            state.dataLoopNodes->Node(MixedAirOutNode).GenContam = state.dataLoopNodes->Node(PriInNode).GenContam;
        } else {
            state.dataLoopNodes->Node(MixedAirOutNode).GenContam =
                (state.dataLoopNodes->Node(SecInNode).MassFlowRate * state.dataLoopNodes->Node(SecInNode).GenContam +
                 state.dataLoopNodes->Node(PriInNode).MassFlowRate * state.dataLoopNodes->Node(PriInNode).GenContam) /
                state.dataLoopNodes->Node(MixedAirOutNode).MassFlowRate;
        }
    }

    // update ADU flow data - because SimATMixer is called from the various zone equipment so the updates in SimZoneAirLoopEquipment won't work
    int aduNum = state.dataSingleDuct->SysATMixer(SysNum).ADUNum;
    state.dataDefineEquipment->AirDistUnit(aduNum).MassFlowRateTU = state.dataLoopNodes->Node(PriInNode).MassFlowRate;
    state.dataDefineEquipment->AirDistUnit(aduNum).MassFlowRateZSup = state.dataLoopNodes->Node(PriInNode).MassFlowRate;
    state.dataDefineEquipment->AirDistUnit(aduNum).MassFlowRateSup = state.dataLoopNodes->Node(PriInNode).MassFlowRate;
}

void GetATMixer(EnergyPlusData &state,
                std::string const &ZoneEquipName, // zone unit name name
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

    if (state.dataSingleDuct->GetATMixerFlag) {
        GetATMixers(state);
        state.dataSingleDuct->GetATMixerFlag = false;
    }

    if (state.dataSingleDuct->NumATMixers <= 0) {
        ATMixerNum = 0;
        ATMixerName = "";
        ATMixerPriNode = 0;
        ATMixerSecNode = 0;
        ATMixerOutNode = 0;
        ATMixerType = 0;
        return;
    }

    ATMixerIndex = UtilityRoutines::FindItemInList(ZoneEquipName, state.dataSingleDuct->SysATMixer, &AirTerminalMixerData::ZoneHVACUnitName);
    if (ATMixerIndex > 0) {
        ATMixerNum = ATMixerIndex;
        ATMixerName = state.dataSingleDuct->SysATMixer(ATMixerIndex).Name;
        ATMixerPriNode = state.dataSingleDuct->SysATMixer(ATMixerIndex).PriInNode;
        ATMixerSecNode = state.dataSingleDuct->SysATMixer(ATMixerIndex).SecInNode;
        ATMixerOutNode = state.dataSingleDuct->SysATMixer(ATMixerIndex).MixedAirOutNode;
        ATMixerType = state.dataSingleDuct->SysATMixer(ATMixerIndex).MixerType;
        if (ATMixerType == ATMixer_InletSide) {
            state.dataSingleDuct->SysATMixer(ATMixerIndex).ZoneInletNode = ZoneEquipOutletNode;
        } else {
            state.dataSingleDuct->SysATMixer(ATMixerIndex).ZoneInletNode = ATMixerOutNode;
        }
        state.dataSingleDuct->SysATMixer(ATMixerNum).InitATMixer(state, false);
    } else {
        ATMixerNum = 0;
        ATMixerName = "";
        ATMixerPriNode = 0;
        ATMixerSecNode = 0;
        ATMixerOutNode = 0;
        ATMixerType = 0;
    }
}

void SetATMixerPriFlow(EnergyPlusData &state,
                       int const ATMixerNum,                     // Air terminal mixer index
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
    PriAirNode = state.dataSingleDuct->SysATMixer(ATMixerNum).PriInNode;
    if (present(PriAirMassFlowRate)) {
        state.dataLoopNodes->Node(PriAirNode).MassFlowRate = PriAirMassFlowRate;
    } else {
        state.dataLoopNodes->Node(PriAirNode).MassFlowRate = state.dataLoopNodes->Node(PriAirNode).MassFlowRateMaxAvail;
    }
}

void setATMixerSizingProperties(EnergyPlusData &state,
                                int const &inletATMixerIndex, // index to ATMixer at inlet of zone equipment
                                int const &controlledZoneNum, // controlled zone number
                                int const &curZoneEqNum       // current zone equipment being simulated
)
{
    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &FinalZoneSizing(state.dataSize->FinalZoneSizing);

    if (inletATMixerIndex == 0) return; // protect this function from bad inputs
    if (controlledZoneNum == 0) return;
    if (curZoneEqNum == 0) return;
    if (state.dataSingleDuct->SysATMixer(inletATMixerIndex).MixerType == DataHVACGlobals::No_ATMixer) return;

    // ATMixer properties only affect coil sizing when the mixer is on the inlet side of zone equipment
    if (state.dataSingleDuct->SysATMixer(inletATMixerIndex).MixerType == DataHVACGlobals::ATMixer_SupplySide) {
        // check if user has selected No to account for DOAS system
        if (FinalZoneSizing.allocated() && state.dataSingleDuct->SysATMixer(inletATMixerIndex).printWarning) {
            if (!FinalZoneSizing(curZoneEqNum).AccountForDOAS && FinalZoneSizing(curZoneEqNum).DOASControlStrategy != DOANeutralSup) {
                ShowWarningError(state, "AirTerminal:SingleDuct:Mixer: " + state.dataSingleDuct->SysATMixer(inletATMixerIndex).Name);
                ShowContinueError(
                    state,
                    " Supply side Air Terminal Mixer does not adjust zone equipment coil sizing and may result in inappropriately sized coils.");
                ShowContinueError(state,
                                  " Set Account for Dedicated Outdoor Air System = Yes in Sizing:Zone object for zone = " +
                                      FinalZoneSizing(curZoneEqNum).ZoneName);
            }
            state.dataSingleDuct->SysATMixer(inletATMixerIndex).printWarning = false;
        }
        return; // do nothing else if this is a supply side ATMixer
    }
    // check if user has selected Yes to account for DOAS system
    if (FinalZoneSizing.allocated() && state.dataSingleDuct->SysATMixer(inletATMixerIndex).printWarning) {
        if (FinalZoneSizing(curZoneEqNum).AccountForDOAS && FinalZoneSizing(curZoneEqNum).DOASControlStrategy != DOANeutralSup) {
            ShowWarningError(state, "AirTerminal:SingleDuct:Mixer: " + state.dataSingleDuct->SysATMixer(inletATMixerIndex).Name);
            ShowContinueError(state, " Inlet side Air Terminal Mixer automatically adjusts zone equipment coil sizing.");
            ShowContinueError(state,
                              " Set Account for Dedicated Outdoor Air System = No in Sizing:Zone object for zone = " +
                                  FinalZoneSizing(curZoneEqNum).ZoneName);
            state.dataSingleDuct->SysATMixer(inletATMixerIndex).printWarning = false;
        }
    }

    // proceed to set ATMixer properties used for sizing coils

    int airLoopIndex = // find air loop associated with ATMixer
        state.dataZoneEquip->ZoneEquipConfig(controlledZoneNum)
            .InletNodeAirLoopNum(state.dataSingleDuct->SysATMixer(inletATMixerIndex).CtrlZoneInNodeIndex);

    // must be a system sizing run or calculations are not possible
    bool SizingDesRunThisAirSys = false;                                      // Sizing:System object found flag
    CheckThisAirSystemForSizing(state, airLoopIndex, SizingDesRunThisAirSys); // check for Sizing:System object

    if (SizingDesRunThisAirSys) {

        // set ATMixer outlet air flow rate in ZoneEqSizing array for ATMixer. If this value > 0, then the Sizer will know an ATMixer exists
        ZoneEqSizing(curZoneEqNum).ATMixerVolFlow = state.dataSingleDuct->SysATMixer(inletATMixerIndex).DesignPrimaryAirVolRate;

        // If air loop has heating coil use SA conditions, else if OA sys has coils then use precool conditions, else use OA conditions
        if (state.dataAirSystemsData->PrimaryAirSystems(airLoopIndex).CentralHeatCoilExists) {
            // if central heating coil exists, ATMixer outlet is assumed to be at supply air conditions described in sizing input
            ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = FinalSysSizing(airLoopIndex).HeatSupTemp;
            ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = FinalSysSizing(airLoopIndex).HeatSupHumRat;
        } else if (state.dataAirSystemsData->PrimaryAirSystems(airLoopIndex).NumOAHeatCoils > 0) {
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
                Real64 CoilInEnthalpyForSizing =
                    OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).PreheatTemp, FinalSysSizing(airLoopIndex).PreheatHumRat) +
                    (1 - OutAirFrac) *
                        Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatRetTemp, FinalSysSizing(airLoopIndex).HeatRetHumRat);

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
                Real64 CoilInEnthalpyForSizing =
                    OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatOutTemp, FinalSysSizing(airLoopIndex).HeatOutHumRat) +
                    (1 - OutAirFrac) *
                        Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).HeatRetTemp, FinalSysSizing(airLoopIndex).HeatRetHumRat);

                // back calculate temperature based on humrat and enthalpy state points
                Real64 CoilInTempForSizing = Psychrometrics::PsyTdbFnHW(CoilInEnthalpyForSizing, CoilInHumRatForSizing);

                ZoneEqSizing(curZoneEqNum).ATMixerHeatPriDryBulb = CoilInTempForSizing;
                ZoneEqSizing(curZoneEqNum).ATMixerHeatPriHumRat = CoilInHumRatForSizing;
            }
        }

        // If air loop has cooling coil use SA conditions, else if OA sys has coils then use precool conditions, else use OA conditions
        if (state.dataAirSystemsData->PrimaryAirSystems(airLoopIndex).CentralCoolCoilExists) {
            // if central cooling coil exists, ATMixer outlet is assumed to be at supply air conditions described in sizing input
            ZoneEqSizing(curZoneEqNum).ATMixerCoolPriDryBulb = FinalSysSizing(airLoopIndex).CoolSupTemp;
            ZoneEqSizing(curZoneEqNum).ATMixerCoolPriHumRat = FinalSysSizing(airLoopIndex).CoolSupHumRat;
        } else if (state.dataAirSystemsData->PrimaryAirSystems(airLoopIndex).NumOACoolCoils > 0) {
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
                Real64 CoilInEnthalpyForSizing =
                    OutAirFrac * Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).PrecoolTemp, FinalSysSizing(airLoopIndex).PrecoolHumRat) +
                    (1 - OutAirFrac) *
                        Psychrometrics::PsyHFnTdbW(FinalSysSizing(airLoopIndex).RetTempAtCoolPeak, FinalSysSizing(airLoopIndex).RetHumRatAtCoolPeak);

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

void SingleDuctAirTerminal::CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state)
{
    // calculates zone outdoor air volume flow rate using the supply air flow rate and OA fraction
    if (this->AirLoopNum > 0) {
        this->OutdoorAirFlowRate =
            (this->sd_airterminalOutlet.AirMassFlowRate / state.dataEnvrn->StdRhoAir) * state.dataAirLoop->AirLoopFlow(this->AirLoopNum).OAFrac;
    } else {
        this->OutdoorAirFlowRate = 0.0;
    }
}

//        End of Reporting subroutines for the Sys Module
// *****************************************************************************

} // namespace EnergyPlus::SingleDuct
